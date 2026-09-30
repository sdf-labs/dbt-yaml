extern crate proc_macro2;
extern crate quote;
extern crate syn;

extern crate proc_macro;

use std::str::FromStr;

use heck::ToKebabCase as _;
use heck::ToLowerCamelCase as _;
use heck::ToPascalCase as _;
use heck::ToSnakeCase as _;
use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;
use syn::parse_macro_input;
use syn::spanned::Spanned;

struct Variant<'a> {
    ident: syn::Ident,
    fields: &'a syn::Fields,
    /// Whether the variant carries `#[serde(untagged)]`.
    untagged: bool,
}

impl<'a> Variant<'a> {
    pub fn try_from_ast(variant: &'a syn::Variant) -> syn::Result<Self> {
        let mut untagged = false;
        for attr in &variant.attrs {
            if !attr.path().is_ident("serde") {
                continue;
            }
            if let Ok(syn::Expr::Path(expr_path)) = attr.parse_args()
                && expr_path.path.is_ident("untagged")
            {
                untagged = true;
                continue;
            }
            return Err(syn::Error::new(
                attr.span(),
                "UntaggedEnumDeserialize: only #[serde(untagged)] is supported on variants",
            ));
        }

        Ok(Variant {
            ident: variant.ident.clone(),
            fields: &variant.fields,
            untagged,
        })
    }

    fn gen_untagged_type_name(&self) -> syn::Result<proc_macro2::TokenStream> {
        match self.fields {
            syn::Fields::Unit => Ok(quote! { <() as __serde::Deserialize> }),
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    // If there's only one unnamed field, we can use its type directly
                    let ty = &fields.unnamed[0].ty;
                    Ok(quote! { <#ty as __serde::Deserialize> })
                } else {
                    // If there are multiple unnamed fields, we create a tuple type
                    let types = fields
                        .unnamed
                        .iter()
                        .map(|f| f.ty.clone())
                        .collect::<Vec<_>>();
                    Ok(quote! { <(#(#types),*) as __serde::Deserialize> })
                }
            }
            syn::Fields::Named(_) => Err(syn::Error::new(
                self.ident.span(),
                "UntaggedEnumDeserialize: inlined struct variants are not supported -- use a named struct type instead",
            )),
        }
    }

    fn gen_constructor(&self) -> syn::Result<proc_macro2::TokenStream> {
        let enum_name = &self.ident;
        match self.fields {
            syn::Fields::Unit => Ok(quote! { #enum_name }),
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    Ok(quote! { #enum_name(__inner) })
                } else {
                    let elems = (0..fields.unnamed.len())
                        .map(|i| {
                            let i = syn::Index::from(i);
                            quote! { __inner.#i }
                        })
                        .collect::<Vec<proc_macro2::TokenStream>>();
                    Ok(quote! { #enum_name(#(#elems),*) })
                }
            }
            syn::Fields::Named(_) => Err(syn::Error::new(
                self.ident.span(),
                "UntaggedEnumDeserialize: inlined struct variants are not supported -- use a named struct type instead",
            )),
        }
    }

    fn get_name(&self, default_rename_policy: Option<RenamePolicy>) -> String {
        if let Some(policy) = default_rename_policy {
            policy.apply(&self.ident)
        } else {
            self.ident.to_string()
        }
    }

    /// Generates the deserialization attempt for a tagged variant, using
    /// `de` as the deserializer expression.
    fn gen_tagged_deserialize_expr(
        &self,
        enum_name: &syn::Ident,
        de: proc_macro2::TokenStream,
    ) -> syn::Result<proc_macro2::TokenStream> {
        match self.fields {
            syn::Fields::Unit => {
                let enum_name = enum_name.to_string();
                let variant_name = self.ident.to_string();

                Ok(quote! {
                    __serde::Deserializer::deserialize_any(
                        #de,
                        __serde_yaml::__private::InternallyTaggedUnitVisitor::new(
                            #enum_name,
                            #variant_name
                        )
                    )
                })
            }
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    let ty = &fields.unnamed[0].ty;

                    Ok(quote! {
                        <#ty as __serde::Deserialize>::deserialize(#de)
                    })
                } else {
                    Err(syn::Error::new(
                        self.ident.span(),
                        "UntaggedEnumDeserialize: tuple variants are not allowed in internally tagged enums",
                    ))
                }
            }
            syn::Fields::Named(_) => Err(syn::Error::new(
                self.ident.span(),
                "UntaggedEnumDeserialize: inlined struct variants are not supported -- use a named struct type instead",
            )),
        }
    }

    fn gen_tagged_deserialize_arm(
        &self,
        enum_name: &syn::Ident,
        default_rename_policy: Option<RenamePolicy>,
    ) -> syn::Result<proc_macro2::TokenStream> {
        let expr = self.gen_tagged_deserialize_expr(enum_name, quote! { __deserializer })?;
        let constructor = self.gen_constructor()?;
        let tag_name = if let Some(policy) = default_rename_policy {
            policy.apply(&self.ident)
        } else {
            self.ident.to_string()
        };

        let block = quote! {
            Some(#tag_name) => {
                let __inner = #expr.map_err(|e| {
                    __serde::de::Error::custom(e)
                })?;
                return Ok(#enum_name::#constructor);
            }
        };

        Ok(block)
    }

    fn gen_untagged_deserialize_block(&self) -> syn::Result<proc_macro2::TokenStream> {
        let type_name = self.gen_untagged_type_name()?;

        let block = quote! {
            __unused_keys.clear();
            let __inner = {
                let mut collect_unused_keys =
                    |path: __serde_yaml::Path<'_>, key: &__serde_yaml::Value, value: &__serde_yaml::Value| {
                        __unused_keys.push((path.to_owned_path(), key.clone(), value.clone()));
                    };

                #type_name::deserialize(__state.get_deserializer(Some(&mut collect_unused_keys)))
            };
        };

        Ok(block)
    }

    /// Generates the block that constructs the enum variant after a
    /// successful attempt, forwarding collected unused keys to the saved
    /// callback.
    ///
    /// When `tag_key_to_skip` is given, the tag key itself is not forwarded:
    /// it belongs to the enum's dispatch, not to the matched variant.
    fn gen_constructor_block(
        &self,
        enum_name: &syn::Ident,
        tag_key_to_skip: Option<&str>,
    ) -> syn::Result<proc_macro2::TokenStream> {
        let constructor = self.gen_constructor()?;

        let skip_tag_key = tag_key_to_skip.map(|tag_key| {
            quote! {
                if __state.is_direct_child(path, #tag_key) {
                    continue;
                }
            }
        });

        let block = quote! {
            if let Ok(__inner) = __inner {
                if let Some(mut __callback) = __unused_key_callback {
                    for (path, key, value) in __unused_keys.iter() {
                        #skip_tag_key
                        __callback(*path.as_path(), key, value);
                    }
                }
                return Ok(#enum_name::#constructor);
            }
        };

        Ok(block)
    }

    /// Generates the dispatch arm for a tagged variant of a mixed enum: the
    /// variant is attempted with the tag key stripped; on failure the tag is
    /// restored and dispatch falls through to the untagged variants.
    fn gen_mixed_tagged_arm(
        &self,
        enum_name: &syn::Ident,
        tag_key: &str,
        default_rename_policy: Option<RenamePolicy>,
    ) -> syn::Result<proc_macro2::TokenStream> {
        let expr = self.gen_tagged_deserialize_expr(
            enum_name,
            quote! { __state.get_deserializer(Some(&mut collect_unused_keys)) },
        )?;
        let constructor = self.gen_constructor()?;
        let tag_name = self.get_name(default_rename_policy);

        let block = quote! {
            Some(#tag_name) => {
                let __stripped_tag = __state.strip_tag_key(#tag_key);
                __unused_keys.clear();
                let __inner = {
                    let mut collect_unused_keys =
                        |path: __serde_yaml::Path<'_>, key: &__serde_yaml::Value, value: &__serde_yaml::Value| {
                            __unused_keys.push((path.to_owned_path(), key.clone(), value.clone()));
                        };

                    #expr
                };
                if let Ok(__inner) = __inner {
                    if let Some(mut __callback) = __unused_key_callback {
                        for (path, key, value) in __unused_keys.iter() {
                            __callback(*path.as_path(), key, value);
                        }
                    }
                    return Ok(#enum_name::#constructor);
                }
                if let Some(__stripped_tag) = __stripped_tag {
                    __state.restore_tag_key(__stripped_tag);
                }
            }
        };

        Ok(block)
    }

    /// Generates the dispatch arm for a tagged variant of an
    /// externally-tagged mixed enum: the variant is attempted against the
    /// inner content of the single-key mapping; on failure the original value
    /// is restored and dispatch falls through to the untagged variants.
    ///
    /// Unit variants attempt `()` against the content, which only succeeds
    /// for null content (`{Unit: null}`), matching serde.
    fn gen_mixed_external_arm(
        &self,
        enum_name: &syn::Ident,
        default_rename_policy: Option<RenamePolicy>,
    ) -> syn::Result<proc_macro2::TokenStream> {
        let type_name = self.gen_untagged_type_name()?;
        let constructor = self.gen_constructor()?;
        let tag_name = self.get_name(default_rename_policy);

        let block = quote! {
            Some(#tag_name) => {
                let (__original, __tag_index, __tag_key) = __state
                    .focus_external_content(#tag_name)
                    .expect("externally-tagged shape checked by extraction");
                __unused_keys.clear();
                let __inner = {
                    let mut collect_unused_keys =
                        |path: __serde_yaml::Path<'_>, key: &__serde_yaml::Value, value: &__serde_yaml::Value| {
                            __unused_keys.push((path.to_owned_path(), key.clone(), value.clone()));
                        };

                    #type_name::deserialize(__state.get_deserializer(Some(&mut collect_unused_keys)))
                };
                if let Ok(__inner) = __inner {
                    __state.release_external_content(__original);
                    if let Some(mut __callback) = __unused_key_callback {
                        for (path, key, value) in __unused_keys.iter() {
                            __callback(*path.as_path(), key, value);
                        }
                        __state.forward_sibling_keys(#tag_name, &mut *__callback);
                    }
                    return Ok(#enum_name::#constructor);
                }
                __state.restore_external_content(__original, __tag_index, __tag_key);
            }
        };

        Ok(block)
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RenamePolicy {
    /// Rename the field to its snake_case equivalent
    SnakeCase,
    /// Rename the field to its camelCase equivalent
    CamelCase,
    /// Rename the field to its lower_case equivalent
    LowerCase,
    /// Rename the field to its UPPER_CASE equivalent
    UpperCase,
    /// Rename the field to its PascalCase equivalent
    PascalCase,
    /// Rename the field to its kebab-case equivalent
    KebabCase,
}

impl FromStr for RenamePolicy {
    type Err = syn::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "snake_case" => Ok(RenamePolicy::SnakeCase),
            "camelCase" => Ok(RenamePolicy::CamelCase),
            "lowercase" => Ok(RenamePolicy::LowerCase),
            "UPPERCASE" => Ok(RenamePolicy::UpperCase),
            "PascalCase" => Ok(RenamePolicy::PascalCase),
            "kebab-case" => Ok(RenamePolicy::KebabCase),
            _ => Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("Unknown rename policy: {s}"),
            )),
        }
    }
}

impl RenamePolicy {
    fn apply(&self, ident: &syn::Ident) -> String {
        match self {
            RenamePolicy::SnakeCase => ident.to_string().to_snake_case(),
            RenamePolicy::CamelCase => ident.to_string().to_lower_camel_case(),
            RenamePolicy::LowerCase => ident.to_string().to_lowercase(),
            RenamePolicy::UpperCase => ident.to_string().to_uppercase(),
            RenamePolicy::PascalCase => ident.to_string().to_pascal_case(),
            RenamePolicy::KebabCase => ident.to_string().to_kebab_case(),
        }
    }
}

struct EnumDef<'a> {
    ident: syn::Ident,
    generics: &'a syn::Generics,
    variants: Vec<Variant<'a>>,
    tag: Option<String>,
    /// Whether the container carries `#[serde(untagged)]`.
    untagged_container: bool,
    rename_all: Option<RenamePolicy>,
}

impl<'a> EnumDef<'a> {
    pub fn try_from_ast(input: &'a DeriveInput) -> syn::Result<Self> {
        // Check if the input is an enum
        let syn::Data::Enum(data_enum) = &input.data else {
            return Err(syn::Error::new(
                input.span(),
                "UntaggedEnumDeserialize: can only be derived for enums",
            ));
        };

        // Check for #[serde(untagged)] attribute
        let has_untagged_attr = input.attrs.iter().any(|attr| {
            if !attr.path().is_ident("serde") {
                return false;
            }
            if let Ok(syn::Expr::Path(expr_path)) = attr.parse_args() {
                return expr_path.path.is_ident("untagged");
            }
            false
        });
        // Check for #[serde(tag = "...")] attribute
        let tag_attr = input.attrs.iter().find_map(|attr| {
            if !attr.path().is_ident("serde") {
                return None;
            }
            let Ok(syn::Expr::Assign(expr)) = attr.parse_args() else {
                return None;
            };
            let syn::Expr::Path(expr_path) = *expr.left else {
                return None;
            };
            if !expr_path.path.is_ident("tag") {
                return None;
            }

            match *expr.right {
                syn::Expr::Lit(lit) => {
                    match lit.lit {
                        syn::Lit::Str(lit) => Some(lit.value()),
                        _ => None, // Invalid tag attribute
                    }
                }
                _ => None,
            }
        });

        // Extract any #[serde(rename_all = "...")] directives
        let rename_all_attr = input.attrs.iter().find_map(|attr| {
            if !attr.path().is_ident("serde") {
                return None;
            }
            let Ok(syn::Expr::Assign(expr)) = attr.parse_args() else {
                return None;
            };
            let syn::Expr::Path(expr_path) = *expr.left else {
                return None;
            };
            if !expr_path.path.is_ident("rename_all") {
                return None;
            }

            match *expr.right {
                syn::Expr::Lit(lit) => {
                    match lit.lit {
                        syn::Lit::Str(lit) => Some(lit.value()),
                        _ => None, // Invalid rename_all attribute
                    }
                }
                _ => None,
            }
        });
        let rename_all = rename_all_attr
            .map(|a| RenamePolicy::from_str(a.as_str()))
            .transpose()?;

        // Check the enum has no borrowed lifetimes
        for param in &input.generics.params {
            if let syn::GenericParam::Lifetime(lifetime_param) = param {
                return Err(syn::Error::new(
                    lifetime_param.lifetime.span(),
                    "UntaggedEnumDeserialize: borrowed lifetimes are not supported",
                ));
            }
        }

        let ident = input.ident.clone();
        let generics = &input.generics;
        let variants = data_enum
            .variants
            .iter()
            .map(Variant::try_from_ast)
            .collect::<syn::Result<Vec<_>>>()?;

        let has_untagged_variants = variants.iter().any(|v| v.untagged);

        // Without a container attribute the enum is externally tagged, which
        // is only supported when untagged variants provide the fallback.
        if !has_untagged_attr && tag_attr.is_none() && !has_untagged_variants {
            return Err(syn::Error::new(
                input.span(),
                "UntaggedEnumDeserialize: can only be derived for enums with #[serde(untagged)] or #[serde(tag = \"...\")] attributes, or with at least one #[serde(untagged)] variant",
            ));
        }

        // Untagged variants must come after all tagged variants (matching
        // serde). In an untagged container every variant is effectively
        // untagged, so the rule does not apply.
        if !has_untagged_attr {
            let mut seen_untagged = false;
            for variant in &variants {
                if variant.untagged {
                    seen_untagged = true;
                } else if seen_untagged {
                    return Err(syn::Error::new(
                        variant.ident.span(),
                        "UntaggedEnumDeserialize: all variants with the #[serde(untagged)] attribute must be placed at the end of the enum",
                    ));
                }
            }
        }

        Ok(EnumDef {
            ident,
            generics,
            variants,
            tag: tag_attr,
            untagged_container: has_untagged_attr,
            rename_all,
        })
    }

    fn build_impl_generics(&self) -> syn::Generics {
        let mut generics = self.generics.clone();
        // Inject a 'de lifetime bound for deserialization
        generics
            .params
            .push(syn::GenericParam::Lifetime(syn::LifetimeParam {
                attrs: Vec::new(),
                lifetime: syn::Lifetime::new("'de", self.ident.span()),
                colon_token: None,
                bounds: syn::punctuated::Punctuated::new(),
            }));

        // Inject a where clause bound `T: serde::de::Deserialize<'_>` for each
        // non-lifetime type parameter `T`:
        for param in &mut generics.params {
            if let syn::GenericParam::Type(ty_param) = param {
                ty_param
                    .bounds
                    .push(syn::parse_quote!(__serde::de::DeserializeOwned));
            }
        }

        generics
    }

    fn gen_untagged_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let enum_name = &self.ident;
        let generics = self.build_impl_generics();
        let (impl_generics, _, where_clause) = generics.split_for_impl();
        let (_, ty_generics, _) = self.generics.split_for_impl();

        let mut variant_blocks = Vec::new();
        for variant in &self.variants {
            let deserialize_block = variant.gen_untagged_deserialize_block()?;
            let constructor_block = variant.gen_constructor_block(enum_name, None)?;
            variant_blocks.push(quote! {
                #deserialize_block
                #constructor_block
            });
        }

        let err_message = format!("data did not match any variant of untagged enum {enum_name}");

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics __serde::Deserialize<'de> for #enum_name #ty_generics #where_clause {
                fn deserialize<__D>(deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: __serde::de::Deserializer<'de>,
                {
                    let mut __state = __serde_yaml::value::extract_reusable_deserializer_state(deserializer)?;
                    let __unused_key_callback = __state.take_unused_key_callback();
                    let mut __unused_keys = vec![];

                    #( #variant_blocks )*

                    Err(__serde::de::Error::custom(#err_message))
                }
            }
        })
    }

    fn gen_internally_tagged_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let enum_name = &self.ident;
        let tag_key = self.tag.as_ref().expect("Expected tag key");
        let generics = self.build_impl_generics();
        let (impl_generics, _, where_clause) = generics.split_for_impl();
        let (_, ty_generics, _) = self.generics.split_for_impl();

        let variant_arms = self
            .variants
            .iter()
            .map(|variant| variant.gen_tagged_deserialize_arm(enum_name, self.rename_all))
            .collect::<syn::Result<Vec<_>>>()?;
        let variant_names = self
            .variants
            .iter()
            .map(|variant| variant.get_name(self.rename_all))
            .collect::<Vec<_>>();

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics __serde::Deserialize<'de> for #enum_name #ty_generics #where_clause {
                fn deserialize<__D>(deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: __serde::de::Deserializer<'de>,
                {
                    let (__tag, mut __state) = __serde_yaml::value::extract_tag_and_deserializer_state(deserializer, #tag_key)?;
                    let __deserializer = __state.get_owned_deserializer();

                    match __tag.as_str() {
                        #( #variant_arms )*
                        Some(tag) => {
                            return Err(__serde::de::Error::unknown_variant(
                                tag,
                                &[ #( #variant_names ),* ]
                             ));
                        }
                        None => {
                            return Err(__serde::de::Error::invalid_value(
                                __tag.unexpected(),
                                &"a valid tag for internally tagged enum"
                            ));
                        }
                    }
                }
            }
        })
    }

    /// Generates the impl for an internally-tagged enum that also has
    /// untagged variants: a recognized tag is attempted first (with the tag
    /// key stripped); any failure falls back to trying the untagged variants
    /// in order against the full original value.
    fn gen_mixed_tagged_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let enum_name = &self.ident;
        let tag_key = self.tag.as_ref().expect("Expected tag key");
        let generics = self.build_impl_generics();
        let (impl_generics, _, where_clause) = generics.split_for_impl();
        let (_, ty_generics, _) = self.generics.split_for_impl();

        let mut tagged_arms = Vec::new();
        let mut untagged_blocks = Vec::new();
        for variant in &self.variants {
            if variant.untagged {
                let deserialize_block = variant.gen_untagged_deserialize_block()?;
                let constructor_block = variant.gen_constructor_block(enum_name, Some(tag_key))?;
                untagged_blocks.push(quote! {
                    #deserialize_block
                    #constructor_block
                });
            } else {
                tagged_arms.push(variant.gen_mixed_tagged_arm(
                    enum_name,
                    tag_key,
                    self.rename_all,
                )?);
            }
        }

        let err_message = format!("data did not match any variant of untagged enum {enum_name}");

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics __serde::Deserialize<'de> for #enum_name #ty_generics #where_clause {
                fn deserialize<__D>(deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: __serde::de::Deserializer<'de>,
                {
                    let (__tag, mut __state) = __serde_yaml::value::extract_optional_tag_and_deserializer_state(deserializer, #tag_key)?;
                    let __unused_key_callback = __state.take_unused_key_callback();
                    let mut __unused_keys = vec![];

                    match __tag.as_ref().and_then(|__v| __v.as_str()) {
                        #( #tagged_arms )*
                        _ => {}
                    }

                    #( #untagged_blocks )*

                    Err(__serde::de::Error::custom(#err_message))
                }
            }
        })
    }

    /// Generates the impl for an externally-tagged enum (no container
    /// attribute) with untagged variants: a recognized tag is attempted
    /// first (against the inner content of the single-key mapping); any
    /// failure falls back to trying the untagged variants in order against
    /// the full original value.
    fn gen_mixed_external_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let enum_name = &self.ident;
        let generics = self.build_impl_generics();
        let (impl_generics, _, where_clause) = generics.split_for_impl();
        let (_, ty_generics, _) = self.generics.split_for_impl();

        let mut bare_unit_arms = Vec::new();
        let mut wrapped_arms = Vec::new();
        let mut untagged_blocks = Vec::new();
        for variant in &self.variants {
            if variant.untagged {
                let deserialize_block = variant.gen_untagged_deserialize_block()?;
                let constructor_block = variant.gen_constructor_block(enum_name, None)?;
                untagged_blocks.push(quote! {
                    #deserialize_block
                    #constructor_block
                });
            } else {
                if let syn::Fields::Unit = variant.fields {
                    let tag_name = variant.get_name(self.rename_all);
                    let ident = &variant.ident;
                    bare_unit_arms.push(quote! {
                        Some(#tag_name) => return Ok(#enum_name::#ident),
                    });
                }
                wrapped_arms.push(variant.gen_mixed_external_arm(enum_name, self.rename_all)?);
            }
        }

        let tagged_variant_names: Vec<String> = self
            .variants
            .iter()
            .filter(|v| !v.untagged)
            .map(|v| v.get_name(self.rename_all))
            .collect();

        let err_message = format!("data did not match any variant of untagged enum {enum_name}");

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics __serde::Deserialize<'de> for #enum_name #ty_generics #where_clause {
                fn deserialize<__D>(deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: __serde::de::Deserializer<'de>,
                {
                    let (__tag, mut __state) = __serde_yaml::value::extract_external_tag_and_deserializer_state(deserializer, &[#( #tagged_variant_names ),*])?;
                    let __unused_key_callback = __state.take_unused_key_callback();
                    let mut __unused_keys = vec![];

                    match __tag {
                        Some(__serde_yaml::value::ExternalTag::Bare(__tag)) => {
                            match __tag.as_str() {
                                #( #bare_unit_arms )*
                                _ => {}
                            }
                        }
                        Some(__serde_yaml::value::ExternalTag::Wrapped(__tag)) => {
                            match __tag.as_str() {
                                #( #wrapped_arms )*
                                _ => {}
                            }
                        }
                        None => {}
                    }

                    #( #untagged_blocks )*

                    Err(__serde::de::Error::custom(#err_message))
                }
            }
        })
    }

    fn gen_deserialize_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let has_untagged_variants = self.variants.iter().any(|v| v.untagged);
        match (&self.tag, self.untagged_container, has_untagged_variants) {
            (Some(_), _, true) => self.gen_mixed_tagged_impl(),
            (Some(_), _, false) => self.gen_internally_tagged_impl(),
            (None, true, _) => self.gen_untagged_impl(),
            (None, false, true) => self.gen_mixed_external_impl(),
            (None, false, false) => unreachable!("rejected by EnumDef::try_from_ast"),
        }
    }
}

fn expand_derive_deserialize(
    input: &mut syn::DeriveInput,
) -> syn::Result<proc_macro2::TokenStream> {
    let enum_def = EnumDef::try_from_ast(input)?;
    let deserialize_impl = enum_def.gen_deserialize_impl()?;

    let block = quote! {
        const _: () = {
            #[allow(unused_extern_crates, clippy::useless_attribute)]
            extern crate dbt_yaml as __serde_yaml;
            #[allow(unused_extern_crates, clippy::useless_attribute)]
            extern crate serde as __serde;
            #deserialize_impl
        };
    };

    Ok(block)
}

/// Derives `Deserialize` for an enum, with span preservation and unused-key
/// forwarding, matching serde's behavior for mixed tagged/untagged enums.
///
/// Supported container attributes:
///
/// | Attribute | Mode |
/// |---|---|
/// | `#[serde(untagged)]` | Variants are tried in declaration order |
/// | `#[serde(tag = "...")]` | Internally tagged; `#[serde(untagged)]` variants are the fallback |
/// | *(none)* | Externally tagged; requires at least one `#[serde(untagged)]` variant as fallback |
///
/// The only variant attribute is `#[serde(untagged)]`; untagged variants must
/// come after all tagged variants (matching serde). On any tagged-dispatch
/// failure — unknown tag, missing tag, or a rejected variant body — the
/// untagged variants are tried in order against the full original value.
///
/// Not supported: adjacent tagging (`tag` + `content`), other variant
/// attributes (`rename`, `skip`, ...), inlined struct variants, and borrowed
/// lifetimes.
#[proc_macro_derive(UntaggedEnumDeserialize, attributes(serde))]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    expand_derive_deserialize(&mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expand(input: syn::DeriveInput) -> syn::Result<()> {
        let mut input = input;
        expand_derive_deserialize(&mut input).map(|_| ())
    }

    #[test]
    fn untagged_variants_must_come_last() {
        let input = syn::parse_quote! {
            #[serde(tag = "type")]
            enum E {
                #[serde(untagged)]
                A(i32),
                B(String),
            }
        };
        let err = expand(input).unwrap_err();
        assert!(
            err.to_string()
                .contains("must be placed at the end of the enum"),
            "unexpected error: {err}"
        );

        // Same rule without a container attribute.
        let input = syn::parse_quote! {
            enum E {
                #[serde(untagged)]
                A(i32),
                B(String),
            }
        };
        assert!(expand(input).is_err());
    }

    #[test]
    fn external_mode_requires_an_untagged_variant() {
        let input = syn::parse_quote! {
            enum E {
                A(i32),
                B(String),
            }
        };
        let err = expand(input).unwrap_err();
        assert!(
            err.to_string()
                .contains("at least one #[serde(untagged)] variant"),
            "unexpected error: {err}"
        );

        let input = syn::parse_quote! {
            enum E {
                A(i32),
                #[serde(untagged)]
                B(String),
            }
        };
        assert!(expand(input).is_ok());
    }

    #[test]
    fn only_untagged_is_supported_on_variants() {
        let input = syn::parse_quote! {
            #[serde(untagged)]
            enum E {
                #[serde(rename = "a")]
                A(i32),
            }
        };
        let err = expand(input).unwrap_err();
        assert!(
            err.to_string()
                .contains("only #[serde(untagged)] is supported on variants"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn untagged_variant_attr_is_a_noop_in_untagged_container() {
        let input = syn::parse_quote! {
            #[serde(untagged)]
            enum E {
                #[serde(untagged)]
                A(i32),
                B(String),
            }
        };
        assert!(expand(input).is_ok());
    }
}
