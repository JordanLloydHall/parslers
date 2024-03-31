extern crate proc_macro;

use proc_macro2::Ident;
use quote::{quote, ToTokens};

#[proc_macro_derive(Reflect)]
pub fn reflect_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_reflect(&ast)
}

fn impl_reflect(ast: &syn::DeriveInput) -> proc_macro::TokenStream {
    let name = &ast.ident;
    let data = &ast.data;
    let gens = &ast.generics;

    match data {
        syn::Data::Struct(ds) => impl_data_struct(name, ds, gens),
        syn::Data::Enum(de) => impl_enum(name, de, gens),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    }
}

fn impl_data_struct(
    name: &Ident,
    ds: &syn::DataStruct,
    gens: &syn::Generics,
) -> proc_macro::TokenStream {
    let fields = &ds.fields;
    let (impl_generics, ty_generics, where_clause) = gens.split_for_impl();

    let field_getter = fields.iter().enumerate().map(|(i, f)| {
        let field_name = &f.ident;
        if let Some(_) = field_name {
            quote! {
            format!("{}: {}", stringify!(#field_name), self.#field_name.reflect())
            }
        } else {
            quote! {
            format!("{}", self.#i.reflect())
            }
        }
    });

    let gen = match fields {
        syn::Fields::Named(_) => {
            quote! {
                impl #impl_generics Reflect for #name #ty_generics #where_clause {
                    fn name(&self) -> &'static str {
                        stringify!(#name)
                    }
                    fn reflect(&self) -> String {
                        format!("{} {{ {} }}", std::any::type_name::<Self>(), [#(#field_getter),*].join(","))
                    }
                }
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                impl #impl_generics Reflect for #name #ty_generics #where_clause {
                    fn name(&self) -> &'static str {
                        stringify!(#name)
                    }
                    fn reflect(&self) -> String {
                        format!("{} ( {} )", std::any::type_name::<Self>().replace("<", "::<"), [#(#field_getter),*].join(","))
                    }
                }
            }
        }
        syn::Fields::Unit => {
            quote! {
                impl #impl_generics Reflect for #name #ty_generics #where_clause {
                    fn name(&self) -> &'static str {
                        stringify!(#name)
                    }
                    fn reflect(&self) -> String {
                        format!("{} ()", std::any::type_name::<Self>().replace("<", "::<"))
                    }
                }
            }
        }
    };
    gen.into()
}

fn impl_enum(name: &Ident, de: &syn::DataEnum, gens: &syn::Generics) -> proc_macro::TokenStream {
    let variants = &de.variants;

    let variant_idents = variants.iter().enumerate().map(|(_i, v)| {
        let ident = &v.ident;

        match &v.fields {
            syn::Fields::Unnamed(un) => {
                let valss = un.unnamed.clone();
                let vals = valss.iter().enumerate().map(|(i, _f)| {let id = &syn::parse_str::<syn::Ident>(&format!("i{}", i)).unwrap(); quote! {
                    #id . reflect()
                }});

                let val_names = valss.iter().enumerate().map(|(i, _)| {syn::parse_str::<syn::Ident>(&format!("i{}", i)).unwrap().to_token_stream()});

                quote! {
                    Self:: #ident (#(#val_names),*) => format!("{}({})", stringify!(#name :: #ident), [#(#vals),*].join(","))
                }
            },
            syn::Fields::Unit => quote! {
                Self:: #ident => format!("{}", stringify!(#ident))
            },
            _ => unimplemented!()
        }
    });

    let (impl_generics, ty_generics, where_clause) = gens.split_for_impl();

    let gen = quote! {
        impl #impl_generics Reflect for #name #ty_generics #where_clause {
            fn name(&self) -> &'static str {
                stringify!(#name)
            }
            fn reflect(&self) -> String {
                format!("{}::{}",
                    std::any::type_name::<Self>().replace("<", "::<"),
                    match self {
                        #(#variant_idents),*
                    }
                )
            }
        }
    };
    gen.into()
}
