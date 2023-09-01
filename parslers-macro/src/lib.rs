#![feature(assert_matches)]
#![feature(box_patterns)]

extern crate proc_macro;

use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{Expr, ExprCall, ExprPath, Path, PathSegment, Stmt, Type, TypePath, TypeTraitObject};

#[proc_macro_attribute]
pub fn reflect(
    arg: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let function = syn::parse_macro_input!(item as syn::ItemFn);
    let function_name = function.sig.ident.clone();
    let function_arg_names = function
        .sig
        .inputs
        .clone()
        .into_iter()
        .map(|item| {
            if let syn::FnArg::Typed(pat_type) = item {
                *pat_type.pat
            } else {
                panic!("Expected typed argument")
            }
        })
        .collect::<Vec<_>>();
    let function_arg_types: Vec<_> = function
        .sig
        .inputs
        .clone()
        .into_iter()
        .map(|item| {
            if let syn::FnArg::Typed(pat_type) = item {
                *pat_type.ty
            } else {
                panic!("Expected typed argument")
            }
        })
        .collect();
    let function_arg_type_idents = function_arg_types
        .iter()
        .map(|t| t.to_token_stream().to_string())
        .collect::<Vec<_>>();
    let all_function_generics = function.sig.generics.clone();
    let _function_generics = function
        .sig
        .generics
        .clone()
        .params
        .into_iter()
        .filter_map(|x| match &x {
            syn::GenericParam::Type(t) => {
                if function_arg_type_idents.contains(&t.ident.to_string()) {
                    Some(x)
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let function_body = function.block.clone().stmts;
    let mut impl_ty = None;
    let function_output = match function.sig.output.clone() {
        syn::ReturnType::Default => quote! {()},
        syn::ReturnType::Type(_, ty) => {
            if let Type::Path(TypePath {
                qself: None,
                path:
                    Path {
                        leading_colon: None,
                        segments,
                    },
            }) = ty.clone().as_ref()
            {
                if let Some(PathSegment {
                    ident,
                    arguments: syn::PathArguments::AngleBracketed(args),
                }) = segments.first()
                {
                    if ident == "Box" {
                        if let Some(syn::GenericArgument::Type(Type::TraitObject(
                            TypeTraitObject {
                                dyn_token: Some(_),
                                bounds,
                            },
                        ))) = args.args.first()
                        {
                            impl_ty = Some(quote! {
                                impl #bounds
                            })
                        }
                    }
                }
            }
            quote! {
                #ty
            }
        }
    };

    let mut new_function_body = function_body.clone();

    if impl_ty.is_some() {
        if let Stmt::Expr(
            Expr::Call(ExprCall {
                attrs: _,
                func:
                    box Expr::Path(ExprPath {
                        attrs: _,
                        qself: None,
                        path:
                            Path {
                                leading_colon: None,
                                segments,
                            },
                    }),
                paren_token: _,
                args,
            }),
            None,
        ) = function_body.last().cloned().unwrap()
        {
            let mut iter = segments.iter();
            if let Some(PathSegment {
                ident,
                arguments: _,
            }) = iter.next()
            {
                if ident == "Box" && iter.next().unwrap().ident == "new" {
                    *new_function_body.last_mut().unwrap() =
                        Stmt::Expr(args.first().unwrap().clone(), None);
                }
            }
        }
    }

    let impl_ty = impl_ty.unwrap_or_else(|| quote! {#function_output});

    let new_function = if arg.to_string() == "unbox" {
        quote! {
            fn #function_name #all_function_generics(#(#function_arg_names: #function_arg_types,)*) -> #impl_ty {
                #(#new_function_body)*
            }
        }
    } else {
        quote! {#function}
    };

    quote! {
        #[allow(non_camel_case_types)]
        #[allow(incorrect_ident_case)]
        #[derive(Clone, Copy, Debug, Hash)]
        pub struct #function_name;

        impl #all_function_generics FnOnce<(#(#function_arg_types,)*)> for #function_name
        {
            type Output = #function_output;
            // Required method
            extern "rust-call" fn call_once(self, (#(#function_arg_names,)*): (#(#function_arg_types,)*)) -> Self::Output {
                #(#function_body)*
            }
        }

        impl Reflect for #function_name {
            fn name(&self) -> &'static str {
                stringify!(#function_name)
            }
            fn reflect(&self) -> String {
                stringify!(#new_function).to_owned()
            }
        }
    }
    .into()
}

#[proc_macro_derive(Reflected)]
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

fn impl_data_struct(name: &Ident, ds: &syn::DataStruct, gens: &syn::Generics) -> proc_macro::TokenStream {
    let fields = &ds.fields;
    let (impl_generics, ty_generics, where_clause) = gens.split_for_impl();
    
    // let impl_generics = impl_generics.
    
    let field_getter = fields.iter().enumerate().map(|(i, f)| {
        // let b = &syn::parse_str::<syn::Ident>(&i.to_string()).unwrap();
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
        syn::Fields::Unnamed(_) => {quote! {
            impl #impl_generics Reflect for #name #ty_generics #where_clause {
                fn name(&self) -> &'static str {
                    stringify!(#name)
                }
                fn reflect(&self) -> String {
                    format!("{} ( {} )", std::any::type_name::<Self>().replace("<", "::<"), [#(#field_getter),*].join(","))
                }
            }
        }}
        syn::Fields::Unit => {quote! {
            impl #impl_generics Reflect for #name #ty_generics #where_clause {
                fn name(&self) -> &'static str {
                    stringify!(#name)
                }
                fn reflect(&self) -> String {
                    format!("{} ()", std::any::type_name::<Self>().replace("<", "::<"))
                }
            }
        }}
    };
    // let gen = 
    // eprintln!("{}", gen.to_string());
    gen.into()
}

// impl Reflect for Brainfuck {
//     fn reflect(&self) -> String {
//         format!(
//             "{}::{}",
//             std::any::type_name::<Self>(),
//             match self {
//                 Brainfuck::Add => "Add".to_owned(),
//                 Brainfuck::Sub => "Sub".to_owned(),
//                 Brainfuck::Left => "Left".to_owned(),
//                 Brainfuck::Right => "Right".to_owned(),
//                 Brainfuck::Read => "Read".to_owned(),
//                 Brainfuck::Print => "Print".to_owned(),
//                 Brainfuck::Loop(l) => format!("Loop({})", l.reflect()),
//             }
//         )
//     }
// }

fn impl_enum(name: &Ident, de: &syn::DataEnum, gens: &syn::Generics) -> proc_macro::TokenStream {
    let variants = &de.variants;

    let variant_idents = variants.iter().enumerate().map(|(_i, v)| {
        let ident = &v.ident;

        match &v.fields {
            // syn::Fields::Named(_) => {

            //     quote! {
            //         #ident => #ident
            //     }
            // },
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

    


    // let gen = match fields {
    //     syn::Fields::Named(_) => {
    //         quote! {
    //             impl #impl_generics Reflect for #name #ty_generics #where_clause {
    //                 fn name(&self) -> &'static str {
    //                     stringify!(#name)
    //                 }
    //                 fn reflect(&self) -> String {
    //                     format!("{} {{ {} }}", std::any::type_name::<Self>(), [#(#field_getter),*].join(","))
    //                 }
    //             }
    //         }
    //     }
    //     syn::Fields::Unnamed(_) => {quote! {
    //         impl #impl_generics Reflect for #name #ty_generics #where_clause {
    //             fn name(&self) -> &'static str {
    //                 stringify!(#name)
    //             }
    //             fn reflect(&self) -> String {
    //                 format!("{} ( {} )", std::any::type_name::<Self>(), [#(#field_getter),*].join(","))
    //             }
    //         }
    //     }}
    //     syn::Fields::Unit => {quote! {
    //         impl #impl_generics Reflect for #name #ty_generics #where_clause {
    //             fn name(&self) -> &'static str {
    //                 stringify!(#name)
    //             }
    //             fn reflect(&self) -> String {
    //                 format!("{} ()", std::any::type_name::<Self>())
    //             }
    //         }
    //     }}
    // };
    // let gen = 
    // eprintln!("{}", gen.to_string());
    gen.into()
}