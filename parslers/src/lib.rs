use std::collections::HashMap;

use compiler::CompileContext;
use parser::ItemContext;
use quote::quote;

pub mod auxiliary;
pub(crate) mod compiler;
pub mod parser;

pub use parslers_reflect;

pub struct Builder {
    compile_context: compiler::CompileContext,
    parsers: HashMap<String, ItemContext<compiler::Parser>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            compile_context: CompileContext::new(),
            parsers: HashMap::new(),
        }
    }
    pub fn add_parser<'a, P: parser::Parser<'a>>(
        mut self,
        name: &str,
        parser: parser::ItemContext<P>,
    ) -> Self {
        self.parsers.insert(
            name.to_owned(),
            parser::ItemContext {
                import_path: parser.import_path,
                crate_name: parser.crate_name,
                module_path: parser.module_path,
                type_name: parser.type_name,
                t: parser.t.compile(&mut self.compile_context),
            },
        );
        self
    }
    pub fn build(self, output: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        let parsers = self.parsers.into_iter().map(|(name, parser)| {
            let name = syn::parse_str::<syn::Expr>(&format!("{name}")).unwrap();
            let parser = parser.t.compile();
            quote! {fn #name(input: &mut Chars) { #parser } }
        });

        let recursive_parsers =
            self.compile_context
                .recursive_parsers
                .into_iter()
                .map(|(name, parser)| {
                    let name = syn::parse_str::<syn::Expr>(&format!("p_{name}")).unwrap();
                    let parser = parser.compile();
                    quote! {fn #name(input: &mut Chars) { #parser } }
                });

        let all_in_one = quote! {
            #(#recursive_parsers)*

            #(#parsers)*
        };

        let file = syn::parse2::<syn::File>(all_in_one).unwrap();

        write!(output, "{}", prettyplease::unparse(&file))
    }
}
