use crate::{ast::Statement, code_gen::CompileContext, parsler::Parsler};
use std::collections::{HashMap, HashSet};

pub struct Builder {
    parsers: HashMap<String, Statement>,
    compile_context: CompileContext,
    reduce: bool,
    usage_analysis: bool,
}

impl Builder {
    pub fn new(name: &str, parser: impl Parsler) -> Self {
        let mut context = CompileContext::new();
        let statement = crate::code_gen::compile(name, parser, &mut context);
        let mut parsers = HashMap::new();
        parsers.insert(name.to_owned(), statement);

        Self {
            parsers,
            compile_context: context,
            reduce: false,
            usage_analysis: false,
        }
    }

    pub fn add_parser(mut self, name: &str, parser: impl Parsler) -> Self {
        assert!(!self.parsers.contains_key(name));
        let statement = crate::code_gen::compile(name, parser, &mut self.compile_context);
        self.parsers.insert(name.to_owned(), statement);
        self
    }

    pub fn reduce(mut self) -> Self {
        self.reduce = true;
        self
    }

    pub fn usage_analysis(mut self) -> Self {
        self.usage_analysis = true;
        self
    }

    pub fn build(mut self, file_name: &str) {
        let mut statements = self.parsers.into_values().collect::<Vec<_>>();
        if self.reduce {
            for s in &mut statements {
                s.parser = s.parser.clone().reduce();
            }
            for s in self.compile_context.named_parsers.values_mut().flatten() {
                s.parser = s.parser.clone().reduce();
            }
        }

        if self.usage_analysis {
            for s in &mut statements {
                s.parser
                    .output_used_analysis(true, &mut self.compile_context.parsers_with_unused);
            }
            for s in self.compile_context.named_parsers.values_mut().flatten() {
                s.parser
                    .output_used_analysis(true, &mut self.compile_context.parsers_with_unused);
            }
            let mut finished = HashSet::new();
            while finished.len() != self.compile_context.parsers_with_unused.len() {
                let mut new = HashSet::new();
                for name in self.compile_context.parsers_with_unused.iter() {
                    let mut parser = self
                        .compile_context
                        .named_parsers
                        .get(name)
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .clone();
                    let new_name = format!("{}_unused", name);

                    let type_ = syn::parse_str::<syn::Type>("()").unwrap();
                    parser.parser.output_used_analysis(false, &mut new);
                    self.compile_context.named_parsers.insert(
                        new_name.clone(),
                        Some(Statement {
                            public: false,
                            ident: new_name,
                            type_,
                            parser: parser.parser.clone(),
                        }),
                    );
                    finished.insert(name.clone());
                }
                self.compile_context.parsers_with_unused = self
                    .compile_context
                    .parsers_with_unused
                    .union(&new)
                    .cloned()
                    .collect();
            }
        }

        eprintln!("Parsers: {:?}", statements.iter().chain(self.compile_context.named_parsers.values().flatten()).map(|s| (&s.ident, s.parser.size())).collect::<HashMap<_, _>>());

        let parslers_out =
            crate::code_gen::gen_statement(&statements, &mut self.compile_context).to_string();

        std::fs::write(file_name, parslers_out).unwrap();
    }
}
