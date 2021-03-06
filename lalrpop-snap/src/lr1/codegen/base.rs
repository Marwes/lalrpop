//! Base helper routines for a code generator.

use grammar::repr::*;
use lr1::core::*;
use rust::RustWrite;
use std::io::{self, Write};

/// Base struct for various kinds of code generator. The flavor of
/// code generator is customized by supplying distinct types for `C`
/// (e.g., `self::ascent::RecursiveAscent`).
pub struct CodeGenerator<'codegen, 'grammar: 'codegen, W: Write + 'codegen, C> {
    /// the complete grammar
    pub grammar: &'grammar Grammar,

    /// some suitable prefix to separate our identifiers from the user's
    pub prefix: &'grammar str,

    /// types from the grammar
    pub types: &'grammar Types,

    /// the start symbol S the user specified
    pub user_start_symbol: NonterminalString,

    /// the synthetic start symbol S' that we specified
    pub start_symbol: NonterminalString,

    /// the vector of states
    pub states: &'codegen [LR1State<'grammar>],

    /// where we write output
    pub out: &'codegen mut RustWrite<W>,

    /// where to find the action routines (typically `super`)
    pub action_module: String,

    /// custom fields for the specific kind of codegenerator
    /// (recursive ascent, table-driven, etc)
    pub custom: C,

    pub repeatable: bool,
}

impl<'codegen, 'grammar, W: Write, C> CodeGenerator<'codegen, 'grammar, W, C> {
    pub fn new(grammar: &'grammar Grammar,
               user_start_symbol: NonterminalString,
               start_symbol: NonterminalString,
               states: &'codegen [LR1State<'grammar>],
               out: &'codegen mut RustWrite<W>,
               repeatable: bool,
               action_module: &str,
               custom: C)
               -> Self {
        CodeGenerator {
            grammar: grammar,
            prefix: &grammar.prefix,
            types: &grammar.types,
            states: states,
            user_start_symbol: user_start_symbol,
            start_symbol: start_symbol,
            out: out,
            custom: custom,
            repeatable: repeatable,
            action_module: action_module.to_string(),
        }
    }

    pub fn write_parse_mod<F>(&mut self, body: F) -> io::Result<()>
        where F: FnOnce(&mut Self) -> io::Result<()>
    {
        rust!(self.out, "");
        rust!(self.out, "mod {}parse{} {{", self.prefix, self.start_symbol);

        // these stylistic lints are annoying for the generated code,
        // which doesn't follow conventions:
        rust!(self.out,
              "#![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, \
               unused_imports)]");
        rust!(self.out, "");

        try!(self.write_uses());

        try!(body(self));

        rust!(self.out, "}}");
        Ok(())
    }

    pub fn write_uses(&mut self) -> io::Result<()> {
        try!(self.out.write_uses(&format!("{}::", self.action_module), &self.grammar));

        if self.grammar.intern_token.is_none() {
            rust!(self.out, "use {}::{}ToTriple;", self.action_module, self.prefix);
        }

        Ok(())
    }

    pub fn start_parser_fn(&mut self) -> io::Result<()> {
        let error_type = self.types.error_type();
        let parse_error_type = self.parse_error_type();

        let (type_parameters, parameters, mut where_clauses);

        if self.grammar.intern_token.is_some() {
            // if we are generating the tokenizer, we just need the
            // input, and that has already been added as one of the
            // user parameters
            type_parameters = vec![];
            parameters = vec![];
            where_clauses = vec![];
        } else {
            // otherwise, we need an iterator of type `TOKENS`
            let mut user_type_parameters = String::new();
            for type_parameter in &self.grammar.type_parameters {
                user_type_parameters.push_str(&format!("{}, ", type_parameter));
            }
            type_parameters = vec![format!("{}TOKEN: {}ToTriple<{}Error={}>",
                                           self.prefix,
                                           self.prefix,
                                           user_type_parameters,
                                           error_type),
                                   format!("{}TOKENS: IntoIterator<Item={}TOKEN>",
                                           self.prefix,
                                           self.prefix)];
            parameters = vec![format!("{}tokens0: {}TOKENS", self.prefix, self.prefix)];
            where_clauses = vec![];

            if self.repeatable {
                where_clauses.push(format!("{}TOKENS: Clone", self.prefix));
            }
        }

        try!(self.out.write_pub_fn_header(self.grammar,
                                          format!("parse_{}", self.user_start_symbol),
                                          type_parameters,
                                          parameters,
                                          format!("Result<{}, {}>",
                                                  self.types.nonterminal_type(self.start_symbol),
                                                  parse_error_type),
                                          where_clauses));
        rust!(self.out, "{{");

        Ok(())
    }

    pub fn define_tokens(&mut self) -> io::Result<()> {
        if self.grammar.intern_token.is_some() {
            // if we are generating the tokenizer, create a matcher as our input iterator
            rust!(self.out,
                  "let mut {}tokens = {}::{}intern_token::{}Matcher::new(input);",
                  self.prefix,
                  self.action_module,
                  self.prefix,
                  self.prefix);
        } else {
            // otherwise, convert one from the `IntoIterator`
            // supplied, using the `ToTriple` trait which inserts
            // errors/locations etc if none are given
            let clone_call = if self.repeatable { ".clone()" } else { "" };
            rust!(self.out,
                  "let {}tokens = {}tokens0{}.into_iter();",
                  self.prefix,
                  self.prefix,
                  clone_call);

            rust!(self.out,
                  "let mut {}tokens = {}tokens.map(|t| {}ToTriple::to_triple(t));",
                  self.prefix,
                  self.prefix,
                  self.prefix);
        }

        Ok(())
    }

    pub fn end_parser_fn(&mut self) -> io::Result<()> {
        rust!(self.out, "}}");
        Ok(())
    }

    pub fn parse_error_type(&self) -> String {
        format!("{}lalrpop_util::ParseError<{},{},{}>",
                self.prefix,
                self.types.terminal_loc_type(),
                self.types.terminal_token_type(),
                self.types.error_type())
    }
}
