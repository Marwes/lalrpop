//! A compiler from an LR(1) table to a traditional table driven parser.

use collections::{Map, Set};
use grammar::repr::*;
use lr1::core::*;
use lr1::lookahead::Token;
use rust::RustWrite;
use std::io::{self, Write};
use tls::Tls;
use util::{Escape, Sep};

use super::base::CodeGenerator;

const DEBUG_PRINT: bool = false;

pub fn compile<'grammar, W: Write>(grammar: &'grammar Grammar,
                                   user_start_symbol: NonterminalString,
                                   start_symbol: NonterminalString,
                                   states: &[LR1State<'grammar>],
                                   action_module: &str,
                                   out: &mut RustWrite<W>)
                                   -> io::Result<()> {
    let mut table_driven = CodeGenerator::new_table_driven(grammar,
                                                           user_start_symbol,
                                                           start_symbol,
                                                           states,
                                                           action_module,
                                                           out);
    table_driven.write()
}

// We create three parse tables:
//
// - `ACTION[state * num_states + terminal]: i32`: given a state and next token,
//   yields an integer indicating whether to shift/reduce (see below)
// - `EOF_ACTION[state]: i32`: as above, but for the EOF token
// - `GOTO[state * num_states + nonterminal]: i32`: index + 1 of state to jump to when given
//   nonterminal is pushed (no error is possible)
//
// For the `ACTION` and `EOF_ACTION` tables, the value is an `i32` and
// its interpretation varies depending on whether it is positive or
// negative:
//
// - if zero, parse error.
// - if a positive integer (not zero), it is the next state to shift to.
// - if a negative integer (not zero), it is the index of a reduction
//   action to execute (actually index + 1).
//
// We maintain two stacks: one is a stack of state indexes (each an
// u32). The other is a stack of values and spans: `(L, T, L)`. `L` is
// the location type and represents the start/end span. `T` is the
// value of the symbol. The type `T` is an `enum` that we synthesize
// which contains a variant for all the possibilities:
//
// ```
// enum Value<> {
//     // One variant for each terminal:
//     Term1(Ty1),
//     ...
//     TermN(TyN),
//
//     // One variant for each nonterminal:
//     Nt1(Ty1),
//     ...
//     NtN(TyN),
// }
// ```
//
// The action parser function looks like this (pseudo-code):
//
// ```
// fn parse_fn<TOKENS>(tokens: TOKENS) -> Result<T, Error>
//    where TOKENS: Iterator<Item=Result<(Location, Token, Location), Error>>
// {
//    let mut states = vec![0]; // initial state is zero
//    let mut symbols = vec![];
//    'shift: loop {
//        // Code to shift the next symbol and determine which terminal
//        // it is; emitted by `shift_symbol()`.
//        let lookahead = match tokens.next() {
//            Some(Ok(l)) => l,
//            None => break 'shift,
//            Some(Err(e)) => return Err(e),
//        };
//        let integer = match lookahead {
//            (_, PatternForTerminal0(...), _) => 0,
//            ...
//        };
//
//        // Code to process next symbol.
//        loop {
//            let symbol = match lookahead {
//               (l, PatternForTerminal0(...), r) => {
//                   (l, Value::VariantForTerminal0(...), r),
//               }
//               ...
//            };
//            let state = *states.last().unwrap() as usize;
//            let action = ACTION[state * NUM_STATES + integer];
//            if action > 0 { // shift
//                states.push(action - 1);
//                symbols.push(symbol);
//                continue 'shift;
//            } else if action < 0 { // reduce
//                try!(reduce(action, Some(&lookahead.0), &mut states, &mut symbols));
//            } else {
//                return Err(...);
//            }
//        }
//    }
//
//    // Process EOF
//    while let Some(state) = self.states.pop() {
//        let action = EOF_ACTION[state * NUM_STATES];
//        if action < 0 { // reduce
//            try!(reduce(action, None, &mut states, &mut symbols));
//        } else {
//            return Err(...);
//        }
//    }
// }
//
// // generated by `emit_reduce_actions()`
// fn reduce(action: i32, lookahead: Option<&(L, Tok, L)>,
//           states: &mut Vec<i32>, symbols: &mut Vec<(L, Symbol, L)) {
//     let nonterminal = match -(action + 1) {
//        0 => {
//            // execute reduce action 0 to produce nonterminal N, popping from stacks etc
//            states.pop(); // however many times
//            symbols.pop(); // however many times
//            let data = action_fn0(...);
//            symbols.push((l, Value::VariantForNonterminalN(data), r));
//            N
//        }
//        ...
//     };
//     let state = *states.last().unwrap();
//     let next_state = GOTO[state * NUM_STATES + nonterminal];
//     state_stack.push(next_state - 1);
// }
// ```

struct TableDriven<'grammar> {
    /// type parameters for the `Nonterminal` type
    symbol_type_params: Vec<TypeParameter>,

    /// a list of each nonterminal in some specific order
    all_nonterminals: Vec<NonterminalString>,

    reduce_indices: Map<&'grammar Production, usize>,
}

impl<'ascent, 'grammar, W: Write> CodeGenerator<'ascent, 'grammar, W, TableDriven<'grammar>> {
    fn new_table_driven(grammar: &'grammar Grammar,
                        user_start_symbol: NonterminalString,
                        start_symbol: NonterminalString,
                        states: &'ascent [LR1State<'grammar>],
                        action_module: &str,
                        out: &'ascent mut RustWrite<W>)
                        -> Self {
        // The nonterminal type needs to be parameterized by all the
        // type parameters that actually appear in the types of
        // nonterminals.  We can't just use *all* type parameters
        // because that would leave unused lifetime/type parameters in
        // some cases.
        let referenced_ty_params: Set<TypeParameter> = grammar.types
                                                              .nonterminal_types()
                                                              .into_iter()
                                                              .chain(grammar.types
                                                                            .terminal_types())
                                                              .flat_map(|t| t.referenced())
                                                              .collect();

        let symbol_type_params: Vec<_> = grammar.type_parameters
                                                .iter()
                                                .filter(|t| referenced_ty_params.contains(t))
                                                .cloned()
                                                .collect();

        // Assign each production a unique index to use as the values for reduce
        // actions in the ACTION and EOF_ACTION tables.
        let reduce_indices: Map<&'grammar Production, usize> = grammar.nonterminals
                                                                      .values()
                                                                      .flat_map(|nt| {
                                                                          &nt.productions
                                                                      })
                                                                      .zip(0..)
                                                                      .collect();

        CodeGenerator::new(grammar,
                           user_start_symbol,
                           start_symbol,
                           states,
                           out,
                           false,
                           action_module,
                           TableDriven {
                               symbol_type_params: symbol_type_params,
                               all_nonterminals: grammar.nonterminals
                                                        .keys()
                                                        .cloned()
                                                        .collect(),
                               reduce_indices: reduce_indices,
                           })
    }

    fn write(&mut self) -> io::Result<()> {
        self.write_parse_mod(|this| {
            try!(this.write_value_type_defn());
            try!(this.write_parse_table());
            try!(this.write_parser_fn());
            try!(this.emit_reduce_actions());
            try!(this.emit_downcast_fns());
            Ok(())
        })
    }

    fn write_value_type_defn(&mut self) -> io::Result<()> {
        // sometimes some of the variants are not used, particularly
        // if we are generating multiple parsers from the same file:
        rust!(self.out, "#[allow(dead_code)]");
        rust!(self.out,
              "pub enum {}Symbol<{}> {{",
              self.prefix,
              Sep(", ", &self.custom.symbol_type_params));

        // make one variant per terminal
        for &term in &self.grammar.terminals.all {
            let name = self.variant_name_for_symbol(Symbol::Terminal(term));
            let ty = self.types.terminal_type(term).clone();
            rust!(self.out, "{}({}),", name, ty);
        }

        // make one variant per nonterminal
        for &nt in self.grammar.nonterminals.keys() {
            let name = self.variant_name_for_symbol(Symbol::Nonterminal(nt));
            let ty = self.types.nonterminal_type(nt).clone();
            rust!(self.out, "{}({}),", name, ty);
        }

        let error_type = self.types.parse_error_type();
        rust!(self.out, "Error({})", error_type);

        rust!(self.out, "}}");
        Ok(())
    }

    fn write_parse_table(&mut self) -> io::Result<()> {
        // The table is a two-dimensional matrix indexed first by state
        // and then by the terminal index. The value is described above.
        rust!(self.out, "const {}ACTION: &'static [i32] = &[", self.prefix);

        for (index, state) in self.states.iter().enumerate() {
            rust!(self.out, "// State {}", index);

            if Tls::session().emit_comments {
                for item in state.items.vec.iter() {
                    rust!(self.out, "//     {:?}", item);
                }
            }

            // Write an action for each terminal (either shift, reduce, or error).
            for &terminal in &self.grammar.terminals.all {
                if let Some(new_state) = state.shifts.get(&terminal) {
                    rust!(self.out,
                          "{}, // on {}, goto {}",
                          new_state.0 + 1,
                          terminal,
                          new_state.0);
                } else {
                    try!(self.write_reduction(state, Token::Terminal(terminal)));
                }
            }
        }

        rust!(self.out, "];");

        // Only need the ERROR table if error recovery is used
        if self.states.iter().any(|state| state.error.is_some()) {
            // Error transitions which are positive integers for states that can be recovered from and zero
            // otherwise
            rust!(self.out, "const {}ERRORS: &'static [i32] = &[", self.prefix);

            for (index, state) in self.states.iter().enumerate() {
                rust!(self.out, "// State {}", index);

                if let Some(ref new_state) = state.error {
                    rust!(self.out,
                            "{}, // goto {}",
                            new_state.0 + 1,
                            new_state.0);
                } else {
                    rust!(self.out, "0, // error");
                }
            }

            rust!(self.out, "];");
        }

        // Actions on EOF. Indexed just by state.
        rust!(self.out,
              "const {}EOF_ACTION: &'static [i32] = &[",
              self.prefix);
        for state in self.states {
            try!(self.write_reduction(state, Token::EOF));
        }
        rust!(self.out, "];");

        // The goto table is indexed by state and *nonterminal*.
        rust!(self.out, "const {}GOTO: &'static [i32] = &[", self.prefix);
        for (index, state) in self.states.iter().enumerate() {
            rust!(self.out, "// State {}", index);
            for nonterminal in self.grammar.nonterminals.keys() {
                if let Some(&new_state) = state.gotos.get(nonterminal) {
                    rust!(self.out,
                          "{}, // on {}, goto {}",
                          new_state.0 + 1,
                          nonterminal,
                          new_state.0);
                } else {
                    rust!(self.out, "0, // on {}, error", nonterminal);
                }
            }
        }
        rust!(self.out, "];");

        Ok(())
    }

    fn write_reduction(&mut self, state: &LR1State, token: Token) -> io::Result<()> {
        let reduction = state.reductions
                             .iter()
                             .filter(|&&(ref t, _)| t.contains(token))
                             .map(|&(_, p)| p)
                             .next();
        if let Some(production) = reduction {
            let action = self.custom.reduce_indices[production];
            rust!(self.out,
                  "-{}, // on {}, reduce `{:?}`",
                  action + 1,
                  token,
                  production);
        } else {
            // Otherwise, this is an error. Store 0.
            rust!(self.out, "0, // on {}, error", token);
        }
        Ok(())
    }

    fn write_parser_fn(&mut self) -> io::Result<()> {
        let phantom_data_expr = self.phantom_data_expr();

        try!(self.start_parser_fn());

        try!(self.define_tokens());

        // State and data stack.
        rust!(self.out, "let mut {}states = vec![0_i32];", self.prefix);
        rust!(self.out, "let mut {}symbols = vec![];", self.prefix);
        rust!(self.out, "let mut {}last_location = Default::default();", self.prefix);

        // Outer loop: each time we continue around this loop, we
        // shift a new token from the input. We break from the loop
        // when the end of the input is reached (we return early if an
        // error occurs).
        rust!(self.out, "'{}shift: loop {{", self.prefix);
        if DEBUG_PRINT {
            rust!(self.out, "println!(\"outer loop\");");
        }

        // Read next token from input; defines `integer` and `symbol`.
        try!(self.next_token());
        try!(self.token_to_integer(false));

        // Loop.
        rust!(self.out, "loop {{");
        if DEBUG_PRINT {
            rust!(self.out, "println!(\"inner loop\");");
        }
        rust!(self.out,
              "let {}state = *{}states.last().unwrap() as usize;",
              self.prefix,
              self.prefix);

        // Load the next action to take.
        rust!(self.out,
              "let {}action = {}ACTION[{}state * {} + {}integer];",
              self.prefix,
              self.prefix,
              self.prefix,
              self.grammar.terminals.all.len(),
              self.prefix);

        if DEBUG_PRINT {
            rust!(self.out,
                  "println!(\"state: {{}} lookahead: {{}} action: {{}} stack-depth: {{}}\", \
                   {}state, {}integer, {}action, {}symbols.len());",
                  self.prefix,
                  self.prefix,
                  self.prefix,
                  self.prefix);
        }

        // Shift.
        rust!(self.out, "if {}action > 0 {{", self.prefix);
        if DEBUG_PRINT {
            rust!(self.out, "println!(\"--> shift\");");
        }
        try!(self.token_to_symbol());
        rust!(self.out,
              "{}states.push({}action - 1);",
              self.prefix,
              self.prefix);
        rust!(self.out,
              "{}symbols.push(({}lookahead.0, {}symbol, {}lookahead.2));",
              self.prefix,
              self.prefix,
              self.prefix,
              self.prefix);
        rust!(self.out, "continue '{}shift;", self.prefix);

        // Reduce.
        rust!(self.out, "}} else if {}action < 0 {{", self.prefix);
        if DEBUG_PRINT {
            rust!(self.out, "println!(\"--> reduce\");");
        }
        rust!(self.out,
              "if let Some(r) = {}reduce({}{}action, Some(&{}lookahead.0), &mut {}states, &mut \
               {}symbols, {}) {{",
              self.prefix,
              self.grammar.user_parameter_refs(),
              self.prefix,
              self.prefix,
              self.prefix,
              self.prefix,
              phantom_data_expr);
        rust!(self.out, "return r;");
        rust!(self.out, "}}");

        // Error.
        rust!(self.out, "}} else {{");

        rust!(self.out,
              "let {}error = {}lalrpop_util::ParseError::UnrecognizedToken {{",
              self.prefix,
              self.prefix);
        rust!(self.out, "token: Some({}lookahead),", self.prefix);
        rust!(self.out, "expected: vec![],");
        rust!(self.out, "}};");

        if self.states.iter().any(|state| state.error.is_some()) {
            try!(self.error_recovery());
        } else {
            rust!(self.out, "return Err({}error);", self.prefix);
        }

        rust!(self.out, "}}"); // if-else-if-else

        rust!(self.out, "}}"); // reduce loop

        rust!(self.out, "}}"); // shift loop

        // EOF loop
        rust!(self.out, "loop {{");
        rust!(self.out,
              "let {}state = *{}states.last().unwrap() as usize;",
              self.prefix,
              self.prefix);
        if DEBUG_PRINT {
            rust!(self.out,
                  "println!(\"EOF loop state: {{}}\", {}state);",
                  self.prefix);
        }
        rust!(self.out,
              "let {}action = {}EOF_ACTION[{}state];",
              self.prefix,
              self.prefix,
              self.prefix);
        if DEBUG_PRINT {
            rust!(self.out,
                  "println!(\"EOF in state {{}} takes action {{}}\", {}state, {}action);",
                  self.prefix,
                  self.prefix);
        }
        rust!(self.out, "if {}action < 0 {{", self.prefix);
        rust!(self.out,
              "if let Some(r) = {}reduce({}{}action, None, &mut {}states, &mut {}symbols, {}) {{",
              self.prefix,
              self.grammar.user_parameter_refs(),
              self.prefix,
              self.prefix,
              self.prefix,
              phantom_data_expr);
        rust!(self.out, "return r;");
        rust!(self.out, "}}");
        rust!(self.out, "}} else {{");

        // EOF error recovery
        rust!(self.out,
              "let {}error = {}lalrpop_util::ParseError::UnrecognizedToken {{",
              self.prefix,
              self.prefix);
        rust!(self.out, "token: None,");
        rust!(self.out, "expected: vec![],");
        rust!(self.out, "}};");

        if self.states.iter().any(|state| state.error.is_some()) {
            rust!(self.out, "let {}original_state_len = {}states.len();",
                self.prefix,
                self.prefix);
            
            // Loop which pops states until a state that can be recovered from is found
            rust!(self.out, "loop {{");
            rust!(self.out, "match {}states.last().cloned() {{", self.prefix);

            rust!(self.out, "Some({}state) => {{", self.prefix);
            rust!(self.out, "let {}error_state = {}ERRORS[{}state as usize];",
                self.prefix,
                self.prefix,
                self.prefix);

            rust!(self.out, "if {}error_state != 0 {{", self.prefix);
            rust!(self.out, "if {}EOF_ACTION[({}error_state as usize - 1)] == 0 {{",
                self.prefix,
                self.prefix);
            rust!(self.out, "return Err({}error);", self.prefix);
            rust!(self.out, "}}");
            rust!(self.out, "let {}new_len = {}symbols.len() - ({}original_state_len - {}states.len());",
                self.prefix,
                self.prefix,
                self.prefix,
                self.prefix);
            rust!(self.out, "{}symbols.truncate({}new_len);",
                self.prefix,
                self.prefix);
            rust!(self.out, "{}states.push({}error_state - 1);",
                self.prefix,
                self.prefix);
            rust!(self.out,
                "{}symbols.push(({}last_location.clone(), {}Symbol::Error({}error), {}last_location.clone()));",
                self.prefix,
                self.prefix,
                self.prefix,
                self.prefix,
                self.prefix);
            rust!(self.out, "break;");
            rust!(self.out, "}}");
            rust!(self.out, "{}states.pop();", self.prefix);
            rust!(self.out, "}}"); // Some
            
            rust!(self.out, "None => {{");
            rust!(self.out,
                "return Err({}error);",
                self.prefix);
            rust!(self.out, "}}"); // None

            rust!(self.out, "}}"); // match
            rust!(self.out, "}}"); // loop
        } else {
            rust!(self.out, "return Err({}error);", self.prefix);
        }

        rust!(self.out, "}}"); // else

        rust!(self.out, "}}"); // while let

        self.end_parser_fn()
    }

    fn peek_token(&mut self) -> io::Result<()> {
        rust!(self.out,
              "let {}lookahead = match {}tokens.peek() {{",
              self.prefix,
              self.prefix);
        rust!(self.out, "Some(&Ok(ref v)) => v,");
        rust!(self.out, "_ => continue '{}shift,", self.prefix); // EOF: break out
        rust!(self.out, "}};");
        Ok(())
    }

    fn next_token(&mut self) -> io::Result<()> {
        rust!(self.out,
              "let {}lookahead = match {}tokens.next() {{",
              self.prefix,
              self.prefix);
        rust!(self.out, "Some(Ok(v)) => v,");
        rust!(self.out, "None => break '{}shift,", self.prefix); // EOF: break out
        if self.grammar.intern_token.is_some() {
            // when we generate the tokenizer, the generated errors are `ParseError` values
            rust!(self.out, "Some(Err(e)) => return Err(e),");
        } else {
            // otherwise, they are user errors
            rust!(self.out,
                  "Some(Err(e)) => return Err({}lalrpop_util::ParseError::User {{ error: e }}),",
                  self.prefix);
        }
        rust!(self.out, "}};");
        rust!(self.out, "{}last_location = {}lookahead.2.clone();",
              self.prefix,
              self.prefix);
        Ok(())
    }

    fn token_to_integer(&mut self, peek: bool) -> io::Result<()> {
        rust!(self.out,
              "let {}integer = match {}lookahead.1 {{",
              self.prefix,
              self.prefix);
        for (&terminal, index) in self.grammar.terminals.all.iter().zip(0..) {
            let pattern = self.grammar.pattern(terminal).map(&mut |_| "_");
            rust!(self.out, "{} if true => {},", pattern, index);
        }

        rust!(self.out, "_ => {{");
        rust!(self.out,
              "return Err({}lalrpop_util::ParseError::UnrecognizedToken {{",
              self.prefix);
        if peek {
            rust!(self.out, "token: None,");
        } else {
            rust!(self.out, "token: Some({}lookahead),", self.prefix);
        }
        rust!(self.out, "expected: vec![],");
        rust!(self.out, "}});");
        rust!(self.out, "}}");

        rust!(self.out, "}};");
        Ok(())
    }

    fn token_to_symbol(&mut self) -> io::Result<()> {
        rust!(self.out,
              "let {}symbol = match {}integer {{",
              self.prefix,
              self.prefix);
        for (&terminal, index) in self.grammar.terminals.all.iter().zip(0..) {
            rust!(self.out, "{} => match {}lookahead.1 {{", index, self.prefix);

            let mut pattern_names = vec![];
            let pattern = self.grammar.pattern(terminal).map(&mut |_| {
                let index = pattern_names.len();
                pattern_names.push(format!("{}tok{}", self.prefix, index));
                pattern_names.last().cloned().unwrap()
            });

            let mut pattern = format!("{}", pattern);
            if pattern_names.is_empty() {
                pattern_names.push(format!("{}tok", self.prefix));
                pattern = format!("{}tok @ {}", self.prefix, pattern);
            }

            let variant_name = self.variant_name_for_symbol(Symbol::Terminal(terminal));
            rust!(self.out,
                  "{} => {}Symbol::{}({}),",
                  pattern,
                  self.prefix,
                  variant_name,
                  pattern_names.join(", "));
            rust!(self.out, "_ => unreachable!(),");
            rust!(self.out, "}},");
        }

        rust!(self.out, "_ => unreachable!(),");

        rust!(self.out, "}};");
        Ok(())
    }

    fn emit_reduce_actions(&mut self) -> io::Result<()> {
        let success_type = self.types.nonterminal_type(self.start_symbol);
        let parse_error_type = self.types.parse_error_type();
        let loc_type = self.types.terminal_loc_type();
        let spanned_symbol_type = self.spanned_symbol_type();

        let parameters = vec![format!("{}action: i32", self.prefix),
                              format!("{}lookahead_start: Option<&{}>", self.prefix, loc_type),
                              format!("{}states: &mut ::std::vec::Vec<i32>", self.prefix),
                              format!("{}symbols: &mut ::std::vec::Vec<{}>",
                                      self.prefix,
                                      spanned_symbol_type),
                              format!("_: {}", self.phantom_data_type())];

        try!(self.out.write_pub_fn_header(self.grammar,
                                          format!("{}reduce", self.prefix),
                                          vec![],
                                          parameters,
                                          format!("Option<Result<{},{}>>",
                                                  success_type,
                                                  parse_error_type),
                                          vec![]));
        rust!(self.out, "{{");

        rust!(self.out,
              "let {}nonterminal = match -{}action {{",
              self.prefix,
              self.prefix);
        for (production, index) in self.grammar
                                       .nonterminals
                                       .values()
                                       .flat_map(|nt| &nt.productions)
                                       .zip(1..) {

            rust!(self.out, "{} => {{", index);
            try!(self.emit_reduce_action(production));
            rust!(self.out, "}}");
        }
        rust!(self.out,
              "_ => panic!(\"invalid action code {{}}\", {}action)",
              self.prefix);
        rust!(self.out, "}};");
        rust!(self.out,
              "let {}state = *{}states.last().unwrap() as usize;",
              self.prefix,
              self.prefix);
        rust!(self.out,
              "let {}next_state = {}GOTO[{}state * {} + {}nonterminal] - 1;",
              self.prefix,
              self.prefix,
              self.prefix,
              self.grammar.nonterminals.len(),
              self.prefix);
        if DEBUG_PRINT {
            rust!(self.out,
                  "println!(\"goto state {{}} from {{}} due to nonterminal {{}}\", {}next_state, \
                   {}state, {}nonterminal);",
                  self.prefix,
                  self.prefix,
                  self.prefix);
        }
        rust!(self.out,
              "{}states.push({}next_state);",
              self.prefix,
              self.prefix);
        rust!(self.out, "None");
        rust!(self.out, "}}");
        Ok(())
    }

    fn emit_reduce_action(&mut self, production: &Production) -> io::Result<()> {
        rust!(self.out, "// {:?}", production);

        // Pop each of the symbols and their associated states.
        for (index, &symbol) in production.symbols.iter().enumerate().rev() {
            let name = self.variant_name_for_symbol(symbol);
            rust!(self.out,
                "let {}sym{} = {}pop_{}({}symbols);",
                self.prefix,
                index,
                self.prefix,
                name,
                self.prefix);
        }
        let transfer_syms: Vec<_> = (0..production.symbols.len())
                                        .map(|i| format!("{}sym{}", self.prefix, i))
                                        .collect();

        // Execute the action fn
        // identify the "start" location for this production; this
        // is typically the start of the first symbol we are
        // reducing; but in the case of an empty production, it
        // will be the last symbol pushed, or at worst `default`.
        if let Some(first_sym) = transfer_syms.first() {
            rust!(self.out,
                  "let {}start = {}.0.clone();",
                  self.prefix,
                  first_sym);
        } else {
            // we pop no symbols, so grab from the top of the stack
            // (unless we are in the start state, in which case the
            // stack will be empty)
            rust!(self.out,
                  "let {}start = {}symbols.last().map(|s| s.2.clone()).unwrap_or_default();",
                  self.prefix,
                  self.prefix);
        }

        // identify the "end" location for this production;
        // this is typically the end of the last symbol we are reducing,
        // but in the case of an empty production it will come from the
        // lookahead
        if let Some(last_sym) = transfer_syms.last() {
            rust!(self.out, "let {}end = {}.2.clone();", self.prefix, last_sym);
        } else {
            rust!(self.out,
                  "let {}end = {}lookahead_start.cloned().unwrap_or_else(|| \
                   {}start.clone());",
                  self.prefix,
                  self.prefix,
                  self.prefix);
        }

        let transfered_syms = transfer_syms.len();

        let mut args = transfer_syms;
        if transfered_syms == 0 {
            args.push(format!("&{}start", self.prefix));
            args.push(format!("&{}end", self.prefix));
        }

        // invoke the action code
        let is_fallible = self.grammar.action_is_fallible(production.action);
        if is_fallible {
            rust!(self.out,
                  "let {}nt = match {}::{}action{}::<{}>({}{}) {{",
                  self.prefix,
                  self.action_module,
                  self.prefix,
                  production.action.index(),
                  Sep(", ", &self.grammar.non_lifetime_type_parameters()),
                  self.grammar.user_parameter_refs(),
                  Sep(", ", &args));
            rust!(self.out, "Ok(v) => v,");
            rust!(self.out, "Err(e) => return Some(Err(e)),");
            rust!(self.out, "}};");
        } else {
            rust!(self.out,
                  "let {}nt = {}::{}action{}::<{}>({}{});",
                  self.prefix,
                  self.action_module,
                  self.prefix,
                  production.action.index(),
                  Sep(", ", &self.grammar.non_lifetime_type_parameters()),
                  self.grammar.user_parameter_refs(),
                  Sep(", ", &args));
        }

        // if this is the final state, return it
        if production.nonterminal == self.start_symbol {
            rust!(self.out, "return Some(Ok({}nt));", self.prefix);
            return Ok(());
        }

        // pop the consumed states from the stack
        rust!(self.out,
              "let {}states_len = {}states.len();",
              self.prefix,
              self.prefix);
        rust!(self.out,
              "{}states.truncate({}states_len - {});",
              self.prefix,
              self.prefix,
              production.symbols.len());

        // push the produced value on the stack
        let name = self.variant_name_for_symbol(Symbol::Nonterminal(production.nonterminal));
        rust!(self.out,
              "{}symbols.push(({}start, {}Symbol::{}({}nt), {}end));",
              self.prefix,
              self.prefix,
              self.prefix,
              name,
              self.prefix,
              self.prefix);

        // produce the index that we will use to extract the next state
        // from GOTO array
        let index = self.custom
                        .all_nonterminals
                        .iter()
                        .position(|&x| x == production.nonterminal)
                        .unwrap();
        rust!(self.out, "{}", index);

        Ok(())
    }

    fn variant_name_for_symbol(&mut self, s: Symbol) -> String {
        match s {
            Symbol::Nonterminal(nt) => format!("Nt{}", Escape(nt)),
            Symbol::Terminal(t) => format!("Term{}", Escape(t)),
            Symbol::Error => format!("Error")
        }
    }

    fn emit_downcast_fns(&mut self) -> io::Result<()> {
        for &term in &self.grammar.terminals.all {
            let name = self.variant_name_for_symbol(Symbol::Terminal(term));
            let ty = self.types.terminal_type(term).clone();
            try!(self.emit_downcast_fn(&name, ty));
        }

        for &nt in self.grammar.nonterminals.keys() {
            let name = self.variant_name_for_symbol(Symbol::Nonterminal(nt));
            let ty = self.types.nonterminal_type(nt).clone();
            try!(self.emit_downcast_fn(&name, ty));
        }

        // Generate an extra variant for storing the error for `error` recovery
        let error_type = self.types.parse_error_type().clone();
        try!(self.emit_downcast_fn("Error", error_type));

        Ok(())
    }

    fn emit_downcast_fn(&mut self, variant_name: &str, variant_ty: TypeRepr) -> io::Result<()> {
        let spanned_symbol_type = self.spanned_symbol_type();

        rust!(self.out, "fn {}pop_{}<", self.prefix, variant_name);
        for type_parameter in &self.custom.symbol_type_params {
            rust!(self.out, "  {},", type_parameter);
        }
        rust!(self.out, ">(");
        rust!(self.out,
              "{}symbols: &mut ::std::vec::Vec<{}>",
              self.prefix,
              spanned_symbol_type);
        rust!(self.out, ") -> {} {{", self.types.spanned_type(variant_ty));

        if DEBUG_PRINT {
            rust!(self.out, "println!(\"pop_{}\");", variant_name);
        }
        rust!(self.out, "match {}symbols.pop().unwrap() {{", self.prefix);
        rust!(self.out,
              "({}l, {}Symbol::{}({}v), {}r) => ({}l, {}v, {}r),",
              self.prefix,
              self.prefix,
              variant_name,
              self.prefix,
              self.prefix,
              self.prefix,
              self.prefix,
              self.prefix);
        rust!(self.out, "_ => panic!(\"symbol type mismatch\")");
        rust!(self.out, "}}");

        rust!(self.out, "}}");

        Ok(())
    }

    fn error_recovery(&mut self) -> io::Result<()> {
        rust!(self.out, "let {}original_state_len = {}states.len();",
            self.prefix,
            self.prefix);

        // Loop which pops states until a state that can be recovered from is found
        rust!(self.out, "loop {{");
        rust!(self.out, "match {}states.last().cloned() {{", self.prefix);
        rust!(self.out, "Some({}state) => {{", self.prefix);
        rust!(self.out, "let {}error_state = {}ERRORS[{}state as usize];",
            self.prefix,
            self.prefix,
            self.prefix);

        rust!(self.out, "if {}error_state != 0 {{", self.prefix);

        if DEBUG_PRINT {
            rust!(self.out, "println!(\"Attempting to recover on state: {{}}, error_state: {{}}, symbols: {{}}\", {}state, {}error_state - 1, {}symbols.len());",
                self.prefix,
                self.prefix,
                self.prefix);
        }
        
        // Loop which drops tokens until parsing can resume again
        rust!(self.out, "loop {{");
        rust!(self.out, "let ({}start, {}integer, {}end) = {{",
            self.prefix,
            self.prefix,
            self.prefix);
        try!(self.peek_token());
        try!(self.token_to_integer(true));
        rust!(self.out, "({}lookahead.0.clone(), {}integer, {}lookahead.2.clone())",
            self.prefix,
            self.prefix,
            self.prefix);
        rust!(self.out, "}};");

        rust!(self.out, "if {}ACTION[({}error_state as usize - 1) * {} + {}integer] != 0 {{",
            self.prefix,
            self.prefix,
            self.grammar.terminals.all.len(),
            self.prefix);
        rust!(self.out, "let {}new_len = {}symbols.len() - ({}original_state_len - {}states.len());",
            self.prefix,
            self.prefix,
            self.prefix,
            self.prefix);
        rust!(self.out, "{}symbols.truncate({}new_len);",
            self.prefix,
            self.prefix);
        rust!(self.out, "{}states.push({}error_state - 1);",
            self.prefix,
            self.prefix);
        rust!(self.out,
            "{}symbols.push(({}start, {}Symbol::Error({}error), {}end));",
            self.prefix,
            self.prefix,
            self.prefix,
            self.prefix,
            self.prefix);
        
        if DEBUG_PRINT {
            rust!(self.out, "println!(\"Recovering on state: {{}}, lookahead: {{}}, symbols: {{}}\", {}error_state - 1, {}integer, {}symbols.len());",
                self.prefix,
                self.prefix,
                self.prefix);
        }
        
        rust!(self.out, "continue '{}shift;", self.prefix);
        rust!(self.out, "}}");// if ACTION

        rust!(self.out, "{}tokens.next();", self.prefix);

        if DEBUG_PRINT {
            rust!(self.out, "println!(\"Skipping token: {{}}\", {}integer);",
                self.prefix);
        }
        
        rust!(self.out, "}}"); // loop
        rust!(self.out, "}}");
        rust!(self.out, "{}states.pop();", self.prefix);
        rust!(self.out, "}}"); // Some 
        rust!(self.out, "None => {{");
        rust!(self.out,
            "return Err({}error);",
            self.prefix);
        rust!(self.out, "}}");
        rust!(self.out, "}}"); // match

        if DEBUG_PRINT {
            rust!(self.out, "println!(\"Dropping state: {{}}\", {}state);",
                self.prefix);
        }
        
        rust!(self.out, "}}"); // loop

        Ok(())
    }

    fn symbol_type(&self) -> String {
        format!("{}Symbol<{}>",
                self.prefix,
                Sep(", ", &self.custom.symbol_type_params))
    }

    fn spanned_symbol_type(&self) -> String {
        let loc_type = self.types.terminal_loc_type();
        format!("({},{},{})", loc_type, self.symbol_type(), loc_type)
    }
}
