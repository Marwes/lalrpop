//! Code for generating action code.
//!
//! From the outside, action fns have one of two forms. If they take
//! symbols as input, e.g. from a production like `X = Y Z => ...`
//! (which takes Y and Z as input), they have this form:
//!
//! ```
//! fn __action17<
//!     'input,                       // user-declared type parameters (*)
//! >(
//!     input: &'input str,           // user-declared parameters
//!     __0: (usize, usize, usize),   // symbols being reduced, if any
//!     ...
//!     __N: (usize, Foo, usize),     // each has a type (L, T, L)
//! ) -> Box<Expr<'input>>
//! ```
//!
//! Otherwise, they have this form:
//!
//! ```
//! fn __action17<
//!     'input,                       // user-declared type parameters (*)
//! >(
//!     input: &'input str,           // user-declared parameters
//!    __lookbehind: &usize,          // value for @R -- "end of previous token"
//!    __lookahead: &usize,           // value for @L -- "start of next token"
//! ) -> Box<Expr<'input>>
//! ```
//!
//! * -- in this case, those "user-declared" parameters are inserted by
//! the "internal tokenizer".

use grammar::repr as r;
use rust::RustWrite;
use std::io::{self, Write};

#[derive(Clone, Debug)]
pub enum Arg {
    Content(r::TypeRepr),
    Lookahead,
    Lookbehind,
}

pub struct ArgUses {
    actions: Vec<Vec<Arg>>,
}

impl ArgUses {
    pub fn arg_uses(&self, action: usize) -> &[Arg] {
        &self.actions[action]
    }

    pub fn arg_use(&self, action: usize, argument: usize) -> &Arg {
        &self.actions[action][argument]
    }

    pub fn new(grammar: &r::Grammar) -> ArgUses {
        let mut uses = ArgUses {
            actions: vec![Vec::new(); grammar.action_fn_defns.len()],
        };
        let mut done = vec![false; grammar.action_fn_defns.len()];
        loop {
            let mut all_done = true;
            for (i, defn) in grammar.action_fn_defns.iter().enumerate() {
                if done[i] {
                    continue;
                }
                match defn.kind {
                    r::ActionFnDefnKind::User(ref data) => {
                        uses.actions[i] = data
                            .arg_types
                            .iter()
                            .map(|t| Arg::Content(t.clone()))
                            .collect();
                        done[i] = true;
                    }
                    r::ActionFnDefnKind::Lookaround(ref data) => {
                        uses.actions[i] = vec![match *data {
                            r::LookaroundActionFnDefn::Lookahead => Arg::Lookahead,
                            r::LookaroundActionFnDefn::Lookbehind => Arg::Lookbehind,
                        }];

                        done[i] = true;
                    }
                    r::ActionFnDefnKind::Inline(ref data) => {
                        if !(done[data.action.index()]
                            || data.symbols.iter().all(|sym| match sym {
                                r::InlinedSymbol::Original(_) => true,
                                r::InlinedSymbol::Inlined(inlined_action, _) => {
                                    done[inlined_action.index()]
                                }
                            }))
                        {
                            all_done = false;
                            continue;
                        }
                        if i == 14 {
                            writeln!(
                                std::fs::OpenOptions::new()
                                    .create(true)
                                    .append(true)
                                    .open("../../test")
                                    .unwrap(),
                                "{:?} {:?}",
                                &data.symbols,
                                &uses.actions[data.action.index()],
                            )
                            .unwrap();
                        }

                        let mut inlined_args = uses.actions[data.action.index()].iter();
                        uses.actions[i] = data
                            .symbols
                            .iter()
                            .flat_map(|sym| match *sym {
                                r::InlinedSymbol::Original(_) => {
                                    vec![inlined_args.next().unwrap().clone()]
                                }
                                r::InlinedSymbol::Inlined(inlined_action, _) => {
                                    inlined_args.next();
                                    uses.actions[inlined_action.index()].clone()
                                }
                            })
                            .collect();
                        done[i] = true;
                    }
                }
            }
            if all_done {
                break;
            }
        }
        uses
    }
}

pub fn emit_action_code<W: Write>(
    grammar: &r::Grammar,
    uses: &ArgUses,
    rust: &mut RustWrite<W>,
) -> io::Result<()> {
    for (i, defn) in grammar.action_fn_defns.iter().enumerate() {
        rust!(rust, "");

        // we always thread the parameters through to the action code,
        // even if they are not used, and hence we need to disable the
        // unused variables lint, which otherwise gets very excited.
        if !grammar.parameters.is_empty() {
            rust!(rust, "#[allow(unused_variables)]");
        }

        match defn.kind {
            r::ActionFnDefnKind::User(ref data) => {
                emit_user_action_code(grammar, rust, i, defn, data)?
            }
            r::ActionFnDefnKind::Lookaround(ref variant) => {
                emit_lookaround_action_code(grammar, rust, i, defn, variant)?
            }
            r::ActionFnDefnKind::Inline(ref data) => {
                emit_inline_action_code(&uses, grammar, rust, i, defn, data)?
            }
        }
    }

    Ok(())
}

fn ret_type_string(grammar: &r::Grammar, defn: &r::ActionFnDefn) -> String {
    if defn.fallible {
        format!(
            "Result<{},{}lalrpop_util::ParseError<{},{},{}>>",
            defn.ret_type,
            grammar.prefix,
            grammar.types.terminal_loc_type(),
            grammar.types.terminal_token_type(),
            grammar.types.error_type()
        )
    } else {
        format!("{}", defn.ret_type)
    }
}

fn emit_user_action_code<W: Write>(
    grammar: &r::Grammar,
    rust: &mut RustWrite<W>,
    index: usize,
    defn: &r::ActionFnDefn,
    data: &r::UserActionFnDefn,
) -> io::Result<()> {
    let ret_type = ret_type_string(grammar, defn);

    // For each symbol to be reduced, we will receive
    // a (L, T, L) triple where the Ls are locations and
    // the T is the data. Ignore the locations and bind
    // the data to the name the user gave.
    let arguments: Vec<String> = data
        .arg_patterns
        .iter()
        .zip(data.arg_types.iter())
        .map(|(name, ty)| format!("{}: {}", name, ty))
        .collect();

    rust.fn_header(
        &r::Visibility::Priv,
        format!("{}action{}", grammar.prefix, index),
    )
    .with_grammar(grammar)
    .with_parameters(arguments)
    .with_return_type(ret_type)
    .emit()?;

    rust!(rust, "{{");
    rust!(rust, "{}", data.code);
    rust!(rust, "}}");
    Ok(())
}

fn emit_lookaround_action_code<W: Write>(
    grammar: &r::Grammar,
    rust: &mut RustWrite<W>,
    index: usize,
    _defn: &r::ActionFnDefn,
    data: &r::LookaroundActionFnDefn,
) -> io::Result<()> {
    rust.fn_header(
        &r::Visibility::Priv,
        format!("{}action{}", grammar.prefix, index),
    )
    .with_grammar(grammar)
    .with_parameters(vec![match *data {
        r::LookaroundActionFnDefn::Lookbehind => format!(
            "{}lookbehind: &{}",
            grammar.prefix,
            grammar.types.terminal_loc_type()
        ),
        r::LookaroundActionFnDefn::Lookahead => format!(
            "{}lookahead: &{}",
            grammar.prefix,
            grammar.types.terminal_loc_type()
        ),
    }])
    .with_return_type(format!("{}", grammar.types.terminal_loc_type()))
    .emit()?;

    rust!(rust, "{{");
    match *data {
        r::LookaroundActionFnDefn::Lookbehind => {
            // take lookbehind or supply default
            rust!(rust, "{}lookbehind.clone()", grammar.prefix);
        }
        r::LookaroundActionFnDefn::Lookahead => {
            // take the lookahead, if any; otherwise, we are
            // at EOF, so taker the lookbehind (end of last
            // pushed token); if that is missing too, then
            // supply default.
            rust!(rust, "{}lookahead.clone()", grammar.prefix);
        }
    }
    rust!(rust, "}}");
    Ok(())
}

fn emit_inline_action_code<W: Write>(
    arg_uses: &ArgUses,
    grammar: &r::Grammar,
    rust: &mut RustWrite<W>,
    index: usize,
    defn: &r::ActionFnDefn,
    data: &r::InlineActionFnDefn,
) -> io::Result<()> {
    let ret_type = ret_type_string(grammar, defn);

    let arg_types = arg_uses.arg_uses(index);

    // this is the number of symbols we expect to be passed in; it is
    // distinct from data.symbols.len(), because sometimes we have
    // inlined actions with no input symbols
    let num_flat_args = arg_types.len();

    let arguments: Vec<_> = arg_types
        .iter()
        .map(|arg| match arg {
            Arg::Content(t) => t.clone(),
            Arg::Lookahead | Arg::Lookbehind => grammar.types.terminal_loc_type(),
        })
        .enumerate()
        .map(|(i, t)| format!("{}{}: {}", grammar.prefix, i, t))
        .collect();

    if index == 14 {
        writeln!(
            std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("../../test")
                .unwrap(),
            "--- {:?} {:?} {:?} {:?}",
            &data.symbols,
            &arg_types,
            &arguments,
            &arg_uses.arg_uses(index)
        )
        .unwrap();
    }

    rust.fn_header(
        &r::Visibility::Priv,
        format!("{}action{}", grammar.prefix, index),
    )
    .with_grammar(grammar)
    .with_parameters(arguments)
    .with_return_type(ret_type)
    .emit()?;
    rust!(rust, "{{");

    // For each inlined thing, compute the start/end locations.
    // Do this first so that none of the arguments have been moved
    // yet and we can easily access their locations.
    let mut arg_counter = 0;
    let mut temp_counter = 0;
    for (arg_index, symbol) in data.symbols.iter().enumerate() {
        match *symbol {
            r::InlinedSymbol::Original(_) => {
                arg_counter += 1;
            }
            r::InlinedSymbol::Inlined(_, ref syms) => {
                if !syms.is_empty() {
                    // If we are reducing symbols, then start and end
                    // can be the start/end location of the first/last
                    // symbol respectively. Easy peezy.

                    match arg_uses.arg_use(data.action.index(), arg_index) {
                        Arg::Content(_) => (),
                        Arg::Lookahead => {
                            let last_arg_index = arg_counter + syms.len() - 1;
                            rust!(
                                rust,
                                "let {p}end{} = {p}{}.2.clone();",
                                temp_counter,
                                last_arg_index,
                                p = grammar.prefix,
                            );
                        }
                        Arg::Lookbehind => {
                            rust!(
                                rust,
                                "let {p}start{} = {p}{}.0.clone();",
                                temp_counter,
                                arg_counter,
                                p = grammar.prefix
                            );
                        }
                    };
                } else {
                    // If we have no symbols, then `arg_counter`
                    // represents index of the first symbol after this
                    // inlined item (if any), and `arg_counter-1`
                    // represents index of the symbol before this
                    // item.

                    match arg_uses.arg_use(data.action.index(), arg_index) {
                        Arg::Content(_) => (),
                        Arg::Lookahead => {
                            if arg_counter < num_flat_args {
                                rust!(
                                    rust,
                                    "let {}end{} = {}{}.0.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix,
                                    arg_counter
                                );
                            } else if num_flat_args > 0 {
                                rust!(
                                    rust,
                                    "let {}end{} = {}{}.2.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix,
                                    num_flat_args - 1
                                );
                            } else {
                                rust!(
                                    rust,
                                    "let {}end{} = {}lookahead.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix
                                );
                            }
                        }
                        Arg::Lookbehind => {
                            if arg_counter > 0 {
                                rust!(
                                    rust,
                                    "let {}start{} = {}{}.2.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix,
                                    arg_counter - 1
                                );
                            } else if num_flat_args > 0 {
                                rust!(
                                    rust,
                                    "let {}start{} = {}{}.0.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix,
                                    arg_counter
                                );
                            } else {
                                rust!(
                                    rust,
                                    "let {}start{} = {}lookbehind.clone();",
                                    grammar.prefix,
                                    temp_counter,
                                    grammar.prefix
                                );
                            }
                        }
                    }
                }

                temp_counter += 1;
                arg_counter += syms.len();
            }
        }
    }

    // Now create temporaries for the inlined things
    let mut arg_counter = 0;
    let mut temp_counter = 0;

    // if there are type parameters then type annotation is required
    let annotate = !grammar.non_lifetime_type_parameters().is_empty();
    let lparen = if annotate { "::<" } else { "(" };

    for (arg_index, symbol) in data.symbols.iter().enumerate() {
        match *symbol {
            r::InlinedSymbol::Original(_) => {
                arg_counter += 1;
            }
            r::InlinedSymbol::Inlined(inlined_action, ref syms) => {
                // execute the inlined reduce action
                rust!(
                    rust,
                    "let {}temp{} = {}action{}{}",
                    grammar.prefix,
                    temp_counter,
                    grammar.prefix,
                    inlined_action.index(),
                    lparen
                );
                for t in grammar.non_lifetime_type_parameters() {
                    rust!(rust, "{},", t);
                }
                if annotate {
                    rust!(rust, ">(");
                }
                for parameter in &grammar.parameters {
                    rust!(rust, "{},", parameter.name);
                }

                let mut i = 0;
                for arg in arg_uses.arg_uses(inlined_action.index()) {
                    match arg {
                        Arg::Content(_) => {
                            rust!(rust, "{}{},", grammar.prefix, arg_counter + i);
                            i += 1;
                        }
                        Arg::Lookbehind => {
                            if arg_counter + i == 0 {
                                rust!(rust, "{}start{},", grammar.prefix, arg_counter + i)
                            } else {
                                rust!(rust, "{}end{},", grammar.prefix, arg_counter + i - 1)
                            }
                        }
                        Arg::Lookahead => {
                            rust!(rust, "{}start{},", grammar.prefix, arg_counter + i)
                        }
                    }
                }
                rust!(rust, ");");

                let select = match arg_uses.arg_use(data.action.index(), arg_index) {
                    Arg::Content(_) => "temp",
                    Arg::Lookahead => "start",
                    Arg::Lookbehind => "end",
                };
                rust!(
                    rust,
                    "let {p}temp{c} = {p}{}{c};",
                    select,
                    c = temp_counter,
                    p = grammar.prefix,
                );

                temp_counter += 1;
                arg_counter += syms.len();
            }
        }
    }
    rust!(
        rust,
        "{}action{}{}",
        grammar.prefix,
        data.action.index(),
        lparen
    );
    for t in grammar.non_lifetime_type_parameters() {
        rust!(rust, "{},", t);
    }
    if annotate {
        rust!(rust, ">(");
    }
    for parameter in &grammar.parameters {
        rust!(rust, "{},", parameter.name);
    }
    let mut arg_counter = 0;
    let mut temp_counter = 0;
    for symbol in &data.symbols {
        match *symbol {
            r::InlinedSymbol::Original(_) => {
                rust!(rust, "{}{},", grammar.prefix, arg_counter);
                arg_counter += 1;
            }
            r::InlinedSymbol::Inlined(_, ref syms) => {
                rust!(rust, "{}temp{},", grammar.prefix, temp_counter);
                temp_counter += 1;
                arg_counter += syms.len();
            }
        }
    }
    assert!(!data.symbols.is_empty());
    rust!(rust, ")");

    rust!(rust, "}}");
    Ok(())
}
