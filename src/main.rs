use rustyline::{ error::ReadlineError, Editor, config::Configurer };
use std::collections::HashMap;

mod evaluate;
mod parse;
mod print;

fn main() {
    let mut reader = Editor::<()>::new();
    reader.set_auto_add_history(true);
    let mut bindings = HashMap::new();

    loop { match reader.readline("λ : ") {
        Ok(line) => {
            match parse::statement(&line, &mut bindings) {
                Ok(parse::Action::Evaluate(expr)) => {
                    let expr = evaluate::evaluate(expr);
                    println!("  > {}", print::pretty_printer(expr, &bindings));
                }
                Ok(parse::Action::Bind(ident, expr)) => {
                    let expr = evaluate::evaluate(expr);
                    println!("  > {}", print::pretty_printer(expr.clone(), &bindings));
                    println!("Bound to {}", ident);
                    bindings.insert(ident, expr);
                }
                Ok(parse::Action::TestAlphaEquivalence(left, right)) => {
                    let left = evaluate::evaluate(left);
                    println!("  > {}", print::pretty_printer(left.clone(), &bindings));
                    let right = evaluate::evaluate(right);
                    println!("  > {}", print::pretty_printer(right.clone(), &bindings));
                    println!("α > {}", left == right);
                }
                Ok(parse::Action::DoNothing) => {}
                Err(msg) => println!("Error: {}", msg)
            }
        }
        Err(ReadlineError::Interrupted) => break,
        Err(ReadlineError::Eof) => break,
        Err(e @ _) => {
            println!("An error occured while reading input: {}", e);
            break
        }
    }}
}

type Bindings = HashMap<String, LambdaTerm>;

#[derive(Clone, Eq, Hash, Debug)]
pub enum LambdaTerm {
    Abstract(String, Box<LambdaTerm>),
    Apply(Box<LambdaTerm>, Box<LambdaTerm>),
    Symbol(String)
}

impl PartialEq for LambdaTerm {
    fn eq(&self, other: &LambdaTerm) -> bool {
        alpha_equivalent(self, other, &mut Vec::new())
    }
}

fn alpha_equivalent<'a>(
    lhs: &'a LambdaTerm, rhs: &'a LambdaTerm, name_pairs: &mut Vec<(&'a str, &'a str)>
) -> bool {
    match (lhs, rhs) {
        (LambdaTerm::Symbol(ls), LambdaTerm::Symbol(rs)) => {
            for (ls_name, rs_name) in name_pairs.iter().rev() {
                if ls == ls_name && rs == rs_name {
                    return true
                } else if ls == ls_name || rs == rs_name {
                    return false
                }
            }
            unreachable!()
        }
        (LambdaTerm::Abstract(ls, lb), LambdaTerm::Abstract(rs, rb)) => {
            name_pairs.push((ls, rs));
            let result = alpha_equivalent(lb, rb, name_pairs);
            name_pairs.pop();
            result
        }
        (LambdaTerm::Apply(lleft, lright), LambdaTerm::Apply(rleft, rright)) => {
            alpha_equivalent(lleft, rleft, name_pairs) && alpha_equivalent(lright, rright, name_pairs)
        }
        _ => false
    }
}