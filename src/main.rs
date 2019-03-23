use rustyline::{ error::ReadlineError, Editor, config::Configurer };
use std::collections::HashMap;

mod evaluate;
mod parse;
mod print;

fn main() {
    let mut reader = Editor::<()>::new();
    reader.set_auto_add_history(true);
    let mut bindings = HashMap::new();

    loop { match reader.readline("λ < ") {
        Ok(line) => {
            match parse::statement(&line, &mut bindings) {
                Ok(parse::Action::Evaluate(expr)) => {
                    eval(expr, &mut reader, &bindings);
                }
                Ok(parse::Action::Bind(ident, expr)) =>
                    if let Some(expr) = eval(expr, &mut reader, &bindings) {
                        println!("Bound to {}", ident);
                        bindings.insert(ident, expr);
                    }
                Ok(parse::Action::TestAlphaEquivalence(left, right)) => {
                    let left = eval(left, &mut reader, &bindings);
                    let right = eval(right, &mut reader, &bindings);
                    if let (Some(left), Some(right)) = (left, right) {
                        println!("α > {}", left == right);
                    }
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

fn eval(mut expr: LambdaTerm, reader: &mut Editor<()>, bindings: &Bindings) -> Option<LambdaTerm> {
    loop {
        match evaluate::evaluate(expr) {
            evaluate::Evaluation::BetaNormal(expr) => {
                println!("  > {}", print::pretty_printer(expr.clone(), bindings));
                return Some(expr)
            }
            evaluate::Evaluation::Unfinished(e) => {
                println!("This computation has not terminated after 100,000 steps.");
                match reader.readline("Continue? ") {
                    Err(ReadlineError::Interrupted) => return None,
                    Err(ReadlineError::Eof) => return None,
                    Err(e @ _) => {
                        println!("An error occured while reading input: {}", e);
                        return None
                    }
                    Ok(line) => match &*line.to_lowercase() {
                        "yes" | "y" => expr = e,
                        _ => return None
                    }
                }
            }
        }
    }
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