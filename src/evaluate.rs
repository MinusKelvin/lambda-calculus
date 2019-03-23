use std::collections::HashSet;
use crate::LambdaTerm;

pub fn evaluate(mut expr: LambdaTerm) -> Evaluation {
    println!("  < {}", expr);
    let mut counter = 0;
    loop {
        match beta_reduce(expr) {
            Reduction::Preformed(transformation, e) => {
                expr = e;
                println!("{} | {}", transformation, expr);
            }
            Reduction::NotPreformed(e) => {
                return Evaluation::BetaNormal(e)
            }
        }
        counter += 1;
        if counter == 100000 {
            return Evaluation::Unfinished(expr)
        }
    }
}

fn beta_reduce(expr: LambdaTerm) -> Reduction {
    match expr {
        LambdaTerm::Apply(left, right) => {
            if let LambdaTerm::Abstract(symbol, body) = *left {
                let mut outside_variables = HashSet::new();
                collect_outside_variables(&right, &mut outside_variables, &mut vec![]);
                match alpha_convert(*body, &symbol, &outside_variables) {
                    Reduction::Preformed(transformation, expr) => Reduction::Preformed(
                        transformation,
                        LambdaTerm::Apply(Box::new(LambdaTerm::Abstract(
                            symbol, Box::new(expr)
                        )), right)
                    ),
                    Reduction::NotPreformed(expr) => Reduction::Preformed(
                        Transformation::BetaReduction,
                        replace(&symbol, expr, *right)
                    )
                }
            } else {
                let left = match beta_reduce(*left) {
                    Reduction::Preformed(transformation, expr) => return Reduction::Preformed(
                        transformation,
                        LambdaTerm::Apply(Box::new(expr), right)
                    ),
                    Reduction::NotPreformed(expr) => expr
                };
                let left = Box::new(left);
                match beta_reduce(*right) {
                    Reduction::Preformed(transformation, expr) => Reduction::Preformed(
                        transformation,
                        LambdaTerm::Apply(left, Box::new(expr))
                    ),
                    Reduction::NotPreformed(expr) => Reduction::NotPreformed(
                        LambdaTerm::Apply(left, Box::new(expr))
                    )
                }
            }
        }
        LambdaTerm::Abstract(s, body) => {
            match beta_reduce(*body) {
                Reduction::Preformed(transformation, expr) => Reduction::Preformed(
                    transformation,
                    LambdaTerm::Abstract(s, Box::new(expr))
                ),
                Reduction::NotPreformed(expr) => Reduction::NotPreformed(
                    LambdaTerm::Abstract(s, Box::new(expr))
                ),
            }
        }
        _ => Reduction::NotPreformed(expr)
    }
}

fn replace(symbol: &str, body: LambdaTerm, with: LambdaTerm) -> LambdaTerm {
    match body {
        LambdaTerm::Abstract(ref s, _) if s == symbol => body,
        LambdaTerm::Abstract(s, body) => LambdaTerm::Abstract(
            s, Box::new(replace(symbol, *body, with))
        ),
        LambdaTerm::Apply(left, right) => LambdaTerm::Apply(
            Box::new(replace(symbol, *left, with.clone())),
            Box::new(replace(symbol, *right, with))
        ),
        LambdaTerm::Symbol(ref s) if s == symbol => with,
        LambdaTerm::Symbol(_) => body
    }
}

fn collect_outside_variables<'a>(
    term: &'a LambdaTerm, outside_variables: &mut HashSet<&'a str>, exclude: &mut Vec<&'a str>
) {
    match term {
        LambdaTerm::Abstract(s, body) => {
            exclude.push(s);
            collect_outside_variables(body, outside_variables, exclude);
            exclude.pop();
        }
        LambdaTerm::Apply(left, right) => {
            collect_outside_variables(left, outside_variables, exclude);
            collect_outside_variables(right, outside_variables, exclude);
        }
        LambdaTerm::Symbol(s) => {
            if !exclude.contains(&&**s) {
                outside_variables.insert(s);
            }
        }
    }
}

fn alpha_convert(term: LambdaTerm, replacing: &str, possible_conflicts: &HashSet<&str>) -> Reduction {
    match term {
        LambdaTerm::Abstract(ref s, _) if s == replacing => Reduction::NotPreformed(term),
        LambdaTerm::Abstract(s, body) => {
            if possible_conflicts.contains(&*s) {
                let mut outside_variables = HashSet::new();
                collect_outside_variables(&body, &mut outside_variables, &mut vec![]);
                if outside_variables.contains(replacing) {
                    let renamed = s.clone() + "'";
                    return Reduction::Preformed(Transformation::AlphaConversion, LambdaTerm::Abstract(
                        renamed.clone(),
                        Box::new(replace(&s, *body, LambdaTerm::Symbol(renamed)))
                    ))
                }
            }
            match alpha_convert(*body, replacing, possible_conflicts) {
                Reduction::Preformed(transformation, expr) => Reduction::Preformed(
                    transformation, LambdaTerm::Abstract(s, Box::new(expr))
                ),
                Reduction::NotPreformed(expr) => Reduction::NotPreformed(
                    LambdaTerm::Abstract(s, Box::new(expr))
                )
            }
        }
        LambdaTerm::Apply(left, right) => {
            let left = match alpha_convert(*left, replacing, possible_conflicts) {
                Reduction::Preformed(transformation, expr) => return Reduction::Preformed(
                    transformation,
                    LambdaTerm::Apply(Box::new(expr), right)
                ),
                Reduction::NotPreformed(expr) => expr
            };
            match alpha_convert(*right, replacing, possible_conflicts) {
                Reduction::Preformed(transformation, expr) => Reduction::Preformed(
                    transformation,
                    LambdaTerm::Apply(Box::new(left), Box::new(expr))
                ),
                Reduction::NotPreformed(expr) => Reduction::NotPreformed(
                    LambdaTerm::Apply(Box::new(left), Box::new(expr))
                )
            }
        }
        LambdaTerm::Symbol(_) => Reduction::NotPreformed(term)
    }
}

enum Reduction {
    Preformed(Transformation, LambdaTerm),
    NotPreformed(LambdaTerm)
}

enum Transformation {
    BetaReduction,
    AlphaConversion
}

pub enum Evaluation {
    BetaNormal(LambdaTerm),
    Unfinished(LambdaTerm)
}

impl std::fmt::Display for Transformation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Transformation::AlphaConversion => write!(f, "α"),
            Transformation::BetaReduction   => write!(f, "β"),
        }
    }
}