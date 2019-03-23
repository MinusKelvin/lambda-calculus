use crate::{ LambdaTerm, Bindings };

impl std::fmt::Display for LambdaTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LambdaTerm::Symbol(s) => write!(f, "{}", s),
            LambdaTerm::Abstract(s, body) => {
                write!(f, "λ{}. {}", s, body)
            },
            LambdaTerm::Apply(left, right) => {
                if ends_with_abstraction(left) {
                    write!(f, "({}) ", left)?
                } else {
                    write!(f, "{} ", left)?
                }
                if let LambdaTerm::Apply(_, _) = **right {
                    write!(f, "({})", right)
                } else {
                    write!(f, "{}", right)
                }
            }
        }
    }
}

fn ends_with_abstraction(expr: &LambdaTerm) -> bool {
    match expr {
        LambdaTerm::Abstract(_, _) => true,
        LambdaTerm::Apply(_, right) => ends_with_abstraction(right),
        LambdaTerm::Symbol(_) => false
    }
}

fn symbolize(expr: LambdaTerm, bindings: &Bindings) -> LambdaTerm {
    let mut valid_symbols = vec![];
    for (name, term) in bindings {
        if *term == expr {
            valid_symbols.push(name);
        }
    }
    if valid_symbols.len() == 1 {
        LambdaTerm::Symbol(valid_symbols.first().unwrap().to_string())
    } else {
        match expr {
            LambdaTerm::Symbol(_) => expr,
            LambdaTerm::Abstract(s, b) => LambdaTerm::Abstract(s, Box::new(symbolize(*b, bindings))),
            LambdaTerm::Apply(l, r) => LambdaTerm::Apply(
                Box::new(symbolize(*l, bindings)),
                Box::new(symbolize(*r, bindings))
            )
        }
    }
}

pub fn pretty_printer(expr: LambdaTerm, bindings: &Bindings) -> PrettyPrinter {
    PrettyPrinter(symbolize(expr, bindings))
}

pub struct PrettyPrinter(LambdaTerm);

impl std::fmt::Display for PrettyPrinter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}