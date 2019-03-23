use crate::LambdaTerm;

pub fn evaluate(mut expr: LambdaTerm) -> LambdaTerm {
    println!("  < {}", expr);
    loop {
        match beta_reduce(expr) {
            Reduction::Preformed(transformation, e) => {
                expr = e;
                println!("{} | {}", transformation, expr);
            }
            Reduction::NotPreformed(e) => {
                return e
            }
        }
    }
}

fn beta_reduce(expr: LambdaTerm) -> Reduction {
    match expr {
        LambdaTerm::Apply(left, right) => {
            let left = match beta_reduce(*left) {
                Reduction::Preformed(transformation, expr) => return Reduction::Preformed(
                    transformation,
                    LambdaTerm::Apply(Box::new(expr), right)
                ),
                Reduction::NotPreformed(expr) => expr
            };
            if let LambdaTerm::Abstract(symbol, body) = left {
                Reduction::Preformed(Transformation::BetaReduction, replace(&symbol, *body, *right))
            } else {
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

enum Reduction {
    Preformed(Transformation, LambdaTerm),
    NotPreformed(LambdaTerm)
}

enum Transformation {
    BetaReduction,
    AlphaConversion
}

impl std::fmt::Display for Transformation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Transformation::AlphaConversion => write!(f, "α"),
            Transformation::BetaReduction   => write!(f, "β"),
        }
    }
}