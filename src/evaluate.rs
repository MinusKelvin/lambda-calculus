use crate::LambdaTerm;

pub fn evaluate(mut expr: LambdaTerm) -> LambdaTerm {
    println!("  < {}", expr);
    loop {
        match beta_reduce(expr) {
            Reduction::Preformed(e) => {
                expr = e;
                println!("β | {}", expr);
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
                Reduction::Preformed(expr) => return Reduction::Preformed(
                    LambdaTerm::Apply(Box::new(expr), right)
                ),
                Reduction::NotPreformed(expr) => expr
            };
            if let LambdaTerm::Abstract(symbol, body) = left {
                Reduction::Preformed(replace(&symbol, *body, *right))
            } else {
                let left = Box::new(left);
                match beta_reduce(*right) {
                    Reduction::Preformed(expr) => Reduction::Preformed(
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
                Reduction::Preformed(expr) => Reduction::Preformed(
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
    Preformed(LambdaTerm),
    NotPreformed(LambdaTerm)
}