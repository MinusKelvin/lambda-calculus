use rustyline::{ error::ReadlineError, Editor, config::Configurer };
use std::collections::HashMap;
use std::str::Chars;

fn main() {
    let mut reader = Editor::<()>::new();
    reader.set_auto_add_history(true);
    let mut bindings = HashMap::new();

    loop { match reader.readline("λ : ") {
        Ok(line) => {
            match parse_statement(&line, &mut bindings) {
                Ok(Action::Evaluate(expr)) => {
                    let expr = evaluate(expr);
                    println!("  > {}", symbolize(expr, &bindings));
                }
                Ok(Action::Bind(ident, expr)) => {
                    let expr = evaluate(expr);
                    println!("  > {}", symbolize(expr.clone(), &bindings));
                    bindings.insert(ident, expr);
                    println!("Bound.")
                },
                Ok(Action::DoNothing) => {}
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

fn evaluate(mut expr: LambdaTerm) -> LambdaTerm {
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

enum Reduction {
    Preformed(LambdaTerm),
    NotPreformed(LambdaTerm)
}

fn parse_statement(line: &str, bindings: &Bindings) -> Result<Action, &'static str> {
    let mut lexer = Lexer::new(line.chars());
    match lexer.peek() {
        None => Ok(Action::DoNothing),
        Some(Token::Let) => {
            parse_let_binding(&mut lexer, bindings)
        },
        Some(_) => {
            let expr = parse_expression(&mut lexer, bindings, &mut Vec::new())?;
            if lexer.next().is_some() {
                Err("Unexpected tokens after expression")
            } else {
                Ok(Action::Evaluate(expr))
            }
        }
    }
}

fn parse_let_binding(lexer: &mut Lexer, bindings: &Bindings) -> Result<Action, &'static str> {
    lexer.next(); // Clear let token
    match lexer.next() {
        Some(Token::Ident(id)) => if lexer.next() != Some(Token::Equals) {
            Err("Expected '=' after identifier in let binding.")
        } else {
            let expr = parse_expression(lexer, bindings, &mut Vec::new())?;
            if lexer.next().is_some() {
                Err("Unexpected tokens after expression")
            } else {
                Ok(Action::Bind(id, expr))
            }
        },
        _ => Err("Expected identifier after 'let'.")
    }
}

fn parse_immediate_expression(
    lexer: &mut Lexer, bindings: &Bindings, shadowed: &mut Vec<String>
) -> Result<LambdaTerm, &'static str> {
    match lexer.next() {
        Some(Token::LeftParen) => {
            let result = parse_expression(lexer, bindings, shadowed)?;
            if lexer.next() != Some(Token::RightParen) {
                Err("Unmatched parenthesis")
            } else {
                Ok(result)
            }
        }
        Some(Token::Lambda) => {
            let ident = match lexer.next() {
                Some(Token::Ident(id)) => id,
                _ => return Err("Expected identifier after λ.")
            };
            if lexer.next() != Some(Token::Dot) {
                Err("Expected '.' after identifier in λ expression.")
            } else {
                shadowed.push(ident.clone());
                let e = parse_expression(lexer, bindings, shadowed)
                        .map(|e| LambdaTerm::Abstract(ident, Box::new(e)));
                shadowed.pop();
                e
            }
        }
        Some(Token::Ident(id)) => {
            let result = if shadowed.contains(&id) {
                LambdaTerm::Symbol(id)
            } else if bindings.contains_key(&id) {
                bindings[&id].clone()
            } else {
                return Err("Unbound identifier encountered")
            };
            Ok(result)
        }
        _ => {
            Err("Expected an identifier, 'λ', or '('")
        }
    }
}

fn parse_continued_expression(
    lexer: &mut Lexer, bindings: &Bindings, shadowed: &mut Vec<String>, left: LambdaTerm
) -> Result<LambdaTerm, &'static str> {
    match lexer.peek() {
        Some(Token::LeftParen) | Some(Token::Lambda) | Some(Token::Ident(_)) => {
            let right = parse_immediate_expression(lexer, bindings, shadowed)?;
            parse_continued_expression(lexer, bindings, shadowed, LambdaTerm::Apply(
                Box::new(left), Box::new(right)
            ))
        },
        _ => Ok(left)
    }
}

fn parse_expression(
    lexer: &mut Lexer, bindings: &Bindings, shadowed: &mut Vec<String>
) -> Result<LambdaTerm, &'static str> {
    let left = parse_immediate_expression(lexer, bindings, shadowed)?;
    parse_continued_expression(lexer, bindings, shadowed, left)
}

enum Action {
    Bind(String, LambdaTerm),
    Evaluate(LambdaTerm),
    DoNothing
}

#[derive(Clone, Eq, Hash, Debug)]
enum LambdaTerm {
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

type Bindings = HashMap<String, LambdaTerm>;

struct Lexer<'a> {
    chars: Chars<'a>,
    peek: Option<char>,
    next_token: Option<Token>
}

impl<'a> Lexer<'a> {
    fn new(mut chars: Chars<'a>) -> Self {
        Lexer {
            peek: chars.next(),
            chars,
            next_token: None
        }
    }

    fn next_char(&mut self) {
        self.peek = self.chars.next();
    }

    fn peek(&mut self) -> Option<Token> {
        if self.next_token.is_none() {
            self.next_token = self.next();
        }
        return self.next_token.clone();
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        if self.next_token.is_some() {
            return self.next_token.take();
        }
        while let Some(c) = self.peek {
            if !c.is_whitespace() { break }
            self.next_char();
        }
        let c = self.peek?;
        self.next_char();
        Some(match c {
            'λ' | '\\' => Token::Lambda,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '.' => Token::Dot,
            '=' => Token::Equals,
            _ => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(c) = self.peek {
                    if !c.is_alphanumeric() { break }
                    ident.push(c);
                    self.next_char();
                }
                match &*ident {
                    "lambda" => Token::Lambda,
                    "let" => Token::Let,
                    _ => Token::Ident(ident)
                }
            }
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Token {
    LeftParen,
    RightParen,
    Lambda,
    Dot,
    Let,
    Equals,
    Ident(String)
}