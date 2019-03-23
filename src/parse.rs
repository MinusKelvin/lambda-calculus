use std::str::Chars;
use crate::{ Bindings, LambdaTerm };

pub fn statement(line: &str, bindings: &Bindings) -> Result<Action, &'static str> {
    let mut lexer = Lexer::new(line.chars());
    match lexer.peek() {
        None => Ok(Action::DoNothing),
        Some(Token::Let) => {
            parse_let_binding(&mut lexer, bindings)
        },
        Some(_) => {
            let expr = parse_expression(&mut lexer, bindings, &mut Vec::new())?;
            match lexer.next() {
                Some(Token::Equals) => {
                    let other = parse_expression(&mut lexer, bindings, &mut Vec::new())?;
                    if lexer.next().is_some() {
                        Err("Expected end of input.")
                    } else {
                        Ok(Action::TestAlphaEquivalence(expr, other))
                    }
                },
                Some(_) => Err("Expected '=' or end of input."),
                None => Ok(Action::Evaluate(expr))
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

pub enum Action {
    Bind(String, LambdaTerm),
    TestAlphaEquivalence(LambdaTerm, LambdaTerm),
    Evaluate(LambdaTerm),
    DoNothing
}

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