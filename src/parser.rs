use mongodb::bson::{doc, Document};
use std::{
    iter::{Extend, Peekable},
    slice::Iter,
};

use crate::{
    error::{Result, SyntaxError},
    scanner::Token,
};

#[derive(Debug, PartialEq, Eq)]
enum ComparisonOperators {
    EqualEqual,
    GreaterThan,
    LessThan,
}

impl TryFrom<Token> for ComparisonOperators {
    type Error = &'static str;

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::EqualEqual => Ok(ComparisonOperators::EqualEqual),
            Token::GreaterThan => Ok(ComparisonOperators::GreaterThan),
            Token::LessThan => Ok(ComparisonOperators::LessThan),
            _ => Err("Can only convert comparison operators"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum LogicalOperators {
    And,
    Or,
}

impl TryFrom<Token> for LogicalOperators {
    type Error = &'static str;

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::And => Ok(LogicalOperators::And),
            Token::Or => Ok(LogicalOperators::Or),
            _ => Err("Can only convert logical operators"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Value {
    String(String),
    Integer(i32),
    Boolean(bool),
}

impl TryFrom<Token> for Value {
    type Error = &'static str;

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::String(v) => Ok(Value::String(v)),
            Token::Integer(v) => Ok(Value::Integer(v)),
            Token::Boolean(v) => Ok(Value::Boolean(v)),
            _ => Err("Can only convert logical operators"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Expression {
    LogicalOperators(LogicalOperators, Box<Expression>, Box<Expression>),
    ComparisonOperators(ComparisonOperators, String, Value),
}

impl Expression {
    fn eval(mut self) -> Result<Document> {
        match self {
            Expression::ComparisonOperators(comparison_operator, field, value) => {
                match comparison_operator {
                    ComparisonOperators::EqualEqual => match value {
                        Value::String(v) => Ok(doc! { field: v}),
                        Value::Integer(v) => Ok(doc! { field: v}),
                        Value::Boolean(v) => Ok(doc! { field: v}),
                    },
                    _ => Err(SyntaxError::new_parse_error(
                        "Invalid Expression".to_string(),
                    )),
                }
            }
            Expression::LogicalOperators(logical_operator, expr, rhs) => match logical_operator {
                LogicalOperators::Or => Ok(doc! {
                "$or": [
                    expr.eval()?,
                    rhs.eval()?
                    ]
                }),
                LogicalOperators::And => {
                    let mut d1 = expr.eval()?;
                    let d2 = rhs.eval()?;
                    d1.extend(d2);
                    Ok(d1)
                }
                _ => Err(SyntaxError::new_parse_error(
                    "Invalid Expression".to_string(),
                )),
            },
            _ => Err(SyntaxError::new_parse_error(
                "Invalid Expression".to_string(),
            )),
        }
    }
}

struct Parser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        Parser { iter }
    }

    fn assert_next(&mut self, token: Token) -> Result {
        let next = self.iter.next();
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                token,
                next.unwrap(),
            )));
        }

        Ok(())
    }

    fn primary(&mut self) -> Result<Expression> {
        let next = self.iter.next().unwrap();

        match next {
            Token::Var(v) => {
                let comparison_operator = match self.iter.next().unwrap() {
                    Token::EqualEqual => ComparisonOperators::EqualEqual,
                    _ => todo!(),
                };

                let value = match self.iter.next().unwrap() {
                    Token::String(v) => Value::String(v.to_owned()),
                    Token::Integer(v) => Value::Integer(v.to_owned()),
                    Token::Boolean(v) => Value::Boolean(v.to_owned()),
                    _ => todo!(),
                };

                return Ok(Expression::ComparisonOperators(
                    comparison_operator,
                    v.to_owned(),
                    value,
                ));
            }

            // Token::RightParen => {
            //     let expr = self.expression()?;
            //     self.assert_next(Token::LeftParen)?;
            //     Ok(expr)
            // }
            _ => {
                return Err(SyntaxError::new_parse_error(format!(
                    "Unexpected token {:?}",
                    next
                )))
            }
        }

        todo!();
    }

    fn and(&mut self) -> Result<Expression> {
        let mut expr: Expression = self.primary()?;

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression> {
        let mut expr: Expression = self.and()?;

        while let Some(tok) = self.iter.peek() {
            match tok {
                Token::And => {
                    self.iter.next();
                    let rhs: Expression = self.and()?;

                    expr = Expression::LogicalOperators(
                        LogicalOperators::And,
                        Box::new(expr),
                        Box::new(rhs),
                    )
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression> {
        let mut expr: Expression = self.or()?;

        while let Some(tok) = self.iter.peek() {
            match tok {
                Token::Or => {
                    self.iter.next();
                    let rhs: Expression = self.or()?;

                    expr = Expression::LogicalOperators(
                        LogicalOperators::Or,
                        Box::new(expr),
                        Box::new(rhs),
                    )
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    pub fn parse(mut self) -> Result<Expression> {
        // AKA Abstract Syntax Trees
        let ast = self.expression()?;
        self.assert_next(Token::End)?;
        Ok(ast)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::scanner::Token;
    use mongodb::bson::doc;

    #[test]
    fn string_eq() {
        let tokens = vec![
            Token::Var("name".to_owned()),
            Token::EqualEqual,
            Token::String("Ricardo".to_owned()),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "name": "Ricardo" }, filter);
    }

    #[test]
    fn integer_eq() {
        let tokens = vec![
            Token::Var("age".to_owned()),
            Token::EqualEqual,
            Token::Integer(10),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "age": 10 }, filter);
    }

    #[test]
    fn boolean_eq() {
        let tokens = vec![
            Token::Var("active".to_owned()),
            Token::EqualEqual,
            Token::Boolean(false),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "active": false }, filter);
    }

    #[test]
    fn or_operator() {
        let tokens = vec![
            Token::Var("name".to_owned()),
            Token::EqualEqual,
            Token::String("Ricardo".to_owned()),
            Token::Or,
            Token::Var("age".to_owned()),
            Token::EqualEqual,
            Token::Integer(10),
            Token::End,
        ];

        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(
            doc! {
                "$or": [
                    { "name": "Ricardo" },
                    { "age": 10 }
                ]
            },
            filter
        );
    }

    #[test]
    fn and_operator() {
        let tokens = vec![
            Token::Var("name".to_owned()),
            Token::EqualEqual,
            Token::String("Ricardo".to_owned()),
            Token::And,
            Token::Var("age".to_owned()),
            Token::EqualEqual,
            Token::Integer(10),
            Token::End,
        ];

        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "name": "Ricardo", "age": 10 }, filter);
    }

    #[test]
    fn and_or_operator() {
        let tokens = vec![
            Token::Var("name".to_owned()),
            Token::EqualEqual,
            Token::String("Ricardo".to_owned()),
            Token::And,
            Token::Var("age".to_owned()),
            Token::EqualEqual,
            Token::Integer(10),
            Token::Or,
            Token::Var("active".to_owned()),
            Token::EqualEqual,
            Token::Boolean(false),
            Token::End,
        ];

        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(
            doc! {
                "$or": [
                    { "name": "Ricardo", "age": 10 },
                    { "active": false }
                ]
            },
            filter
        );
    }
}
