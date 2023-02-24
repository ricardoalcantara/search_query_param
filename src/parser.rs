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
pub enum ComparisonOperators {
    EqualEqual,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl TryFrom<&Token> for ComparisonOperators {
    type Error = &'static str;

    fn try_from(token: &Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::EqualEqual => Ok(ComparisonOperators::EqualEqual),
            Token::NotEqual => Ok(ComparisonOperators::NotEqual),
            Token::GreaterThan => Ok(ComparisonOperators::GreaterThan),
            Token::GreaterThanOrEqual => Ok(ComparisonOperators::GreaterThanOrEqual),
            Token::LessThan => Ok(ComparisonOperators::LessThan),
            Token::LessThanOrEqual => Ok(ComparisonOperators::LessThanOrEqual),
            _ => Err("Can only convert comparison operators"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOperators {
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
pub enum Value {
    String(String),
    Integer(i32),
    Boolean(bool),
}

impl TryFrom<&Token> for Value {
    type Error = &'static str;

    fn try_from(token: &Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::String(v) => Ok(Value::String(v.to_owned())),
            Token::Integer(v) => Ok(Value::Integer(v.to_owned())),
            Token::Boolean(v) => Ok(Value::Boolean(v.to_owned())),
            _ => Err("Can only convert logical operators"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    LogicalOperators(LogicalOperators, Box<Expression>, Box<Expression>),
    ComparisonOperators(ComparisonOperators, String, Value),
}

impl Expression {
    pub fn eval(self) -> Result<Document> {
        match self {
            Expression::ComparisonOperators(comparison_operator, field, value) => {
                match comparison_operator {
                    ComparisonOperators::EqualEqual => match value {
                        Value::String(v) => Ok(doc! { field: v}),
                        Value::Integer(v) => Ok(doc! { field: v}),
                        Value::Boolean(v) => Ok(doc! { field: v}),
                    },
                    ComparisonOperators::NotEqual => match value {
                        Value::String(v) => Ok(doc! { field: { "$ne": v }}),
                        Value::Integer(v) => Ok(doc! { field: { "$ne": v }}),
                        Value::Boolean(v) => Ok(doc! { field: { "$ne": v }}),
                    },
                    ComparisonOperators::GreaterThan => match value {
                        Value::String(v) => Ok(doc! { field: { "$gt": v }}),
                        Value::Integer(v) => Ok(doc! { field: { "$gt": v }}),
                        Value::Boolean(v) => Ok(doc! { field: { "$gt": v }}),
                    },
                    ComparisonOperators::GreaterThanOrEqual => match value {
                        Value::String(v) => Ok(doc! { field: { "$gte": v }}),
                        Value::Integer(v) => Ok(doc! { field: { "$gte": v }}),
                        Value::Boolean(v) => Ok(doc! { field: { "$gte": v }}),
                    },
                    ComparisonOperators::LessThan => match value {
                        Value::String(v) => Ok(doc! { field: { "$lt": v }}),
                        Value::Integer(v) => Ok(doc! { field: { "$lt": v }}),
                        Value::Boolean(v) => Ok(doc! { field: { "$lt": v }}),
                    },
                    ComparisonOperators::LessThanOrEqual => match value {
                        Value::String(v) => Ok(doc! { field: { "$lte": v }}),
                        Value::Integer(v) => Ok(doc! { field: { "$lte": v }}),
                        Value::Boolean(v) => Ok(doc! { field: { "$lte": v }}),
                    },
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
            },
        }
    }
}

pub struct Parser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
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

    fn and(&mut self) -> Result<Expression> {
        let next = self.iter.next().unwrap();

        match next {
            Token::Var(v) => {
                let comparison_operator =
                    ComparisonOperators::try_from(self.iter.next().unwrap()).unwrap();

                let value = Value::try_from(self.iter.next().unwrap()).unwrap();

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
    }

    // fn and(&mut self) -> Result<Expression> {
    //     let mut expr: Expression = self.primary()?;

    //     Ok(expr)
    // }

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
    fn string_nq() {
        let tokens = vec![
            Token::Var("name".to_owned()),
            Token::NotEqual,
            Token::String("Ricardo".to_owned()),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "name": { "$ne": "Ricardo"} }, filter);
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
    fn integer_gt() {
        let tokens = vec![
            Token::Var("age".to_owned()),
            Token::GreaterThan,
            Token::Integer(10),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "age": { "$gt": 10 } }, filter);
    }

    #[test]
    fn integer_gte() {
        let tokens = vec![
            Token::Var("age".to_owned()),
            Token::GreaterThanOrEqual,
            Token::Integer(10),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "age": { "$gte": 10 } }, filter);
    }

    #[test]
    fn integer_lt() {
        let tokens = vec![
            Token::Var("age".to_owned()),
            Token::LessThan,
            Token::Integer(10),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "age": { "$lt": 10 } }, filter);
    }

    #[test]
    fn integer_lte() {
        let tokens = vec![
            Token::Var("age".to_owned()),
            Token::LessThanOrEqual,
            Token::Integer(10),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "age": { "$lte": 10 } }, filter);
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
    fn boolean_ne() {
        let tokens = vec![
            Token::Var("active".to_owned()),
            Token::NotEqual,
            Token::Boolean(false),
            Token::End,
        ];
        let mut token_iter = tokens.iter().peekable();
        let parser = Parser::new(&mut token_iter);
        let expression = parser.parse().unwrap();

        let filter = expression.eval().unwrap();
        assert_eq!(doc! { "active": { "$ne": false } }, filter);
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
