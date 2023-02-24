use error::Result;
use mongodb::bson::Document;
use parser::Parser;
use scanner::Scanner;

pub mod error;
pub mod parser;
pub mod scanner;

pub fn parse_filter(query: &str) -> Result<Document> {
    let tokens = Scanner::new().scan(query)?;
    let mut token_iter = tokens.iter().peekable();
    let parser = Parser::new(&mut token_iter);
    parser.parse()?.eval()
}

pub fn parse_filter_whitelist(query: &str, whitelist: &Vec<&str>) -> Result<Document> {
    let tokens = Scanner::new().scan(query)?;
    let mut token_iter = tokens.iter().peekable();
    let parser = Parser::new(&mut token_iter).set_whitelist(whitelist);
    parser.parse()?.eval()
}

#[cfg(test)]
mod tests {
    use crate::parse_filter;
    use mongodb::bson::doc;

    #[test]
    fn string_eq() {
        let query = "name == \"Ricardo\"";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "name": "Ricardo" }, filter);
    }

    #[test]
    fn string_ne() {
        let query = "name != \"Ricardo\"";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "name": { "$ne": "Ricardo"} }, filter);
    }

    #[test]
    fn integer_eq() {
        let query = "age == 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": 10 }, filter);
    }

    #[test]
    fn integer_ne() {
        let query = "age != 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": { "$ne": 10 } }, filter);
    }

    #[test]
    fn integer_gt() {
        let query = "age > 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": { "$gt": 10 } }, filter);
    }

    #[test]
    fn integer_gte() {
        let query = "age >= 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": { "$gte": 10 } }, filter);
    }

    #[test]
    fn integer_lt() {
        let query = "age < 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": { "$lt": 10 } }, filter);
    }

    #[test]
    fn integer_lte() {
        let query = "age <= 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "age": { "$lte": 10 } }, filter);
    }

    #[test]
    fn boolean_eq() {
        let query = "active == false";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "active": false }, filter);
    }

    #[test]
    fn boolean_ne() {
        let query = "active != false";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "active": { "$ne": false } }, filter);
    }

    #[test]
    fn or_operator() {
        let query = "name == \"Ricardo\" or age == 10";
        let filter = parse_filter(query).unwrap();

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
        let query = "name == \"Ricardo\" and age == 10";
        let filter = parse_filter(query).unwrap();

        assert_eq!(doc! { "name": "Ricardo", "age": 10 }, filter);
    }

    #[test]
    fn and_or_operator() {
        let query = "name == \"Ricardo\" and age == 10 or active == false";
        let filter = parse_filter(query).unwrap();

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
