use crate::error::{Result, SyntaxError};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Var(String),
    String(String),
    Integer(i32),
    Boolean(bool),
    And,
    Or,
    EqualEqual,
    GreaterThan,
    LessThan,
    End,
}

const SPECIAL_CHAR_LIST: [char; 4] = ['=', '>', '<', ' '];

fn is_spaciel_char(c: &char) -> bool {
    SPECIAL_CHAR_LIST.contains(c)
}

#[derive(Default)]
pub struct Scanner {}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner::default()
    }

    pub fn scan(self, query: &str) -> Result<Vec<Token>> {
        let mut char_indices = query.char_indices().peekable();
        let mut tokens: Vec<Token> = Vec::new();
        while let Some((pos, ch)) = char_indices.next() {
            let token = match ch {
                ' ' => continue,
                '=' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                    Some(_equals) => Token::EqualEqual,
                    None => {
                        return Err(SyntaxError::new_lex_error(
                            "Equal alone is not valid.".to_string(),
                        ))
                    }
                },
                '"' => {
                    let mut last_matched: char = '\0';
                    let s: String = char_indices
                        .by_ref()
                        .take_while(|(_pos, c)| {
                            last_matched = *c;
                            *c != '"'
                        })
                        .map(|(_pos, c)| c)
                        .collect();

                    match last_matched {
                        '"' => Token::String(s),
                        _ => {
                            return Err(SyntaxError::new_lex_error(
                                "Unterminated string.".to_string(),
                            ))
                        }
                    }
                }
                ch if char::is_alphabetic(ch) => {
                    let mut char = vec![ch];
                    while let Some((_, nch)) = char_indices.next_if(|(_, c)| !is_spaciel_char(c)) {
                        char.push(nch);
                    }
                    let s: String = char.iter().collect();

                    Token::Var(s)
                }
                ch if ch.is_numeric() => {
                    let mut char = vec![ch];
                    while let Some((_, nch)) =
                        char_indices.next_if(|(_, c)| char::is_ascii_digit(c))
                    {
                        char.push(nch);
                    }
                    let s: String = char.iter().collect();

                    Token::Integer(s.parse().unwrap())
                }
                _ => {
                    return Err(SyntaxError::new_lex_error(format!(
                        "Invalid Token: {} {}",
                        pos, ch
                    )))
                }
            };

            tokens.push(token)
        }

        tokens.push(Token::End);
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::{Scanner, Token};

    #[test]
    fn string_eq() {
        let query = "name == \"Ricardo\"";
        let tokens = Scanner::new().scan(query).unwrap();
        assert_eq!(
            vec![
                Token::Var("name".to_owned()),
                Token::EqualEqual,
                Token::String("Ricardo".to_owned()),
                Token::End
            ],
            tokens
        );

        let query = "name==\"Ricardo\"";
        let tokens = Scanner::new().scan(query).unwrap();
        assert_eq!(
            vec![
                Token::Var("name".to_owned()),
                Token::EqualEqual,
                Token::String("Ricardo".to_owned()),
                Token::End
            ],
            tokens
        );
    }

    #[test]
    fn number_eq() {
        let query = "age == 10";
        let tokens = Scanner::new().scan(query).unwrap();
        assert_eq!(
            vec![
                Token::Var("age".to_owned()),
                Token::EqualEqual,
                Token::Integer(10),
                Token::End
            ],
            tokens
        );

        let query = "age==10";
        let tokens = Scanner::new().scan(query).unwrap();
        assert_eq!(
            vec![
                Token::Var("age".to_owned()),
                Token::EqualEqual,
                Token::Integer(10),
                Token::End
            ],
            tokens
        );
    }
}
