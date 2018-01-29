use std::str::Chars;
use quick_xml::events::Event;
use super::{Positioning, Tracker, TrackerSnapshot};

#[derive(Debug)]
enum Operator {
    // `/name`
    Descendant(String),
    // `//name`
    DeeperDescendant(String),
    // `*`
    Wildcard,
    // `[1]`
    Position(Box<Operator>, usize),
    List(Vec<Operator>),
    //Predicate(Box<Operator>, Predicate),
}

impl Operator {
    fn is_match(&self, snapshot: &TrackerSnapshot) -> bool {
        match *self {
            Operator::Descendant(ref name) => {
                snapshot.path.len() > 0 && snapshot.path[0] == name.as_bytes()
            },
            Operator::DeeperDescendant(ref name) => {
                snapshot.path.iter().any(|x| &x[..] == name.as_bytes())
            },
            Operator::Wildcard => {
                !snapshot.path.is_empty()
            },
            Operator::Position(ref inner, one_based_index) => {
                // this cannot handle (/a/*)[2]
                let counts = snapshot.counts;
                inner.is_match(snapshot) && counts[counts.len() - 1] + 1 == one_based_index
            },
            Operator::List(ref inners) => {
                let mut snapshot = *snapshot;

                for inner in inners {
                    let m = inner.is_match(&snapshot);
                    if !m {
                        return false;
                    }
                    snapshot = snapshot.tail();
                }

                true
            }
        }
    }
}

pub struct Selector {
    operators: Operator,
    tracker: Tracker,
}

impl Selector {
    pub(crate) fn update_on_event(&mut self, pos: &Positioning, event: &Event) {
        self.tracker.on_event(pos, event);
    }

    pub fn is_match(&self) -> bool {
        self.operators.is_match(&self.tracker.snapshot())
    }

    pub fn parse(s: &str) -> Selector {
        let mut tokens = Tokenizer::from(s.chars());
        let mut buffer: Option<Token> = None;

        let mut operators = Vec::new();

        loop {
            let next = buffer.take().or_else(|| tokens.next());
            match next {
                Some(Token::Slash) => {

                    if let Some(Token::Name(name)) = tokens.next() {
                        operators.push(Operator::Descendant(name));
                        continue;
                    }

                    panic!("Expected name after Slash: {}", s);
                },
                Some(Token::DoubleSlash) => {

                    if let Some(Token::Name(name)) = tokens.next() {
                        operators.push(Operator::DeeperDescendant(name));
                        continue;
                    }

                    panic!("Expected name after DoubleSlash: {}", s);
                }
                Some(Token::Name(_)) => {
                    panic!("Invalid top level name: {}", s);
                }
                Some(other) => panic!("Unsupported: {}", s),
                None => break,
            }
        }

        let operators = if operators.len() == 1 {
            operators.pop().unwrap()
        } else {
            Operator::List(operators)
        };

        Selector {
            operators,
            tracker: Tracker::new(),
        }
    }
}

#[test]
fn parse_simple() {
    use quick_xml::reader::Reader;
    let mut s = Selector::parse("/foo/bar/car");

    let input =
        r#"<foo>
             <a />
             <b />
             <bar>
                <c />
                <car>foo</car> <!-- first -->
                <car>bar</car> <!-- second -->
             </bar>
             <d>
                something
             </d>
             <bar>
                <c />
                <car>foo</car> <!-- third -->
                <car>bar</car> <!-- fourth -->
            </bar>
          </foo>"#;

    let mut reader = Reader::from_str(input);
    let mut buffer = Vec::new();

    let mut matches = 0;

    loop {
        let evt = reader.read_event(&mut buffer).unwrap();
        s.update_on_event(&reader, &evt);
        match evt {
            Event::Start(ref e) | Event::Empty(ref e) => if s.is_match() { matches += 1; },
            Event::Eof => break,
            _ => {},
        }
    }

    assert_eq!(matches, 4);
}

#[derive(Debug)]
enum Token {
    Slash,
    DoubleSlash,
    Name(String),
    Dot,
    Star,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
}

struct Tokenizer<'a> {
    chars: Chars<'a>,
    buffer: Option<char>,
    eoffed: bool,
}

impl<'a> From<Chars<'a>> for Tokenizer<'a> {
    fn from(chars: Chars<'a>) -> Tokenizer<'a> {
        Tokenizer {
            chars,
            buffer: None,
            eoffed: false,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {

        if self.eoffed && self.buffer.is_none() {
            return None;
        }

        let next = self.buffer.take().or_else(|| self.chars.next());

        match next {
            Some('/') => {
                match self.chars.next() {
                    Some(lookahead) if lookahead == '/' => Some(Token::DoubleSlash),
                    Some(other) => {
                        self.buffer = Some(other);
                        Some(Token::Slash)
                    },
                    None => {
                        self.eoffed = true;
                        Some(Token::Slash)
                    }
                }
            },
            Some('(') => Some(Token::OpenParen),
            Some(')') => Some(Token::CloseParen),
            Some('[') => Some(Token::OpenBracket),
            Some(']') => Some(Token::CloseBracket),
            Some('.') => Some(Token::Dot),
            Some('*') => Some(Token::Star),
            Some(other) => {
                let mut s = String::new();

                s.push(other);
                let mut colon = false;

                loop {
                    match self.chars.next() {
                        Some(ch @ ':') => {
                            assert!(!colon);
                            colon = true;
                            s.push(ch);
                        },
                        Some(ch @ '_') |
                        Some(ch @ '-') => s.push(ch),
                        Some(digit @ '0'...'9') => s.push(digit),
                        Some(ch @ 'A'...'Z') |
                        Some(ch @ 'a'...'z') => s.push(ch),
                        Some(other) => {
                            self.buffer = Some(other);
                            return Some(Token::Name(s));
                        }
                        None => {
                            self.eoffed = true;
                            return Some(Token::Name(s));
                        }
                    }
                }
            }
            None => {
                self.eoffed = true;
                None
            },
        }
    }
}
