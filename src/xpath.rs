use std::str::Chars;
use quick_xml::events::Event;
use super::{Span, Tracker, TrackerSnapshot};

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
    fn is_match(&mut self, snapshot: &TrackerSnapshot) -> bool {
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
            Operator::Position(ref mut inner, ref mut one_based_index) => {
                if one_based_index == &0 {
                    return false;
                }

                if inner.is_match(snapshot) {
                    *one_based_index -= 1;
                    return one_based_index == &0;
                }

                return false;
            },
            Operator::List(ref mut inners) => {
                let mut snapshot = *snapshot;

                for inner in inners.iter_mut() {
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
    pub(crate) fn update_on_event(&mut self, span: Option<Span>, event: &Event) {
        self.tracker.on_event(span, event);
    }

    pub fn is_match(&mut self) -> bool {
        self.operators.is_match(&self.tracker.snapshot())
    }

    pub fn parse(s: &str) -> Selector {
        let mut tokens = Tokenizer::from(s.chars());
        let mut buffer: Option<Token> = None;

        let mut operators = Vec::new();

        let mut token_buffer = Vec::new();

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
                Some(token @ Token::OpenParen) => {
                    token_buffer.push((token, operators.len()));
                },
                Some(Token::CloseParen) => {
                    if let Some((Token::OpenParen, count)) = token_buffer.pop() {
                        let mut sublist = operators.split_off(count);

                        let inner;
                        if sublist.len() > 1 {
                            inner = Operator::List(sublist);
                        } else if sublist.len() > 0 {
                            inner = sublist.pop().unwrap();
                        } else {
                            panic!("Empty parens: {}", s);
                        }

                        operators.push(inner);
                    } else {
                        panic!("CloseParen without matching OpenParen: {}", s);
                    }
                },
                Some(token @ Token::OpenBracket) => {
                    if operators.is_empty() {
                        panic!("OpenBracket without preceding operator: {}", s);
                    }

                    token_buffer.push((token, operators.len()));
                },
                Some(token @ Token::Number(_)) => {
                    match token_buffer.last() {
                        Some(&(Token::OpenBracket, _)) => { /* expected */ },
                        _ => panic!("Numbers outside [...] not supported: {}", s),
                    }

                    token_buffer.push((token, operators.len()));
                }
                Some(Token::CloseBracket) => {
                    let number = token_buffer.pop();
                    let open_bracket = token_buffer.pop();

                    match (open_bracket, number) {
                        (Some((Token::OpenBracket, first_count)), Some((Token::Number(n), last_count))) => {
                            if first_count != last_count {
                                panic!("Anything other than number inside brackets is unsupported: {}", s);
                            }

                            assert_eq!(first_count, operators.len());

                            if n < 1 {
                                panic!("Position should be 1..: {}", s);
                            }

                            let preceding = operators.pop().unwrap();
                            operators.push(Operator::Position(Box::new(preceding), n as usize));
                        }
                        _ => {
                            panic!("Invalid tokens preceding closebracket: {}", s);
                        }
                    }
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
    use super::Positioning;
    use quick_xml::reader::Reader;
    let mut s1 = Selector::parse("/foo/bar/car");
    let mut s2 = Selector::parse("(//car)[3]");

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
                <car>foobar</car> <!-- third -->
                <car>bar</car> <!-- fourth -->
            </bar>
          </foo>"#;

    let mut reader = Reader::from_str(input);
    let mut buffer = Vec::new();

    let mut s1_matches = 0;
    let mut s2_matches = 0;
    let mut s2_text = None;

    loop {
        let evt = reader.read_event(&mut buffer).unwrap();
        s1.update_on_event(reader.span_for(&evt), &evt);
        s2.update_on_event(reader.span_for(&evt), &evt);
        match evt {
            Event::Start(ref e) | Event::Empty(ref e) => {
                if s1.is_match() { s1_matches += 1; }
                if s2.is_match() { s2_matches += 1; }
            },
            Event::Text(ref e) if s2_matches == 1 && !e.is_empty() && s2_text.is_none() => {
                s2_text = Some(e.unescaped().unwrap().into_owned());
            }
            Event::Eof => break,
            _ => {},
        }
    }

    assert_eq!(s1_matches, 4);
    assert_eq!(s2_matches, 1);
    assert_eq!(s2_text, Some(b"foobar".to_vec()));
}

#[derive(Debug)]
enum Token {
    Slash,
    DoubleSlash,
    Name(String),
    Number(isize),
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
            Some(ch @ '0'...'9') => {
                let mut s = String::new();

                s.push(ch);

                loop {
                    match self.chars.next() {
                        Some(ch @ '0'...'9') => s.push(ch),
                        Some(other) => {
                            self.buffer = Some(other);
                            return Some(Token::Number(s.parse::<isize>().expect("Failed to parse digits")));
                        },
                        None => {
                            self.eoffed = true;
                            return Some(Token::Number(s.parse::<isize>().expect("Failed to parse digits")));
                        }
                    }
                }

            },
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
