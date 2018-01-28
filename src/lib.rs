#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
extern crate quick_xml;

use std::io::BufRead;
use std::str;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::Entry;
use std::collections::VecDeque;

use quick_xml::reader::Reader;
use quick_xml::events::{Event, BytesStart, BytesEnd, BytesText};
use quick_xml::events::attributes::Attributes;
use quick_xml::events::attributes::Attribute;

// FIXME: it'd be great if quick_xml provided these
trait Positioning {
    fn start_tag_span(&self, tag: &BytesStart, empty: bool) -> Span;
    fn end_tag_span(&self, tag: &BytesEnd) -> Span;
}

impl<R: BufRead> Positioning for Reader<R> {
    fn start_tag_span(&self, e: &BytesStart, empty: bool) -> Span {
        let pos = self.buffer_position();

        let end = pos - 1;
        let start = end - e.len() - 1 - if empty { 1 } else { 0 };

        Span::new(start, end)
    }

    fn end_tag_span(&self, e: &BytesEnd) -> Span {
        let pos = self.buffer_position();

        let end = pos - 1;
        let start = end - e.len() - 2;

        Span::new(start, end)
    }
}

// These could be Rc<[u8]> or Rc<Vec<u8>>
type Bytes = Vec<u8>;

#[derive(Debug, Eq, Clone)]
struct OrderedElement {
    // path to this element, as in the list of ancestors including the name of this element.
    path: Vec<Bytes>,
    // "order" as in for our path, what are the order numbers of our path elements.
    // For example given a path `/a/b/c` and counts Some(vec![1, 2, 3]) means
    // this element is:
    //
    // ```
    // <a>
    //   <b><!-- anything --></b>
    //   <b>
    //      <c><!-- HERE --></c>
    //   </b>
    //   <!-- anything -->
    // </a>
    // ```
    //
    // `None` or shorter (from the end) order is given for the OrderedElement values
    // read from the example to ensure matching only the children of a particular parent.
    order: Option<Vec<usize>>,
    // attributes from the input or example.
    attributes: HashMap<Bytes, Bytes>,
    // precalculated hash of attributes in some stable order
    attr_hash: u64,
    // was this element empty; turns out this might not be so interesting
    empty: bool
}

impl OrderedElement {
    fn new(path: Vec<Bytes>, order: Vec<usize>, attributes: HashMap<Bytes, Bytes>, empty: bool) -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;
        use std::hash::Hash;

        let mut hasher = DefaultHasher::default();

        {
            let mut tmp = attributes.iter().collect::<Vec<_>>();
            tmp.sort_by(|a, b| a.0.cmp(b.0));
            tmp.hash(&mut hasher);
        }

        let attr_hash = hasher.finish();

        OrderedElement {
            path,
            order: Some(order),
            attributes,
            attr_hash,
            empty
        }
    }

    fn take_order(&mut self) -> Option<Vec<usize>> {
        self.order.take()
    }

    fn set_order(&mut self, order: Vec<usize>) {
        self.order = Some(order);
    }
}

impl std::hash::Hash for OrderedElement {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
        //self.order.hash(state);
        self.attr_hash.hash(state);

        // since these comparisons are (hopefully) mostly done to find the original using an
        // example, empty is not included here as it's not included in PartialEq.
    }
}

impl PartialEq for OrderedElement {
    fn eq(&self, other: &OrderedElement) -> bool {
        use std::cmp::min;

        match (self.order.as_ref(), other.order.as_ref()) {
            (Some(our_order), Some(other_order)) => {
                if our_order.len() == other_order.len() && our_order != other_order {
                    return false;
                }

                let smaller_len = min(our_order.len(), other_order.len());
                assert!(smaller_len > 0);

                if &our_order[..smaller_len] != &other_order[..smaller_len] {
                    return false;
                }
            }
            _ => {
                // allow wildcard matching
            },
        }

        self.path == other.path
            && self.attr_hash == other.attr_hash
            && self.attributes == other.attributes
    }
}

mod xpath {
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

}

pub struct ReorderingOptionsBuilder {
    filtered_attributes: Vec<(xpath::Selector, Cow<'static, str>)>,
}

impl ReorderingOptionsBuilder {
    pub fn new() -> Self {
        ReorderingOptionsBuilder {
            filtered_attributes: Vec::new(),
        }
    }

    pub fn add_filtered_attribute(&mut self, xpath: &str, attr: &str) {
        self.filtered_attributes.push((xpath::Selector::parse(xpath), Cow::Owned(String::from(attr))));
    }


    pub fn build(self) -> ReorderingOptions<'static> {
        ReorderingOptions::new(self.filtered_attributes)
    }
}

#[derive(Default)]
pub struct ReorderingOptions<'a> {
    selectors: Vec<xpath::Selector>,
    matches: Vec<bool>,
    filtered_attributes: Vec<(usize, Cow<'a, str>)>,
    matched_input: bool,
}

impl<'a> ReorderingOptions<'a> {
    fn new(filtered_attributes: Vec<(xpath::Selector, Cow<'a, str>)>) -> Self {

        let mut selectors = Vec::new();
        let mut matches = Vec::new();
        let mut filtered = Vec::new();

        for (i, (sel, attr)) in filtered_attributes.into_iter().enumerate() {
            selectors.push(sel);
            matches.push(false);
            filtered.push((i, attr));
        }

        ReorderingOptions {
            selectors,
            matches,
            filtered_attributes: filtered,
            matched_input: false,
        }
    }

    fn on_input_event(&mut self, pos: &Positioning, evt: &Event) {
        for (i, selector) in self.selectors.iter_mut().enumerate() {
            selector.update_on_event(pos, evt);

            match *evt {
                Event::Start(_) |
                Event::Empty(_) => self.matches[i] = selector.is_match(),
                _ => self.matches[i] = false,
            }
        }

        self.matched_input = true;
    }

    fn on_example_event(&mut self, pos: &Positioning, evt: &Event) {
        self.on_input_event(pos, evt);
        self.matched_input = false;
    }

    fn filter_input_attributes(&self, attributes: &mut HashMap<Bytes, Bytes>) {
        assert!(self.matched_input);

        for (i, m) in self.matches.iter().enumerate() {
            if *m {
                attributes.remove(self.filtered_attributes[i].1.as_bytes());
            }
        }
    }

    fn filter_example_attributes(&self, attributes: &mut HashMap<Bytes, Bytes>) {
        assert!(!self.matched_input);

        for (i, m) in self.matches.iter().enumerate() {
            if *m {
                attributes.remove(self.filtered_attributes[i].1.as_bytes());
            }
        }
    }
}

pub fn reorder<A: BufRead, B: BufRead>(mut example: Reader<A>, mut input: Reader<B>, mut options: ReorderingOptions) -> Vec<Span> {

    let mut buffer = Vec::new();

    // child spans of a parent which need to be flushed when the parent is closing
    //let mut child_spans: HashMap<OrderedElement, VecDeque<(Span, Option<Span>)>> = HashMap::new();

    let mut input_spans = {
        // input spans for <b> in order in which they appeared
        let mut input_spans: HashMap<OrderedElement, VecDeque<Span>> = HashMap::new();

        let mut input_tracker = Tracker::new();

        let mut missing_end: Vec<(OrderedElement, Span, Vec<Span>)> = Vec::new();

        loop {
            {
                let evt = input.read_event(&mut buffer).unwrap();
                input_tracker.on_event(&input, &evt);
                options.on_input_event(&input, &evt);

                match evt {
                    Event::Eof => break,
                    Event::Text(ref e) if e.len() == 0 => continue,
                    Event::Start(ref e) => {
                        let mut attrs = convert_attributes(e.attributes());
                        options.filter_input_attributes(&mut attrs);

                        let key = convert_orderedelement(&input_tracker, attrs, false);
                        let start = input_tracker.last_span().unwrap();

                        if let Some(&mut (_, _, ref mut siblings)) = missing_end.last_mut() {
                            siblings.push(start.clone());
                        }

                        missing_end.push((key, start, Vec::new()));
                    },
                    Event::Empty(ref e) => {
                        let mut attrs = convert_attributes(e.attributes());
                        options.filter_input_attributes(&mut attrs);

                        let key = convert_orderedelement(&input_tracker, attrs, true);

                        let span = input_tracker.last_span().unwrap();

                        let mut spans = VecDeque::with_capacity(1);
                        spans.push_back(span.clone());

                        assert!(input_spans.insert(key, spans).is_none());

                        if let Some(&mut (_, _, ref mut siblings)) = missing_end.last_mut() {
                            siblings.push(span);
                        }
                    },
                    Event::End(ref e) => {
                        let end = input_tracker.last_span().unwrap();
                        let (key, start, children) = missing_end.pop().unwrap();

                        let mut spans = VecDeque::new();
                        spans.push_back(start.clone());

                        for child_span in children {
                            spans.push_back(child_span);
                        }

                        spans.push_back(end.clone());

                        // these should be unique since we have the order for each
                        assert!(input_spans.insert(key, spans).is_none());

                        if let Some(&mut (_, _, ref mut siblings)) = missing_end.last_mut() {
                            siblings.push(end);
                        }
                    },
                    // not handling any of these as we are ensuring they get written by growing
                    // span starts as a last step ... that does cause the need to sort everything
                    /*Event::Text(ref e) |
                    Event::Comment(ref e) |
                    Event::CData(ref e) => {
                        if e.len() > 0 {
                            if let Some((key, mut start)) = missing_end.pop() {
                                let span = input.any_text_span(Some(&start), e).unwrap();

                                start.fuse(&span);
                                missing_end.push((key, start));
                            } else {
                                order.push(input.any_text_span(None, e).unwrap());
                            }
                        }
                    }*/
                    _ => {},
                }
            }

            buffer.clear();
        }
        input_spans
    };

    let mut tracker = Tracker::new();
    let mut order: Vec<Span> = Vec::new();
    let mut order_spans: HashSet<Span> = HashSet::new();

    // pushed on open, popped on end
    // search key, number of elements in the order at <start> time and matched nodes counters
    let mut stack: Vec<(OrderedElement, usize, Option<Vec<usize>>)> = Vec::new();

    loop {
        {
            let evt = example.read_event(&mut buffer).unwrap();

            options.on_example_event(&example, &evt);
            tracker.on_event(&example, &evt);

            match evt {
                Event::Eof => break,
                Event::Text(ref e) if e.len() == 0 => continue,
                Event::Start(ref e) => {
                    let mut attrs = convert_attributes(e.attributes());
                    options.filter_example_attributes(&mut attrs);
                    // TODO: filter attributes
                    let mut key = convert_orderedelement(&tracker, attrs, false);

                    let try_matching = if tracker.parent_counts().len() > 1 {
                        // looking at non-root
                        if let Some(prefix) = stack.last().as_ref().and_then(|t| t.2.clone()) {
                            key.set_order(prefix);
                            true
                        } else {
                            false
                        }
                    } else {
                        key.take_order();
                        // root should be easy
                        true
                    };

                    let matching_order = if try_matching {
                        // TODO: this clone shouldn't be deep at least
                        match input_spans.entry(key.clone()) {
                            Entry::Occupied(mut o) => {
                                // push our start tag already
                                let start = o.get_mut().pop_front().expect("At least our start tag must be found, if nothing else");
                                order.push(start.clone());
                                assert!(order_spans.insert(start));

                                // this order is to make sure the descendants we find are the
                                // actual descendants of this node, not from some other tree
                                o.key().order.clone()
                            },
                            _ => {
                                None
                                // we should probably skip the whole subtree here?
                                //panic!("Could not find unique element for {:?}", key);
                            },
                        }
                    } else {
                        // no prefix, we are skipping
                        None
                    };

                    stack.push((key, order.len(), matching_order));
                },
                Event::Empty(ref e) => {
                    let mut attrs = convert_attributes(e.attributes());
                    options.filter_example_attributes(&mut attrs);

                    let mut key = convert_orderedelement(&tracker, attrs, true);

                    if let Some(prefix) = stack.last().as_ref().and_then(|t| t.2.clone()) {
                        key.set_order(prefix);

                        if let Some(spans) = input_spans.remove(&key) {
                            for span in spans {
                                order.push(span.clone());
                                assert!(order_spans.insert(span));
                            }
                        } else {
                            // otherwise we just skipped the whole subtree
                        }
                    } else {
                        // skipping
                    };
                },
                Event::End(ref e) => {
                    let (key, insert_at, prefix) = stack.pop().unwrap();

                    if prefix.is_some() {

                        let spans = input_spans.remove(&key)
                            .expect("Since prefix was resolved, expected there to be a match");

                        for span in spans {
                            if !order_spans.insert(span.clone()) {
                                continue;
                            }

                            order.push(span);
                        }

                    } else {
                        // we were skipping
                    }

                }
                _ => {},
            }
        }

        buffer.clear();
    }

    {
        let mut holes = order.iter().cloned().enumerate().collect::<Vec<_>>();
        holes.sort_unstable_by_key(|&(_, ref span)| span.start);

        assert_eq!(holes[0].0, 0);
        order[0].start = 0;
        let mut last = order[0].end;
        let mut prev_index = 0;

        for (i, _) in holes.into_iter().skip(1) {
            let span = order[i].clone();

            if span.start > last + 1 {
                // in some cases, it might make more sense to bind to previous, when we are
                // descending for example

                order[i].start = order[prev_index].end + 1;
                //order[prev_index].end = order[i].start - 1;
            }

            assert!(order[prev_index].start < order[prev_index].end);
            assert!(order[i].start > order[prev_index].end);
            assert!(order[i].start < order[i].end);

            last = order[i].end;
            prev_index = i;
        }
    }

    order
}

fn record_span(order: &mut Vec<Span>, insert_at: usize, spans: Option<(Span, Option<Span>)>) {
    match spans {
        Some((mut start, Some(end))) => {

            if insert_at == order.len() {
                // leaves
                start.join(&end);
                order.push(start);
                return;
            }

            if !order.get_mut(insert_at).map(|x| x.try_fuse(&start)).unwrap_or(false) {
                order.insert(insert_at, start);
            }

            let last_index = order.len() - 1;
            if !order[last_index].try_fuse(&end) {
                order.push(end);
            }
        },
        Some((start, None)) => {
            let last_index = order.len() - 1;
            if !order[last_index].try_fuse(&start) {
                order.push(start);
            }
        },
        None => {},
    }
}

fn record_found<V: std::fmt::Debug>(h: &mut HashMap<OrderedElement, VecDeque<V>>, key: OrderedElement, value: V) {
    match h.entry(key) {
        Entry::Occupied(mut o) => { o.get_mut().push_back(value); },
        Entry::Vacant(v) => {
            let mut deque = VecDeque::new();
            deque.push_back(value);
            v.insert(deque);
        },
    }
}

fn pop_found<V>(h: &mut HashMap<OrderedElement, VecDeque<V>>, key: OrderedElement) -> Option<V> {
    let ret;

    match h.get_mut(&key) {
        Some(ref mut deque) => {
            ret = deque.pop_front();

            if !deque.is_empty() {
                return ret;
            }
        },
        None => return None,
    }

    h.remove(&key);
    ret
}

fn convert_orderedelement(tracker: &Tracker, attrs: HashMap<Bytes, Bytes>, empty: bool) -> OrderedElement {
    let path = tracker.path().iter().cloned().collect::<Vec<_>>();
    let order = tracker.parent_counts().iter().cloned().collect::<Vec<_>>();

    OrderedElement::new(path, order, attrs, empty)
}

fn convert_attributes<'a>(it: Attributes<'a>) -> HashMap<Bytes, Bytes> {
    let mut ret = HashMap::new();

    for attr in it {
        let attr = attr.unwrap();
        let key = attr.key.to_vec();
        let value = attr.value.into_owned();
        let old = ret.insert(key, value);

        assert_eq!(old, None);
    }

    ret.shrink_to_fit();
    ret
}

#[derive(Debug, Clone, Copy)]
struct TrackerSnapshot<'a> {
    pub path: &'a [Vec<u8>],
    pub counts: &'a [usize],
    pub last_span: Option<&'a Span>,
}

impl<'a> TrackerSnapshot<'a> {
    // without first values
    fn tail(&self) -> TrackerSnapshot<'a> {
        TrackerSnapshot {
            path: &self.path[1..],
            counts: &self.counts[1..],
            last_span: self.last_span.clone(),
        }
    }
}

struct Tracker {
    recycler: Vec<Vec<u8>>,
    path: Vec<Vec<u8>>,
    counters: Vec<usize>,
    last_span: Option<Span>,
    at_empty: bool,
}

impl Tracker {
    fn new() -> Self {
        Tracker {
            recycler: Vec::new(),
            path: Vec::new(),
            counters: vec![0],
            last_span: None,
            at_empty: false,
        }
    }

    fn snapshot(&self) -> TrackerSnapshot {
        TrackerSnapshot {
            path: self.path.as_ref(),
            counts: self.counters.as_ref(),
            last_span: self.last_span.as_ref(),
        }
    }

    fn path(&self) -> &[Vec<u8>] {
        self.path.as_ref()
    }

    fn counts(&self) -> &[usize] {
        self.counters.as_ref()
    }

    fn parent_counts(&self) -> &[usize] {
        let len = self.counters.len();
        &self.counters[0..(len - 1)]
    }

    fn last_span(&self) -> Option<Span> {
        self.last_span.clone()
    }

    fn on_event(&mut self, pos: &Positioning, event: &Event) {
        if self.at_empty {
            self.pop_path(None);
            self.last_span = None;
            self.at_empty = false;
        }

        match *event {
            Event::Start(ref e) => {
                self.push_path(e.name());

                if let Some(last) = self.counters.last_mut() {
                    *last += 1;
                }

                self.counters.push(0); // does not yet have any children
                self.last_span = Some(pos.start_tag_span(e, false));
                //self.spans.push(pos.start_tag_span(e, false));
            },
            Event::Empty(ref e) => {
                self.at_empty = true;

                self.push_path(e.name());

                if let Some(last) = self.counters.last_mut() {
                    *last += 1;
                }

                self.last_span = Some(pos.start_tag_span(e, true));

                //self.spans.push(pos.start_tag_span(e, true));
                //return Some(self.spans.last().cloned().unwrap());
            },
            Event::End(ref e) => {
                self.pop_path(Some(e.name()));

                self.counters.pop().unwrap();

                let end = pos.end_tag_span(e);
                self.last_span = Some(end);

                //return Some((start.0, end.1));
            },
            _ => {},
        }
    }

    fn push_path(&mut self, name: &[u8]) {
        let name = if let Some(mut ready) = self.recycler.pop() {
            ready.extend(name);
            ready
        } else {
            name.to_vec()
        };

        self.path.push(name);
    }

    fn pop_path(&mut self, name: Option<&[u8]>) {
        let mut popped = self.path.pop().unwrap();
        if let Some(name) = name {
            assert_eq!(popped.as_slice(), name);
        }
        popped.clear();
        self.recycler.push(popped);
    }
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl PartialEq<(usize, usize)> for Span {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.start == other.0 && self.end == other.1
    }
}

impl Span {
    fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    fn connected_with(&self, other: &Span) -> bool {
        self.start == other.end + 1 || (other.start > 0 && self.end == other.start - 1)
    }

    fn fuse(&mut self, other: &Span) {
        if self.start == other.end + 1 {
            self.start = other.start;
        } else if other.start > 0 && self.end == other.start - 1 {
            self.end = other.end;
        } else {
            assert!(false, "Cannot fuse {:?} with unconnected {:?}", self, other);
        }
    }

    fn try_fuse(&mut self, other: &Span) -> bool {
        if self.connected_with(other) {
            self.fuse(other);
            true
        } else {
            false
        }
    }

    fn join(&mut self, other: &Span) {
        use std::cmp;

        self.start = cmp::min(self.start, other.start);
        self.end = cmp::max(self.end, other.end);
    }

    pub fn to_range(&self) -> std::ops::Range<usize> {
        self.start..self.end + 1
    }
}

#[cfg(test)]
mod tests {
    use std::str;
    use std::collections::HashMap;
    use std::fmt::Display;
    use super::{reorder, convert_attributes, convert_orderedelement};
    use super::{Span, Tracker, Positioning, OrderedElement, ReorderingOptions, ReorderingOptionsBuilder};
    use super::xpath::Selector;
    use quick_xml::reader::Reader;
    use quick_xml::events::{Event, BytesStart};

    #[test]
    fn test_positioning() {
        let mut reader = Reader::from_str(r#"<a><b /><c some="id" other="42"   >end</c> </a>"#);
        //                                             1         2         3         4
        //                                   0123456789012345678901234567890123456789012345

        let mut spans = Vec::new();
        let mut buffer = Vec::new();
        loop {
            match reader.read_event(&mut buffer).unwrap() {
                Event::Start(ref e) => spans.push(reader.start_tag_span(e, false)),
                Event::Empty(ref e) => spans.push(reader.start_tag_span(e, true)),
                Event::End(ref e) => spans.push(reader.end_tag_span(e)),
                /*Event::Text(ref e) if e.len() > 0 => {
                    let span = reader.any_text_span(spans.last(), e);
                    spans.push(span.unwrap());
                },*/
                Event::Eof => break,
                _ => {}
            }
            buffer.clear();
        }

        assert_eq!(
            spans,
            vec![
                (0, 2),
                (3, 7),
                (8, 34),
                (38, 41),
                (43, 46)
            ]);
    }

    #[test]
    fn test_orderedelement_hashing() {
        let input = b"a id=\"foo\"";

        let attr = convert_attributes(BytesStart::borrowed(input, 1).attributes());

        let oe1 = OrderedElement::new(vec![b"foo".to_vec(), b"bar".to_vec(), b"a".to_vec()], vec![1, 1, 1, 0], attr.clone(), false);
        let oe2 = OrderedElement::new(vec![b"foo".to_vec(), b"bar".to_vec(), b"a".to_vec()], vec![1, 1, 1, 0], attr.clone(), false);
        let oe3 = OrderedElement::new(vec![b"foo".to_vec(), b"bar".to_vec(), b"a".to_vec()], vec![1, 1, 1, 0], attr, true);

        assert_eq!(oe1, oe2);
        assert_eq!(oe2, oe3);

        let mut map = HashMap::new();
        assert_eq!(map.insert(oe1, 0), None);
        assert_eq!(map.insert(oe2, 1), Some(0));
        assert_eq!(map.insert(oe3, 2), Some(1));
    }

    #[test]
    fn test_tracking() {
        let mut reader = Reader::from_str(r#"<a><b /><b><c><d></d><e></e><f></f></c></b><c some="id" other="42"   >end</c> </a>"#);
        let mut tracker = Tracker::new();

        let mut paths = Vec::new();
        let mut counts = Vec::new();

        let mut buffer = Vec::new();
        let mut s = String::new();
        loop {
            {
                let evt = reader.read_event(&mut buffer).unwrap();
                tracker.on_event(&reader, &evt);
                match evt {
                    Event::Eof => break,
                    Event::Text(ref e) if e.len() == 0 => continue,
                    _ => {},
                }
            }

            for segment in tracker.path() {
                s.push_str("/");
                s.push_str(str::from_utf8(segment).unwrap());
            }

            paths.push(s.clone());
            counts.push(tracker.counts().iter().cloned().collect::<Vec<_>>());

            s.clear();
            buffer.clear();
        }

        assert_eq!(
            paths,
            vec![
                "/a",
                "/a/b",
                "/a/b",
                "/a/b/c",
                "/a/b/c/d",
                "/a/b/c",
                "/a/b/c/e",
                "/a/b/c",
                "/a/b/c/f",
                "/a/b/c",
                "/a/b",
                "/a",
                "/a/c",
                "/a/c",
                "/a",
                "/a",
                "",
            ]);

        assert_eq!(
            counts,
            vec![
                vec![1, 0],
                vec![1, 1], // empty element does not create child counter
                vec![1, 2, 0],
                vec![1, 2, 1, 0],
                vec![1, 2, 1, 1, 0],
                vec![1, 2, 1, 1],
                vec![1, 2, 1, 2, 0],
                vec![1, 2, 1, 2],
                vec![1, 2, 1, 3, 0],
                vec![1, 2, 1, 3],
                vec![1, 2, 1],
                vec![1, 2],
                vec![1, 3, 0],
                vec![1, 3, 0],
                vec![1, 3],
                vec![1, 3],
                vec![1],
            ]);
    }

    trait StringExt {
        fn join<T: Display>(&self, slice: &[T]) -> String;
    }

    impl<'a> StringExt for &'a str {
        fn join<T: Display>(&self, slice: &[T]) -> String {
            use std::fmt::Write;
            let mut out = String::with_capacity(slice.len() * 2 * self.len());
            for (i, item) in slice.iter().enumerate() {
                write!(out, "{}", item).unwrap();
                if i < slice.len() - 1 {
                    out.push_str(self);
                }
            }
            out
        }
    }

    #[test]
    fn test_reordering() {
        use std::cmp;
        // xpath --> Order(start_tagspan, end_tagspan)
        //
        // similarity:
        //   - attributes ... in any order?

        let example = r#"<a><b><c id="1">c1</c><c id="2">c2</c><c id="3">orig</c></b></a>"#;
        let input   = r#"<!-- foo --><a><b><c id="2">c2</c><c id="3"><![CDATA[mod]]></c><c id="1">c1</c></b></a>"#;
        //                         11111111112222222222333333333344444444445555555555666666666677777
        //               012345678901234567890123456789012345678901234567890123456789012345678901234
        //
        let output  = r#"<!-- foo --><a><b><c id="1">c1</c><c id="2">c2</c><c id="3"><![CDATA[mod]]></c></b></a>"#;

        let actual = super::reorder(Reader::from_str(example), Reader::from_str(input), ReorderingOptions::default());

        assert_eq!(render(input, &actual), output);
    }

    #[test]
    fn test_reordering_with_indentation() {
        let example = r#"<a><b><c id="1">c1</c><c id="2">c2</c><c id="3">orig</c></b></a>"#;
        let input   = r#"<!-- foo -->
<a>
    <b>
        right after b-open
        <c id="2">
          c2
        </c>
        after 2
        <c id="3">
          <![CDATA[
            mod
          ]]>
        </c>
        before 1
        <c id="1">
          c1
        </c>
        before b-closes
    </b>
</a>"#;

        // keeping the "right after b-open" near <b> would require keeping some ... depth_delta
        // between spans, like:
        //   1, 1, 0, 0, 0, -1, -1

        let output  = r#"<!-- foo -->
<a>
    <b>
        before 1
        <c id="1">
          c1
        </c>
        right after b-open
        <c id="2">
          c2
        </c>
        after 2
        <c id="3">
          <![CDATA[
            mod
          ]]>
        </c>
        before b-closes
    </b>
</a>"#;

        let actual = super::reorder(Reader::from_str(example), Reader::from_str(input), ReorderingOptions::default());
        for span in &actual {
            println!("{:?}", span);
        }

        let s = render(input, &actual);
        println!("{}", s);

        assert_eq!(s, output);
    }

    #[test]
    fn test_filtering_attributes_to_match() {
        let example = r#"<a ver="toolversion1"><b ver="toolversion1"><c id="1">c1</c><c id="2">c2</c><c id="3">orig</c></b></a>"#;
        let input   = r#"<a ver="toolversion2"><b ver="toolversion2"><c id="2">c2</c><c id="3"><![CDATA[mod]]></c><c id="1">c1</c></b></a>"#;
        let output  = r#"<a ver="toolversion2"><b ver="toolversion2"><c id="1">c1</c><c id="2">c2</c><c id="3"><![CDATA[mod]]></c></b></a>"#;

        let mut options = ReorderingOptionsBuilder::new();

        options.add_filtered_attribute("/a", "ver");
        options.add_filtered_attribute("/a/b", "ver");

        let actual = super::reorder(Reader::from_str(example), Reader::from_str(input), options.build());

        assert_eq!(render(input, &actual), output);
    }

    #[test]
    fn test_it_flushes_extras_at_right_level() {
        let example = r#"<a><b><c id="1">c1</c></b></a>"#;
        let input   = r#"<a><inserted>before</inserted><b><c id="2">c2</c><c id="3"><![CDATA[mod]]></c><c id="1">c1</c></b><b id="more_extra"><just>something</just></b></a>"#;
        let output  = r#"<a><b><c id="1">c1</c><c id="2">c2</c><c id="3"><![CDATA[mod]]></c></b><inserted>before</inserted><b id="more_extra"><just>something</just></b></a>"#;
        // c[@id = "2"] and c[@id = "3"] should be output in the order in which they appear
        // outer <inserted> and <b id="more_extra"> are also flushed in the same order, but at
        // different time

        let actual = super::reorder(Reader::from_str(example), Reader::from_str(input), ReorderingOptions::default());

        assert_eq!(render(input, &actual), output);
    }

    #[test]
    fn test_it_reorders_with_empties_in_example() {
        let example = r#"<a><b><c id="1" /></b></a>"#;
        let input   = r#"<a><inserted>before</inserted><b><c id="2">c2</c><c id="3"><![CDATA[mod]]></c><c id="1">c1</c></b><b id="more_extra"><just>something</just></b></a>"#;
        let output  = r#"<a><b><c id="1">c1</c><c id="2">c2</c><c id="3"><![CDATA[mod]]></c></b><inserted>before</inserted><b id="more_extra"><just>something</just></b></a>"#;

        let actual = super::reorder(Reader::from_str(example), Reader::from_str(input), ReorderingOptions::default());

        assert_eq!(render(input, &actual), output);
    }

    fn render(input: &str, spans: &Vec<Span>) -> String {
        let mut s = String::new();

        for span in spans {
            let range  = span.to_range();
            s.push_str(&input[range]);
        }

        s
    }

}
