use std::collections::HashMap;
use std::hash::{Hash, Hasher};


use super::Bytes;

#[derive(Debug, Eq, Clone)]
pub(crate) struct OrderedElement {
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
    pub(crate) fn new(path: Vec<Bytes>, order: Vec<usize>, attributes: HashMap<Bytes, Bytes>, empty: bool) -> Self {
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

    pub(crate) fn take_order(&mut self) -> Option<Vec<usize>> {
        self.order.take()
    }

    pub(crate) fn set_order(&mut self, order: Vec<usize>) {
        self.order = Some(order);
    }

    pub(crate) fn clone_order_in_tree(&self) -> Option<Vec<usize>> {
        self.order.clone()
    }
}

impl Hash for OrderedElement {
    fn hash<H: Hasher>(&self, state: &mut H) {
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::OrderedElement;
    use quick_xml::events::BytesStart;
    use super::super::convert_attributes;

    #[test]
    fn hash_partialeq() {
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
}
