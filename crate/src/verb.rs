//! Structs for verbs.
//!
//! This module provides the [Verb] struct.
//!
//! # Examples
//!
//! ```
//! use en_inflexion::verb::Verb;
//! use en_inflexion::term::Term; // Provides many of the interesting methods
//!
//! let verb = Verb::new("eats");
//! let plural = verb.plural();
//! assert_eq!(plural, "eat");
//!
//! let plural = verb.singular();
//! assert_eq!(plural, "eats");
//!
//! assert!(verb.is_singular());
//! assert!(!verb.is_plural());
//! ```
use crate::{ll::verbs as ll, term::Term, util};
use std::borrow::Cow;

/// A `Verb` is a single verb which will be pluralized to its modern form.
#[derive(Debug)]
pub struct Verb<'a>(&'a str);

impl<'a> Term<'a> for Verb<'a> {
    /// Creates a new verb from a string. Note that nothing in the code
    /// actually ensures that this is a verb, so you could write
    /// `Verb::new("food")` and it would work, for some value of
    /// "work". Actually parsing and understanding English is well beyond the
    /// remit of this crate.
    fn new(verb: &'a str) -> Self {
        Self(verb)
    }

    fn singular(&self) -> Cow<'a, str> {
        let new = ll::convert_to_singular(self.0);
        if self.0 != new {
            return util::fix_case(self.0, new);
        }
        new
    }

    fn plural(&self) -> Cow<'a, str> {
        let new = ll::convert_to_plural(self.0);
        if self.0 != new {
            return util::fix_case(self.0, new);
        }
        new
    }

    fn is_singular(&self) -> bool {
        ll::is_singular(self.0)
    }

    fn is_plural(&self) -> bool {
        ll::is_plural(self.0)
    }
}

// impl<'a> Verb<'a> {
//     fn person(&self, person: Person) -> Cow<'a, str> {
//         self.singular()
//     }
// }
