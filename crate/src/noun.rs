//! Structs for nouns.
//!
//! This module provides two structs, [Noun] and [ClassicalNoun]. The only
//! difference between them is that `ClassicalNoun` pluralizes to the
//! classical form rather than modern.
//!
//! # Examples
//!
//! ```
//! use en_inflexion::noun::Noun;
//! use en_inflexion::term::Term; // Provides most of the interesting methods
//!
//! let noun = Noun::new("dog");
//! let plural = noun.plural();
//! assert_eq!(plural, "dogs");
//!
//! let plural = noun.singular();
//! assert_eq!(plural, "dog");
//!
//! assert!(noun.is_singular());
//! assert!(!noun.is_plural());
//! ```
use crate::{ll::nouns as ll, term::Term, util};
use std::borrow::Cow;

/// A `Noun` is a single noun which will be pluralized to its modern form.
#[derive(Debug)]
pub struct Noun<'a>(&'a str);

impl<'a> Term<'a> for Noun<'a> {
    /// Creates a new noun from a string. Note that nothing in the code
    /// actually ensures that this is a noun, so you could write
    /// `Noun::new("eat")` and it would work, for some value of
    /// "work". Actually parsing and understanding English is well beyond the
    /// remit of this crate.
    fn new(noun: &'a str) -> Self {
        Self(noun)
    }

    fn singular(&self) -> Cow<'a, str> {
        let new = ll::convert_to_singular(self.0);
        if self.0 != new {
            return util::fix_case(self.0, new);
        }
        new
    }

    fn plural(&self) -> Cow<'a, str> {
        let new = ll::convert_to_modern_plural(self.0);
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

/// A `ClassicalNoun` is a single noun which will be pluralized to its
/// classical form.
pub struct ClassicalNoun<'a>(&'a str);

impl<'a> Term<'a> for ClassicalNoun<'a> {
    /// Creates a new noun from a string. This noun will use the classical
    /// plural form.
    fn new(noun: &'a str) -> Self {
        Self(noun)
    }

    fn singular(&self) -> Cow<'a, str> {
        let new = ll::convert_to_singular(self.0);
        if self.0 != new {
            return util::fix_case(self.0, new);
        }
        new
    }

    fn plural(&self) -> Cow<'a, str> {
        let new = ll::convert_to_classical_plural(self.0);
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

#[cfg(test)]
mod tests {
    use crate::term::Term;

    #[test]
    fn noun() {
        let noun = super::Noun::new("staff");
        assert_eq!(noun.plural(), "staffs");
        assert_eq!(noun.singular(), "staff");
        assert!(noun.is_singular());
        assert!(!noun.is_plural());

        let noun = super::Noun::new("staffs");
        assert_eq!(noun.plural(), "staffs");
        assert_eq!(noun.singular(), "staff");
        assert!(!noun.is_singular());
        assert!(noun.is_plural());

        let noun = super::Noun::new("Staff");
        assert_eq!(noun.plural(), "Staffs");
        assert_eq!(noun.singular(), "Staff");
        assert!(noun.is_singular());
        assert!(!noun.is_plural());

        let noun = super::Noun::new("Staffs");
        assert_eq!(noun.plural(), "Staffs");
        assert_eq!(noun.singular(), "Staff");
        assert!(!noun.is_singular());
        assert!(noun.is_plural());

        let noun = super::Noun::new("Man-at-Arms");
        assert_eq!(noun.plural(), "Men-at-Arms");

        let noun = super::Noun::new("sheep");
        assert!(noun.is_singular());
        assert!(noun.is_plural());
    }

    #[test]
    fn classical_noun() {
        let noun = super::ClassicalNoun::new("staff");
        assert_eq!(noun.plural(), "staves");
        assert_eq!(noun.singular(), "staff");
        assert!(noun.is_singular());
        assert!(!noun.is_plural());

        let noun = super::ClassicalNoun::new("staves");
        assert_eq!(noun.plural(), "staves");
        assert_eq!(noun.singular(), "stave");
        assert!(!noun.is_singular());
        assert!(noun.is_plural());

        let noun = super::ClassicalNoun::new("Staff");
        assert_eq!(noun.plural(), "Staves");
        assert_eq!(noun.singular(), "Staff");
        assert!(noun.is_singular());
        assert!(!noun.is_plural());

        let noun = super::ClassicalNoun::new("Staves");
        assert_eq!(noun.plural(), "Staves");
        assert_eq!(noun.singular(), "Stave");
        assert!(!noun.is_singular());
        assert!(noun.is_plural());
    }
}
