use crate::{ll::nouns as ll, term::Term, util};
use std::borrow::Cow;

pub struct Adjective<'a>(&'a str);

impl<'a> Term<'a> for Adjective<'a> {
    /// Creates a new adjective from a string. Note that nothing in the code
    /// actually ensures that this is a adjective, so you could write
    /// `Adjective::new("bird")` and it would work, for some value of
    /// "work". Actually parsing and understanding English is well beyond the
    /// remit of this crate.
    fn new(adj: &'a str) -> Self {
        Self(adj)
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
