// #![deny(missing_docs)]
#![deny(rustdoc::private_intra_doc_links)]
#![deny(rustdoc::broken_intra_doc_links)]

//! Inflect English nouns, verbs, adjectives, and articles.
//!
//! This crate is based on the Perl distribution
//! [Lingua-EN-Inflexion](https://metacpan.org/dist/Lingua-EN-Inflexion),
//! created by Damian Conway. It uses the same source data and algorithms
//! (mostly), and some of the documentation comes from that distribution as
//! well.
//!
//! Per Damian's documentation of the original Perl distribution: "This
//! \[crate\] allows you to properly inflect English nouns and verbs, as well
//! a small number of adjectives and articles ("a", "an", "the") that still
//! decline in modern English.
//!
//! "By default, the crate follows the conventions of modern formal British
//! English (i.e. OED and Fowler's), but also attempts to support other
//! dialects as far as possible. The rules of inflexion it uses are almost
//! entirely table-driven, so they can easily be adapted for local
//! requirements if necessary.
//!
//! "Where an English noun has both a modern and a classical/unassimilated
//! plural form (e.g. "maximums" and "maxima", "indexes" and "indices",
//! "librettos" and "libretti"), the module favours the modern inflexion,
//! unless the older form is specifically requested (see "classical() and
//! unassimilated()").
//!
//! "In the few cases where a word has two or more singular inflexions
//! (e.g. plural "bases" to singular "base" or "basis") or is otherwise
//! ambiguous (e.g. plural "opera" to singular "opus" vs singular "opera" to
//! plural "operas"), the module provides a best guess about the more common
//! usage. These guesses can be changed by rearranging the source tables."
//!
//! # Source Data
//!
//! This crate's source data comes from the Perl distribution
//! Lingua-EN-Inflexion, created and maintained by Damian Conway. Damian has
//! written about a number of linguistic issues and why certain inflections
//! are the way they are. See
//! <https://metacpan.org/pod/Lingua::EN::Inflexion#LINGUISTIC-ABYSSES> for
//! more details. Please read this before submitting bug reports about this
//! data.
//!
//! # Bug Reports
//!
//! Please report all bugs for this crate to [this crate's GitHub issue
//! tracker](https://github.com/houseabsolute/en-inflexion/issues). **Do not
//! report bugs to Damian Conway via email or rt.cpan.org.** I would like to
//! review all bug reports in order to avoid bothering Damian with issues that
//! are specific to this crate or which I can answer after reviewing the Perl
//! distribution myself. If a bug needs to be reported "upstream" to the Perl
//! distribution I will take care of that.

mod util;

pub mod adjective;
pub mod indefinite;
pub mod ll;
pub mod noun;
pub mod person;
pub mod pronoun;
pub mod term;
pub mod verb;
