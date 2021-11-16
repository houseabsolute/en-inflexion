//! Provides the [Term] trait, which defines methods shared by all word
//! structs.

use std::borrow::Cow;

/// This trait is implemented by all words, such as nouns, verbs, etc.
pub trait Term<'a> {
    fn new(word: &'a str) -> Self;

    /// Returns the singular form of the given word. If the word is already
    /// singular it will return the word given to `new`. Whenever possible
    /// this method avoid allocating a new [String]. Whenever possible this
    /// method avoids allocating a new [String](std::string::String).
    fn singular(&self) -> Cow<'a, str>;

    /// Returns the modern plural form of the given word. If the word is
    /// already plural it will return the word given to `new`. It will try to
    /// make sure that the returned word matches the case of the word it was
    /// given, so "dog" becomes "dogs", "Dog" => "Dogs", and "DOG" =>
    /// DOGS". This also applies to hyphenated words, so "Man-at-Arms" will
    /// become "Men-at-Arms". Whenever possible this method avoids allocating
    /// a new [String](std::string::String).
    fn plural(&self) -> Cow<'a, str>;

    /// Returns a boolean indicating whether the word is singular. Note that a
    /// word can be *both* singular and plural, like "sheep", "eat", or
    /// "blue".
    fn is_singular(&self) -> bool;

    /// Returns a boolean indicating whether the word is plural. Note that a
    /// word can be *both* singular and plural, like "sheep", "eat", or
    /// "blue"..
    fn is_plural(&self) -> bool;
}
