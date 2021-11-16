/// The `Person` enum has options for various inflections of pronouns. For
/// example, "I", which is first person, inflects to "you" in second person
/// and "it" in third.
#[derive(Clone, Debug, PartialEq)]
pub enum Person {
    /// The Default form, which is first person for most cases.
    Default,
    /// First person ("I", "me", "mine", "myself", etc.)
    First,
    /// First person ("you", "you", "yours", "yourself", etc.)
    Second,
    /// First person ("he", "she", "they", "it", etc.)
    Third,
}

impl Person {
    pub(crate) fn idx(&self) -> usize {
        match self {
            Person::Default => 0,
            Person::First => 1,
            Person::Second => 2,
            Person::Third => 3,
        }
    }
}
