use crate::{ll, person::Person, term::Term, util};
use once_cell::sync::Lazy;
use regex::Regex;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};
use thiserror::Error;

/// A `Pronoun` is a single pronoun which can be inflected in various ways.
#[derive(Clone, Debug)]
pub struct Pronoun<'a>(&'a str);

#[derive(Debug)]
struct Inflexion {
    singular: [&'static str; 4],
    plural: [&'static str; 4],
}

#[derive(Debug, Hash, PartialEq)]
enum Case {
    Nominative,
    Objective,
    Possessive,
    Reflexive,
}

impl Eq for Case {}

#[derive(Debug)]
enum Form {
    Singular,
    Plural,
}

static PREPOSITION_AND_TERM_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(&format!(r"(\s*{}\s+)?(.+)", util::PREPOSITION_REGEX_STR))
        .expect("Could not parse preposition regex")
});

#[derive(Debug, Error)]
#[error("{0} is not a valid pronoun")]
pub struct Error<'a>(&'a str);

impl<'a> Pronoun<'a> {
    pub fn try_new(pronoun: &'a str) -> Result<Self, Error<'a>> {
        let lc_pronoun = if util::is_lowercase(pronoun) {
            Cow::Borrowed(pronoun)
        } else {
            Cow::Owned(pronoun.to_lowercase())
        };
        if !VALID_PRONOUNS.contains(lc_pronoun.as_ref()) {
            return Err(Error(pronoun));
        }
        Ok(Self(pronoun))
    }

    fn singular_for_person(&self, person: Person) -> Cow<'a, str> {
        self.inflect(person, Form::Singular)
    }

    fn plural_for_person(&self, person: Person) -> Cow<'a, str> {
        self.inflect(person, Form::Plural)
    }

    fn inflect(&self, person: Person, form: Form) -> Cow<'a, str> {
        let (preposition, pronoun) = self.split_preposition();
        match preposition {
            None => inflect_pronoun(pronoun, form, person, false),
            Some(preposition) => {
                let inflected = inflect_pronoun(pronoun, form, person, true);
                if inflected == pronoun {
                    return Cow::Borrowed(self.0);
                }
                Cow::Owned(format!("{}{}", preposition, inflected))
            }
        }
    }

    fn split_preposition(&self) -> (Option<&'a str>, &'a str) {
        let caps = match PREPOSITION_AND_TERM_REGEX.captures(self.0) {
            Some(c) => c,
            None => return (None, ""),
        };
        (
            caps.get(1).map(|m| m.as_str()),
            caps.get(2).map_or("", |c| c.as_str()),
        )
    }
}

impl<'a> Term<'a> for Pronoun<'a> {
    fn new(pronoun: &'a str) -> Self {
        Self(pronoun)
    }

    fn singular(&self) -> Cow<'a, str> {
        self.singular_for_person(Person::Default)
    }

    fn plural(&self) -> Cow<'a, str> {
        self.plural_for_person(Person::Default)
    }

    fn is_singular(&self) -> bool {
        ll::nouns::is_singular(self.0)
    }

    fn is_plural(&self) -> bool {
        ll::nouns::is_plural(self.0)
    }
}

const ALL_CASES_WITHOUT_PREPOSITION: &[Case] = &[
    Case::Nominative,
    Case::Objective,
    Case::Possessive,
    Case::Reflexive,
];

const ALL_CASES_WITH_PREPOSITION: &[Case] = &[
    Case::Objective,
    Case::Possessive,
    Case::Reflexive,
    Case::Nominative,
];

fn inflect_pronoun(
    pronoun: &str,
    form: Form,
    person: Person,
    has_preposition: bool,
) -> Cow<'_, str> {
    let lc_pronoun = if util::is_lowercase(pronoun) {
        Cow::Borrowed(pronoun)
    } else {
        Cow::Owned(pronoun.to_lowercase())
    };

    let cases = if has_preposition {
        ALL_CASES_WITH_PREPOSITION
    } else {
        ALL_CASES_WITHOUT_PREPOSITION
    };

    for c in cases {
        let case = INFLEXIONS_OF.get(c).unwrap();
        if let Some(inflexion) = case.get(lc_pronoun.as_ref()) {
            return match form {
                Form::Singular => {
                    util::fix_case(pronoun, Cow::Borrowed(inflexion.singular[person.idx()]))
                }
                Form::Plural => {
                    util::fix_case(pronoun, Cow::Borrowed(inflexion.plural[person.idx()]))
                }
            };
        }
    }

    // The pronoun is apparently not a pronoun.
    match form {
        Form::Singular => ll::nouns::convert_to_singular(pronoun),
        Form::Plural => ll::nouns::convert_to_modern_plural(pronoun),
    }
}

static INFLEXIONS_OF: Lazy<HashMap<Case, HashMap<&'static str, Inflexion>>> = Lazy::new(|| {
    let mut nominative: HashMap<&'static str, Inflexion> = HashMap::new();
    nominative.insert(
        "i",
        Inflexion {
            singular: ["I", "I", "you", "it"],
            plural: ["we", "we", "you", "they"],
        },
    );
    nominative.insert(
        "you",
        Inflexion {
            singular: ["you", "I", "you", "it"],
            plural: ["you", "we", "you", "they"],
        },
    );
    nominative.insert(
        "she",
        Inflexion {
            singular: ["she", "I", "you", "she"],
            plural: ["they", "we", "you", "they"],
        },
    );
    nominative.insert(
        "he",
        Inflexion {
            singular: ["he", "I", "you", "he"],
            plural: ["they", "we", "you", "they"],
        },
    );
    nominative.insert(
        "it",
        Inflexion {
            singular: ["it", "I", "you", "it"],
            plural: ["they", "we", "you", "they"],
        },
    );
    nominative.insert(
        "we",
        Inflexion {
            singular: ["I", "I", "you", "it"],
            plural: ["we", "we", "you", "they"],
        },
    );
    nominative.insert(
        "they",
        Inflexion {
            singular: ["it", "I", "you", "it"],
            plural: ["they", "we", "you", "they"],
        },
    );
    nominative.insert(
        "one",
        Inflexion {
            singular: ["one", "I", "you", "one"],
            plural: ["some", "we", "you", "some"],
        },
    );
    nominative.insert(
        "this",
        Inflexion {
            singular: ["this", "this", "this", "this"],
            plural: ["these", "these", "these", "these"],
        },
    );
    nominative.insert(
        "that",
        Inflexion {
            singular: ["that", "that", "that", "that"],
            plural: ["those", "those", "those", "those"],
        },
    );
    nominative.insert(
        "these",
        Inflexion {
            singular: ["this", "this", "this", "this"],
            plural: ["these", "these", "these", "these"],
        },
    );
    nominative.insert(
        "those",
        Inflexion {
            singular: ["that", "that", "that", "that"],
            plural: ["those", "those", "those", "those"],
        },
    );
    nominative.insert(
        "who",
        Inflexion {
            singular: ["who", "who", "who", "who"],
            plural: ["who", "who", "who", "who"],
        },
    );
    nominative.insert(
        "whoever",
        Inflexion {
            singular: ["whoever", "whoever", "whoever", "whoever"],
            plural: ["whoever", "whoever", "whoever", "whoever"],
        },
    );
    nominative.insert(
        "whosoever",
        Inflexion {
            singular: ["whosoever", "whosoever", "whosoever", "whosoever"],
            plural: ["whosoever", "whosoever", "whosoever", "whosoever"],
        },
    );

    let mut objective: HashMap<&'static str, Inflexion> = HashMap::new();
    objective.insert(
        "me",
        Inflexion {
            singular: ["me", "me", "you", "it"],
            plural: ["us", "us", "you", "them"],
        },
    );
    objective.insert(
        "you",
        Inflexion {
            singular: ["you", "me", "you", "it"],
            plural: ["you", "us", "you", "them"],
        },
    );
    objective.insert(
        "her",
        Inflexion {
            singular: ["her", "me", "you", "her"],
            plural: ["them", "us", "you", "them"],
        },
    );
    objective.insert(
        "him",
        Inflexion {
            singular: ["him", "me", "you", "him"],
            plural: ["them", "us", "you", "them"],
        },
    );
    objective.insert(
        "it",
        Inflexion {
            singular: ["it", "me", "you", "it"],
            plural: ["them", "us", "you", "them"],
        },
    );
    objective.insert(
        "one",
        Inflexion {
            singular: ["one", "me", "you", "one"],
            plural: ["some", "us", "you", "some"],
        },
    );
    objective.insert(
        "us",
        Inflexion {
            singular: ["me", "me", "you", "it"],
            plural: ["us", "us", "you", "them"],
        },
    );
    objective.insert(
        "them",
        Inflexion {
            singular: ["it", "me", "you", "it"],
            plural: ["them", "us", "you", "them"],
        },
    );
    objective.insert(
        "this",
        Inflexion {
            singular: ["this", "this", "this", "this"],
            plural: ["these", "these", "these", "these"],
        },
    );
    objective.insert(
        "that",
        Inflexion {
            singular: ["that", "that", "that", "that"],
            plural: ["those", "those", "those", "those"],
        },
    );
    objective.insert(
        "these",
        Inflexion {
            singular: ["this", "this", "this", "this"],
            plural: ["these", "these", "these", "these"],
        },
    );
    objective.insert(
        "those",
        Inflexion {
            singular: ["that", "that", "that", "that"],
            plural: ["those", "those", "those", "those"],
        },
    );
    objective.insert(
        "whom",
        Inflexion {
            singular: ["whom", "whom", "whom", "whom"],
            plural: ["whom", "whom", "whom", "whom"],
        },
    );
    objective.insert(
        "whomever",
        Inflexion {
            singular: ["whomever", "whomever", "whomever", "whomever"],
            plural: ["whomever", "whomever", "whomever", "whomever"],
        },
    );
    objective.insert(
        "whomsoever",
        Inflexion {
            singular: ["whomsoever", "whomsoever", "whomsoever", "whomsoever"],
            plural: ["whomsoever", "whomsoever", "whomsoever", "whomsoever"],
        },
    );

    let mut possessive: HashMap<&'static str, Inflexion> = HashMap::new();
    possessive.insert(
        "mine",
        Inflexion {
            singular: ["mine", "mine", "yours", "its"],
            plural: ["ours", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "yours",
        Inflexion {
            singular: ["yours", "mine", "yours", "its"],
            plural: ["yours", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "hers",
        Inflexion {
            singular: ["hers", "mine", "yours", "hers"],
            plural: ["theirs", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "his",
        Inflexion {
            singular: ["his", "mine", "yours", "his"],
            plural: ["theirs", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "its",
        Inflexion {
            singular: ["its", "mine", "yours", "its"],
            plural: ["theirs", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "one's",
        Inflexion {
            singular: ["one's", "mine", "yours", "one's"],
            plural: ["theirs", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "ours",
        Inflexion {
            singular: ["mine", "mine", "yours", "its"],
            plural: ["ours", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "theirs",
        Inflexion {
            singular: ["its", "mine", "yours", "its"],
            plural: ["theirs", "ours", "yours", "theirs"],
        },
    );
    possessive.insert(
        "whose",
        Inflexion {
            singular: ["whose", "whose", "whose", "whose"],
            plural: ["whose", "whose", "whose", "whose"],
        },
    );
    possessive.insert(
        "whosever",
        Inflexion {
            singular: ["whosever", "whosever", "whosever", "whosever"],
            plural: ["whosever", "whosever", "whosever", "whosever"],
        },
    );
    possessive.insert(
        "whosesoever",
        Inflexion {
            singular: ["whosesoever", "whosesoever", "whosesoever", "whosesoever"],
            plural: ["whosesoever", "whosesoever", "whosesoever", "whosesoever"],
        },
    );

    let mut reflexive: HashMap<&'static str, Inflexion> = HashMap::new();
    reflexive.insert(
        "myself",
        Inflexion {
            singular: ["myself", "myself", "yourself", "itself"],
            plural: ["ourselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "yourself",
        Inflexion {
            singular: ["yourself", "myself", "yourself", "itself"],
            plural: ["yourselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "herself",
        Inflexion {
            singular: ["herself", "myself", "yourself", "herself"],
            plural: ["themselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "himself",
        Inflexion {
            singular: ["himself", "myself", "yourself", "himself"],
            plural: ["themselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "themself",
        Inflexion {
            singular: ["themselves", "myself", "yourself", "themselves"],
            plural: ["themselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "itself",
        Inflexion {
            singular: ["itself", "myself", "yourself", "itself"],
            plural: ["themselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "oneself",
        Inflexion {
            singular: ["oneself", "myself", "yourself", "oneself"],
            plural: ["oneselves", "ourselves", "yourselves", "oneselves"],
        },
    );
    reflexive.insert(
        "ourselves",
        Inflexion {
            singular: ["myself", "myself", "yourself", "itself"],
            plural: ["ourselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "yourselves",
        Inflexion {
            singular: ["yourself", "myself", "yourself", "itself"],
            plural: ["yourselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "themselves",
        Inflexion {
            singular: ["themselves", "myself", "yourself", "themselves"],
            plural: ["themselves", "ourselves", "yourselves", "themselves"],
        },
    );
    reflexive.insert(
        "oneselves",
        Inflexion {
            singular: ["oneself", "myself", "yourself", "oneself"],
            plural: ["oneselves", "ourselves", "yourselves", "oneselves"],
        },
    );

    let mut inflexions_of = HashMap::new();
    inflexions_of.insert(Case::Nominative, nominative);
    inflexions_of.insert(Case::Objective, objective);
    inflexions_of.insert(Case::Possessive, possessive);
    inflexions_of.insert(Case::Reflexive, reflexive);

    inflexions_of
});

static VALID_PRONOUNS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    let mut hs: HashSet<&'static str> = HashSet::new();
    for case in INFLEXIONS_OF.values() {
        for word in case.keys() {
            hs.insert(word);
        }
    }
    hs
});

#[cfg(test)]
mod tests {
    use super::{Person, Pronoun};
    use crate::term::Term;

    #[test]
    fn try_new() {
        for word in ["I", "you", "they", "whom", "who"] {
            assert!(
                super::Pronoun::try_new(word).is_ok(),
                "{} is a pronoun",
                word
            );
        }

        for word in ["dog", "equivocate", "pronoun", "tofu", "lovely"] {
            assert!(
                super::Pronoun::try_new(word).is_err(),
                "{} is not a pronoun",
                word
            );
        }
    }

    #[test]
    fn singular() {
        let tester = |pronoun, expect| {
            assert_eq!(
                Pronoun::new(pronoun).singular(),
                expect,
                "singular of {} is {}",
                pronoun,
                expect,
            )
        };
        let tests = [
            // Nominative
            ("I", "I"),
            ("we", "I"),
            ("you", "you"),
            ("it", "it"),
            ("they", "it"),
            // Objective
            ("me", "me"),
            ("us", "me"),
            ("you", "you"),
            ("it", "it"),
            ("them", "it"),
            // Possessive
            ("mine", "mine"),
            ("ours", "mine"),
            ("yours", "yours"),
            ("its", "its"),
            ("theirs", "its"),
            // Reflexive
            ("myself", "myself"),
            ("ourselves", "myself"),
            ("yourself", "yourself"),
            ("yourselves", "yourself"),
            ("itself", "itself"),
            ("themselves", "themselves"),
            // Nominative
            ("to I", "to I"),
            ("to we", "to I"),
            ("to you", "to you"),
            ("to it", "to it"),
            ("to they", "to it"),
            // Objective
            ("to me", "to me"),
            ("to us", "to me"),
            ("to you", "to you"),
            ("to it", "to it"),
            ("to them", "to it"),
            // Possessive
            ("to mine", "to mine"),
            ("to ours", "to mine"),
            ("to yours", "to yours"),
            ("to its", "to its"),
            ("to theirs", "to its"),
            // Reflexive
            ("to myself", "to myself"),
            ("to ourselves", "to myself"),
            ("to yourself", "to yourself"),
            ("to yourselves", "to yourself"),
            ("to itself", "to itself"),
            ("to themselves", "to themselves"),
        ];
        for test in tests {
            tester(test.0, test.1);
        }
    }

    #[test]
    fn plural() {
        let tester = |pronoun, expect| {
            assert_eq!(
                Pronoun::new(pronoun).plural(),
                expect,
                "plural of {} is {}",
                pronoun,
                expect,
            )
        };
        let tests = [
            // Nominative
            ("I", "we"),
            ("you", "you"),
            ("it", "they"),
            // Objective
            ("me", "us"),
            ("you", "you"),
            // The default form is Nominative without a preposition.
            ("it", "they"),
            // Possessive
            ("mine", "ours"),
            ("yours", "yours"),
            ("its", "theirs"),
            // Reflexive
            ("myself", "ourselves"),
            ("yourself", "yourselves"),
            ("themselves", "themselves"),
            // Nominative
            ("to I", "to we"),
            ("to you", "to you"),
            // The default form is Objective with a preposition.
            ("to it", "to them"),
            // Objective
            ("to me", "to us"),
            ("to you", "to you"),
            ("to it", "to them"),
            // Possessive
            ("to mine", "to ours"),
            ("to yours", "to yours"),
            ("to its", "to theirs"),
            // Reflexive
            ("to myself", "to ourselves"),
            ("to yourself", "to yourselves"),
            ("to themselves", "to themselves"),
        ];
        for test in tests {
            tester(test.0, test.1);
        }
    }

    #[test]
    fn singular_for_person() {
        let tester = |form: Person, pronoun, expect| {
            assert_eq!(
                Pronoun::new(pronoun).singular_for_person(form.clone()),
                expect,
                "{:?} person form singular of {} is {}",
                form,
                pronoun,
                expect,
            )
        };
        let tests = [
            // Nominative
            (Person::First, "I", "I"),
            (Person::Second, "I", "you"),
            (Person::Third, "I", "it"),
            (Person::First, "we", "I"),
            (Person::Second, "we", "you"),
            (Person::Third, "we", "it"),
            (Person::First, "you", "I"),
            (Person::Second, "you", "you"),
            (Person::Third, "you", "it"),
            (Person::First, "it", "I"),
            (Person::Second, "it", "you"),
            (Person::Third, "it", "it"),
            (Person::First, "they", "I"),
            (Person::Second, "they", "you"),
            (Person::Third, "they", "it"),
            // Objective
            (Person::First, "me", "me"),
            (Person::Second, "me", "you"),
            (Person::Third, "me", "it"),
            (Person::First, "us", "me"),
            (Person::Second, "us", "you"),
            (Person::Third, "us", "it"),
            // Nominative case comes before Objective when so we turn "you"
            // into "I" without a preposition or an explicit case.
            (Person::First, "you", "I"),
            (Person::Second, "you", "you"),
            (Person::Third, "you", "it"),
            (Person::First, "it", "I"),
            (Person::Second, "it", "you"),
            (Person::Third, "it", "it"),
            (Person::First, "them", "me"),
            (Person::Second, "them", "you"),
            (Person::Third, "them", "it"),
            // Possessive
            (Person::First, "mine", "mine"),
            (Person::Second, "mine", "yours"),
            (Person::Third, "mine", "its"),
            (Person::First, "ours", "mine"),
            (Person::Second, "ours", "yours"),
            (Person::Third, "ours", "its"),
            (Person::First, "yours", "mine"),
            (Person::Second, "yours", "yours"),
            (Person::Third, "yours", "its"),
            (Person::First, "its", "mine"),
            (Person::Second, "its", "yours"),
            (Person::Third, "its", "its"),
            (Person::First, "theirs", "mine"),
            (Person::Second, "theirs", "yours"),
            (Person::Third, "theirs", "its"),
            // Reflexive
            (Person::First, "myself", "myself"),
            (Person::Second, "myself", "yourself"),
            (Person::Third, "myself", "itself"),
            (Person::First, "ourselves", "myself"),
            (Person::Second, "ourselves", "yourself"),
            (Person::Third, "ourselves", "itself"),
            (Person::First, "yourself", "myself"),
            (Person::Second, "yourself", "yourself"),
            (Person::Third, "yourself", "itself"),
            (Person::First, "yourselves", "myself"),
            (Person::Second, "yourselves", "yourself"),
            (Person::Third, "yourselves", "itself"),
            (Person::First, "itself", "myself"),
            (Person::Second, "itself", "yourself"),
            (Person::Third, "itself", "itself"),
            (Person::First, "themselves", "myself"),
            (Person::Second, "themselves", "yourself"),
            (Person::Third, "themselves", "themselves"),
            // Nominative
            (Person::First, "to I", "to I"),
            (Person::Second, "to I", "to you"),
            (Person::Third, "to I", "to it"),
            (Person::First, "to we", "to I"),
            (Person::Second, "to we", "to you"),
            (Person::Third, "to we", "to it"),
            (Person::First, "to you", "to me"),
            (Person::Second, "to you", "to you"),
            (Person::Third, "to you", "to it"),
            (Person::First, "to it", "to me"),
            (Person::Second, "to it", "to you"),
            (Person::Third, "to it", "to it"),
            (Person::First, "to they", "to I"),
            (Person::Second, "to they", "to you"),
            (Person::Third, "to they", "to it"),
            // Objective
            (Person::First, "to me", "to me"),
            (Person::Second, "to me", "to you"),
            (Person::Third, "to me", "to it"),
            (Person::First, "to us", "to me"),
            (Person::Second, "to us", "to you"),
            (Person::Third, "to us", "to it"),
            (Person::First, "to you", "to me"),
            (Person::Second, "to you", "to you"),
            (Person::Third, "to you", "to it"),
            (Person::First, "to it", "to me"),
            (Person::Second, "to it", "to you"),
            (Person::Third, "to it", "to it"),
            (Person::First, "to them", "to me"),
            (Person::Second, "to them", "to you"),
            (Person::Third, "to them", "to it"),
            // Possessive
            (Person::First, "to mine", "to mine"),
            (Person::Second, "to mine", "to yours"),
            (Person::Third, "to mine", "to its"),
            (Person::First, "to ours", "to mine"),
            (Person::Second, "to ours", "to yours"),
            (Person::Third, "to ours", "to its"),
            (Person::First, "to yours", "to mine"),
            (Person::Second, "to yours", "to yours"),
            (Person::Third, "to yours", "to its"),
            (Person::First, "to its", "to mine"),
            (Person::Second, "to its", "to yours"),
            (Person::Third, "to its", "to its"),
            (Person::First, "to theirs", "to mine"),
            (Person::Second, "to theirs", "to yours"),
            (Person::Third, "to theirs", "to its"),
            // Reflexive
            (Person::First, "to myself", "to myself"),
            (Person::Second, "to myself", "to yourself"),
            (Person::Third, "to myself", "to itself"),
            (Person::First, "to ourselves", "to myself"),
            (Person::Second, "to ourselves", "to yourself"),
            (Person::Third, "to ourselves", "to itself"),
            (Person::First, "to yourself", "to myself"),
            (Person::Second, "to yourself", "to yourself"),
            (Person::Third, "to yourself", "to itself"),
            (Person::First, "to yourselves", "to myself"),
            (Person::Second, "to yourselves", "to yourself"),
            (Person::Third, "to yourselves", "to itself"),
            (Person::First, "to itself", "to myself"),
            (Person::Second, "to itself", "to yourself"),
            (Person::Third, "to itself", "to itself"),
            (Person::First, "to themselves", "to myself"),
            (Person::Second, "to themselves", "to yourself"),
            (Person::Third, "to themselves", "to themselves"),
        ];
        for test in tests {
            tester(test.0, test.1, test.2);
        }
    }

    #[test]
    fn plural_for_person() {
        let tester = |form: Person, pronoun, expect| {
            assert_eq!(
                Pronoun::new(pronoun).plural_for_person(form.clone()),
                expect,
                "{:?} person form plural of {} is {}",
                form,
                pronoun,
                expect,
            )
        };
        let tests = [
            // Nominative
            (Person::First, "I", "we"),
            (Person::Second, "I", "you"),
            (Person::Third, "I", "they"),
            (Person::First, "we", "we"),
            (Person::Second, "we", "you"),
            (Person::Third, "we", "they"),
            (Person::First, "you", "we"),
            (Person::Second, "you", "you"),
            (Person::Third, "you", "they"),
            (Person::First, "it", "we"),
            (Person::Second, "it", "you"),
            (Person::Third, "it", "they"),
            (Person::First, "they", "we"),
            (Person::Second, "they", "you"),
            (Person::Third, "they", "they"),
            // Objective
            (Person::First, "me", "us"),
            (Person::Second, "me", "you"),
            (Person::Third, "me", "them"),
            (Person::First, "us", "us"),
            (Person::Second, "us", "you"),
            (Person::Third, "us", "them"),
            // Nominative case comes before Objective when so we turn
            // "you"/"it" into "we" and "they" without a preposition or an
            // explicit case.
            (Person::First, "you", "we"),
            (Person::Second, "you", "you"),
            (Person::Third, "you", "they"),
            (Person::First, "it", "we"),
            (Person::Second, "it", "you"),
            (Person::Third, "it", "they"),
            (Person::First, "them", "us"),
            (Person::Second, "them", "you"),
            (Person::Third, "them", "them"),
            // Possessive
            (Person::First, "mine", "ours"),
            (Person::Second, "mine", "yours"),
            (Person::Third, "mine", "theirs"),
            (Person::First, "ours", "ours"),
            (Person::Second, "ours", "yours"),
            (Person::Third, "ours", "theirs"),
            (Person::First, "yours", "ours"),
            (Person::Second, "yours", "yours"),
            (Person::Third, "yours", "theirs"),
            (Person::First, "its", "ours"),
            (Person::Second, "its", "yours"),
            (Person::Third, "its", "theirs"),
            (Person::First, "theirs", "ours"),
            (Person::Second, "theirs", "yours"),
            (Person::Third, "theirs", "theirs"),
            // Reflexive
            (Person::First, "myself", "ourselves"),
            (Person::Second, "myself", "yourselves"),
            (Person::Third, "myself", "themselves"),
            (Person::First, "ourselves", "ourselves"),
            (Person::Second, "ourselves", "yourselves"),
            (Person::Third, "ourselves", "themselves"),
            (Person::First, "yourself", "ourselves"),
            (Person::Second, "yourself", "yourselves"),
            (Person::Third, "yourself", "themselves"),
            (Person::First, "yourselves", "ourselves"),
            (Person::Second, "yourselves", "yourselves"),
            (Person::Third, "yourselves", "themselves"),
            (Person::First, "itself", "ourselves"),
            (Person::Second, "itself", "yourselves"),
            (Person::Third, "itself", "themselves"),
            (Person::First, "themselves", "ourselves"),
            (Person::Second, "themselves", "yourselves"),
            (Person::Third, "themselves", "themselves"),
            // Nominative
            (Person::First, "to I", "to we"),
            (Person::Second, "to I", "to you"),
            (Person::Third, "to I", "to they"),
            (Person::First, "to we", "to we"),
            (Person::Second, "to we", "to you"),
            (Person::Third, "to we", "to they"),
            (Person::First, "to you", "to us"),
            (Person::Second, "to you", "to you"),
            (Person::Third, "to you", "to them"),
            (Person::First, "to it", "to us"),
            (Person::Second, "to it", "to you"),
            (Person::Third, "to it", "to them"),
            (Person::First, "to they", "to we"),
            (Person::Second, "to they", "to you"),
            (Person::Third, "to they", "to they"),
            // Objective
            (Person::First, "to me", "to us"),
            (Person::Second, "to me", "to you"),
            (Person::Third, "to me", "to them"),
            (Person::First, "to us", "to us"),
            (Person::Second, "to us", "to you"),
            (Person::Third, "to us", "to them"),
            (Person::First, "to you", "to us"),
            (Person::Second, "to you", "to you"),
            (Person::Third, "to you", "to them"),
            (Person::First, "to it", "to us"),
            (Person::Second, "to it", "to you"),
            (Person::Third, "to it", "to them"),
            (Person::First, "to them", "to us"),
            (Person::Second, "to them", "to you"),
            (Person::Third, "to them", "to them"),
            // Possessive
            (Person::First, "to mine", "to ours"),
            (Person::Second, "to mine", "to yours"),
            (Person::Third, "to mine", "to theirs"),
            (Person::First, "to ours", "to ours"),
            (Person::Second, "to ours", "to yours"),
            (Person::Third, "to ours", "to theirs"),
            (Person::First, "to yours", "to ours"),
            (Person::Second, "to yours", "to yours"),
            (Person::Third, "to yours", "to theirs"),
            (Person::First, "to its", "to ours"),
            (Person::Second, "to its", "to yours"),
            (Person::Third, "to its", "to theirs"),
            (Person::First, "to theirs", "to ours"),
            (Person::Second, "to theirs", "to yours"),
            (Person::Third, "to theirs", "to theirs"),
            // Reflexive
            (Person::First, "to myself", "to ourselves"),
            (Person::Second, "to myself", "to yourselves"),
            (Person::Third, "to myself", "to themselves"),
            (Person::First, "to ourselves", "to ourselves"),
            (Person::Second, "to ourselves", "to yourselves"),
            (Person::Third, "to ourselves", "to themselves"),
            (Person::First, "to yourself", "to ourselves"),
            (Person::Second, "to yourself", "to yourselves"),
            (Person::Third, "to yourself", "to themselves"),
            (Person::First, "to yourselves", "to ourselves"),
            (Person::Second, "to yourselves", "to yourselves"),
            (Person::Third, "to yourselves", "to themselves"),
            (Person::First, "to itself", "to ourselves"),
            (Person::Second, "to itself", "to yourselves"),
            (Person::Third, "to itself", "to themselves"),
            (Person::First, "to themselves", "to ourselves"),
            (Person::Second, "to themselves", "to yourselves"),
            (Person::Third, "to themselves", "to themselves"),
        ];
        for test in tests {
            tester(test.0, test.1, test.2);
        }
    }

    #[test]
    fn case_is_fixed() {
        for test in [
            ("mine", "ours"),
            ("Mine", "Ours"),
            ("MINE", "OURS"),
            ("to me", "to us"),
            ("To Mine", "To Ours"),
            ("TO MINE", "TO OURS"),
        ] {
            let pronoun = Pronoun::new(test.0);
            assert_eq!(pronoun.plural(), test.1);
        }
    }
}
