use once_cell::sync::Lazy;
use regex::{Captures, Regex};
use std::borrow::Cow;

pub(crate) const PREPOSITION_REGEX_STR: &str = r#"(?:(?ix)
    about | above | across | after | among | around | athwart | at | before | behind |
    below | beneath | besides? | between | betwixt | beyond | but | by | during |
    except | for | from | into | in | near | off | of | onto | on | out | over |
    since | till | to | under | until | unto | upon | with)"#;

static CASE_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r#"(?x)
    \b   # a word boundary (which can include the start of the string)
    (?:
        (?P<lower> [a-z0-9]+ )       |  # a word entirely in lower case
        (?P<title> [A-Z][a-z0-9]+ )  |  # a word in title case
        (?P<upper> [A-Z0-9]+ )          # a word entirely in upper case
    )
    (?:
        (?P<separator> [\s-]* )  |  # a separator (space or dash) 
        $                           # or the end of the string
    )
"#,
    )
    .expect("Could not parse casing regex")
});

static SEPARATOR_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"[\s-]+").expect("Could not parse separator regex"));

pub(crate) fn fix_case<'a>(orig: &'a str, new: Cow<'a, str>) -> Cow<'a, str> {
    if orig == "I" || new == "I" {
        return new;
    }

    let mut fixed: Vec<Cow<'_, str>> = vec![];
    let mut pieces = SEPARATOR_REGEX.split(&new);

    for caps in CASE_REGEX.captures_iter(orig) {
        if caps.name("lower").is_some() {
            match pieces.next() {
                Some(piece) => {
                    if is_lowercase(piece) {
                        fixed.push(Cow::Borrowed(piece));
                    } else {
                        fixed.push(Cow::Owned(piece.to_lowercase()));
                    }
                }
                // Something went horribly wrong and the new string doesn't
                // split in the same way as the old. That should never happen.
                None => return new,
            }
        }

        if caps.name("title").is_some() {
            match pieces.next() {
                Some(piece) => {
                    let mut chars = piece.chars();
                    match chars.next() {
                        Some(c) => {
                            if c.is_uppercase() {
                                // If the first character is uppercase we can
                                // check the rest.
                                if chars.any(char::is_uppercase) {
                                    fixed.push(Cow::Owned(titlecase_word(piece)));
                                } else {
                                    fixed.push(Cow::Borrowed(piece));
                                }
                            } else {
                                // If the first char is lowercase this can't
                                // be titlecase.
                                fixed.push(Cow::Owned(titlecase_word(piece)));
                            }
                        }
                        // Something went horribly wrong ...
                        None => return new,
                    }
                }
                // Something went horribly wrong ...
                None => return new,
            }
        }

        if caps.name("upper").is_some() {
            match pieces.next() {
                Some(piece) => {
                    if piece.chars().all(char::is_uppercase) {
                        fixed.push(Cow::Borrowed(piece));
                    } else {
                        fixed.push(Cow::Owned(piece.to_uppercase()));
                    }
                }
                // Something went horribly wrong ...
                None => return new,
            }
        }

        if let Some(sep) = caps.name("separator") {
            let sep = sep.as_str();
            if !sep.is_empty() {
                fixed.push(Cow::Borrowed(sep));
            }
        }
    }

    if fixed.iter().all(|f| matches!(f, Cow::Borrowed(_))) {
        return new;
    }

    Cow::Owned(fixed.join(""))
}

fn titlecase_word(word: &str) -> String {
    format!(
        "{}{}",
        &word[0..1].to_uppercase(),
        &word[1..].to_lowercase()
    )
}

pub(crate) fn is_lowercase(word: &str) -> bool {
    word.chars()
        .all(|c| c.is_lowercase() || !c.is_ascii_alphabetic())
}

static TITLECASE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("([^ -]+)([ -]|$)").unwrap());

pub(crate) fn to_titlecase(word: &str) -> String {
    return TITLECASE_REGEX
        .replace_all(word, |caps: &Captures| {
            let w = caps.get(1).unwrap().as_str().to_string();
            let mut replacement = format!("{}{}", &w[0..1].to_uppercase(), &w[1..].to_lowercase());
            let sep = caps.get(2).unwrap().as_str();
            if !sep.is_empty() {
                replacement.push_str(sep);
            }
            replacement
        })
        .to_string();
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    #[test]
    fn to_titlecase() {
        let tests = [
            ("dog", "Dog"),
            ("DOG", "Dog"),
            ("Dog", "Dog"),
            ("dog-town", "Dog-Town"),
            ("DOG-TOWN", "Dog-Town"),
            ("Dog-Town", "Dog-Town"),
            ("dog town", "Dog Town"),
            ("DOG TOWN", "Dog Town"),
            ("Dog Town", "Dog Town"),
            ("dog-town shed", "Dog-Town Shed"),
            ("dog-TOWN shed", "Dog-Town Shed"),
            ("dog-TOWN Shed", "Dog-Town Shed"),
            ("Dog-TOWN Shed", "Dog-Town Shed"),
            ("DoG-TowN Shed", "Dog-Town Shed"),
        ];
        for test in tests {
            assert_eq!(super::to_titlecase(test.0), test.1);
        }
    }

    #[test]
    fn fix_case() {
        let tests = [
            // orig is lowercase
            ("dog", "dogs", "dogs"),
            ("dog", "DOGs", "dogs"),
            ("dog", "Dogs", "dogs"),
            ("dog", "doGs", "dogs"),
            // orig is titlecase
            ("Dog", "dogs", "Dogs"),
            ("Dog", "DOGs", "Dogs"),
            ("Dog", "Dogs", "Dogs"),
            ("Dog", "doGs", "Dogs"),
            // orig is uppercase
            ("DOG", "dogs", "DOGS"),
            ("DOG", "DOGs", "DOGS"),
            ("DOG", "Dogs", "DOGS"),
            ("DOG", "doGs", "DOGS"),
            // orig is lowercase
            ("dog cat fish", "dogs cats fishes", "dogs cats fishes"),
            ("dog cat fish", "DOGS CATS FISHES", "dogs cats fishes"),
            ("dog cat fish", "Dogs Cats Fishes", "dogs cats fishes"),
            ("dog cat fish", "doGs Cats FISHes", "dogs cats fishes"),
            // orig is titlecase
            ("Dog Cat Fish", "dogs cats fishes", "Dogs Cats Fishes"),
            ("Dog Cat Fish", "DOGS CATS FISHES", "Dogs Cats Fishes"),
            ("Dog Cat Fish", "Dogs Cats Fishes", "Dogs Cats Fishes"),
            ("Dog Cat Fish", "doGs Cats FISHes", "Dogs Cats Fishes"),
            // orig is uppercase
            ("DOG CAT FISH", "dogs cats fishes", "DOGS CATS FISHES"),
            ("DOG CAT FISH", "DOGS CATS FISHES", "DOGS CATS FISHES"),
            ("DOG CAT FISH", "Dogs Cats Fishes", "DOGS CATS FISHES"),
            ("DOG CAT FISH", "doGs Cats FISHes", "DOGS CATS FISHES"),
            // orig is lowercase
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            ("dog-cat-fish", "DOGS-CATS-FISHES", "dogs-cats-fishes"),
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            // orig is titlecase
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            ("dog-cat-fish", "DOGS-CATS-FISHES", "dogs-cats-fishes"),
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            ("dog-cat-fish", "dogs-cats-fishes", "dogs-cats-fishes"),
            // orig is uppercase
            ("DOG-CAT-FISH", "dogs-cats-fishes", "DOGS-CATS-FISHES"),
            ("DOG-CAT-FISH", "DOGS-CATS-FISHES", "DOGS-CATS-FISHES"),
            ("DOG-CAT-FISH", "dogs-cats-fishes", "DOGS-CATS-FISHES"),
            ("DOG-CAT-FISH", "dogs-cats-fishes", "DOGS-CATS-FISHES"),
            // mixed separators
            (
                "dog  cat - - fish",
                "DOGS  CATS - - FISHES",
                "dogs  cats - - fishes",
            ),
        ];

        for test in tests {
            assert_eq!(
                super::fix_case(test.0, Cow::Borrowed(test.1)),
                test.2,
                "fix_case({}, {}) = {}",
                test.0,
                test.1,
                test.2,
            );

            let orig = "dog cat fish";
            let new = "dogs cats fishes";
            let ok = match super::fix_case(orig, Cow::Borrowed(new)) {
                Cow::Borrowed(_) => true,
                Cow::Owned(_) => false,
            };
            assert!(ok, "fix_case returns a Cow::Borrowed when it does not make any changes to the new string");
        }
    }
}
