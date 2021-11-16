/*!
Indefinite article ("a" and "an") selection for nouns.
*/
use once_cell::sync::Lazy;
use regex::Regex;

static ORDINAL_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A [aefhilmnorsx]   -?th $").expect("Could not parse ordinal an regex")
});

static ORDINAL_A_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A [bcdgjkpqtuvwyz] -?th $").expect("Could not parse ordinal a regex")
});

static EXPLICIT_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A (?: euler | hour | heir | honest | hono )")
        .expect("Could not parse explicit an regex")
});

static HOURI_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?ix) \A houri").expect("Could not parse houri regex"));

static SINGLE_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A [aefhilmnorsx]   $").expect("Could not parse single an regex")
});

static SINGLE_A_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A [bcdgjkpqtuvwyz] $").expect("Could not parse single a regex")
});

// Damian says: This pattern matches strings of capitals (i.e. abbreviations)
// that start with a "vowel-sound" consonant followed by another consonant,
// and which are not likely to be real words (oh, all right then, it's just
// magic!)...
static UPPERCASE_ABBREV_AN_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?x) \A [FHLMNRSX][A-Z]").expect("Could not parse abbrev an regex"));

// In the Perl version this is done as a negative lookahead in the abbrev "an"
// regex, but we need to split this up.
static NOT_UPPERCASE_ABBREV_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    let re = r#"(?x)
        \A
        FJO | [HLMNS]Y.  | RY[EO] | SQU |
        (?:
            F[LR]? | [HL] | MN? | N | RH? | S[CHKLMNPTVW]? | X(?:YL)?
        ) [AEIOU]
    "#;
    Regex::new(re).expect("Could not parse not abbrev an regex")
});

static ABBREV_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?xi) \A [aefhilmnorsx][.-]").expect("Could not parse lowercase abbrev regex")
});

static ABBREV_A_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?xi) \A [a-z][.-]").expect("Could not parse lowercase abbrev regex")
});

static CONSONANT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?xi) \A [^aeiouy]").expect("Could not parse consonant regex"));

static SPECIAL_CASE_EUW_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?xi) \A e [uw]").expect("Could not parse special case euw regex"));

static SPECIAL_CASE_ONC_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?xi) \A onc?e \b").expect("Could not parse special case onc regex"));

static SPECIAL_CASE_UNI_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?xi) \A uni (?: [^nmd] | mo)").expect("Could not parse special case uni regex")
});

static SPECIAL_CASE_UT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?xi) \A ut[th]").expect("Could not parse special case ut regex"));

static SPECIAL_CASE_U_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?xi) \A u [bcfhjkqrst] [aeiou]").expect("Could not parse special case u regex")
});

static SPECIAL_CASE_CAPITAL_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?x) \A U [NK] [AIEO]?").expect("Could not parse special case capital regex")
});

static VOWEL_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?xi) \A [aeiou]").expect("Could not parse vowel regex"));

// Damian says: This pattern codes the beginnings of all english words
// begining with a 'Y' followed by a consonant. Any other Y-consonant prefix
// therefore implies an abbreviation...
static INITIAL_Y_AN_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?ix) \A y (?: b[lor] | cl[ea] | fere | gg | p[ios] | rou | tt)")
        .expect("Could not parse initial y an regex")
});

/// Returns an appropriate indefinite article ("a" or "an") for the given
/// word.
pub fn indefinite_article_for(word: &str) -> &'static str {
    // Handle ordinal forms...
    if ORDINAL_A_REGEX.is_match(word) {
        // println!("ORDINAL_A_REGEX matches {}", word);
        return "a";
    }
    if ORDINAL_AN_REGEX.is_match(word) {
        // println!("ORDINAL_AN_REGEX matches {}", word);
        return "an";
    }

    // Handle special cases...
    //
    // The perl code checks for "!= houri" with a negative look-ahead but the
    // regex crate doesn't support that.
    if EXPLICIT_AN_REGEX.is_match(word) && !HOURI_REGEX.is_match(word) {
        return "an";
    }
    if SINGLE_AN_REGEX.is_match(word) {
        // println!("SINGLE_AN_REGEX matches {}", word);
        return "an";
    }
    if SINGLE_A_REGEX.is_match(word) {
        // println!("SINGLE_A_REGEX matches {}", word);
        return "a";
    }

    // Handle abbreviations...
    if UPPERCASE_ABBREV_AN_REGEX.is_match(word) {
        // println!("UPPERCASE_ABBREV_AN_REGEX matches {}", word);
        if !NOT_UPPERCASE_ABBREV_AN_REGEX.is_match(word) {
            // println!("NOT_UPPERCASE_ABBREV_AN_REGEX does not match {}", word);
            return "an";
        }
    }
    if ABBREV_AN_REGEX.is_match(word) {
        // println!("ABBREV_AN_REGEX matches {}", word);
        return "an";
    }
    if ABBREV_A_REGEX.is_match(word) {
        // println!("ABBREV_A_REGEX matches {}", word);
        return "a";
    }

    // Handle consonants
    if CONSONANT_REGEX.is_match(word) {
        // println!("CONSONANT_REGEX matches {}", word);
        return "a";
    }

    // Handle special vowel-forms
    if SPECIAL_CASE_EUW_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_EUW_REGEX matches {}", word);
        return "a";
    }
    if SPECIAL_CASE_ONC_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_ONC_REGEX matches {}", word);
        return "a";
    }
    if SPECIAL_CASE_UNI_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_UNI_REGEX matches {}", word);
        return "a";
    }
    if SPECIAL_CASE_UT_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_UT_REGEX matches {}", word);
        return "an";
    }
    if SPECIAL_CASE_U_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_U_REGEX matches {}", word);
        return "a";
    }

    // Handle special capitals
    if SPECIAL_CASE_CAPITAL_REGEX.is_match(word) {
        // println!("SPECIAL_CASE_CAPITAL_REGEX matches {}", word);
        return "a";
    }

    // Handle vowels
    if VOWEL_REGEX.is_match(word) {
        // println!("VOWEL_REGEX matches {}", word);
        return "an";
    }

    // Handle Y... (before certain consonants implies (unnaturalized) "I.."
    // sound)
    if INITIAL_Y_AN_REGEX.is_match(word) {
        // println!("INITIAL_Y_REGEX matches {}", word);
        return "an";
    }

    // Otherwise, guess "A"
    "a"
}

#[cfg(test)]
mod tests {
    #[test]

    fn indefinite_article_for() {
        let tests = [
            ("an", "Ath"),
            ("a", "Bth"),
            ("a", "Cth"),
            ("a", "Dth"),
            ("an", "Eth"),
            ("an", "Fth"),
            ("a", "Gth"),
            ("an", "Hth"),
            ("an", "Ith"),
            ("a", "Jth"),
            ("a", "Kth"),
            ("an", "Lth"),
            ("an", "Mth"),
            ("an", "Nth"),
            ("an", "Oth"),
            ("a", "Pth"),
            ("a", "Qth"),
            ("an", "Rth"),
            ("an", "Sth"),
            ("a", "Tth"),
            ("a", "Uth"),
            ("a", "Vth"),
            ("a", "Wth"),
            ("an", "Xth"),
            ("a", "Yth"),
            ("a", "Zth"),
            ("an", "a-th"),
            ("a", "b-th"),
            ("a", "c-th"),
            ("a", "d-th"),
            ("an", "e-th"),
            ("an", "f-th"),
            ("a", "g-th"),
            ("an", "h-th"),
            ("an", "i-th"),
            ("a", "j-th"),
            ("a", "k-th"),
            ("an", "l-th"),
            ("an", "m-th"),
            ("an", "n-th"),
            ("an", "o-th"),
            ("a", "p-th"),
            ("a", "q-th"),
            ("an", "r-th"),
            ("an", "s-th"),
            ("a", "t-th"),
            ("a", "u-th"),
            ("a", "v-th"),
            ("a", "w-th"),
            ("an", "x-th"),
            ("a", "y-th"),
            ("a", "z-th"),
            ("an", "A.B.C"),
            ("an", "AI"),
            ("an", "AGE"),
            ("an", "agendum"),
            ("an", "aide-de-camp"),
            ("an", "albino"),
            ("a", "B.L.T. sandwich"),
            ("a", "BMW"),
            ("a", "BLANK"),
            ("a", "bacterium"),
            ("a", "Burmese restaurant"),
            ("a", "C.O."),
            ("a", "CCD"),
            ("a", "COLON"),
            ("a", "cameo"),
            ("a", "CAPITAL"),
            ("a", "D.S.M."),
            ("a", "DNR"),
            ("a", "DINNER"),
            ("a", "dynamo"),
            ("an", "E.K.G."),
            ("an", "ECG"),
            ("an", "EGG"),
            ("an", "embryo"),
            ("an", "erratum"),
            ("a", "eucalyptus"),
            ("an", "Euler number"),
            ("a", "eulogy"),
            ("a", "euphemism"),
            ("a", "euphoria"),
            ("a", "ewe"),
            ("a", "ewer"),
            ("an", "extremum"),
            ("an", "eye"),
            ("an", "F.B.I. agent"),
            ("an", "FSM"),
            ("a", "FACT"),
            ("a", "FAQ"),
            ("an", "F.A.Q."),
            ("a", "fish"),
            ("a", "G-string"),
            ("a", "GSM phone"),
            ("a", "GOD"),
            ("a", "genus"),
            ("a", "Governor General"),
            ("an", "H-Bomb"),
            ("an", "H.M.S Ark Royal"),
            ("an", "HSL colour space"),
            ("a", "HAL 9000"),
            ("an", "H.A.L. 9000"),
            ("a", "has-been"),
            ("a", "height"),
            ("an", "heir"),
            ("a", "honed blade"),
            ("an", "honest man"),
            ("a", "honeymoon"),
            ("an", "honorarium"),
            ("an", "honorary degree"),
            ("an", "honoree"),
            ("an", "honorific"),
            ("a", "Hough transform"),
            ("a", "hound"),
            ("an", "hour"),
            ("an", "hourglass"),
            ("a", "houri"),
            ("a", "house"),
            ("an", "I.O.U."),
            ("an", "IQ"),
            ("an", "IDEA"),
            ("an", "inferno"),
            ("an", "Inspector General"),
            ("a", "jumbo"),
            ("a", "knife"),
            ("an", "L.E.D."),
            ("a", "LED"),
            ("an", "LCD"),
            ("a", "lady in waiting"),
            ("a", "leaf"),
            ("an", "M.I.A."),
            ("a", "MIASMA"),
            ("an", "MTV channel"),
            ("a", "Major General"),
            ("an", "N.C.O."),
            ("an", "NCO"),
            ("a", "NATO country"),
            ("a", "note"),
            ("an", "O.K."),
            ("an", "OK"),
            ("an", "OLE"),
            ("an", "octavo"),
            ("an", "octopus"),
            ("an", "okay"),
            ("a", "once-and-future-king"),
            ("an", "oncologist"),
            ("a", "one night stand"),
            ("an", "onerous task"),
            ("an", "opera"),
            ("an", "optimum"),
            ("an", "opus"),
            ("an", "ox"),
            ("a", "Ph.D."),
            ("a", "PET"),
            ("a", "P.E.T. scan"),
            ("a", "plateau"),
            ("a", "quantum"),
            ("an", "R.S.V.P."),
            ("an", "RSVP"),
            ("a", "REST"),
            ("a", "reindeer"),
            ("an", "S.O.S."),
            ("a", "SUM"),
            ("an", "SST"),
            ("a", "salmon"),
            ("a", "T.N.T. bomb"),
            ("a", "TNT bomb"),
            ("a", "TENT"),
            ("a", "thought"),
            ("a", "tomato"),
            ("a", "U-boat"),
            ("a", "UNESCO representative"),
            ("a", "U.F.O."),
            ("a", "UFO"),
            ("a", "UK citizen"),
            ("a", "ubiquity"),
            ("a", "unicorn"),
            ("an", "unidentified flying object"),
            ("a", "uniform"),
            ("a", "unimodal system"),
            ("an", "unimpressive record"),
            ("an", "uninformed opinion"),
            ("an", "uninvited guest"),
            ("a", "union"),
            ("a", "uniplex"),
            ("a", "uniprocessor"),
            ("a", "unique opportunity"),
            ("a", "unisex hairdresser"),
            ("a", "unison"),
            ("a", "unit"),
            ("a", "unitarian"),
            ("a", "united front"),
            ("a", "unity"),
            ("a", "univalent bond"),
            ("a", "univariate statistic"),
            ("a", "universe"),
            ("an", "unordered meal"),
            ("a", "uranium atom"),
            ("an", "urban myth"),
            ("an", "urbane miss"),
            ("an", "urchin"),
            ("a", "urea detector"),
            ("a", "urethane monomer"),
            ("an", "urge"),
            ("an", "urgency"),
            ("a", "urinal"),
            ("an", "urn"),
            ("a", "usage"),
            ("a", "use"),
            ("an", "usher"),
            ("a", "usual suspect"),
            ("a", "usurer"),
            ("a", "usurper"),
            ("a", "utensil"),
            ("a", "utility"),
            ("an", "utmost urgency"),
            ("a", "utopia"),
            ("an", "utterance"),
            ("a", "V.I.P."),
            ("a", "VIPER"),
            ("a", "viper"),
            ("an", "X-ray"),
            ("an", "X.O."),
            ("a", "XYLAPHONE"),
            ("an", "XY chromosome"),
            ("a", "xenophobe"),
            ("a", "Y-shaped pipe"),
            ("a", "Y.Z. plane"),
            ("a", "YMCA"),
            ("an", "YBLENT eye"),
            ("an", "yblent eye"),
            ("an", "yclad body"),
            ("a", "yellowing"),
            ("a", "yield"),
            ("a", "youth"),
            ("a", "youth"),
            ("an", "ypsiliform junction"),
            ("an", "yttrium atom"),
            ("a", "zoo"),
        ];
        for test in tests {
            assert_eq!(
                super::indefinite_article_for(test.1),
                test.0,
                "indefinite_article_for({}) = {}",
                test.1,
                test.0,
            );
        }
    }
}
