use crate::util::*;
use once_cell::sync::Lazy;
use regex::{Captures, Regex};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
enum Noun {
    Simple(SimpleNoun),
    Patterns(NounPatterns),
}

#[derive(Debug)]
struct SimpleNoun {
    singular: String,
    modern_plural: String,
    classical_plural: Option<String>,
}

#[derive(Debug)]
struct NounPatterns {
    to_modern_plural: Transformation,
    to_classical_plural: Option<Transformation>,
    to_singular_from_modern: Transformation,
    to_singular_from_classical: Option<Transformation>,
    tags: Tags,
}

#[derive(Clone, Debug)]
struct Tags {
    nonindicative: bool,
    only_dashed: bool,
}

#[derive(Debug, PartialEq)]
struct Transformation {
    from_re: String,
    to: Replacement,
}

#[derive(Debug, PartialEq)]
enum Replacement {
    // A replacement string that can be passed to `Regex::replace` like
    // "${1}staffs".
    Stringy(String),
    // A snippet of code that will do something with the Regex captures. This
    // is used when the pattern contains generics like "(PREP)" or "(SING)",
    // and is used to transform preposition-containing words like
    // "son-of-a-gun" => "sons-of-guns", => "mother-in-law" to
    // "mothers-in-law", and "man-at-arms" => "men-at-arms".
    Logicy(String, Option<usize>),
}

#[derive(Clone, Debug)]
struct ParsedLine<'a> {
    tags: Tags,
    is_generic_re_str: Option<&'static str>,
    is_pure_generic: bool,
    singular: &'a str,
    modern_plural: &'a str,
    classical_plural: Option<&'a str>,
}

impl<'a> ParsedLine<'a> {
    fn is_generic(&self) -> bool {
        self.is_generic_re_str.is_some()
    }

    fn is_recursive(&self) -> bool {
        self.singular.contains("(SING)") || self.singular.contains("(PREP)")
    }
}

pub(crate) fn generate(source_dir: PathBuf, target_dir: PathBuf) -> (String, PathBuf) {
    generate_nouns_rs_file(source_dir, target_dir)
}

const NOUNS_TEMPLATE: &str = include_str!("nouns.rs.template");

fn generate_nouns_rs_file(source_dir: PathBuf, target_dir: PathBuf) -> (String, PathBuf) {
    let lines = read_lines(&source_dir, "nouns.lei");
    let nouns = lines_to_nouns(&lines);

    let mut nouns_rs = insert_simple_data(&nouns);
    nouns_rs = insert_pattern_data(nouns_rs, &nouns);
    nouns_rs = insert_test_data(&source_dir, nouns_rs);

    let mut nouns_rs_file = target_dir;
    nouns_rs_file.push("nouns.rs");

    (nouns_rs, nouns_rs_file)
}

fn lines_to_nouns(lines: &[String]) -> Vec<Noun> {
    lines
        .iter()
        .map(|l| line_to_noun(l.as_str()))
        .flatten()
        .flatten()
        .collect::<Vec<Noun>>()
}

static NOUN_RE: Lazy<Regex> = Lazy::new(|| {
    let comment = r"(?:\# .*)";
    let ws = r"[\s]*";
    // Note that in Damian's code, he captures the generic marker for the
    // classical plural, but then when the code actually uses the regex,
    // it ignores that field. So we just won't capture it at all.
    let re_str = format!(
        r#"(?xms)
            \A
              (?: {ws} < (?P<tags>[^>]+) > )?           # ...optional category tag
              {ws} (?P<is_generic>[*-])? {ws}             # ...leading whitespace and optional generic marker
              (?P<singular>.*?)                         # ...singular word
              {ws} =>                                   # ...singular/plural separator
              {ws} (?P<is_pure_generic>[*-]?) {ws}        # ...leading whitespace and optional generic marker
              (?P<plural>.*?)                           # ...plural of word
              (?:                                       # ...optionally:
                {ws} \|                                 #    ...modern/classical separator
                {ws} (?:[*-])? {ws}                       #    ...leading whitespace and optional generic marker
                (?P<classical_plural>.*?)               #    ...classical plural of word
              )?                                        # ...trailing whitespace
            {ws}                                        # Optional trailing comment
            {comment}?                                  # ...trailing whitespace
            \z
        "#,
        ws = ws,
        comment = comment,
    );
    Regex::new(&re_str).expect("Could not parse data line regex")
});

fn line_to_noun(line: &str) -> Option<Vec<Noun>> {
    if BLANK_LINE_RE.is_match(line) || COMMENT_LINE_RE.is_match(line) {
        return None;
    }

    let parsed = parse_data_from_line(line);
    if parsed.is_generic() {
        let mut nouns = parsed_to_generic_patterns(parsed.clone());
        if !parsed.is_pure_generic {
            let mut simple = parsed_to_simple(parsed);
            nouns.append(&mut simple);
        }
        return Some(nouns);
    }
    if parsed.is_recursive() {
        return Some(parsed_to_recursive_patterns(parsed));
    }
    Some(parsed_to_simple(parsed))
}

fn parse_data_from_line(line: &str) -> ParsedLine {
    let caps = NOUN_RE
        .captures(line)
        .unwrap_or_else(|| panic!("Noun RE did not match line: [{}]", line));

    let tags = caps.name("tags").map(|t| t.as_str());
    let is_generic_re_str = maybe_is_generic_re_str(caps.name("is_generic").map(|m| m.as_str()));

    let singular = {
        let c = caps
            .name("singular")
            .unwrap_or_else(|| panic!("Line did not have a singular form: {}", line));
        c.as_str()
    };
    if singular.is_empty() && is_generic_re_str.is_none() {
        panic!(
            "Line has an empty string for singular form and is not a generic: {}",
            line
        );
    }

    let mp = caps.name("plural");
    let cp = caps.name("classical_plural");
    let modern_plural = match mp {
        Some(m) if !m.as_str().is_empty() => m.as_str(),
        Some(_) | None => cp.map_or("", |c| c.as_str()),
    };
    let classical_plural = match cp {
        // XXX - There's one line (for virus) where the regex produces an
        // empty string for the classical plural. But when I tried to fix the
        // regex to not match for this capture everything went wonky.
        Some(c) => match c.as_str() {
            "" => None,
            c => Some(c),
        },
        None => None,
    };

    let tags: HashSet<String> = tags.map_or_else(HashSet::new, |t: &str| {
        t.split(',').map(String::from).collect()
    });

    let is_pure_generic = matches!(
        caps.name("is_pure_generic").map(|m| m.as_str()),
        Some("-") | Some("*"),
    );

    ParsedLine {
        tags: Tags {
            nonindicative: tags.contains("nonindicative"),
            only_dashed: tags.contains("only-dashed"),
        },
        is_generic_re_str,
        is_pure_generic,
        singular,
        modern_plural,
        classical_plural,
    }
}

fn parsed_to_generic_patterns(parsed: ParsedLine<'_>) -> Vec<Noun> {
    let leading_re_str = parsed.is_generic_re_str.unwrap();
    let (singular_replacement, singular_from_re) =
        replacement_and_regex_str_for_pattern_match(leading_re_str, parsed.singular);
    let (modern_plural_replacement, modern_plural_from_re) =
        replacement_and_regex_str_for_pattern_match(leading_re_str, parsed.modern_plural);
    let (classical_plural_replacement, classical_plural_from_re) = match parsed.classical_plural {
        None => (None, None),
        Some(cp) => {
            let (classical_plural_replacement, classical_plural_from_re) =
                replacement_and_regex_str_for_pattern_match(leading_re_str, cp);
            (
                Some(classical_plural_replacement),
                Some(classical_plural_from_re),
            )
        }
    };

    vec![Noun::Patterns(NounPatterns {
        to_modern_plural: Transformation {
            from_re: singular_from_re.clone(),
            to: Replacement::Stringy(format!("${{1}}{}", modern_plural_replacement)),
        },
        to_classical_plural: classical_plural_replacement.map(|cps| Transformation {
            from_re: singular_from_re,
            to: Replacement::Stringy(format!("${{1}}{}", cps)),
        }),
        to_singular_from_modern: Transformation {
            from_re: modern_plural_from_re,
            to: Replacement::Stringy(format!("${{1}}{}", singular_replacement)),
        },
        to_singular_from_classical: classical_plural_from_re.map(|cp_re| Transformation {
            from_re: cp_re,
            to: Replacement::Stringy(format!("${{1}}{}", singular_replacement)),
        }),
        tags: parsed.tags,
    })]
}

fn parsed_to_recursive_patterns(parsed: ParsedLine) -> Vec<Noun> {
    let singular = parsed.singular;
    let modern_plural = parsed.modern_plural;

    let mut variations: Vec<(String, String)> =
        vec![(singular.to_string(), modern_plural.to_string())];
    if !parsed.tags.only_dashed {
        variations.push((singular.replace('-', " "), modern_plural.replace('-', " ")));
    }
    // We want one version with dashes and one without.
    variations.dedup();

    variations
        .iter()
        .map(|(singular, modern_plural)| {
            // These are recursive because the logic that does conversion will
            // end up calling the convert_to_X function again internally. For
            // example, when we call `convert_to_modern_plural("son-in-law")`,
            // internally it has to call `convert_to_modern_plural("son")`
            let (
                singular_from_re,
                modern_plural_from_re,
                classical_plural_from_re,
                to_singular_logic,
                to_modern_plural_logic,
                to_classical_plural_logic,
                recursive_capture_idx,
            ) = match_and_logic_for_recursive_pattern(singular, modern_plural);

            Noun::Patterns(NounPatterns {
                to_modern_plural: Transformation {
                    from_re: singular_from_re.clone(),
                    to: Replacement::Logicy(to_modern_plural_logic, recursive_capture_idx),
                },
                to_classical_plural: Some(Transformation {
                    from_re: singular_from_re,
                    to: Replacement::Logicy(to_classical_plural_logic, recursive_capture_idx),
                }),
                to_singular_from_modern: Transformation {
                    from_re: modern_plural_from_re,
                    to: Replacement::Logicy(to_singular_logic.clone(), recursive_capture_idx),
                },
                to_singular_from_classical: Some(Transformation {
                    from_re: classical_plural_from_re,
                    to: Replacement::Logicy(to_singular_logic, recursive_capture_idx),
                }),
                tags: parsed.tags.clone(),
            })
        })
        .collect::<Vec<Noun>>()
}

fn match_and_logic_for_recursive_pattern(
    singular: &str,
    modern_plural: &str,
) -> (
    String,
    String,
    String,
    String,
    String,
    String,
    Option<usize>,
) {
    let (singular_from_re, to_modern_plural_logic, recursive_capture_idx) =
        from_re_and_logic_for_recursive_pattern(singular, modern_plural, "modern_plural");
    let (_, to_classical_plural_logic, _) =
        from_re_and_logic_for_recursive_pattern(singular, modern_plural, "classical_plural");
    let (modern_plural_from_re, to_singular_logic, _) =
        from_re_and_logic_for_recursive_pattern(modern_plural, singular, "singular");
    // The classical pattern is always the same as modern, because these
    // patterns are generic and have nothing to do with specific plural words.
    let (classical_plural_from_re, _, _) =
        from_re_and_logic_for_recursive_pattern(modern_plural, singular, "singular");

    (
        singular_from_re,
        classical_plural_from_re,
        modern_plural_from_re,
        to_singular_logic,
        to_modern_plural_logic,
        to_classical_plural_logic,
        recursive_capture_idx,
    )
}

static RECURSIVE_PATTERN_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r#"(?x)
          (?P<star>  \*          )
        | (?P<sing>  \(SING\)    )
        | (?P<plur>  \(PL\)      )
        | (?P<prep>  \(PREP\)    )
        | (?P<text>  [^\*\(\)]+  )
        "#,
    )
    .expect("Could not parse recursive pattern")
});

// The logic in this is pretty complicated. First we go through the
// `from_pattern` and replace markers like "*" or "(SING)" with capturing regex
// matches, turning the pattern into an actual regex. That regex will either
// have capturing groups that match anything, a preposition, or specific text
// like "-of-a-gun".
//
// At the same time, as we go through the `from_pattern` we are recording two
// things. We record bits of logic (Rust code) to assign the results of the
// regex capture (the regex that we're creating as we go) to variables. And we
// also record what to push into the new word we're creating, which is
// basically "capture at index X". Those are the replacements.
//
// Finally, we go through the `to_pattern` and use its contents to determine
// how to generate the new word. Wherever the `to_pattern` has a marker like
// "*" or "(SING)" we know that piece will be replaced by a particular capture
// at a specific index. For all other text, we use that text as-is.
//
// If this doesn't make any sense the best way to understand this is to search
// for "(SING)" in the nouns.lei file, and then look at the code that is
// generated for `convert_to_modern_plural` or any of the other `convert_to_X`
// functions.
fn from_re_and_logic_for_recursive_pattern(
    from_pattern: &str,
    to_pattern: &str,
    to_case: &str,
) -> (String, String, Option<usize>) {
    let orig_from_pattern = from_pattern.to_string();

    let mut replacements: Vec<String> = vec![];
    let mut cap_count = 0;
    let mut logic = String::new();
    let mut recursive_capture_idx: Option<usize> = None;

    let from_pattern = RECURSIVE_PATTERN_RE
        .replace_all(from_pattern, |caps: &Captures| {
            let var_name = if caps.name("text").is_none() {
                cap_count += 1;
                let v = format!("cap{}", cap_count);
                logic.push_str(&format!(
                    "let {} = caps.get({}).unwrap().as_str();",
                    v,
                    cap_count,
                ));
                Some(v)
            } else {
                None
            };
            if caps.name("star").is_some() {
                    replacements.push(var_name.unwrap());
                    return "(.*?)".to_string();
            }
            if caps.name("sing").is_some() {
                recursive_capture_idx = Some(cap_count);
                logic.push_str(&format!(
                    r#"let plural = if is_singular({var_name}) {{ convert_to_{to_case}({var_name}).to_string() }} else {{ {var_name}.to_string() }};"#,
                    var_name = var_name.unwrap(),
                    to_case = to_case,
                ));
                replacements.push("&plural".to_string());
                return "(.*?)".to_string();
            }
            if caps.name("plur").is_some() {
                recursive_capture_idx = Some(cap_count);
                logic.push_str(&format!(
                    r#"let singular = if is_plural({var_name}) {{ convert_to_{to_case}({var_name}).to_string() }} else {{ {var_name}.to_string() }};"#,
                    var_name = var_name.unwrap(),
                    to_case = to_case,
                ));
                replacements.push("&singular".to_string());
                return "(.*?)".to_string();
            }
            if caps.name("prep").is_some() {
                replacements.push(var_name.unwrap());
                return "({preposition_re})".to_string();
            }
            if let Some(e) = caps.name("text") {
                return format!("(?:{})", e.as_str());
            }
            panic!("Unexpected match in recursive pattern! {:?}", caps);
        })
        .to_string();

    logic.push_str("let mut new_word = String::new();");

    let mut replacements = replacements.into_iter();
    for caps in RECURSIVE_PATTERN_RE.captures_iter(to_pattern) {
        if caps.name("star").is_some()
            || caps.name("sing").is_some()
            || caps.name("plur").is_some()
            || caps.name("prep").is_some()
        {
            logic.push_str(&format!(
                r##"new_word.push_str({});"##,
                replacements.next().unwrap()
            ));
            continue;
        }
        if let Some(e) = caps.name("text").map(|t| t.as_str()) {
            if e.len() == 1 {
                logic.push_str(&format!(r#"new_word.push('{}');"#, e));
            } else {
                logic.push_str(&format!(r#"new_word.push_str("{}");"#, e));
            }
            continue;
        }
        panic!(
            "to_pattern contained more matches than from_pattern: {} => {}",
            orig_from_pattern, to_pattern
        );
    }

    logic.push('\n');
    logic.push_str(r##"        #[cfg(feature = "debug")]"##);
    logic.push('\n');
    logic.push_str(&format!(r##"        println!(r#"  word '{{}}' matched recursive pattern '{}' and became {{}}"#, word, new_word);"##, orig_from_pattern));

    if replacements.next().is_some() {
        panic!(
            "to_pattern contained fewer matches than from_pattern: {} => {}",
            orig_from_pattern, to_pattern
        );
    }

    // This pattern needs to be anchored so that a trailing "(.*?)" matches to
    // the end of the string instead of matching nothing.
    (
        format!(r"(?i)\A{}$", from_pattern),
        logic,
        recursive_capture_idx,
    )
}

fn parsed_to_simple(parsed: ParsedLine) -> Vec<Noun> {
    let singular = parsed.singular;
    let modern_plural = parsed.modern_plural;
    let classical_plural = parsed.classical_plural;

    let mut nouns = vec![Noun::Simple(SimpleNoun {
        singular: singular.to_string(),
        modern_plural: modern_plural.to_string(),
        classical_plural: classical_plural.map(|cp| cp.to_string()),
    })];
    // For words with dashes like "break-away" we also add a version where the
    // dashes are replaced with spaces, e.g. "break away".
    if singular.contains('-') && !parsed.tags.only_dashed {
        nouns.push(Noun::Simple(SimpleNoun {
            singular: singular.replace('-', " "),
            modern_plural: modern_plural.replace('-', " "),
            classical_plural: classical_plural.map(|cp| cp.replace('-', " ")),
        }));
    }

    nouns
}

fn insert_simple_data(nouns: &[Noun]) -> String {
    let mut modern_plural_of: HashMap<&str, &str> = HashMap::new();
    let mut classical_plural_of: HashMap<&str, &str> = HashMap::new();
    let mut singular_of: HashMap<&str, &str> = HashMap::new();

    let mut is_plural: Vec<&str> = vec![];
    let mut is_singular: Vec<&str> = vec![];

    for n in nouns.iter().filter_map(|n| match n {
        Noun::Simple(n) => Some(n),
        _ => None,
    }) {
        if !n.modern_plural.is_empty() {
            // There are some cases where there are two pluralizations of a
            // word because it's present as both a generic and a simple
            // conversion, like "*nucleus" and "nucleus".
            if !modern_plural_of.contains_key(n.singular.as_str()) {
                modern_plural_of.insert(&n.singular, &n.modern_plural);
            }
            // There are cases where there are plurals that have ambiguous
            // singular forms, like "bases", which can become either "base" or
            // "basis". We always want to prefer the conversion that comes
            // first in the source data.
            if !singular_of.contains_key(n.modern_plural.as_str()) {
                singular_of.insert(&n.modern_plural, &n.singular);
            }
            is_plural.push(&n.modern_plural);
        }

        if let Some(cp) = &n.classical_plural {
            if !classical_plural_of.contains_key(n.singular.as_str()) {
                classical_plural_of.insert(&n.singular, cp);
            }
            if !singular_of.contains_key(cp.as_str()) {
                singular_of.insert(cp, &n.singular);
            }
            is_plural.push(cp);
        } else {
            // Absent an explicit classical plural, we always fall back to
            // modern.
            if !classical_plural_of.contains_key(n.singular.as_str()) {
                classical_plural_of.insert(&n.singular, &n.modern_plural);
            }
        }

        is_singular.push(&n.singular);
    }

    let mut nouns_rs = NOUNS_TEMPLATE.to_string();
    for (var_name, hm) in [
        ("SINGULAR_OF", singular_of),
        ("MODERN_PLURAL_OF", modern_plural_of),
        ("CLASSICAL_PLURAL_OF", classical_plural_of),
    ] {
        nouns_rs = add_hashmap_from_pairs_list(nouns_rs, var_name, &hashmap_to_sorted_pairs(hm));
    }

    for (var_name, mut list) in [("IS_SINGULAR", is_singular), ("IS_PLURAL", is_plural)] {
        list.dedup();
        list.sort_by_key(|w| w.to_lowercase());
        nouns_rs = add_hashset_from_list(nouns_rs, var_name, &list);
    }

    nouns_rs
}

fn insert_pattern_data(mut nouns_rs: String, nouns: &[Noun]) -> String {
    let mut plural_to_singular_transformations: Vec<(&Transformation, bool)> = vec![];
    let mut singular_to_classical_plural_transformations: Vec<(&Transformation, bool)> = vec![];
    let mut singular_to_modern_plural_transformations: Vec<(&Transformation, bool)> = vec![];

    for n in nouns.iter().filter_map(|n| match n {
        Noun::Patterns(n) => Some(n),
        _ => None,
    }) {
        plural_to_singular_transformations.push((&n.to_singular_from_modern, n.tags.nonindicative));
        if let Some(t) = &n.to_singular_from_classical {
            plural_to_singular_transformations.push((t, n.tags.nonindicative));
        }
        singular_to_modern_plural_transformations.push((&n.to_modern_plural, n.tags.nonindicative));
        singular_to_classical_plural_transformations.push((
            match &n.to_classical_plural {
                Some(t) => t,
                None => &n.to_modern_plural,
            },
            n.tags.nonindicative,
        ));
    }

    // There can be dupes because this combines both classical and modern
    // plurals, and there are a few cases where these are identical. We dedupe
    // by looking at the recognizer regex because if we have two identical
    // regexes, any regex after the first will never be invoked. A case where
    // this happens is with "staves", which is both a modern plural (for
    // "stave") and classical plural (for "staff"). But we will always convert
    // "staves" back to "staff", because we don't have any notion of a
    // "classical singular".
    plural_to_singular_transformations.dedup_by_key(|(trans, _)| &trans.from_re);

    // We only need to do this once, since all the singular recognizers are in
    // the singular_to_modern_plural_transformations Vec.
    nouns_rs = add_recognizers(
        &mut nouns_rs,
        "SINGULAR_RECOGNIZERS",
        &singular_to_modern_plural_transformations,
    );

    nouns_rs = add_recognizers(
        &mut nouns_rs,
        "PLURAL_RECOGNIZERS",
        &plural_to_singular_transformations,
    );

    nouns_rs = add_transformations(
        &mut nouns_rs,
        "SINGULAR_RECOGNIZERS",
        "SINGULAR_TO_MODERN_PLURAL_TRANSFORMATIONS",
        &singular_to_modern_plural_transformations
            .iter()
            .map(|p| p.0)
            .collect::<Vec<&Transformation>>(),
    );
    nouns_rs = add_transformations(
        &mut nouns_rs,
        "SINGULAR_RECOGNIZERS",
        "SINGULAR_TO_CLASSICAL_PLURAL_TRANSFORMATIONS",
        &singular_to_classical_plural_transformations
            .iter()
            .map(|p| p.0)
            .collect::<Vec<&Transformation>>(),
    );
    nouns_rs = add_transformations(
        &mut nouns_rs,
        "PLURAL_RECOGNIZERS",
        "PLURAL_TO_SINGULAR_TRANSFORMATIONS",
        &plural_to_singular_transformations
            .iter()
            .map(|p| p.0)
            .collect::<Vec<&Transformation>>(),
    );

    nouns_rs
}

fn add_recognizers(
    template: &mut String,
    which: &str,
    transformations: &[(&Transformation, bool)],
) -> String {
    let recognizers_str = transformations
        .iter()
        .enumerate()
        .map(|(i, (t, n))| {
            if t.from_re.contains("{preposition_re}") {
                format!(
                    r##"Recognizer {{ regex: Regex::new(&format!(r#"{regex}"#, preposition_re = PREPOSITION_REGEX_STR) ).unwrap(), is_nonindicative: {is_nonindicative}, recursive_capture_idx: {recursive_capture_idx} }}, // {idx}"##,
                    regex = t.from_re,
                    is_nonindicative = n,
                    idx = i,
                    recursive_capture_idx = t.recursive_capture_idx(),
                )
            } else {
                format!(
                    r##"Recognizer {{ regex: Regex::new(r#"{regex}"#).unwrap(), is_nonindicative: {is_nonindicative}, recursive_capture_idx: {recursive_capture_idx} }}, // {idx}"##,
                    regex = t.from_re,
                    is_nonindicative = n,
                    idx = i,
                    recursive_capture_idx = t.recursive_capture_idx(),
                )
            }
        })
        .collect::<Vec<String>>()
        .join("\n");
    let re = template_match_re(which);
    re.replace(template, recognizers_str).to_string()
}

fn add_transformations(
    template: &mut String,
    recognizer: &str,
    which: &str,
    transformations: &[&Transformation],
) -> String {
    let transformations_str = transformations
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let transformation = match &t.to {
                Replacement::Stringy(r) => format!(
                    r##"
    // {regex}
    if let Some(caps) = {recognizer}[{i}].regex.captures(word) {{
        let mut new_word = String::new();
        caps.expand(r#"{replacement}"#, &mut new_word);
        #[cfg(feature = "debug")]
        println!(r#"  word '{{}}' matched regex {{:?}} (idx {i}) and became {{}}"#, word, {recognizer}[{i}].regex, new_word);
        return Cow::Owned(new_word);
    }}
"##,
                    regex = t.from_re,
                    recognizer = recognizer,
                    i = i,
                    replacement = r
                ),
                Replacement::Logicy(l, _) => format!(
                    r##"
    // {regex}
    if let Some(caps) = {recognizer}[{i}].regex.captures(word) {{
        {logic}
        return Cow::Owned(new_word);
    }}
"##,
                    regex = t.from_re,
                    recognizer = recognizer,
                    i = i,
                    logic = l
                ),
            };

            transformation
                .trim_start_matches('\n')
                .trim_end_matches('\n')
                .to_string()
        })
        .collect::<Vec<String>>()
        .join("\n");

    // If the string we pass to re.replace as the replace contains dollar
    // signs, those are interpreted as references to matching groups. We could
    // try to escape them, but I tried that and couldn't get it right. So
    // we'll just replace them with a fun Unicode symbol and then replace them
    // back after the call to re.replace.
    let dollar_replacer = "👍";

    let re = template_match_re(which);
    re.replace(template, &transformations_str.replace("$", dollar_replacer))
        // This is the str.replace method
        .replace(dollar_replacer, "$")
}

impl Transformation {
    fn recursive_capture_idx(&self) -> String {
        match self.to {
            Replacement::Stringy(_) | Replacement::Logicy(_, None) => String::from("None"),
            Replacement::Logicy(_, Some(recursive_capture_idx)) => {
                format!("Some({})", recursive_capture_idx)
            }
        }
    }
}

struct Plurals {
    modern: Option<String>,
    classical: Option<String>,
    classical_to_singular: Option<String>,
    is_ambiguous: bool,
}

fn insert_test_data(source_dir: &Path, nouns_rs: String) -> String {
    let mut conversions = conversions_from_nouns_general(source_dir);
    conversions = conversions_from_custom_data(conversions);

    let widths = (
        conversions.keys().map(String::len).max().unwrap(),
        conversions
            .values()
            .map(|p| p.modern.as_ref().map(|m| m.len()).unwrap_or(0))
            .max()
            .unwrap(),
        conversions
            .values()
            .map(|p| p.classical.as_ref().map(|c| c.len()).unwrap_or(0))
            .max()
            .unwrap(),
        conversions
            .values()
            .map(|p| {
                p.classical_to_singular
                    .as_ref()
                    .map(|c| c.len())
                    .unwrap_or(0)
            })
            .max()
            .unwrap(),
    );

    let mut test_data = one_conversion_line(
        widths,
        "Singular",
        "Modern Plural",
        "Classical Plural",
        "Classical Plural to Singular",
        true,
        false,
    );
    test_data.push_str(&one_conversion_line(
        widths,
        &"_".repeat(widths.0),
        &"_".repeat(widths.1),
        &"_".repeat(widths.2),
        &"_".repeat(widths.3),
        true,
        false,
    ));

    let mut keys = conversions
        .keys()
        .map(|k| k.as_str())
        .collect::<Vec<&str>>();
    keys.sort_by_key(|k| k.to_lowercase());
    for singular in keys {
        let plurals = conversions.get(singular).unwrap();
        test_data.push_str(&one_conversion_line(
            widths,
            singular,
            plurals.modern.as_deref().unwrap_or(""),
            plurals.classical.as_deref().unwrap_or(""),
            plurals.classical_to_singular.as_deref().unwrap_or(""),
            false,
            plurals.is_ambiguous,
        ));
    }

    nouns_rs.replace(
        "    // CONVERSIONS_TEST_DATA",
        &format!(
            r##"static CONVERSIONS_TEST_DATA: &str = r#"{}{}"#;"##,
            "\n", test_data
        ),
    )
}

static NOUNS_DATA_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r"(?x)
        \A \s*
        (?P<is_ambiguous>!?)
        \s*
        (?P<singular>[- a-zA-Z]+)
        \s* => \s*
        (?P<modern_plural>[- a-zA-Z]*?)
        \s*
        (?:
            [|]
            \s*
            (?P<classical_plural>[- a-zA-Z]+)
        )?
        (?:
            \s*
            =>
            \s*
            (?P<classical_plural_to_singular>[- a-zA-Z]+)
        )?
        \s* $",
    )
    .unwrap()
});

fn conversions_from_nouns_general(source_dir: &Path) -> HashMap<String, Plurals> {
    let source = read_lines(source_dir, "noun_general.t");
    conversions_from_test_data(
        HashMap::new(),
        &source
            .iter()
            .skip_while(|l| !l.starts_with("__DATA__"))
            .collect::<Vec<&String>>(),
    )
}

fn conversions_from_custom_data(conversions: HashMap<String, Plurals>) -> HashMap<String, Plurals> {
    let custom_data = r#"
  # words that don't inflect at all in classical form
  djinn              =>  djinns              |  djinn
  poise              =>  poises              |  poise
  veg                =>  veges               |  veg

  # generics
  stave              =>  staves              |  staves              =>  stave
  staff              =>  staffs              |  staves              =>  stave
  quarterstave       =>  quarterstaves       |                      =>  quarterstave
  quarterstaff       =>  quarterstaffs       |  quarterstaves       =>  quarterstave
  pikestaff          =>  pikestaffs          |  pikestaves          =>  pikestave
  pikestave          =>  pikestaves          |                      =>  pikestave
  waterfowl          =>  waterfowls          |  waterfowl
  gas                =>  gases
  biogas             =>  biogases
  glottis            =>  glottises
  epiglottis         =>  epiglottises
  five star general  =>  five star generals

  horse              =>  horses
  horse              =>  horses
  hearse             =>  hearses
  curse              =>  curses
  sense              =>  senses

  # recursive patterns
  consul-general     =>  consuls-general
  # skipping the "(PREP) it => (PREP) them" pattern because it never converts
  # back to singular - see https://rt.cpan.org/Ticket/Display.html?id=140272
  son-of-a-gun       =>  sons-of-guns
  son of a gun       =>  sons of guns
  # these next two fail because of an ordering issue - see
  # https://rt.cpan.org/Ticket/Display.html?id=140273
  # son-of-a-staff     =>  sons-of-staffs      |  sons-of-staves
  # son of a staff     =>  sons of staffs      |  sons of staves
  mother-in-law      =>  mothers-in-law
  mother in law      =>  mothers in law
  from cow           =>  from cows           |  from kine
  knight-errant      =>  knights-errant
  knight errant      =>  knights errant
  passer-by          =>  passers-by
  passer by          =>  passers by
"#;
    conversions_from_test_data(conversions, &custom_data.lines().collect::<Vec<&str>>())
}

fn conversions_from_test_data<A: AsRef<str>>(
    mut conversions: HashMap<String, Plurals>,
    source: &[A],
) -> HashMap<String, Plurals> {
    for caps in source
        .iter()
        .map(|s| s.as_ref())
        .filter_map(|l| NOUNS_DATA_REGEX.captures(l))
    {
        let singular = caps
            .name("singular")
            .map(|m| m.as_str().to_string())
            .unwrap_or_else(|| panic!("Line in nouns_general.t did not have a singular form"));
        conversions.insert(
            singular,
            Plurals {
                modern: caps.name("modern_plural").map(|m| m.as_str().to_string()),
                classical: caps
                    .name("classical_plural")
                    .map(|m| m.as_str().to_string()),
                classical_to_singular: caps
                    .name("classical_plural_to_singular")
                    .map(|m| m.as_str().to_string()),
                is_ambiguous: matches!(caps.name("is_ambiguous").map(|m| m.as_str()), Some("!")),
            },
        );
    }

    conversions
}

fn one_conversion_line(
    widths: (usize, usize, usize, usize),
    singular: &str,
    modern: &str,
    classical: &str,
    classical_to_singular: &str,
    is_comment: bool,
    is_ambiguous: bool,
) -> String {
    let lead = if is_comment {
        "# "
    } else if is_ambiguous {
        "! "
    } else {
        "  "
    };
    format!(
        "{lead}{s:w0$}  |  {mp:w1$}  |  {cp:w2$} |  {cp2s:w3$}\n",
        lead = lead,
        s = singular,
        w0 = widths.0,
        mp = modern,
        w1 = widths.1,
        cp = classical,
        w2 = widths.2,
        cp2s = classical_to_singular,
        w3 = widths.3,
    )
}
