use crate::util::*;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

#[derive(Debug)]
enum Verb {
    Simple(SimpleVerb),
    Patterns(VerbPatterns),
}

#[derive(Debug)]
struct SimpleVerb {
    singular: String,
    plural: String,
    singular_past: Option<String>,
    plural_past: Option<String>,
    present_participle: Option<String>,
    past_participle: Option<String>,
}

#[derive(Debug)]
struct VerbPatterns {
    singular_recognizer: String,
    singular_replacement: String,
    plural_recognizer: String,
    plural_replacement: String,
    past_recognizer: Option<String>,
    past_replacement: Option<String>,
    present_participle_recognizer: Option<String>,
    present_participle_replacement: Option<String>,
    past_participle_recognizer: Option<String>,
    past_participle_replacement: Option<String>,
}

#[derive(Debug)]
struct ParsedLine<'a> {
    singular_re_str: Option<&'a str>,
    singular: &'a str,
    plural_re_str: Option<&'a str>,
    plural: &'a str,
    past_re_str: Option<&'a str>,
    singular_past: Option<&'a str>,
    plural_past: Option<&'a str>,
    present_participle_re_str: Option<&'a str>,
    present_participle: Option<&'a str>,
    past_participle_re_str: Option<&'a str>,
    past_participle: Option<&'a str>,
}

impl<'a> ParsedLine<'a> {
    fn is_generic(&self) -> bool {
        [
            self.singular_re_str,
            self.plural_re_str,
            self.past_re_str,
            self.present_participle_re_str,
            self.past_participle_re_str,
        ]
        .iter()
        .any(Option::is_some)
    }
}

pub(crate) fn generate(source_dir: PathBuf, target_dir: PathBuf) -> (String, PathBuf) {
    generate_verbs_rs_file(source_dir, target_dir)
}

const VERBS_TEMPLATE: &str = include_str!("verbs.rs.template");

fn generate_verbs_rs_file(source_dir: PathBuf, target_dir: PathBuf) -> (String, PathBuf) {
    let lines = read_lines(&source_dir, "verbs.lei");
    let verbs = lines_to_verbs(&lines);

    let mut verbs_rs = insert_simple_data(&verbs);
    verbs_rs = insert_pattern_data(verbs_rs, &verbs);
    verbs_rs = insert_test_data(&source_dir, verbs_rs);

    let mut verbs_rs_file = target_dir;
    verbs_rs_file.push("verbs.rs");

    (verbs_rs, verbs_rs_file)
}

fn lines_to_verbs(lines: &[String]) -> Vec<Verb> {
    lines
        .iter()
        .map(|l| line_to_verb(l.as_str()))
        .flatten()
        .flatten()
        .collect::<Vec<Verb>>()
}

fn line_to_verb(line: &str) -> Option<Vec<Verb>> {
    if BLANK_LINE_RE.is_match(line) || COMMENT_LINE_RE.is_match(line) {
        return None;
    }

    let parsed = parse_data_from_line(line);
    if parsed.is_generic() {
        return Some(parsed_to_patterns(parsed));
    }
    Some(parsed_to_simple(parsed))
}

fn parse_data_from_line(line: &str) -> ParsedLine {
    let verbs = line.split_whitespace().collect::<Vec<_>>();
    if verbs.len() != 5 && verbs.len() != 6 {
        panic!(
            "Splitting line on whitespace produced a surprising number of elements ({}): [{}]",
            verbs.len(),
            line
        );
    }

    let (singular_re_str, singular) = split_generic(verbs[0]);
    if singular.is_none() {
        panic!("Splitting line did not produce a singular verb: [{}]", line);
    }

    let (plural_re_str, plural) = split_generic(verbs[1]);
    if plural.is_none() {
        panic!("Splitting line did not produce a plural verb: [{}]", line);
    }

    let (past_re_str, past) = split_generic(verbs[2]);
    let (singular_past, plural_past) = if let Some(p) = past {
        let split = p.split('|').collect::<Vec<_>>();
        if split.len() == 2 {
            (Some(split[0]), Some(split[1]))
        } else {
            (Some(p), Some(p))
        }
    } else {
        (None, None)
    };

    let (present_participle_re_str, present_participle) = split_generic(verbs[3]);
    let (past_participle_re_str, past_participle) = split_generic(verbs[4]);

    ParsedLine {
        singular_re_str,
        singular: singular.unwrap(),
        plural_re_str,
        plural: plural.unwrap(),
        past_re_str,
        singular_past,
        plural_past,
        present_participle_re_str,
        present_participle,
        past_participle_re_str,
        past_participle,
    }
}

fn split_generic(verb: &str) -> (Option<&str>, Option<&str>) {
    if verb == "_" {
        return (None, None);
    }

    let (generic, split_verb) = verb.split_at(1);
    if generic.contains(&['*', '-'][..]) {
        return (maybe_is_generic_re_str(Some(generic)), Some(split_verb));
    }
    (None, Some(verb))
}

fn parsed_to_simple(parsed: ParsedLine) -> Vec<Verb> {
    let singular = parsed.singular;
    let plural = parsed.plural;

    let mut verbs = vec![Verb::Simple(SimpleVerb {
        singular: singular.to_string(),
        plural: plural.to_string(),
        singular_past: parsed.singular_past.map(|p| p.to_string()),
        plural_past: parsed.plural_past.map(|p| p.to_string()),
        present_participle: parsed.present_participle.map(|p| p.to_string()),
        past_participle: parsed.past_participle.map(|p| p.to_string()),
    })];

    // For words with dashes we also add a version where the dashes are
    // replaced with spaces, e.g. "break away". As of this writing there are
    // no verbs with dashes, but there could be in the future.
    if singular.contains('-') {
        verbs.push(Verb::Simple(SimpleVerb {
            singular: singular.replace("-", " "),
            plural: plural.replace("-", " "),
            singular_past: parsed.singular_past.map(|p| p.replace("-", " ")),
            plural_past: parsed.plural_past.map(|p| p.replace("-", " ")),
            present_participle: parsed.present_participle.map(|p| p.replace("-", " ")),
            past_participle: parsed.past_participle.map(|p| p.replace("-", " ")),
        }));
    }

    verbs
}

fn parsed_to_patterns(parsed: ParsedLine) -> Vec<Verb> {
    let (singular_replacement, singular_recognizer) = replacement_and_regex_str_for_pattern_match(
        parsed.singular_re_str.unwrap(),
        parsed.singular,
    );
    let (plural_replacement, plural_recognizer) =
        replacement_and_regex_str_for_pattern_match(parsed.plural_re_str.unwrap(), parsed.plural);
    let (past_replacement, past_recognizer) = match parsed.past_re_str {
        None => (None, None),
        Some(re) => {
            let (re, replacement) =
                replacement_and_regex_str_for_pattern_match(re, parsed.singular_past.unwrap());
            (Some(re), Some(replacement))
        }
    };
    let (past_participle_replacement, past_participle_recognizer) = match parsed
        .past_participle_re_str
    {
        None => (None, None),
        Some(re) => {
            let (re, replacement) =
                replacement_and_regex_str_for_pattern_match(re, parsed.past_participle.unwrap());
            (Some(re), Some(replacement))
        }
    };
    let (present_participle_replacement, present_participle_recognizer) = match parsed
        .present_participle_re_str
    {
        None => (None, None),
        Some(re) => {
            let (re, replacement) =
                replacement_and_regex_str_for_pattern_match(re, parsed.present_participle.unwrap());
            (Some(re), Some(replacement))
        }
    };

    // println!(
    //     "SIN REC = {} - PL REP = {}",
    //     singular_recognizer, plural_replacement
    // );
    vec![Verb::Patterns(VerbPatterns {
        singular_recognizer,
        singular_replacement: format!("${{1}}{}", singular_replacement),
        plural_recognizer,
        plural_replacement: format!("${{1}}{}", plural_replacement),
        past_recognizer,
        past_replacement: past_replacement.map(|p| format!("${{1}}{}", p)),
        present_participle_recognizer,
        present_participle_replacement: present_participle_replacement
            .map(|p| format!("${{1}}{}", p)),
        past_participle_recognizer,
        past_participle_replacement: past_participle_replacement.map(|p| format!("${{1}}{}", p)),
    })]
}

fn insert_simple_data(verbs: &[Verb]) -> String {
    let mut plural_of: HashMap<&str, &str> = HashMap::new();
    let mut singular_of: HashMap<&str, &str> = HashMap::new();
    let mut past_of: HashMap<&str, &str> = HashMap::new();
    let mut present_participle_of: HashMap<&str, &str> = HashMap::new();
    let mut past_participle_of: HashMap<&str, &str> = HashMap::new();
    let mut is_singular: Vec<&str> = vec![];
    let mut is_plural: Vec<&str> = vec![];
    let mut is_past: Vec<&str> = vec![];
    let mut is_present_participle: Vec<&str> = vec![];
    let mut is_past_participle: Vec<&str> = vec![];

    for v in verbs.iter().filter_map(|n| match n {
        Verb::Simple(n) => Some(n),
        _ => None,
    }) {
        plural_of.insert(&v.singular, &v.plural);
        singular_of.insert(&v.plural, &v.singular);

        insert_past_mappings(&mut past_of, v);
        insert_present_participle_mappings(&mut present_participle_of, v);
        insert_past_participle_mappings(&mut past_participle_of, v);

        is_singular.push(&v.singular);
        is_plural.push(&v.plural);
        if let Some(p) = &v.singular_past {
            is_past.push(p);
        }
        if let Some(p) = &v.plural_past {
            is_past.push(p);
        }
        if let Some(p) = &v.present_participle {
            is_present_participle.push(p);
        }
        if let Some(p) = &v.past_participle {
            is_past_participle.push(p);
        }
    }

    let mut verbs_rs = VERBS_TEMPLATE.to_string();
    for (var_name, hm) in [
        ("SINGULAR_OF", singular_of),
        ("PLURAL_OF", plural_of),
        ("PAST_OF", past_of),
        ("PRESENT_PARTICIPLE_OF", present_participle_of),
        ("PAST_PARTICIPLE_OF", past_participle_of),
    ] {
        verbs_rs = add_hashmap_from_pairs_list(verbs_rs, var_name, &hashmap_to_sorted_pairs(hm));
    }

    for (var_name, mut v) in [
        ("IS_SINGULAR", is_singular),
        ("IS_PLURAL", is_plural),
        ("IS_PAST", is_past),
        ("IS_PRESENT_PARTICIPLE", is_present_participle),
        ("IS_PAST_PARTICIPLE", is_past_participle),
    ] {
        v.dedup();
        v.sort_by_key(|w| w.to_lowercase());
        verbs_rs = add_hashset_from_list(verbs_rs, var_name, &v);
    }

    verbs_rs
}

fn insert_past_mappings<'a, 'b: 'a>(
    past_of: &'a mut HashMap<&'b str, &'b str>,
    verb: &'b SimpleVerb,
) {
    // This is a special case.
    if !maybe_insert_mapping(past_of, Some(&verb.plural), verb.plural_past.as_ref()) {
        maybe_insert_mapping(past_of, Some(&verb.plural), verb.singular_past.as_ref());
    }

    for pair in [
        (Some(&verb.singular), verb.singular_past.as_ref()),
        (verb.past_participle.as_ref(), verb.singular_past.as_ref()),
        (
            verb.present_participle.as_ref(),
            verb.singular_past.as_ref(),
        ),
    ] {
        maybe_insert_mapping(past_of, pair.0, pair.1);
    }
}

fn insert_present_participle_mappings<'a, 'b: 'a>(
    pres_part_of: &'a mut HashMap<&'b str, &'b str>,
    verb: &'b SimpleVerb,
) {
    for pair in [
        (Some(&verb.singular), verb.present_participle.as_ref()),
        (Some(&verb.plural), verb.present_participle.as_ref()),
        (
            verb.singular_past.as_ref(),
            verb.present_participle.as_ref(),
        ),
        (verb.plural_past.as_ref(), verb.present_participle.as_ref()),
        (
            verb.past_participle.as_ref(),
            verb.present_participle.as_ref(),
        ),
    ] {
        maybe_insert_mapping(pres_part_of, pair.0, pair.1);
    }
}

fn insert_past_participle_mappings<'a, 'b: 'a>(
    past_part_of: &'a mut HashMap<&'b str, &'b str>,
    verb: &'b SimpleVerb,
) {
    for pair in [
        (Some(&verb.singular), verb.past_participle.as_ref()),
        (Some(&verb.plural), verb.past_participle.as_ref()),
        (verb.singular_past.as_ref(), verb.past_participle.as_ref()),
        (verb.plural_past.as_ref(), verb.past_participle.as_ref()),
        (
            verb.present_participle.as_ref(),
            verb.past_participle.as_ref(),
        ),
    ] {
        maybe_insert_mapping(past_part_of, pair.0, pair.1);
    }
}

fn maybe_insert_mapping<'a, 'b: 'a>(
    mapping: &'a mut HashMap<&'b str, &'b str>,
    from: Option<&'b String>,
    to: Option<&'b String>,
) -> bool {
    if let (Some(from), Some(to)) = (from, to) {
        mapping.insert(from, to);
        return true;
    }
    false
}

struct Transformation<'a> {
    idx: usize,
    recognizer_var: &'static str,
    recognizer: &'a str,
    replacement: &'a str,
}

fn insert_pattern_data(mut verbs_rs: String, verbs: &[Verb]) -> String {
    let mut plural_to_singular_transformations: Vec<Transformation> = vec![];
    let mut singular_to_plural_transformations: Vec<Transformation> = vec![];
    let mut past_recognizer_regexes: Vec<&str> = vec![];
    let mut to_past_transformations: Vec<Transformation> = vec![];
    let mut present_participle_recognizer_regexes: Vec<&str> = vec![];
    let mut to_present_participle_transformations: Vec<Transformation> = vec![];
    let mut past_participle_recognizer_regexes: Vec<&str> = vec![];
    let mut to_past_participle_transformations: Vec<Transformation> = vec![];

    for (idx, v) in verbs
        .iter()
        .filter_map(|v| match v {
            Verb::Patterns(v) => Some(v),
            _ => None,
        })
        .enumerate()
    {
        plural_to_singular_transformations.push(Transformation {
            idx,
            recognizer_var: "PLURAL_RECOGNIZER_REGEXES",
            recognizer: &v.plural_recognizer,
            replacement: &v.singular_replacement,
        });
        singular_to_plural_transformations.push(Transformation {
            idx,
            recognizer_var: "SINGULAR_RECOGNIZER_REGEXES",
            recognizer: &v.singular_recognizer,
            replacement: &v.plural_replacement,
        });

        if let Some(replacement) = &v.past_replacement {
            past_recognizer_regexes.push(v.past_recognizer.as_ref().unwrap());
            to_past_transformations.push(Transformation {
                idx,
                recognizer_var: "SINGULAR_RECOGNIZER_REGEXES",
                recognizer: &v.singular_recognizer,
                replacement,
            });
            to_past_transformations.push(Transformation {
                idx,
                recognizer_var: "PLURAL_RECOGNIZER_REGEXES",
                recognizer: &v.plural_recognizer,
                replacement,
            });
        }

        if let Some(replacement) = &v.present_participle_replacement {
            present_participle_recognizer_regexes
                .push(v.present_participle_recognizer.as_ref().unwrap());
            to_present_participle_transformations.push(Transformation {
                idx,
                recognizer_var: "SINGULAR_RECOGNIZER_REGEXES",
                recognizer: &v.singular_recognizer,
                replacement,
            });
            to_present_participle_transformations.push(Transformation {
                idx,
                recognizer_var: "PLURAL_RECOGNIZER_REGEXES",
                recognizer: &v.plural_recognizer,
                replacement,
            });
        }

        if let Some(replacement) = &v.past_participle_replacement {
            past_participle_recognizer_regexes.push(v.past_participle_recognizer.as_ref().unwrap());
            to_past_participle_transformations.push(Transformation {
                idx,
                recognizer_var: "SINGULAR_RECOGNIZER_REGEXES",
                recognizer: &v.singular_recognizer,
                replacement,
            });
            to_past_participle_transformations.push(Transformation {
                idx,
                recognizer_var: "PLURAL_RECOGNIZER_REGEXES",
                recognizer: &v.plural_recognizer,
                replacement,
            });
        }
    }

    // XXX - deduping this breaks the idx value we set earlier. I'm not sure
    // how best to dedupe and keep the idx var correct for all transformations.
    // plural_to_singular_transformations.dedup_by_key(|t| t.recognizer);
    // singular_to_plural_transformations.dedup_by_key(|t| t.recognizer);

    verbs_rs = add_regexes(
        &mut verbs_rs,
        "SINGULAR_RECOGNIZER_REGEXES",
        &singular_to_plural_transformations
            .iter()
            .map(|t| t.recognizer)
            .collect::<Vec<&str>>(),
    );
    verbs_rs = add_transformations(
        &mut verbs_rs,
        "SINGULAR_TO_PLURAL_TRANSFORMATIONS",
        &singular_to_plural_transformations,
        false,
    );

    verbs_rs = add_regexes(
        &mut verbs_rs,
        "PLURAL_RECOGNIZER_REGEXES",
        &plural_to_singular_transformations
            .iter()
            .map(|t| t.recognizer)
            .collect::<Vec<&str>>(),
    );
    verbs_rs = add_transformations(
        &mut verbs_rs,
        "PLURAL_TO_SINGULAR_TRANSFORMATIONS",
        &plural_to_singular_transformations,
        false,
    );

    verbs_rs = add_regexes(
        &mut verbs_rs,
        "PAST_RECOGNIZER_REGEXES",
        &past_recognizer_regexes,
    );
    verbs_rs = add_transformations(
        &mut verbs_rs,
        "TO_PAST_TRANSFORMATIONS",
        &to_past_transformations,
        true,
    );

    verbs_rs = add_regexes(
        &mut verbs_rs,
        "PRESENT_PARTICIPLE_RECOGNIZER_REGEXES",
        &present_participle_recognizer_regexes,
    );
    verbs_rs = add_transformations(
        &mut verbs_rs,
        "TO_PRESENT_PARTICIPLE_TRANSFORMATIONS",
        &to_present_participle_transformations,
        true,
    );

    verbs_rs = add_regexes(
        &mut verbs_rs,
        "PAST_PARTICIPLE_RECOGNIZER_REGEXES",
        &past_participle_recognizer_regexes,
    );
    verbs_rs = add_transformations(
        &mut verbs_rs,
        "TO_PAST_PARTICIPLE_TRANSFORMATIONS",
        &to_past_participle_transformations,
        true,
    );

    verbs_rs
}

fn add_regexes(template: &mut String, which: &str, regexes: &[&str]) -> String {
    let regexes_str = regexes
        .iter()
        .enumerate()
        .map(|(i, r)| {
            format!(
                r##"Regex::new(r#"{regex}"#).unwrap(), // {idx}"##,
                regex = r,
                idx = i,
            )
        })
        .collect::<Vec<String>>()
        .join("\n");
    let re = template_match_re(which);
    re.replace(template, regexes_str).to_string()
}

fn add_transformations(
    template: &mut String,
    which: &str,
    transformations: &[Transformation],
    return_is_option: bool,
) -> String {
    let transformations_str = transformations
        .iter()
        .map(|t| {
            let transformation = format!(
                r##"
    // {regex}
    if let Some(caps) = {recognizer_var}[{i}].captures(word) {{
        let mut new_word = String::new();
        caps.expand(r#"{replacement}"#, &mut new_word);
        #[cfg(feature = "debug")]
        println!(r#"  word '{{}}' matched regex {{:?}} (idx {i}) and became {{}}"#, word, {recognizer_var}[{i}], new_word);
        return {return_val};
    }}
"##,
                regex = t.recognizer,
                recognizer_var = t.recognizer_var,
                i = t.idx,
                replacement = t.replacement,
                return_val = if return_is_option {
                    "Some(Cow::Owned(new_word))"
                } else {
                    "Cow::Owned(new_word)"
                },
            );

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

struct Inflections {
    plural: String,
    past: String,
    present_participle: String,
    past_participle: String,
}

fn insert_test_data(source_dir: &Path, verbs_rs: String) -> String {
    let mut conversions = conversions_from_verbs_general(source_dir);
    conversions = conversions_from_custom_data(conversions);

    let widths = (
        conversions.keys().map(String::len).max().unwrap(),
        conversions.values().map(|p| p.plural.len()).max().unwrap(),
        conversions.values().map(|p| p.past.len()).max().unwrap(),
        conversions
            .values()
            .map(|p| p.present_participle.len())
            .max()
            .unwrap(),
        conversions
            .values()
            .map(|p| p.past_participle.len())
            .max()
            .unwrap(),
    );

    let mut test_data = one_conversion_line(
        widths,
        "Singular",
        "Plural",
        "Past",
        "Present Participle",
        "Past Participle",
        true,
    );
    test_data.push_str(&one_conversion_line(
        widths,
        &"_".repeat(widths.0),
        &"_".repeat(widths.1),
        &"_".repeat(widths.2),
        &"_".repeat(widths.3),
        &"_".repeat(widths.4),
        true,
    ));

    let mut keys = conversions
        .keys()
        .map(|k| k.as_str())
        .collect::<Vec<&str>>();
    keys.sort_by_key(|k| k.to_lowercase());
    for singular in keys {
        let inflections = conversions.get(singular).unwrap();
        test_data.push_str(&one_conversion_line(
            widths,
            singular,
            &inflections.plural,
            &inflections.past,
            &inflections.present_participle,
            &inflections.past_participle,
            false,
        ));
    }

    verbs_rs.replace(
        "    // CONVERSIONS_TEST_DATA",
        &format!(
            r##"static CONVERSIONS_TEST_DATA: &str = r#"{}{}"#;"##,
            "\n", test_data
        ),
    )
}

fn conversions_from_verbs_general(source_dir: &Path) -> HashMap<String, Inflections> {
    let source = read_lines(source_dir, "verb_general.t");
    let mut iter = source.iter().skip_while(|l| !l.starts_with("__DATA__"));
    iter.next();
    conversions_from_test_data(HashMap::new(), &iter.collect::<Vec<&String>>())
}

fn conversions_from_custom_data(
    conversions: HashMap<String, Inflections>,
) -> HashMap<String, Inflections> {
    let custom_data = r#"
#   Singular      Plural        Preterite        Pres particple     Past participle
#   __________    ___________   _______          ______________     __________
    # \A(.*)bears$
    bears         bear          bore             bearing            borne
    forbears      forbear       forbore          forbearing         forborne
    # \A(.*)bids$
    bids          bid           bade             bidding            bidden
    forbids       forbid        forbade          forbidding         forbidden
    # \A(.*)buys$
    buys          buy           bought           buying             bought
    outbuys       outbuy        outbought        outbuying          outbought
    # \A(.*)casts$
    casts         cast          cast             casting            cast
    spellcasts    spellcast     spellcast        spellcasting       spellcast
    # \A(.*)clads$
    clads         clad          clad             cladding           clad
    ironclads     ironclad      ironclad         ironcladding       ironclad
    # \A(.*)cuts$
    cuts          cut           cut              cutting            cut
    crosscuts     crosscut      crosscut         crosscutting       crosscut
    # \A(.*)does$
    does          do            did              doing              done
    outdoes       outdo         outdid           outdoing           outdone
    # \A(.*)draws$
    draws         draw          drew             drawing            drawn
    outdraws      outdraw       outdrew          outdrawing         outdrawn
    # \A(.*)feeds$
    feeds         feed          fed              feeding            fed
    overfeeds     overfeed      overfed          overfeeding        overfed
    # \A(.*)freezes$
    freezes       freeze        froze            freezing           frozen
    flashfreezes  flashfreeze   flashfroze       flashfreezing      flashfrozen
    # \A(.*)grows$
    grows         grow          grew             growing            grown
    overgrows     overgrow      overgrew         overgrowing        overgrown
    # \A(.*)hangs$
    hangs         hang          hung             hanging            hung
    overhangs     overhang      overhung         overhanging        overhung
    # \A(.*)shears$
    shears        shear         sheared          shearing           shorn
    reshears      reshear       resheared        reshearing         reshorn
    # \A(.*)hears$
    hears         hear          heard            hearing            heard
    overhears     overhear      overheard        overhearing        overheard
    # \A(.*)hides$
    hides         hide          hid              hiding             hidden
    rehides       rehide        rehid            rehiding           rehidden
    # \A(.*)inputs$
    inputs        input         input            inputting          input
    reinputs      reinput       reinput          reinputting        reinput
    # \A(.*)knits$
    knits         knit          knitted          knitting           knitted
    reknits       reknit        reknitted        reknitting         reknitted
    # \A(.*)lends$
    lends         lend          lent             lending            lent
    relends       relend        relent           relending          relent
    # \A(.*)lets$
    lets          let           let              letting            let
    relets        relet         relet            reletting          relet
    # \A(.*)lights$
    lights        light         lit              lighting           lit
    relights      relight       relit            relighting         relit
    # \A(.*)makes$
    makes         make          made             making             made
    remakes       remake        remade           remaking           remade
    # \A(.*)mows$
    mows          mow           mowed            mowing             mown
    remows        remow         remowed          remowing           remown
    # \A(.*)pays$
    pays          pay           paid             paying             paid
    repays        repay         repaid           repaying           repaid
    # \A(.*)reads$
    reads         read          read             reading            read
    rereads       reread        reread           rereading          reread
    # \A(.*)says$
    says          say           said             saying             said
    resays        resay         resaid           resaying           resaid
    # \A(.*)sees$
    sees          see           saw              seeing             seen
    resees        resee         resaw            reseeing           reseen
    # \A(.*)sells$
    sells         sell          sold             selling            sold
    resells       resell        resold           reselling          resold
    # \A(.*)sends$
    sends         send          sent             sending            sent
    resends       resend        resent           resending          resent
    # \A(.*)sets$
    sets          set           set              setting            set
    resets        reset         reset            resetting          reset
    # \A(.*)sews$
    sews          sew           sewed            sewing             sewn
    resews        resew         resewed          resewing           resewn
    # \A(.*)shines$
    shines        shine         shone            shining            shone
    reshines      reshine       reshone          reshining          reshone
    # \A(.*)shoots$
    shoots        shoot         shot             shooting           shot
    reshoots      reshoot       reshot           reshooting         reshot
    # \A(.*)shuts$
    shuts         shut          shut             shutting           shut
    reshuts       reshut        reshut           reshutting         reshut
    # \A(.*)sleeps$
    sleeps        sleep         slept            sleeping           slept
    oversleeps    oversleep     overslept        oversleeping       overslept
    # \A(.*)slings$
    slings        sling         slung            slinging           slung
    reslings      resling       reslung          reslinging         reslung
    # \A(.*)spins$
    spins         spin          spun             spinning           spun
    respins       respin        respun           respinning         respun
    # \A(.*)splits$
    splits        split         split            splitting          split
    resplits      resplit       resplit          resplitting        resplit
    # \A(.*)spreads$
    spreads       spread        spread           spreading          spread
    respreads     respread      respread         respreading        respread
    # \A(.*)sticks$
    sticks        stick         stuck            sticking           stuck
    resticks      restick       restuck          resticking         restuck
    # \A(.*)strikes$
    strikes       strike        struck           striking           struck
    restrikes     restrike      restruck         restriking         restruck
    # \A(.*)strings$
    strings       string        strung           stringing          strung
    restrings     restring      restrung         restringing        restrung
    # \A(.*)takes$
    takes         take          took             taking             taken
    retakes       retake        retook           retaking           retaken
    # \A(.*)teaches$
    teaches       teach         taught           teaching           taught
    reteaches     reteach       retaught         reteaching         retaught
    # \A(.*)tells$
    tells         tell          told             telling            told
    retells       retell        retold           retelling          retold
    # \A(.*)thinks$
    thinks        think         thought          thinking           thought
    rethinks      rethink       rethought        rethinking         rethought
    # \A(.*)throws$
    throws        throw         threw            throwing           thrown
    rethrows      rethrow       rethrew          rethrowing         rethrown
    # \A(.*)tries$
    tries         try           tried            trying             tried
    retries       retry         retried          retrying           retried
    # \A(.*)weaves$
    weaves        weave         wove             weaving            woven
    reweaves      reweave       rewove           reweaving          rewoven
    # \A(.*)weds$
    weds          wed           wed              wedding            wed
    reweds        rewed         rewed            rewedding          rewed
    # \A(.*)wets$
    wets          wet           wet              wetting            wetted
    rewets        rewet         rewet            rewetting          rewetted
    # \A(.*)winds$
    winds         wind          wound            winding            wound
    rewinds       rewind        rewound          rewinding          rewound
    # \A(.*)writes$
    writes        write         wrote            writing            written
    rewrites      rewrite       rewrote          rewriting          rewritten
    # \A(.*)adds$
    adds          add           added            adding             added
    readds        readd         readded          readding           readded
    # \A(.*)alights$
    alights       alight        alit             alighting          alit
    realights     realight      realit           realighting        realit
    # \A(.*)allows$
    allows        allow         allowed          allowing           allowed
    reallows      reallow       reallowed        reallowing         reallowed
    # \A(.*)appears$
    appears       appear        appeared         appearing          appeared
    reappears     reappear      reappeared       reappearing        reappeared
    # \A(.*)believes$
    believes      believe       believed         believing          believed
    disbelieves   disbelieve    disbelieved      disbelieving       disbelieved
    # \A(.*)calls$
    calls         call          called           calling            called
    recalls       recall        recalled         recalling          recalled
    # \A(.*)clothes$
    clothes       clothe        clothed          clothing           clothed
    reclothes     reclothe      reclothed        reclothing         reclothed
    # \A(.*)considers$
    considers     consider      considered       considering        considered
    reconsiders   reconsider    reconsidered     reconsidering      reconsidered
    # \A(.*)creates$
    creates       create        created          creating           created
    recreates     recreate      recreated        recreating         recreated
    # \A(.*)fits$
    fits          fit           fitted           fitting            fitted
    refits        refit         refitted         refitting          refitted
    # \A(.*)includes$
    includes      include       included         including          included
    reincludes    reinclude     reincluded       reincluding        reincluded
    # \A(.*)melts$
    melts         melt          melted           melting            melted
    remelts       remelt        remelted         remelting          remelted
    # \A(.*)offers$
    offers        offer         offered          offering           offered
    reoffers      reoffer       reoffered        reoffering         reoffered
    # \A(.*)opens$
    opens         open          opened           opening            opened
    reopens       reopen        reopened         reopening          reopened
    # \A(.*)plays$
    plays         play          played           playing            played
    replays       replay        replayed         replaying          replayed
    # \A(.*)serves$
    serves        serve         served           serving            served
    reserves      reserve       reserved         reserving          reserved
    # \A(.*)smells$
    smells        smell         smelled          smelling           smelled
    resmells      resmell       resmelled        resmelling         resmelled
    # \A(.*)spells$
    spells        spell         spelled          spelling           spelled
    respells      respell       respelled        respelling         respelled
    # \A(.*)spills$
    spills        spill         spilled          spilling           spilled
    respills      respill       respilled        respilling         respilled
    # \A(.*)starts$
    starts        start         started          starting           started
    restarts      restart       restarted        restarting         restarted
    # \A(.*)turns$
    turns         turn          turned           turning            turned
    returns       return        returned         returning          returned
    # \A(.*)uses$
    uses          use           used             using              used
    reuses        reuse         reused           reusing            reused
    # \A(.*)works$
    works         work          worked           working            worked
    reworks       rework        reworked         reworking          reworked
    # \A(.+)n't$
    # This rule doesn't make much sense in LEI right now - see https://rt.cpan.org/Ticket/Display.html?id=140229
    # don't         don't         didn't           -                  -
    # \A(.+[aeiou])ys$
    # also covered above
    arrays        array         arrayed          arraying           arrayed
    rearrays      rearray       rearrayed        rearraying         rearrayed
    buoys         buoy          buoyed           buoying            buoyed
    # \A(.+[aiy])nxes$
    # already covered by sphinxes
    # \A(.+)ceps$
    # already covered by forceps
    # \A(.+[cs])hes$
    beaches       beach         beached          beaching           beached
    # \A(.+)oes$
    # already covered by oboes
    # \A(.+)ieus$
    # already covered by adieus
    # \A(.+)eaus$
    # already covered by chateaus
    # \A(.+)sses$
    # already covered by kisses and misses
    # \A(.+)trixes$
    # is this when you make a great first work in a trilogy and then screw it
    # up in the latter two parts, like "he really matrixed that trilogy up"?
    matrixes      matrix        matrixed         matrixing          matrixed
    # \A(.+)zzes$
    # already covered by buzzes
    # \A(.+)zes$
    # already covered by razes
    # \A(.+)ues$
    # already covered by fondues and glues
    # \A(.+)is$
    # already covered by graffitis, alibis, etc.
    # \A(.+)ees$
    # already covered by decrees
    # \A(.+)yes$
    # already covered by eyes
    # \A(.+[au])es$
    # the "ues" version is already covered earlier - https://rt.cpan.org/Ticket/Display.html?id=140231
    # LEI doesn't inflect this correctly - https://rt.cpan.org/Ticket/Display.html?id=140233
    # spaes         spae          spaed            spaeing            spaed
    # \A(.+[^b])is$
    # will never match - https://rt.cpan.org/Ticket/Display.html?id=140235
    # \A(.+)ies$
    # covered by cries and scries    
    # \A(.+)ys$
    # covered by toys
    # \A(.+[^e])es$
    # covered by japes    
    # \A(.+)ers$
    # covered by bothers
    # \A(.+[^s])s$
    # covered by lots of stuff
"#;
    conversions_from_test_data(conversions, &custom_data.lines().collect::<Vec<&str>>())
}

fn conversions_from_test_data<A: AsRef<str>>(
    mut conversions: HashMap<String, Inflections>,
    source: &[A],
) -> HashMap<String, Inflections> {
    for line in source
        .iter()
        .map(|s| s.as_ref())
        .filter(|l| !(l.contains('#') || l.is_empty()))
    {
        let inflections = line.split_whitespace().collect::<Vec<&str>>();
        conversions.insert(
            inflections[0].to_string(),
            Inflections {
                plural: inflections[1].to_string(),
                past: inflections[2].to_string(),
                present_participle: inflections[3].to_string(),
                past_participle: inflections[4].to_string(),
            },
        );
    }

    conversions
}

fn one_conversion_line(
    widths: (usize, usize, usize, usize, usize),
    singular: &str,
    plural: &str,
    past: &str,
    present_participle: &str,
    past_participle: &str,
    is_comment: bool,
) -> String {
    let lead = if is_comment { "# " } else { "  " };
    format!(
        "{lead}{s:w0$}    {pl:w1$}    {pa:w2$}    {prep:w3$}    {pasp:w4$}\n",
        lead = lead,
        s = singular,
        w0 = widths.0,
        pl = plural,
        w1 = widths.1,
        pa = past,
        w2 = widths.2,
        prep = present_participle,
        w3 = widths.3,
        pasp = past_participle,
        w4 = widths.4,
    )
}
