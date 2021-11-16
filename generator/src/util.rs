use once_cell::sync::Lazy;
use regex::Regex;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub(crate) fn read_lines(source_dir: &Path, typ: &str) -> Vec<String> {
    let mut path = source_dir.to_owned();
    path.push(typ);
    let file =
        File::open(&path).unwrap_or_else(|_| panic!("Could not open `{}`", path.to_string_lossy()));

    BufReader::new(file)
        .lines()
        .into_iter()
        .collect::<Result<Vec<String>, io::Error>>()
        .unwrap_or_else(|_| panic!("Error reading line from {}", path.to_string_lossy()))
}

pub(crate) fn maybe_is_generic_re_str(cap: Option<&str>) -> Option<&'static str> {
    match cap {
        Some(sym) => match sym {
            "*" => Some(".*"),
            "-" => Some(".+"),
            s => panic!("Unexpected generic symbol: {}", s),
        },
        None => None,
    }
}

pub(crate) static BLANK_LINE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A\s*$").expect("Could not parse blank line regex"));

pub(crate) static COMMENT_LINE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A\s*#").expect("Could not parse comment line regex"));

pub(crate) static CHARACTER_CLASS_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?P<class>\[.+?\])(?P<replacement>.*)")
        .expect("Could not parse character class regex")
});

// This turns a generic pattern into a full regex for matching that
// pattern. Some patterns are straightforward, like `(.*)staff` which turns
// into `${1}staves`. But some patterns in the nouns.lei file contain
// character classes. In that case, the character class piece needs to be
// included in the capturing part of the regex. So we turn something like
// `-[aiy]nx` into `(.+[aiy])nx`, with the replacement pattern being
// `${1}nges`.
pub(crate) fn replacement_and_regex_str_for_pattern_match(
    leading_re: &str,
    pattern: &str,
) -> (String, String) {
    let mut re = format!(r"(?i)\A({}", leading_re);

    let replacement = match CHARACTER_CLASS_RE.captures(pattern) {
        Some(caps) => {
            re.push_str(&format!(
                r"{}){}$",
                caps.name("class").unwrap().as_str(),
                caps.name("replacement").unwrap().as_str(),
            ));
            caps.name("replacement").unwrap().as_str().to_string()
        }
        None => {
            re.push_str(&format!(r"){}$", pattern));
            pattern.to_string()
        }
    };

    (replacement, re)
}

pub(crate) fn add_hashmap_from_pairs_list(
    template: String,
    which: &str,
    pairs: &[(&str, &str)],
) -> String {
    let pairs_str = pairs
        .iter()
        .map(|p| format!(r#"("{}", "{}"),"#, p.0, p.1))
        .collect::<Vec<String>>()
        .join("\n");
    let re_str = format!(
        r#"(?xsm)
            [\ ]+//[\ ]start[\ ]{which}\n
            [\ ]+[^\n]+,\n
            [\ ]+//[\ ]end[\ ]{which}"#,
        which = which,
    );

    let re = Regex::new(&re_str).unwrap_or_else(|_| panic!("Could not parse regex\n{}", re_str));
    re.replace(&template, pairs_str).to_string()
}

pub(crate) fn add_hashset_from_list(template: String, which: &str, list: &[&str]) -> String {
    let list_str = list
        .iter()
        .map(|w| format!(r#""{}","#, w,))
        .collect::<Vec<String>>()
        .join("\n");
    let re = template_match_re(which);
    re.replace(&template, list_str).to_string()
}

pub(crate) fn template_match_re(which: &str) -> Regex {
    let re_str = format!(
        r#"(?xsm)
            [\ ]+//[\ ]start[\ ]{which}\n
            .+?
            [\ ]+//[\ ]end[\ ]{which}"#,
        which = which,
    );
    Regex::new(&re_str).unwrap_or_else(|_| panic!("Could not parse regex\n{}", re_str))
}

pub(crate) fn hashmap_to_sorted_pairs<'a>(
    hm: HashMap<&'a str, &'a str>,
) -> Vec<(&'a str, &'a str)> {
    let mut pairs = hm.into_iter().collect::<Vec<(&str, &str)>>();
    sort_str_pairs(&mut pairs);
    pairs
}

pub(crate) fn sort_str_pairs(pairs: &mut Vec<(&str, &str)>) {
    let cmps = &[
        |a: &(&str, &str), b: &(&str, &str)| {
            a.0.to_lowercase().partial_cmp(&b.0.to_lowercase()).unwrap()
        },
        |a: &(&str, &str), b: &(&str, &str)| {
            a.1.to_lowercase().partial_cmp(&b.1.to_lowercase()).unwrap()
        },
        |a: &(&str, &str), b: &(&str, &str)| a.0.partial_cmp(b.0).unwrap(),
        |a: &(&str, &str), b: &(&str, &str)| a.1.partial_cmp(b.1).unwrap(),
    ];
    pairs.sort_by(|a, b| {
        for cmp in cmps {
            let o = cmp(a, b);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    });
}
