// Based on the generate_nouns and generate_verbs code in the
// Lingua-En-Inflexion Perl distribution
// (https://metacpan.org/dist/Lingua-EN-Inflexion).
mod nouns;
mod util;
mod verbs;

use clap::Parser;
use std::{
    fs,
    path::{Path, PathBuf},
    process,
    str::FromStr,
};

#[derive(Debug, Parser)]
#[clap(name = "en-inflexion-generator")]
#[clap(author = "Dave Rolsky <autarch@urth.org>")]
#[clap(about = "Generates code containing the noun and verb data for the en-inflexion crate")]
struct Args {
    #[clap(short, long)]
    #[clap(help = "The directory containing the word files.")]
    source_dir: String,
    #[clap(short, long)]
    #[clap(help = "The tag to download. Defaults to the latest release.")]
    target_dir: String,
}

fn main() {
    let args = Args::parse();
    let source_dir = PathBuf::from_str(&args.source_dir).unwrap_or_else(|_| {
        panic!(
            "Could not turn source dir `{}` into a PathBuf",
            args.source_dir,
        )
    });
    let mut target_dir = PathBuf::from_str(&args.target_dir).unwrap_or_else(|_| {
        panic!(
            "Could not turn target dir `{}` into a PathBuf",
            args.target_dir,
        )
    });
    target_dir.push("src");
    target_dir.push("ll");

    for f in [nouns::generate, verbs::generate] {
        let (mut content, rs_file) = f(source_dir.clone(), target_dir.clone());
        content = format!("// vim:set ro: -*- buffer-read-only:t -*-\n{}", content);
        fs::write(rs_file.clone(), content)
            .unwrap_or_else(|_| panic!("Could not turn write to `{}`", rs_file.to_string_lossy(),));
        run_precious(&rs_file);
    }
}

fn run_precious(file: &Path) {
    let mut c = process::Command::new("precious");
    c.arg("tidy");
    c.arg(file.to_path_buf());
    c.status()
        .unwrap_or_else(|_| panic!("Failed to run `precious tidy {}`", file.to_string_lossy(),));
}
