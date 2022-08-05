use clac::*;
use clap::Parser;
use permissions::*;
use rustyline::{config::Builder, error::ReadlineError, EditMode, Editor};
use shellexpand::tilde;
use std::{fs::canonicalize, path::Path, path::PathBuf};

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    // TODO: come up with some more

    // TODO: think about making this action = clap::ArgAction::Count and int
    #[clap(short, long)]
    verbose: bool,

    #[clap(short = 'H', long, value_parser = validate_file, default_value_t = String::from("~/.λ_history"))]
    // TODO: think a few hours about renaming this to just `history`
    history_file: String,

    #[clap(long)]
    vi: bool,
    // TODO: --no-stdlib
}

// TODO: test this piece of crap
fn validate_file(s: &str) -> Result<String, String> {
    if s.ends_with("/") {
        return Err(String::from("is a directory"));
    }
    let e = tilde(s);
    let mut p = PathBuf::from(&*e);
    match canonicalize(&p) {
        Ok(c) => p = c,
        Err(_) => {}
    }
    if !p.is_absolute() {
        p = Path::new(".").join(p);
    }
    if match is_writable(&p) {
        Ok(b) => b,
        Err(_) => false,
    } || match is_creatable(&p) {
        Ok(b) => b,
        Err(_) => false,
    } {
        Ok(String::from(e))
    } else {
        Err(String::from("neither writable nor creatable"))
    }
}

#[allow(unused_must_use)]
fn main() -> Result<(), ReadlineError> {
    let args = Args::parse();
    let mut ec = Result::<(), ReadlineError>::Ok(());

    let mut rl = Editor::<()>::with_config(
        Builder::new()
            .edit_mode(if args.vi {
                EditMode::Vi
            } else {
                EditMode::Emacs
            })
            .build(),
    )?;

    if args.verbose {
        println!("{:?}", args);
    }

    if args.history_file != "none" {
        if args.verbose {
            match rl.load_history(&args.history_file) {
                Ok(()) => println!("Using history file: {}", args.history_file),
                Err(err) => println!("Can't load history: {}", err),
            }
        } else {
            rl.load_history(&args.history_file);
        }
    }

    let mut calc = ΛCalculus::with_stdlib(args.verbose);

    loop {
        match rl.readline("λ> ") {
            Ok(line) => {
                // TODO: think about not putting some things (maybe errors) in the history
                // FIXME: weirdly doesnt seem to work...︎︎︎︎︎⸘
                rl.add_history_entry(line.as_str());
                // TODO: catch parser errors
                match ΛCalculus::parse(&line, args.verbose) {
                    Ok(x) => {
                        for expr in x {
                            if args.verbose {
                                // TODO: think about if these are the right things to list (as above)
                                println!("expression:     {:?}", expr);
                                println!("as string:      {}", expr.to_string());
                                println!("normal-form:    {}", calc.eval(expr).to_string());
                            } else {
                                println!("{}", calc.eval(expr).to_string());
                            }
                        }
                    }
                    Err(err) => println!("{}", err),
                }
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                ec = Err(err);
                break;
            }
        }
    }

    if args.verbose {
        println!("vardefs:");
        for (name, expr) in calc.vardefs.clone() {
            println!("  {} ← {}", name, expr.to_string());
        }

        println!("");
        println!("typedefs:");
        for (name, ty) in calc.typedefs.clone() {
            println!("  {} ∈ {}", name, ty.to_string());
        }
    }

    if args.history_file != "none" {
        if args.verbose {
            println!("Saving history file: {}", args.history_file);
        } else {
            rl.save_history(&args.history_file).unwrap();
        }
    }

    ec
}
