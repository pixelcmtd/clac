use clac::*;
use clap::Parser;
use permissions::*;
use rustyline::{config::Builder, error::ReadlineError, DefaultEditor, EditMode};
use shellexpand::tilde;
use std::{fs::canonicalize, fs::read_to_string, path::Path, path::PathBuf};

#[derive(Parser, Debug)]
#[clap(version, author, about)]
struct Args {
    // TODO: help = "..." for everything
    // TODO: come up with some more
    #[clap(value_parser)]
    files: Vec<String>,

    // TODO: think about making this action = clap::ArgAction::Count and int
    #[clap(short, long)]
    verbose: bool,

    #[clap(short = 'H', long, value_parser = validate_file, default_value_t = String::from("~/.λ_history"), value_name = "FILE")]
    // TODO: think a few hours about renaming this to just `history`
    // TODO: think about making this an Option
    history_file: String,

    #[clap(
        long,
        help = "Use Vi-like keybinds for the REPL (the default is Emacs/readline-like)"
    )]
    vi: bool,

    #[clap(long, help = "Don't include the standard library")]
    no_stdlib: bool,
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

fn ast_to_dot(ast: &ΛNode, types: bool, i: usize) -> (String, usize) {
    match ast {
        // TODO: types
        ΛNode::Σ(s, ty) => (
            if types {
                format!("n{} [label=\"{} ∈ {}\"]", i, s, ty.to_string())
            } else {
                format!("n{} [label=\"{}\"]", i, s)
            },
            i + 1,
        ),
        ΛNode::Λ(a, b, ty) => {
            let mut s = if types {
                format!("n{} [label=\"λ ∈ {}\"]\n", i, ty.to_string())
            } else {
                format!("n{} [label=\"λ\"]\n", i)
            };
            s += &format!("a{} [label=\"{}\"]\n", i, a);
            let (t, j) = ast_to_dot(b, types, i + 1);
            s += &t;
            s += &format!("\nn{} -> a{}", i, i);
            s += &format!("\nn{} -> n{}", i, i + 1);
            (s, j)
        }
        ΛNode::Α(f, x) => {
            let mut s = format!("n{} [shape=point]\n", i);
            let (t, j) = ast_to_dot(f, types, i + 1);
            let (u, k) = ast_to_dot(x, types, j);
            s += &t;
            s += "\n";
            s += &u;
            s += &format!("\nn{} -> n{}", i, i + 1);
            s += &format!("\nn{} -> n{}", i, j);
            (s, k)
        }
        ΛNode::Χ(_n, _b) => (String::from("// Χ nodes are a TODO"), i),
        ΛNode::Τ(_n, _t) => (String::from("// TODO: Τ nodes"), i),
    }
}

#[allow(unused_must_use)]
fn main() -> Result<(), ReadlineError> {
    let args = Args::parse();
    let mut ec = Result::<(), ReadlineError>::Ok(());

    // TODO: dont even set up the readline if we're not using a repl
    let mut rl = DefaultEditor::with_config(
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

    let mut calc = if args.no_stdlib {
        ΛCalculus::new()
    } else {
        ΛCalculus::with_stdlib(args.verbose)
    };

    if args.files.len() == 0 {
        // read
        // eval
        // print
        loop {
            match rl.readline("λ> ") {
                Ok(line) => {
                    // FIXME: weirdly doesnt seem to work...︎︎︎︎︎⸘
                    rl.add_history_entry(line.as_str());
                    // TODO: catch parser errors
                    match ΛCalculus::parse(&line, args.verbose) {
                        Ok(x) => {
                            for expr in x {
                                if args.verbose {
                                    // TODO: think about if these are the right things to list (as above)
                                    println!("digraph {{\n{}\n}}", ast_to_dot(&expr, true, 0).0);
                                    println!("expression:     {:?}", expr);
                                    println!("as string:      {}", expr.to_string());
                                    println!("normal-form:    {}", calc.eval(expr).to_string());
                                } else {
                                    println!("{}", calc.eval(expr).to_string());
                                }
                            }
                        }
                        Err(err) => eprintln!("{}", err),
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
    } else {
        for path in args.files {
            match read_to_string(&path) {
                Err(err) => panic!("couldn't read {}: {}", path, err),
                Ok(rawcontents) => {
                    match ΛCalculus::parse(&rawcontents, args.verbose) {
                        Ok(x) => {
                            for expr in x {
                                // TODO: think about not printing anything if we implement something like an io monad
                                if args.verbose {
                                    println!("expression:     {:?}", expr);
                                    println!("as string:      {}", expr.to_string());
                                    println!("normal-form:    {}", calc.eval(expr).to_string());
                                } else {
                                    println!("{}", calc.eval(expr).to_string());
                                }
                            }
                        }
                        Err(err) => eprintln!("{}", err),
                    }
                }
            };
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
