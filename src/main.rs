use clac::*;
use clap::Parser;
use home::home_dir;
use rustyline::{config::Builder, error::ReadlineError, EditMode, Editor, Result};

#[derive(Parser, Debug)]
struct Args {
    // TODO: come up with some more
    #[clap(short, long)]
    verbose: bool,

    #[clap(short = 'H', long)]
    history_file: Option<String>,

    #[clap(long)]
    vi: bool,
}

#[allow(unused_must_use)]
fn main() -> Result<()> {
    let args = Args::parse();
    let mut ec = Result::<()>::Ok(());

    let hist = match &args.history_file {
        Some(h) => h.clone(),
        None => match home_dir() {
            Some(p) => p.display().to_string() + "/.λ_history",
            None => {
                println!("Can't get home dir, so we won't be able to save your history.");
                String::from("none")
            }
        },
    };

    let mut rl = Editor::<()>::with_config(
        Builder::new()
            .edit_mode(if args.vi {
                EditMode::Vi
            } else {
                EditMode::Emacs
            })
            .build(),
    );

    if args.verbose {
        println!("{:?}", args);
    }

    if hist != "none" {
        if args.verbose {
            match rl.load_history(&hist) {
                Ok(()) => println!("Using history file: {}", hist),
                Err(err) => println!("Can't load history: {}", err),
            }
        } else {
            rl.load_history(&hist);
        }
    }

    let mut calc = ΛCalculus::new();

    for expr in ΛCalculus::parse(&include_str!("stdlib.λ")) {
        if args.verbose {
            println!("");
            // TODO: think about if these are the right things to list
            println!("expression:     {:?}", expr);
            println!("as string:      {}", expr.to_string());
            println!("normal-form:    {}", calc.eval(expr).to_string());
        } else {
            calc.eval(expr);
        }
    }

    if args.verbose {
        println!("");
        println!("––– END STDLIB –––");
        println!("");
    }

    loop {
        match rl.readline("λ> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                for expr in ΛCalculus::parse(&line) {
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

    if hist != "none" {
        if args.verbose {
            println!("Saving history file: {}", hist);
        } else {
            rl.save_history(&hist).unwrap();
        }
    }

    ec
}
