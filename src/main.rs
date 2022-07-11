extern crate pest;
#[macro_use]
extern crate pest_derive;

mod clac;

use clac::*;
use clap::Parser;
use home::home_dir;
use rustyline::{error::ReadlineError, Editor, Result};

// TODO: more and better tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_λs() {
        for case in [
            include_str!("../test/I.λ"),
            include_str!("../test/2.λ"),
            include_str!("../test/succ.λ"),
            include_str!("../test/KII.λ"),
            include_str!("../test/1+1.λ"),
            include_str!("../test/Ix.λ"),
            include_str!("../test/2÷2.λ"),
        ] {
            let mut split = case.split(" → ");
            let tree = &ΛCalculus::parse(split.next().unwrap())[0];
            let normal_form = ΛCalculus::parse(split.next().unwrap())[0].clone();
            assert_eq!(
                tree.to_string(),
                ΛCalculus::parse(&tree.to_string())[0].clone().to_string()
            );
            assert_eq!(*tree, normal_form);
        }
    }
}

#[derive(Parser, Debug)]
struct Args {
    // TODO: come up with some more
    #[clap(short, long)]
    verbose: bool,

    #[clap(short = 'H', long)]
    history_file: Option<String>,
    // TODO: vi mode
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut ec = Result::<()>::Ok(());

    let hist = match &args.history_file {
        Some(h) => h.clone(),
        None => match home_dir() {
            Some(p) => p.display().to_string() + "/.λ_history",
            None => panic!("Can't get home dir"),
        },
    };

    let mut rl = Editor::<()>::new();

    if args.verbose {
        println!("{:?}", args);

        match rl.load_history(&hist) {
            Ok(()) => println!("Using history file: {}", hist),
            Err(err) => println!("Can't load history: {}", err),
        }
    } else {
        rl.load_history(&hist);
    }

    let mut calc = ΛCalculus::new();

    for expr in ΛCalculus::parse(&include_str!("stdlib.λ")) {
        if args.verbose {
            println!("");
            println!("expression:     {:?}", expr);
            println!("as string:      {}", expr.to_string());
            println!("normal-form:    {}", calc.eval(expr).to_string());
        // TODO: combine?
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
                        println!("expression:     {:?}", expr);
                        println!("as string:      {}", expr.to_string());
                        println!("normal-form:    {}", calc.eval(expr).to_string());
                    // TODO: combine?
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

        println!("Saving history file: {}", hist);
    }

    rl.save_history(&hist).unwrap();

    ec
}
