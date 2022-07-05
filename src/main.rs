extern crate pest;
#[macro_use]
extern crate pest_derive;

mod clac;

use clac::*;
use clap::Parser;
use rustyline::Editor;

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
}

fn main() {
    let args = Args::parse();

    if args.verbose {
        println!("{:?}", args);
    }

    let mut rl = Editor::<()>::new();
    rl.load_history("~/.λ_history");

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
            // TODO: proper handling:
            // - just exit on ctrl-c/d
            // - print error and return 1 else
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }

    rl.save_history("~/.λ_history").unwrap();

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
}
