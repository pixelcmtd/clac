extern crate pest;
#[macro_use]
extern crate pest_derive;

mod clac;

use clac::*;
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

fn main() {
    // TODO: cli args
    let debug = true;

    let mut rl = Editor::<()>::new();
    rl.load_history("~/.λ_history");

    let mut calc = ΛCalculus::new();

    loop {
        match rl.readline("λ> ") {
            Ok(line) => {
                for expr in ΛCalculus::parse(&line) {
                    if debug {
                        println!("expression:     {:?}", expr);
                        println!("as string:      {}", expr.to_string());
                        println!("normal-form:    {}", calc.eval(expr).to_string());
                        println!("");
                        println!("");
                        println!("");
                    // TODO: combine?
                    } else {
                        println!("{}", calc.eval(expr).to_string());
                    }
                }
            }
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }

    rl.save_history("~/.λ_history").unwrap();

    if debug {
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
