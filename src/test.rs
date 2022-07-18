mod clac;
use clac::*;

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
