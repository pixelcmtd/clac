extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;
use std::collections::HashMap;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Clone, Debug, PartialEq)]
enum ΛNode {
    Symbol(String),
    Lambda(String, Box<ΛNode>),
    Application(Box<ΛNode>, Box<ΛNode>),
    Assignment(String, Box<ΛNode>),
    Definition(String, Box<ΛNode>),
}

// TODO:
//enum ΛDialect {
//    CompSci,
//    Maths,
//    ΜHaskell,
//}

// TODO:
//impl PartialEq for ΛNode {
//    // TODO: η reduction and α conversion
//    fn eq(&self, other: &Self) -> bool {
//        match self {
//            ΛNode::Symbol(s) => match other {
//                ΛNode::Symbol(o) => s == o,
//                _ => false,
//            },
//            ΛNode::Lambda(sa, sb) => match other {
//                ΛNode::Lambda(oa, ob) => sa == oa && sb == ob,
//                _ => false,
//            },
//            ΛNode::Application(sf, sa) => match other {
//                ΛNode::Application(of, oa) => sf == of && sa == oa,
//                _ => false,
//            },
//        }
//    }
//}

impl ΛNode {
    fn from_parse_tree(tree: Pair<Rule>) -> Option<Self> {
        match tree.as_rule() {
            Rule::WHITESPACE | Rule::params | Rule::statement => None,
            Rule::variable => Some(ΛNode::Symbol(String::from(tree.as_str()))),
            Rule::item | Rule::body | Rule::pexpr => {
                ΛNode::from_parse_tree(tree.into_inner().next()?)
            }
            Rule::expr => {
                let mut items: Vec<Pair<Rule>> = tree.into_inner().rev().collect();
                Some(if items.len() == 1 {
                    ΛNode::from_parse_tree(items.pop()?)?
                } else {
                    let mut expr = ΛNode::Application(
                        Box::from(ΛNode::from_parse_tree(items.pop()?)?),
                        Box::from(ΛNode::from_parse_tree(items.pop()?)?),
                    );
                    for item in items {
                        expr = ΛNode::Application(
                            Box::from(expr),
                            Box::from(ΛNode::from_parse_tree(item)?),
                        );
                    }
                    expr
                })
            }
            Rule::func => {
                let mut inner = tree.into_inner().filter(|x| match x.as_rule() {
                    Rule::params | Rule::body => true,
                    _ => false,
                });
                let mut params = inner.next()?.into_inner().rev();
                let body = inner.next()?;
                let mut func = ΛNode::Lambda(
                    String::from(params.next()?.as_str()),
                    Box::from(ΛNode::from_parse_tree(body)?),
                );
                for param in params {
                    func = ΛNode::Lambda(String::from(param.as_str()), Box::from(func));
                }
                Some(func)
            }
            Rule::definition => {
                let mut inner = tree.into_inner();
                let name = inner.next()?.as_str();
                let body = ΛNode::from_parse_tree(inner.next()?)?;
                Some(ΛNode::Definition(String::from(name), Box::from(body)))
            }
            Rule::assignment => {
                let mut inner = tree.into_inner();
                let name = inner.next()?.as_str();
                let body = ΛNode::from_parse_tree(inner.next()?)?;
                Some(ΛNode::Assignment(String::from(name), Box::from(body)))
            }
        }
    }

    //fn wrap_with_defs(&self, defs: Vec<(String, ΛNode)>) -> ΛNode {
    //    let mut tree = self.clone();
    //    for pair in defs {
    //        tree = ΛNode::Application(
    //            Box::from(ΛNode::Lambda(pair.0, Box::from(tree))),
    //            Box::from(pair.1),
    //        );
    //    }
    //    tree
    //}

    fn to_string(&self) -> String {
        match self {
            ΛNode::Symbol(s) => s.clone(),
            ΛNode::Definition(name, expr) => name.clone() + " ← " + &expr.to_string(),
            ΛNode::Assignment(name, expr) => name.clone() + " ≔ " + &expr.to_string(),
            ΛNode::Lambda(arg, body) => String::from("λ") + arg + "." + &body.to_string(),
            ΛNode::Application(func, arg) => {
                (match **func {
                    ΛNode::Symbol(_) | ΛNode::Application(_, _) => func.to_string(),
                    ΛNode::Lambda(_, _) | ΛNode::Definition(_, _) | ΛNode::Assignment(_, _) => {
                        String::from("(") + &func.to_string() + ")"
                    }
                } + &(match **arg {
                    ΛNode::Symbol(_) => {
                        String::from(match **func {
                            ΛNode::Symbol(_) => " ",
                            _ => "",
                        }) + &arg.to_string()
                    }
                    ΛNode::Lambda(_, _)
                    | ΛNode::Application(_, _)
                    | ΛNode::Definition(_, _)
                    | ΛNode::Assignment(_, _) => String::from("(") + &arg.to_string() + ")",
                }))
            }
        }
    }

    fn η_reduce(&self) -> ΛNode {
        match self {
            ΛNode::Symbol(_) => self.clone(),
            // TODO: rethink whether this is actually a good idea
            ΛNode::Definition(n, e) => ΛNode::Definition(n.clone(), Box::from(e.η_reduce())),
            ΛNode::Assignment(n, e) => ΛNode::Assignment(n.clone(), Box::from(e.η_reduce())),
            ΛNode::Lambda(a, b) => ΛNode::Lambda(
                a.clone(),
                Box::from(
                    (match &**b {
                        ΛNode::Application(f, p) => {
                            if match &**p {
                                ΛNode::Symbol(s) => *a == *s,
                                _ => false,
                            } {
                                &f
                            } else {
                                b
                            }
                        }
                        _ => b,
                    })
                    .η_reduce(),
                ),
            ),
            ΛNode::Application(f, a) => {
                ΛNode::Application(Box::from(f.η_reduce()), Box::from(a.η_reduce()))
            }
        }
    }

    fn β_reduce(&self) -> ΛNode {
        β_reduce(self, &String::new(), &ΛNode::Symbol(String::new()))
    }
}

fn β_reduce(node: &ΛNode, name: &String, arg: &ΛNode) -> ΛNode {
    match node {
        ΛNode::Symbol(ref s) => if *s == *name { arg } else { node }.clone(),
        // TODO: rethink whether this is actually a good idea
        ΛNode::Definition(n, e) => {
            ΛNode::Definition(n.clone(), Box::from(β_reduce(e, name, arg)))
        }
        ΛNode::Assignment(n, e) => {
            ΛNode::Assignment(n.clone(), Box::from(β_reduce(e, name, arg)))
        }
        ΛNode::Lambda(param, body) => {
            if *param == *name {
                node.clone()
            } else {
                ΛNode::Lambda(param.clone(), Box::from(β_reduce(body, name, arg)))
            }
        }
        ΛNode::Application(func, param) => {
            let func = β_reduce(func, name, arg);
            let param = β_reduce(param, name, arg);
            match func {
                ΛNode::Lambda(fparam, body) => {
                    β_reduce(&β_reduce(&*body, &fparam, &param), name, arg)
                }
                _ => ΛNode::Application(Box::from(func), Box::from(param)),
            }
        }
    }
}

struct ΛCalculus {
    definitions: HashMap<String, ΛNode>,
    assignments: HashMap<String, ΛNode>,
}

impl ΛCalculus {
    fn new() -> Self {
        ΛCalculus {
            definitions: HashMap::new(),
            assignments: HashMap::new(),
        }
    }

    fn parse(statements: &str) -> Vec<ΛNode> {
        ΛParser::parse(Rule::statement, statements)
            .unwrap_or_else(|e| panic!("{}", e))
            .map(|p| ΛNode::from_parse_tree(p))
            .filter_map(Some)
            .map(|x| x.unwrap())
            .collect()
    }
}

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
        ] {
            let mut split = case.split(" → ");
            let tree = &ΛCalculus::parse(split.next().unwrap())[0];
            let βnf = ΛCalculus::parse(split.next().unwrap())[0];
            assert_eq!(tree, ΛCalculus::parse(&tree.to_string())[0]);
            assert_eq!(tree.β_reduce(), βnf);
        }
    }
}

fn main() -> io::Result<()> {
    // TODO: cli args
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let exprs = ΛCalculus::parse(&buffer);
    let calc = ΛCalculus::new();

    // TODO: print variable content when just the variable is given

    for raw_expr in exprs {
        let expr = raw_expr;
        println!("{:?}", expr);
        println!("expression: {}", expr.to_string());
        println!("β-normal-form: {}", expr.β_reduce().to_string());
        println!("η-normal-form: {}", expr.η_reduce().to_string());
        // TODO: combine?
    }

    Ok(())
}
