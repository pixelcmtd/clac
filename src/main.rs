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
    Σ(String),
    Λ(String, Box<ΛNode>), //ΤNode
    Α(Box<ΛNode>, Box<ΛNode>),
    Χ(String, Box<ΛNode>),
    //Τ(String, Box<ΛType>),
}

// TODO: think about an any type
//enum ΤNode {
//    Π(String),
//    Σ(String),
//    Λ(Box<ΤNode>, Box<ΤNode>),
//    Υ(Box<ΤNode>, Box<ΤNode>),
//}

// TODO:
//impl PartialEq for ΛNode {
//    // TODO: α-conversion (or η-reduction, maybe β-reduction)
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
    fn λ(params: Vec<String>, body: ΛNode) -> ΛNode {
        let mut params = params.into_iter().rev();
        let mut func = ΛNode::Λ(params.next().unwrap(), Box::from(body));
        for param in params {
            func = ΛNode::Λ(String::from(param.as_str()), Box::from(func));
        }
        func
    }

    fn α(func: ΛNode, arg: ΛNode) -> ΛNode {
        ΛNode::Α(Box::from(func), Box::from(arg))
    }

    fn χ(name: String, body: ΛNode) -> ΛNode {
        ΛNode::Χ(name, Box::from(body))
    }

    fn from_parse_tree(tree: Pair<Rule>) -> Option<Self> {
        match tree.as_rule() {
            Rule::WHITESPACE
            | Rule::params
            | Rule::mparams
            | Rule::statement
            | Rule::statements
            | Rule::COMMENT
            | Rule::EOI => None,
            Rule::variable | Rule::mvariable => Some(ΛNode::Σ(String::from(tree.as_str()))),
            Rule::item | Rule::mitem | Rule::body | Rule::mbody => {
                ΛNode::from_parse_tree(tree.into_inner().next()?)
            }
            Rule::expr | Rule::mexpr => {
                let mut items: Vec<Pair<Rule>> = tree.into_inner().rev().collect();
                Some(if items.len() == 1 {
                    ΛNode::from_parse_tree(items.pop()?)?
                } else {
                    let mut expr = ΛNode::α(
                        ΛNode::from_parse_tree(items.pop()?)?,
                        ΛNode::from_parse_tree(items.pop()?)?,
                    );
                    for item in items {
                        expr = ΛNode::α(expr, ΛNode::from_parse_tree(item)?);
                    }
                    expr
                })
            }
            Rule::func | Rule::mfunc => {
                let mut inner = tree.into_inner().filter(|x| match x.as_rule() {
                    Rule::params | Rule::mparams | Rule::body | Rule::mbody => true,
                    _ => false,
                });
                let params = inner.next()?.into_inner();
                let body = inner.next()?;
                Some(ΛNode::λ(
                    params.map(|x| String::from(x.as_str())).collect(),
                    ΛNode::from_parse_tree(body)?,
                ))
            }
            Rule::vardef => {
                let mut inner = tree.into_inner();
                let name = inner.next()?.as_str();
                let body = ΛNode::from_parse_tree(inner.next()?)?;
                Some(ΛNode::χ(String::from(name), body))
            }
        }
    }

    fn contains(self, free_variable: &String) -> bool {
        match self {
            ΛNode::Σ(s) => *free_variable == s,
            ΛNode::Λ(arg, body) => arg != *free_variable && body.contains(free_variable),
            ΛNode::Α(func, arg) => func.contains(free_variable) || arg.contains(free_variable),
            ΛNode::Χ(name, body) => name == *free_variable || body.contains(free_variable),
        }
    }

    fn to_string(&self) -> String {
        match self {
            ΛNode::Σ(s) => s.clone(),
            ΛNode::Χ(name, expr) => name.clone() + " ← " + &expr.to_string(),
            ΛNode::Λ(arg, body) => String::from("λ") + arg + "." + &body.to_string(),
            ΛNode::Α(func, arg) => {
                (match **func {
                    ΛNode::Σ(_) | ΛNode::Α(_, _) => func.to_string(),
                    ΛNode::Λ(_, _) | ΛNode::Χ(_, _) => {
                        String::from("(") + &func.to_string() + ")"
                    }
                } + &(match **arg {
                    ΛNode::Σ(_) => {
                        String::from(match **func {
                            ΛNode::Σ(_) | ΛNode::Α(_, _) => " ",
                            _ => "",
                        }) + &arg.to_string()
                    }
                    ΛNode::Λ(_, _) | ΛNode::Α(_, _) | ΛNode::Χ(_, _) => {
                        String::from(" (") + &arg.to_string() + ")"
                    }
                }))
            }
        }
    }

    fn reduce(&self) -> ΛNode {
        reduce(self, &String::new(), &ΛNode::Σ(String::new()))
    }
}

fn reduce(node: &ΛNode, name: &String, arg: &ΛNode) -> ΛNode {
    match node {
        ΛNode::Σ(ref s) => if *s == *name { arg } else { node }.clone(),
        // TODO: rethink whether this is actually a good idea
        ΛNode::Χ(n, e) => ΛNode::Χ(n.clone(), Box::from(reduce(e, name, arg))),
        ΛNode::Λ(param, body) => match &**body {
            ΛNode::Α(func, prm) => {
                if match &**prm {
                    ΛNode::Σ(s) => *param == *s && !func.clone().contains(&s.clone()),
                    _ => false,
                } {
                    reduce(&*func, name, arg)
                } else if *param == *name {
                    node.clone()
                } else {
                    ΛNode::Λ(param.clone(), Box::from(reduce(body, name, arg)))
                }
            }
            _ => {
                if *param == *name {
                    node.clone()
                } else {
                    ΛNode::Λ(param.clone(), Box::from(reduce(body, name, arg)))
                }
            }
        },
        ΛNode::Α(func, param) => {
            let func = reduce(func, name, arg);
            let param = reduce(param, name, arg);
            match func {
                ΛNode::Λ(fparam, body) => reduce(&reduce(&*body, &fparam, &param), name, arg),
                _ => ΛNode::Α(Box::from(func), Box::from(param)),
            }
        }
    }
}

struct ΛCalculus {
    vardefs: HashMap<String, ΛNode>,
    //typedefs: HashMap<String, ΤNode>,
}

impl ΛCalculus {
    fn new() -> Self {
        ΛCalculus {
            vardefs: HashMap::new(),
            //typedefs: HashMap::new(),
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

    fn eval(&mut self, tree: ΛNode) -> ΛNode {
        let tree = tree.reduce();
        match tree.clone() {
            // TODO:
            ΛNode::Σ(_) | ΛNode::Λ(_, _) | ΛNode::Α(_, _) => {}
            // TODO: think about evaling the exprs
            ΛNode::Χ(name, expr) => {
                self.vardefs.insert(name, *expr);
            }
        };
        tree
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
            include_str!("../test/Ix.λ"),
        ] {
            let mut split = case.split(" → ");
            let tree = &ΛCalculus::parse(split.next().unwrap())[0];
            let normal_form = ΛCalculus::parse(split.next().unwrap())[0];
            assert_eq!(tree, ΛCalculus::parse(&tree.to_string())[0]);
            assert_eq!(tree.reduce(), normal_form);
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

    // TODO: variables

    for raw_expr in exprs {
        let expr = raw_expr;
        println!("{:?}", expr);
        println!("expression: {}", expr.to_string());
        println!("η-β-normal-form: {}", expr.reduce().to_string());
        // TODO: combine?
    }

    Ok(())
}
