extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

// TODO: good equality
#[derive(Clone, Debug, PartialEq)]
enum ΛNode {
    Symbol(String),
    Lambda(String, Box<ΛNode>),
    Application(Box<ΛNode>, Box<ΛNode>),
}

impl ΛNode {
    fn from_parse_tree(tree: Pair<Rule>) -> Option<Self> {
        match tree.as_rule() {
            Rule::WHITESPACE | Rule::params => None,
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
        }
    }

    fn to_string(&self) -> String {
        match self {
            ΛNode::Symbol(s) => s.clone(),
            ΛNode::Lambda(arg, body) => String::from("λ") + arg + "." + &body.to_string(),
            ΛNode::Application(func, arg) => {
                (match **func {
                    ΛNode::Symbol(_) | ΛNode::Application(_, _) => func.to_string(),
                    ΛNode::Lambda(_, _) => String::from("(") + &func.to_string() + ")",
                } + &(match **arg {
                    ΛNode::Symbol(_) => {
                        String::from(match **func {
                            ΛNode::Application(_, _) | ΛNode::Symbol(_) => " ",
                            _ => "",
                        }) + &arg.to_string()
                    }
                    ΛNode::Lambda(_, _) | ΛNode::Application(_, _) => {
                        String::from("(") + &arg.to_string() + ")"
                    }
                }))
            }
        }
    }

    fn β_reduce(&self) -> Box<ΛNode> {
        β_reduce(self, &String::new(), &ΛNode::Symbol(String::new()))
    }
}

fn β_reduce(node: &ΛNode, name: &String, arg: &ΛNode) -> Box<ΛNode> {
    match node {
        ΛNode::Symbol(ref s) => Box::from(if *s == *name { arg } else { node }.clone()),
        ΛNode::Lambda(param, body) => Box::from(if *param == *name {
            node.clone()
        } else {
            ΛNode::Lambda(param.clone(), Box::from(β_reduce(body, name, arg)))
        }),
        // TODO: fix those names
        ΛNode::Application(func, param) => {
            let func = β_reduce(func, name, arg);
            let param = β_reduce(param, name, arg);
            match *func {
                ΛNode::Lambda(fparam, body) => {
                    β_reduce(&β_reduce(&*body, &fparam, &*param), name, arg)
                }
                _ => Box::from(ΛNode::Application(func, param)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hacky_parse(expr: &str) -> ΛNode {
        ΛNode::from_parse_tree(ΛParser::parse(Rule::expr, expr).unwrap().next().unwrap()).unwrap()
    }

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
            let tree = hacky_parse(split.next().unwrap());
            let βnf = hacky_parse(split.next().unwrap());
            assert_eq!(tree, hacky_parse(&tree.to_string()));
            assert_eq!(*tree.β_reduce(), βnf);
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let pairs = ΛParser::parse(Rule::expr, &buffer).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        println!("{:?}", pair);
        if let Some(expr) = ΛNode::from_parse_tree(pair) {
            println!("expression: {:?}", expr.to_string());
            println!("β-normal-form: {:?}", expr.β_reduce().to_string());
        }
    }

    Ok(())
}
