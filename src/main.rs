extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Clone, Debug)]
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

    fn β_reduce<'a>(&'a self) -> Option<Box<ΛNode>> {
        β_reduce(self, &String::from(""), &ΛNode::Symbol(String::from("")))
    }
}

fn β_reduce<'a>(node: &'a ΛNode, name: &'a String, arg: &'a ΛNode) -> Option<Box<ΛNode>> {
    match node {
        ΛNode::Symbol(ref s) => Some(if *s == *name {
            Box::from(arg.clone())
        } else {
            Box::from(node.clone())
        }),
        ΛNode::Lambda(param, body) => {
            if *param == *name {
                // TODO: shadowing?!
                None
            } else {
                Some(Box::from(ΛNode::Lambda(
                    param.clone(),
                    Box::from(β_reduce(body, name, arg)?),
                )))
            }
        }
        ΛNode::Application(func, param) => {
            // for some reason i cant directly use this
            let skr = &β_reduce(
                &match &**func {
                    ΛNode::Lambda(_, b) => (**b).clone(),
                    _ => ΛNode::Symbol(String::from("")),
                },
                &match &**func {
                    ΛNode::Lambda(p, _) => p.clone(),
                    _ => String::new(),
                },
                param,
            )?;

            β_reduce(skr, name, arg)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_λs() {
        for s in [
            include_str!("../test/I.λ"),
            include_str!("../test/2.λ"),
            include_str!("../test/succ.λ"),
        ] {
            let _pairs = ΛParser::parse(Rule::func, s).unwrap_or_else(|e| panic!("{}", e));
        }
        // TODO: actually test
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let pairs = ΛParser::parse(Rule::expr, &buffer).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        println!("{:?}", pair);
        let expr = ΛNode::from_parse_tree(pair);
        println!("Expr: {:?}", expr);
        match expr {
            Some(x) => println!("ΒNF: {:?}", x.β_reduce()),
            None => {}
        }
    }

    Ok(())
}
