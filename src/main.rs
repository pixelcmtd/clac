extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Clone, Copy, Debug, PartialEq)]
enum ΛSerializationType {
    Func = 0,
    Numeral,
    Boolean,
}

#[derive(Clone, Debug, PartialEq)]
enum ΛNode {
    Symbol(String),
    Lambda(String, Box<ΛNode>, ΛSerializationType),
    Application(Box<ΛNode>, Box<ΛNode>),
}

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
                    ΛSerializationType::Func,
                );
                for param in params {
                    func = ΛNode::Lambda(
                        String::from(param.as_str()),
                        Box::from(func),
                        ΛSerializationType::Func,
                    );
                }
                Some(func)
            }
        }
    }

    fn wrap_with_defs(&self, defs: Vec<(String, ΛNode)>) -> ΛNode {
        let mut tree = self.clone();
        for pair in defs {
            tree = ΛNode::Application(
                Box::from(ΛNode::Lambda(
                    pair.0,
                    Box::from(tree),
                    ΛSerializationType::Func,
                )),
                Box::from(pair.1),
            );
        }
        tree
    }

    fn to_string(&self) -> String {
        match self {
            ΛNode::Symbol(s) => s.clone(),
            ΛNode::Lambda(arg, body, ty) => match ty {
                ΛSerializationType::Func => String::from("λ") + arg + "." + &body.to_string(),
                ΛSerializationType::Boolean => {
                    String::from(if let ΛNode::Lambda(y, inner, _) = &**body {
                        if **inner == ΛNode::Symbol(y.clone()) {
                            "F"
                        } else if **inner == ΛNode::Symbol(arg.clone()) {
                            "T"
                        } else {
                            "BOOL"
                        }
                    } else {
                        "BOOL"
                    })
                }
                ΛSerializationType::Numeral => {
                    let mut c = *body.clone();
                    let mut n = 0;
                    while let ΛNode::Application(_, x) = c {
                        n += 1;
                        c = *x;
                    }
                    n.to_string()
                }
            },
            ΛNode::Application(func, arg) => {
                (match **func {
                    ΛNode::Symbol(_) | ΛNode::Application(_, _) => func.to_string(),
                    ΛNode::Lambda(_, _, _) => String::from("(") + &func.to_string() + ")",
                } + &(match **arg {
                    ΛNode::Symbol(_) => {
                        String::from(match **func {
                            ΛNode::Symbol(_) => " ",
                            _ => "",
                        }) + &arg.to_string()
                    }
                    ΛNode::Lambda(_, _, ty) => match ty {
                        ΛSerializationType::Func => String::from("(") + &arg.to_string() + ")",
                        _ => arg.to_string(),
                    },
                    _ => String::from("(") + &arg.to_string() + ")",
                }))
            }
        }
    }

    fn η_reduce(&self) -> ΛNode {
        match self {
            ΛNode::Symbol(_) => self.clone(),
            ΛNode::Lambda(a, b, t) => ΛNode::Lambda(
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
                *t,
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
        ΛNode::Lambda(param, body, ty) => {
            if *param == *name {
                node.clone()
            } else {
                ΛNode::Lambda(param.clone(), Box::from(β_reduce(body, name, arg)), *ty)
            }
        }
        // TODO: fix those names
        ΛNode::Application(func, param) => {
            let func = β_reduce(func, name, arg);
            let param = β_reduce(param, name, arg);
            match func {
                ΛNode::Lambda(fparam, body, _) => {
                    β_reduce(&β_reduce(&*body, &fparam, &param), name, arg)
                }
                _ => ΛNode::Application(Box::from(func), Box::from(param)),
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
            assert_eq!(tree.β_reduce(), βnf);
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let pairs = ΛParser::parse(Rule::expr, &buffer).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        println!("{:?}", pair);
        if let Some(raw_expr) = ΛNode::from_parse_tree(pair) {
            let expr = raw_expr.wrap_with_defs(vec![(
                String::from("T"),
                // TODO: make it possible to use our usual functions for this
                // (or just make everything better otherwise)
                ΛNode::Lambda(
                    String::from("x"),
                    Box::from(ΛNode::Lambda(
                        String::from("y"),
                        Box::from(ΛNode::Symbol(String::from("x"))),
                        ΛSerializationType::Boolean,
                    )),
                    ΛSerializationType::Boolean,
                ),
            )]);
            println!("expression: {:?}", expr.to_string());
            println!("β-normal-form: {:?}", expr.β_reduce().to_string());
            println!("η-normal-form: {:?}", expr.η_reduce().to_string());
            // TODO: combine
        }
    }

    Ok(())
}
