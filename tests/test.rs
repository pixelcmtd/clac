use clac::*;

// TODO: more and better tests
#[test]
fn test_λs() {
    for case in [
        "λa.a → λa.a",
        "λn f a.f(n f a) → λn f a.f(n f a)",
        "(λa b.a)(λa.a)(λb.b) → λa.a",
        "(λ m n f x . m f (n f x)) 1 1 → λf x.f(f x)",
        "(λx.(λa.a)x)x → x",
        "(λn m f x.m (λn f x.n (λg h.h (g f)) (λa.a) (λb.x)) m (λx.λb.λa.a) (λa.λb.a) (f (x x (m (λn f x.n (λg h.h (g f)) (λa.a) (λb.x)) m) x f m)) x)(λf x.f(f x))(λf x.f(f x)) → λf x.f x",
    ] {
        let mut split = case.split(" → ");
        let tree = &ΛCalculus::parse(split.next().unwrap(), false).unwrap()[0];
        let normal_form = ΛCalculus::parse(split.next().unwrap(), false).unwrap()[0].clone();
        assert_eq!(tree.to_string(), ΛCalculus::parse(&tree.to_string(), false).unwrap()[0].clone().to_string());
        assert_eq!(*tree, normal_form);
    }
}
