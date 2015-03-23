# Propsitioinal Logic Assistant

> Building Axioms, Applying MP rules and theorems/axioms. User Interface: REPL Looping

## Spec
Comparing to a Coq-like PA, this has a much simple syntax. To simplify the problem, the "Proof" principles, such as MP rule, is built-in logic.

A basic example:

    > Axiom L1(p, q): p -> (q -> p)
    > Axiom L2(p, q, r): (p -> (q -> r)) -> ((p -> r) -> (p -> r))
    > Axiom L3(p, q): (!p -> !q) -> (q -> p)
    > Check L1
    L1(p, q): p -> (q -> p)
    > Theorem id_rule(p): |- p -> p
    id_rule > L1 p, (p -> p)
    S0: p -> ((p -> p) -> p)
    id_rule > L2 S0
    S1: (p -> ((p -> p) -> p)) -> ((p -> (p -> p)) -> (p -> p))
    id_rule > mp S0, S1
    S2: (p -> (p -> p)) -> (p -> p)
    id_rule > L1 p p
    S3: p -> (p -> p)
    id_rule > mp S2, S3
    S4: p -> p
    id_rule > qed S4
    Theorem proved, id_rule: |- p -> p

## Internals
A much simple parser is required, which could be built on top of a compatible tokenizer as the non-existing PA.

The axiom/theorem is intefaced like a function, but structured as a tree-like construct. When "Called", the "manager" will bind the name with the axiom/theorem in current environment and print out a instantiated if needed.

`L1`, `L2`, `L3` can be customized, but we may set it as default to bootstrap. `MP` is a hard-wired function, which receives two parameters, indicating the "assumption" and the related "proposition".

Error would happen very often, so detector should be built in early.

