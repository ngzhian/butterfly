# Butterfly

Butterfly is a toy functional language with a type and effect system.

It doesn't do any evaluation (yet), it can only tell you the type of an expression.

## Type and effects system

The terms of butterfly are split into two kinds, effect-free pure expressions, and possibly effectful computations.

```
Expressions e       ::= <x> | true | false | () | fun x : A -> c | h | i
Effect interface i  ::= effect I : A
Handler h           ::= handler | val x : A -> c [ocs]
Operation cases ocs ::= #op x k -> c | ocs
Computation c       ::=   val e
                        | #op e
                        | handle c with e
                        | e_1 e_2
```

The expressions contains, variables, the booleans, unit, function abstraction, handlers, and effect interface definitions.
A handler is a pattern-matcher for effects, it contains multiple operation cases to match on effects.
Computations contains expressions, an operation call, handle construct, function application.

Consequently, the type system admits two kinds of types, the pure types for expressions and the dirty type for computations.

```
Pure types A, B    ::= bool | unit | A -> C_ | E^R | C_ => D_
Dirty types C_, D_ ::= A!delta
```

Pure types contain the `bool`, the `unit` type, functional abstraction from `A` to a computation `C_`, an effect type defined by an effect interface definition,
and handler types that take computations of incoming type `C_` to outgoing types `D_`.

A dirty type `C_` is a pure type `A` tagged with a `delta`, which is a set of effect types `E^R` that may possibly be called during evaluation.

TODO: algebraic effect, many ops in one effect

## Examples

(This project is written in OCaml, and requires menhir, so make sure that is installed.)

First, build the project:

```
make
```

This should build a executable called `main.native`, run it on a example file:

```
./main.native test.bfly
```

This will output the terms in the file and also their inferred types.
