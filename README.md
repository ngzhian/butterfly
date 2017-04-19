# Butterfly

Butterfly is a mini functional language with a type and effect system.

## Type and effects system

The terms of butterfly are split into two kinds, effect-free pure expressions, and possibly effectful computations.

```
Expressions e       ::= <x> | true | false | () | fun x : A -> c | h | i
Effect interface i  ::= effect I : A
Handler h           ::= handler ocs
Operation cases ocs ::= #op x k -> c | ocs
Computation c       ::=   val e
                        | #op e (y k)
                        | handle c with e
```

The expressions contains, variables, the booleans, unit, function abstraction, handlers, and effect interface definitions.
A handler is a pattern-matcher for effects, it contains multiple operation cases to match on effects.
Computations contains expressions, an operation call, or a handle construct.

Consequently, the type system admits two kinds of types, the pure types for expressions and the dirty type for computations.

```
Pure types A, B    ::= bool | unit | A -> C_ | E^R | C_ => D_
Dirty types C_, D_ ::= A!delta
```

Pure types contain the `bool`, the `unit` type, functional abstraction from `A` to a computation `C_`, an effect type defined by an effect interface definition,
and handler types that take computations of incoming type `C_` to outgoing types `D_`.

A dirty type `C_` is a pure type `A` tagged with a `delta`, which is a set of effect types `E^R` that may possibly be called during evaluation.

TODO: add function application
TODO: algebraic effect, many ops in one effect

## Examples

```
-- boolean type
true
-- function abstraction
fun x : unit -> true
-- declare effect
effect choice : unit -> bool
-- operation call, has type (bool ! choice)
#choice ()

fun x : unit -> #choice ()
-- has type (unit -> (bool ! choice))

handle
    x ()
with
    | #choice () k -> k true

-- has type true
handler
| #choice k -> true
-- has type #choice => true
```
