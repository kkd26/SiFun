# SiFun
Interpreter for SiFun (Simple Functional) Language with three different type systems (supports Higher-Rank Polymorphism).

Currently supports Hindley–Milner type system (simply typed lambda calculus) and Hindley–Milner with Visible Type Application (similar to System F).

Under development is a Bidirectional Typechecking System which will support Higher-Rank Polymorphism without typed function and type application.

## Installation
You can clone this repository using:

```bash
git clone git@github.com:kkd26/SiFun.git
cd SiFun
opam install .
```

or use the official [opam](https://opam.ocaml.org/) release using:

```
opam install sifun
```

## Usage
There are two ways to interact with the interpreter:

1. SiFun REPL (Read–eval–print loop) - interactive console

    ```
    sifuni
    ```

2. SiFun Interpreter - run from file

    ```
    sifun <filename>
    ```

## SiFun Language syntax

The basic language supports (similar to Hindley–Milner type system):

- integers `1`, `2`, `3`, ...
- booleans `true`, `false`
- variables `x`, `y`, ...
- pairs `(1, true)`
- first and second `fst (1,3)`, `snd (4,5)`, ...
- functions `fn x => x`, `fn x => fn y => (x,y)`, ...
- application `(fn x => (1,x)) 2`, ...

The additional features are (similar to System F):

- typed functions `fn x : int => x`, ...
- type variables `a`, `b`, `c`, ...
- poly types `forall a. a->a`, `forall a b c. a->b->c`, ...
- big lambda `lam a. (int, fn x : a => x)`, `lam a b c. fn x : a => x`, ...
- type application `(lam a. fn x : a => x) {int}`, ...

Note that `;` separates blocks of code and `;;` sends EOF, so stops execution.

You can check [`./examples`](https://github.com/kkd26/SiFun/tree/main/examples) for more information about the syntax and semantics.
