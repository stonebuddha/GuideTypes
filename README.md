gtypes is a prototype implementation of a probabilistic programming language
that (i) features a *coroutine*-based paradigm for implementing generative
models and custom inference guides (e.g., proposals for importance sampling),
and (ii) uses a novel guide-type system to ensure that the distributions
specified by a model and its guide have the same *support*; as a consequence,
the model-guide pair is provably sound for probabilistic inference. Currently,
our implementation compiles models and guides to [Pyro](https://pyro.ai/) programs,
for either importance sampling or variational inference.

## INSTALLATION

1) Installing [opam](https://opam.ocaml.org/doc/Install.html):
``` bash
$ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
$ opam init -c 4.10.0
$ eval $(opam env)
```

2) Installing the required packages:
``` bash
$ opam install dune dune-build-info core menhir
```

3) Installing [pyro](https://github.com/pyro-ppl/pyro) and [greenlet](https://github.com/python-greenlet/greenlet):
``` bash
$ pip3 install pyro-ppl greenlet
```

4) Compiling gtypes:

Run `make` to compile the main binary. You may then
also run `make install` to install gtypes as an opam package.

## INPUT FILES

gtypes inputs are monadic typed functional programs. Primitive types include
`unit`, `bool`, `nat[n]` for natural numbers smaller than `n`, `nat` for natural
numbers, `ureal` for real numbers on the unit interval, `preal` for positive
real numbers, and `real` for real numbers. Basic types are primitive types,
arrows (for functions), products (for pairs), `Tb dist` for distributions on
inhabitants of basic type `Tb`, and `(Tp, [m1; m2; ...]) tensor` for tensors of
primitive type `Tp` and shape `[m1; m2; ...]`. For interaction with external code,
gtypes also allows external types, which are arbitrary identifiers.

gtypes programs separate *expressions* and *commands*. Expressions describe pure,
effect-less computations, i.e., deterministic computations without any randomness.
The expression language is essentially a simply-typed lambda calculus with
constructors for primitive types. Commands deal with *randomness*, introduced by
sampling commands, as well as *communication* among coroutines. gtypes adapts a message-passing mechanism; in a system, there are some named *channels*, each of
which connects two processes, and processes can send/receive a value on channels.

Below is a list of probability distributions supported by gtypes:

- `BER(prob)`: Bernoulli distributions

- `UNIF`: The uniform distribution on the unit interval

- `BETA(concentration1; concentration0)`: Beta distributions

- `GAMMA(concentration; rate)`: Gamma distributions

- `Normal(loc; scale)`: Normal distributions

- `CAT(prob1; prob2; ...; probn)`: Categorical distributions

- `BIN(total_count; prob)`: Binomial distributions

- `GEO(prob)`: Geometric distributions

- `POIS(rate)`: Poisson distributions

Commands are organized in a *monadic* syntax:

- `return E`: Evaluate the expression `E` and return its value.

- `x <- C1; C2`: Execute the command `C1`, bind the value to `x`, and continue
to the command `C2`.

- `Pname(E1; E2; ...)`: Call the procedure `Pname` with arguments `E1`, `E2`, ...

- `sample{ch}(E)`: Evaluate the expression `E` to get a distribution `d`, draw
a random sample `v` from `d`, and send `v` to the channel named `ch`.

- `observe{ch}(E)`: Evaluate the expression `E` to get a distribution `d`, receive
a value `v` from the channel named `ch`, and proceed *as if* `v` is sampled from
`d`.

- `if{ch} E then C1 else C2`: Evaluate the expression E to get a Boolean `b`,
pick `C1` or `C2` based on `b`, and send `b` to the channel named `ch`.

- `if{ch} . then C1 else C2`: Receive a Boolean `b` from the channel `ch`, and
proceed to `C1` or `C2` based on the value of `b`.

- `if E then C1 else C2`: A standard conditional command.

- `loop[n; Einit]( fn (x : Tb) -> C )`: Loop for `n` iterations with an accumulator
of basic type `Tb` and initial value `Einit`; in each iteration, bind the
accumulator from last iteration to `x` and execute the command `C`, whose return
value is the next accumulator.

- `iter[E; Einit]( fn t (x : Tb) -> C )`: Evaluate `E` to get a tensor, and
then loop through the tensor along its first dimension with an accumulator of
basic type `Tb` and initial value `Einit`; in each iteration, bind the sub-tensor
to `t` and the accumulator from last iteration to `x`, then execute the command
`C`.

A program consists of a sequence of top-level bindings, each of which takes one
of the forms below:

- `external ename : Tb`

    Declare an external binding of type `Tb`. This is usually used to allow
    operations on values of external types.

- `type Gname`

    Declare a *guide*-type identifer `Gname`. gtypes requires each channel in
    the system to be associate with such an identifier; when type-checking,
    gtypes infers a communication protocol for each channel and ensures that
    different parties that operates on the same channel have followed the same
    communication protocol.

- `proc Pname(x1 : Tb1; x2 : Tb2; ...) -> Tbr | ch1 : Gname1 | ch2 : Gname2 = C`

    Define a procedure `Pname`. Its formal parameters are `x1`, `x2`, ... with
    basic types `Tb1`, `Tb2`, ..., respectively. Its return type is `Tbr`. gtypes
    assumes each process is an invocation of a procedure, and the process can
    access at most two channels: it *consumes* `ch1` of guide-type identifier
    `Gname1`, and *provides* `ch2` of guide-type identifer `Gname2`. In case a
    procedure does not consume or provide a channel, you can write `.` for the
    `ch : Gname` part.

    PS: in some inference algorithms, it is useful to have a notion for *global*
    variables, which are not possible in gtypes. For example, variational
    inference defines guides as parameterized distributions and then optimizes
    the parameters. To aid the inference, gtypes allows
    `proc Pname[y1 : Tb1; y2 : Tb2; ...](...) ...`
    to specify those global parameters `y1`, `y2`, ...

Examples can be found in the folder `bench`.

## COMMAND LINE USAGE

For a description of the command-line usage options, run `gtypes help`.

gtypes has several modes that specify different stages of compilation to Pyro:

- `only-parse FILE`: Only parse the input program and check lexing/parsing errors

- `type-check FILE`: Type-check the input program, infer and check all the communication
protocols

- `normalize FILE`: Translate the input program to a-normal-form (this mode currently
does not have any output, so it is mainly used to test normalization)

- `compile-m FILE -o file.py`: Compile a model program to a Pyro program and write it
to `file.py`

- `compile-g -o file.py -m FILE1 -g FILE2`: Compile a guide program (for a specific
model program) to a Pyro program and write it to `file.py`

Example usage:
``` bash
$ gtypes compile-g -m bench/anglican/branching -g bench/anglican/branching-proposal -o guide.py
```
