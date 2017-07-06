# FEZ - an fsharp to core erlang experiment

### What is it?

Fez is an early doors experiment in compiling [fsharp](http://fsharp.org) to
[BEAM](https://github.com/erlang/otp) compatible
[core erlang](https://www.it.uu.se/research/group/hipe/cerl).

The primary aim is to implement enough of the language to evaluate what how
well an ML type of language could become a practical language for writing
code to be run on the beam.

Should this experiment succeed then it may lead to a fuller and usable fsharp
backend.

See [thoughts.md](https://github.com/kjnilsson/fez/blob/HEAD/thoughts.md)
for some discussions on potential implementation approaches.


### Getting started

#### Requirements

* [.NET core](https://dotnet.github.io/)
* [erlang 19+](https://www.erlang.org/downloads)


```
./build.sh   [on Linux/Mac]
./build.cmd  [on Windows]

```

after this you can:


```
./fezc <file.fs> <file2.fs> -o out     [on Linux/Mac]
./fezc.cmd <file.fs> <file2.fs> -o out [on Windows]
```

to try to compile fsharp modules to the beam.

You can use the `--nobeam` parameter if you only want to compile as core erlang files.

To run the tests:

```
./run-tests.sh

```

### What works?

FSharp is a multi-paradigmatic language and `fez` is targeting a solely functional
language (core erlang). Given this impedence mismatch it is only reasonable to
expect `fez` to adequately cover the functional language constructs of FSharp.
That said there are aspects of the imperative and object oriented paradigms that
can be covered, to a certain extent.

The most up-to-date resource for what language construct are covered is to look
at [basics.fs](basics.fs) but it is fairly unstructured.

The lists below list feature that are implemented to one of the following
degrees:

* `complete`

    The feature is reasonably complete and unsupported APIs and constructs
    either generate appropriate compile-time or run-time errors. Any strange
    behaviour here needs to be reported!

* `partial`

    The feature is tried and proven viable but not yet fully implemented.

* `prototype`

    The feature is present to some limited degree. Likely to have unexpected
    or unusual constraints. Feature may not be kept.

* `unstarted`

    The feature has not been started or investigated for feasibilty.

#### Functional

* Modules [`complete`]

    An fsharp module is compiled as an erlang module. Nested modules are compiled
    as indiviual erlang modules with fully-qualified names.

* Functions [`complete`]

    Private functions are not included in the erlang `export` statement. Function
    application, currying, inner functions and lambdas are all supported.

* Operators [`partial`]

    Most of the common operators (`|>`, `>>`, `+`, `-`, `/`, `snd`, `fst`) have
    been implemented but not to completion. Please report omissions.

* Pattern matching / control flow. [`complete`]

    Pattern matching, including guards is supported as are `if then else`
    expressions.

* `list`/`seq` comprehensions [`partial`]

    List and Seq comprehensions are supported but haven't been verified for
    completeness.


* List.* [`complete`]

    List API is complete apart from any functions including `Array`.

* Seq.* [`partial`]
* Set.* [`partial`],
* Option.* [`partial`]
* Result.* [`complete`]
* Map.* [`partial`],
* String.* [`complete`]
* Array* [`unstarted`]

* Tuples [`complete`]
* Records [`complete`]
* Discriminated Unions [`complete`]
* Active Patterns [`partial`]

    Limited testing. Please report issues.

* `try/finally/with` [`partial`]

* Primitive types [`partial`]
    * Currently all integers are translated to erlang integers and all
      float types to erlang floats. Erlang integers are "big ints" hence things
      like overflows and bitshifting probably wont work as expected.

* `sprintf` / `printfn` [`partial`]

* `query` expressions [`prototype`]

    Only works with `seq`.

* `async` [`partial`]

    `Async.Start` and `Async.StartChild` will execute computation in a separate
    process. The rest is mostly erased as all IO is already non-blocking in
    erlang.

* Computation Expressions [`partial`]

    Limited testing.


#### Object programming

* Member methods on records and discriminated unions [`complete`]

    NB: type based method overload do not work.

* Static members on records and discriminated unions [`complete`]
* Extension methods [`complete`]
* Interface implementations on records and unions [`complete`]
* `use` expressions [`complete`]

* constructors [`partial`]

    A `.ctor` function is generated but currently returns `unit` rather than
    a data structure to hold field data etc.

#### Imperative

* Ref cells [`prototype`]

    No automatic GC of the backing process dictionary key.

* Fast integer for loop. [`partial`]
* While loops [`unstarted`]


#### Erlang interop

* FFI [`partial`]

    The `ModCall` attribute can be used to generate a function stub that delegates
    to the configured erlang function.

* ErlangTerms [`partial`]

    The `ErlangTerm` attribute can be used to decorate a discriminated union
    such that a more erlang compatible encoding will be generated instead of the
    standard DU representation.

        * Cases without fields will be lowercased and generated as atoms
        * Cases with fields will be generated as tuples.


### How can I help?

Try it! Read [thoughts.md](https://github.com/kjnilsson/fez/blob/HEAD/thoughts.md)
and raise issues with the smallest possible repro program bearing in mind that the current
status is little more than a proof of concept and soooooooo many fundamental parts
are not yet implemented.

Code is also welcome of course.
