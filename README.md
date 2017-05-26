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

Currently the scripts are only unix compatible but should be easy to translate
to windows if you're such inclined.

#### Requirements

* [.NET core](https://dotnet.github.io/)
* erlang 19+


```
./build.sh

```

after this you can:


```
./fezc <file.fs>
```

to try to compile fsharp modules to core erlang -> beam.


To run the tests:

```
./run-tests.sh

```


### How can I help?

Try it! Read [thoughts.md](https://github.com/kjnilsson/fez/blob/HEAD/throughts.md)
and raise issues with the smallest possible repro program bearing in mind that the current
status is little more than a proof of concept and soooooooo many fundamental parts
are not yet implemented.

Code is also welcome of course.
