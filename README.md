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

See [thoughts.md](https://github.com/kjnilsson/fez/blob/HEAD/test/basics.fs)
for some discussions on potential implementation approaches.


### Getting started

Currently the scripts are only unix compatible but should be easy to translate.

```
dotnet restore src/fez/fez.fsproj
dotnet restore src/fez.core/fez.core.fsproj
dotnet restore test/test.fsproj

./run-tests.sh

```
or

```
dotnet build src/fez/fez/fsproj
```

after this you can:

```
./fezc <file.fs>
```

to try to compile fsharp modules to core erlang -> beam.


### How can I help?

Try it! Read [thoughts.md](https://github.com/kjnilsson/fez/blob/HEAD/test/basics.fs)
and raise issues with the smallest possible repro program bearing in mind that the current
status is little more than a proof of concept and soooooooo many fundamental parts
are not yet implemented.

Code is also welcome of course.



