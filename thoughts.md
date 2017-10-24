# Implementation approach discussion


## Records

Records in fsharp are (unless marked `private`) shared and accessible to any
module. In erlang sharing records is a bit of a no-no. At the very
least they should not form part of the public API.

When sharing records in erlang they are typically declared in an '.hrl' file.
Modules that need the record import this file which has the effect of the record
being re-declared inside this module. (As an attribute in the .core file) just
as if it had been declared in the module anyway.
The recommended way to share some state (such as a record) is to export an
opaque type. FSharp doesn't really have an equivalent of this.

When declaring a public record in fsharp we could simply limit it to the current
'project' (effectively making it internal). In this case we'd just need to
include the core erlang attribute declaration in every module that uses it.
If we wanted to make it sharable outside of the current project we could generate
an .hrl file for every module that declares a public record. This way erlang
code that wants to consume it can.


## Mutation

Mutation (<-) _could_ possibly be supported using the process dictionary. We'd
have to re-implement fsharp's scoping rules though to ensure a process
dictionary key isn't doesn't escape. I.e. generate "unique" key names that are
deleted at the end of a scope.

I think at a first cut we simply disallow mutation as it isn't as beneficial
(performance wise) in erlang as it is on .NET.

## Objects

Objects are tricky as fine-grained object programming is well supported in
fsharp but not supported in erlang. High-level object programming could
possibly be supported using processes and or behaviours.

Alternatively we could generate a data type + functions but like with generating
.hrl it could well be fraught with problems.

First cut - no objects.

# Interfaces

Behaviours are the closest to an interface erlang has. They operate at the
module level. This is the same problem as objects. We _could_ possibly generate
a new module exporting an opaque type and a function for each method.

## Behaviours

FSharp doesn't have module level interfaces (can a static class implement an
interface?). We _could_ implement something like the old school erlang
`behaviour_info/1` function.


## Discriminated Unions (IMPLEMENTED)

These could simply be translated into tagged tuples.

E.g:
```

type Test =
    | One
    | Two of int

```

could become a type spec of:

```
-type 'Test'() :: 'One' | {'Two', integer()}
```

DUs could also be used to define receive match statements when interacting
with other erlang modules.

`receive` matches are interesting as there they would need to have erlang
semantics, i.e. the receive would be _selective_. However we'd declare a total
fsharp type describing the subset of messages we are matching at a particular time.

Taking the `Test` DU above a receive may look something like in fsharp:

```

match receive<Test>() with
| One -> ...
| Two -> ...

```

As `receive` is generic we can use different DUs to describe the messages we
are matching on at a particular point. This might allow for an interesting
way to write FSMs where each state would be defined by a separate DU.

In erlang the messages would look like:

```

Pid ! 'One',
Pid ! {'Two', 42}

```



## MailboxProcessor

It's like erlang processes right? ;)

No it is not. MailboxProcessors have .NET semantics and are fully typed.
Erlang processes always use selective receive. MailboxProcessors are quite
unnecessary. See DU discussion above.


## System.*

System.* namespace pretty much means .NET. I don't think it makes sense
to try to implement any of System.*. Instead it is better to provide an
easy FFI to call erlang modules and provide typed responses.
