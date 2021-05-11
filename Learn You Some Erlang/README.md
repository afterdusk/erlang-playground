# Learn You Some Erlang

This set of notes was prepared with the version of LYSE written for Erlang version R13B+

# Contents

# Introduction

Features

- Dynamically, Strongly Typed
- Functional
    - Referential Transparency (except when it makes sense to break it, like date/time functions)
    - Declarative (concise, easier to read and maintain)
- Concurrent
    - Actor Model (message passing)
    - Thousands of concurrent processes
    - Supervision trees
- High Availability
    - Hot Reload (built for systems that need to be always available)
    - Error Recovery (let it crash philosophy)
- Development Environment
    - Bytecode Compilation (compile once, run anywhere on VM)
    - Development Tools (compiler, debugger, profiler, test framework)
    - Open Telecom Platform (web server, parser generator, key-value store)

Yes, Erlang has lightweight processes, but not everything can be solved by spamming processes. Erlang is also typically bad at things like image/signal processing (too much **number crunching** and **data moving**) and operating system drivers (too **low level**). Erlang is great for server software though.

# Starting Out

> The Erlang shell has a built-in line editor based on a subset of Emacs

Shortcuts

- `Ctrl-A` Bring cursor to beginning of line
- `Ctrl-E` Bring cursor to end of line
- `Ctrl-G` User switch command

Common Shell Functions

- `help().` help
- `q().` quit
- `f().` erase all variables
- `f(Variable).` erase variable
- `cd(Path).` change the directory (by default, shell only looks for files in the directory it was started and in the standard library)
- `c()`
- `c(Module)`

Use `erl -man` to get information about modules

# Starting Out (for real)

## Numbers

Operators

- `/` floating-point division
- `div` integer division
- `rem` modulo operator

To express integers in non-base 10, use `Base#Value` syntax.

```erlang
1> 2#101010.
42
2> 16#AE.
174
```

`=` will bind the variable on the LHS to the value on the RHS if it is unbound, else it acts as comparison (pattern matching)

Variables must start with capital letters or `_` (to represent values you do not intend to use but want to name)

`_`, if used on its own, does not store any value and cannot be bound

## Atoms

Starts with lowercase.

Enclose in single quotes if atom does not begin with a lowercase letter or if it contains characters other than alphanumeric characters, underscore or @.

Some atoms are reserved to be function names, operators and expressions etc. and cannot be used.

Atoms are referred to in an atom table (4 bytes/atom in 32-bit system, 8 bytes/atom in 64-bit system). The table is not garbage collected and so might tip the system over from memory usage or from being full.

Atoms should not be dynamically generated and only be used by the developer

## Booleans

`true` and `false` are atoms and do not have corresponding integer values

Operators

- `and`
- `or`
- `xor`
- `not`

`and` and `or` will always evaluate both sides of the operator; use `andalso` and `orelse` to short-circuit

## Comparisons

Operators

- `=:=` equality in the manner of pattern matching (cannot equate integer value to corresponding float)
- `=/=` inequality in the manner of pattern matching
- `==` arithmetic equality
- `/=` arithmetic inequality
- `=<` <= in Erlang

Comparison between different types (e.g. number and atom) does not yield an error

`number < atom < reference < fun < port < pid < tuple < list < bit string`

## Tuples

**tagged tuple:** tuple which contains an atom with one element following it (e.g. `{celsius, 23.213}`)

## Lists

Implemented as linked lists

Can contain multiple data types

```erlang
1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3,{numbers,[4,5,6]},5.34,atom]
```

Operations

- `++` right associative concatenation
- `--` right associative dis-concatenation
- `hd/1`
- `tl/1`
- `|` cons (constructor)

Built-in functions (BIFs) are usually functions that are defined in the implementation language (C) for speed reasons or because they cannot be implemented in pure Erlang

**improper list:** lists in the form of `[1 | 2]`. Will work in pattern matching but not in standard list functions

**proper list:** list that ends with an empty list in their last cell. Lists in the form of `[1 | [2]]` are valid as `[2]` is automatically formed in the proper manner (which is `[2|[]]`).

List comprehensions

- **generator expression**: `Pattern <- List`, can have more than one. Also serves as a filter when coupled with pattern matching (failed matches are ignored)
- `<-` same as `=` but does not throw exception

```erlang
1> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
[2,4,6,8,10]
```

While it may appear like Erlang's data structures are relatively limited, lists and tuples are usually enough to build other complex data structures (e.g. a binary tree node can be represented as `{node, Value, Left, Right}`)

## Strings

- Implemented as lists
- Will be printed as numbers only when at least one element cannot be represented as letter

Erlang does not have built-in string types due to its origins in telecom usage. Now, VM natively supports Unicode strings. Strings can also be stored as binary structures, making them light and fast

## Bits

Binary abstractions were necessary for the telecom applications Erlang was applied to

```erlang
1> Color = 16#F09A29.
15768105
```

Base 16 declaration

```erlang
2> Pixel = <<Color:24>>.
<<240,154,41>>
```

Binary declaration (24 bits of space)

Is the default segment 8 bits (1 byte)?

```erlang
3> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```

12-byte binary declaration

```erlang
4> <<Pix1,Pix2,Pix3,Pix4>> = Pixels.
** exception error: no match of right hand side value <<213,45,132,64,76,32,76,
0,0,234,32,15>>
```

Pattern matching 4 variables to 12 bytes. Error as Erlang assumes a variable represents a segment.

```erlang
5> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```

Pattern matching 4 variables to 12 bytes (explicit 24 bits per variable, or 3 segments per variable)

```erlang
6> <<R:8, G:8, B:8>> = <<Pix1:24>>.
<<213,45,132>>
```

Pattern matching 3 variables/segments to 24 bits (explicit 8 bits per variable)

```erlang
7> <<R:8, Rest/binary>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```

Pattern matching with unknown length

Binary segment syntax

- `Value`
- `Value:Size`
- `Value/TypeSpecifierList`
- `Value:Size/TypeSpecifierList`

where

- `Size` bits or bytes
- `TypeSpecifierList`, comprising the following attributes separated by `-`
    - Type `integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32`
        - `bytes` is `binary` and `bits` is `bitstring`
        - Default `integer`
    - Signedness `signed | unsigned`
        - Only matters for matching when the type is `integer`
        - Default `unsigned`
    - Endianess `big | little | native`
        - Only matters when type is `integer`, `utf16`, `utf32` or `float`
        - `native` endianess chosen at run-time by CPU
        - Default `big`
    - Unit `unit:Integer`
        - Size of each segment, in bits
        - Allowed range 1..256 (set by default to 1 for integers, floats and bits and to 8 for binary)
        - `utf8`, `utf16` and `utf32` require no units to be defined
        - Size x Unit = number of bits the ~~segment~~ variable? will take, must be evenly divisible of 8

```erlang
10> <<X1/unsigned>> =  <<-44>>.
<<"Ô">>
11> X1.
212
12> <<X2/signed>> =  <<-44>>. 
<<"Ô">>
13> X2.
-44
14> <<X2/integer-signed-little>> =  <<-44>>.
<<"Ô">>
15> X2.
-44
16> <<N:8/unit:1>> = <<72>>.
<<"H">>
17> N.
72
18> <<N/integer>> = <<72>>.
<<"H">>
19> <<Y:4/little-unit:8>> = <<72,0,0,0>>.     
<<72,0,0,0>>
20> Y.
72
```

Operators

- `bsl` bit shift left
- `bsr` bit shift right
- `band` bit and
- `bor` bit or
- `bxor` bit xor
- `bnot` bit not

Bit syntax and notation allows for easy parsing and pattern matching of binary data

Erlang is slow compared to C/C++, despite the easy-to-work with binary syntax. Software primarily performing number crunching (e.g. video conversion) should not be implemented in Erlang

Binary comprehensions

- Compared to list comprehensions,
    - `<=` instead of `<-` - use a binary stream as a generator
    - `<<>>` instead of `[]` - using binaries instead of lists
- Example: `RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= <<213,45,132,64,76,32,76,0,0,234,32,15>> ].`
- Converse example: `<< <<R:8, G:8, B:8>> || {R,G,B} <- RGB >>.`

Elements of resulting binary require a clearly defined size if generator returns binaries. 

```erlang
1> << <<Bin>> || Bin <- [<<3,7,5,4,7>>] >>.
** exception error: bad argument
2> << <<Bin/binary>> || Bin <- [<<3,7,5,4,7>>] >>. 
<<3,7,5,4,7>>
3> << <<(X+1)/integer>> || <<X>> <= <<3,7,5,4,7>> >>.
<<4,8,6,5,8>>
```

## Bit Strings

Normal lists are linked lists - bit strings are more like C arrays

Syntax: `<<"this is a bit string!">>`

Disadvantage of bit strings over strings is the loss in simplicity of pattern matching and manipulation. Binary strings tend to be used when storing text that won't be manipulated or when space efficiency is a real issue

Avoid using bit strings to tag values (e.g. `{<<"temperature">>, 50}`). Atoms take a fixed amount of space (4 or 8 bytes) regardless of how long they are. Also avoid using atoms to replace strings - strings can be manipulated while atoms can only be compared.

Erlang has no concept of a null value

# Modules

> Modules are a bunch of functions regrouped in a single file, under a single name

All functions must be defined in modules

File name should match module name

BIFs and arithmetic/logic/boolean operators belong to `erlang` module, which are automatically imported

Avoid circular dependencies

**attributes:** metadata describing the module itself such, functions visible to the outside, author of the code etc.

- `-Name(Attribute).`
- Gives hints to compiler
- Allows the retrieval of useful information from compiled code without consulting source, via the implicitly defined `module_info/1` function
- Can be user-declared, although there are limited uses in production

Notable attributes

- `-module(Name).`
    - only attribute necessary for module to be compilable; first attribute and statement of a file
- `-export([Function/Arity, ..., Function/Arity]).`
    - exported functions represent a module's interface - define one revealing strictly what is necessary and nothing more
- `-import(Module, [Function/Arity, ..., Function/Arity]).`
    - often discouraged as it reduces readability (unable to tell which module a function is imported from without referring to import statement)
    - often the only imported functions are from the list module
- `-compile([Flag1, Flag2]).`
- `-vsn(VersionNumber).` specify a version value for hot-loading. If not specified, a unique value for the code (excluding comments) will be automatically generated

Functions

- `Name(Args) -> Body.`
- Different functions within a module can share the same name if they have a different arity
- Last logical expression will have its value returned to the caller automatically - Erlang functions always return something

Macros

- `define(MACRO, some_value)` and used as `?MACRO`

## Compilation

Options

- `erlc flags file.erl` in CLI
- `compile:file(Module, OptionsList)` in shell or in module
- `c(Module)` or`c(Module, OptionsList)` in shell

Compiled bytecode will have a file name `module.beam`

BEAM stands for Bogdan/Björn's Erlang Abstract Machine. Other VMs like JAM (Joe's Abstract Machine) are not used anymore. JAM attempted to compile Erlang to C to native code, but was found to have little benefits

Instead of compiling to bytecode, an alternative is to compile to native code (not supported on every platform). This can be more performant, and is performed in the following ways:

- `hipe:c(Module, OptionsList)`
- `c(Module, [native])`

The beam file generated will contain both native (not portable across platforms) and non-native code (bytecode, portable)

Notable flags

- `-debug_info` additional debug info for debuggers, code coverage and static analysis tools
- `-{output, Dir}` the directory to create the beam files
- `-export_all` ignore the export attribute and export all functions defined (dev purposes, do not use in production)
- `-{d, Macro} or {d, Macro, Value}` defines a macro to be used in the module, most frequently used in unit-testing. `Value` is `true` by default if not defined

Flags can be defined from within a module via the `compile` attribute

# Syntax in Functions

## Pattern Matching

Pattern matching is used to avoid binding parameters and comparing them (in an if-else block)

**function clause:** function declaration like the one below

```erlang
function(X) ->
	Expression;
```

**function declaration:** formed by a group of function clauses separated by semicolons, with the final function clause ending with a period

```erlang
function(X) ->
	Expression;
function(Y) ->
	Expression;
function(_) ->
	Expression.
```

`io:format` is done with the help of tokens being replaced in a string similar to C's `printf` format specifiers. `~n` line breaks, `~s` strings/bitstrings, `~p` pretty print Erlang term (indentation and everything), `~5` to print 5 spaces. Escape `~` with `~`.

When calling `same(a, a).` on the following function, Erlang sees that the first `X` is unbound and binds `a` to it. When the second `a` is processed, it sees that `X` is already bound and verifies that the value matches with the value bound to `X`. The pattern matching succeeds.

```erlang
same(X, X) ->
  true;
same(_, _) ->
  false.
```

You can use the `=` operator in the function head to pattern match both the content inside a term and the term as a whole.

```erlang
check(Date = {Y,M,D})->
	io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]).
```

Note that in pattern matching, variables can match to anything - it can refer to:

- precise values like text, numbers or atoms
- abstract values like head|tail of a list or a tuple

Guards are used to restrict the types that match a function head

## Guards

Accepts comparisons, boolean evaluations (including math operations) and functions about data types (e.g. `is_integer/1`)

Does not accept user-defined functions so as to avoid side effects

`,` and `;` vs `andalso` and `orelse`

- `,` acts the same way as `andalso`
- `;` acts the same way as `orelse`
- `,` and `;` will catch exceptions as they happen while `andalso` and `orelse` won't
    - `Expr1; Expr2` an error thrown in `Expr1` will still result in `Expr2` being evaluated
    - `Expr1 orelse Expr2` an error in `Expr1` will cause `Expr2` to be skipped
- `andalso` and `orelse` can be nested inside guards
    - `(A orelse B) andalso C` valid guard
    - `(A; B), C` invalid guard

[Type test BIFs]() constitute more than half of the functions allowed in guard expressions. The rest are also BIFs: `abs(Number)`, `bit_size(Bitstring)`, `byte_size(Bitstring)`, `element(N, Tuple)`, `float(Term)`, `hd(List)`, `length(List)`, `node()`, `node(Pid|Ref|Port)`, `round(Number)`, `self()`, `size(Tuple|Bitstring)`, `tl(List)`, `trunc(Number)`, `tuple_size(Tuple)`.

## Ifs (Guard Patterns)

> Ifs act like guards and share guards' syntax, but outside of a function clause's head

```erlang
guard_pattern(N) ->
	if N =:= 2 -> might_succeed;
		 N =:= 3 -> also_might_succeed;
		 true -> always_does %% Erlang's 'else'
  end.
```

Ifs was added as a short way to have guards without needing to write the whole pattern matching part when it wasn't needed

As every expression in Erlang has to return something, `if` expressions that cannot find a way to succeed (e.g. `1 =:= 2`) will result in a crash

`true` should be avoided altogether - ifs are easier to read when you cover all logical ends rather than relying on a catch-all clause

## Case ... Of

> If the `if` expression is like a guard, a `case ... of` expression is like the whole function head: you can have the **complex pattern matching** you can use with each argument, and you can have **guards** on top of it

Case expression can be rewritten as a bunch of function heads with guards

- Differences are minimal - they are represented in the same way at a lower level and have the same performance cost
- Consider using function calls when there are multiple arguments that need to be evaluated, as the case syntax `case {A,B} of ... end` might surprise the reader (personal opinion: looks fine tbh)

[Syntax Comparison](https://www.notion.so/92de93478f0c4f85ac2c36266847e53b)

# Types (or lack thereof)

## Dynamic Typing

Erlang is the dynamically typed; this means that errors may only be caught at runtime and the compiler won't always tell you when things result in failure

While many think statically typed languages are safer than dynamic counterparts, Erlang achieves "safety" by being built on the notion that failure in one of the components should not affect the whole system. The language includes features that will allow you to distribute a program over to different nodes, handle unexpected errors and never stop running

Essentially, Erlang assumes that errors will happen anyway and does not use its dynamic type system as a barrier to reliability and safety of programs

Dynamic typing was historically chosen for simple reasons; those who implemented the language mostly came from dynamically typed languages

## Strongly Typed

Erlang is strongly typed and is unable to do implicit type conversions between terms

```erlang
1> 6 + "1".
** exception error: bad argument in an arithmetic expression
	in operator  +/2
		called as 6 + "1"
```

## Type Conversions

Type conversions are performed with BIFs as many of the conversions could not be implemented in Erlang itself

Conversion BIFs take the form `<type>_to_<type>` and are implemented in the `erlang` module

```erlang
1> erlang:list_to_integer("54").
54
2> erlang:integer_to_list(54).
"54"
3> erlang:list_to_integer("54.32").
** exception error: bad argument
	in function  list_to_integer/1
		called as list_to_integer("54.32")
4> erlang:list_to_float("54.32").
54.32
5> erlang:atom_to_list(true).
"true"
6> erlang:list_to_bitstring("hi there").
<<"hi there">>
7> erlang:bitstring_to_list(<<"hi there">>).
"hi there"
```

## Guarding Data Types

Partial guarding of certain data types is possible with pattern matching: e.g. tuples with curly braces, lists with square brackets (`[H|_]`)

Guards help take this further by allowing us to specify expressions that the parameters must fulfill. To restrict the type in guards, there are BIFs to ensure that patterns match against data of a single specific type (e.g. `is_atom/1`, `is_port/1`, `is_record/2`)

You might be wondering why there isn't a function `type_of(X) -> Type` - Erlang is about programming for what you expect, everything else should cause errors as soon as possible

## Static Type Analysis

---

An attempt to build a type system for Erlang was made by Simon Marlow, one of the lead developers of the Glasgow Haskell Compiler, and Phil Wadler, who worked on Haskell's design and has contributed to the theory behind monads. Only a subset of the language was type-checkable, the major omission being process types and inter-process messages.

[](http://homepages.inf.ed.ac.uk/wadler/papers/erlang/erlang.pdf)

(Unsuccessful) Typing System Paper

---

### Dialyzer & TypEr

The HiPE project, which attempts to make Erlang perform better, produced *Dialyzer*, a static analysis tool. The type system is based on *success typings*, a concept different from *Hindley-Milner* or *soft-typing* type systems

**success typings:** type-inference will not try to find the exact type of every expression, but it will guarantee that the types it infers are right, and the type errors it finds are really errors

The details about type definitions and function annotations are described in Erlang Enhancement Proposal 8 ([EEP 8](https://learnyousomeerlang.com/types-or-lack-thereof)) and the [official type documentation](http://erlang.org/doc/reference_manual/typespec.html)

You can narrow the types accepted/returned by a function with Dialyzer. It can then statically check if type safety is violated by a caller of the function

You can use polymorphic types in type specifications: e.g. `-spec([A]) -> A.` for the `hd()` function

Dialyzer and TypEr is unable to work with [type classes](https://en.wikipedia.org/wiki/Type_class) with constructors, first order types and recursive types (since version R13B04, recursive types are available as an experimental feature)

What are first order types and recursive types?

Dialyzer and *TypEr*, a type annotator, are both included in the standard distribution of Erlang. You can use Dialyzer and TypEr on the CLI by typing `typer --help` and `dialyzer --help`

[](http://www.it.uu.se/research/group/hipe/papers/succ_types.pdf)

Dialyzer Paper

[](http://user.it.uu.se/~tobiasl/publications/typer.pdf)

TypEr Paper

The types of Erlang are only annotations without effects or restrictions on actual compiling. It is not a full-blown type system and is not as strict as powerful as what Scala, Haskell or Ocaml propose

# Recursion

> Functional programming languages usually do not offer looping constructs like `for` and `while`. Instead, functional programmers rely on... *recursion*

```erlang
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).
```

```erlang
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
	lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
	++ [Pivot] ++
	lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
```

Recursive definitions are more declarative (if you get this input, do this) than the imperative counterparts (do this then that)

Note that unlike loops, recursion can cause the stack to grow very large (causing slowdown or crashes, especially on functions that loop infinitely)

## Tail Recursion

Tail recursion is a way of transforming recursion from being linear (causing space complexity to grow) to iterative. This is done with the help of an *accumulator* variable.

**accumulator:** temporary variable which stores the results of computations as they happen in order to limit the growth of calls

A proper tail recursive approach is similar to a while loop - the same elements are present, just in different positions, which is why tail recursion is said to be iterative

When working with lists, tail recursion can be used to avoid traversing a list multiple times as a result of the `++` operator

```erlang
tail_fac(N) -> tail_fac(N,1).

tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).
```

In the above snippet, space usage is constant regardless of the size of N

**tail-call optimization (TCO):** TCO is done whenever the VM sees a function calling itself in a *tail position* (the last expression to be evaluated in a function). The current stack frame is eliminated, preventing the memory consumed from growing. TCO is a special case of the more general *LCO*

**last call optimization (LCO):** LCO is done whenever the last expression to be evaluated in a function body is another function call. As with TCO, the VM avoids storing the stack frame and potentially overflowing the stack. LCO enables tail recursion to be effective between multiple functions

> Recursion and pattern matching is sometimes an optimal solution to the problem of writing concise algorithms that are easy to understand. By subdividing each part of a problem into separate functions until they can no longer be simplified, the algorithm becomes nothing but assembling a bunch of correct answers coming from short routines

# Higher Order Functions

> A function that can accept other functions transported around that way is named a *higher order function*

The concept of higher order functions are rooted in [lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus)

If function names are written without a parameter list then those names are interpreted as atoms, not functions (and thus the call fails)

To refer to a function, use the syntax `fun Module:Function/Arity`

```erlang
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
 
incr(X) -> X + 1.
decr(X) -> X - 1.
```

## Anonymous Functions

Anonymous functions let you declare a function inline, without naming them (and as such cannot be called recursively)

```erlang
fun(Args1) ->
		Expression1, Exp2, ..., ExpN;
	 (Args2) ->
		Expression1, Exp2, ..., ExpN;
	 (Args3) ->
		Expression1, Exp2, ..., ExpN
end
```

Anonymous functions allow us to write abstractions at a very low level, allowing functional programmers to focus on what is done rather than how to do it

### Variable Scope

```erlang
% F inherit's base's scope; can see A and B
base(A) ->
	B = A + 1,
	F = fun() -> A * B end,
	F().

% base cannot access F's scope
base(A) ->
	B = A + 1,
	F = fun() -> C = A * B end,
	F(),
	C. % error
```

Inherited scope is captured in the anonymous function, allowing parameters and content to be carried out of the original context.

As the anonymous function inherits the parent's scope, you cannot redefine variables declared in the parent's scope, ***unless*** the variable is present in the function (shadowing, which will throw a compiler warning)

**shadowing**: shadowing is the term used to describe the act of defining a new variable that has the same name in the parent scope

### Anonymous Named Function

Anonymous functions can be named, but the name is only visible within the anonymous function's scope, not outside of it. The main advantage is enabling recursion

```erlang
something() ->
  fun Loop() ->
		Loop()
	end
end.
```

## List Functions

When abstracting functional code, try to get rid of what's the same and let the programmer supply in the parts that change

Fold is universal in that you can implement pretty much any other recursive function on lists with fold - reverse, map, filter

Read the [documentation on lists](http://erldocs.com/18.0/stdlib/lists.html) - you'll find that you rarely need to write recursive functions since most things are already abstracted away by smart people

# Errors and Exceptions

Erlang has a few ways to handle errors in functional code, but most of the time we should let it crash (handled by concurrent part of the language)

Errors

- Compile-time
- Logical
- Run-time
- Generated

Compile-time errors are usually syntactic mistakes - check function names, tokens (brackets, periods etc), arity of functions etc.

Logical errors are a result of programmer error. You're on your own, but you might find Erlang's test frameworks, Dialyzer, debugger or tracer useful.

Run-time errors crash your code, but Erlang has a way to deal with them.

Read the [Erlang Efficiency Guide](http://erlang.org/doc/efficiency_guide/advanced.html#id2265856) if you run into or are concerned with the `system_limit` run-time error. Some of these errors are serious enough to crash the whole VM.

## Raising Exceptions

In trying to monitor the execution of code and protect against logical errors, it's often a good idea to provoke run-time crashes so problems are spotted early

### Errors

Calling `erlang:error(Reason)` will end the execution in the current process and include a stack trace of the last functions called with their arguments when you catch it - this is a run-time error

Raise errors when there's nothing that can be done by the code to recover. If there is a need to inform the calling function of the error, consider using a tuple or an atom like `undefined`.

### Exits

There are "internal" and "external" exits. 

Internal exits

- Triggered by calling the function `exit/1` and make the current process stop its execution
- Have the same use cases as errors (historically, they were the same). Differences:
    - Intent: Internal exits act as a means of communicating errors to other processes. They act as a dying process' last breath, and the receiver can decide whether to terminate
    - Stack Trace: `erlang:error/1` returns a stack trace while `exit/1` doesn't. This adds to the difference in intent, you wouldn't want listening processes to copy a potentially large stack or lots of arguments

External exits

- Called with `exit/2` and have to do with multiple processes in the concurrent aspect of Erlang (more details later)

### Throws

Class of exceptions used for cases that the programmer can be expected to handle, thus do not carry an intention to crash the process. Treat this as a control flow abstraction.

It's a good idea to document the use of throws within a module.

Invoked with `throw(Reason)`

Can be used for non-local returns when in deep recursion. Examples:

- Pushing error tuples back to a top-level function, which then catches the throw and returns the tuple. This means that the programmer only needs to write successful cases and have one function deal with all the exceptions on top of it all.
- Prevent propagating values down the recursive stack. The nested calls can invoke throw and the top-level function can do what's needed with the value.

Try to limit the use of throws for non-local returns to a single module for clarity and detaching the interface of the module from the innards

## Catching Exceptions

### try ... of ... catch ... after

Use `try ... catch` to handle throws, errors and exits

```erlang
try Expression of
	SuccessfulPattern1 [Guards] ->
		Expression1;
	SuccessfulPattern2 [Guards] ->
		Expression2
catch
	TypeOfError:ExceptionPattern1 ->
		Expression3;
	TypeOfError:ExceptionPattern2 ->
		Expression4
after % this always gets executed
	Expression5
end.
```

The `Expression` in between `try` and `of` is said to be **protected**. This means that exceptions happening within that call will be caught by the `catch` block. You can have multiple statements over here, and the last one will be pattern matched with the `of` block.

The patterns and expressions in between the `of` and `catch` behave in exactly the same manner as a `case ... of`. The `of` block is optional.

In `catch`, you can replace `TypeOfError` by either `error`, `throw`, or `exit` (default `throw` if no type provided)

The `after` block corresponds to "finally" that you see in other languages, and will always be executed regardless of errors. You cannot get any return value out of this block, and thus `after` is mostly used to run code with side effects (e.g. making sure a file gets closed).

The protected part of an exception cannot be tail recursive. The VM must always keep a reference there in case there's an exception pops up. Thus, calling a recursive function from there might be dangerous for programs supposed to run for a long time (Erlang's niche). After enough iterations, memory will run out or the program will get slower. Instead, place the recursive class between `of` and `catch` and explot LCO.

### catch

`catch` is another error handling structure and captures all types of exceptions on top of non-error results

```erlang
1> catch throw(whoa).
whoa
2> catch exit(die).
{'EXIT',die}
3> catch 1/0.
{'EXIT',{badarith,[{erlang,'/',[1,0]},
					{erl_eval,do_apply,5},
					{erl_eval,expr,5},
					{shell,exprs,6},
					{shell,eval_exprs,6},
					{shell,eval_loop,3}]}}
```

Note that throws remain the same, but that exits and errors are both represented as `{'EXIT', Reason}`. This is because errors were bolted onto the language on top of exits, and thus kept a similar representation for backward compatibility.

Reading the above stack trace:

- Error type is `badarith`
- Stack trace is in list after error type
- Tuple on top of stack trace represents the last function to be called (`{Module, Function, Arguments}`)
- Tuples after that are the functions called before the error. They are of the form `{Module, Function, Arity}`. Note that the exact arguments are not provided

You can manually get a stack trace by calling `erlang:get_stacktrace/0` in the process that crashed.

```erlang
case catch X/Y of
	{'EXIT', {badarith,_}} -> "uh oh";
	N -> N
end.
```

Pattern matching is commonly used with catch in the above manner.

Problems

- Operator precedence can be weird - for example, parenthesis are required for the following statement

    ```erlang
    X = (catch 4+2).
    ```

- It is impossible to distinguish an actual caught error/exit from a function returning something that looks like a caught error (manually constructing `{'EXIT', ...}`)
- It is impossible to distinguish a returned atom from a caught throw with the same atom

    ```erlang
    % one_or_two(1) -> return;
    % one_or_two(2) -> throw(return).
    1> catch exceptions:one_or_two(1).
    return
    2> catch exceptions:one_or_two(2).
    return
    ```

It was these problems that warranted the addition of the `try ... catch` construct in the R10B release.

# Functionally Solving Problems

## Reverse Polish Notation Calculator

Reverse Polish Notation (RPN) is postfix notation

- `(2 + 2) / 5` → `2 2 + 5 /`
- `9 * 5 + 7` →`9 5 * 7 +`
- `10 * 2 * (3 + 4) / 2` →`10 2 * 3 4 + * 2 /`

```erlang
-module(calc).

-export([rpn/1, rpn_test/0]).

rpn("+", [N1, N2 | S]) -> [N2 + N1 | S];
rpn("-", [N1, N2 | S]) -> [N2 - N1 | S];
rpn("*", [N1, N2 | S]) -> [N2 * N1 | S];
rpn("/", [N1, N2 | S]) -> [N2 / N1 | S];
rpn("^", [N1, N2 | S]) -> [math:pow(N2, N1) | S];
rpn("ln", [N | S]) -> [math:log(N) | S];
rpn("log10", [N | S]) -> [math:log10(N) | S];
%% reader exercise
rpn("sum", List) when is_list(List) -> [lists:foldl(fun(Val, Acc) -> Val + Acc end, 0, List)];
rpn("prod", List) when is_list(List) -> [lists:foldl(fun(Val, Acc) -> Val * Acc end, 1, List)];
%%
rpn(X, Stack) -> [read(X) | Stack].

read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.

rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try rpn("90 34 12 33 55 66 + * - +") catch
             error:{badmatch, [_ | _]} -> ok
         end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    1.0e+1 = rpn("10 10 10 20 sum 5 /"),
    1.0e+3 = rpn("10 10 20 0.5 prod"),
    ok.
```

## Heathrow to London