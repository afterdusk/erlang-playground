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
        - Every thread is only concerned with what its own sequential code
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
- `c(Module)` compile and load specified module
- `rd(Name, Definition)` define a record
- `rr("*")` load all records
- `rr(Module)` load record definitions from specified module
- `rl()` print all record definitions
- `rl(Name)` print record definition of given name
- `rf()` unload all records
- `rf(Name)` unload specific definitions
- `rp(Term)` convert tuple to record (given the definition exists)
- `regs()` gets a list of all registered processes

Use `erl -man` to get information about modules

Information Tags

- `[async-threads:N]` Number of threads in the async thread pool, which indirectly tells us how many system calls can be spun into background threads before emulator stalls
- `[smp:X:Y]` X cores available, Y schedulers
- More information can be found in the [SO thread](https://stackoverflow.com/questions/1182025/what-do-the-erlang-emulator-info-statements-mean)

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
- `-include("header.hrl")` include a header file

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
string:
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

![images/Untitled.png](images/Untitled.png)

Find the Shortest Path

```erlang
-module(road).

-compile(export_all).

main([FileName]) ->
    {ok, Bin} = file:read_file(FileName),
    Map = parse_map(Bin),
    io:format("~p~n", [optimal_path(Map)]),
    erlang:halt(0).

%% Transform a string into a readable map of triples
parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X)
              || X <- string:tokens(Str, "\r\n\t ")],
    group_vals(Values, []).

group_vals([], Acc) -> lists:reverse(Acc);
group_vals([A, B, X | Rest], Acc) ->
    group_vals(Rest, [{A, B, X} | Acc]).

%% Picks the best of all paths, woo!
optimal_path(Map) ->
    {A, B} = lists:foldl(fun shortest_step/2,
                         {{0, []}, {0, []}},
                         Map),
    {_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
                       hd(element(2, B)) =/= {x, 0} -> B
                    end,
    lists:reverse(Path).

%% actual problem solving
%% change triples of the form {A,B,X}
%% where A,B,X are distances and a,b,x are possible paths
%% to the form {DistanceSum, PathList}.
shortest_step({A, B, X},
              {{DistA, PathA}, {DistB, PathB}}) ->
    OptA1 = {DistA + A, [{a, A} | PathA]},
    OptA2 = {DistB + B + X, [{x, X}, {b, B} | PathB]},
    OptB1 = {DistB + B, [{b, B} | PathB]},
    OptB2 = {DistA + A + X, [{x, X}, {a, A} | PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.
```

The above code can be called from outside the Erlang shell, using the following commands:

```bash
$ erlc road.erl
$ erl -noshell -run road main road.txt
[{b,10},{x,30},{a,5},{x,20},{b,2},{b,8}]
```

# Common Data Structures

## Records

Records are an afterthought to the language and are syntactic sugar over tuples.

As such, they have their share of inconveniences, but are still useful when you need a small data structure where you want to access attributes by name.

```erlang
-record(robot, {name,
								type=industrial,
								hobbies,
								details=[]}).
```

Record attributes can have default values. If not set, Erlang sets the value to `undefined`.

The `rr` command loads record definitions and makes them easier to work with in shell. 

```erlang
1> c(records).
{ok,records}
2> records:first_robot().
{robot,"Mechatron",handmade,undefined,
			["Moved by a small man inside"]}
3> rr(records).
[robot]
4> records:first_robot().        
#robot{name = "Mechatron",type = handmade,
			hobbies = undefined,
			details = ["Moved by a small man inside"]}
```

Other Relevant Shell Commands

- `rd(Name, Definition)` define a record
- `rr("*")` load all records
- `rr(Module)` load record definitions from specified module
- `rl()` print all record definitions
- `rl(Name)` print record definition of given name
- `rl([Names])` print record definitions of given names
- `rf()` unload all records
- `rf(Name)` unload a specific definition
- `rf([Names])` unload specific definitions
- `rp(Term)` convert tuple to record (given the definition exists)

```erlang
1> NestedBot = #robot{details=#robot{name="erNest"}}.
#robot{name = undefined,type = industrial,
			hobbies = undefined,
			details = #robot{name = "erNest",type = industrial,
			hobbies = undefined,details = []}}
```

- Access a record's attribute with syntax like so: `NestedBot#robot.type`
- Access a nested record's attribute with syntax like so: `NestedBot#robot.details#robot.name`
- To output which element of the underlying tuple an attribute is: `#robot.type` (returns 3)

Records can be used in function heads to pattern match, and also in guards to filter for specific values or conditions.

```erlang
admin_panel(#user{name=Name, group=admin}) -> ...;
admin_panel(#user{name=Name}) -> ....
```

```erlang
adult_section(U = #user{}) when U#user.age >= 18 -> ...;
adult_section(_) -> ....
```

Note that it's not necessary to match on all the parts of the record, or even know how many there are when writing the function. If you were to use a tuple instead of a record, you would have to specify the exact number of elements in the function head and update it if the tuple changes.

You can update a record by reassigning its attributes. While it looks like we are modifying the record in place, a call is made to `erlang:setelement/3` which returns a new tuple.

To share records across modules, Erlang makes use of *header files* (`.hrl`). Like their C counterpart, they are snippets of code that are added to the module as if it were written there when included with `-include()`.

While some projects keep a project-wide .`hrl` file for records shared across all modules, it is probably a better idea to keep record definitions local and write functions to access its fields to keep details as private as possible. This helps prevent name clashes, avoids problems when upgrading code, and generally improves the readability/maintainability.

## Key-Value Stores

For small amounts of data:

- proplists
- orddicts

For larger amounts of data:

- dicts
- gb trees

### Proplists

Any list of tuples of the form `[{Key, Value}]`.

Use helper functions from the `proplists` module, e.g. `proplists:delete/2`, `proplists:get_value/2`, `proplists:get_all_values/2`, `proplists:lookup/2`.

There is no function to add or update an element of the list - you must use cons manually and `lists:keyreplace`. This highlights how loosely defined proplists are as a data structure, and are more like a common pattern that appears (the `proplist` module being a toolbox over such a pattern)

### Orddict

Orddicts (ordered dictionaries) are proplists with a taste of formality

- Keys must be unique
- List is sorted for faster average lookup

Use `orddict` module functions to perform CRUD e.g. `orddict:store/3`, `orddict:find/2` , `orddict:fetch/2` and `orddict:erase/2`.

Orddicts are a generally good compromise between complexity and efficiency up to about 75 elements, after which it's probably wise to switch to another KV store

### Dicts

Dictionaries have the same interface as orddicts: `dict:store/3`, `dict:find/2`,`dict:fetch/2`, `dict:erase/2` etc. They are thus great options for scaling up from orddicts

### GB Trees

General Balanced Trees have a bunch more functions compared to orddicts/dicts, giving more control over how the structure is to be used

Two modes:

- "smart mode": where you know your structure in and out.
    - Functions are `gb_trees:insert/3`, `gb_trees:get/2`, `gb_trees:update/3` and `gb_trees:delete/2`.
    - Smart functions assume that key is present in the tree, skipping all safety checks.
- "naive mode": you can't assume much about the DS.
    - Functions are `gb_trees:enter/3`,`gb_trees:lookup/2` and `gb_trees:delete_any/2`.
    - As gb trees are balanced trees, whenever you insert or delete elements it might cause the tree to rebalance itself. This takes time and memory (even for useless checks).

    Wouldn't smart function insert or delete also cause the tree to rebalance itself? How is this a disadvantage unique to gb trees?

gb trees vs dicts

- gb trees have similar performance with dicts. Dicts generally have better read speeds while the gb trees tend to be a little quicker on other operations.
- dicts have a fold function, gb_trees don't - they instead have an iterator function which returns a bit of the tree on which you can call the `gb_trees:next(Iterator)` to get the following values in order. You will need to write your own recursive function on top of gb trees rather than use a generic fold.
- gb trees let you have quick access to the smallest and largest elements of a structure with `gb_trees:smallest/1` and `gb_trees:largest/1`

Other than the above, there are also ETS tables, DETS tabls and mnesia databases. They will be covered later in detail.

Starting from version 17.0, the language supports a new native key-value data type, the de-facto replacement for dicts. This is covered in the postscript. 

## Arrays

> Erlang arrays, at the opposite of their imperative counterparts, are not able to have such things as constant-time insertion or lookup. Because they're usually slower than those in languages which support destructive assignment and that the style of programming done with Erlang doesn't necessary lend itself too well to arrays and matrices, they are rarely used in practice.

Generally, Erlang programmers who need to do matrix manipulations and other uses requiring arrays tend to use concepts called [Ports](http://erlang.org/doc/tutorial/c_port.html) to let other languages do the heavy lifting, or [C-Nodes](http://erlang.org/doc/apps/erl_interface/ei_users_guide.html), [Linked in drivers](http://erlang.org/doc/tutorial/c_portdriver.html) and [NIFs](https://erldocs.com/18.0/erts/erl_nif.html) (Experimental, R13B03+).

Arrays are also weird in the sense that they're one of the few data structures to be 0-indexed (at the opposite of tuples or lists), along with indexing in the regular expressions module. Be careful with them.

## Sets

Types

- ordsets
- sets
- gb sets
- sofs

### Ordsets

Implemented as a sorted list.

Mainly useful for small sets and are the slowest kind, but have the simplest and most readable representation.

### Sets

Sets (the module) is implemented on top of a structure similar to the one used in dict. 

They implement the same interface as ordsets, but scale better.

Like dictionaries, they're especially good for read-intensive manipulations.

### GB Sets

gb sets themselves are constructed above a General Balanced Tree structure similar to the one used in the `gb_trees` module. 

gb sets are to sets what gb trees are to dicts - an implementation that is faster for non-read operations and leaves the programmer more control. Like gb trees, there are smart and naive functions, iterators, quick access to smallest and largest values.

### Sofs

Sets of sets (sofs) are implemented with sorted lists, inside a tuple with some metadata. They give full control over relationships between sets, families, enforce set types etc. 

Because set theory is closely related to directed graphs, the `sofs` modules contains functions to convert family to digraphs and digraphs to families.

While variety is great, some implementation details can be frustrating. For example, gb sets, orsets and sofs all use `==` to compare values, the `sets` module uses the `=:=` operator, which means you can't necessarily switch over every implementation.

Björn Gustavsson, from the Erlang/OTP team [suggests](http://erlang.org/pipermail/erlang-questions/2010-March/050332.html) using gb sets in most circumstances, using ordset when you need a clear representation that you want to process with your own code and `sets` when you need the `=:=` operator.

Always test and measure when deciding whether to use a data structure over another.

## Directed Graphs

Implemented in two modules:

- `digraph`
    - Allows the construction and modification of a directed graph: manipulating edges and vertices, finding paths and cycles etc.
- `digraph_utils`
    - Allows the navigation of graph (postorder, preorder), testing for cycles, arborescences of or trees, finding neighbours, and so on.

**arborescence** a directed graph in which, for a vertex u called the root and any other vertex v, there is exactly one directed path from u to v.

## Queues

The `queue` module implements a double-ended FIFO queue.

Implemented as two lists (stacks) that allow both appending and prepending elements rapidly.

Seek more details about how the two lists are implemented.

The queue module separates functions into 3 interfaces of varying complexity

- Original API
    - Contains functions at the base of the queue concept - `new/0` for creating empty queues, `in/2` for inserting new elements, `out/1` for removing elements.
    - Also contains functions to convert to lists, reverse the queue, find a value in queue.
- Extended API
    - Adds introspection power and flexibility: look at front of the queue without removing the first element (`get/1` or `peek/1`), removing elements without caring about them (`drop/1`).
- Okasaki API
    - Derived from Chris Okasaki's [Purely Functional Data Structures](http://books.google.ca/books?id=SxPzSTcTalAC&lpg=PP1&dq=chris%20okasaki%20purely%20functional%20data%20structures&pg=PP1#v=onepage&q=&f=false), provides operations similar to what was available in the previous APIs.
    - Unless you know you want this API, ignore it.

Use queues when you need to ensure that the first item ordered is the first one processed. Thus far, we've used list as accumulators that would then be reversed. In cases where you can't do the reversing at once and elements are frequently added, use queues.

# Hitchhiker's Guide to Concurrency

**concurrency** refers to the idea of having many actors running independently, but not necessarily at the same time

**parallelism** having actors running exactly at the same time

Erlang had concurrency from the beginning, when everything was done on single core processors. Each Erlang process would have its own time slice to run, much like desktop applications did before multi-core systems. 

To achieve parallelism back then, we needed a second computer running code and communicating with the first one. Nowadays, multi- core systems allow for parallelism on a single computer and Erlang takes advantage of that.

There's a misconception that Erlang was ready for multi-core computers years before it actually was. True symmetric mulitprocessing (SMP) was only available in 2000s and prior, parallelism on multi-core computers was achieved by starting many instances of the VM.

## Scalability

As actors are processes which only reacted upon certain events, an ideal system would support processes doing small computations, switching between them as events came through.

To make it efficient, it made sense for processes to be started, destroyed, and switched between very fast. This is achieved with lightweight processes.

Additionally, lightweight processes also helped avoid having to set limits on the number of processes a program can start.

It is also important to be able to bypass hardware limitations. You can do this by:

- Making hardware better
    - Only useful up until a certain point, after which it becomes extremely expensive (buying a supercomputer)
- Adding more hardware
    - Usually cheaper - distribution is thus useful to have as part of the language

As applications need to be reliable, processes were forbidden from sharing memory, since that could leave things in an inconsistent state after crashes. Message passing risks being slower and thus being bad for scalability, but is safer.

## Fault-Tolerance

It is near impossible to prevent bugs, plus a bug-free program can still face hardware failures - thus it is a good idea to handle errors and problems rather than trying to prevent them all.

Lightweight processes with quick restarts and shutdowns help errors  which cause corrupt data to quickly crash the faulty part of the system. This prevents errors and bad data from propagating to the rest of the system.

There exist many ways for a system to terminate, two of which are clean shutdowns and crashes. The worse case is the crash, thus we can strive to make sure all crashes are the same as clean shutdowns, by:

- sharing nothing
- single assignment (which isolates a process' memory)
- avoiding locks (could happen to not be unlocked during a crash)

Error handling mechanisms are also crucial in helping processes monitor other processes to know when processes die and handle the event.

To accommodate hardware failure, distribute your program over machines (needed for scaling anyway). Message passing and no-shared memory helps because the programs work the same way locally or on a different computer, making fault tolerance through distribution nearly transparent to the programmer.

Note that you cannot assume that a node will be alive throughout the whole remote function call, or that it will execute correctly. With asynchronous message passing, messages sent from one process to another are stored in a mailbox inside the receiving process:

- Messages are sent without checking if receiving process exists or not
- It's impossible to know if a process will crash between the time a message is sent and received.
- If received, it's impossible to know if it will be acted upon or if receiving process will die before that.

Asynchronous messages allow safe remote function calls because there's no assumption about what will happen, the programmer implements confirmations by sending further messages.

## Implementation

OSes can't be trusted to handle the processes - they have different ways of handling processes and their performance varies a lot. Most of them are too slow or too heavy for what is needed in standard Erlang applications.

Instead, Erlang handles processes in the VM and lets the language implementers keep control of optimization and reliability. 

Erlang's processes take about 300 words of memory each and can be created in a manner of microseconds — not something doable on major operating systems these days.

The VM starts one thread per core which acts as a *scheduler*. However, as of R15B, the runtime system does not bind schedulers to logical processors by default. This is because performance will be poor if the processors are performing work other than running BEAM.

**scheduler** each scheduler has a *run queue.*

**run queue** a list of Erlang processes on which to spend a slice of time. 

When one of these schedulers has too many tasks in the run queue, some are migrated to another one. The Erlang VM thus takes care of load-balancing.

The rate at which messages can be sent on overloaded processes is also limited in order to regulate and distribute the load.

Inspect the shell information tags to gain insight on how many cores, schedulers and async threads BEAM is using. See [here](https://stackoverflow.com/questions/1182025/what-do-the-erlang-emulator-info-statements-mean).

## Linear Scaling

Performance doesn't linearly scale the number of cores/processors.

Problems that scale well are said to be *embarrassingly parallel* (e.g. ray-tracing, brute-force searches)

Erlang's embarrassingly parallel problems are at a higher level. Usually, they have to do with concepts such as chat servers, phone switches, web servers, message queues, web crawlers or other systems where work can be represented as independent logical entities (actors).

Amdahl's law demonstrates that getting rid of the last few sequential parts of a program allows a relatively huge theoretical speedup compared to removing the same amount of sequential code tin a program that is not very parallel to begin with.

When running purely sequential applications, hardware optimizations cause the VM to spend time doing useless stuff and run much slower on many cores than on a single one. You can disable symmetric multiprocessing (`$ erl +S 1`) in this case.

## Concurrency Primitives

Key Primitives

- Spawning new processes
- Sending messages
- Receiving messages

A process is a function with some hidden state (mailbox for messages).

```erlang
1> spawn(fun() -> 2 + 2 end).
<0.44.0>
```

A process is created with `spawn/1`, and the result is a *Process Identifier* (PID)

**process identifier** an arbitrary value representing any process that exists (or might have existed) at some point in the VM's life. It is used as the process' address for communication.

`self/0` returns the pid of the current process

`exit/1` kills a process

You can also create a process with `spawn(Module, Function, [Arguments])`

```erlang
1> self() ! hello.
hello
2> self() ! self() ! double.
double
```

A message is sent with the `!` symbol. 

In the above, the message has been put in the process' mailbox, but it hasn't been read yet - the `hello` seen in the shell is the return value of the send operation.

Messages in a mailbox are kept in the order they are received.

`flush/0` outputs the contents of the current mailbox.

```erlang
receive
	Pattern1 when Guard1 -> Expr1;
	Pattern2 when Guard2 -> Expr2;
	Pattern3 -> Expr3;
end
```

Receive is syntactically similar to `case ... of` and works the same way, except they bind variables coming from messages.

In order to send a reply, the sender must include their own PID with the message, i.e. `{Pid, Message}`.

In order for the receiving process to "stay alive" after responding to a message, it should call itself in a tail position. This will not blow the stack even if the function loops indefinitely, thanks to tail recursion.

# More on Multiprocessing

Transient state can be maintained as parameters of the recursive receiving function.

For a cleaner interface, use helper functions to abstract away the spawning of the fridge process, as well as the actual sending and receiving. This also allows us to modify the internals (e.g. add logging when fridge starts) without changing the interface.

```erlang
fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food|FoodList]);
		{From, {take, Food}} ->
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, Food}},
					fridge2(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge2(FoodList)
			end;
		terminate ->
			ok
	end.

store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.
 
take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

start(FoodList) ->
	spawn(?MODULE, fridge2, [FoodList]). % ?MODULE is a macro returning current module's name
```

## Timeout

```erlang
receive
	Match -> Expression1
after Delay ->
	Expression2
end.
```

`Delay` is an integer representing milliseconds. After that amount of time has been spent without receiving a message that matches the `Match` pattern, `Expression2` is executed.

`Delay` can also be the atom `infinity` - while this is not useful in most cases since you can just remove the `after` clause, it is sometimes used when `Delay` is parameterised. The caller can submit `infinity` to wait forever.

Other than giving up after too long, timeout can also be used to trigger events after a certain amount of time (think `timer:sleep/1`).

```erlang
flush() ->
	receive
		_ -> flush()
	after 0 ->
		ok
	end.
```

`Delay` can also be set as `0` to serve as a "finally". In the above, `flush()` repeatedly calls itself every time there's a message in the mailbox, and executes `after` when the mailbox is empty.

## Selective Receives

Receive takes the first message that matches its available patterns, ignoring all messages that do not. 

```erlang
important() ->
	receive
		{Priority, Message} when Priority > 10 ->
		[Message | important()]
	after 0 ->
		normal()
	end.
 
normal() ->
	receive
		{_, Message} ->
			[Message | normal()]
	after 0 ->
		[]
	end.
```

In the process of reading messages from the mailbox, Erlang puts messages that are seen but not matched into a *save queue*, which is then put back into the mailbox after a match is found (or not found). As there are more non-matched messages, the process of reading useful messages thus get slower.

For example, the first 366 message in a mailbox are useless. They will always have to be taken and put into a save queue everytime a useful message is searched for.

This is a frequent cause of performance problems in Erlang. If your application is slow and there are a lot of messages, this could be a cause.

The aforementioned issue can be solved by:

- Making sure every message will match at least one clause. A catch-all pattern can process the unexpected message.
- If you need selective receives and can't implement a catch-all, implement a min-heap or use `gb_trees` to store messages. This will allow for quick searching by "priority".

In R14A, an optimization was added to the compiler. By making a reference (`make_ref()`) and having selective receive look out for the reference, the compiler automatically makes sure the VM will skip messages received before the creation of that reference.

# Errors and Processes

## Links

When a link is set up between two processes, if one of the processes dies from an unexpected throw, error or exit, the other linked process also dies.

Links help processes fail as soon as possible to stop further errors. If a process crashes, links help us stop the other processes that depend on the crashed process.

`link/1` takes a pid as an argument, creating a link between the current process and the one identified by the pid.

Links cannot be stacked - linking 15 times for the same two processes will result in one link being established.

`unlink/1` gets rid of a link between the current process and that identified by the supplied pid.

```erlang
1> link(spawn(fun() -> exit(reason) end)).
** exception error: reason
```

When a linked process crashes, a special message is sent with information of what happened. 

- This message is not sent on normal termination.
- The exception message cannot be caught with a `try ... catch`.
- It propagates from the source of the crash along the established links (furthest process receives signal last).

Note that `link(spawn(Function))` happens in more than one step and its possible for a process to die before the link has been set up. `spawn_link/1-3` was added to address this, performing the steps as one atomic operation.

## Traps

Error propagation across processes is done similarly as message passing, but with a special type of message called *signals*. Exit signals kill processes.

To achieve reliability, an application needs to be able to both kill and restart a process quickly. 

System processes are normal processes that can convert exit signals to regular messages, and can thus detect when a process has died. They do this by calling `process_flag(trap_exit, true)` in a running process.

```erlang
1> process_flag(trap_exit, true).
false
2> spawn_link(fun() -> exit(reason) end).
<0.49.0>
3> receive X -> X end.
{'EXIT',<0.49.0>, reason}
```

Trapped vs Untrapped Results

- **Exception source: `spawn_link(fun() -> ok end)`**

    **Untrapped Result**: - nothing -

    **Trapped Result**: {'EXIT', <0.61.0>, normal}

    The process exited normally, without a problem. Note that this looks a bit like the result of `catch exit(normal)`, except a PID is added to the tuple to know what processed failed.

- **Exception source: `spawn_link(fun() -> exit(reason) end)`**

    **Untrapped Result**: ** exception exit: reason

    **Trapped Result**: {'EXIT', <0.55.0>, reason}

    The process has terminated for a custom reason. In this case, if there is no trapped exit, the process crashes. Otherwise, you get the above message.

- **Exception source: `spawn_link(fun() -> exit(normal) end)`**

    **Untrapped Result**: - nothing -

    **Trapped Result**: {'EXIT', <0.58.0>, normal}

    This successfully emulates a process terminating normally. In some cases, you might want to kill a process as part of the normal flow of a program, without anything exceptional going on. This is the way to do it.

- **Exception source: `spawn_link(fun() -> 1/0 end)`**

    **Untrapped Result**: Error in process <0.44.0> with exit value: {badarith, [{erlang, '/', [1,0]}]}

    **Trapped Result**: {'EXIT', <0.52.0>, {badarith, [{erlang, '/', [1,0]}]}}

    The error (`{badarith, Reason}`) is never caught by a `try ... catch` block and bubbles up into an 'EXIT'. At this point, it behaves exactly the same as `exit(reason)` did, but with a stack trace giving more details about what happened.

- **Exception source: `spawn_link(fun() -> erlang:error(reason) end)`**

    **Untrapped Result**: Error in process <0.47.0> with exit value: {reason, [{erlang, apply, 2}]}

    **Trapped Result**: {'EXIT', <0.74.0>, {reason, [{erlang, apply, 2}]}}

    Same effect as `1/0` - `erlang:error/1` is intended to behave like that.

- **Exception source: `spawn_link(fun() -> throw(rocks) end)`**

    **Untrapped Result**: Error in process <0.51.0> with exit value: {{nocatch, rocks}, [{erlang, apply, 2}]}

    **Trapped Result**: {'EXIT', <0.79.0>, {{nocatch, rocks}, [{erlang, apply, 2}]}}

    Because the `throw` is never caught by a `try ... catch`, it bubbles up into an error, which in turn bubbles up into an EXIT. Without trapping exit, the process fails. Otherwise it deals with it fine.

- **Exception source: `exit(self(), normal)`**

    **Untrapped Result**: ** exception exit: normal

    **Trapped Result**: {'EXIT', <0.31.0>, normal}

    When not trapping exits, `exit(self(), normal)` acts the same as `exit(normal)`. Otherwise, you receive a message with the same format you would have had by listening to links from foreign processes dying.

- **Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), normal)`**

    **Untrapped Result**: - nothing -

    **Trapped Result**: - nothing -

    This basically is a call to `exit(Pid, normal)`. This command doesn't do anything useful, because a process cannot be remotely killed with the reason `normal` as an argument.

- **Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), reason)`**

    **Untrapped Result**: ** exception exit: reason

    **Trapped Result**: {'EXIT', <0.52.0>, reason}

    This is the foreign process terminating for reason itself. Looks the same as if the foreign process called `exit(reason)` on itself.

- **Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), kill)`**

    **Untrapped Result**: ** exception exit: killed

    **Trapped Result**: {'EXIT', <0.58.0>, killed}

    The spawner now receives `killed` instead of `kill`. That's because `kill` is a special exit signal.

- **Exception source: `exit(self(), reason)`**

    **Untrapped Result**: ** exception exit: reason

    **Trapped Result**: {'EXIT', <0.31.0>, reason}

    System processes can trap exit called on the system process itself. Oddly, calling `exit(reason)` directly in a system process does result in a crash.

- **Exception source: `exit(self(), kill)`**

    **Untrapped Result**: ** exception exit: killed

    **Trapped Result**: ** exception exit: killed

    `kill` is a special signal that cannot be trapped, ensuring any process terminated with it will really be dead. It's a last resort (e.g. system process is stuck in an infinite loop). 

    As the `kill` reason cannot be trapped, it needs to be changed to `killed` when other processes receive the message, else every other process linked will die.

- **Exception source: `spawn_link(fun() -> exit(kill) end)`**

    **Untrapped Result**: ** exception exit: killed

    **Trapped Result**: {'EXIT', <0.67.0>, kill}

    Exit can be trapped when it happens in a linked process.

## Monitors

Monitors are a special type of link with two differences:

- they are unidirectional
- they can be stacked

Use monitors when a process needs to know what's going on in a second process, but neither of them is vital to each other.

Links are more of an organizational construct - some processes will supervise others, some couldn't live without a twin process. Links are used to establish structure that is fixed and known in advance.

Say you use X number of libraries in a process that all need to establish links with another process. If one of them unlinks with the other process, all of the links are removed - this is why stackable links are necessary.

It might also be useful for the other process to be unaware of said libraries, thus unidirectional links are desirable.

```erlang
1> monitor(process, spawn(fun() -> timer:sleep(500) end)).
#Ref<0.0.0.77>
2> flush().
Shell got {'DOWN',#Ref<0.0.0.77>,process,<0.63.0>,normal}
ok
```

Every time a process being monitored goes down, a message `{'DOWN', MonitorReference, process, Pid, Reason}` is received. Since monitors are stackable, the reference allows the monitor-er to tear down the specific monitor.

`monitor/2` takes the `process` atom and a pid, setting up a monitor from the current process on the processed identified by the pid. If a process with the Pid does not exist (e.g. already dead), the `'DOWN'` message is sent immediately with `Reason` set to `noproc`.

`demonitor/1` accepts a monitor reference and tears down the monitor.

`demonitor/2` accepts a list of options as the second parameter.

- `info` causes the function to return a boolean indicating whether a monitor existed when the function was called
- `flush` removes the `DOWN` message from the mailbox if it existed

As with links, there is an atomic `spawn_monitor/1-3`.

## Naming Processes

Naming a process replaces the unpredictable pid with an atom. The atom can be used exactly as a pid when sending messages.

`register/2` gives a process the supplied name

`unregister/1` removes the name from a process. Note that when a process dies, it automatically loses its name.

`registered/0` gets a list of all registered processes.

`whereis/1` takes an atom and returns a pid corresponding to a process named as that atom.

```erlang
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, receiver, []),
	register(receiver, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		{'EXIT', Pid, _} ->
	restarter()
	end.

sender(Message) ->
	receiver ! {self(), {Message}},
	Pid = whereis(receiver),
	receive
		{Pid, Response} -> Response
	after 2000 ->
		timeout
	end.
```

When pattern matching against messages, you will need to use `whereis` to find the exact pid of the named process. 

However, this introduces the implicit assumption that the named process' pid will remain the same between receiving the message, and the call to `whereis`. The process might have died in between the two steps and restarted, giving a different pid.

This *race condition* is a result of sharing state across processes (the name of the process). Essentially, the name can be accessed/modified by different processes at the same time, causing inconsistent information and software errors.

Despite what people might say, Erlang is not free of race conditions or deadlocks - parallel code is not automatically safe. Named processes are one of the ways parallel code can go wrong, others include file access, updating databases etc.

To address the above issue, don't assume the name process remains the same. Use references (`make_ref()`) as unique values to identify messages.

```erlang
sender2(Message) ->
	Ref = make_ref(),
	critic ! {self(), Ref, Message},
	receive
		{Ref, Response} -> Response
	after 2000 ->
		timeout
	end.
```

Named processes help processes continue to function even when a process that they depend on is restarted (and thus change pid). 

However, atoms should never be created dynamically. Naming processes should be reserved for important services unique to an instance of a VM that stay alive for the entire lifetime of the application.
If you need a transient named process or if its not unique to the VM, it is probably better represented as a group. Linking and restarting them together if they crash is a better option.

# Designing a Concurrent Application

This section will build a reminder app.

## Specification

- Add an event. Events contain a deadline (the time to warn at), an event name and a description.
- Show a warning when the time has come for it.
- Cancel an event by name.
- No persistent disk storage. It's not needed to show the architectural concepts we'll see. It will suck for a real app, but I'll instead just show where it could be inserted if you wanted to do it and also point to a few helpful functions.
- Given we have no persistent storage, we have to be able to update the code while it is running.
- The interaction with the software will be done via the command line, which can be extended later.

    ![images/Untitled%201.png](images/Untitled%201.png)

Event Server

- Accepts subscriptions from clients
- Forwards notifications from event processes to each of the subscribers
- Accepts messages to add events (and start the x, y, z processes needed)
- Can accept messages to cancel an event and subsequently kill the event processes
- Can be terminated by a client
- Can have its code reloaded via the shell.

Client

- Subscribes to the event server and receive notifications as messages. As such it should be easy to design a bunch of clients all subscribing to the event server. Each of these could potentially be a gateway to different interaction points (GUI, web page, instant messaging software, email, etc.)
- Asks the server to add an event with all its details
- Asks the server to cancel an event
- Monitors the server (to know if it goes down)
- Shuts down the event server if needed

Event (x, y, z)

- Represent a notification waiting to fire (they're basically just timers linked to the event server)
- Send a message to the event server when the time is up
- Receive a cancellation message and die

In a real-world application, using a process per event is overkill and will affect scaling. Use `timer:send_after` to avoid spawning too many processes.

## Protocol Definition

This section will make a list of all messages that will be sent and specify what they look like.

Client - Event Server

- Client monitors event server, since it doesn't work without the server. However, we do not want to assume client wants to crash when the server crashes.
- Event server monitors client.
- Subscription

    Client →Event Server: `{subscribe, Self}`

    Event Server → Client: `ok`

- Add Event

    Client → Event Server: `{add, Name, Description, TimeOut)`

    Event Server → Client: `ok | {error, Reason}`

- Remove Event

    Client → Event Server: `{cancel, Name}`

    Event Server → Client: `ok`

- Notification

    Event Server → Client: `{done, Name, Description}`

- Shutdown

    Client → Event Server: `shutdown`

- Crash

    Event Server → Client: `{'DOWN', Ref, process, Pid, shutdown}`

Event Server - Event

- Event servers are linked to events - we want all events to die if the server does.
- Done

    Event → Event Server: `{done, Id}`

- Cancel

    Event Server → Event: `cancel`

    Event → Event Server: `ok`

Shell - Event Server

- Code Change

    Shell → Event Server: `code_change`

## Directory Structure

Erlang directory structure (standard OTP practice):

- `ebin/` where files go once they are compiled
- `include/` used to store `.hrl` files that are to be included by other applications
- `priv/` executables that might have to interact with Erlang e.g. drivers
- `src/` contains private `.hrl` files and main `.erl` source

Optional, non-standard directories:

- `conf/` for configuration files
- `doc/` for documentation files
- `lib/` third party libraries required for application to run

An `Emakefile` can be used to help the Erlang compiler compile the `.beam` files

```erlang
{'src/*', [debug_info,
           {i, "src"},
           {i, "include"},
           {outdir, "ebin"}]}.
```

The above tells the compiler to add `debug_info` to the files (apparently an option that is almost always included), look for files in the `src/` and `include/` directory, and output them in `ebin/`.

- `erl -make` calls the compiler with the configuration specificed in the `Emakefile`
- `erl -pa ebin/` starts the erlang shell with the bytecode in the `ebin/` folder. The `-pa directory` flag tells the Erlang VM to add the path to the places it looks for modules.

You can also start the shell and call `make:all([load])`  to have the shell look for a file named 'Emakefile' in the current directory, recompile it if it changed and load the new files.

## Event Module

Messages will be wrapped in the form `{Pid, Ref, Message}`

- `Pid` is the sender
- `Ref` is a unique message identifier so that responses can be associated with messages

State will have to contain data such as the timeout value, name of the event and the server's pid

```erlang
-module(event).
-compile(export_all).
-record(state, {server,
							name="",
							to_go=0}).

loop(S = #state{server=Server}) ->
	receive
		{Server, Ref, cancel} ->
			Server ! {Ref, ok}
		after S#state.to_go*1000 ->
			Server ! {done, S#state.name}
	end.
```

Language wart: `S#state.server` is secretly expanded to `element(2, S)`, which isn't a valid pattern to match on. As such, in the above piece of code, `S#state.server` is bound to `Server`.

```erlang
1> c(event).
{ok,event}
2> rr(event, state).
[state]
3> spawn(event, loop, [#state{server=self(), name="test", to_go=5}]).
<0.60.0>
4> flush().
ok
5> flush().
Shell got {done,"test"}
ok
```

Erlang's timeout value is limited to about 50 days in milliseconds.

```erlang
-module(event).
-export([start/2, start_link/2, cancel/1, init/3]).
-record(state, {server, name="", to_go=[0]}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} -> 
      Server ! {Ref, ok}
  after T*1000 ->
    if Next =:= [] ->
      Server ! {done, S#state.name};
    Next =/= [] ->
      loop(S#state{to_go=Next})
    end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0  -> ToGo;
          ToGo =< 0 -> 0
        end,
  normalize(Secs).

%%% Event's innards
init(Server, EventName, DateTime) ->
  loop(#state{server=Server,
              name=EventName,
              to_go=time_to_go(DateTime)}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

cancel(Pid) ->
  %% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid!{self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.
```

# Event Server

Descriptions of events are kept in the event server rather than sent to the event, to avoid unnecessary traffic (since it will need to be sent back).

Orddicts are used to store both clients and events - we're unlikely to have hundreds of them at once.

```erlang
-module(evserv).
-compile([]).
-record(state, {events,    %% list of #event{} records
                clients}). %% list of Pids
-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      %% monitor clients to be notified when they go down
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid!{MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, Timeout),
          NewEvents = orddict:store(Name,
                                    #event{name=Name,
                                          description=Description,
                                          pid=EventPid,
                                          timeout=Timeout},
                                          S#state.events),
          Pid!{MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid!{MsgRef, {error, bad_timeout}},
          loop(S)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                {ok, E} ->
                  event:cancel(E#event.pid),
                  orddict:erase(Name, S#state.events);
                error ->
                  S#state.events
              end,
      Pid!{MsgRef, ok},
      loop(S#state{events=Events});
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description},
                          S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(S#state{events=NewEvents});
        error ->
          %% happens if event fires and is cancelled at the same time
          loop(S)
      end;
    shutdown ->
      %% other shutdown code goes here
      exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
    code_change ->
      ?MODULE:loop(S);
    Unknown ->
      %% production system would log with a dedicated module
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)
  end.

init() ->
  %% Loading events from a static file could be done here.
  %% You would need to pass an argument to init telling where the
  %% resource to find the events is. Then load it from here.
  %% Another option is to just pass the events straight to the server
  %% through this function.
  loop(#state{events=orddict:new(),
              clients=orddict:new()}).

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid!Msg end, ClientDict).

valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
    false
  end;
    valid_datetime(_) ->
    false.
 
%% unfortunately the calendar module does not such a function
valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                      M >= 0, M < 60,
                      S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
```

## Hot Code Loading

To perform hot code loading, Erlang has a *code server*.  

**code server:** is a VM process in charge of an *ETS table*, and in-memory database table. It holds two versions of a single module in memory and both version can run at once.

A new version is automatically loaded when compiling with `c(Module)`, when loading with `l(Module)`, or with one of the functions of the code module.

**local calls:** function calls you can make with functions that might not be exported, with the format `Atom(Args)`.

**external calls:** can only be done with exported functions and has the form `Module:Function(Args)`.

- When there are two versions of a module loaded in the VM, all local calls are done through the currently running version in a process
- External calls are always done on the newest version of the code available in the code server
- If local calls are made from within the external call, they are in the new version of the code.

Given that processes do recursive calls to change state, new versions of an actor can be loaded with an external recursive call (.e.g. `?MODULE:loop/1`).

If a third version of a module is loaded while a process is still running with the first one, that process gets killed by the VM, which assumes it was an orphan process without a supervisor or a way to upgrade itself. If the old version is not being run, it is dropped.

It is possible to bind the process to a system module that will send messages whenever a new version of a module is loaded, giving more control to when code upgrades are performed (e.g. via a dedicated `MyModule:Upgrade(CurrentState)` which allows us to transform the state data structure to the new specification).

```erlang
-module(hotload).
-export([server/1, upgrade/1]).
 
server(State) ->
	receive
		update ->
			NewState = ?MODULE:upgrade(State),
			?MODULE:server(NewState);  %% loop in the new version of the module
		SomeMessage ->
				%% do something here
				server(State)  %% stay in the same version no matter what.
	end.
 
upgrade(OldState) ->
	%% to be implemented in the new version to transform state from old version
```

## Message Hiding

```erlang
start() ->
	%% for large apps, register names with the global module or the gproc library
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.
 
start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.
 
terminate() ->
  ?MODULE!shutdown.

subscribe(Pid) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE!{Self(), Ref, {subscribe, Pid}},
	receive
		{Ref, ok} ->
			{ok, Ref};
		{'DOWN', Ref, process, _Pid, Reason} ->
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

add_event(Name, Description, TImeOut) ->
	Ref = make_ref(),
	?MODULE!{self(), Ref, {add, Name, Description, TimeOut}},
	receive
		{Ref, Msg} -> Msg
	after 5000 ->
		{error, timeout}
	end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE!{self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  %% accumulates all messages during a given period of time
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay * 1000 ->
    []
  end.
```

```erlang
1> evserv:start().
<0.34.0>
2> evserv:subscribe(self()).
{ok,#Ref<0.0.0.31>}
3> evserv:add_event("Hey there", "test", FutureDateTime).
ok
4> evserv:listen(5).
[]
5> evserv:cancel("Hey there").
ok
6> evserv:add_event("Hey there2", "test", NextMinuteDateTime).
ok
7> evserv:listen(2000).
[{done,"Hey there2","test"}]
```

## Adding Supervision

```erlang
-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).
 
start(Mod,Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).
 
start_link(Mod,Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).
 
init({Mod,Args}) ->
  process_flag(trap_exit, true),
  loop({Mod,start_link,Args}).
 
loop({M,F,A}) ->
  Pid = apply(M,F,A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown); % will kill the child too
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
      loop({M,F,A})
  end.
```

The above supervisor will restart the process it watches indefinitely until the supervisor itself is terminated with a shutdown exit signal.

Killing the supervisor will also kill the linked child process.

```erlang
1> SupPid = sup:start(evserv, []).
<0.43.0>
2> whereis(evserv).
<0.44.0>
3> exit(whereis(evserv), die).
true
Process <0.44.0> exited for reason die
4> exit(whereis(evserv), die).
Process <0.48.0> exited for reason die
true
5> exit(SupPid, shutdown).
true
6> whereis(evserv).
undefined
```

# Namespaces

As Erlang has a flat module structure, some applications frequently enter conflicts (e.g. `user` module that almost every projects attempts to define, which clashes with the `user` module that ships with Erlang).

Clashes can be tested with the function `code:clash/0`, which searches the entire code space for module names with identical names and writes a report to stdout.

A common pattern is thus to prefix every module name with the name of your project. Some programmers add a module named after the application itself which wraps common calls that programmers could use when using their own application.

Registered names and database tables also result in clashes.

# Open Telecom Platform

> If half of Erlang's greatness comes from its concurrency and distribution and the other half from its error handling capabilities, then the OTP framework is the third half of it.

The previous chapters demonstrated how we can write concurrent applications with the languages' built-in facilities: links, monitors, servers, timeouts, trapping exits, etc. There were some gotchas: avoiding race conditions, always remember that a process can die any time.

Doing all of this manually is time consuming and sometimes prone to error. There are corner cases that are easily overlooked. The OTP framework takes care of this by grouping these essential practices into a set of libraries that have been carefully engineered and battle-hardened over years.

The OTP framework is also a set of modules and standards designed to help you build applications. Given most Erlang programmers use OTP and thus Erlang applications tend to follow these standards.

## The Common Process, Abstracted

![images/Untitled%202.png](images/Untitled%202.png)

Parts present in all concurrent programs

OTP abstracts them into standard libraries built with far more caution (gen, sys, proc_lib), including functions to safely spawn and initialize processes, send messages to them in a fault-tolerant manner, and many other things.

However, the abstractions these OTP libraries contain are so basic and universal that a lot more interesting things were built on top of them (gen_*, supervisors). We use those libraries instead.

```erlang
%%%%% Naive version
-module(kitty_server).
 
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
 
-record(cat, {name, color=green, description}).
 
%%% Client API
start_link() -> spawn_link(fun init/0).
 
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, {order, Name, Color, Description}},
	receive
		{Ref, Cat} ->
			erlang:demonitor(Ref, [flush]),
			Cat;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
 
%% Asynchronous call (does not wait for response)
return_cat(Pid, Cat = #cat{}) ->
	Pid ! {return, Cat},
	ok.
 
%% Synchronous call
close_shop(Pid) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, terminate},
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
	 
%%% Server functions
init() -> loop([]).
 
loop(Cats) ->
	receive
		{Pid, Ref, {order, Name, Color, Description}} ->
			if Cats =:= [] ->
					Pid ! {Ref, make_cat(Name, Color, Description)},
					loop(Cats);
				Cats =/= [] -> % got to empty the stock
					Pid ! {Ref, hd(Cats)},
					loop(tl(Cats))
			end;
		{return, Cat = #cat{}} ->
			loop([Cat|Cats]);
		{Pid, Ref, terminate} ->
			Pid ! {Ref, ok},
			terminate(Cats);
		Unknown ->
			%% do some logging here too
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(Cats)
	end.
	 
%%% Private functions
make_cat(Name, Col, Desc) ->
	#cat{name=Name, color=Col, description=Desc}.
	 
terminate(Cats) ->
	[io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
	ok.
```

The above is a kitty store. You describe a cat and you get that cat. If someone returns a cat, it's added to a list and is then automatically sent as the next order instead of what the client actually asked for.

```erlang
1> c(kitty_server).
{ok,kitty_server}
2> rr(kitty_server).
[cat]
3> Pid = kitty_server:start_link().
<0.57.0>
4> Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
#cat{name = carl,color = brown,
description = "loves to burn bridges"}
5> kitty_server:return_cat(Pid, Cat1).
ok
6> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = carl,color = brown,
description = "loves to burn bridges"}
7> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = jimmy,color = orange,description = "cuddly"}
8> kitty_server:return_cat(Pid, Cat1).
ok
9> kitty_server:close_shop(Pid).
carl was set free.
ok
```

In the next part, we will abstract the common components out of the kitty server.

## A Generic Server

Synchronous and asynchronous calls can be abstracted to a generic call function. Async calls are typically referred to as "casts".

```erlang
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
```

```erlang
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.
```

The loop can be simplified to separate the pattern matching from the loop itself.

```erlang
loop(Module, State) ->
    receive
        {async, Msg} ->
            loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.
```

The client application needs to implement `handle_cast` and `handle_call` accordingly for each expected pattern.

Note that `{Pid, Ref}` is intentionally put into a tuple so that they can be passed as a single argument as a variable like `From`. The user doesn't have to know anything about the tuple's innards, preventing the abstraction from leaking. We also provide a function to send replies that should understand what `From` contains.

```erlang
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.
```

We specify starter functions `start`, `start_link` and `init` that pass around the module names.

```erlang
%%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).
```

With the above generic server functions (inside a module `my_server`), we can rewrite the kitty store.

```erlang
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]). %% client API
-export([init/1, handle_call/3, handle_cast/2]).                  %% my_server API

-record(cat, {name, color = green, description}).

%%% Client API
start_link() ->
    my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).

%%% Server functions
init([]) ->
    []. %% no treatment of info here!

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
           my_server:reply(From, make_cat(Name, Color, Description)),
           Cats;
       Cats =/= [] ->
           my_server:reply(From, hd(Cats)),
           tl(Cats)
    end;
handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat | Cats].

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name = Name,
         color = Col,
         description = Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    exit(normal).
```

What we've just done is the core of OTP - taking all the generic components, extracting them into libraries, making sure they work well and then reusing that code whenever possible. All that's left is to focus on the things specific to the application.

For small applications like the kitty server, abstraction is for little benefit. For large applications it is worth it to separate generic parts from specific parts.

Adding servers adds complexity (code, testing, maintenance and understanding). By using common abstractions:

- people understand the basic concept of the module immediately (e.g. it's a server)
- there is a single generic implementation to test, document
- if someone optimises the generic implementation, every process benefits (which is what happens with OTP)
- unit testing modules are easier (e.g. no need to spawn a server, just test the `handle_call`)
- bugs are less likely from human error

Some things not explored in this section, but present in OTP `gen_server`:

- configuring timeouts
- adding debug information
- handling unexpected messages
- handling specific errors
- hot code loading
- abstracting away replies
- handling server shutdown
- working with supervisors

# Clients and Servers

## Generic Servers

`gen_server` is one of the most used OTP behaviours and has an interface similar to `my_server` from the previous section.

Your application is expected to implement some function's that `gen_server` will use.

`init`

- Initializes server state and do one-time tasks
- Can return `{ok, State}`, `{ok, State, TimeOut}`, `{ok, State, hibernate}`, `{stop, Reason}` or `ignore`
    - `State` will be passed directly to the main loop of the process as the state to keep later on
    - `TimeOut` is meant to be added to the tuple whenever you need a deadline before which you expect the server to receive a message. If no message is received before deadline, the atom `timeout` will be sent to the server (`handle_info/2` should take care of this)
    - `hibernate` can be returned if we expect the application process to take a long time before getting a reply - hibernation reduces the size of the process' state until it gets a message at the expense of some processing power
    - `stop` is returned when something went wrong during the initialization
- When `init/1` is running, execution is blocked in the process that spawned the server. It awaits a "ready" message to be sent automatically by the `gen_server`module

When the BIF `erlang:hibernate(M, F, A)` is called, the call stack for the currently running process is discarded (the function never returns). Garbage collection kicks in and what's left is one continuous heap that is shrunken to the size of the data in the process. Once the process receives a message, the function `M:F` with `A` as the arguments is called and the execution resumes.

`handle_call`

- Handles synchronous messages and takes in `Request`, `From` and `State`
- Can return `{reply,Reply,NewState}`, `{reply,Reply,NewState,Timeout}`, `{reply,Reply,NewState,hibernate}`, `{noreply,NewState}`, `{noreply,NewState,Timeout}`, `{noreply,NewState,hibernate}`, `{stop,Reason,Reply,NewState}` and `{stop,Reason,NewState}`
    - `Timeout` and `hibernate` works in the same way as `init/1`
    - `Reply` will be sent back to whoever called the server in the first place
    - `noreply` tells the `gen_server` that you will take care of sending the reply back yourself with `gen_server:reply/2`. This is done when you want another process to send the reply for you or when you want to send an acknowledgment but still process it afterwards

`handle_cast`

- Handles asynchronous messages and takes the parameters `Message` and `State`
- Can return `{noreply,NewState}`, `{noreply,NewState,Timeout}`, `{noreply,NewState,hibernate}` and
`{stop,Reason,NewState}`

`handle_info`

- Used to handle messages that do not fit interface, behaving similarly to `handle_cast` but only handles messaages that were sent directly with the `!` operator and special messages like `init/1`'s `timeout`, monitors' notifications and `'EXIT'` signals
    - Messages sent with `call` and `cast` do not result in a call to `handle_info`
- Returns the same tuples as `handle_cast`

`terminate`

- Called whenever one of the three `handle_*` functions returns a tuple of the form `{stop, Reason, NewState}` or `{stop, Reason, Reply, NewState}`, taking in the parameters `Reason` and `State` from the tuples
- Also called when its parent (the process that spawned the `gen_server`) dies, if and only if the `gen_server` is trapping exits
- If terminate is called with anything other than `normal`, `shutdown` or `{shutdown, Term}` is used when `terminate/2` is called, the OTP framework will see this as a failure and start logging a bunch of stuff
- This function should be seens as the opposite of `init/1`, closing or shutting down any resources initialized there
- The return value of this function doesn't matter because the code stops executing after it's been called

`code_change`

- Serves to upgrade code, taking the form `code_change(PreviousVersion, State, Extra)`. `PreviousVersion` is either the version term itself in the case of an upgrade or `{down, Version}` in the case of a downgrade
- Should return `{ok, NewState}` which should contain the transformed state to be passed into the target version

## Behaviours

A behaviour is basically a way for a module to specify functions it expects another module to have.

It is the contract sealing the deal between the well-behaved generic part of the code and the specific, error-prone part of the code.

Both `behaviour` and `behavior` are accepted by the Erlang compiler.

```erlang
-module(my_behaviour).
-export([behaviour_info/1]).
 
%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
behaviour_info(_) -> undefined.
```

## Using gen_server

`gen_server:start_link/3-4` , `gen_server:start/3-4`

- The first parameter is the callback module, the second is the list of parameters to pass to `init/1` and the third is for debug options. An optional fourth parameter can be placed in the first position indicating the name to register the server with
- Returns `{ok, Pid}`

`gen_server:call/2-3`

- A third parameter can be passed to call to give a timeout. If a timeout is not provided (or the timeout value is `infinity`), the default of 5 seconds is set. If no reply is received from the application before the time is up, the call crashes

`gen_server:cast/2`

```erlang
-module(kitty_gen_server).

-behaviour(gen_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(cat, {name, color = green, description}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) ->
    {ok, []}. %% no treatment of info here!

handle_call({order, Name, Color, Description}, _From, Cats) ->
    if Cats =:= [] ->
           {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] ->
           {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used.
    {ok, State}.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name = Name,
         color = Col,
         description = Desc}.
```

Usage of the kitty store implemented with `gen_server` is similar to the one we implemented without.

# Rage Against the Finite-State Machines

![images/Untitled%203.png](images/Untitled%203.png)

Cat FSM

![images/Untitled%204.png](images/Untitled%204.png)

Dog FSM

```erlang
-module(cat_fsm).
-export([start/0, event/2]).

start() ->
    spawn(fun() -> dont_give_crap() end).

event(Pid, Event) ->
    Ref = make_ref(), % won't care for monitors here
    Pid ! {self(), Ref, Event},
    receive
        {Ref, Msg} ->
            {ok, Msg}
    after 5000 ->
        {error, timeout}
    end.

dont_give_crap() ->
    receive
        {Pid, Ref, _Msg} ->
            Pid ! {Ref, meh};
        _ ->
            ok
    end,
    io:format("Switching to 'dont_give_crap' state~n"),
    dont_give_crap().
```

```erlang
-module(dog_fsm).
-export([start/0, squirrel/1, pet/1]).

start() ->
    spawn(fun() -> bark() end).

squirrel(Pid) ->
    Pid ! squirrel.

pet(Pid) ->
    Pid ! pet.

bark() ->
    io:format("Dog says: BARK! BARK!~n"),
    receive
        pet ->
            wag_tail();
        _ ->
            io:format("Dog is confused~n"),
            bark()
    after 2000 ->
        bark()
    end.

wag_tail() ->
    io:format("Dog wags its tail~n"),
    receive
        pet ->
            sit();
        _ ->
            io:format("Dog is confused~n"),
            wag_tail()
    after 30000 ->
        bark()
    end.

sit() ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    receive
        squirrel ->
            bark();
        _ ->
            io:format("Dog is confused~n"),
            sit()
    end.
```

## Generic Finite-State Machines

`gen_fsm` is a specialised version of `gen_server`. Instead of handling calls and casts, a `gen_fsm` handles synchronous and synchronous events. Each state is represented by a function.

The following callbacks must be implemented by the application to use the `gen_fsm` behaviour.

`init`

- Same as `init/1` as used for generic servers
- Can return `{ok, StateName, Data}`, `{ok, StateName, Data, Timeout}`, `{ok, StateName, Data, hibernate}` and `{stop, Reason}`
    - `stop`, `hibernate` and `Timeout` works in the same manner as `gen_server`
    - `StateName` is an atom and represents the next callback function to be called

`StateName`

- `StateName/2-3` are placeholder names for the application to decide. If `init/1` returns the tuple `{ok, sitting, dog}`, the finite state machine will be in a sitting state
    - This "state" is not the same kind of state as in `gen_server`, instead of dictating the context in which you will handle the next event
    - There's no limit on how many of these functions you can have as long as they are exported
- `StateName/2` is called for asynchronous events and take in `Event` (the actual message sent as an event) and `StateData` (the data carried over the calls). The event is sent with `send_event/2`
- `StateName/2` can return the tuples `{next_state, NextStateName, NewStateData}`, `{next_state, NextStateName, NewStateData, Timeout}`, `{next_state, NextStateName, NewStateData, hibernate}` and `{stop, Reason, NewStateData}`
- `StateName/3` is called for synchronous ones, except there is a `From` variable in between `Event` and `StateData`. `From` and `gen_fsm:reply/2` is used in the same way as it was for `gen_server`. The event is sent with `sync_send_event/2-3`
- `StateName/3` can return the following tuples

    ```erlang
    {reply, Reply, NextStateName, NewStateData}
    {reply, Reply, NextStateName, NewStateData, Timeout}
    {reply, Reply, NextStateName, NewStateData, hibernate}

    {next_state, NextStateName, NewStateData}
    {next_state, NextStateName, NewStateData, Timeout}
    {next_state, NextStateName, NewStateData, hibernate}

    {stop, Reason, Reply, NewStateData}
    {stop, Reason, NewStateData}
    ```

    - `NextStateName` will determine the next function called

`handle_event`

- To handle global events that would trigger a specific reaction no matter the state, use the `handle_event/3` callback. Such events are sent with `send_all_state_event/2`
- The function takes arguments similar to `StateName/2` with the exception that it accepts a `StateName` variable in between them, telling you what the state was when the event was received
- Returns the same values as `StateName/2`

`handle_sync_event`

- Synchronous version of `handle_event`. These events are sent with `sync_send_all_state_event/2-3`
- Takes the same parameters as `handle_event`
- Returns the same tuples as `StateName/3`

`code_change`

- Works exactly the same as it did for `gen_server`, except that it takes an extra parameter when called like `code_change(OldVersion, StateName, Data, Extra)`
- Returns a tuple of the form `{ok, NextStateName, NewStateData}`

`terminate`

- Acts like in generic servers. `terminate/3` should do the opposite of `init/1`

## A Trading System Specification

In this section, we implement a server where players speak and trade items. A broker is not used so that the system would be distributable.

In short, the following actions should be possible:

- ask for a trade
- accept a trade
- offer items
- retract an offer
- declare self as ready
- brutally cancel the trade

When each of these actions is taken, the other player's FSM should be made aware of it. Each player would talk to their FSM, which would talk to another player's FSM.

When dealing with two identical processes communicating, we want to avoid synchronous calls as much as possible. If A sends B a message while B sends a message to A, they could both end up waiting for the other without every replying, causing a deadlock.

We can wait for timeout, but there will be leftover messages in both processes' mailboxes.

The simplest way is to avoid synchronous messages altogether.

Note that in our problem, players can send synchronous messages to their FSMs because the FSM won't need to call the player and no deadlock can occur.

![images/Untitled%205.png](images/Untitled%205.png)

Overall flow

![images/Untitled%206.png](images/Untitled%206.png)

State transition diagram

Both FSMs are in an idle state. When you ask Jim to trade, Jim has to accept before things move on. Then both of you can offer items or withdraw them. When you are both declaring yourself ready, the trade can take place. This is a simplified version of all that can happen and we'll see all possible cases with more detail in the next paragraphs.

![images/Untitled%207.png](images/Untitled%207.png)

At first, both finite-state machines start in the idle state. At this point, one thing we can do is ask some other player to negotiate with us.

![images/Untitled%208.png](images/Untitled%208.png)

We go into idle_wait mode in order to wait for an eventual reply after our FSM forwarded the demand. Once the other FSM sends the reply, ours can switch to negotiate.

![images/Untitled%209.png](images/Untitled%209.png)

The other player should also be in negotiate state after this. Obviously, if we can invite the other, the other can invite us. 

![images/Untitled%2010.png](images/Untitled%2010.png)

In the event the other player asks to trade at the same time we ask to trade, both FSms switch to idle_wait. If an FSM receives an ask negotiate message while in the idle_wait state, we know that this race condition is hit and both FSMs can move into the negotiate state.

![images/Untitled%2011.png](images/Untitled%2011.png)

We must support users offering items and then retracting the offer. All this does is forward our client's message to the other FSM. Both FSMs will need to hold a list of items offered by either player, so they can update that list when receiving such messages. 

![images/Untitled%2012.png](images/Untitled%2012.png)

To officialize the trade, we have to synchronize both players by using an intermediate state (as we did for idle and idle_wait).

![images/Untitled%2013.png](images/Untitled%2013.png)

As soon as our player is ready, our FSM asks Jim's FSM if he's ready. Pending its reply, our own FSM falls into its wait state. The reply we'll get will depend on Jim's FSM state: if it's in wait state, it'll tell us that it's ready. Otherwise, it'll tell us that it's not ready yet. That's precisely what our FSM automatically replies to Jim if he asks us if we are ready when in negotiate state.

![images/Untitled%2014.png](images/Untitled%2014.png)

Our finite state machine will remain in negotiate mode until our player says he's ready. Let's assume he did and we're now in the wait state. However, Jim's not there yet. This means that when we declared ourselves as ready, we'll have asked Jim if he was also ready and his FSM will have replied 'not yet'.

![images/Untitled%2015.png](images/Untitled%2015.png)

While waiting after Jim, who's still negotiating by the way, it is possible that he will try to send us more items or maybe cancel his previous offers. As soon as he changes the items offered, we go back into the negotiate state so we can either modify our own offer, or examine the current one and decide we're ready.

![images/Untitled%2016.png](images/Untitled%2016.png)

At some point, Jim will be ready to finalise the trade too. When this happens, his finite-state machine will ask ours if we are ready. What our FSM does is reply that we indeed are ready. We stay in the waiting state and refuse to move to the ready state though.

![images/Untitled%2017.png](images/Untitled%2017.png)

Because of the way messages are recieved, we could possibly only process item offer after we declared ourselves ready after both parties declare themselves ready (see image). If Jim had automatically moved to ready, he'd be caught waiting as we were still in negotiate.

![images/Untitled%2018.png](images/Untitled%2018.png)

When we receive ready from the other FSM, we send ready back again. This will create a superfluous ready message in one of the two FSMs, but we will just ignore it. We then send an ack message before moving to ready state.

![images/Untitled%2019.png](images/Untitled%2019.png)

In the ready state, we then use a simplified version of a two-phase commit. The ack event is used to kickstart the two-phase commit.

Finally, we have to allow the trade to be cancelled at any time. This means that somehow, no matter what state we're in, we're going to listen to the "cancel" message from both sides and quit the transaction.

```erlang
-module(trade_fsm).
-behaviour(gen_fsm).
-record(state, {name = "", other, ownitems = [], otheritems = [], monitor, from}).

%% public API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2,
         ready/1, cancel/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4, idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2,
         ready/2, ready/3]).

%%% PUBLIC API
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% ask for a begin session. Returns when/if the other accepts
trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% Accept someone's trade offer.
accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% Send an item on the table to be traded
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Cancel trade offer
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that you're ready for a trade. When the other
%% player also declares being ready, the trade is done
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the transaction.
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%% Ask the other FSM's Pid for a trade session
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% forward a client's offer
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% forward a client's offer cancellation
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% Ask the other side if he's ready to trade.
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

%% Reply that the side is not ready to trade
%% i.e. is not in 'wait' state.
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

%% Tells the other fsm that the user is currently waiting
%% for the ready state. State should transition to 'ready'
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

%% Acknowledge that the fsm is in a ready state.
ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

%% ask if ready to commit
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

%% begin the synchronous commit
do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).

%%% GEN_FSM CALLBACKS
init(Name) ->
    {ok, idle, #state{name = Name}}.

idle({ask_negotiate, OtherPid}, S = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state,
     idle_wait,
     S#state{other = OtherPid,
             monitor = Ref,
             from = From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
%% The other side has accepted our offer. Move to negotiate state
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};
%% Own side retracting an item offer
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};
%% other side offering an item
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
%% other side retracting an item offer
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
    notice(S, "Other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(S,
           "Other user ready to transfer goods:~n"
           "You get ~p, The other side gets ~p",
           [S#state.otheritems, S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from = From}};
negotiate(Event, _From, S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "Other side cancelling offer of ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
wait(are_you_ready, S = #state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready, and I am. Waiting for same reply", []),
    {next_state, wait, S};
wait(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait('ready!', S = #state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side is ready. Moving to ready state", []),
    {next_state, ready, S};
%% Don't care about these!
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

ready(ack, S = #state{}) ->
    case priority(self(), S#state.other) of
        true ->
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "committing...", []),
                commit(S),
                {stop, normal, S}
            catch
                Class:Reason ->
                    %% abort! Either ready_commit or do_commit failed
                    notice(S, "commit failed", []),
                    {stop, {Class, Reason}, S}
            end;
        false ->
            {next_state, ready, S}
    end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

ready(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "committing...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%%% PRIVATE FUNCTIONS
%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(#state{name = N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N | Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

%% adds an item to an item list
add(Item, Items) ->
    [Item | Items].

%% remove an item from an item list
remove(Item, Items) ->
    Items -- [Item].

priority(OwnPid, OtherPid) when OwnPid > OtherPid ->
    true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid ->
    false.

commit(S = #state{}) ->
    io:format("Transaction completed for ~s. "
              "Items sent are:~n~p,~n received are:~n~p.~n"
              "This operation should have some atomic save "
              "in a database.~n",
              [S#state.name, S#state.ownitems, S#state.otheritems]).

%% The other player has sent this cancel event
%% stop whatever we're doing and shut down!
handle_event(cancel, _StateName, S = #state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason},
            _,
            S = #state{other = Pid, monitor = Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%% Transaction completed.
terminate(normal, ready, S = #state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.
```

Notes

- The client calls the fsm synchronously (mostly), but the other FSM does it asynchronously. Having the client synchronous simplifies logic by limiting the contradicting messages that can be sent one after the other.
- There'll be a few out of band messages that could be a result of race conditions. It's usually safe to ignore them but we can't easily get rid of them.
- Using asynchronous messages on both sides for `negotiate` result in some difficulty differentiating between player-to-FSM and FSM-to-FSM communications.
- When in the ready state, both players' actions become useless (except cancelling). We won't care about new item offers. To begin this commit without either player acting, we'll need the ack event to trigger an action from the FSMs.
- `try ... catch`: if the replying FSM dies or its player cancels the transaction, the synchronous calls will crash after a timeout. The commit should be aborted in this case.
- Even if the cancel or DOWN events happen while we're in the commit, everything should be safe and nobody should get its items stolen.

The pids of any process can be compared to each other and sorted. This can be done no matter when the process was spawned, whether it's still alive or not, or if it comes from another VM (we'll see more about this when we get into distributed Erlang).

Subtle concurrency bugs and race conditions can often rear their ugly heads a long time after they were written, and even if they've been running for years.

# Event Handlers

In our previous reminder app and trading system, we needed to notify a client process/application about an event that happened at some point of time.

Using subscriber processes is pretty useful when each of the subscribers has a long-running operation to do after receiving an event. In simpler cases, where you do not necessarily want a subscriber process to be on standby, another approach is to use an event manager process.

The event manager accepts functions and lets them run on any incoming event. This has the following advantages:

- If there are many subscribers, the server can keep going because it only needs to forward events once (to the event manager)
- If there is a lot of data to be transferred, it's only done once and all callbacks operate on the same instance of the data (in the event manager)
- You don't need to spawn subscriber processes to handle short lived tasks

Disadvantages:

- If all functions need to run for a long time, they will block each other. In this case, the event manager can forward the event to a process, turning it into an event forwarder
- In fact, a function that loops indefinitely can prevent any new event from being handled until something crashes

## Generic Event Handlers

Unlike `gen_server` and `gen_fsm`, you never need to start a process with the `gen_event` behaviour. It instead runs the process that accepts and calls functions, and you only provide a module with these functions. All managing is done for free and you only provide what's specific to your application.

- In other words, the `gen_event` behaviour represents a single event handler. The event manager server does not need to be implemented, and is simply started with `gen_event:start_link()` (this can be wrapped in an abstractino module). Event handlers implementing the `gen_event` behaviour are then registered with this started process.

Instead of the `spawn -> init -> loop -> terminate` pattern, we have `spawn event manager -> attach handler -> init handler -> loop -> exit handlers`.

Each event handler can hold its own state, carried around by the manager for them. Each event handler does `init -> handle messages -> terminate`.

`init` and `terminate`

- Similar to what we've seen in the previous behaviours
- `init/1` takes a list of arguments and returns `{ok, State}`. What happens in `init/1` should have its counterpart in `terminate/2`

`handle_event`

- `handle_event(Event, State)` is the core of `gen_event`'s callback modules. It works like `gen_server`'s `handle_cast/2` in that it is asynchronous.
- It returns `{ok, NewState}`, `{ok, NewState, hibernate}` (which puts the event manager itself into hibernation until the next event), `remove_handler` and `{swap_handler, Args1, NewState, NewHandler, Args2}`
    - `{ok, NewState}` works in a way similar to what we've seen with `gen_server:handle_cast/2` - it updates its own state and doesn't reply
    - `{ok, NewState, hibernate}` the whole event manager is going to be put in hibernation (remember that event handlers run in the same process as the event manager)
    - `remove_handler` drops the handler from the manager
    - `{swap_handler, Args1, NewState, NewHandler, Args2}` is not used frequently. It removes the current event handler and replaces it with a new one by calling `NewHandler:init(Args2, ResultFromTerminate)`. This can be useful in the cases where you know some specific event happened and you want to give control to a new handler.
- All incoming events can come from:
    - `gen_event:notify/2` (asynchronous like `gen_server:cast/2`)
    - `gen_event:sync_notify/2` (which is synchronous). Note that `handle_event/2` remains asynchronous - the idea here is that the function only returns once all event handlers have seen and treated the new message. Until then, the event manager will keep blocking the calling process by not replying.

`handle_call`

- Similar to `gen_server`'s `handle_call`. As there are multiple event handlers, we do not expect all of them to reply. We are forced to choose only one handler to reply.
- Can return `{ok, Reply, NewState}`, `{ok, Reply, NewState, hibernate}`, `{remove_handler, Reply}` or `{swap_handler, Reply, Args1, NewState, Handler2, Args2}`.
- `gen_event:call/3-4` is used to make the call.

`handle_info`

- Similar to `handle_event`, with the exception that it only treats out of band messages, such as exit signals, messages sent directly to the event manager with the `!` operator. Use cases similar to `handle_info` in `gen_server`/`gen_fsm`
- Same return values as `handle_event`.

`code_change`

- Works in exactly the same manner as it does for `gen_server`, except it's for each individual event handler. It takes 3 arguments `OldVsn`, `State` and `Extra`.
- Returns `{ok, NewState}`.

## Curling Example

In this section, we're making a set of event handlers used to track game updates for curling.

In curling, there are two teams and they send a curling stone as close to the center of a red circle. They do this with 16 stones and the team with the stone closest to the center wins a point at the end of the round. There are 10 rounds and the team with the most points at the end of the rounds wins the game.

In this problem, we program a system that will let some official enter game events, such as when a stone has been thrown, when a round ends or when a game is over, and then route these events to a scoreboard/stats system/reporter feeds.

```erlang
-module(curling_scoreboard_hw).
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).

%% This is a 'dumb' module that's only there to replace what a real hardware
%% controller would likely do. The real hardware controller would likely hold
%% some state and make sure everything works right, but this one doesn't mind.

%% Shows the teams on the scoreboard.
set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
    io:format("Scoreboard: round over~n").

add_point(Team) ->
    io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).

reset_board() ->
    io:format("Scoreboard: All teams are undefined and all scores are 0~n").
```

```erlang
-module(curling_scoreboard).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

init([]) ->
    {ok, []}.

handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

This is a skeleton that we can use for every `gen_event` callback module. For now, the scoreboard event handler itself won't need to do anything special except forward the calls to the hardware module.

The events will come from `gen_event:notify/2` so the handling of the protocol should be done in `handle_event/2`.

```erlang
%% this event handler simply forwards events to hardware
handle_event({set_teams, TeamA, TeamB}, State) ->
    curling_scoreboard_hw:set_teams(TeamA, TeamB),
    {ok, State};
handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
    {ok, State};
handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.
```

```erlang
1> c(curling_scoreboard_hw).
{ok,curling_scoreboard_hw}
2> c(curling_scoreboard).
{ok,curling_scoreboard}
3> {ok, Pid} = gen_event:start_link().
{ok,<0.43.0>}
4> gen_event:add_handler(Pid, curling_scoreboard, []).
ok
5> gen_event:notify(Pid, {set_teams, "Pirates", "Scotsmen"}).
Scoreboard: Team Pirates vs. Team Scotsmen
ok
6> gen_event:notify(Pid, {add_points, "Pirates", 3}).
ok
Scoreboard: increased score of team Pirates by 1
Scoreboard: increased score of team Pirates by 1
Scoreboard: increased score of team Pirates by 1
7> gen_event:notify(Pid, next_round).
Scoreboard: round over
ok
8> gen_event:delete_handler(Pid, curling_scoreboard, turn_off).
ok
9> gen_event:notify(Pid, next_round).
ok
```

In the above:

- We're starting `gen_event` as a standalone process.
- We attach our event handler dynamically with `gen_event:add_handler/3`. This can be done as many times as we need, however, this might cause problems when we want to work with a particular event handler in `handle_call`. If you want to call, add or delete a specific handler when there's more than one instance of it, you'll have to find a way to uniquely identify it (e.g. `make_ref()`, then add it by calling `gen_event:add_handler(Pid, {Module, Ref}` and thereafter using `{Module, Ref}` to talk to the specific handler ).
- We then send messages to the event handler, which successfully calls the hardware module.
- We then remove the handler. `turn_off` is an argument to `terminate/2`, which our current implementation ignores.
- The handler is gone, but we can still send events to the event manager.

Instead of calling the `gen_event` module directly, we can write an abstraction module that wraps what we need. This hides the implementation and also lets us specify what handlers are necessary to include for a standard curling game.

```erlang
-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1]).

start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
    gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) ->
    gen_event:notify(Pid, next_round).
```

Now that we've got the basic scoreboard done, we want international reporters to be able to get live data from our official in charge of updating our system. Each news organization will register their own handler that just forwards them the data they need.

We add the following to our `curling.erl` module:

```erlang
%% Subscribes the pid ToPid to the event feed.
%% The specific event handler for the newsfeed is
%% returned in case someone wants to leave
join_feed(Pid, ToPid) ->
    HandlerId = {curling_feed, make_ref()},
    gen_event:add_handler(Pid, HandlerId, [ToPid]),
    HandlerId.

leave_feed(Pid, HandlerId) ->
    gen_event:delete_handler(Pid, HandlerId, leave_feed).
```

Joining the feed is done by inputting the right Pid for the event manager and the Pid to forward all the events to. This then returns a unique value that can then be used to unsubscribe from the feed.

`{curling_feed, make_ref()}` is necessary because all news organizations will use the `curling_feed` handler, and as such a unique ref is necessary to distinguish each instance of `curling_feed` from the others.

```erlang
-module(curling_feed).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

%% this handler's state is just the new's organization's feed's Pid
init([Pid]) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! {curling_feed, Event},
    {ok, Pid}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

```erlang
1> c(curling), c(curling_feed).
{ok,curling_feed}
2> {ok, Pid} = curling:start_link("Saskatchewan Roughriders", "Ottawa Roughriders").
Scoreboard: Team Saskatchewan Roughriders vs. Team Ottawa Roughriders
{ok,<0.165.0>}
3> HandlerId = curling:join_feed(Pid, self()).
{curling_feed,#Ref<0.0.0.909>}
4> curling:add_points(Pid, "Saskatchewan Roughriders", 2).
Scoreboard: increased score of team Saskatchewan Roughriders by 1
ok
Scoreboard: increased score of team Saskatchewan Roughriders by 1
5> flush().
Shell got {curling_feed,{add_points,"Saskatchewan Roughriders",2}}
ok
6> curling:leave_feed(Pid, HandlerId).
ok
7> curling:next_round(Pid).
Scoreboard: round over
ok
8> flush().
ok
```

We added ourselves to the feed, got the updates, then left and stopped receiving them.

Consider what would happen if one of the curling feed subscribers crashes. Do we keep the handler going on there? Ideally, we wouldn't have to.

We change the call from `gen_event:add_handler/3` to `gen_event:add_sup_handler/3`, and now when the feed subscriber crashes, the handler is gone. 

- Note that the curling feed subscriber is also the caller of `gen_event:add_handler.`

On the opposite end, if the event manager crashes, the message `{gen_event_EXIT, Handler, Reason}` is sent back to the you (the curling feed subscriber).

Whenever the caller uses `gen_event:add_sup_handler/3`, a link is set up between the caller and the event manager so both of them are supervised. `gen_event` predates monitors in Erlang and a commitment to backward compatibility introduces a bad wart. Because you could have the same process acting as the parent of many event handlers, the library doesn't ever unlink the processes except when they terminate for good.

This means that everything goes alright when:

- Your own process crashes: the supervised handler is terminated (with the call to `terminate({stop, Reason}, State)`).
- Your handler itself crashes (but not the event manager): you will recieve `{gen_event_EXIT, HandlerId, Reason}`.

When the event manager is shut down though, you will either:

- Receive `{gen_event_EXIT, HandlerId, Reason}` then crash because you're not trapping exits.
- Receive `{gen_event_EXIT, HandlerId, Reason}` then a standard `EXIT` message that is (possibly?) superfluous.

Switching the event handler to a supervised one is thus safer but risks being annoying in some cases. Safety should come first.

Retracing back to the problem, we want to add an additional event handler `curling_accumulator` to accumulate stats for any late feed subscribers. We add/modify the following interface functions to `curling`:

```erlang
start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    %% Start the stats accumulator
    gen_event:add_handler(Pid, curling_accumulator, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.

%% Returns the current game state.
game_info(Pid) ->
    gen_event:call(Pid, curling_accumulator, game_data).
```

Note that `game_info/1` uses only `curling_accumulator` as a handler id as there's only ever going to be one instance of the handler. Also note that `curling_accumulator` starts automatically like the scoreboard.

```erlang
-module(curling_accumulator).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {teams = orddict:new(), round = 0}).

init([]) ->
    {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S = #state{teams = T}) ->
    Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
    {ok, S#state{teams = Teams}};
handle_event({add_points, Team, N}, S = #state{teams = T}) ->
    Teams = orddict:update_counter(Team, N, T),
    {ok, S#state{teams = Teams}};
handle_event(next_round, S = #state{}) ->
    {ok, S#state{round = S#state.round + 1}};
handle_event(_Event, Pid) ->
    {ok, Pid}.

handle_call(game_data, S = #state{teams = T, round = R}) ->
    {ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

`curling_accumulator` should be able to hold state for the curling game: so far we have teams, score and rounds to track. This will be held in a state record, changed on each event received. Finally, we will only need to reply to the `game_data` call, as above.

```erlang
1> c(curling), c(curling_accumulator).
{ok,curling_accumulator}
2> {ok, Pid} = curling:start_link("Pigeons", "Eagles").
Scoreboard: Team Pigeons vs. Team Eagles
{ok,<0.242.0>}
3> curling:add_points(Pid, "Pigeons", 2).
Scoreboard: increased score of team Pigeons by 1
ok
Scoreboard: increased score of team Pigeons by 1
4> curling:next_round(Pid).
Scoreboard: round over
ok
5> curling:add_points(Pid, "Eagles", 3).
Scoreboard: increased score of team Eagles by 1
ok
Scoreboard: increased score of team Eagles by 1
Scoreboard: increased score of team Eagles by 1
6> curling:next_round(Pid).
Scoreboard: round over
ok
7> curling:game_info(Pid).
{[{"Eagles",3},{"Pigeons",2}],{round,2}}
```

We are done with our curling scoring system. Note that the most common use of event handlers are actually to do with logging and system alarms.

At this point, we've looked at the main OTP behaviours used in active code development. The remaining behaviours left to visit include those that act as glue to all of the worker processes.

# Who Supervises The Supervisors?

# Outstanding Reading List

Dialyzer Paper

Foldl vs Foldr

BEAM

escript

# Tools

Dialyzer: Static Type Checker

escript: Erlang scripts