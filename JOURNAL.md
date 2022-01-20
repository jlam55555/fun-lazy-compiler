# JOURNAL.md

This is a log of the work that has been accomplished here. In other words, a running set of notes, including but not limited to: work that was done, new revelations, general things learned, questions, interesting design decisions (from the tutorial), interesting design decisions (deviating from the tutorial), etc. I've found that this has worked well for keeping me on track with other projects (namely, previous compiler, and my Master's thesis), so I will spend the time to do it here as well.

### 01/20/22
- Begin journal.
- Begin reading tutorial: read through preface and about half of Chapter 1. Also implemented Appendix A, the `Utils` module. Luckily, Miranda is almost exactly the same syntactically as Haskell, so no confusion (yet).
  - The abstraction/implementation for a data heap is interesting. We basically have an infinite stream of `Addr`s (integers), so they are all unique and originally free. To allocate, we simply draw from this infinite stream; to free, we return the `Addr` to the infinite stream (prepending to an infinite stream -- woah!).
  - Some things in appendix are in comomn Haskell modules, e.g., `Data.Set` or `Data.Sort`, so don't need to implement them manually.
  - *Question*: What does `cts` stand for in the heap implementation?
  - *Question*: Can the name generation for lambda lifting generate duplicate/taken names?
- Notes from preface:
  - The Core language (the language to implement) is meant to be a very minimal functional language: stripping out most of the fancy details (mostly syntactical), but leaving behind all of the core ideas in a high-level functional language. I.e., we should be able to rewrite most any high-level functional program into an equivalent Core program, perhaps at the cost of concision and legibility.
  - Core program is a set of **supercombinators** (a top-level binding, which must be fully bound and self-contained), including the distinguished supercombinator `main`.
    - Supercombinators without arguments (such as `main`) are called **constant applicative forms** (CAFs) and require special treatment.
- Local definitions are allowed using the `let` and `letrec` forms. These do not allow pattern matching (and thus do not allow local functions to be defined.)
- Functions can only be defined as top-level supercombinators.
  - We will introduce two concepts in Chapter 6 called **lambda abstractions** and **lambda lifting**. Lambda abstractions are anonymous lambda functions, which are a convenient syntactical form. These will be converted to supercombinators through the process known as lambda lifting.
  - *Question*: Why (technically) can't we have local functions?
- **Structured data types** (a.k.a., **algebraic data types** (ADTs))
  - Sum types, product types, polymorphic types, and combinations of these (same as in Haskell or other languages)
  - Need a simple representation, so we don't allow for user-defined constructors; only have a single form to introduce ADTs: `Pack{tag, arity}`. Tag is used to differentiate variants in sum types, and arity is used to indicate the number of arguments for a particular variant type (tag).
    - Assuming that a program is well-typed, we only need to make tags unique between variants of a single sum type (rather than making them globally unique).
- Case expressions are used to destructure ADTs.
  - Outlaws all but the simplest pattern matching with a single variable as the scrutinee, and only tag numbers and variables (that match the tag arity) are allowed. No:
    - Nested patterns
    - Overlapping patterns
    - Guards
- Syntax of the Core language:
  - *Question*: Why is everything right-associative? And why can't we define associativity of `/` and `-`?
  - No unary operators. `negate` and `not` are provided as functions
  - BNF grammar for the Core language is located on [page 18 of the tutorial](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/01/student.pdf#page=19)
  - We have the following operator precedence/associativity table:
  
| Precedence | Associativity | Operator                         |
|------------|---------------|----------------------------------|
| 6          | left          | ` ` (function application)       |
| 5          | right         | `*`                              |
| 5          | none          | `/`                              |
| 4          | right         | `+`                              |
| 4          | none          | `-`                              |
| 3          | none          | `==`, `~=`, `>`, `>=`, `<`, `<=` |
| 2          | right         | `&`                              |
| 1          | right         | `|`                              |

- Data types for the Core languague:
  - The book derives the `Text` typeclass for the `Expr` datatype; [this has long been replaced by the `Read` and `Show` typeclasses](https://www.haskell.org/definition/from12to13.html#newtext).
  - The data type `Expr` is parameterized w.r.t. its **binders**. Binders are the type used to represent a variable. For our purposes, we will mostly use `Name` as the binding type. In Chapter 6, we will see types other than name in the binding position.
    - (Saw this in EECS490 with de Bruijin indices, where the binders are numbers indicating the depth of a variable binding, so that two structurally equal de Bruijin types are alpha-equivalent)
- Random thoughts/realizations:
  - I finally understand what people mean when they say that (Haskell's) typeclasses are more powerful than (Java's) interfaces. Typeclasses allows one to define functions that are universally quantified *over multiple types*, not just the receiving type. You also get non-functions (e.g., constants or CAFs) in typeclasses. Typeclasses also allow for the visitor pattern (not being defined on the typeclass itself, but on all implementations of it). See [this SO answer](https://stackoverflow.com/a/6948534/2397327) -- I finally understand it now.
  - I am starting to get a grasp of the higher-level type concepts, usually enabled via extensions:
    - `MultiParamTypeClasses`: like type classes and multiple dispatch (multiple receivers)
    - `TypeApplications`: explicitly specifying the concrete type of a polymorphic function
    - `RankNTypes`: universally-quantified polymorphism over part of a type declaration (in other words, multiple "levels" of universally-quantified polymorphism, extending prenex (rank-1) polymorphism)
    - `TypeFamilies`: polymorphism at the type constructor level; each instance (type) of a type family can have different type constructor implementations, just as each instance (type) of a typeclass can have different method implementations
  - On the other hand, not too familiar with a lot of other common extensions, partially due to just not having enough time to spend understanding them:
    - `GADT`: what does the "generalized" mean in generalized algebraic data types?
    - `ExistentialQuantification`: I understand universal polymorphism, and nested universal polymorphism (rank-n types) but not this; why does this also use the `forall` keyword (universal quantification also uses this?)
    - I found this: [24 Days of GHC Extensions](https://blog.ocharles.org.uk/blog/posts/2014-12-01-24-days-of-ghc-extensions.html) (a blog series)
  - I could really use a faster computer for my my Haskell/OCaml work. Even Emacs is slow (but that is probably partially due to some combination of my extensions) :(
