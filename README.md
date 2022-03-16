# Fun Lazy Compiler (`flc`)
A (simulated) compiler for a lazy functional language, following the tutorial by Simon Peyton Jones

See tutorial (for [students][student.pdf]; for [tutors][tutor.pdf])

---

### Usage
```text
fun-lazy-compiler -- a compiler and runtime for the Core lazy functional
language

Usage: flc [FILES] [-v|--verbose]
  Compile and evaluate a program in the Core language

Available options:
  FILES                    Source file(s)
  -v,--verbose             Print extra debugging information
  -h,--help                Show this help text
```

Source files may be a path to a file or `-` to read from stdin. If no source files are specified, `flc` will read a from stdin until EOF.

---

### Build instructions
This project uses the Haskell [Stack][stack] build environment. To build or run tests:
```bash
$ stack build       # build compiler binary
$ stack run -- FILE # run compiler binary
$ stack test        # run unit tests
```

You may also install the binary to your path, like so:

```bash
$ stack install     # install to PATH (e.g., `~/.local/bin`)
$ flc FILE          # run compiler binary
$ make uninstall    # uninstall from PATH
```

All Haskell code is auto-formatted using the [brittany][brittany] formatter.

---

### Examples
Sample source files are listed in the `examples/` directory.

```bash
$ flc nats.core map.core pe1.core
233168
```

---

### Journal
<strike>See [JOURNAL.md][JOURNAL.md] for a running set of notes on this project.</strike> Note: currently out-of-date.

---

### Implementation overview
The implementation language is Haskell. Tests are run against the `latest` GHC, Cabal, and resolver versions -- see [the test specification][test-spec] for more details on the Haskell build environment.

The following diagram is taken from the tutorial's preface.
![Implementation overview][implementation_overview.png]

The following diagram is Figure 1.1: the (BNF) grammar of the Core language.
![Core language grammar][grammar.png]

[test-spec]: ./.github/workflows/test.yml
[stack]: https://docs.haskellstack.org/en/stable/README/
[student.pdf]: https://www.microsoft.com/en-us/research/wp-content/uploads/1992/01/student.pdf
[tutor.pdf]: https://www.microsoft.com/en-us/research/uploads/prod/1992/01/tutor.pdf
[implementation_overview.png]: ./res/implementation_overview.png
[grammar.png]: ./res/bnf_grammar.png
[JOURNAL.md]: ./JOURNAL.md
[brittany]: https://hackage.haskell.org/package/brittany
