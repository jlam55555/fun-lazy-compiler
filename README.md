# fun-lazy-compiler (flc)
A (simulated) compiler for a lazy functional language, following the tutorial by Simon Peyton Jones

See tutorial (for [students][student.pdf]; for [tutors][tutor.pdf])

### Implementation overview
The implementation language of choice is Haskell. It is not very different from the language used in the book (Miranda), and has a larger community due to its longer open-source and academic development. Like the tutorial's use of Miranda (or my use of C17 to implement a compiler for a limited subset of ANSI C for a previous project), I do not find it detrimental to use a more powerful language than the Core language (the language to be implemented) to implement the Core language; rather, this will make less important details less tedious.

The following diagram is taken from the tutorial's preface.
![Implementation overview][implementation_overview.png]

[student.pdf]: https://www.microsoft.com/en-us/research/wp-content/uploads/1992/01/student.pdf
[tutor.pdf]: https://www.microsoft.com/en-us/research/uploads/prod/1992/01/tutor.pdf
[implementation_overview.png]: ./res/implementation_overview.png
