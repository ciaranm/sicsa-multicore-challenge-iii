Some results for the DIMACS instances. The format of the files is:

    size number_of_colourings
    vertices in the result
    runtime_in_ms

The runtime is from a bit-parallel, non-threaded C++ implementation running on
a Xeon E5-4650 v2, and should be viewed as an "order of magnitude" kind of
figure.

A few DIMACS instances are missing: these are either open problems, or problems
where only a parallel runtime is known, or a problem which has only been solved
using a special algorithm.
