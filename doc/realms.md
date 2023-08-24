# Realms

Realms are objects that describe the shape of data.  First and
foremost, they can serve as documentation, describing the data flowing
through your programs, similarly to types.  (Realms are values though,
of course.)

Thus, they are in a similar space as
[`clojure.spec`](https://clojure.org/about/spec),
[Schema](https://github.com/plumatic/schema), and
[Malli](https://github.com/plumatic/schema), and we would have loved
to avoid creating an alternative.  However, we couldn't make any
existing system meet all of our requirements:

- support tagged records
- support mixed data aka "sum types"
- function as a description that can be inspected by a program
- enable automatic creation of generators for property-based testing
- fast predicates for data classification
- efficient validation

Ultimately, you'll see that the existing systems focus much of their
support on data-as-keyword-keyed maps, which is not a good match for
the design-recipes-style development that we favor.  (For some
background on our motivation, see [this
video](https://www.youtube.com/watch?v=MVThzZf-tJ4&t=778s).)

