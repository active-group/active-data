# Introduction to Active Data

Clojure endorses [data-oriented
programming](https://clojureverse.org/t/review-what-is-data-oriented-programming/6065) -
the idea that all structured data should be represented as maps with
public keys.

This is great for introspection and makes it very easy to access data,
but it also comes at a cost: an impoverished idea of domain modelling
that encourages sloppiness, loss of abstraction, and hidden coupling.

This library enables a style of programming based on systematic data
modelling, recovering what is lost with data-oriented programming.
The central ideas are from [How to Design
Program](https://htdp.org/)'s design recipes.  (For similar material in our
native German, check out
[DeinProgramm!](https://www.deinprogramm.de/).)

The central idea with design recipes is to distinguish between *compound
data* and *mixed data*.

Compound data refers to values that have several properties, or
consist of several parts.  ("A position on the plane consists of X and
Y coordinates." "An address has name, street, zip code, and city.")

Mixed data refers to values that are mixed together from different
kinds of data.  ("An animal can be an armadillo or a parrot."  "An
article can be a fruit or a vegetable.")

Note the contrasting use of "and" and "or" in the examples.

As Clojure is dynamically typed, we don't need to do anything special
to mix data.  With compound data, we need something in the language to
package data up.  Clojure natively has two ways of doing that, maps
and records, where records are special cases of maps.

Both are [suboptimal](#gripes) for implementing design recipes.  This
is where Active Data comes in, which offers a superior replacement for
Clojure's records,[`active.data.record`](active.data.record.html).

Our records have lightweight notation, and are tagged, so instances of
one record can always be distinguished from other types of data.  This
specifically enables a common patterrn in design recipes, where data
is mixed from several kinds of compound data.  Here's an example:

An animal is one of the following:
- An armadillo, which is dead or alive, AND weight -OR-
- A (talking) parrot, which has a sentence, AND weight.

This translates to record definitions as follows:

```clojure
(def-record dillo [dillo-liveness dillo-weight])
(def-record parrot [parrot-sentence parrot-weight])
```

Coincidentally, this also corresponds to algebraic data types in
ML-like languages.

The `weight` field, which is common to both record types, can also be
lifted into a common record base type through record *extension*:

```clojure
(def-record animal [animal-weight])
(def-record dillo :extends animal [dillo-liveness])
(def-record parrot :extends animal [parrot-sentence])
```

Active Data comes with another major component, [realms](realms.html).
Realms are a language for describing data, consistent with the ideas
of Active Data and design recipes.  As such, they are a replacement
for [`clojure.spec`](https://clojure.org/about/spec), and support
validation and user uses.

## <a name="gripes"></a> Our Gripes with Data-Oriented Programming

Why Active Data you might wonder - what's so bad about data-oriented
programming?  This section has some details, along with how Active
Data addresses those gripes.

The central technique of data-oriented programming is to use "maps for
everything", and to make the keys (namespaced) keywords, making them
inherently inspectable.  

This is a poor fit for design recipes, as it supports compound data,
but not mixed data. 

To illustrate, in the above example, we could represent armadillos
like so:

```clojure
(def dillo {:animal/animal-weight 8, :animal/dillo-liveness true})
(def parrot {:animal/animal-weight 1, :animal/parrot-sentence "hello, world" })
```

This is very lightweight - no need to define a record type.  But this
representation has several drawbacks:

1. There's no standard way to tell whether we're looking at an
   armadillo or a parrot.  (We've added `dillo-` to the liveness
   attribute, but might rename it later when other animals also gain
   this attribute.)
   Sure, we could add a special `:type` entry, but it's unlikely all
   parts of our software could agree on a common convention.
  
2. Clojure makes it very easy to make mistakes dealing with keyword
   maps: If you make a typo such as `(::animal/armadillo-liveness
   dillo)`, Clojure does not raise an exception but just returns
   `nil` - with the result of the error silently propagating through
   the program.
  
3. As there's no way to restrict access to keyworded map entries, code
   using maps is inherently coupled to a concrete representation,
   making the code architecturally brittle and hard to refactor.

4. There's no way to prevent maps floating around your program that
   have invalid/unexpected values attached to certain keys.  You can
   validate using `clojure.spec`, but the program needs to re-validate
   deeply every single time it encounters such a map.  This is not
   practical.
  
Clojure's [defrecord](https://clojuredocs.org/clojure.core/defrecord)
doesn't really address most of these problems: Records are still
keyworded maps.  While records are distinguishable via `instance?`,
`assoc` and `dissoc` are unrestricted, meaning that `instance?`
doesn't really tell you the structure of the record you're looking at.
Moreover, `defrecord` has issues with AOT compilation, making it
primarily useful as a Java interop mechanism.

Active Data's records address the above issues as follows:

1. The [`is-a?`](active.data.record.html#var-is-a.3F`) predicate is
   robust, as records always have exactly the keys in the `def-record`
   declaration.

2. While records can be treated as maps (which you can, and possibly
   should avoid), the keys are first-class objects.  Access to those
   keys can be restricted via Clojure's private annotation.
   
3. Record fields work as lenses, so there's no need to expose the fact
   that records are maps.
   
4. Records enable Yaron Minsky's ["make illegal states
   unrepresentable"](https://blog.janestreet.com/effective-ml-revisited/)
   doctrine: You can (and should) implement a validating constructor,
   or specify realms for the record fields, to make sure that only
   valid records are created.  If your program then encounters a
   record, there's no need to validate its contents.
