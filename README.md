# chickn

![clojure.yml](https://github.com/kongeor/chickn/actions/workflows/clojure.yml/badge.svg)
[![Clojars Project](https://img.shields.io/clojars/v/com.github.kongeor/chickn.svg)](https://clojars.org/com.github.kongeor/chickn)

Evolutionary algorithms library for Clojure(script)


## Install

Add the following dependency to your `project.clj`:

```
[com.github.kongeor/chickn "0.1.105"]
```

or: 

```
com.github.kongeor/chickn {:mvn/version "0.1.105"}
```

if you are using `deps.edn`

## Usage

In the following example we will try to solve one of the most trivial problems to understand the concepts of
the library:

First we need to create a function that will randomize ones and zeros. This will be used for the initial population
but also for our mutation function:

```clojure
(def one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0)))
```

Let's define a population size:

```clojure
(def population-size 20)
```

The `chromo-gen` function is used to create the initial population:

```clojure
(def chromo-gen #(repeatedly population-size one-or-zero))
```

Fitness is the function that assigns a score to each possible solution. In this case it's just the sum of
all numbers. Fitness for solution `[0 0 1 1 0]` would be 2, for `[0 0 1 1 1]` 3 etc.

```clojure
(defn fitness [xs]
  (apply + xs))
```

In some cases we may be able to define a function that determines if the problem is solved, which will allow us
to avoid wasting iteration cycles when we have found the solution we are looking for. This is an optional key.

```clojure
(defn solved? [_ {:keys [best-chromosome]}]
  (every? #(= 1 %) best-chromosome))
```

We need to customize the build-in `rand-mutation` operator and specify the mutation function, which is the
same we used for initializing the population. This is suboptimal, we could have just flipped the bit here,
but for this example it should be all right.

```clojure
(def mutation-op
  #:chickn.mutation
          {:type          :chickn.mutation/rand-mutation
           :rate          0.3
           :random-func   rand
           :mutation-func one-or-zero})
```

`Chickn` comes with a default config, but some customization is needed (`chromo-gen` and `fitness` have to be provided).
Here we are also specifying the `:solved?` which was explained above. We are muting the `:reporter` to avoid getting
prints, setting up our mutation operator, and setting that solutions with higher scores are preferred.

```clojure
(def config (merge
              default-cfg
              #:chickn.core
                      {:chromo-gen chromo-gen
                       :fitness    fitness
                       :solved?    solved?
                       :reporter   util/noop
                       :mutation   mutation-op
                       :comparator higher-is-better}))
```

It's time to fire the process!

```clojure
(dissoc
    (init-and-evolve config 100) :population)
=> {:solved? true, :iteration 9, :best-chromosome [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1], :time 1}
```

We are `dissoc`ing the `:population` as it will include the entire population and the output can
be quite verbose. 

You can find the code for this example here [src/chickn/examples/hello_world.cljc](src/chickn/examples/hello_world.cljc).


## Examples

[Examples namespace](/src/chickn/examples) has a few code examples.

[Using chickn from cljs](https://kongeor.github.io/chicknism/) for solving the
Traveling Salesman Problem.

[Evolduo](https://github.com/kongeor/evolduo-app) is using Chickn for evolving musical phrases.


## Project Status

Chickn should be considered alpha quality software.


## License

Copyright © 2018-2024 Kostas Georgiadis

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
