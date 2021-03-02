expressible: user-behavior-generative https://probprog.github.io/anglican/examples/viewer/?worksheet=practical/faithful_user
* Requires dividing the loop into two loops of length five: first to sample `faking` variables, and then `events`.

expressible: infer-drift https://probprog.github.io/anglican/examples/viewer/?worksheet=irr-1

expressible: predict-irr https://probprog.github.io/anglican/examples/viewer/?worksheet=irr-1

expressible: learn-gbm https://probprog.github.io/anglican/examples/viewer/?worksheet=irr-2
* Uses a GP, but does so by manually computing GP weight, and flipping a Bernoulli, which trace-types can also express.

expressible: linear-regression https://probprog.github.io/anglican/examples/viewer/?worksheet=linear_regression

expressible: gda-trainer https://probprog.github.io/anglican/examples/viewer/?worksheet=classification
* Anglican defines a special distribution for mixtures of Gaussians, which can also be done for trace-types (it would have type D R, distribution over reals).

expressible: scientists https://probprog.github.io/anglican/examples/viewer/?worksheet=gaussian-posteriors

expressible: kalman https://probprog.github.io/anglican/examples/viewer/?worksheet=kalman
* Requires the use of `unroll` (Figure 12 in trace types paper). It would also be possible to add a `foreach` loop to trace-types that allows state computed in one iteration to be visible to the next (as in `while`). This is fine, so long as the trace type of the loop body does not depend on the previously computed state.

expressible: gmm https://probprog.github.io/anglican/examples/viewer/?worksheet=gmm-iris
* Requires adding a Dirichlet distribution and a ground type for vectors on the simplex
* The collapsed representation would require foreach with state, described under `kalman`.

expressible: run-pencil-factory https://probprog.github.io/anglican/examples/viewer/?worksheet=pencil-factory

expressible: sprinkler-bayes-net https://probprog.github.io/anglican/examples/viewer/?worksheet=bayes-net

expressible: kalman-chaos-opt https://probprog.github.io/anglican/examples/viewer/?source=github&user=probprog&repo=bopp&path=worksheets/chaos.clj
* Requires first sampling from delta-dist in a loop, and then computing the deterministic functions of those variables that enable sampling observations

not expressible: dp-example-1 https://probprog.github.io/anglican/examples/viewer/?worksheet=nonparametrics/dp-mixture-model
* Uses `mem` to implement a Dirichlet Process Mixture Model.

not expressible: hdp-example-1 https://probprog.github.io/anglican/examples/viewer/?worksheet=nonparametrics/hdp
* Uses `mem` to implement a hierarchical Dirichlet Process.

not expressible: pdia https://probprog.github.io/anglican/examples/viewer/?worksheet=nonparametrics/pdia
* Uses `crp` (which uses `mem`) to implement probabilistic deterministic infinite automata

expressible: drill-or-not-with-sounding https://probprog.github.io/anglican/examples/viewer/?worksheet=decision-diagram

expressible: meet-by-chance https://probprog.github.io/anglican/examples/viewer/?worksheet=coordination-game

expressible: amy, bob https://probprog.github.io/anglican/examples/viewer/?worksheet=coordination-game
* Not directly representable as a pair of mutually recursive procedures, but can use a stateful `foreach` loop (described above, under `kalman`): start by sampling a location for Bob, then loop up to the given depth, sampling Bernoullis for whether each person's preference equals the last-sampled location. (The reason, fundamentally, why this is expressible, is that the number and types of sample and observe statements are completely determined by the number `depth`. The limitations of trace-types concern stochastic control flow.)

not expressible: poisson-trace https://probprog.github.io/anglican/examples/viewer/?worksheet=poisson-trace
* Technically, this model is equivalent to sampling from a Poisson; but that is missing the point -- trace types cannot express a loop where the number of iterations depends via a complex deterministic function on multiple stochastic choices.

(note: we do not include the indian GPA example: https://probprog.github.io/anglican/examples/viewer/?worksheet=indian-gpa. Trace types cannot express it, but Anglican's inference engine makes unsound assumptions, exactly of the sort that trace types help to prevent.)

not expressible: firstplayer https://probprog.github.io/anglican/examples/viewer/?worksheet=nested-number-guessing
* Hard constraints (dirac distribution) not expressible with trace types.

not expressible: marsaglia-normal https://probprog.github.io/anglican/examples/viewer/?worksheet=marsaglia
* As in the Poisson case, this is a recursive procedure with a complex termination condition based on determinsitic implications of multiple random choices.

expressible: aircraft https://probprog.github.io/anglican/examples/viewer/?worksheet=aircraft
* Uses a nested forInRandomRange loop to sample number of aircraft; if number of blips were from Poisson or Geometric, could use another forInRandomRange loop to sample the blips themselves. Instead, use withProbability twice to decide between 0, 1, and 2 blips.

expressible: gaussian https://probprog.github.io/anglican/examples/viewer/?worksheet=aistats/gaussian-aistats

expressible: hmm https://probprog.github.io/anglican/examples/viewer/?worksheet=aistats/hmm-aistats

not expressible: branching https://probprog.github.io/anglican/examples/viewer/?worksheet=aistats/branching-aistats
* Expressing this would require manually computing the probability that the draw from the Poisson is < 4, and using withProbability to decide whether to draw another sample.
