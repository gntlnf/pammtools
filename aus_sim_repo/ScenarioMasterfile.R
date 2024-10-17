


### scenario_master list

scenarioMaster <- list(
  list(
    Scenario = "Poisson_rand_nterm", N = 100, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_nterm", N = 25, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_nterm", N = 50, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_nterm", N = 200, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),

  ## sec2

  list(
    Scenario = "Poisson_rand_term", N = 100, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_term", N = 25, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_term", N = 50, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_term", N = 200, alpha = c(0.05, 0.05, 0.05, 0.05), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),

  # sec3
  list(
    Scenario = "Poisson_nrand_nterm", N = 100, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_nrand_nterm", N = 25, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_nrand_nterm", N = 50, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_rand_nterm", N = 200, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  # sec4
  list(
    Scenario = "Poisson_nrand_term", N = 100, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_nrand_nterm", N = 25, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_nrand_nterm", N = 50, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Poisson_nrand_nterm", N = 200, alpha = c(0.05, 0.05, 0.05, 0.05), cens = FALSE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),

  # sec5
  list(
    Scenario = "Markov_rand_nterm", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_nterm", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_nterm", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_nterm", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  # sec6
  list(
    Scenario = "Markov_rand_term", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_term", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_term", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_rand_term", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),

  # sec7
  list(
    Scenario = "Markov_nrand_nterm", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_nterm", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_nterm", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_nterm", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  # sec8
  list(
    Scenario = "Markov_nrand_term", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_term", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_term", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "Markov_nrand_term", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = FALSE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),


  # sec9
  list(
    Scenario = "nMarkov_rand_nterm", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_nterm", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_nterm", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_nterm", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  # sec10
  list(
    Scenario = "nMarkov_rand_term", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_term", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_term", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_rand_term", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = FALSE,
    cens.haz = c(0, 0, 0, 0, 0), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),

  # sec11
  list(
    Scenario = "nMarkov_nrand_nterm", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_nterm", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_nterm", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_nterm", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0, 0, 0, 0, 0), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  # sec8
  list(
    Scenario = "nMarkov_nrand_term", N = 100, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_term", N = 25, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_term", N = 50, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  ),
  list(
    Scenario = "nMarkov_nrand_term", N = 200, alpha = c(0.05, 0.02, 0.01, 0.1, 0.001), cens = TRUE,
    gamma = TRUE, type2 = FALSE, m = 400,
    staggered = FALSE, stat.depend = TRUE,
    cens.haz = c(0.005, 0.08, 0.005, 0.005, 0.005), death.haz = c(0.005, 0.005, 0.005, 0.005, 0.005), nondeg = FALSE, EOS = 100,
    censRate = 1 / 60, stopcrit = 2
  )
)
