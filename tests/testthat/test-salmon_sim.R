context("salmon_sim.R")

test_that("salmon_sim() gives correct answer with original default inputs",{
          set.seed(42)
          expect_equal(salmon_sim(alpha = 0.8,
                                  beta = c(0.8, 0.2, 0.1, 0.1),
                                  p_prime = c(0.01, 0.98, 0.01),
                                  rho = 0.6,
                                  omega = 0.8,
                                  sigma_nu = 0.75,
                                  sigma_epsilon = 1,
                                  phi_1 = 0.1,
                                  T = 100,
                                  # h_t = rep(0.2, T), # in case T is globally defined
                                  R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1, 0.1, 0.1)),
                       default_sim_seed_42)
})

test_that("salmon_sim() gives error if any non-numeric inputs",{
          expect_error(salmon_sim(T = "a"))
})

test_that("salmon_sim() gives error if length(h_t) != T",{
          expect_error(salmon_sim(T = 50,
                                  h_t = rep(0.2, 49)))
})

test_that("salmon_sim() gives error if length(R_t_init) != 8",{
          expect_error(salmon_sim(R_t_init = 1:7))
})

test_that("salmon_sim() gives error if sum(p_prime) != 1",{
          expect_error(salmon_sim(p_prime = c(0.1, 0.1, 0.1)))
})

test_that("salmon_sim() gives error if all parameters not >= 0",{
          expect_error(salmon_sim(alpha = -1))
})

test_that("plot_sim() can create a plot for default run (no seed set)",{
          expect_silent(salmon_run())
          expect_silent(salmon_run(new_plot=FALSE))
          dev.off()
})
