# Requires rstan
#' This script runs Bayesian and Frequentist linear regression models
#' for simulated data used to demonstrate the problems with NHST with
#' small effects and apparent samples. It includes ways of summarising 
#' probability (under frequestist probability distributions and Bayesian
#' posterior draws)


# Load libraries
library(tidyverse)
library(rstan)
library(brms)
library(car)
library(lmPerm)
library(ggridges)
options(mc.cores = parallel::detectCores())

# read in data
dat_n50   <- read_csv("data/example_data_n50.csv")
dat_n150  <- read_csv("data/example_data_n150.csv")
dat_n1000 <- read_csv("data/example_data_n1000.csv")

# Create frequentist linear regression model
lin_m <- lm(data = dat_n50, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
lin_m_s <- summary(lin_m)

# Deriving one sided tests for above and below (Wald test but using t distribution)
# One sided p-values (p > 0)
pt(coef(summary(lin_m))[, 3], lin_m$df, lower = TRUE)
# One sided p-values (p < 0)
pt(coef(summary(lin_m))[, 3], lin_m$df, lower = FALSE)

# exact t-value (two-tailed)
2 * pt(-abs(2.354), 45)

# Approximate simulating from the implied t-distribution for any range
rt_sample <- function(N, nu, mu, standard_dev){
  x1 <- rt(N, nu) # 1
  x2 <- x1/sqrt(nu/(nu-2)) # 2
  x3 <- x2 * standard_dev # 3
  x4 <- x3 + mu # 4
  return(x4)
}

n_sim <- 1e6
t_sim_pov <- rt_sample(n_sim, lin_m$df.residual, lin_m$coefficients["poverty_z"], summary(lin_m)$coefficients["poverty_z", "Std. Error"])

# close to original p-value (two tailed),
2 * (1 - sum(t_sim_pov > 0) / n_sim)


# simulation one-tailed H > 0
sum(t_sim_pov > 0) / n_sim
# simulation one-tailed H < 0
sum(t_sim_pov < 0) / n_sim


# Amount above arbitrary 0.2 effect
sum(t_sim_pov > 0.2) / n_sim

# ROPE
rope_range <- 0.01361602
# Amount within ROPE
sum(t_sim_pov < 0 + rope_range & t_sim_pov > 0 - rope_range) / n_sim
# Amount above ROPE
sum(t_sim_pov > 0 + rope_range) / n_sim
# Amount below ROPE
sum(t_sim_pov < 0 - rope_range) / n_sim


t_sim_ineq <- rt_sample(n_sim, lin_m$df.residual, lin_m$coefficients["inequal_z"], summary(lin_m)$coefficients["inequal_z", "Std. Error"])
# close to original p-value (one tailed),
2 * (1 - sum(t_sim_ineq > 0) / n_sim)
# simulation one-tailed H > 0
sum(t_sim_ineq > 0) / n_sim
# simulation one-tailed H < 0
sum(t_sim_ineq < 0) / n_sim

# Downsides however are that the simulated distribution is inflexible (the t distribution
# will be imposed on it, where this may not always be appropriate (compared to Bayesian))



# Add bootstrapped confidence intervals (This solves the philosophical 
# objection)
set.seed(2022)
n_R <- 10000
lin_m_bs <- car::Boot(lin_m, R = n_R)
summary(lin_m_bs)
confint(lin_m_bs)
hist(lin_m_bs)

# The same method can then be used for the coefficients from each bootstrap
# proportion greater than zero
sum(lin_m_bs$t[,"poverty_z"] > 0) / n_R
sum(lin_m_bs$t[,"poverty_z"] < 0) / n_R
sum(lin_m_bs$t[,"inequal_z"] > 0) / n_R
sum(lin_m_bs$t[,"inequal_z"] < 0) / n_R
sum(lin_m_bs$t[,"spending_z"] > 0) / n_R
sum(lin_m_bs$t[,"spending_z"] < 0) / n_R
sum(lin_m_bs$t[,"staffing_z"] > 0) / n_R
sum(lin_m_bs$t[,"staffing_z"] < 0) / n_R

# proportion greater than a pre_defined strength (e.g. 0.2)
sum(lin_m_bs$t[,"poverty_z"] > 0.2) / n_R


# Add permutation tests (https://cran.r-project.org/web/packages/lmPerm/vignettes/lmPerm.pdf)
set.seed(2022)
lin_m_perm <- lmp(data = dat_n50, 
                  formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z, 
                  perm = "Prob", maxIter = 1000000, Ca = 0.00001, nCycle = 1, model = TRUE)

lin_m_perm_s <- summary(lin_m_perm)


# A p-value is the probability of the observed results under the assumption
# that the null hypothesis best describes the data generating process and the data 
# is a sample drawn from a population through a random process or otherwise 
# representative process.

# Bootstrap coefficients refer to the coefficients generated through sampling with
# replacement from the observed data, which is reflective of the idea that the 
# data generating process could have generated any of the observed subjects and is
# therefore comparable to sampling coefficients from the superpopulation.

# A permutation test calculates the p-value as the proportion of permuted datasets 
# which produce a test statistic at least as extreme as the one observed from the 
# actual data. (https://users.ox.ac.uk/~scro1407/slides_permutations_CGAT_16feb17.pdf)

# Over 1,000,000 permutations, an estimate at or greater than 0.37139 for poverty is
# observed with 0.023 probability
# Over 1,000,000 permutations, an estimate at or greater than 0.24254 for inequality is
# observed with 0.113 probability

# Create Bayesian linear regression model with weakly informative priors



brm_mod <- brm(data = dat_n50, 
               formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z, iter = 0)

brms::prior_summary(brm_mod)

flat_priors <- prior(uniform(-100, 100), lb = -100, ub = 100, class = "b") +
               prior(uniform(-100, 100), lb = -100, ub = 100, class = "Intercept") +
               prior(uniform(0, 100), lb = 0, ub = 100, class = "sigma")

# flat priors
bayesmod_flat <- brm(data = dat_n50, 
                     formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z, 
                     chains = 4, cores = 4, iter = 5000,
                     sample_prior = "yes", prior = flat_priors)

summary(bayesmod_flat)
plot(density(as_draws_df(bayesmod_flat)$prior_Intercept, bw = 2))
plot(density(as_draws_df(bayesmod_flat)$prior_b, bw = 2))

# weakly informative priors
weak_priors <- prior(normal(0, 0.5), class = "b") +
  prior(normal(0, 0.5), class = "Intercept") +
  prior(exponential(1), class = "sigma")

bayesmod_weak <- brm(data = dat_n50, 
                     formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z, 
                     chains = 4, cores = 4, iter = 5000,
                     sample_prior = "yes", prior = weak_priors)

summary(bayesmod_weak)
summary(lin_m)

# informative priors without incorporating directionality
weakdir_priors <- prior(normal(0.5, 0.3), class = "b", coef = "poverty_z") +
                  prior(normal(0.25, 0.5), class = "b", coef = "inequal_z") +
                  prior(normal(-0.25, 0.3), class = "b", coef = "spending_z") +
                  prior(normal(0.1, 0.6), class = "b", coef = "staffing_z") +
                  prior(normal(0, 0.5), class = "Intercept") +
                  prior(exponential(1), class = "sigma")

bayesmod_weakdir <- brm(data = dat_n50, 
                     formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z, 
                     chains = 4, cores = 4, iter = 5000,
                     sample_prior = "yes", prior = weakdir_priors)

summary(bayesmod_weakdir)
summary(lin_m)


plot(density(as_draws_df(bayesmod_flat)$prior_b))
lines(density(as_draws_df(bayesmod_weak)$prior_b), col = "blue")
lines(density(as_draws_df(bayesmod_weakdir)$prior_b_poverty_z), col = "pink")
plot(density(as_draws_df(bayesmod_flat)$b_poverty_z), ylim = c(0, 3))
lines(density(as_draws_df(bayesmod_weak)$b_poverty_z), col = "blue")
lines(density(as_draws_df(bayesmod_weakdir)$b_poverty_z), col = "pink")


sum((as_draws_df(bayesmod_flat)$b_poverty_z > 0)) / nrow(as_draws_df(bayesmod_flat))



# table: pr(>=t), prt > 0, prt < 0, 
#       sim_t > 0, sim_t < 0, sim_t > v, 
#       sim_t in rope, sim_t > rope, sim_t < rope
#       bayes_flat > 0, bayes_flat < 0, bayes_flat > v, bayes_flat in rope > rope < rope etc.

# poverty 

# Frequentist
# probability of observing t of this size in either direction from 0
(pov_prt_2t <- 2 * pt(-abs(lin_m_s$coefficients["poverty_z", "t value"]), lin_m$df.residual))
# permutation two-tailed p
lin_m_perm_s$coefficients["poverty_z", "Pr(Prob)"]
# probability of observing t if B > 0
pt(coef(summary(lin_m))["poverty_z", "t value"], lin_m$df, lower = TRUE)
# probability of observing t if B < 0
pt(coef(summary(lin_m))["poverty_z", "t value"], lin_m$df, lower = FALSE)
# probability of observing b from simulation if B < 0
sum(t_sim_pov < 0) / n_sim
# probability of observing b from simulation if B > 0
sum(t_sim_pov > 0) / n_sim
# probability of observing b from simulation if B > 0.2
sum(t_sim_pov > 0.2) / n_sim
# probability of t-distributed effect from simulation within ROPE 
sum(t_sim_pov < 0 + rope_range & t_sim_pov > 0 - rope_range) / n_sim
# probability of t-distributed effect from simulation above ROPE 
sum(t_sim_pov > 0 + rope_range) / n_sim
# probability of t-distributed effect from simulation below ROPE
sum(t_sim_pov < 0 + rope_range) / n_sim

# From bootstraps
# proportion bs > 0
sum(lin_m_bs$t[, "poverty_z"] > 0) / n_R
# proportion bs < 0
sum(lin_m_bs$t[, "poverty_z"] < 0) / n_R
# proportion bs > 0.2
sum(lin_m_bs$t[, "poverty_z"] > 0.2) / n_R
# proportion within ROPE 
sum(lin_m_bs$t[, "poverty_z"] < 0 + rope_range & lin_m_bs$t[, "poverty_z"] > 0 - rope_range) / n_R
# proportion above ROPE
sum(lin_m_bs$t[, "poverty_z"] > 0 + rope_range) / n_R
# proportion below ROPE
sum(lin_m_bs$t[, "poverty_z"] < 0 - rope_range) / n_R

# From Bayesian models
bflat_draws <- as_draws_df(bayesmod_flat)
# b > 0
sum(bflat_draws$b_poverty_z > 0) / nrow(bflat_draws)
# b < 0
sum(bflat_draws$b_poverty_z < 0) / nrow(bflat_draws)
# b > 0.2
sum(bflat_draws$b_poverty_z > 0.2) / nrow(bflat_draws)
# b within ROPE
sum(bflat_draws$b_poverty_z < 0 + rope_range & bflat_draws$b_poverty_z > 0 - rope_range) / nrow(bflat_draws)
# b above ROPE
sum(bflat_draws$b_poverty_z > 0 + rope_range) / nrow(bflat_draws)
# b below ROPE
sum(bflat_draws$b_poverty_z < 0 - rope_range) / nrow(bflat_draws)


# From Bayesian models
bweak_draws <- as_draws_df(bayesmod_weak)
# b > 0
sum(bweak_draws$b_poverty_z > 0) / nrow(bweak_draws)
# b < 0
sum(bweak_draws$b_poverty_z < 0) / nrow(bweak_draws)
# b > 0.2
sum(bweak_draws$b_poverty_z > 0.2) / nrow(bweak_draws)
# b within ROPE
sum(bweak_draws$b_poverty_z < 0 + rope_range & bweak_draws$b_poverty_z > 0 - rope_range) / nrow(bweak_draws)
# b above ROPE
sum(bweak_draws$b_poverty_z > 0 + rope_range) / nrow(bweak_draws)
# b below ROPE
sum(bweak_draws$b_poverty_z < 0 - rope_range) / nrow(bweak_draws)


# From Bayesian models
bweakdir_draws <- as_draws_df(bayesmod_weakdir)
# b > 0
sum(bweakdir_draws$b_poverty_z > 0) / nrow(bweakdir_draws)
# b < 0
sum(bweakdir_draws$b_poverty_z < 0) / nrow(bweakdir_draws)
# b > 0.2
sum(bweakdir_draws$b_poverty_z > 0.2) / nrow(bweakdir_draws)
# b within ROPE
sum(bweakdir_draws$b_poverty_z < 0 + rope_range & bweakdir_draws$b_poverty_z > 0 - rope_range) / nrow(bweakdir_draws)
# b above ROPE
sum(bweakdir_draws$b_poverty_z > 0 + rope_range) / nrow(bweakdir_draws)
# b below ROPE
sum(bweakdir_draws$b_poverty_z < 0 - rope_range) / nrow(bweakdir_draws)


# Write function to collate all of these from arguments:

# Frequentist model object, permutation object, boot object,
# flat bayes model, weak bayes model, informative bayes model,
# variable of interest, critically important values, rope

inference_compare <- function (lm_obj, perm_obj, boot_obj, flat_bayes, 
                               weak_bayes, inform_bayes, 
                               var, important_val, iv_gt = TRUE, rope,
                               t_sim_n = 1e6) {
  
  # required packages
  require(brms)
  require(car)
  require(lmPerm)
  
  # random sampler from t distribution from n, df, mean coef and std err
  rt_sample <- function(N, nu, mu, standard_dev){
    x1 <- rt(N, nu) # 1
    x2 <- x1/sqrt(nu/(nu-2)) # 2
    x3 <- x2 * standard_dev # 3
    x4 <- x3 + mu # 4
    return(x4)
  }
  
  # Create additional summary and draws objects for later functions
  lm_obj_summary <- summary(lm_obj)
  perm_obj_summary <- summary(perm_obj)
  boot_obj_summary <- summary(boot_obj)
  
  flat_bayes_draws <- as_draws_df(flat_bayes)
  weak_bayes_draws <- as_draws_df(weak_bayes)
  inform_bayes_draws <- as_draws_df(inform_bayes)
  
  # Simulate coefficients from implied t distribution
  t_samples <- rt_sample(t_sim_n, lm_obj$df.residual, 
                         lm_obj$coefficients[var], 
                         lm_obj_summary$coefficients[var, "Std. Error"])
  
  # values to be estimated
  estvals <- c("Two-tailed t-test p",
              "Permutation p", 
              "One tailed t-test (H0: B > 0)",
              "One tailed t-test (H0: B < 0)",
              "Proportion Simulated Coefficients (t-distributed) B > 0",
              "Proportion Simulated Coefficients (t-distributed) B < 0",
              "Proportion Simulated Coefficients (t-distributed) B >|< Important Value",
              "Proportion Simulated Coefficients (t-distributed) B in ROPE",
              "Proportion Simulated Coefficients (t-distributed) B > ROPE",
              "Proportion Simulated Coefficients (t-distributed) B < ROPE",
              "Proportion Bootstrapped Coefficients B > 0",
              "Proportion Bootstrapped Coefficients B < 0",
              "Proportion Bootstrapped Coefficients B >|< Important Value",
              "Proportion Bootstrapped Coefficients B in ROPE",
              "Proportion Bootstrapped Coefficients B > ROPE",
              "Proportion Bootstrapped Coefficients B < ROPE",
              "Proportion Posterior Draws (Uninformative Priors) B > 0",
              "Proportion Posterior Draws (Uninformative Priors) B < 0",
              "Proportion Posterior Draws (Uninformative Priors) B >|< Important Value",
              "Proportion Posterior Draws (Uninformative Priors) B in ROPE",
              "Proportion Posterior Draws (Uninformative Priors) B > ROPE",
              "Proportion Posterior Draws (Uninformative Priors) B < ROPE",
              "Proportion Posterior Draws (Weakly Informative Priors) B > 0",
              "Proportion Posterior Draws (Weakly Informative Priors) B < 0",
              "Proportion Posterior Draws (Weakly Informative Priors) B >|< Important Value",
              "Proportion Posterior Draws (Weakly Informative Priors) B in ROPE",
              "Proportion Posterior Draws (Weakly Informative Priors) B > ROPE",
              "Proportion Posterior Draws (Weakly Informative Priors) B < ROPE",
              "Proportion Posterior Draws (Informative Priors) B > 0",
              "Proportion Posterior Draws (Informative Priors) B < 0",
              "Proportion Posterior Draws (Informative Priors) B >|< Important Value",
              "Proportion Posterior Draws (Informative Priors) B in ROPE",
              "Proportion Posterior Draws (Informative Priors) B > ROPE",
              "Proportion Posterior Draws (Informative Priors) B < ROPE"
    )
  
  # Hypothesis tests
  estval1 <- 2 * pt(-abs(coef(lm_obj_summary)[var, "t value"]), lm_obj$df.residual)
  estval2 <- perm_obj_summary$coefficients[var, "Pr(Prob)"]
  estval3 <- pt(coef(lm_obj_summary)[var, "t value"], lm_obj$df.residual, lower = TRUE)
  estval4 <- pt(coef(lm_obj_summary)[var, "t value"], lm_obj$df.residual, lower = FALSE)
  
  # Simulated t-distributed coefficients
  estval5 <- sum(t_samples > 0) / length(t_samples)
  estval6 <- sum(t_samples < 0) / length(t_samples)
  estval7 <-ifelse(iv_gt == TRUE, 
                   sum(t_samples > important_val) / length(t_samples),
                   sum(t_samples < important_val) / length(t_samples)
                   )
  estval8 <- sum(t_samples < 0 + rope & t_samples > 0 - rope) / length(t_samples)
  estval9 <- sum(t_samples > 0 + rope) / length(t_samples)
  estval10 <- sum(t_samples < 0 + rope) / length(t_samples)
  
  # Proportion of bootstrapped coefficients
  estval11 <- sum(boot_obj$t[, var] > 0) / length(boot_obj$t[, var])
  estval12 <- sum(boot_obj$t[, var] < 0) / length(boot_obj$t[, var])
  estval13 <-ifelse(iv_gt == TRUE, 
                   sum(boot_obj$t[, var] > important_val) / length(boot_obj$t[, var]),
                   sum(boot_obj$t[, var] < important_val) / length(boot_obj$t[, var])
  )
  estval14 <- sum(boot_obj$t[, var] < 0 + rope & boot_obj$t[, var] > 0 - rope) / length(boot_obj$t[, var])
  estval15 <- sum(boot_obj$t[, var] > 0 + rope) / length(boot_obj$t[, var])
  estval16 <- sum(boot_obj$t[, var] < 0 + rope) / length(boot_obj$t[, var])
  
  
  # Proportion of posterior (uninformative priors)
  estval17 <- sum(flat_bayes_draws[,paste0("b_", var)] > 0) / nrow(flat_bayes_draws)
  estval18 <- sum(flat_bayes_draws[,paste0("b_", var)] < 0) / nrow(flat_bayes_draws)
  
  estval19 <- ifelse(iv_gt == TRUE,
                     sum(flat_bayes_draws[,paste0("b_", var)] > important_val) / nrow(flat_bayes_draws),
                     sum(flat_bayes_draws[,paste0("b_", var)] < important_val) / nrow(flat_bayes_draws)
  )
  
  estval20 <- sum(flat_bayes_draws[,paste0("b_", var)] < 0 + rope & flat_bayes_draws[,paste0("b_", var)] > 0 - rope) / nrow(flat_bayes_draws)
  estval21 <- sum(flat_bayes_draws[,paste0("b_", var)] > 0 + rope) / nrow(flat_bayes_draws)
  estval22 <- sum(flat_bayes_draws[,paste0("b_", var)] < 0 - rope) / nrow(flat_bayes_draws)
  
  # Proportion of posterior (weakly informative priors)
  estval23 <- sum(weak_bayes_draws[,paste0("b_", var)] > 0) / nrow(weak_bayes_draws)
  estval24 <- sum(weak_bayes_draws[,paste0("b_", var)] < 0) / nrow(weak_bayes_draws)
  
  estval25 <- ifelse(iv_gt == TRUE,
                     sum(weak_bayes_draws[,paste0("b_", var)] > important_val) / nrow(weak_bayes_draws),
                     sum(weak_bayes_draws[,paste0("b_", var)] < important_val) / nrow(weak_bayes_draws)
  )
  
  estval26 <- sum(weak_bayes_draws[,paste0("b_", var)] < 0 + rope & weak_bayes_draws[,paste0("b_", var)] > 0 - rope) / nrow(weak_bayes_draws)
  estval27 <- sum(weak_bayes_draws[,paste0("b_", var)] > 0 + rope) / nrow(weak_bayes_draws)
  estval28 <- sum(weak_bayes_draws[,paste0("b_", var)] < 0 - rope) / nrow(weak_bayes_draws)
  
  # Proportion of posterior (informative priors)
  estval29 <- sum(inform_bayes_draws[,paste0("b_", var)] > 0) / nrow(inform_bayes_draws)
  estval30 <- sum(inform_bayes_draws[,paste0("b_", var)] < 0) / nrow(inform_bayes_draws)
  
  estval31 <- ifelse(iv_gt == TRUE,
                     sum(inform_bayes_draws[,paste0("b_", var)] > important_val) / nrow(inform_bayes_draws),
                     sum(inform_bayes_draws[,paste0("b_", var)] < important_val) / nrow(inform_bayes_draws)
  )
  
  estval32 <- sum(inform_bayes_draws[,paste0("b_", var)] < 0 + rope & inform_bayes_draws[,paste0("b_", var)] > 0 - rope) / nrow(inform_bayes_draws)
  estval33 <- sum(inform_bayes_draws[,paste0("b_", var)] > 0 + rope) / nrow(inform_bayes_draws)
  estval34 <- sum(inform_bayes_draws[,paste0("b_", var)] < 0 - rope) / nrow(inform_bayes_draws)
  
  results <- tibble(
    estvals,
    p = c(estval1, estval2, estval3, estval4, estval5, estval6, estval7, estval8, estval9, estval10,
      estval11, estval12, estval13, estval14, estval15, estval16, estval17, estval18, estval19, estval20,
      estval21, estval22, estval23, estval24, estval25, estval26, estval27, estval28, estval29, estval30,
      estval31, estval32, estval33, estval34)
  )
  
  names(results)[2] <- paste0("p_", var)
  
  return(results)
  
}


inf_comp_poverty <- inference_compare(lin_m, lin_m_perm, lin_m_bs, bayesmod_flat,
                  bayesmod_weak, bayesmod_weakdir, "poverty_z",
                  0.5, iv_gt = TRUE, rope = 0.1) 

inf_comp_inequal <- inference_compare(lin_m, lin_m_perm, lin_m_bs, bayesmod_flat,
                                      bayesmod_weak, bayesmod_weakdir, "inequal_z",
                                      0.6, iv_gt = TRUE, rope = 0.1) 

inf_comp_spend <- inference_compare(lin_m, lin_m_perm, lin_m_bs, bayesmod_flat,
                                      bayesmod_weak, bayesmod_weakdir, "spending_z",
                                      -0.05, iv_gt = FALSE, rope = 0.1) 

inf_comp_staffing <- inference_compare(lin_m, lin_m_perm, lin_m_bs, bayesmod_flat,
                                    bayesmod_weak, bayesmod_weakdir, "staffing_z",
                                    -0.025, iv_gt = FALSE, rope = 0.1) 

betap_table <- left_join(inf_comp_poverty, inf_comp_inequal, by = "estvals") %>%
  left_join(., inf_comp_spend, by = "estvals") %>%
  left_join(., inf_comp_staffing, by = "estvals") %>% 
  mutate_at(vars(p_poverty_z:p_staffing_z), ~round(., 3)) %>%
  rename(
    Condition = 1,
    Poverty = 2, 
    Inequality = 3,
    Spending = 4,
    Staffing = 5
  )

# write_csv(betap_table, "tabs_out/betap_out_example.csv")

# Visualise implied uncertainty distributions under each method
bayesmod_flat_draws <- as_draws_df(bayesmod_flat)
bayesmod_weak_draws <- as_draws_df(bayesmod_weak)
bayesmod_inform_draws <- as_draws_df(bayesmod_weakdir)

bootstrap_draws <- as_tibble(lin_m_bs$t) %>%
  rename(
    b_Intercept = 1,
    b_poverty_z = 2, 
    b_inequal_z = 3,
    b_spending_z = 4,
    b_staffing_z = 5
  )

# t distribution samples
# - problem, they are all treated as independent
set.seed(1212)
t_sim_draws <- tibble(
  b_Intercept = rt_sample(1e4, lin_m$df.residual, lin_m$coefficients["(Intercept)"], 
                          summary(lin_m)$coefficients["(Intercept)", "Std. Error"]),
  b_poverty_z = rt_sample(1e4, lin_m$df.residual, lin_m$coefficients["poverty_z"], 
                          summary(lin_m)$coefficients["poverty_z", "Std. Error"]),
  b_inequal_z = rt_sample(1e4, lin_m$df.residual, lin_m$coefficients["inequal_z"], 
                          summary(lin_m)$coefficients["inequal_z", "Std. Error"]),
  b_spending_z = rt_sample(1e4, lin_m$df.residual, lin_m$coefficients["spending_z"], 
                          summary(lin_m)$coefficients["spending_z", "Std. Error"]),
  b_staffing_z = rt_sample(1e4, lin_m$df.residual, lin_m$coefficients["staffing_z"], 
                           summary(lin_m)$coefficients["staffing_z", "Std. Error"])
)


combined_draws <- bind_rows(bayesmod_flat_draws %>% select(b_Intercept:b_staffing_z), 
          bayesmod_weak_draws %>% select(b_Intercept:b_staffing_z), 
          bayesmod_inform_draws %>% select(b_Intercept:b_staffing_z), 
          bootstrap_draws,
          t_sim_draws,
          .id = "model") %>%
  mutate(
    model = case_when(model == 1 ~ "Bayesian (Uninformative Priors)",
                      model == 2 ~ "Bayesian (Weakly Informative Priors)",
                      model == 3 ~ "Bayesian (Informative Priors)",
                      model == 4 ~ "Bootstrap",
                      model == 5 ~ "Samples from t-distribution")
  )



# Reorder models as factor

example_density_plot <- combined_draws %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "Coefficient", values_to = "Estimate") %>%
  mutate(Coefficient = case_when(
    Coefficient == "b_Intercept" ~ "Intercept",
    Coefficient == "b_poverty_z" ~ "Poverty",
    Coefficient == "b_inequal_z" ~ "Inequality",
    Coefficient == "b_spending_z" ~ "Spending",
    Coefficient == "b_staffing_z" ~ "Staffing",
  )) %>%
  mutate(Coefficient = factor(Coefficient, levels = c("Intercept", "Poverty", "Inequality", "Spending", "Staffing")),
         model = factor(model, levels = c("Bayesian (Informative Priors)", 
                                          "Bayesian (Weakly Informative Priors)",
                                          "Bayesian (Uninformative Priors)",
                                          "Bootstrap", 
                                          "Samples from t-distribution"))) %>%
  ggplot(aes(fill = factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    aes(y = model, x = Estimate), 
    rel_min_height = 0.005,
    quantile_lines = FALSE, 
    quantiles = c(0.055, 0.5, 0.945),
    size = 0.3, scale = 0.95) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    aes(y = model, x = Estimate), 
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.95
  ) +
  ylab("") +
  geom_vline(xintercept = 0, lty = 2, linewidth = 0.3) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  expand_limits(y = c(1.1, 6)) +
  facet_wrap(~Coefficient, ncol = 5) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_identity() 


# ggsave(example_density_plot, filename = "plots/example_density_plot.png", units = "in", dpi = 300,
#       width = 8.25, height = 11.75/4, scale = 1.4)


# Table with regression output for:
# - Bayesian models (uniform, weakly informative, informative priors)
# - Bootstraps
# - Standard linear regression model



coefficients(lin_m)


t_samples <- function(t_sim_n, var, lm_obj, lm_obj_summary) { 
  
                       rt_sample(t_sim_n, lm_obj$df.residual, 
                       lm_obj$coefficients[var], 
                       lm_obj_summary$coefficients[var, "Std. Error"])
}

intercept_b_samples <- t_samples(10000, "(Intercept)", lin_m, lin_m_s)
poverty_b_samples <- t_samples(10000, "poverty_z", lin_m, lin_m_s)
inequality_b_samples <- t_samples(10000, "inequal_z", lin_m, lin_m_s)
spending_b_samples <- t_samples(10000, "spending_z", lin_m, lin_m_s)
staffing_b_samples <- t_samples(10000, "staffing_z", lin_m, lin_m_s)

reg_tabs <- tibble(
  name = c(rep("b_Intercept", 10000), rep("b_poverty_z", 10000), rep("b_inequal_z", 10000), rep("b_spending_z", 10000), rep("b_staffing_z", 10000)),
  value = c(intercept_b_samples, poverty_b_samples, inequality_b_samples, spending_b_samples, staffing_b_samples)
) %>%
  group_by(name) %>%
  summarise(
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975)
  ) %>%
  ungroup() %>%
  mutate(
    name = factor(name, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  arrange(name) %>%
  mutate(point_estimate = coefficients(lin_m), .before = q2.5)


bs_reg_tab <- bootstrap_draws %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975)
  ) %>%
  ungroup() %>%
  mutate(
    name = factor(name, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  arrange(name) %>%
  mutate(point_estimate = coefficients(lin_m), .before = q2.5) 

bayes_flat_regtab <- as_tibble(bayesmod_flat_draws) %>%
  select(b_Intercept:b_staffing_z) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    point_estimate = median(value),
    q5.5 =  rethinking::HPDI(value)[1],
    q94.5 = rethinking::HPDI(value)[2]
  ) %>%
  mutate(
    name = factor(name, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  arrange(name)


bayes_weak_regtab <- as_tibble(bayesmod_weak_draws) %>%
  select(b_Intercept:b_staffing_z) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    point_estimate = median(value),
    q5.5 =  rethinking::HPDI(value)[1],
    q94.5 = rethinking::HPDI(value)[2]
  ) %>%
  mutate(
    name = factor(name, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  arrange(name)

bayes_inform_regtab <- as_tibble(bayesmod_inform_draws) %>%
  select(b_Intercept:b_staffing_z) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    point_estimate = median(value),
    q5.5 =  rethinking::HPDI(value)[1],
    q94.5 = rethinking::HPDI(value)[2]
  ) %>%
  mutate(
    name = factor(name, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  arrange(name)

reg_tabs_combined <- left_join(reg_tabs, bs_reg_tab, by = c("name"), suffix = c("_t", "_bs")) %>%
  left_join(bayes_flat_regtab, by = c("name")) %>%
  left_join(bayes_weak_regtab, by = c("name"), suffix = c("_bayes_u", "_bayes_w")) %>%
  left_join(bayes_inform_regtab, by = c("name")) %>%
  rename(point_estimate_bayes_i = point_estimate,
         q5.5_bayes_i = q5.5,
         q94.5_bayes_i = q94.5)

reg_tabs_combined <- reg_tabs_combined %>%
  mutate_at(vars(point_estimate_t:q94.5_bayes_i), ~round(., 2))

write_csv(reg_tabs_combined, "tabs_out/reg_tabs_combined.csv")


