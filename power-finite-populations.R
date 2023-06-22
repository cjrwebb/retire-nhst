# ------------------------------------------------------------------------------------
#' This script simulates the data used in the data visualisations and analysis for
#' the paper: Statistical Inference in Social Policy Research with Apparent Populations
# ------------------------------------------------------------------------------------

#' This script visualises power function (under various measures) and runs simulations
#' of NHSTs under varying modelling and data treatment conditions. 

# Libraries
library(tidyverse)
library(patchwork)
library(effectsize)
library(pwr)
library(faux)
library(brms)


# Simulate statistical power across a range of Pearson's correlation (R)
# and Cohen's D values (d) for equal size groups at sample sizes of 1000, 
# 150 (e.g. English LAs), and 50 (e.g. US states)

pwrtest_out_1000ref <- numeric()
ds <- seq(0.01, 1.5, 0.01)
rs <- seq(0.01, 1, length.out = 150)

for (i in 1:length(rs) ) {
  
  pwrtest_out_1000ref[i] <- pwr::pwr.r.test(n = 1000, r = rs[i], sig.level = 0.05, alternative = "two.sided")$power
  
}

# Highlight 0.8 cutoff point
pwrtest_out_1000ref_hline <- pwrtest_out_1000ref[13]
pwrtest_out_1000ref_vline <- rs[13]


pwr_ttest_out_1000ref <- numeric()
for (i in 1:length(ds) ) {
  
  pwr_ttest_out_1000ref[i] <- pwr::pwr.t.test(n = 500, d = ds[i], sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  
}

# Highlight 0.8 cutoff point
pwr_ttest_out_1000_hline <- pwr_ttest_out_1000ref[18]
pwr_ttest_out_1000_vline <- ds[18]


pwrtest_out_eng <- numeric()

for (i in 1:length(rs) ) {
  
  pwrtest_out_eng[i] <- pwr::pwr.r.test(n = 150, r = rs[i], sig.level = 0.05, alternative = "two.sided")$power
  
}

# Highlight 0.8 cutoff point
pwrtest_out_eng_hline <- pwrtest_out_eng[34]
pwrtest_out_eng_vline <- rs[34]


pwr_ttest_out_eng <- numeric()
for (i in 1:length(ds) ) {
  
  pwr_ttest_out_eng[i] <- pwr::pwr.t.test(n = 75, d = ds[i], sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  
}

# Highlight 0.8 cutoff point
pwr_ttest_out_eng_hline <- pwr_ttest_out_eng[47]
pwr_ttest_out_eng_vline <- ds[47]



pwrtest_out_us <- numeric()

# 50 US States
for (i in 1:length(rs) ) {
  
  pwrtest_out_us[i] <- pwr::pwr.r.test(n = 50, r = rs[i], sig.level = 0.05, alternative = "two.sided")$power
  
}

# Highlight 0.8 cutoff point
pwrtest_out_us_hline <- pwrtest_out_us[58]
pwrtest_out_us_vline <- rs[58]


# Assuming equal in each state
pwr_ttest_out_us <- numeric()
for (i in 1:length(ds) ) {
  
  pwr_ttest_out_us[i] <- pwr::pwr.t.test(n = 25, d = ds[i], sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  
}

pwr_ttest_out_us_hline <- pwr_ttest_out_us[81]
pwr_ttest_out_us_vline <- ds[81]


#' Plot the statistical power function for each of the sample sizes across
#' the strengths of the effects. 

# CLES from: https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full
# https://pubmed.ncbi.nlm.nih.gov/18331151/

pwr_cor_1000ref <- ggplot() +
  geom_point(aes(x = rs, y = pwrtest_out_1000ref), se = FALSE) +
  geom_vline(xintercept = pwrtest_out_1000ref_vline) +
  geom_hline(yintercept = pwrtest_out_1000ref_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  ylab("Correlation Statistical Power at a = 0.05") +
  #xlab("Cohen's f^2") +
  labs(x = expression("Pearson's"~r)) +
  ggtitle("N = 1000")

pwr_t_1000ref <- ggplot() +
  geom_point(aes(x = ds, y = pwr_ttest_out_1000ref), se = FALSE) +
  geom_vline(xintercept = pwr_ttest_out_1000_vline) +
  geom_hline(yintercept = pwr_ttest_out_1000_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(sec.axis=sec_axis(trans=~ pnorm(./sqrt(2), 0, 1), name="CLES (p-superiority)", breaks = seq(0.5, 1, 0.05)), breaks = seq(0, 1.4, 0.2)) +
  ylab("Two-sample t-test Statistical Power at a = 0.05") +
  xlab("Cohen's d") +
  ggtitle("Group 1 N = 500; Group 2 N = 500")

pwr_cor_eng <- ggplot() +
  geom_point(aes(x = rs, y = pwrtest_out_eng), se = FALSE) +
  geom_vline(xintercept = pwrtest_out_eng_vline) +
  geom_hline(yintercept = pwrtest_out_eng_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  ylab("Correlation Statistical Power at a = 0.05") +
  #xlab("Cohen's f^2") +
  labs(x = expression("Pearson's"~r)) +
  ggtitle("N = 150")

pwr_cor_us <- ggplot() +
  geom_point(aes(x = rs, y = pwrtest_out_us), se = FALSE) +
  geom_vline(xintercept = pwrtest_out_us_vline) +
  geom_hline(yintercept = pwrtest_out_us_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  ylab("Correlation Statistical Power at a = 0.05") +
  #xlab("Cohen's f^2") +
  labs(x = expression("Pearson's"~r)) +
  ggtitle("N = 50")

pwr_t_eng <- ggplot() +
  geom_point(aes(x = ds, y = pwr_ttest_out_eng), se = FALSE) +
  geom_vline(xintercept = pwr_ttest_out_eng_vline) +
  geom_hline(yintercept = pwr_ttest_out_eng_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(sec.axis=sec_axis(trans=~ pnorm(./sqrt(2), 0, 1), name="CLES (p-superiority)", breaks = seq(0.5, 1, 0.05)), breaks = seq(0, 1.4, 0.2)) +
  ylab("Two-sample t-test Statistical Power at a = 0.05") +
  xlab("Cohen's d") +
  ggtitle("Group 1 N = 75; Group 2 N = 75")

pwr_t_us <- ggplot() +
  geom_point(aes(x = ds, y = pwr_ttest_out_us), se = FALSE) +
  geom_vline(xintercept =  pwr_ttest_out_us_vline) +
  geom_hline(yintercept = pwr_ttest_out_us_hline) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(sec.axis=sec_axis(trans=~ pnorm(./sqrt(2), 0, 1), name="CLES (p-superiority)", breaks = seq(0.5, 1, 0.05)), breaks = seq(0, 1.4, 0.2)) +
  ylab("Two-sample t-test Statistical Power at a = 0.05") +
  xlab("Cohen's d") +
  ggtitle("Group 1 N = 25; Group 2 N = 25")



#' Combine these plots into a single image and save the results (1/2 page)

power_comp_plot <- (pwr_cor_1000ref + pwr_cor_eng + pwr_cor_us) / (pwr_t_1000ref + pwr_t_eng + pwr_t_us)

ggsave(power_comp_plot, filename = "plots/power_comp_plot.png", units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.4)




# Simulate data with a strong, moderate, small, and negligible effect size using
#' the below specified z-score effect sizes
# effect sizes as correlations: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/
beta_strong        <- 0.4
beta_moderate      <- 0.3
beta_small         <- -1 * 0.15 # negative effect
beta_negligible    <- 0.05 # more than half of small


# Simulate 1000 studies at n = 1000 from a multivariate normal distribution
n = 1000
set.seed(89)
sims_n1000 <- list()

for (i in 1:1000) {
  
  dat <- rnorm_multi(n = n,
              varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
              mu = 0,
              sd = 1,
              r = c(1, beta_strong, beta_moderate, beta_small, beta_negligible,
                    beta_strong, 1, 0, 0, 0,
                    beta_moderate, 0, 1, 0, 0,
                    beta_small, 0, 0, 1, 0,
                    beta_negligible, 0, 0, 0, 1)
  )
  
  sims_n1000[[i]] <- lm(data = dat, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) 
  
}


# Simulate 1000 studies at n = 150 from a multivariate normal distribution
n = 150
set.seed(89)
sims_n150 <- list()

for (i in 1:1000) {
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_strong, beta_moderate, beta_small, beta_negligible,
                           beta_strong, 1, 0, 0, 0,
                           beta_moderate, 0, 1, 0, 0,
                           beta_small, 0, 0, 1, 0,
                           beta_negligible, 0, 0, 0, 1)
  )
  
  sims_n150[[i]] <- lm(data = dat, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) 
  
}


# Simulate 1000 studies at n = 50 from a multivariate normal distribution
n = 50
set.seed(89)
sims_n50 <- list()

for (i in 1:1000) {
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_strong, beta_moderate, beta_small, beta_negligible,
                           beta_strong, 1, 0, 0, 0,
                           beta_moderate, 0, 1, 0, 0,
                           beta_small, 0, 0, 1, 0,
                           beta_negligible, 0, 0, 0, 1)
  )
  
  sims_n50[[i]] <- lm(data = dat, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) 
  
}



# Collate the coefficients, s.e.s, and p-values from each of the simulated datasets
# Mutate these to long format, tidy up output and make naming conventions consistent

# Coefficients
coefs_sims_n1000 <- bind_rows(coef(sims_n1000[[1]]))
for(i in 2:1000) {
  coefs_sims_n1000 <- bind_rows(coefs_sims_n1000, coef(sims_n1000[[i]]))
}

coefs_sims_n150 <- bind_rows(coef(sims_n150[[1]]))
for(i in 2:1000) {
  coefs_sims_n150 <- bind_rows(coefs_sims_n150, coef(sims_n150[[i]]))
}

coefs_sims_n50 <- bind_rows(coef(sims_n50[[1]]))
for(i in 2:1000) {
  coefs_sims_n50 <- bind_rows(coefs_sims_n50, coef(sims_n50[[i]]))
}


# SEs
se_sims_n1000 <- bind_rows(summary(sims_n1000[[1]])$coefficients[,2])
for (i in 2:1000) {
  se_sims_n1000 <- bind_rows(se_sims_n1000, summary(sims_n1000[[i]])$coefficients[,2])
}

se_sims_n150 <- bind_rows(summary(sims_n150[[1]])$coefficients[,2])
for (i in 2:1000) {
  se_sims_n150 <- bind_rows(se_sims_n150, summary(sims_n150[[i]])$coefficients[,2])
}

se_sims_n50 <- bind_rows(summary(sims_n50[[1]])$coefficients[,2])
for (i in 2:1000) {
  se_sims_n50 <- bind_rows(se_sims_n50, summary(sims_n50[[i]])$coefficients[,2])
}


# p-values
p_sims_n1000 <- bind_rows(summary(sims_n1000[[1]])$coefficients[,4])
for (i in 2:1000) {
  p_sims_n1000 <- bind_rows(p_sims_n1000, summary(sims_n1000[[i]])$coefficients[,4])
}

p_sims_n150 <- bind_rows(summary(sims_n150[[1]])$coefficients[,4])
for (i in 2:1000) {
  p_sims_n150 <- bind_rows(p_sims_n150, summary(sims_n150[[i]])$coefficients[,4])
}

p_sims_n50 <- bind_rows(summary(sims_n50[[1]])$coefficients[,4])
for (i in 2:1000) {
  p_sims_n50 <- bind_rows(p_sims_n50, summary(sims_n50[[i]])$coefficients[,4])
}


# Pivot data for plotting

coefs_sims_n1000_l <- coefs_sims_n1000 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z) %>%
  filter(!name %in% c("(Intercept)"))

se_sims_n1000_l <- se_sims_n1000 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "se") %>%
  filter(!name %in% c("(Intercept)"))

p_sims_n1000_l <- p_sims_n1000 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "p") %>%
  filter(!name %in% c("(Intercept)"))

# Join coefficients, SEs, and p-values
coefs_sims_n1000_l <- left_join(coefs_sims_n1000_l, se_sims_n1000_l, by = c("row", "name")) %>% left_join(., p_sims_n1000_l, by = c("row", "name"))

coefs_sims_n1000_l <- coefs_sims_n1000_l %>%
  mutate(name = factor(name, levels = c("poverty_z", "inequal_z", "spending_z", "staffing_z")))

coefs_sims_n1000_l <- coefs_sims_n1000_l %>%
  mutate(hline = case_when(name == "poverty_z" ~ beta_strong,
                           name == "inequal_z" ~ beta_moderate,
                           name == "spending_z" ~ beta_small,
                           name == "staffing_z" ~ beta_negligible))


coefs_sims_n150_l <- coefs_sims_n150 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z) %>%
  filter(!name %in% c("(Intercept)"))

se_sims_n150_l <- se_sims_n150 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "se") %>%
  filter(!name %in% c("(Intercept)"))

p_sims_n150_l <- p_sims_n150 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "p") %>%
  filter(!name %in% c("(Intercept)"))

# Join coefficients, SEs, and p-values
coefs_sims_n150_l <- left_join(coefs_sims_n150_l, se_sims_n150_l, by = c("row", "name")) %>% left_join(., p_sims_n150_l, by = c("row", "name"))

coefs_sims_n150_l <- coefs_sims_n150_l %>%
  mutate(name = factor(name, levels = c("poverty_z", "inequal_z", "spending_z", "staffing_z")))

coefs_sims_n150_l <- coefs_sims_n150_l %>%
  mutate(hline = case_when(name == "poverty_z" ~ beta_strong,
                           name == "inequal_z" ~ beta_moderate,
                           name == "spending_z" ~ beta_small,
                           name == "staffing_z" ~ beta_negligible))


coefs_sims_n50_l <- coefs_sims_n50 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z) %>%
  filter(!name %in% c("(Intercept)"))

se_sims_n50_l <- se_sims_n50 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "se") %>%
  filter(!name %in% c("(Intercept)"))

p_sims_n50_l <- p_sims_n50 %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "name", values_to = "p") %>%
  filter(!name %in% c("(Intercept)"))


coefs_sims_n50_l <- left_join(coefs_sims_n50_l, se_sims_n50_l, by = c("row", "name")) %>% left_join(., p_sims_n50_l, by = c("row", "name"))

coefs_sims_n50_l <- coefs_sims_n50_l %>%
  mutate(name = factor(name, levels = c("poverty_z", "inequal_z", "spending_z", "staffing_z")))

# Set true effect sizes that data were drawn from
coefs_sims_n50_l <- coefs_sims_n50_l %>%
  mutate(hline = case_when(name == "poverty_z" ~ beta_strong,
                           name == "inequal_z" ~ beta_moderate,
                           name == "spending_z" ~ beta_small,
                           name == "staffing_z" ~ beta_negligible))

# Join simulated datasets from different sample sizes together 
sims_freq <- bind_rows(coefs_sims_n1000_l, coefs_sims_n150_l, coefs_sims_n50_l, .id = "n") %>%
  mutate(n = case_when(n == 1 ~ "1000",
                       n == 2 ~ "150",
                       n == 3 ~ "50"))

# Create a plot that shows the differences in decisions made about the 
# size of an effect from many repeated simulations
nsim_compare_plot <- sims_freq %>% 
  group_by(n, name) %>%
  slice(1:500) %>%
  mutate(
    n = case_when(n == 1000 ~ "N = 1000",
                  n == 150 ~ "N = 150",
                  n == 50 ~ "N = 50"),
    name = case_when(name == "poverty_z" ~ "Poverty",
                     name == "inequal_z" ~ "Inequality",
                     name == "spending_z" ~ "Spending",
                     name == "staffing_z" ~ "Staffing")
  ) %>%
  mutate(
    name = factor(name, levels = c("Poverty", "Inequality", "Spending", "Staffing")) 
  ) %>%
  ggplot() +
  geom_point(aes(x = row, y = value, 
                 col = ifelse(p < 0.05, "black", "grey")), 
             size = 0.3) +
  geom_segment(aes(x = row, xend = row, y = value + 1.96*se, 
                   yend = value - 1.96*se, 
                   col = ifelse(p < 0.05, "black", "grey")), 
               size = 0.2) +
  # True effects
  geom_hline(aes(yintercept = hline), col = "white", size = 1) +
  geom_hline(aes(yintercept = hline), col = "black", size = 0.6) +
  ylab("B Coefficient") +
  xlab("Simulation") +
  facet_grid(n~name) +
  scale_colour_identity() +
  theme_classic()

# Save plot
ggsave(nsim_compare_plot, filename = "plots/nsim_compare_plot.png", units = "in", dpi = 300,
       width = 8.25, height = 11.75/3, scale = 1.4)


# Generate and save tables which show the oroportions accepted/rejected at different sample sizes
janitor::tabyl(coefs_sims_n1000_l %>% mutate(p = ifelse(p < 0.05, "Reject", "Fail to Reject")), 
               name, p) %>% janitor::adorn_percentages("row") %>%
  write_csv("tabs_out/prop_rej_n1000.csv")

janitor::tabyl(coefs_sims_n150_l %>% mutate(p = ifelse(p < 0.05, "Reject", "Fail to Reject")), 
               name, p) %>% janitor::adorn_percentages("row") %>%
  write_csv("tabs_out/prop_rej_n150.csv")

janitor::tabyl(coefs_sims_n50_l %>% mutate(p = ifelse(p < 0.05, "Reject", "Fail to Reject")), 
               name, p) %>% janitor::adorn_percentages("row") %>%
  write_csv("tabs_out/prop_rej_n50.csv")



# Simulate pooled data using the faux library

# define parameters
time_n = 20  # number of time points
state_n = 150   # number of states
b0 = 0       # intercept

# Sum to 1 for normal distribution
# Use R-squared from multivariate normal simulation to get correct 
# variance for the generated statistic to have a normal distribution N(0,1)
u0s_sd = 0.5 + 0.5*0.2725   # random intercept SD for states
sigma_sd = 0.5 + 0.5*0.2725 # error SD
sigma_sd_i = 0.5 + 0.2725

# set up data structure
ml_data <- add_random(time_la = time_n, state = state_n) %>%
  # add and recode categorical variables
  add_between("time_la", year = 1:10) %>%
  # add random effects - all uncorrelated
  add_ranef("state", u0s_care = u0s_sd) %>%
  add_ranef("state", u0s_poverty = u0s_sd) %>%
  add_ranef("state", u0s_inequal = u0s_sd) %>%
  add_ranef("state", u0s_spending = u0s_sd) %>%
  add_ranef("state", u0s_staffing = u0s_sd) %>%
  add_ranef(sigma_care = sigma_sd,
            sigma_poverty = sigma_sd_i,
            sigma_inequal = sigma_sd_i,
            sigma_spending = sigma_sd_i,
            sigma_staffing = sigma_sd_i) %>%
  mutate(
    # Calculate IVs assuming the same higher level deviation
    poverty_z = b0 + u0s_poverty + sigma_poverty,
    inequal_z = b0 + u0s_inequal + sigma_inequal,
    spending_z = b0 + u0s_spending + sigma_spending,
    staffing_z = b0 + u0s_staffing + sigma_staffing,
    # calculate DV
    care_z = b0 + beta_strong*poverty_z + beta_moderate*inequal_z + beta_small*spending_z + beta_negligible*staffing_z + u0s_care + sigma_care
    ) 

# Check simulated data parameters
hist(ml_data$care_z)
sd(ml_data$care_z)
mean(ml_data$care_z)
sd(ml_data$poverty_z)
sd(ml_data$inequal_z)
sd(ml_data$staffing_z)

# Check parameters are retrievable
mlm_mod_nol2 <- glmmTMB::glmmTMB(data = ml_data,
                 formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)

summary(mlm_mod_nol2)


ml_data <- ml_data %>%
  group_by(state) %>%
  # Group mean centre variables
  mutate_at(vars(poverty_z:care_z), list(gr_mean = ~mean(.))) %>% 
  mutate_at(vars(poverty_z:care_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
  # Grand mean centre and cluster mean variables (level 2)
  ungroup() %>%
  mutate_at(vars(poverty_z_gr_mean:care_z_gr_mean), ~scale(., center = TRUE, scale = FALSE)[,1]) 

mlm_mod <- glmmTMB::glmmTMB(data = ml_data,
                                 formula = care_z ~ poverty_z_gr_mean + inequal_z_gr_mean + spending_z_gr_mean + staffing_z_gr_mean + # level 2 group means
                                                    poverty_z_grmc + inequal_z_grmc + spending_z_grmc + staffing_z_grmc + (1|state)) # level 1 grand mean centered


# shows how standard errors change

summary(mlm_mod)


# simulate where l2 effect is opposite

# set up data structure
ml_data <- add_random(time_la = time_n, state = state_n) %>%
  # add and recode categorical variables
  add_between("time_la", year = 1:10) %>%
  # add random effects - all uncorrelated
  add_ranef("state", u0s_care = u0s_sd) %>%
  add_ranef("state", u0s_poverty = u0s_sd) %>%
  add_ranef("state", u0s_inequal = u0s_sd) %>%
  add_ranef("state", u0s_spending = u0s_sd) %>%
  add_ranef("state", u0s_staffing = u0s_sd) %>%
  add_ranef(sigma_care = sigma_sd,
            sigma_poverty = sigma_sd_i,
            sigma_inequal = sigma_sd_i,
            sigma_spending = sigma_sd_i,
            sigma_staffing = sigma_sd_i) %>%
  mutate(
    # Calculate IVs assuming the same higher level deviation
    poverty_z = b0 + u0s_poverty + sigma_poverty,
    inequal_z = b0 + u0s_inequal + sigma_inequal,
    spending_z = b0 + u0s_spending + sigma_spending,
    staffing_z = b0 + u0s_staffing + sigma_staffing) %>%
  group_by(state) %>%
  # Group mean centre variables
  mutate_at(vars(poverty_z:staffing_z), list(gr_mean = ~mean(.))) %>% 
  mutate_at(vars(poverty_z:staffing_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
  # Grand mean centre variables
  ungroup() %>%
  mutate_at(vars(poverty_z_gr_mean:staffing_z_gr_mean), list(gmc = ~scale(., center = TRUE, scale = FALSE)[,1])) %>%
  mutate(
    # calculate DV
    care_z = b0 + beta_strong*poverty_z_grmc + beta_moderate*inequal_z_grmc + beta_small*spending_z_grmc + beta_negligible*staffing_z_grmc + 
      (-1*beta_strong)*poverty_z_gr_mean_gmc + (-1*beta_moderate)*inequal_z_gr_mean_gmc + (-1*beta_small)*spending_z_gr_mean_gmc + (-1*beta_negligible)*staffing_z_gr_mean_gmc + 
      u0s_care + sigma_care
  ) 

# Check parameters are retrivable under correct model
mlm_mod_nol2 <- glmmTMB::glmmTMB(data = ml_data,
                                 formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)

summary(mlm_mod_nol2)

mlm_mod <- glmmTMB::glmmTMB(data = ml_data,
                            formula = care_z ~ poverty_z_gr_mean_gmc + inequal_z_gr_mean_gmc + spending_z_gr_mean_gmc + staffing_z_gr_mean_gmc + 
                              poverty_z_grmc + inequal_z_grmc + spending_z_grmc + staffing_z_grmc + (1|state))

summary(mlm_mod)



# Simulate 200 studies from MLM - simple nested data
time_n = 20  # number of time points
state_n = 50   # number of states
b0 = 0       # intercept

# Sum to 1 for normal distribution
u0s_sd = 0.5 + 0.5*0.2725   # random intercept SD for states
sigma_sd = 0.5 + 0.5*0.2725 # error SD
sigma_sd_i = 0.5 + 0.5*0.2725

non_mlm_mods <- list()
mlm_mods <- list()

# No level 1 effect, only level 2
for (i in 1:200) {
set.seed(89*i)
print(paste("sim", i))
# set up data structure
ml_data <- add_random(time_la = time_n, state = state_n) %>%
  # add and recode categorical variables
  add_between("time_la", year = 1:10) %>%
  # add random effects - all uncorrelated
  add_ranef("state", u0s_care = u0s_sd) %>%
  add_ranef("state", u0s_poverty = u0s_sd) %>%
  add_ranef("state", u0s_inequal = u0s_sd) %>%
  add_ranef("state", u0s_spending = u0s_sd) %>%
  add_ranef("state", u0s_staffing = u0s_sd) %>%
  add_ranef(sigma_care = sigma_sd,
            sigma_poverty = sigma_sd_i,
            sigma_inequal = sigma_sd_i,
            sigma_spending = sigma_sd_i,
            sigma_staffing = sigma_sd_i) %>%
  mutate(
    # Calculate IVs assuming the same higher level deviation
    poverty_z = b0 + u0s_poverty + sigma_poverty,
    inequal_z = b0 + u0s_inequal + sigma_inequal,
    spending_z = b0 + u0s_spending + sigma_spending,
    staffing_z = b0 + u0s_staffing + sigma_staffing
  ) %>%
  group_by(state) %>%
  # Group mean centre variables
  mutate_at(vars(poverty_z:staffing_z), list(gr_mean = ~mean(.))) %>% 
  mutate_at(vars(poverty_z:staffing_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
  # Grand mean centre variables
  ungroup() %>%
  mutate_at(vars(poverty_z_gr_mean:staffing_z_gr_mean), list(gmc = ~scale(., center = TRUE, scale = FALSE)[,1])) %>%
  mutate(
    # calculate DV
    care_z = b0 + 0*poverty_z_grmc + 0*inequal_z_grmc + 0*spending_z_grmc + 0*staffing_z_grmc + 
      beta_strong*poverty_z_gr_mean_gmc + beta_moderate*inequal_z_gr_mean_gmc + beta_small*spending_z_gr_mean_gmc + beta_negligible*staffing_z_gr_mean_gmc + 
      u0s_care + sigma_care
  ) 

non_mlm_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                                 formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)

# ml_data <- ml_data %>%
#   group_by(state) %>%
#   # Group mean centre variables
#   mutate_at(vars(poverty_z:care_z), list(gr_mean = ~mean(.))) %>% 
#   mutate_at(vars(poverty_z:care_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
#   # Grand mean centre variables
#   ungroup() %>%
#   mutate_at(vars(poverty_z:care_z), ~scale(., center = TRUE, scale = FALSE)[,1]) 

mlm_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                            formula = care_z ~ poverty_z_gr_mean_gmc + inequal_z_gr_mean_gmc + spending_z_gr_mean_gmc + staffing_z_gr_mean_gmc + poverty_z_grmc + inequal_z_grmc + spending_z_grmc + staffing_z_grmc + (1|state))

}

# Example where level 1 effect differs from level two effect (complete inverse)

non_mlm_suppression_mods <- list()
mlm_suppression_mods <- list()

# Opposite level 1 and level 2 effects
for (i in 1:200) {
  set.seed(89*i)
  print(paste("sim", i))
  # set up data structure
  ml_data <- add_random(time_la = time_n, state = state_n) %>%
    # add and recode categorical variables
    add_between("time_la", year = 1:10) %>%
    # add random effects - all uncorrelated
    add_ranef("state", u0s_care = u0s_sd) %>%
    add_ranef("state", u0s_poverty = u0s_sd) %>%
    add_ranef("state", u0s_inequal = u0s_sd) %>%
    add_ranef("state", u0s_spending = u0s_sd) %>%
    add_ranef("state", u0s_staffing = u0s_sd) %>%
    add_ranef(sigma_care = sigma_sd,
              sigma_poverty = sigma_sd_i,
              sigma_inequal = sigma_sd_i,
              sigma_spending = sigma_sd_i,
              sigma_staffing = sigma_sd_i) %>%
    mutate(
      # Calculate IVs assuming the same higher level deviation
      poverty_z = b0 + u0s_poverty + sigma_poverty,
      inequal_z = b0 + u0s_inequal + sigma_inequal,
      spending_z = b0 + u0s_spending + sigma_spending,
      staffing_z = b0 + u0s_staffing + sigma_staffing) %>%
    group_by(state) %>%
    # Group mean centre variables
    mutate_at(vars(poverty_z:staffing_z), list(gr_mean = ~mean(.))) %>% 
    mutate_at(vars(poverty_z:staffing_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
    # Grand mean centre variables
    ungroup() %>%
    mutate_at(vars(poverty_z_gr_mean:staffing_z_gr_mean), list(gmc = ~scale(., center = TRUE, scale = FALSE)[,1])) %>%
    mutate(
      # calculate DV
      care_z = b0 + (-1*beta_strong*poverty_z_grmc) + (-1*beta_moderate)*inequal_z_grmc + (-1*beta_small)*spending_z_grmc + (-1*beta_negligible)*staffing_z_grmc + 
        beta_strong*poverty_z_gr_mean_gmc + beta_moderate*inequal_z_gr_mean_gmc + beta_small*spending_z_gr_mean_gmc + beta_negligible*staffing_z_gr_mean_gmc + 
        u0s_care + sigma_care
    ) 
  
  non_mlm_suppression_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                                   formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
  
  mlm_suppression_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                              formula = care_z ~ poverty_z_gr_mean_gmc + inequal_z_gr_mean_gmc + spending_z_gr_mean_gmc + staffing_z_gr_mean_gmc + # level 2
                                        poverty_z_grmc + inequal_z_grmc + spending_z_grmc + staffing_z_grmc + (1|state)) # level 1
  
}


# Visualise results of different modelling strategies with pooled data,
# collate coefficients, SEs, and p-values

# coefficients
summary(non_mlm_mods[[1]])$coefficients$cond[,1]

# std errors
summary(non_mlm_mods[[1]])$coefficients$cond[,2]

# p values
summary(non_mlm_mods[[1]])$coefficients$cond[,4]

non_mlm_coefs <- bind_rows(summary(non_mlm_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  non_mlm_coefs <- bind_rows(non_mlm_coefs, summary(non_mlm_mods[[i]])$coefficients$cond[,1])
}

non_mlm_ses <- bind_rows(summary(non_mlm_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  non_mlm_ses <- bind_rows(non_mlm_ses, summary(non_mlm_mods[[i]])$coefficients$cond[,2])
}

non_mlm_p <- bind_rows(summary(non_mlm_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  non_mlm_p <- bind_rows(non_mlm_p, summary(non_mlm_mods[[i]])$coefficients$cond[,4])
}

non_mlm_coefs_l <- non_mlm_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "coefficient")

non_mlm_ses_l <- non_mlm_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "std_err")

non_mlm_p_l <- non_mlm_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "p")

non_mlm_l <- left_join(non_mlm_coefs_l, non_mlm_ses_l, by = c("sim", "param")) %>% left_join(non_mlm_p_l, by = c("sim", "param"))

# mlm_outputs

mlm_coefs <- bind_rows(summary(mlm_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  mlm_coefs <- bind_rows(mlm_coefs, summary(mlm_mods[[i]])$coefficients$cond[,1])
}

mlm_ses <- bind_rows(summary(mlm_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  mlm_ses <- bind_rows(mlm_ses, summary(mlm_mods[[i]])$coefficients$cond[,2])
}

mlm_p <- bind_rows(summary(mlm_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  mlm_p <- bind_rows(mlm_p, summary(mlm_mods[[i]])$coefficients$cond[,4])
}

mlm_coefs_l <- mlm_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "coefficient")

mlm_ses_l <- mlm_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "std_err")

mlm_p_l <- mlm_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "p")

mlm_l <- left_join(mlm_coefs_l, mlm_ses_l, by = c("sim", "param")) %>% left_join(mlm_p_l, by = c("sim", "param"))

mlm_l <- mlm_l %>%
  mutate(
    param = str_remove_all(param, "_gr_mean_gmc")
  )

left_join(mlm_l, non_mlm_l, by = c("sim", "param"), suffix = c("_mlm", "_lm")) 
ml_comparison_sims <- bind_rows(non_mlm_l, mlm_l, .id = "modtype") %>%
  mutate(modtype = ifelse(modtype == 1, "lm", "mlm"))

ml_comparison_sims <- ml_comparison_sims %>% 
  group_by(modtype, param) %>%
  mutate(
    row = row_number()
  )

ml_comparison_sims <- ml_comparison_sims %>%
  mutate(param = factor(param, levels = c("(Intercept)", "poverty_z", "inequal_z", "spending_z", "staffing_z",
                                          "poverty_z_grmc", "inequal_z_grmc", "spending_z_grmc", "staffing_z_grmc")))

# Add single time period data for comparison
nonpooled <- sims_freq %>%
  rename(modtype = n, coefficient = value, param = name, std_err = se) %>%
  select(-hline) %>%
  mutate(sim = row) %>%
  filter(modtype == 50) %>%
  mutate(modtype = "non-pooled") %>% 
  group_by(param) %>%
  slice(1:200) %>%
  ungroup()

ml_comparison_sims <- ml_comparison_sims %>% 
  filter(!param %in% "(Intercept)") %>%
  bind_rows(., nonpooled) %>%
  mutate(hline = case_when(param == "poverty_z" ~ beta_strong,
                           param == "inequal_z" ~ beta_moderate,
                           param == "spending_z" ~ beta_small,
                           param == "staffing_z" ~ beta_negligible,
                           param == "poverty_z_grmc" ~ 0,
                           param == "inequal_z_grmc" ~ 0,
                           param == "spending_z_grmc" ~ 0,
                           param == "staffing_z_grmc" ~ 0
                           ))

ml_comparison_sims <- ml_comparison_sims %>%
  mutate(
    modtype = case_when(modtype == "lm" ~ "Pooled LM",
                        modtype == "mlm" ~ "Pooled MLM CWC(M)",
                        modtype == "non-pooled" ~ "Non-pooled LM")
  ) 

pooled_plot_a <- ml_comparison_sims %>%
  mutate(param = case_when(param == "poverty_z" ~ "Poverty\n(Between)",
                           param == "inequal_z" ~ "Inequality\n(Between)",
                           param == "spending_z" ~ "Spending\n(Between)",
                           param == "staffing_z" ~ "Staffing\n(Between)",
                           param == "poverty_z_grmc" ~ "Poverty\n(Within)",
                           param == "inequal_z_grmc" ~ "Inequality\n(Within)",
                           param == "spending_z_grmc" ~ "Spending\n(Within)",
                           param == "staffing_z_grmc" ~ "Staffing\n(Within)",
         )
  ) %>%
  mutate(param = factor(param, levels = c("Poverty\n(Between)", "Inequality\n(Between)",
                                          "Spending\n(Between)", "Staffing\n(Between)",
                                          "Poverty\n(Within)", "Inequality\n(Within)",
                                          "Spending\n(Within)", "Staffing\n(Within)"))) %>%
  mutate(modtype = factor(modtype, levels = c("Pooled LM", "Pooled MLM CWC(M)", "Non-pooled LM"))) %>%
  ggplot() +
  geom_point(aes(x = row, y = coefficient, 
                 col = ifelse(p < 0.05, "black", "grey")), 
             size = 0.3) +
  geom_segment(aes(x = row, xend = row, y = coefficient + 1.96*std_err, 
                   yend = coefficient - 1.96*std_err, 
                   col = ifelse(p < 0.05, "black", "grey")), 
               size = 0.2) +
  # True effects
  geom_hline(aes(yintercept = hline), col = "white", size = 1) +
  geom_hline(aes(yintercept = hline), col = "black", size = 0.6) +
  ylab("B Coefficient") +
  xlab("Simulation") +
  ggtitle("a) Within effects equal to zero") +
  facet_grid(modtype~param) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(1, 200)) +
  theme_classic()


ml_comparison_sims %>%
  mutate(
    lower_ci = coefficient - 1.96*std_err,
    upper_ci = coefficient + 1.96*std_err
  ) %>%
  filter(param == "poverty_z" & modtype == "Pooled LM") %>%
  mutate(
    contains_beta_strong = ifelse(lower_ci < beta_strong & beta_strong < upper_ci, TRUE, FALSE)
  ) %>%
  .$contains_beta_strong %>% sum(., na.rm = TRUE)

119 / 200 # Only around 59.5% of CIs contain the true parameter compared to (unbiased but incorrect precision)
191 / 200 # 95.5% in the non-pooled model

ml_comparison_sims %>%
  mutate(
    lower_ci = coefficient - 1.96*std_err,
    upper_ci = coefficient + 1.96*std_err
  ) %>%
  filter(param == "inequal_z" & modtype == "Pooled LM") %>%
  mutate(
    contains_beta_moderate = ifelse(lower_ci < beta_moderate & beta_moderate < upper_ci, TRUE, FALSE)
  ) %>%
  .$contains_beta_moderate %>% sum(., na.rm = TRUE)

ml_comparison_sims %>%
  mutate(
    lower_ci = coefficient - 1.96*std_err,
    upper_ci = coefficient + 1.96*std_err
  ) %>%
  filter(param == "inequal_z" & modtype == "Non-pooled LM") %>%
  mutate(
    contains_beta_moderate = ifelse(lower_ci < beta_moderate & beta_moderate < upper_ci, TRUE, FALSE)
  ) %>%
  .$contains_beta_moderate %>% sum(., na.rm = TRUE)

113 / 200 # 56.5% 
186 / 200 # 93%


# plot for data with opposite within-LA and between-LA effects

# coefficients
summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,1]

# std errors
summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,2]

# p values
summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,4]

non_mlm_suppression_coefs <- bind_rows(summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  non_mlm_suppression_coefs <- bind_rows(non_mlm_suppression_coefs, summary(non_mlm_suppression_mods[[i]])$coefficients$cond[,1])
}

non_mlm_suppression_ses <- bind_rows(summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  non_mlm_suppression_ses <- bind_rows(non_mlm_suppression_ses, summary(non_mlm_suppression_mods[[i]])$coefficients$cond[,2])
}

non_mlm_suppression_p <- bind_rows(summary(non_mlm_suppression_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  non_mlm_suppression_p <- bind_rows(non_mlm_suppression_p, summary(non_mlm_suppression_mods[[i]])$coefficients$cond[,4])
}

non_mlm_suppression_coefs_l <- non_mlm_suppression_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "coefficient")

non_mlm_suppression_ses_l <- non_mlm_suppression_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "std_err")

non_mlm_suppression_p_l <- non_mlm_suppression_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "p")

non_mlm_suppression_l <- left_join(non_mlm_suppression_coefs_l, non_mlm_suppression_ses_l, by = c("sim", "param")) %>% left_join(non_mlm_suppression_p_l, by = c("sim", "param"))

# mlm_outputs

mlm_suppression_coefs <- bind_rows(summary(mlm_suppression_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  mlm_suppression_coefs <- bind_rows(mlm_suppression_coefs, summary(mlm_suppression_mods[[i]])$coefficients$cond[,1])
}

mlm_suppression_ses <- bind_rows(summary(mlm_suppression_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  mlm_suppression_ses <- bind_rows(mlm_suppression_ses, summary(mlm_suppression_mods[[i]])$coefficients$cond[,2])
}

mlm_suppression_p <- bind_rows(summary(mlm_suppression_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  mlm_suppression_p <- bind_rows(mlm_suppression_p, summary(mlm_suppression_mods[[i]])$coefficients$cond[,4])
}

mlm_suppression_coefs_l <- mlm_suppression_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "coefficient")

mlm_suppression_ses_l <- mlm_suppression_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "std_err")

mlm_suppression_p_l <- mlm_suppression_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "p")

mlm_suppression_l <- left_join(mlm_suppression_coefs_l, mlm_suppression_ses_l, by = c("sim", "param")) %>% left_join(mlm_suppression_p_l, by = c("sim", "param"))

mlm_suppression_l <- mlm_suppression_l %>%
  mutate(
    param = case_when(param == "poverty_z_gr_mean_gmc"  ~ "poverty_z",
                      param == "inequal_z_gr_mean_gmc"  ~ "inequal_z",
                      param == "spending_z_gr_mean_gmc" ~ "spending_z",
                      param == "staffing_z_gr_mean_gmc" ~ "staffing_z",
                      TRUE ~ param)
  )


left_join(mlm_suppression_l, non_mlm_suppression_l, by = c("sim", "param"), suffix = c("_mlm", "_lm")) 
ml_suppression_comparison_sims <- bind_rows(non_mlm_suppression_l, mlm_suppression_l, .id = "modtype") %>%
  mutate(modtype = ifelse(modtype == 1, "lm", "mlm"))

ml_suppression_comparison_sims <- ml_suppression_comparison_sims %>% 
  group_by(modtype, param) %>%
  mutate(
    row = row_number()
  )

ml_suppression_comparison_sims <- ml_suppression_comparison_sims %>%
  mutate(param = factor(param, levels = c("(Intercept)", "poverty_z", "inequal_z", "spending_z", "staffing_z",
                                          "poverty_z_grmc", "inequal_z_grmc", "spending_z_grmc", "staffing_z_grmc")))

# Add single time period data for comparison
nonpooled <- sims_freq %>%
  rename(modtype = n, coefficient = value, param = name, std_err = se) %>%
  select(-hline) %>%
  mutate(sim = row) %>%
  filter(modtype == 50) %>%
  mutate(modtype = "non-pooled") %>% 
  group_by(param) %>%
  slice(1:200) %>%
  ungroup()

ml_suppression_comparison_sims <- ml_suppression_comparison_sims %>% 
  filter(!param %in% "(Intercept)") %>%
  bind_rows(., nonpooled) %>%
  mutate(hline = case_when(param == "poverty_z" ~ beta_strong,
                           param == "inequal_z" ~ beta_moderate,
                           param == "spending_z" ~ beta_small,
                           param == "staffing_z" ~ beta_negligible,
                           param == "poverty_z_grmc" ~ -1*beta_strong,
                           param == "inequal_z_grmc" ~ -1*beta_moderate,
                           param == "spending_z_grmc" ~ -1*beta_small,
                           param == "staffing_z_grmc" ~ -1*beta_negligible
  ))

ml_suppression_comparison_sims <- ml_suppression_comparison_sims %>%
  mutate(
    modtype = case_when(modtype == "lm" ~ "Pooled LM",
                        modtype == "mlm" ~ "Pooled MLM CWC(M)",
                        modtype == "non-pooled" ~ "Non-pooled LM")
  ) 


# Suppression effects
pooled_plot_b <- ml_suppression_comparison_sims %>%
  mutate(modtype = factor(modtype, levels = c("Pooled LM", "Pooled MLM CWC(M)", "Non-pooled LM")),
         param = case_when(param == "poverty_z" ~ "Poverty\n(Between)",
                           param == "inequal_z" ~ "Inequality\n(Between)",
                           param == "spending_z" ~ "Spending\n(Between)",
                           param == "staffing_z" ~ "Staffing\n(Between)",
                           param == "poverty_z_grmc" ~ "Poverty\n(Within)",
                           param == "inequal_z_grmc" ~ "Inequality\n(Within)",
                           param == "spending_z_grmc" ~ "Spending\n(Within)",
                           param == "staffing_z_grmc" ~ "Staffing\n(Within)",
                           )
         ) %>%
  mutate(param = factor(param, levels = c("Poverty\n(Between)", "Inequality\n(Between)",
                                          "Spending\n(Between)", "Staffing\n(Between)",
                                          "Poverty\n(Within)", "Inequality\n(Within)",
                                          "Spending\n(Within)", "Staffing\n(Within)"))) %>%
  ggplot() +
  geom_point(aes(x = row, y = coefficient, 
                 col = ifelse(p < 0.05, "black", "grey")), 
             size = 0.3) +
  geom_segment(aes(x = row, xend = row, y = coefficient + 1.96*std_err, 
                   yend = coefficient - 1.96*std_err, 
                   col = ifelse(p < 0.05, "black", "grey")), 
               size = 0.2) +
  # True effects
  geom_hline(aes(yintercept = hline), col = "white", size = 1) +
  geom_hline(aes(yintercept = hline), col = "black", size = 0.6) +
  ylab("B Coefficient") +
  xlab("Simulation") +
  ggtitle("b) Opposite between & within effects") +
  facet_grid(modtype~param) +
  scale_colour_identity() +
  theme_classic()



# Double effect - one within one between

non_mlm_reinforce_mods <- list()
mlm_reinforce_mods <- list()

for (i in 1:200) {
  set.seed(89*i)
  print(paste("sim", i))
  # set up data structure
  ml_data <- add_random(time_la = time_n, state = state_n) %>%
    # add and recode categorical variables
    add_between("time_la", year = 1:10) %>%
    # add random effects - all uncorrelated
    add_ranef("state", u0s_care = u0s_sd) %>%
    add_ranef("state", u0s_poverty = u0s_sd) %>%
    add_ranef("state", u0s_inequal = u0s_sd) %>%
    add_ranef("state", u0s_spending = u0s_sd) %>%
    add_ranef("state", u0s_staffing = u0s_sd) %>%
    add_ranef(sigma_care = sigma_sd,
              sigma_poverty = sigma_sd_i,
              sigma_inequal = sigma_sd_i,
              sigma_spending = sigma_sd_i,
              sigma_staffing = sigma_sd_i) %>%
    mutate(
      # Calculate IVs assuming the same higher level deviation
      poverty_z = b0 + u0s_poverty + sigma_poverty,
      inequal_z = b0 + u0s_inequal + sigma_inequal,
      spending_z = b0 + u0s_spending + sigma_spending,
      staffing_z = b0 + u0s_staffing + sigma_staffing) %>%
    group_by(state) %>%
    # Group mean centre variables
    mutate_at(vars(poverty_z:staffing_z), list(gr_mean = ~mean(.))) %>% 
    mutate_at(vars(poverty_z:staffing_z), list(grmc = ~scale(., scale=FALSE)[,1] )) %>%
    # Grand mean centre variables
    ungroup() %>%
    mutate_at(vars(poverty_z_gr_mean:staffing_z_gr_mean), list(gmc = ~scale(., center = TRUE, scale = FALSE)[,1])) %>%
    mutate(
      # calculate DV
      care_z = b0 + beta_strong*poverty_z_grmc + beta_moderate*inequal_z_grmc + beta_small*spending_z_grmc + beta_negligible*staffing_z_grmc + 
        beta_strong*poverty_z_gr_mean_gmc + beta_moderate*inequal_z_gr_mean_gmc + beta_small*spending_z_gr_mean_gmc + beta_negligible*staffing_z_gr_mean_gmc + 
        u0s_care + sigma_care
    ) 
  
  non_mlm_reinforce_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                                                    formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
  
  mlm_reinforce_mods[[i]] <- glmmTMB::glmmTMB(data = ml_data,
                                                formula = care_z ~ poverty_z_gr_mean_gmc + inequal_z_gr_mean_gmc + spending_z_gr_mean_gmc + staffing_z_gr_mean_gmc + # level 2
                                                poverty_z_grmc + inequal_z_grmc + spending_z_grmc + staffing_z_grmc + (1|state)) # level 1
  
}



# coefficients
summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,1]

# std errors
summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,2]

# p values
summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,4]

non_mlm_reinforce_coefs <- bind_rows(summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  non_mlm_reinforce_coefs <- bind_rows(non_mlm_reinforce_coefs, summary(non_mlm_reinforce_mods[[i]])$coefficients$cond[,1])
}

non_mlm_reinforce_ses <- bind_rows(summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  non_mlm_reinforce_ses <- bind_rows(non_mlm_reinforce_ses, summary(non_mlm_reinforce_mods[[i]])$coefficients$cond[,2])
}

non_mlm_reinforce_p <- bind_rows(summary(non_mlm_reinforce_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  non_mlm_reinforce_p <- bind_rows(non_mlm_reinforce_p, summary(non_mlm_reinforce_mods[[i]])$coefficients$cond[,4])
}

non_mlm_reinforce_coefs_l <- non_mlm_reinforce_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "coefficient")

non_mlm_reinforce_ses_l <- non_mlm_reinforce_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "std_err")

non_mlm_reinforce_p_l <- non_mlm_reinforce_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z, names_to = "param", values_to = "p")

non_mlm_reinforce_l <- left_join(non_mlm_reinforce_coefs_l, non_mlm_reinforce_ses_l, by = c("sim", "param")) %>% left_join(non_mlm_reinforce_p_l, by = c("sim", "param"))

# mlm_outputs

mlm_reinforce_coefs <- bind_rows(summary(mlm_reinforce_mods[[1]])$coefficients$cond[,1])

for (i in 2:200) {
  mlm_reinforce_coefs <- bind_rows(mlm_reinforce_coefs, summary(mlm_reinforce_mods[[i]])$coefficients$cond[,1])
}

mlm_reinforce_ses <- bind_rows(summary(mlm_reinforce_mods[[1]])$coefficients$cond[,2])

for (i in 2:200) {
  mlm_reinforce_ses <- bind_rows(mlm_reinforce_ses, summary(mlm_reinforce_mods[[i]])$coefficients$cond[,2])
}

mlm_reinforce_p <- bind_rows(summary(mlm_reinforce_mods[[1]])$coefficients$cond[,4])

for (i in 2:200) {
  mlm_reinforce_p <- bind_rows(mlm_reinforce_p, summary(mlm_reinforce_mods[[i]])$coefficients$cond[,4])
}

mlm_reinforce_coefs_l <- mlm_reinforce_coefs %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "coefficient")

mlm_reinforce_ses_l <- mlm_reinforce_ses %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "std_err")

mlm_reinforce_p_l <- mlm_reinforce_p %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = `(Intercept)`:staffing_z_grmc, names_to = "param", values_to = "p")

mlm_reinforce_l <- left_join(mlm_reinforce_coefs_l, mlm_reinforce_ses_l, by = c("sim", "param")) %>% left_join(mlm_reinforce_p_l, by = c("sim", "param"))

mlm_reinforce_l <- mlm_reinforce_l %>%
  mutate(
    param = case_when(param == "poverty_z_gr_mean_gmc"  ~ "poverty_z",
                      param == "inequal_z_gr_mean_gmc"  ~ "inequal_z",
                      param == "spending_z_gr_mean_gmc" ~ "spending_z",
                      param == "staffing_z_gr_mean_gmc" ~ "staffing_z",
                      TRUE ~ param)
  )


left_join(mlm_reinforce_l, non_mlm_reinforce_l, by = c("sim", "param"), suffix = c("_mlm", "_lm")) 
ml_reinforce_comparison_sims <- bind_rows(non_mlm_reinforce_l, mlm_reinforce_l, .id = "modtype") %>%
  mutate(modtype = ifelse(modtype == 1, "lm", "mlm"))

ml_reinforce_comparison_sims <- ml_reinforce_comparison_sims %>% 
  group_by(modtype, param) %>%
  mutate(
    row = row_number()
  )

ml_reinforce_comparison_sims <- ml_reinforce_comparison_sims %>%
  mutate(param = factor(param, levels = c("(Intercept)", "poverty_z", "inequal_z", "spending_z", "staffing_z",
                                          "poverty_z_grmc", "inequal_z_grmc", "spending_z_grmc", "staffing_z_grmc")))

# Add single time period data for comparison
nonpooled <- sims_freq %>%
  rename(modtype = n, coefficient = value, param = name, std_err = se) %>%
  select(-hline) %>%
  mutate(sim = row) %>%
  filter(modtype == 50) %>%
  mutate(modtype = "non-pooled") %>% 
  group_by(param) %>%
  slice(1:200) %>%
  ungroup()

ml_reinforce_comparison_sims <- ml_reinforce_comparison_sims %>% 
  filter(!param %in% "(Intercept)") %>%
  bind_rows(., nonpooled) %>%
  mutate(hline = case_when(param == "poverty_z" ~ beta_strong,
                           param == "inequal_z" ~ beta_moderate,
                           param == "spending_z" ~ beta_small,
                           param == "staffing_z" ~ beta_negligible,
                           param == "poverty_z_grmc" ~ beta_strong,
                           param == "inequal_z_grmc" ~ beta_moderate,
                           param == "spending_z_grmc" ~ beta_small,
                           param == "staffing_z_grmc" ~ beta_negligible
  ))

ml_reinforce_comparison_sims <- ml_reinforce_comparison_sims %>%
  mutate(
    modtype = case_when(modtype == "lm" ~ "Pooled LM",
                        modtype == "mlm" ~ "Pooled MLM CWC(M)",
                        modtype == "non-pooled" ~ "Non-pooled LM")
  ) 


# reinforce effects
pooled_plot_c <- ml_reinforce_comparison_sims %>%
  mutate(modtype = factor(modtype, levels = c("Pooled LM", "Pooled MLM CWC(M)", "Non-pooled LM")),
         param = case_when(param == "poverty_z" ~ "Poverty\n(Between)",
                           param == "inequal_z" ~ "Inequality\n(Between)",
                           param == "spending_z" ~ "Spending\n(Between)",
                           param == "staffing_z" ~ "Staffing\n(Between)",
                           param == "poverty_z_grmc" ~ "Poverty\n(Within)",
                           param == "inequal_z_grmc" ~ "Inequality\n(Within)",
                           param == "spending_z_grmc" ~ "Spending\n(Within)",
                           param == "staffing_z_grmc" ~ "Staffing\n(Within)",
         )
  ) %>%
  mutate(param = factor(param, levels = c("Poverty\n(Between)", "Inequality\n(Between)",
                                          "Spending\n(Between)", "Staffing\n(Between)",
                                          "Poverty\n(Within)", "Inequality\n(Within)",
                                          "Spending\n(Within)", "Staffing\n(Within)"))) %>%
  ggplot() +
  geom_point(aes(x = row, y = coefficient, 
                 col = ifelse(p < 0.05, "black", "grey")), 
             size = 0.3) +
  geom_segment(aes(x = row, xend = row, y = coefficient + 1.96*std_err, 
                   yend = coefficient - 1.96*std_err, 
                   col = ifelse(p < 0.05, "black", "grey")), 
               size = 0.2) +
  # True effects
  geom_hline(aes(yintercept = hline), col = "white", size = 1) +
  geom_hline(aes(yintercept = hline), col = "black", size = 0.6) +
  ylab("B Coefficient") +
  xlab("Simulation") +
  ggtitle("c) Within and between effects are exactly the same") +
  facet_grid(modtype~param) +
  scale_colour_identity() +
  theme_classic()

pooled_bias_plots <- pooled_plot_a / pooled_plot_b / pooled_plot_c

ggsave(pooled_bias_plots, filename = "plots/pooled_bias_plots.png", units = "in", dpi = 300,
       width = 8.25, height = (1)*11.75, scale = 1.4)


# Compare Bayesian and Frequentist descriptions of uncertainty
# How can describing uncertainty rather than relying on hypothesis testing help.






# Then, how they evolve over ten studies (with priors versus without priors)
# How does changing priors to new data help refine uncertainty: 
#   1) assuming effects stay the same
#   2) assuming effects change in a certain direction

# Simulate data from 20 studies at n = 50


# Effect stays the same each time
n = 50
set.seed(89)
sims_dat_50_same <- list()

for (i in 1:20) {
  
  beta_1 <- beta_strong
  beta_2 <- beta_moderate
  beta_3 <- beta_small
  beta_4 <- beta_negligible
  
  dat <- rnorm_multi(n = n,
              varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
              mu = 0,
              sd = 1,
              r = c(1, beta_strong, beta_moderate, beta_small, beta_negligible,
                    beta_strong, 1, 0, 0, 0,
                    beta_moderate, 0, 1, 0, 0,
                    beta_small, 0, 0, 1, 0,
                    beta_negligible, 0, 0, 0, 1)
  )
  
  
  sims_dat_50_same[[i]] <- tibble(care_z = dat$care_z, 
                                  poverty_z = dat$poverty_z, 
                                  inequal_z = dat$inequal_z, 
                                  spending_z = dat$spending_z, 
                                  staffing_z = dat$staffing_z, 
                                  beta_1, beta_2, beta_3, beta_4) 
  
}


# Noisy effect 
n = 50
set.seed(89)
sims_dat_50_noisy <- list()

noisy_effect <- function () {
  sample(c(beta_strong, beta_moderate, beta_small, beta_negligible,
           -1*beta_strong, -1*beta_moderate, -1*beta_small, -1*beta_negligible), size = 1)
  }

for (i in 1:20) {
  
  beta_1 <- noisy_effect()
  beta_2 <- noisy_effect()
  beta_3 <- noisy_effect()
  beta_4 <- noisy_effect()
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_50_noisy[[i]] <- tibble(care_z = dat$care_z, 
                                   poverty_z = dat$poverty_z, 
                                   inequal_z = dat$inequal_z, 
                                   spending_z = dat$spending_z, 
                                   staffing_z = dat$staffing_z, 
                                   beta_1, beta_2, beta_3, beta_4)
  
}


# Flipped at study = 10
n = 50
set.seed(89)
sims_dat_50_flipped <- list()

for (i in 1:20) {
  
  beta_1 <- ifelse(i <= 10, beta_strong, -1*beta_strong)
  beta_2 <- ifelse(i <= 10, beta_moderate, -1*beta_moderate)
  beta_3 <- ifelse(i <= 10, beta_small, -1*beta_small)
  beta_4 <- ifelse(i <= 10, beta_negligible, -1*beta_negligible)
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_50_flipped[[i]] <- tibble(care_z = dat$care_z, 
                                     poverty_z = dat$poverty_z, 
                                     inequal_z = dat$inequal_z, 
                                     spending_z = dat$spending_z, 
                                     staffing_z = dat$staffing_z, 
                                     beta_1, beta_2, beta_3, beta_4)
  
}


# Simulate frequentist models and CIs
mod <- summary(lm(data = sims_dat_50_same[[1]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
mod$coefficients

lm_coefs_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_50_same[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_same <- bind_rows(lm_coefs_same, mod$coefficients[,1])
  lm_ses_same <- bind_rows(lm_ses_same, mod$coefficients[,2])
  lm_p_same <- bind_rows(lm_p_same, mod$coefficients[,4])
  
}


# Noisy summaries
lm_coefs_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_50_noisy[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_noisy <- bind_rows(lm_coefs_noisy, mod$coefficients[,1])
  lm_ses_noisy <- bind_rows(lm_ses_noisy, mod$coefficients[,2])
  lm_p_noisy <- bind_rows(lm_p_noisy, mod$coefficients[,4])
  
}


# Flipped summaries
lm_coefs_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_50_flipped[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_flipped <- bind_rows(lm_coefs_flipped, mod$coefficients[,1])
  lm_ses_flipped <- bind_rows(lm_ses_flipped, mod$coefficients[,2])
  lm_p_flipped <- bind_rows(lm_p_flipped, mod$coefficients[,4])
  
}

# Bayesian models

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_50_same[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
           prior(normal(0, 1), class = "b", coef = "inequal_z") +
           prior(normal(0, 1), class = "b", coef = "spending_z") +
           prior(normal(0, 1), class = "b", coef = "staffing_z") +
           prior(normal(0, 1), class = "Intercept") +
           prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_50_same[[1]], 
               prior = bpriors)

bayesmod_same <- list()

bayesmod_same[[1]] <- brms::brm(formula = mod_formula, 
                                data = sims_dat_50_same[[1]], 
                                prior = bpriors, seed = 909,
                                chains = 4, iter = 2000, cores = 4)
  

# get posterior as prior for next run
bayesmod_same_draws <- brms::as_draws_df(bayesmod_same[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_same_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_same_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
              stanvar(new_priors_sds[[2]], name='b_pov_sd') +
              stanvar(new_priors_means[[3]], name='b_ineq_mean') +
              stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
              stanvar(new_priors_means[[4]], name='b_spend_mean') +
              stanvar(new_priors_sds[[4]], name='b_spend_sd') +
              stanvar(new_priors_means[[5]], name='b_staff_mean') +
              stanvar(new_priors_sds[[5]], name='b_staff_sd') +
              stanvar(new_priors_means[[1]], name='b_i_mean') +
              stanvar(new_priors_sds[[1]], name='b_i_sd') +
              stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_same[[i]] <- brms::brm(formula = mod_formula, 
                                  data = sims_dat_50_same[[i]], 
                                  prior = bpriors, seed = 89*i, stanvars = stanvars,
                                  chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_same[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}


bayesmod_same[[1]]
bayesmod_same[[10]]
bayesmod_same[[20]]

# Get draws for all models for later plotting
bayesmod_same_draws <- list()

for (i in 1:20) {
  bayesmod_same_draws[[i]]  <- brms::as_draws_df(bayesmod_same[[i]])
}

bayesmod_same_draws <- bind_rows(bayesmod_same_draws, .id = "mod")

rethinking::HPDI(bayesmod_same_draws[,"b_Intercept"], prob = 0.89)[1]
rethinking::HPDI(bayesmod_same_draws$b_Intercept, prob = 0.89)[2]

bayesmod_same_draws_l <- as_tibble(bayesmod_same_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = 1:20)) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )


tidy_join_lmsims <- function(coefs, ses, ps) {

  coefs <- coefs %>%
  mutate(mod = row_number()) %>%
  pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "est") %>%
  mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                            effect == "poverty_z" ~ "b_poverty_z",
                            effect == "inequal_z" ~ "b_inequal_z",
                            effect == "spending_z" ~ "b_spending_z",
                            effect == "staffing_z" ~ "b_staffing_z")) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))) %>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))

  ses <- ses %>%
  mutate(mod = row_number()) %>%
  pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "se") %>%
  mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                            effect == "poverty_z" ~ "b_poverty_z",
                            effect == "inequal_z" ~ "b_inequal_z",
                            effect == "spending_z" ~ "b_spending_z",
                            effect == "staffing_z" ~ "b_staffing_z")) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))

  ps <- ps %>%
  mutate(mod = row_number()) %>%
  pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "p") %>%
  mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                            effect == "poverty_z" ~ "b_poverty_z",
                            effect == "inequal_z" ~ "b_inequal_z",
                            effect == "spending_z" ~ "b_spending_z",
                            effect == "staffing_z" ~ "b_staffing_z")) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))

  combined <- left_join(coefs, ses, by = c("mod", "effect")) %>% left_join(., ps, by = c("mod", "effect"))

  return(combined)
  
}


lmmods_same <- tidy_join_lmsims(lm_coefs_same, lm_ses_same, lm_p_same)

# meta_analysis
library(Metaan)
metaan_n50_same <- list()
for (i in 1:5) {
metaan_n50_same[[i]] <- Metaan::estmeta(Beta = lm_coefs_same[,i], 
                u = lm_coefs_same[,i] + 1.96*lm_ses_same[,i],
                l = lm_coefs_same[,i] - 1.96*lm_ses_same[,i],
                test = 'FIXED',
                conf.level = 0.95)
}

meta_effs_50_same <- tibble(mod = "M",
       effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
       est = rep(NA, 5),
       se = rep(NA, 5),
       p = rep(NA, 5)
       )

for (i in 1:5) {
  
  meta_effs_50_same[i,3] <- metaan_n50_same[[i]]$Beta_tot
  meta_effs_50_same[i,4] <- -1*(25/49)*(metaan_n50_same[[i]]$Beta_tot - metaan_n50_same[[i]]$u_tot)
  meta_effs_50_same[i,5] <- ifelse(metaan_n50_same[[i]]$l_tot < 0 & metaan_n50_same[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_50_same

lmmods_same <- bind_rows(lmmods_same, meta_effs_50_same) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_same_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                               se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))

meta.brm[[i]] <- brm(meta_an_f,
                 data = meta_bayes_dat,
                 prior = priors, seed = 89*i,
                 iter = 4000)

}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
  )

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )

bayesmod_same_draws_l <- bind_rows(bayesmod_same_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n50_same <- meta_effs_50_same
meta.brm_same_draws_n50 <- meta.brm
lmmods_same_n50 <- lmmods_same
bayesmod_same_draws_l_n50 <- bayesmod_same_draws_l

# overlay true effects and add frequentist ones
effect_facet_labels <- c("Intercept", "Poverty", "Inequality", "Spending", "Staffing")
names(effect_facet_labels) <- levels(bayesmod_same_draws_l$effect)

library(ggridges)
updating_priors_plot1_n50 <- bayesmod_same_draws_l %>%
  ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
    size = 0.1, scale = 0.9
  ) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.9
  ) + 
  geom_vline(aes(xintercept = hline), colour = "white", size = 0.6) +
  geom_vline(aes(xintercept = hline), colour = "black", size = 0.3) +
  geom_segment(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.5) +
  geom_point(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.8) +
  geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
  facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  scale_colour_identity() +
  xlab("Estimate") +
  ylab("Model for Simulated Dataset M") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("a) Effect the same at each study (N = 50)")


# ------ simulations and plots for "noisy" data - no consistent effect each year


mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_50_noisy[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_50_noisy[[1]], 
               prior = bpriors)

bayesmod_noisy <- list()

bayesmod_noisy[[1]] <- brms::brm(formula = mod_formula, 
                                data = sims_dat_50_noisy[[1]], 
                                prior = bpriors, seed = 909,
                                chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_noisy_draws <- brms::as_draws_df(bayesmod_noisy[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_noisy_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_noisy_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_noisy[[i]] <- brms::brm(formula = mod_formula, 
                                  data = sims_dat_50_noisy[[i]], 
                                  prior = bpriors, seed = 89*i, stanvars = stanvars,
                                  chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_noisy[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_noisy_draws <- list()

for (i in 1:20) {
  bayesmod_noisy_draws[[i]]  <- brms::as_draws_df(bayesmod_noisy[[i]])
}

bayesmod_noisy_draws <- bind_rows(bayesmod_noisy_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_50_noisy, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_noisy_draws_l <- as_tibble(bayesmod_noisy_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_noisy <- tidy_join_lmsims(lm_coefs_noisy, lm_ses_noisy, lm_p_noisy)

#### Noisy meta analysis

# meta_analysis
metaan_n50_noisy <- list()
for (i in 1:5) {
  metaan_n50_noisy[[i]] <- Metaan::estmeta(Beta = lm_coefs_noisy[,i], 
                                          u = lm_coefs_noisy[,i] + 1.96*lm_ses_noisy[,i],
                                          l = lm_coefs_noisy[,i] - 1.96*lm_ses_noisy[,i],
                                          test = 'FIXED',
                                          conf.level = 0.95)
}

meta_effs_50_noisy <- tibble(mod = "M",
                            effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                            est = rep(NA, 5),
                            se = rep(NA, 5),
                            p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_50_noisy[i,3] <- metaan_n50_noisy[[i]]$Beta_tot
  meta_effs_50_noisy[i,4] <- -1*(25/49)*(metaan_n50_noisy[[i]]$Beta_tot - metaan_n50_noisy[[i]]$u_tot)
  meta_effs_50_noisy[i,5] <- ifelse(metaan_n50_noisy[[i]]$l_tot < 0 & metaan_n50_noisy[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_50_noisy

lmmods_noisy <- bind_rows(lmmods_noisy, meta_effs_50_noisy) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_noisy_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_noisy_draws_l <- bind_rows(bayesmod_noisy_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n50_noisy <- meta_effs_50_noisy
meta.brm_noisy_draws_n50 <- meta.brm
lmmods_noisy_n50 <- lmmods_noisy
bayesmod_noisy_draws_l_n50 <- bayesmod_noisy_draws_l


# overlay true effects and add frequentist ones

(updating_priors_plot2_n50 <- bayesmod_noisy_draws_l %>%
  ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
    size = 0.1, scale = 0.9
  ) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.9
  ) + 
  geom_segment(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.5) +
  geom_point(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
             aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
             size = 0.8) +
  geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
  geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
  geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
  facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  scale_colour_identity() +
  xlab("Estimate") +
  ylab("Model for Simulated Dataset M") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("b) Effect random at each study (N = 50)"))


# simulations and plots for "flipped" data - sudden change in effect

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_50_flipped[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_50_flipped[[1]], 
               prior = bpriors)

bayesmod_flipped <- list()

bayesmod_flipped[[1]] <- brms::brm(formula = mod_formula, 
                                 data = sims_dat_50_flipped[[1]], 
                                 prior = bpriors, seed = 909,
                                 chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_flipped_draws <- brms::as_draws_df(bayesmod_flipped[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_flipped_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_flipped_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_flipped[[i]] <- brms::brm(formula = mod_formula, 
                                   data = sims_dat_50_flipped[[i]], 
                                   prior = bpriors, seed = 89*i, stanvars = stanvars,
                                   chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_flipped[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_flipped_draws <- list()

for (i in 1:20) {
  bayesmod_flipped_draws[[i]]  <- brms::as_draws_df(bayesmod_flipped[[i]])
}

bayesmod_flipped_draws <- bind_rows(bayesmod_flipped_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_50_flipped, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_flipped_draws_l <- as_tibble(bayesmod_flipped_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_flipped <- tidy_join_lmsims(lm_coefs_flipped, lm_ses_flipped, lm_p_flipped)


#### Noisy meta analysis

# meta_analysis
metaan_n50_flipped <- list()
for (i in 1:5) {
  metaan_n50_flipped[[i]] <- Metaan::estmeta(Beta = lm_coefs_flipped[,i], 
                                           u = lm_coefs_flipped[,i] + 1.96*lm_ses_flipped[,i],
                                           l = lm_coefs_flipped[,i] - 1.96*lm_ses_flipped[,i],
                                           test = 'FIXED',
                                           conf.level = 0.95)
}

meta_effs_50_flipped <- tibble(mod = "M",
                             effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                             est = rep(NA, 5),
                             se = rep(NA, 5),
                             p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_50_flipped[i,3] <- metaan_n50_flipped[[i]]$Beta_tot
  meta_effs_50_flipped[i,4] <- -1*(25/49)*(metaan_n50_flipped[[i]]$Beta_tot - metaan_n50_flipped[[i]]$u_tot)
  meta_effs_50_flipped[i,5] <- ifelse(metaan_n50_flipped[[i]]$l_tot < 0 & metaan_n50_flipped[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_50_flipped

lmmods_flipped <- bind_rows(lmmods_flipped, meta_effs_50_flipped) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_flipped_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_flipped_draws_l <- bind_rows(bayesmod_flipped_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# overlay true effects and add frequentist ones

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n50_flipped <- meta_effs_50_flipped
meta.brm_flipped_draws_n50 <- meta.brm
lmmods_flipped_n50 <- lmmods_flipped
bayesmod_flipped_draws_l_n50 <- bayesmod_flipped_draws_l

(updating_priors_plot3_n50 <- bayesmod_flipped_draws_l %>%
  ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
    size = 0.1, scale = 0.9
  ) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.9
  ) + 
  geom_segment(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.5) +
  geom_point(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
             aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
             size = 0.8) +
  geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
  geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
  geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
  facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  scale_colour_identity() +
  xlab("Estimate") +
  ylab("Model for Simulated Dataset M") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("c) Effect reverses at study 10 (N = 50)"))


updating_prior_n50 <- updating_priors_plot1_n50 / updating_priors_plot2_n50 / updating_priors_plot3_n50
ggsave(updating_prior_n50, filename = "plots/updating_prior_n50.png", units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.75)


# 150 cases updating priors example ---------------------------------------
# N = 150

# Effect stays the same each time
n = 150
set.seed(89)
sims_dat_150_same <- list()

for (i in 1:20) {
  
  beta_1 <- beta_strong
  beta_2 <- beta_moderate
  beta_3 <- beta_small
  beta_4 <- beta_negligible
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )

  sims_dat_150_same[[i]] <- tibble(care_z = dat$care_z, 
                                   poverty_z = dat$poverty_z, 
                                   inequal_z = dat$inequal_z, 
                                   spending_z = dat$spending_z, 
                                   staffing_z = dat$staffing_z, 
                                   beta_1, beta_2, beta_3, beta_4)
  
}


# Noisy effect 
n = 150
set.seed(89)
sims_dat_150_noisy <- list()

noisy_effect <- function () {
  sample(c(beta_strong, beta_moderate, beta_small, beta_negligible,
           -1*beta_strong, -1*beta_moderate, -1*beta_small, -1*beta_negligible), size = 1)
}

for (i in 1:20) {
  
  beta_1 <- noisy_effect()
  beta_2 <- noisy_effect()
  beta_3 <- noisy_effect()
  beta_4 <- noisy_effect()
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_150_noisy[[i]] <- tibble(care_z = dat$care_z, 
                                    poverty_z = dat$poverty_z, 
                                    inequal_z = dat$inequal_z, 
                                    spending_z = dat$spending_z, 
                                    staffing_z = dat$staffing_z, 
                                    beta_1, beta_2, beta_3, beta_4)
  
}


# Flipped at study = 10
n = 150
set.seed(89)
sims_dat_150_flipped <- list()

for (i in 1:20) {
  
  beta_1 <- ifelse(i <= 10, beta_strong, -1*beta_strong)
  beta_2 <- ifelse(i <= 10, beta_moderate, -1*beta_moderate)
  beta_3 <- ifelse(i <= 10, beta_small, -1*beta_small)
  beta_4 <- ifelse(i <= 10, beta_negligible, -1*beta_negligible)
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  
  sims_dat_150_flipped[[i]] <- tibble(care_z = dat$care_z, 
                                      poverty_z = dat$poverty_z, 
                                      inequal_z = dat$inequal_z, 
                                      spending_z = dat$spending_z, 
                                      staffing_z = dat$staffing_z, 
                                      beta_1, beta_2, beta_3, beta_4)
  
}


# Simulate frequentist models and CIs
mod <- summary(lm(data = sims_dat_150_same[[1]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
mod$coefficients

lm_coefs_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_150_same[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_same <- bind_rows(lm_coefs_same, mod$coefficients[,1])
  lm_ses_same <- bind_rows(lm_ses_same, mod$coefficients[,2])
  lm_p_same <- bind_rows(lm_p_same, mod$coefficients[,4])
  
}


# Noisy summaries
lm_coefs_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_150_noisy[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_noisy <- bind_rows(lm_coefs_noisy, mod$coefficients[,1])
  lm_ses_noisy <- bind_rows(lm_ses_noisy, mod$coefficients[,2])
  lm_p_noisy <- bind_rows(lm_p_noisy, mod$coefficients[,4])
  
}


# Flipped summaries
lm_coefs_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_150_flipped[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_flipped <- bind_rows(lm_coefs_flipped, mod$coefficients[,1])
  lm_ses_flipped <- bind_rows(lm_ses_flipped, mod$coefficients[,2])
  lm_p_flipped <- bind_rows(lm_p_flipped, mod$coefficients[,4])
  
}

# Bayesian models

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_150_same[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_150_same[[1]], 
               prior = bpriors)

bayesmod_same <- list()

bayesmod_same[[1]] <- brms::brm(formula = mod_formula, 
                                data = sims_dat_150_same[[1]], 
                                prior = bpriors, seed = 909,
                                chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_same_draws <- brms::as_draws_df(bayesmod_same[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_same_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_same_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_same[[i]] <- brms::brm(formula = mod_formula, 
                                  data = sims_dat_150_same[[i]], 
                                  prior = bpriors, seed = 89*i, stanvars = stanvars,
                                  chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_same[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}


bayesmod_same[[1]]
bayesmod_same[[10]]
bayesmod_same[[20]]

# Get draws for all models for later plotting
bayesmod_same_draws <- list()

for (i in 1:20) {
  bayesmod_same_draws[[i]]  <- brms::as_draws_df(bayesmod_same[[i]])
}

bayesmod_same_draws <- bind_rows(bayesmod_same_draws, .id = "mod")

rethinking::HPDI(bayesmod_same_draws[,"b_Intercept"], prob = 0.89)[1]
rethinking::HPDI(bayesmod_same_draws$b_Intercept, prob = 0.89)[2]

bayesmod_same_draws_l <- as_tibble(bayesmod_same_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = 1:20)) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )


tidy_join_lmsims <- function(coefs, ses, ps) {
  
  coefs <- coefs %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "est") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))) %>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  ses <- ses %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "se") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  ps <- ps %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "p") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  combined <- left_join(coefs, ses, by = c("mod", "effect")) %>% left_join(., ps, by = c("mod", "effect"))
  
  return(combined)
  
}


lmmods_same <- tidy_join_lmsims(lm_coefs_same, lm_ses_same, lm_p_same)

# meta_analysis
library(Metaan)
metaan_n150_same <- list()
for (i in 1:5) {
  metaan_n150_same[[i]] <- Metaan::estmeta(Beta = lm_coefs_same[,i], 
                                          u = lm_coefs_same[,i] + 1.96*lm_ses_same[,i],
                                          l = lm_coefs_same[,i] - 1.96*lm_ses_same[,i],
                                          test = 'FIXED',
                                          conf.level = 0.95)
}

meta_effs_150_same <- tibble(mod = "M",
                            effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                            est = rep(NA, 5),
                            se = rep(NA, 5),
                            p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_150_same[i,3] <- metaan_n150_same[[i]]$Beta_tot
  meta_effs_150_same[i,4] <- -1*(25/49)*(metaan_n150_same[[i]]$Beta_tot - metaan_n150_same[[i]]$u_tot)
  meta_effs_150_same[i,5] <- ifelse(metaan_n150_same[[i]]$l_tot < 0 & metaan_n150_same[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_150_same

lmmods_same <- bind_rows(lmmods_same, meta_effs_150_same) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_same_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )

bayesmod_same_draws_l <- bind_rows(bayesmod_same_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n150_same <- meta_effs_150_same
meta.brm_same_draws_n150 <- meta.brm
lmmods_same_n150 <- lmmods_same
bayesmod_same_draws_l_n150 <- bayesmod_same_draws_l

# overlay true effects and add frequentist ones
effect_facet_labels <- c("Intercept", "Poverty", "Inequality", "Spending", "Staffing")
names(effect_facet_labels) <- levels(bayesmod_same_draws_l$effect)

library(ggridges)
updating_priors_plot1_n150 <- bayesmod_same_draws_l %>%
  ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
    size = 0.1, scale = 0.9
  ) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.9
  ) + 
  geom_vline(aes(xintercept = hline), colour = "white", size = 0.6) +
  geom_vline(aes(xintercept = hline), colour = "black", size = 0.3) +
  geom_segment(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.5) +
  geom_point(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
             aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
             size = 0.8) +
  geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
  facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  scale_colour_identity() +
  xlab("Estimate") +
  ylab("Model for Simulated Dataset M") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("a) Effect the same at each study (N = 150)")


# ------ simulations and plots for "noisy" data - no consistent effect each year


mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_150_noisy[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_150_noisy[[1]], 
               prior = bpriors)

bayesmod_noisy <- list()

bayesmod_noisy[[1]] <- brms::brm(formula = mod_formula, 
                                 data = sims_dat_150_noisy[[1]], 
                                 prior = bpriors, seed = 909,
                                 chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_noisy_draws <- brms::as_draws_df(bayesmod_noisy[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_noisy_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_noisy_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_noisy[[i]] <- brms::brm(formula = mod_formula, 
                                   data = sims_dat_150_noisy[[i]], 
                                   prior = bpriors, seed = 89*i, stanvars = stanvars,
                                   chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_noisy[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_noisy_draws <- list()

for (i in 1:20) {
  bayesmod_noisy_draws[[i]]  <- brms::as_draws_df(bayesmod_noisy[[i]])
}

bayesmod_noisy_draws <- bind_rows(bayesmod_noisy_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_150_noisy, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_noisy_draws_l <- as_tibble(bayesmod_noisy_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_noisy <- tidy_join_lmsims(lm_coefs_noisy, lm_ses_noisy, lm_p_noisy)

#### Noisy meta analysis

# meta_analysis
metaan_n150_noisy <- list()
for (i in 1:5) {
  metaan_n150_noisy[[i]] <- Metaan::estmeta(Beta = lm_coefs_noisy[,i], 
                                           u = lm_coefs_noisy[,i] + 1.96*lm_ses_noisy[,i],
                                           l = lm_coefs_noisy[,i] - 1.96*lm_ses_noisy[,i],
                                           test = 'FIXED',
                                           conf.level = 0.95)
}

meta_effs_150_noisy <- tibble(mod = "M",
                             effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                             est = rep(NA, 5),
                             se = rep(NA, 5),
                             p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_150_noisy[i,3] <- metaan_n150_noisy[[i]]$Beta_tot
  meta_effs_150_noisy[i,4] <- -1*(25/49)*(metaan_n150_noisy[[i]]$Beta_tot - metaan_n150_noisy[[i]]$u_tot)
  meta_effs_150_noisy[i,5] <- ifelse(metaan_n150_noisy[[i]]$l_tot < 0 & metaan_n150_noisy[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_150_noisy

lmmods_noisy <- bind_rows(lmmods_noisy, meta_effs_150_noisy) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_noisy_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_noisy_draws_l <- bind_rows(bayesmod_noisy_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n150_noisy <- meta_effs_150_noisy
meta.brm_noisy_draws_n150 <- meta.brm
lmmods_noisy_n150 <- lmmods_noisy
bayesmod_noisy_draws_l_n150 <- bayesmod_noisy_draws_l

# overlay true effects and add frequentist ones

(updating_priors_plot2_n150 <- bayesmod_noisy_draws_l %>%
    ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
      size = 0.1, scale = 0.9
    ) + 
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
      size = 0.3, fill = "transparent", scale = 0.9
    ) + 
    geom_segment(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
                 aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
                 size = 0.5) +
    geom_point(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.8) +
    geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
    geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
    geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
    facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
    scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
    scale_colour_identity() +
    xlab("Estimate") +
    ylab("Model for Simulated Dataset M") +
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip() +
    ggtitle("b) Effect random at each study (N = 150)"))


# simulations and plots for "flipped" data - sudden change in effect

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_150_flipped[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_150_flipped[[1]], 
               prior = bpriors)

bayesmod_flipped <- list()

bayesmod_flipped[[1]] <- brms::brm(formula = mod_formula, 
                                   data = sims_dat_150_flipped[[1]], 
                                   prior = bpriors, seed = 909,
                                   chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_flipped_draws <- brms::as_draws_df(bayesmod_flipped[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_flipped_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_flipped_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_flipped[[i]] <- brms::brm(formula = mod_formula, 
                                     data = sims_dat_150_flipped[[i]], 
                                     prior = bpriors, seed = 89*i, stanvars = stanvars,
                                     chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_flipped[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_flipped_draws <- list()

for (i in 1:20) {
  bayesmod_flipped_draws[[i]]  <- brms::as_draws_df(bayesmod_flipped[[i]])
}

bayesmod_flipped_draws <- bind_rows(bayesmod_flipped_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_150_flipped, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_flipped_draws_l <- as_tibble(bayesmod_flipped_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_flipped <- tidy_join_lmsims(lm_coefs_flipped, lm_ses_flipped, lm_p_flipped)


#### Noisy meta analysis

# meta_analysis
metaan_n150_flipped <- list()
for (i in 1:5) {
  metaan_n150_flipped[[i]] <- Metaan::estmeta(Beta = lm_coefs_flipped[,i], 
                                             u = lm_coefs_flipped[,i] + 1.96*lm_ses_flipped[,i],
                                             l = lm_coefs_flipped[,i] - 1.96*lm_ses_flipped[,i],
                                             test = 'FIXED',
                                             conf.level = 0.95)
}

meta_effs_150_flipped <- tibble(mod = "M",
                               effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                               est = rep(NA, 5),
                               se = rep(NA, 5),
                               p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_150_flipped[i,3] <- metaan_n150_flipped[[i]]$Beta_tot
  meta_effs_150_flipped[i,4] <- -1*(25/49)*(metaan_n150_flipped[[i]]$Beta_tot - metaan_n150_flipped[[i]]$u_tot)
  meta_effs_150_flipped[i,5] <- ifelse(metaan_n150_flipped[[i]]$l_tot < 0 & metaan_n150_flipped[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_150_flipped

lmmods_flipped <- bind_rows(lmmods_flipped, meta_effs_150_flipped) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_flipped_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_flipped_draws_l <- bind_rows(bayesmod_flipped_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n150_flipped <- meta_effs_150_flipped
meta.brm_flipped_draws_n150 <- meta.brm
lmmods_flipped_n150 <- lmmods_flipped
bayesmod_flipped_draws_l_n150 <- bayesmod_flipped_draws_l


# overlay true effects and add frequentist ones

(updating_priors_plot3_n150 <- bayesmod_flipped_draws_l %>%
    ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
      size = 0.1, scale = 0.9
    ) + 
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
      size = 0.3, fill = "transparent", scale = 0.9
    ) + 
    geom_segment(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
                 aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
                 size = 0.5) +
    geom_point(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.8) +
    geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
    geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
    geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
    facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
    scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
    scale_colour_identity() +
    xlab("Estimate") +
    ylab("Model for Simulated Dataset M") +
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip() +
    ggtitle("c) Effect reverses at study 10 (N = 150)"))


updating_prior_n150 <- updating_priors_plot1_n150 / updating_priors_plot2_n150 / updating_priors_plot3_n150
ggsave(updating_prior_n150, filename = "plots/updating_prior_n150.png", units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.75)




# 1000 cases updating priors example ---------------------------------------
# N = 1000


# Effect stays the same each time
n = 1000
set.seed(89)
sims_dat_1000_same <- list()

for (i in 1:20) {
  
  beta_1 <- beta_strong
  beta_2 <- beta_moderate
  beta_3 <- beta_small
  beta_4 <- beta_negligible
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_1000_same[[i]] <- tibble(care_z = dat$care_z, 
                                    poverty_z = dat$poverty_z, 
                                    inequal_z = dat$inequal_z, 
                                    spending_z = dat$spending_z, 
                                    staffing_z = dat$staffing_z, 
                                    beta_1, beta_2, beta_3, beta_4)
  
}


# Noisy effect 
n = 1000
set.seed(89)
sims_dat_1000_noisy <- list()

noisy_effect <- function () {
  sample(c(beta_strong, beta_moderate, beta_small, beta_negligible,
           -1*beta_strong, -1*beta_moderate, -1*beta_small, -1*beta_negligible), size = 1)
}

for (i in 1:20) {
  
  beta_1 <- noisy_effect()
  beta_2 <- noisy_effect()
  beta_3 <- noisy_effect()
  beta_4 <- noisy_effect()
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_1000_noisy[[i]] <- tibble(care_z = dat$care_z, 
                                     poverty_z = dat$poverty_z, 
                                     inequal_z = dat$inequal_z, 
                                     spending_z = dat$spending_z, 
                                     staffing_z = dat$staffing_z, 
                                     beta_1, beta_2, beta_3, beta_4)
  
}


# Flipped at study = 10
n = 1000
set.seed(89)
sims_dat_1000_flipped <- list()

for (i in 1:20) {
  
  beta_1 <- ifelse(i <= 10, beta_strong, -1*beta_strong)
  beta_2 <- ifelse(i <= 10, beta_moderate, -1*beta_moderate)
  beta_3 <- ifelse(i <= 10, beta_small, -1*beta_small)
  beta_4 <- ifelse(i <= 10, beta_negligible, -1*beta_negligible)
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_1, beta_2, beta_3, beta_4,
                           beta_1, 1, 0, 0, 0,
                           beta_2, 0, 1, 0, 0,
                           beta_3, 0, 0, 1, 0,
                           beta_4, 0, 0, 0, 1)
  )
  
  sims_dat_1000_flipped[[i]] <- tibble(care_z = dat$care_z, 
                                       poverty_z = dat$poverty_z, 
                                       inequal_z = dat$inequal_z, 
                                       spending_z = dat$spending_z, 
                                       staffing_z = dat$staffing_z, 
                                       beta_1, beta_2, beta_3, beta_4)
  
}


# Simulate frequentist models and CIs
mod <- summary(lm(data = sims_dat_1000_same[[1]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
mod$coefficients

lm_coefs_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_same <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_1000_same[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_same <- bind_rows(lm_coefs_same, mod$coefficients[,1])
  lm_ses_same <- bind_rows(lm_ses_same, mod$coefficients[,2])
  lm_p_same <- bind_rows(lm_p_same, mod$coefficients[,4])
  
}


# Noisy summaries
lm_coefs_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_noisy <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_1000_noisy[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_noisy <- bind_rows(lm_coefs_noisy, mod$coefficients[,1])
  lm_ses_noisy <- bind_rows(lm_ses_noisy, mod$coefficients[,2])
  lm_p_noisy <- bind_rows(lm_p_noisy, mod$coefficients[,4])
  
}


# Flipped summaries
lm_coefs_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_ses_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())
lm_p_flipped <- tibble(`(Intercept)` = numeric(), poverty_z = numeric(), inequal_z = numeric(), staffing_z = numeric())

for (i in 1:20) {
  
  mod <- summary(lm(data = sims_dat_1000_flipped[[i]], formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z))
  
  lm_coefs_flipped <- bind_rows(lm_coefs_flipped, mod$coefficients[,1])
  lm_ses_flipped <- bind_rows(lm_ses_flipped, mod$coefficients[,2])
  lm_p_flipped <- bind_rows(lm_p_flipped, mod$coefficients[,4])
  
}

# Bayesian models

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_1000_same[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_1000_same[[1]], 
               prior = bpriors)

bayesmod_same <- list()

bayesmod_same[[1]] <- brms::brm(formula = mod_formula, 
                                data = sims_dat_1000_same[[1]], 
                                prior = bpriors, seed = 909,
                                chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_same_draws <- brms::as_draws_df(bayesmod_same[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_same_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_same_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_same[[i]] <- brms::brm(formula = mod_formula, 
                                  data = sims_dat_1000_same[[i]], 
                                  prior = bpriors, seed = 89*i, stanvars = stanvars,
                                  chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_same[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}


bayesmod_same[[1]]
bayesmod_same[[10]]
bayesmod_same[[20]]

# Get draws for all models for later plotting
bayesmod_same_draws <- list()

for (i in 1:20) {
  bayesmod_same_draws[[i]]  <- brms::as_draws_df(bayesmod_same[[i]])
}

bayesmod_same_draws <- bind_rows(bayesmod_same_draws, .id = "mod")

rethinking::HPDI(bayesmod_same_draws[,"b_Intercept"], prob = 0.89)[1]
rethinking::HPDI(bayesmod_same_draws$b_Intercept, prob = 0.89)[2]

bayesmod_same_draws_l <- as_tibble(bayesmod_same_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = 1:20)) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )


tidy_join_lmsims <- function(coefs, ses, ps) {
  
  coefs <- coefs %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "est") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))) %>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  ses <- ses %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "se") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  ps <- ps %>%
    mutate(mod = row_number()) %>%
    pivot_longer(`(Intercept)`:spending_z, names_to = "effect", values_to = "p") %>%
    mutate(effect = case_when(effect == "(Intercept)" ~ "b_Intercept",
                              effect == "poverty_z" ~ "b_poverty_z",
                              effect == "inequal_z" ~ "b_inequal_z",
                              effect == "spending_z" ~ "b_spending_z",
                              effect == "staffing_z" ~ "b_staffing_z")) %>%
    mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))%>%
    ungroup() %>%
    mutate(mod = factor(mod, levels = 1:20))
  
  combined <- left_join(coefs, ses, by = c("mod", "effect")) %>% left_join(., ps, by = c("mod", "effect"))
  
  return(combined)
  
}


lmmods_same <- tidy_join_lmsims(lm_coefs_same, lm_ses_same, lm_p_same)

# meta_analysis
library(Metaan)
metaan_n1000_same <- list()
for (i in 1:5) {
  metaan_n1000_same[[i]] <- Metaan::estmeta(Beta = lm_coefs_same[,i], 
                                           u = lm_coefs_same[,i] + 1.96*lm_ses_same[,i],
                                           l = lm_coefs_same[,i] - 1.96*lm_ses_same[,i],
                                           test = 'FIXED',
                                           conf.level = 0.95)
}

meta_effs_1000_same <- tibble(mod = "M",
                             effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                             est = rep(NA, 5),
                             se = rep(NA, 5),
                             p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_1000_same[i,3] <- metaan_n1000_same[[i]]$Beta_tot
  meta_effs_1000_same[i,4] <- -1*(25/49)*(metaan_n1000_same[[i]]$Beta_tot - metaan_n1000_same[[i]]$u_tot)
  meta_effs_1000_same[i,5] <- ifelse(metaan_n1000_same[[i]]$l_tot < 0 & metaan_n1000_same[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_1000_same

lmmods_same <- bind_rows(lmmods_same, meta_effs_1000_same) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_same_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(
    hline = case_when(effect == "b_Intercept" ~ 0,
                      effect == "b_poverty_z" ~ beta_strong,
                      effect == "b_inequal_z" ~ beta_moderate,
                      effect == "b_spending_z" ~ beta_small,
                      effect == "b_staffing_z" ~ beta_negligible)
  )

bayesmod_same_draws_l <- bind_rows(bayesmod_same_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))


# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n1000_same <- meta_effs_1000_same
meta.brm_same_draws_n1000 <- meta.brm
lmmods_same_n1000 <- lmmods_same
bayesmod_same_draws_l_n1000 <- bayesmod_same_draws_l

# overlay true effects and add frequentist ones
effect_facet_labels <- c("Intercept", "Poverty", "Inequality", "Spending", "Staffing")
names(effect_facet_labels) <- levels(bayesmod_same_draws_l$effect)

library(ggridges)
updating_priors_plot1_n1000 <- bayesmod_same_draws_l %>%
  ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
    size = 0.1, scale = 0.9
  ) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
    size = 0.3, fill = "transparent", scale = 0.9
  ) + 
  geom_vline(aes(xintercept = hline), colour = "white", size = 0.6) +
  geom_vline(aes(xintercept = hline), colour = "black", size = 0.3) +
  geom_segment(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.5) +
  geom_point(data = lmmods_same, inherit.aes = FALSE, position = position_nudge(y = -0.2),
             aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
             size = 0.8) +
  geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
  facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
  scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
  scale_colour_identity() +
  xlab("Estimate") +
  ylab("Model for Simulated Dataset M") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("a) Effect the same at each study (N = 1000)")


# ------ simulations and plots for "noisy" data - no consistent effect each year


mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_1000_noisy[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_1000_noisy[[1]], 
               prior = bpriors)

bayesmod_noisy <- list()

bayesmod_noisy[[1]] <- brms::brm(formula = mod_formula, 
                                 data = sims_dat_1000_noisy[[1]], 
                                 prior = bpriors, seed = 909,
                                 chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_noisy_draws <- brms::as_draws_df(bayesmod_noisy[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_noisy_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_noisy_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_noisy[[i]] <- brms::brm(formula = mod_formula, 
                                   data = sims_dat_1000_noisy[[i]], 
                                   prior = bpriors, seed = 89*i, stanvars = stanvars,
                                   chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_noisy[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_noisy_draws <- list()

for (i in 1:20) {
  bayesmod_noisy_draws[[i]]  <- brms::as_draws_df(bayesmod_noisy[[i]])
}

bayesmod_noisy_draws <- bind_rows(bayesmod_noisy_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_1000_noisy, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_noisy_draws_l <- as_tibble(bayesmod_noisy_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_noisy <- tidy_join_lmsims(lm_coefs_noisy, lm_ses_noisy, lm_p_noisy)

#### Noisy meta analysis

# meta_analysis
metaan_n1000_noisy <- list()
for (i in 1:5) {
  metaan_n1000_noisy[[i]] <- Metaan::estmeta(Beta = lm_coefs_noisy[,i], 
                                            u = lm_coefs_noisy[,i] + 1.96*lm_ses_noisy[,i],
                                            l = lm_coefs_noisy[,i] - 1.96*lm_ses_noisy[,i],
                                            test = 'FIXED',
                                            conf.level = 0.95)
}

meta_effs_1000_noisy <- tibble(mod = "M",
                              effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                              est = rep(NA, 5),
                              se = rep(NA, 5),
                              p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_1000_noisy[i,3] <- metaan_n1000_noisy[[i]]$Beta_tot
  meta_effs_1000_noisy[i,4] <- -1*(25/49)*(metaan_n1000_noisy[[i]]$Beta_tot - metaan_n1000_noisy[[i]]$u_tot)
  meta_effs_1000_noisy[i,5] <- ifelse(metaan_n1000_noisy[[i]]$l_tot < 0 & metaan_n1000_noisy[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_1000_noisy

lmmods_noisy <- bind_rows(lmmods_noisy, meta_effs_1000_noisy) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_noisy_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_noisy_draws_l <- bind_rows(bayesmod_noisy_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# overlay true effects and add frequentist ones

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n1000_noisy <- meta_effs_1000_noisy
meta.brm_noisy_draws_n1000 <- meta.brm
lmmods_noisy_n1000 <- lmmods_noisy
bayesmod_noisy_draws_l_n1000 <- bayesmod_noisy_draws_l

(updating_priors_plot2_n1000 <- bayesmod_noisy_draws_l %>%
    ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
      size = 0.1, scale = 0.9
    ) + 
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
      size = 0.3, fill = "transparent", scale = 0.9
    ) + 
    geom_segment(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
                 aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
                 size = 0.5) +
    geom_point(data = lmmods_noisy, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.8) +
    geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
    geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
    geom_segment(data = bayesmod_noisy_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
    facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
    scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
    scale_colour_identity() +
    xlab("Estimate") +
    ylab("Model for Simulated Dataset M") +
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip() +
    ggtitle("b) Effect random at each study (N = 1000)"))


# simulations and plots for "flipped" data - sudden change in effect

mod_formula <- brms::brmsformula(formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z)
get_prior(data = sims_dat_1000_flipped[[1]], mod_formula)

bpriors <- prior(normal(0, 1), class = "b", coef = "poverty_z") +
  prior(normal(0, 1), class = "b", coef = "inequal_z") +
  prior(normal(0, 1), class = "b", coef = "spending_z") +
  prior(normal(0, 1), class = "b", coef = "staffing_z") +
  prior(normal(0, 1), class = "Intercept") +
  prior(exponential(1), class = "sigma")

# Check user prior
validate_prior(formula = mod_formula, 
               data = sims_dat_1000_flipped[[1]], 
               prior = bpriors)

bayesmod_flipped <- list()

bayesmod_flipped[[1]] <- brms::brm(formula = mod_formula, 
                                   data = sims_dat_1000_flipped[[1]], 
                                   prior = bpriors, seed = 909,
                                   chains = 4, iter = 2000, cores = 4)


# get posterior as prior for next run
bayesmod_flipped_draws <- brms::as_draws_df(bayesmod_flipped[[1]])


# extract 1 to 6 median and sd * 1.2 # prior addition.5 for next prior

# 1 = intercept, 2 = b_poverty, 3 = b_inequal, 4 = b_spend, 5 = b_staffing, 6 = sigma
new_priors_means <- list()
new_priors_sds <- list()

for (i in 1:6) {
  new_priors_means[[i]] <- median(bayesmod_flipped_draws[[i]])
  new_priors_sds[[i]] <- sd(bayesmod_flipped_draws[[i]]) * 1.2 # prior addition
}

for (i in 2:20) {
  
  print(paste("Now running model", i))
  
  # make new priors into stan variables
  stanvars <- stanvar(new_priors_means[[2]], name='b_pov_mean') + 
    stanvar(new_priors_sds[[2]], name='b_pov_sd') +
    stanvar(new_priors_means[[3]], name='b_ineq_mean') +
    stanvar(new_priors_sds[[3]], name='b_ineq_sd') +
    stanvar(new_priors_means[[4]], name='b_spend_mean') +
    stanvar(new_priors_sds[[4]], name='b_spend_sd') +
    stanvar(new_priors_means[[5]], name='b_staff_mean') +
    stanvar(new_priors_sds[[5]], name='b_staff_sd') +
    stanvar(new_priors_means[[1]], name='b_i_mean') +
    stanvar(new_priors_sds[[1]], name='b_i_sd') +
    stanvar(new_priors_means[[6]], name='b_sigma')
  
  # update priors to previous model's median and sd * 1.2 # prior addition.5
  bpriors <- prior(normal(b_pov_mean, b_pov_sd), class = "b", coef = "poverty_z") +
    prior(normal(b_ineq_mean, b_ineq_sd), class = "b", coef = "inequal_z") +
    prior(normal(b_spend_mean, b_spend_sd), class = "b", coef = "spending_z") +
    prior(normal(b_staff_mean, b_staff_sd), class = "b", coef = "staffing_z") +
    prior(normal(b_i_mean, b_i_sd), class = "Intercept") +
    prior(exponential(b_sigma), class = "sigma")
  
  # Run model
  bayesmod_flipped[[i]] <- brms::brm(formula = mod_formula, 
                                     data = sims_dat_1000_flipped[[i]], 
                                     prior = bpriors, seed = 89*i, stanvars = stanvars,
                                     chains = 4, iter = 2000, cores = 4)
  
  # update priors for next iteration with new model's posterior
  print(paste("Updating priors for model", i+1))
  bayesmod_draws <- brms::as_draws_df(bayesmod_flipped[[i]])
  
  new_priors_means <- list()
  new_priors_sds <- list()
  
  for (k in 1:6) {
    new_priors_means[[k]] <- median(bayesmod_draws[[k]])
    new_priors_sds[[k]] <- sd(bayesmod_draws[[k]]) * 1.2 # prior addition
  }
  
  
}

# Get draws for all models for later plotting
bayesmod_flipped_draws <- list()

for (i in 1:20) {
  bayesmod_flipped_draws[[i]]  <- brms::as_draws_df(bayesmod_flipped[[i]])
}

bayesmod_flipped_draws <- bind_rows(bayesmod_flipped_draws, .id = "mod")

noisy_effects <- bind_rows(sims_dat_1000_flipped, .id = "mod") %>%
  group_by(mod) %>%
  summarise_at(vars(beta_1:beta_4), ~first(.)) %>%
  pivot_longer(cols = beta_1:beta_4, names_to = "effect", values_to = "hline") %>%
  mutate(effect = case_when(effect == "beta_1" ~ "b_poverty_z",
                            effect == "beta_2" ~ "b_inequal_z",
                            effect == "beta_3" ~ "b_spending_z",
                            effect == "beta_4" ~ "b_staffing_z")) %>%
  ungroup()

bayesmod_flipped_draws_l <- as_tibble(bayesmod_flipped_draws) %>%
  select(mod:b_staffing_z) %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  left_join(., noisy_effects, by = c("mod", "effect")) %>%
  mutate(hline = ifelse(is.na(hline), 0, hline)) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = 1:20),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

lmmods_flipped <- tidy_join_lmsims(lm_coefs_flipped, lm_ses_flipped, lm_p_flipped)


#### Flipped meta analysis

# meta_analysis
metaan_n1000_flipped <- list()
for (i in 1:5) {
  metaan_n1000_flipped[[i]] <- Metaan::estmeta(Beta = lm_coefs_flipped[,i], 
                                              u = lm_coefs_flipped[,i] + 1.96*lm_ses_flipped[,i],
                                              l = lm_coefs_flipped[,i] - 1.96*lm_ses_flipped[,i],
                                              test = 'FIXED',
                                              conf.level = 0.95)
}

meta_effs_1000_flipped <- tibble(mod = "M",
                                effect = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_staffing_z", "b_spending_z"),
                                est = rep(NA, 5),
                                se = rep(NA, 5),
                                p = rep(NA, 5)
)

for (i in 1:5) {
  
  meta_effs_1000_flipped[i,3] <- metaan_n1000_flipped[[i]]$Beta_tot
  meta_effs_1000_flipped[i,4] <- -1*(25/49)*(metaan_n1000_flipped[[i]]$Beta_tot - metaan_n1000_flipped[[i]]$u_tot)
  meta_effs_1000_flipped[i,5] <- ifelse(metaan_n1000_flipped[[i]]$l_tot < 0 & metaan_n1000_flipped[[i]]$u_tot > 0, 1, 0)
  
}

meta_effs_1000_flipped

lmmods_flipped <- bind_rows(lmmods_flipped, meta_effs_1000_flipped) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# Bayesian prior
meta_bayes_dat <- as_tibble(bayesmod_flipped_draws) %>%
  mutate(mod = as.numeric(mod)) %>%
  group_by(mod) %>%
  summarise_at(vars(b_Intercept:b_staffing_z), list(med = median,
                                                    se = sd))

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta.brm <- list()
effects <- c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")

for (i in 1:5) {
  
  meta_an_f <- brmsformula(formula = paste0(effects[i], "_med|se(", effects[i], "_se) ~ 1 + (1|mod)"))
  
  meta.brm[[i]] <- brm(meta_an_f,
                       data = meta_bayes_dat,
                       prior = priors, seed = 89*i,
                       iter = 4000)
  
}


for(i in 1:5) {
  meta.brm[[i]] <- as_draws_df(meta.brm[[i]])[,1] 
}

meta.brm <- tibble(
  mod = "M",
  b_Intercept  = meta.brm[[1]][[1]],
  b_poverty_z  = meta.brm[[2]][[1]],
  b_inequal_z  = meta.brm[[3]][[1]],
  b_spending_z = meta.brm[[4]][[1]],
  b_staffing_z = meta.brm[[5]][[1]]
)

meta.brm <- meta.brm %>%
  pivot_longer(cols = b_Intercept:b_staffing_z, names_to = "effect", values_to = "est") %>%
  mutate(mod = factor(mod, levels = c(1:20, "M"))) %>%
  group_by(mod, effect) %>%
  mutate(
    median_est = median(est),
    lb_hdpi89 = rethinking::HPDI(est, prob = 0.89)[1],
    ub_hdpi89 = rethinking::HPDI(est, prob = 0.89)[2],
    effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
  ) %>%
  mutate(hline = NA) %>%
  ungroup() %>%
  mutate(mod = factor(mod, levels = c(1:20, "M")),
         effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")))

bayesmod_flipped_draws_l <- bind_rows(bayesmod_flipped_draws_l, meta.brm) %>%
  mutate(effect = factor(effect, levels = c("b_Intercept", "b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z")),
         mod = factor(mod, levels = c(1:20, "M")))

# overlay true effects and add frequentist ones

# Keep copies of model coefficients and metanalysis for comparing metaanalysis after 
# publication bias
meta_freq_n1000_flipped <- meta_effs_1000_flipped
meta.brm_flipped_draws_n1000 <- meta.brm
lmmods_flipped_n1000 <- lmmods_flipped
bayesmod_flipped_draws_l_n1000 <- bayesmod_flipped_draws_l

(updating_priors_plot3_n1000 <- bayesmod_flipped_draws_l %>%
    ggplot(aes(y = mod, x = est, fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.055, 0.5, 0.945), quantile_lines = FALSE, rel_min_height = 0.01,
      size = 0.1, scale = 0.9
    ) + 
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = c(0.5), quantile_lines = TRUE, rel_min_height = 0.01,
      size = 0.3, fill = "transparent", scale = 0.9
    ) + 
    geom_segment(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
                 aes(y = mod, yend = mod, x = est - 1.96*se, xend = est + 1.96*se, col = ifelse(p <0.05, "black", "darkgrey")),
                 size = 0.5) +
    geom_point(data = lmmods_flipped, inherit.aes = FALSE, position = position_nudge(y = -0.2),
               aes(y = mod,x = est, col = ifelse(p <0.05, "black", "darkgrey")),
               size = 0.8) +
    geom_vline(xintercept = 0, colour = "black", size = 0.2, lty = 2) +
    geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "white", size = 0.6) +
    geom_segment(data = bayesmod_flipped_draws_l, inherit.aes = FALSE, aes(x = hline, xend = hline, y = parse_number(as.character(mod)) + 0.5, yend = parse_number(as.character(mod)) - 0.5), colour = "black", size = 0.3) +
    facet_grid(rows = ~effect, labeller = labeller(effect = effect_facet_labels)) +
    scale_fill_manual(values = c("transparent", "grey", "grey", "transparent")) +
    scale_colour_identity() +
    xlab("Estimate") +
    ylab("Model for Simulated Dataset M") +
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip() +
    ggtitle("c) Effect reverses at study 10 (N = 1000)"))


updating_prior_n1000 <- updating_priors_plot1_n1000 / updating_priors_plot2_n1000 / updating_priors_plot3_n1000

ggsave(updating_prior_n1000, filename = "plots/updating_prior_n1000.png", 
       units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.75)





updating_priors_same_plot <- updating_priors_plot1_n1000 / updating_priors_plot1_n150 / updating_priors_plot1_n50

ggsave(updating_priors_same_plot, filename = "plots/updating_priors_same_plot.png", units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.75)


updating_priors_flipped_plot <- updating_priors_plot3_n1000 / updating_priors_plot3_n150 / updating_priors_plot3_n50

ggsave(updating_priors_flipped_plot, filename = "plots/updating_priors_flipped_plot.png", units = "in", dpi = 300,
       width = 8.25, height = (1/2)*11.75, scale = 1.75)


# Meta analysis where null findings are published all the time, half as often, and 1/4 as often
# versus bayesian - worth simulating?

# Frequentist model summaries
lmmods_same_n50
lmmods_same_n150
lmmods_same_n1000

lmmods_noisy_n50
lmmods_noisy_n150
lmmods_noisy_n1000

lmmods_flipped_n50
lmmods_flipped_n150
lmmods_flipped_n1000

# Bayesian model draws 
# citation: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/bayesian-ma.html 
bayesmod_same_draws_l_n50
bayesmod_same_draws_l_n150
bayesmod_same_draws_l_n1000 

bayesmod_noisy_draws_l_n50
bayesmod_noisy_draws_l_n150
bayesmod_noisy_draws_l_n1000 

bayesmod_flipped_draws_l_n50
bayesmod_flipped_draws_l_n150
bayesmod_flipped_draws_l_n1000 

# Meta analyses - frequentist
meta_freq_n50_same
meta_freq_n150_same 
meta_freq_n1000_same 

meta_freq_n50_noisy
meta_freq_n150_noisy 
meta_freq_n1000_noisy 

meta_freq_n50_flipped 
meta_freq_n150_flipped 
meta_freq_n1000_flipped 


# Meta analyses - bayesian
meta.brm_same_draws_n50
meta.brm_same_draws_n150
meta.brm_same_draws_n1000 

meta.brm_noisy_draws_n50
meta.brm_noisy_draws_n150
meta.brm_noisy_draws_n1000 

meta.brm_flipped_draws_n50
meta.brm_flipped_draws_n150
meta.brm_flipped_draws_n1000 


# Simulate "publication" of non-significant frequentist model estimates 
# summaries with p = 0, p = 0.2, p = 0.4, p = 0.6, p = 0.8, p = 1.0

set.seed(3946)
lmmods_same_n50_missing <- lmmods_same_n50 %>%
  filter(!effect == "b_Intercept") %>%
  rowwise() %>%
  mutate(publish_p0  = ifelse(p > 0.05, rbinom(1, 1, 0), 1),
         publish_p20 = ifelse(p > 0.05, rbinom(1, 1, 0.2), 1),
         publish_p40 = ifelse(p > 0.05, rbinom(1, 1, 0.4), 1),
         publish_p60 = ifelse(p > 0.05, rbinom(1, 1, 0.6), 1),
         publish_p80 = ifelse(p > 0.05, rbinom(1, 1, 0.8), 1),
         publish_p100 = ifelse(p > 0.05, rbinom(1, 1, 1), 1)
  ) 

set.seed(8457)
lmmods_same_n150_missing <- lmmods_same_n150 %>%
  filter(!effect == "b_Intercept") %>%
  rowwise() %>%
  mutate(publish_p0  = ifelse(p > 0.05, rbinom(1, 1, 0), 1),
         publish_p20 = ifelse(p > 0.05, rbinom(1, 1, 0.2), 1),
         publish_p40 = ifelse(p > 0.05, rbinom(1, 1, 0.4), 1),
         publish_p60 = ifelse(p > 0.05, rbinom(1, 1, 0.6), 1),
         publish_p80 = ifelse(p > 0.05, rbinom(1, 1, 0.8), 1),
         publish_p100 = ifelse(p > 0.05, rbinom(1, 1, 1), 1)
  ) 


set.seed(9475)
lmmods_same_n1000_missing <- lmmods_same_n1000 %>%
  filter(!effect == "b_Intercept") %>%
  rowwise() %>%
  mutate(publish_p0  = ifelse(p > 0.05, rbinom(1, 1, 0), 1),
         publish_p20 = ifelse(p > 0.05, rbinom(1, 1, 0.2), 1),
         publish_p40 = ifelse(p > 0.05, rbinom(1, 1, 0.4), 1),
         publish_p60 = ifelse(p > 0.05, rbinom(1, 1, 0.6), 1),
         publish_p80 = ifelse(p > 0.05, rbinom(1, 1, 0.8), 1),
         publish_p100 = ifelse(p > 0.05, rbinom(1, 1, 1), 1)
  ) 

# Meta analysis with missing data from publication bias


lmmods_same_n50_missing <- lmmods_same_n50_missing %>% filter(!mod %in% "M")
lmmods_same_n150_missing <- lmmods_same_n150_missing %>% filter(!mod %in% "M")
lmmods_same_n1000_missing <- lmmods_same_n1000_missing %>% filter(!mod %in% "M")

get_meta_bias <- function(param, model_results) {

metaan_n50_missings <- list()
published <- list()
params <- effects[2:5]

for (k in 1:6) {
  
    filtered_pub <- model_results %>%
      filter(!!sym(names(model_results)[k+5]) == 1)
  
    published[[k]] <- filtered_pub %>% 
      filter(effect %in% params[param])
  }

for (p in 1:6) {
  metaan_n50_missings[[p]] <- Metaan::estmeta(Beta = published[[p]]$est, 
                                              u = published[[p]]$est + 1.96*published[[p]]$se,
                                              l = published[[p]]$est - 1.96*published[[p]]$se,
                                              test = 'FIXED',
                                              conf.level = 0.95)
  
}


results_tab <- tibble(
  param = rep(params[param], 6),
  pub_bias = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  meta_est = NA,
  meta_lb = NA,
  meta_ub = NA
  
)

for (i in 1:6) {
  
  results_tab[i, 3] <- metaan_n50_missings[[i]]$Beta_tot
  results_tab[i, 4] <- metaan_n50_missings[[i]]$l
  results_tab[i, 5] <- metaan_n50_missings[[i]]$u
  
}

return(results_tab)

}

get_meta_bias(4, lmmods_same_n50_missing)
get_meta_bias(1, lmmods_same_n50_missing)
get_meta_bias(2, lmmods_same_n50_missing)

coef_bias_n50 <- bind_rows(get_meta_bias(1, lmmods_same_n50_missing),
                            get_meta_bias(2, lmmods_same_n50_missing),
                            get_meta_bias(3, lmmods_same_n50_missing),
                            get_meta_bias(4, lmmods_same_n50_missing))

coef_bias_n150 <- bind_rows(get_meta_bias(1, lmmods_same_n150_missing),
                            get_meta_bias(2, lmmods_same_n150_missing),
                            get_meta_bias(3, lmmods_same_n150_missing),
                            get_meta_bias(4, lmmods_same_n150_missing))

coef_bias_n1000 <- bind_rows(get_meta_bias(1, lmmods_same_n1000_missing),
                             get_meta_bias(2, lmmods_same_n1000_missing),
                             get_meta_bias(3, lmmods_same_n1000_missing),
                             get_meta_bias(4, lmmods_same_n1000_missing))


add_same_hline <- function(d) {
d %>%
  mutate(
    hline = case_when(param == "b_poverty_z" ~ beta_strong,
                      param == "b_inequal_z" ~ beta_moderate,
                      param == "b_spending_z" ~ beta_small,
                      param == "b_staffing_z" ~ beta_negligible)
  ) %>%
    mutate(
      param = factor(param, levels = c("b_poverty_z", "b_inequal_z", "b_spending_z", "b_staffing_z"))
    )
}

coef_bias_n50 <- add_same_hline(coef_bias_n50)
coef_bias_n150 <- add_same_hline(coef_bias_n150)
coef_bias_n1000 <- add_same_hline(coef_bias_n1000)

pubbias_plot_50 <- coef_bias_n50 %>%
  ggplot() +
  geom_segment(aes(x = pub_bias, xend = pub_bias, y = meta_lb, yend = meta_ub)) +
  geom_point(aes(x = pub_bias, y = meta_est), size = 0.5) +
  geom_hline(aes(yintercept = hline), size = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(rows = ~param, labeller = labeller(param = effect_facet_labels[2:5])) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  ylab("Meta-analysis Estimate") +
  xlab("Probability of Null Results Published") +
  ggtitle("N = 50")

pubbias_plot_150 <- coef_bias_n150 %>%
  ggplot() +
  geom_segment(aes(x = pub_bias, xend = pub_bias, y = meta_lb, yend = meta_ub)) +
  geom_point(aes(x = pub_bias, y = meta_est), size = 0.5) +
  geom_hline(aes(yintercept = hline), size = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(rows = ~param, labeller = labeller(param = effect_facet_labels[2:5])) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  ylab("Meta-analysis Estimate") +
  xlab("Probability of Null Results Published") +
  ggtitle("N = 150")

pubbias_plot_1000 <- coef_bias_n1000 %>%
  ggplot() +
  geom_segment(aes(x = pub_bias, xend = pub_bias, y = meta_lb, yend = meta_ub)) +
  geom_point(aes(x = pub_bias, y = meta_est), size = 0.5) +
  geom_hline(aes(yintercept = hline), size = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(rows = ~param, labeller = labeller(param = effect_facet_labels[2:5])) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  ylab("Meta-analysis Estimate") +
  xlab("Probability of Null Results Published") +
  ggtitle("N = 1000")

pubbias_plot <- pubbias_plot_1000 / pubbias_plot_150 / pubbias_plot_50

ggsave(pubbias_plot, filename = "plots/pubbias_plot.png", units = "in", dpi = 300,
       width = (1/2)*8.25, height = (1/3)*11.75, scale = 1.75)



# Create empirical effect dataset to analyse using both frequentist and bayesian 
# methods and show contrast in how results are interpreted

# update this with multivariate normal
create_empirical_dataset <- function(n) {
  set.seed(50)
  
  dat <- rnorm_multi(n = n,
                     varnames = c("care_z", "poverty_z", "inequal_z", "spending_z", "staffing_z"),
                     mu = 0,
                     sd = 1,
                     r = c(1, beta_strong, beta_moderate, beta_small, beta_negligible,
                           beta_strong, 1, 0, 0, 0,
                           beta_moderate, 0, 1, 0, 0,
                           beta_small, 0, 0, 1, 0,
                           beta_negligible, 0, 0, 0, 1),
                     empirical = TRUE
  )
  
  return(dat)
}

set.seed(89)
example_dat_n50  <- create_empirical_dataset(50)
sd(example_dat_n50$care_z)
sd(example_dat_n50$poverty_z)
example_dat_n150 <- create_empirical_dataset(150)
sd(example_dat_n150$care_z)
sd(example_dat_n150$poverty_z)
example_dat_n1000 <- create_empirical_dataset(1000)
sd(example_dat_n1000$care_z)
sd(example_dat_n1000$poverty_z)

lm(data = example_dat_n50, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) %>% summary()
lm(data = example_dat_n150, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) %>% summary()
lm(data = example_dat_n1000, formula = care_z ~ poverty_z + inequal_z + spending_z + staffing_z) %>% summary()

write_csv(example_dat_n50, "data/example_data_n50.csv")
write_csv(example_dat_n150, "data/example_data_n150.csv")
write_csv(example_dat_n1000, "data/example_data_n1000.csv")


# table for multilevel models - from simulations: median coefficient, IQR, % p <0.05

ml_zero <- ml_comparison_sims %>%
  summarise(
    median_coef = median(coefficient),
    IQR_lb = quantile(coefficient, 0.25),
    IQR_ub = quantile(coefficient, 0.75),
    percent_p = sum(p < 0.05) / n()
  ) %>%
  mutate_at(vars(median_coef:percent_p), ~round(., 2)) %>%
  mutate(
    condition = "Within effect equals 0", .before = modtype
  )

ml_opposite <- ml_suppression_comparison_sims %>%
  summarise(
    median_coef = median(coefficient),
    IQR_lb = quantile(coefficient, 0.25),
    IQR_ub = quantile(coefficient, 0.75),
    percent_p = sum(p < 0.05) / n()
  ) %>%
  mutate_at(vars(median_coef:percent_p), ~round(., 2)) %>%
  mutate(
    condition = "Opposite within and between effects", .before = modtype
  )

ml_same <- ml_reinforce_comparison_sims %>%
  summarise(
    median_coef = median(coefficient),
    IQR_lb = quantile(coefficient, 0.25),
    IQR_ub = quantile(coefficient, 0.75),
    percent_p = sum(p < 0.05) / n()
  ) %>%
  mutate_at(vars(median_coef:percent_p), ~round(., 2)) %>%
  mutate(
    condition = "Equal sized within and between effects", .before = modtype
  )

ml_tab <- bind_rows(ml_zero, ml_opposite, ml_same)

ml_tab <- ml_tab %>%
  mutate(
    IQR = paste0(IQR_lb, ", ", IQR_ub), .before = percent_p
  ) %>%
  select(-IQR_lb, -IQR_ub)

write_csv(ml_tab, file = "tabs_out/ml_tab.csv")

# 




