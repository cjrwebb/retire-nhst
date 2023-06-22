# Should we retire the Null Hypothesis Significance Test in (some) social policy research? Decisive versus descriptive approaches to statistical uncertainty in research on apparent populations with small or modest effects

### Dr. Calum Webb

This repository hosts the code used to produce the simulations presented in the paper "Should we retire the Null Hypothesis Significance Test in (some) social policy research? Decisive versus descriptive approaches to statistical uncertainty in research on apparent populations with small or modest effects", presented at the Social Policy Association 2023 annual conference.

The script `power-finite-populations.R` includes the code used to generate figures 1, 2, 3, 5, and 6 and accompanying tables 1 and 2, covering the statistical power at commonly found sizes of apparent populations, simulations of coefficients and standard errors produced for 'strong', 'moderate', 'small', and 'negligible' effects. It includes simulation of hierarchical time-unit data to illustrate the problems with pooling data from apparent populations. Lastly, it includes the use of frequentist and Bayesian meta-analysis from simulated studies and the effect of publication bias on estimate bias.  

The script `empirical-analysis-example.R` includes the code used to generate figure 4 and tables 3 and 4. It uses a simulation from an empirical distribution for a model with an outcome predicted by a 'strong', 'moderate', 'small', and 'negligible' predictor in order to illustrate how uncertainty can be communicated under a Bayesian modelling framework compared to a frequentist framework using bootstrap sampling with replacement and permutation p-values. It includes examples of using the region of practical equivalence, the probability of direction, and policy relevant values, to describe the uncertainty of the parameter estimates. 

The `data` folder includes csv files containing the empirical simulation that users may wish to use in order to experiment with the descriptive approaches shown. 
