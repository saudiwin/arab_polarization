# Repository for Data and Code for "When Groups Fall Apart: A Study of Transnational Polarization with Twitter from the Arab Uprisings"

## by Robert Kubinec and John Owen

This repository has all the code and data necessary to replicate the analyses in the paper "When Groups Fall Apart: A Study of Transnational Polarization with Twitter from the Arab Uprisings". The PDF of the paper is also available in this repository. This repository is live in the sense that updates to the paper and code will appear here as Git commits. 

A description of the files is as follows:

- `out_fit_id_std_VAR_betax_2018-05-31.rds`: This is a fitted `rstan` model using Bayesian variational inference. It can be used in conjunction with `sum_stats_final.R` to replicate the figures in the paper without having to fit a new model.
- `sum_stats_final.R`: given a fitted model object produced by `run_model_IRT_final.R`, this script will produce all figures and plots that appear in our paper. 
- `run_model_IRT_final.R`: this script takes anonymized Twitter data from both Egypt and Tunisia in the `data/` folder along with coding of Arab Twitter elites to estimate an item-response theory-vector autoregression (IRT-VAR) model. It will save that model as an R RDS object with the prefix `out_fit_id_std_VAR_betax_`today's date`.rds`.
- `irt_var_final.stan`: this script contains the Stan code used to fit the IRT-VAR model. 

For any questions, please send an email to Robert Kubinec @ `rmk7xy@virginia.edu`.