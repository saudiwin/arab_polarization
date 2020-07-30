# Repository for Data and Code for "When Groups Fall Apart: Identifying Transnational Polarization with Twitter from the Arab Uprisings"

## by Robert Kubinec and John Owen

This repository has all the code necessary to replicate the analyses in the paper "When Groups Fall Apart: Identifying Transnational Polarization with Twitter from the Arab Uprisings" (forthcoming *Political Analysis*). 

A description of the files is as follows:

- `analyze_model_maprect.R`: given a fitted model object produced by `run_model_maprect_v2.R`, this script will produce all figures and plots that appear in our paper. 
- `run_model_maprect_v2.R`: this script takes anonymized Twitter data from both Egypt and Tunisia in the `data/` folder along with coding of Arab Twitter elites to estimate an item-response theory-vector autoregression (IRT-VAR) model. Note that the code is intended to be run on a cluster with `cmdstan` as it is too large to fit on a desktop machine. It will produce `.R` files that provide data and initialization values for use with `cmdstan` via the command line (see `cmdstan` getting started guide: https://github.com/stan-dev/cmdstan/wiki/Getting-Started-with-CmdStan). Note that additional `.sql` files are needed to obtain the aggregated Twitter data. These are available in the Harvard Dataverse repository for the paper.
- `irt_var_maprect_nonvarying_2d_v5.stan`: this script contains the Stan code used to fit the IRT-VAR model.
- `simulate_IRT_VAR_maprect.RMD`: this R Markdown file simulates the IRT-VAR model used in the paper and shows accurate parameter recovery. 

For any questions, please send an email to Robert Kubinec @ `rmk7@nyu.edu`.
