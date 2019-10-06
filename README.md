# Repository for Data and Code for "When Groups Fall Apart: Measuring Transnational Polarization with Twitter from the Arab Uprisings"

## by Robert Kubinec and John Owen

This repository has all the code and data necessary to replicate the analyses in the paper "When Groups Fall Apart: A Study of Transnational Polarization with Twitter from the Arab Uprisings". The PDF of the paper is also available in this repository. This repository is live in the sense that updates to the paper and code will appear here as Git commits.

A description of the files is as follows:

- `analyze_model_maprect.R`: given a fitted model object produced by `run_model_maprect_v2.R`, this script will produce all figures and plots that appear in our paper.
- `run_model_maprect_v2.R`: this script takes anonymized Twitter data from both Egypt and Tunisia in the `data/` folder along with coding of Arab Twitter elites to estimate an item-response theory-vector autoregression (IRT-VAR) model. Note that the code is intended to be run on a cluster with `cmdstan` as it is too large to fit on a desktop machine.
- `irt_var_maprect_nonvarying_2d_v5.stan`: this script contains the Stan code used to fit the IRT-VAR model.
- `simulate_IRT_VAR_maprect.RMD`: this R Markdown file simulates the IRT-VAR model used in the paper and shows accurate parameter recovery. 

For any questions, please send an email to Robert Kubinec @ `rmk7@nyu.edu`.
