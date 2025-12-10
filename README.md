# Socio-Environmental-Predictors-of-Participation-in-Tropical-Working-Landscapes

This repository accompanies the manuscript “Who Joins Voluntary Conservation? Socio-Environmental Predictors of Participation in Tropical Working Landscapes", a study dedicated to understand the participation of landowners in the Projeto Conexão Mata Atlântica (PCMA)”.
It provides all scripts and data structures required to reproduce the Bayesian spatial analysis using the INLA framework.
Repository Contents

# Project Files
PCMA_INLA_Project.Rproj
R Project file used to organize the workflow and environment.
# Data
pcma_dataset_anonymized.rds
An anonymized and spatially dislocated dataset of rural properties used in the modelling.
(Municipality codes preserved; coordinates randomized to protect confidentiality.)
# Scripts
# 01_functions.R
Helper functions for data cleaning, quartile generation, model diagnostics, and plotting.
# 02_INLA_model_pipeline.R
Main analysis script implementing:
- preparation of predictors
- hierarchical logistic INLA model
- model fit metrics (DIC, WAIC, CPO)
- forest plot of odds ratios
- posterior summaries and hyperparameter extraction
- LOMO cross-validation and bootstrap validation
# 03_generate_tables_figures.R
Script to export Supplementary Table S1 (fixed effects), Table S2 (hyperparameters),
and Supplementary Figure S1 (posterior of τ).

# Documentation
Table with the data for analysis, anonymized and without geolocation "dataset_ambio_paper.xlsx" 
