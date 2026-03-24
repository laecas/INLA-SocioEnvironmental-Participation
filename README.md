# Who Joins Voluntary Conservation Programs? Socio-Environmental Predictors of Participation in Tropical Working Landscapes

This repository accompanies the manuscript “Who Joins Voluntary Conservation? SocioEnvironmental Predictors of Participation in Tropical Working Landscapes", published in AMBIO. This study is dedicated to understand the participation of landowners in the Projeto Conexão Mata Atlântica (PCMA)”. 

https://doi.org/10.1007/s13280-026-02381-3

It provides all scripts and data structures required to reproduce the Bayesian spatial analysis using the INLA framework.
Repository Contents

# Project Files
R script with full data analysis:
- script_R_analysis_INLA_model.R

Dataset:
- dataset_ambio_paper.xlsx 
(Table with the dataset for analysis, anonymized and without geolocation)

# R script
# Step 1: summary
Helper functions for data cleaning, quartile generation, model diagnostics, and plotting.
# Step 2: INLA model pipeline
Main analysis script implementing:
- preparation of predictors
- hierarchical logistic INLA model
- model fit metrics (DIC, WAIC, CPO)
- forest plot of odds ratios
- posterior summaries and hyperparameter extraction
- LOMO cross-validation and bootstrap validation
# Step 3: generate tables and additional figure
Script to export Supplementary Table S1 (fixed effects), Table S2 (hyperparameters),
and Supplementary Figure S1 (posterior of τ).

__________________________________________________________
# Financial support
Fundação de Amparo à Pesquisa do Estado de São Paulo

Grant numbers 2022/16002-1, 2024/17486-8, 2024/12797-5, 2023/15877-7
