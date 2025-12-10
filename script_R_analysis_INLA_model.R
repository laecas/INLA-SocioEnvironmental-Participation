############################################################
# INLA MODEL FOR PCMA PARTICIPATION (REPRODUCIBLE PIPELINE)
# Author: 
# This script implements the complete modelling workflow
# described in the Ambio manuscript
############################################################

## 0. GENERAL SETUP -------------------------------------------------------

# Clear workspace
rm(list = ls())

# Set seed for reproducibility
set.seed(1234)

# set a working directory
setwd("")

## 1. LOAD PACKAGES AND DATA ---------------------------------------------

# Spatial and data manipulation
library(sf)
library(dplyr)
library(tidyr)
library(forcats)
library(tibble)
library(readxl)

# Modelling and statistics
library(INLA)      # Bayesian INLA
library(pROC)      # ROC curves and AUC
library(caret)     # createDataPartition for stratified sampling

# Visualisation (optional, for diagnostics)
library(ggplot2)
library(stringr)

#-----------------------------------------------------------------------
# Read the dataset of rural properties
# (Replace the path below with your own file path)
#-----------------------------------------------------------------------
pcma_sf <- read_excel("dataset_ambio_paper.xlsx")

# Inspect structure
str(pcma_sf)

#----------------------------------------------------------
# Create sequential ID (same logic as original script)
#----------------------------------------------------------
data <- pcma_sf %>% 
  mutate(ID = 1:nrow(.))

#----------------------------------------------------------
# Apply the filter used in the original model:
# Keep only properties with NUM_AREA >= 0.5 ha
#----------------------------------------------------------
pcma_df <- data %>% 
  filter(NUM_AREA >= 0.5)

# Response variable must be 0/1 integer
pcma_df <- pcma_df %>%
  mutate(
    PCMA = as.integer(as.character(PCMA))  # ensure 0/1
  )

## 2. PREPARE EXPLANATORY VARIABLES --------------------------------------

# 2.1. Helper function to create quartile factors
force_quartiles <- function(x) {
  # Ensure strictly increasing breaks (jitter ties if needed)
  qs <- quantile(x, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
  if (length(unique(qs)) < 5) {
    qs <- jitter(qs, amount = 1e-6)
  }
  cut(x, breaks = qs, include.lowest = TRUE)
}

# 2.2. Create quartile factors for all continuous predictors
pcma_df <- pcma_df %>%
  mutate(
    # Municipality identifiers (factor and integer)
    municipio      = as.factor(geocodigo),
    municipio_id   = as.integer(geocodigo),
    
    # Quartile-based factors used in the OR model
    slope_f        = force_quartiles(slope),
    transcost_f    = force_quartiles(transcost),
    pqmean17_f     = force_quartiles(pqmean17),
    NVCp2017_f     = force_quartiles(NVCp2017),
    NUM_MODULO_f   = force_quartiles(NUM_MODULO),
    wcbio12_f      = force_quartiles(wcbio12),
    APdistmean_f   = force_quartiles(APdistmean),
    APPforestP_f   = force_quartiles(APPforestP),
    APP_ha_f       = force_quartiles(APP_ha),
    Elevation_f    = force_quartiles(Elevation),
    Aspect_f       = force_quartiles(Aspect)
  )

# Check that each factor has 4 levels
sapply(
  pcma_df[, grep("_f$", names(pcma_df))],
  function(x) length(unique(x))
)

# (Optional) Standardize continuous covariates for multicollinearity checks
vars_continuous <- c("NVCp2017", "pqmean17", "transcost", "NUM_MODULO",
                     "slope", "wcbio12", "APdistmean", "APPforestP",
                     "APP_ha", "Elevation", "Aspect")

pcma_df[vars_continuous] <- scale(pcma_df[vars_continuous])

## 3. SPECIFY AND FIT THE MAIN INLA MODEL --------------------------------

# 3.1. Model formula
# Hierarchical logistic regression with an IID random intercept for municipality
formula_pcma <- PCMA ~
  f(municipio_id, model = "iid") +
  NVCp2017_f + pqmean17_f + transcost_f + NUM_MODULO_f +
  slope_f + wcbio12_f +
  APPforestP_f + APP_ha_f + Elevation_f + Aspect_f + APdistmean_f

# 3.2. Fit the Bayesian INLA model
# Here we keep default priors
model_pcma <- inla(
  formula_pcma,
  family  = "binomial",
  data    = pcma_df,
  control.predictor = list(link = 1, compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)

# Quick summary
summary(model_pcma)

## 4. GLOBAL PREDICTIVE PERFORMANCE (FULL DATA) --------------------------

# 4.1. Posterior mean predicted probabilities
pcma_df$prob_pcma <- model_pcma$summary.fitted.values[, "mean"]

# 4.2. ROC curve and AUC
roc_full <- roc(pcma_df$PCMA, pcma_df$prob_pcma)
auc_full <- auc(roc_full)

# 4.3. Tjur's R2 (coefficient of discrimination)
mean_prob_1 <- mean(pcma_df$prob_pcma[pcma_df$PCMA == 1])
mean_prob_0 <- mean(pcma_df$prob_pcma[pcma_df$PCMA == 0])
tjur_R2     <- mean_prob_1 - mean_prob_0

# 4.4. Plot ROC
plot(roc_full, main = paste("ROC curve - full data | AUC =", round(auc_full, 3)))


## 5. YOUDEN-OPTIMAL CUTOFF AND CONFUSION MATRIX ------------------------

# 5.1. Compute Youden-optimal cutoff
youden <- coords(
  roc_full,
  x = "best",
  best.method = "youden",
  transpose = TRUE
)

youden_cutoff <- youden["threshold"]

# 5.2. Classify properties as participants / non-participants using Youden cutoff
pcma_df$pred_binary <- ifelse(pcma_df$prob_pcma >= youden_cutoff, 1, 0)

# 5.3. Confusion matrix and basic metrics
conf_mat <- table(
  Predicted = pcma_df$pred_binary,
  Observed  = pcma_df$PCMA
)

TP <- conf_mat["1", "1"]
TN <- conf_mat["0", "0"]
FP <- conf_mat["1", "0"]
FN <- conf_mat["0", "1"]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy    <- (TP + TN) / sum(conf_mat)

classification_metrics <- data.frame(
  Youden_cutoff = youden_cutoff,
  Sensitivity   = sensitivity,
  Specificity   = specificity,
  Accuracy      = accuracy,
  AUC_full      = as.numeric(auc_full),
  Tjur_R2       = tjur_R2
)

## 6. LOMO: LEAVE-ONE-MUNICIPALITY-OUT VALIDATION -----------------------

# 6.1. Vector of unique municipalities
municipalities <- unique(pcma_df$municipio_id)
auc_lomo <- numeric(0)

for (mun in municipalities) {
  # Training: all municipalities except 'mun'
  train_df <- pcma_df[pcma_df$municipio_id != mun, ]
  # Test: only municipality 'mun'
  test_df  <- pcma_df[pcma_df$municipio_id == mun, ]
  
  # Skip if test set has only one class
  if (length(unique(test_df$PCMA)) < 2) next
  
  # Combine train and test so that indices match the INLA fit
  combined_df <- rbind(train_df, test_df)
  idx_test    <- (nrow(train_df) + 1):nrow(combined_df)
  
  # Fit the model for this LOMO iteration
  model_lomo <- try(
    inla(
      formula_pcma,
      family  = "binomial",
      data    = combined_df,
      control.predictor = list(link = 1, compute = TRUE),
      control.compute   = list(dic = FALSE, waic = FALSE, cpo = FALSE)
    ),
    silent = TRUE
  )
  
  if (inherits(model_lomo, "try-error")) next
  
  # Predicted probabilities for test municipality
  pred_test <- model_lomo$summary.fitted.values[idx_test, "mean"]
  
  # AUC for this municipality
  roc_lomo <- try(roc(test_df$PCMA, pred_test), silent = TRUE)
  if (!inherits(roc_lomo, "try-error")) {
    auc_lomo <- c(auc_lomo, as.numeric(auc(roc_lomo)))
  }
}

# 6.2. Summary statistics for LOMO AUC
lomo_summary <- data.frame(
  mean_AUC   = mean(auc_lomo),
  sd_AUC     = sd(auc_lomo),
  q2_5_AUC   = quantile(auc_lomo, 0.025),
  q50_AUC    = quantile(auc_lomo, 0.5),
  q97_5_AUC  = quantile(auc_lomo, 0.975),
  n_municip  = length(auc_lomo)
)

## 7. BOOTSTRAP VALIDATION (100 SAMPLES) ---------------------------------

# 7.1. Stratified split into training (70%) and fixed test (30%)
set.seed(42)
split_index <- createDataPartition(pcma_df$PCMA, p = 0.7, list = FALSE)
test_fixed  <- pcma_df[-split_index, ]

if (length(unique(test_fixed$PCMA)) < 2) {
  stop("The fixed test set does not contain both classes (0 and 1).")
}

n_boot   <- 100
auc_boot <- numeric(n_boot)

for (b in 1:n_boot) {
  cat("Bootstrap iteration:", b, "of", n_boot, "\n")
  
  # 7.2. Bootstrap sample from the training portion (with replacement)
  train_ids <- sample(split_index, size = length(split_index), replace = TRUE)
  train_bt  <- pcma_df[train_ids, ]
  
  # Ensure both classes are present in training
  if (length(unique(train_bt$PCMA)) < 2) {
    auc_boot[b] <- NA
    next
  }
  
  # Combine bootstrap training and fixed test set
  combined_bt <- rbind(train_bt, test_fixed)
  idx_test_bt <- (nrow(train_bt) + 1):nrow(combined_bt)
  
  # 7.3. Fit INLA model
  model_bt <- try(
    inla(
      formula_pcma,
      family  = "binomial",
      data    = combined_bt,
      control.predictor = list(link = 1, compute = TRUE),
      control.compute   = list(dic = FALSE, waic = FALSE, cpo = FALSE)
    ),
    silent = TRUE
  )
  
  if (inherits(model_bt, "try-error")) {
    auc_boot[b] <- NA
    next
  }
  
  # 7.4. Predicted probabilities for the fixed test subset
  pred_bt <- model_bt$summary.fitted.values[idx_test_bt, "mean"]
  
  roc_bt <- try(roc(test_fixed$PCMA, pred_bt), silent = TRUE)
  if (!inherits(roc_bt, "try-error")) {
    auc_boot[b] <- as.numeric(auc(roc_bt))
  } else {
    auc_boot[b] <- NA
  }
}

# Remove NAs
auc_boot_valid <- auc_boot[is.finite(auc_boot)]

bootstrap_summary <- data.frame(
  mean_AUC   = mean(auc_boot_valid),
  sd_AUC     = sd(auc_boot_valid),
  q2_5_AUC   = quantile(auc_boot_valid, 0.025),
  q50_AUC    = quantile(auc_boot_valid, 0.5),
  q97_5_AUC  = quantile(auc_boot_valid, 0.975),
  n_boot_eff = length(auc_boot_valid)
)

# 8. FOREST PLOT OF FIXED EFFECTS (ODDS RATIOS)

## 8.1. Extract INLA fixed-effect summary
summary_orPCMA <- as.data.frame(model_pcma$summary.fixed)
summary_orPCMA$variavel <- rownames(summary_orPCMA)

## 8.2. Identify categorical (factor quartile) vs. non-factor variables
summary_orPCMA <- summary_orPCMA %>%
  filter(variavel != "(Intercept)") %>%
  mutate(
    OR         = exp(mean),
    lower      = exp(`0.025quant`),
    upper      = exp(`0.975quant`),
    significativo = lower > 1 | upper < 1,
    
    # Identify the base variable name and quartile interval
    nome_var = ifelse(grepl("_f\\(", variavel),
                      gsub("_f\\(.*", "", variavel),
                      variavel),
    
    intervalo = ifelse(grepl("_f\\(", variavel),
                       gsub(".*_f\\((.*)\\]", "\\1", variavel),
                       NA)
  )

## 8.3. Rank quartiles (Q2–Q4) based on numeric cutpoints
summary_orPCMA <- summary_orPCMA %>%
  group_by(nome_var) %>%
  mutate(
    ordem = ifelse(!is.na(intervalo),
                   rank(as.numeric(sub(",.*", "", intervalo))),
                   1),
    
    quartil = case_when(
      is.na(intervalo) ~ "Binary",   # Not quartile-based
      ordem == 1 ~ "Q2",
      ordem == 2 ~ "Q3",
      ordem == 3 ~ "Q4"
    )
  ) %>%
  ungroup()

## 8.4. Add Q1 reference category for each factor variable
factor_vars <- unique(summary_orPCMA$nome_var[summary_orPCMA$quartil != "Binary"])

ref_df <- data.frame(
  nome_var = factor_vars,
  OR       = 1,
  lower    = NA,
  upper    = NA,
  significativo = FALSE,
  quartil  = "Q1",
  rotulo   = NA,     # to be filled next
  shape    = NA,
  ordem_y  = NA
)

## 8.5. Combine observed quartiles with reference quartiles
summary_orPCMA <- bind_rows(summary_orPCMA, ref_df)

## 8.6. Assign human-readable labels and symbol types (shapes)
summary_orPCMA <- summary_orPCMA %>%
  mutate(
    rotulo = case_when(
      # --- Riparian APP area ---
      nome_var == "APP_ha" & quartil == "Q1" ~ "APP: Lowest",
      nome_var == "APP_ha" & quartil == "Q2" ~ "APP: Low",
      nome_var == "APP_ha" & quartil == "Q3" ~ "APP: High",
      nome_var == "APP_ha" & quartil == "Q4" ~ "APP: Highest",
      
      # --- Riparian APP deficit ---
      nome_var == "APPforestP" & quartil == "Q1" ~ "APP deficit: Lowest",
      nome_var == "APPforestP" & quartil == "Q2" ~ "APP deficit: Low",
      nome_var == "APPforestP" & quartil == "Q3" ~ "APP deficit: High",
      nome_var == "APPforestP" & quartil == "Q4" ~ "APP deficit: Highest",
      
      # --- Distance to protected areas ---
      nome_var == "APdistmean" & quartil == "Q1" ~ "Protected area distance: Lowest",
      nome_var == "APdistmean" & quartil == "Q2" ~ "Protected area distance: Low",
      nome_var == "APdistmean" & quartil == "Q3" ~ "Protected area distance: High",
      nome_var == "APdistmean" & quartil == "Q4" ~ "Protected area distance: Highest",
      
      # --- Aspect orientation ---
      nome_var == "Aspect" & quartil == "Q1" ~ "Aspect: North to East",
      nome_var == "Aspect" & quartil == "Q2" ~ "Aspect: East to South",
      nome_var == "Aspect" & quartil == "Q3" ~ "Aspect: South to West",
      nome_var == "Aspect" & quartil == "Q4" ~ "Aspect: West to North",
      
      # --- Elevation ---
      nome_var == "Elevation" & quartil == "Q1" ~ "Elevation: Lowest",
      nome_var == "Elevation" & quartil == "Q2" ~ "Elevation: Low",
      nome_var == "Elevation" & quartil == "Q3" ~ "Elevation: High",
      nome_var == "Elevation" & quartil == "Q4" ~ "Elevation: Highest",
      
      # --- Property size ---
      nome_var == "NUM_MODULO" & quartil == "Q1" ~ "Farm size: Very small",
      nome_var == "NUM_MODULO" & quartil == "Q2" ~ "Farm size: Small",
      nome_var == "NUM_MODULO" & quartil == "Q3" ~ "Farm size: Large",
      nome_var == "NUM_MODULO" & quartil == "Q4" ~ "Farm size: Very large",
      
      # --- Native vegetation cover ---
      nome_var == "NVCp2017" & quartil == "Q1" ~ "Native vegetation (2017): Lowest",
      nome_var == "NVCp2017" & quartil == "Q2" ~ "Native vegetation (2017): Low",
      nome_var == "NVCp2017" & quartil == "Q3" ~ "Native vegetation (2017): High",
      nome_var == "NVCp2017" & quartil == "Q4" ~ "Native vegetation (2017): Highest",
      
      # --- Pasture quality ---
      nome_var == "pqmean17" & quartil == "Q1" ~ "Pasture quality (2017): Lowest",
      nome_var == "pqmean17" & quartil == "Q2" ~ "Pasture quality (2017): Low",
      nome_var == "pqmean17" & quartil == "Q3" ~ "Pasture quality (2017): High",
      nome_var == "pqmean17" & quartil == "Q4" ~ "Pasture quality (2017): Highest",
      
      # --- Slope ---
      nome_var == "slope" & quartil == "Q1" ~ "Slope: Flat",
      nome_var == "slope" & quartil == "Q2" ~ "Slope: Gentle",
      nome_var == "slope" & quartil == "Q3" ~ "Slope: Steep",
      nome_var == "slope" & quartil == "Q4" ~ "Slope: Very steep",
      
      # --- Transportation cost ---
      nome_var == "transcost" & quartil == "Q1" ~ "Transportation cost (2017): Lowest",
      nome_var == "transcost" & quartil == "Q2" ~ "Transportation cost (2017): Low",
      nome_var == "transcost" & quartil == "Q3" ~ "Transportation cost (2017): High",
      nome_var == "transcost" & quartil == "Q4" ~ "Transportation cost (2017): Highest",
      
      # --- Precipitation ---
      nome_var == "wcbio12" & quartil == "Q1" ~ "Precipitation: Lowest",
      nome_var == "wcbio12" & quartil == "Q2" ~ "Precipitation: Low",
      nome_var == "wcbio12" & quartil == "Q3" ~ "Precipitation: High",
      nome_var == "wcbio12" & quartil == "Q4" ~ "Precipitation: Highest",
      
      TRUE ~ paste(nome_var, quartil)
    ),
    
    # Q1 will be displayed using a hollow symbol
    shape = ifelse(quartil == "Q1", "hollow", "solid")
  )

## 8.7. Vertical ordering for plotting (Q1 → Q4)
summary_orPCMA <- summary_orPCMA %>%
  mutate(ordem_y = case_when(
    quartil == "Q1" ~ 1,
    quartil == "Q2" ~ 2,
    quartil == "Q3" ~ 3,
    quartil == "Q4" ~ 4,
    TRUE ~ 5
  )) %>%
  arrange(nome_var, ordem_y)

## 8.8. Insert blank rows to visually separate variable groups
summary_orPCMA_com_espaco <- summary_orPCMA %>%
  group_by(nome_var) %>%
  group_modify(~ add_row(
    .x,
    variavel      = NA_character_,
    mean          = NA_real_,
    `0.025quant`  = NA_real_,
    `0.975quant`  = NA_real_,
    OR            = NA_real_,
    lower         = NA_real_,
    upper         = NA_real_,
    significativo = NA,
    intervalo     = NA_character_,
    ordem         = NA_integer_,
    quartil       = NA_character_,
    rotulo        = "",
    shape         = NA_character_,
    ordem_y       = NA_real_
  )) %>%
  ungroup()

# Remove the final empty row
summary_orPCMA_com_espaco <- head(summary_orPCMA_com_espaco, -1)

## 8.9. Define factor ordering for y-axis labels
summary_orPCMA_com_espaco$rotulo <- factor(
  summary_orPCMA_com_espaco$rotulo,
  levels = rev(unique(summary_orPCMA_com_espaco$rotulo))
)

summary_orPCMA <- summary_orPCMA %>%
  mutate(rotulo = factor(rotulo, levels = rev(unique(rotulo))))

## 8.11. Produce the final forest plot
ggplot(summary_orPCMA, aes(x = OR, y = rotulo, color = significativo)) +
  geom_point(
    aes(shape = shape),
    size = 2.8,
    stroke = ifelse(summary_orPCMA$shape == "hollow", 1.5, 1.0),
    na.rm = TRUE
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    na.rm = TRUE
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(
    values = c("TRUE" = "darkred", "FALSE" = "gray40"),
    name = "Significant"
  ) +
  scale_shape_manual(
    values = c("solid" = 16, "hollow" = 1),
    labels = c("Q2–Q4", "Q1 (reference)"),
    name   = "Quartile"
  ) +
  labs(
    x = "Odds Ratio",
    y = "Variable (Category)",
    title = "Odds Ratios with 95% Credible Intervals",
    subtitle = "Reference quartile (Q1) shown as hollow circles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )

summary_orPCMA <- summary_orPCMA %>%
  mutate(
    clean_label = rotulo
  )

## 9. EXTRACT BAYESIAN RESULTS (TABLES FOR MANUSCRIPT) -------------------

# 9.1 Extract INLA fixed-effects summary (CLEAN + CONSISTENT)

# Criar tabela fixa completa
fixed_table <- as.data.frame(model_pcma$summary.fixed) %>%
  tibble::rownames_to_column("parameter") %>%
  mutate(
    q025 = `0.025quant`,
    q50  = `0.5quant`,
    q975 = `0.975quant`,
    OR       = exp(mean),
    OR_lower = exp(q025),
    OR_upper = exp(q975),
    Significant = OR_lower > 1 | OR_upper < 1
  )

# Clean labels table
table_S1_clean <- summary_orPCMA %>%
  filter(!is.na(rotulo)) %>%          
  transmute(
    parameter = variavel,            
    Variable  = rotulo,              
    Quartile  = quartil              
  )

# 9.2 Build final Supplementary Table S1 (fixed effects)
table_S1_final <- fixed_table %>%
  left_join(table_S1_clean, by = "parameter") %>%      # junta pelos nomes do INLA
  mutate(
    # se não existir label bonito (ex.: Intercept), usa o próprio nome do parâmetro
    Variable = ifelse(is.na(Variable), parameter, Variable)
  ) %>%
  select(
    Variable,
    parameter,
    Quartile,
    mean,
    sd,
    q025,
    q50,
    q975,
    OR,
    OR_lower,
    OR_upper,
    Significant
  )

# 9.3. Hyperparameters (e.g., municipality random effect)
hyper_table <- as.data.frame(model_pcma$summary.hyperpar) %>%
  tibble::rownames_to_column("hyperparameter") %>%
  mutate(
    SD = ifelse(grepl("Precision", hyperparameter),
                1/sqrt(mean),
                NA)
  )

hyper_summary$hyperparameter <- rownames(hyper_summary)
rownames(hyper_summary) <- NULL

# For the municipality IID effect, convert precision to standard deviation
if ("Precision for municipio_id" %in% hyper_summary$hyperparameter) {
  idx <- which(hyper_summary$hyperparameter == "Precision for municipio_id")
  hyper_summary$SD_municipio_id <- NA_real_
  hyper_summary$SD_municipio_id[idx] <-
    1 / sqrt(hyper_summary$mean[idx])
}

# 9.4. Model fit criteria: DIC, WAIC, CPO, marginal likelihood
dic_value   <- model_pcma$dic$dic
dic_p_eff   <- model_pcma$dic$p.eff
waic_value  <- model_pcma$waic$waic
waic_p_eff  <- model_pcma$waic$p.eff
mlik        <- model_pcma$mlik[1, 1]  # marginal log-likelihood

# Summary of CPO (Conditional Predictive Ordinates)
cpo_values <- model_pcma$cpo$cpo
# Remove NA/zero values to avoid issues
cpo_valid  <- cpo_values[is.finite(cpo_values) & cpo_values > 0]
mean_log_cpo <- mean(-log(cpo_valid))

model_fit_summary <- data.frame(
  DIC          = dic_value,
  DIC_p_eff    = dic_p_eff,
  WAIC         = waic_value,
  WAIC_p_eff   = waic_p_eff,
  MarginalLogL = mlik,
  Mean_neglogCPO = mean_log_cpo
)

# 10. DISPLAY RESULTS IN CONSOLE (IMPORTANT FOR INSPECTION)
cat("\n================ FIXED EFFECTS (POSTERIOR SUMMARY) ================\n")
print(fixed_summary)

cat("\n================ ODDS RATIOS (MAIN TABLE) ================\n")
print(or_table)

cat("\n================ HYPERPARAMETERS ================\n")
print(hyper_summary)

cat("\n================ MODEL FIT METRICS ================\n")
print(model_fit_summary)

cat("\n================ CLASSIFICATION METRICS (YOUDEN) ================\n")
print(classification_metrics)

cat("\n================ LOMO VALIDATION SUMMARY ================\n")
print(lomo_summary)

cat("\n================ BOOTSTRAP VALIDATION SUMMARY ================\n")
print(bootstrap_summary)

## 11. EXPORT TABLES FOR MANUSCRIPT / SUPPLEMENT --------------------------

# Create an output directory
if (!dir.exists("output_tables")) dir.create("output_tables")

write.csv2(table_S1_final,
           "output_tables/Supplementary_Table_S1_FIXED_EFFECTS.csv",
           row.names = FALSE)

write.csv(or_table,
          "output_tables/INLA_odds_ratios.csv",
          row.names = FALSE)

write.csv(hyper_table,
          "output_tables/Supplementary_Table_S2_HYPERPARAMETERS.csv",
          row.names = FALSE)

write.csv(model_fit_summary,
          "output_tables/INLA_model_fit_summary.csv",
          row.names = FALSE)

write.csv(classification_metrics,
          "output_tables/INLA_classification_metrics_youden.csv",
          row.names = FALSE)

write.csv(lomo_summary,
          "output_tables/INLA_LOMO_AUC_summary.csv",
          row.names = FALSE)

write.csv(bootstrap_summary,
          "output_tables/INLA_bootstrap_AUC_summary.csv",
          row.names = FALSE)

# 12. Supplementary Figure S1: Posterior marginal of random intercept precision
# Extract marginal distribution for the hyperparameter
marg_precision <- model_pcma$marginals.hyperpar$`Precision for municipio_id`

# Convert to data frame
df_marg <- as.data.frame(marg_precision)
colnames(df_marg) <- c("precision", "density")

# Plot posterior density
p_marg <- ggplot(df_marg, aes(x = precision, y = density)) +
  geom_line(color = "darkblue", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Precision (τ) of municipality random intercept",
    y = "Posterior density",
    title = "Posterior marginal distribution of τ",
    subtitle = "Used in hierarchical random-intercept INLA logistic model"
  )

print(p_marg)

############################################################
# END OF SCRIPT
############################################################
