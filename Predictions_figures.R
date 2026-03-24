## =========================================================
## FIGURES c AND d — based exactly on the fitted INLA script
## =========================================================

# Required packages
library(ggplot2)
library(dplyr)
library(pROC)
library(scales)
library(gridExtra)

# ---------------------------------------------------------
# 1. Safety checks: use objects already created in your script
# ---------------------------------------------------------
if (!exists("pcma_df")) stop("Object 'pcma_df' not found.")
if (!exists("model_pcma")) stop("Object 'model_pcma' not found.")
if (!"prob_pcma" %in% names(pcma_df)) {
  stop("Column 'prob_pcma' not found in pcma_df. Run section 4.1 of your script first.")
}
if (!exists("roc_full")) stop("Object 'roc_full' not found. Run section 4.2 first.")
if (!exists("auc_full")) stop("Object 'auc_full' not found. Run section 4.2 first.")
if (!exists("youden")) stop("Object 'youden' not found. Run section 5.1 first.")
if (!exists("youden_cutoff")) stop("Object 'youden_cutoff' not found. Run section 5.1 first.")
if (!exists("sensitivity")) stop("Object 'sensitivity' not found. Run section 5.3 first.")
if (!exists("specificity")) stop("Object 'specificity' not found. Run section 5.3 first.")

# ---------------------------------------------------------
# 2. AUC confidence interval
# ---------------------------------------------------------
auc_ci <- as.numeric(ci.auc(roc_full))

# Ensure numeric scalar
youden_cutoff_num <- as.numeric(youden_cutoff)
sens_num <- as.numeric(sensitivity)
spec_num <- as.numeric(specificity)
auc_num  <- as.numeric(auc_full)

# ---------------------------------------------------------
# 3. ROC data frame
# ---------------------------------------------------------
roc_df <- data.frame(
  fpr = 1 - roc_full$specificities,
  tpr = roc_full$sensitivities
)

youden_df <- data.frame(
  fpr = 1 - spec_num,
  tpr = sens_num
)

# ---------------------------------------------------------
# 4. Figure c — ROC curve with Youden-optimal point
# ---------------------------------------------------------
fig_c <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.8
  ) +
  geom_line(
    color = "#d4a017",
    linewidth = 1.4
  ) +
  geom_segment(
    data = youden_df,
    aes(x = fpr, xend = fpr, y = 0, yend = tpr),
    inherit.aes = FALSE,
    linetype = "dotted",
    color = "gray40",
    linewidth = 0.8
  ) +
  geom_segment(
    data = youden_df,
    aes(x = 0, xend = fpr, y = tpr, yend = tpr),
    inherit.aes = FALSE,
    linetype = "dotted",
    color = "gray40",
    linewidth = 0.8
  ) +
  geom_point(
    data = youden_df,
    aes(x = fpr, y = tpr),
    inherit.aes = FALSE,
    shape = 4,
    size = 5,
    stroke = 1.3,
    color = "#d32f2f"
  ) +
  annotate(
    "segment",
    x = 0.62, xend = 0.66,
    y = 0.17, yend = 0.17,
    color = "#d4a017",
    linewidth = 1.6
  ) +
  annotate(
    "text",
    x = 0.69, y = 0.17,
    label = "ROC curve",
    hjust = 0,
    size = 3.8
  ) +
  annotate(
    "point",
    x = 0.82, y = 0.17,
    shape = 4,
    size = 4.8,
    stroke = 1.2,
    color = "#d32f2f"
  ) +
  annotate(
    "text",
    x = 0.87, y = 0.17,
    label = "Youden-optimal",
    hjust = 0,
    size = 3.8
  ) +
  annotate(
    "text",
    x = 0.50,
    y = -0.12,
    label = paste0(
      "AUC = ", sprintf("%.3f", auc_num),
      " (95% CI ", sprintf("%.3f", auc_ci[1]), "\u2013", sprintf("%.3f", auc_ci[3]), ")",
      " | Youden cutoff = ", sprintf("%.3f", youden_cutoff_num),
      " | Sens = ", sprintf("%.0f", sens_num * 100), "%, ",
      "Spec = ", sprintf("%.0f", spec_num * 100), "%"
    ),
    size = 3.6
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  labs(
    title = "c. ROC curve with Youden-optimal point",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "plain"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 12, 35, 12)
  )

# ---------------------------------------------------------
# 5. Figure d — distribution of predicted probabilities
# ---------------------------------------------------------
binwidth_hist <- 0.03

hist_info <- hist(
  pcma_df$prob_pcma,
  breaks = seq(0, 1, by = binwidth_hist),
  plot = FALSE
)

dens_obj <- density(
  pcma_df$prob_pcma,
  from = 0,
  to = 1,
  na.rm = TRUE
)

dens_df <- data.frame(
  x = dens_obj$x,
  density = dens_obj$y
)

max_count <- max(hist_info$counts, na.rm = TRUE)
max_dens  <- max(dens_df$density, na.rm = TRUE)
scale_factor <- max_count / max_dens

fig_d <- ggplot(pcma_df, aes(x = prob_pcma)) +
  geom_histogram(
    binwidth = binwidth_hist,
    boundary = 0,
    fill = "#E6C15A",
    color = "gray20",
    linewidth = 0.5
  ) +
  geom_line(
    data = dens_df,
    aes(x = x, y = density * scale_factor),
    inherit.aes = FALSE,
    color = "#d4a017",
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = youden_cutoff_num,
    linetype = "dashed",
    color = "#d32f2f",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = youden_cutoff_num + 0.02,
    y = max_count * 0.63,
    label = paste0("Youden cutoff = ", sprintf("%.3f", youden_cutoff_num)),
    color = "#b04a4a",
    hjust = 0,
    size = 4
  ) +
  scale_x_continuous(
    limits = c(0, 1.02),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / scale_factor, name = "Density")
  ) +
  labs(
    title = "d. Distribution of predicted probabilities",
    x = "Predicted probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "plain"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    axis.title.y.right = element_text(angle = 90, margin = margin(l = 10))
  )

# ---------------------------------------------------------
# 6. Show together
# ---------------------------------------------------------
grid.arrange(fig_c, fig_d, ncol = 2)

# ---------------------------------------------------------
# 7. Save high-resolution outputs
# ---------------------------------------------------------
if (!dir.exists("output_figures")) dir.create("output_figures")

ggsave(
  filename = "output_figures/Fig_c_ROC_Youden.png",
  plot = fig_c,
  width = 8.5,
  height = 6.0,
  dpi = 700,
  bg = "white"
)

ggsave(
  filename = "output_figures/Fig_d_Predicted_probabilities.png",
  plot = fig_d,
  width = 8.5,
  height = 6.0,
  dpi = 700,
  bg = "white"
)

ggsave(
  filename = "output_figures/Fig_cd_panel.png",
  plot = arrangeGrob(fig_c, fig_d, ncol = 2),
  width = 16,
  height = 6.2,
  dpi = 700,
  bg = "white"
)

# Optional vector versions
ggsave(
  filename = "output_figures/Fig_c_ROC_Youden.pdf",
  plot = fig_c,
  width = 8.5,
  height = 6.0,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "output_figures/Fig_d_Predicted_probabilities.pdf",
  plot = fig_d,
  width = 8.5,
  height = 6.0,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "output_figures/Fig_cd_panel.pdf",
  plot = arrangeGrob(fig_c, fig_d, ncol = 2),
  width = 16,
  height = 6.2,
  device = cairo_pdf,
  bg = "white"
)
