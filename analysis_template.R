# METAbolique Validation Analysis Template
# Natalia Soto, MSc
# This script provides a reproducible structure for the main analyses
# performed in the validation study comparing METAbolique vs 24-hour recall.

# =========================
# 1. Packages
# =========================
required_packages <- c(
  "readr", "dplyr", "ggplot2", "broom", "tidyr"
)

installed <- rownames(installed.packages())
for (p in required_packages) {
  if (!p %in% installed) install.packages(p)
}

library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

# =========================
# 2. Load data
# =========================
# Replace with your own path if needed
data <- read_csv("mock_data.csv")

# =========================
# 3. Expected variables
# =========================
# Required columns:
# ID
# SEXO (0 = Male, 1 = Female)
# EDAD
# IMC
# CINTURA_CM
# CH_R24, PROT_R24, LIPIDOS_R24, CALO_R24
# CH_METABOLIQUE, PROT_METABOLIQUE, LIPIDOS_METABOLIQUE, CALO_METABOLIQUE
# CH_COM_EST, PROT_COM_EST, LIPIDOS_COM_EST, CALO_COM_EST
# SUS

# =========================
# 4. Data preparation
# =========================
data <- data %>%
  mutate(
    sexo = factor(SEXO, levels = c(0, 1), labels = c("Male", "Female")),

    error_CH_R24  = CH_COM_EST - CH_R24,
    error_CH_App  = CH_COM_EST - CH_METABOLIQUE,

    error_PRO_R24 = PROT_COM_EST - PROT_R24,
    error_PRO_App = PROT_COM_EST - PROT_METABOLIQUE,

    error_LIP_R24 = LIPIDOS_COM_EST - LIPIDOS_R24,
    error_LIP_App = LIPIDOS_COM_EST - LIPIDOS_METABOLIQUE,

    error_CAL_R24 = CALO_COM_EST - CALO_R24,
    error_CAL_App = CALO_COM_EST - CALO_METABOLIQUE,

    error_cal_abs = abs(CALO_COM_EST - CALO_METABOLIQUE)
  )

# =========================
# 5. Descriptive summaries
# =========================
summary_table <- data %>%
  summarise(
    n = n(),
    median_age = median(EDAD, na.rm = TRUE),
    q1_age = quantile(EDAD, 0.25, na.rm = TRUE),
    q3_age = quantile(EDAD, 0.75, na.rm = TRUE),
    female_n = sum(sexo == "Female", na.rm = TRUE),
    male_n = sum(sexo == "Male", na.rm = TRUE)
  )

print(summary_table)

# Nutritional summaries
nutrition_summary <- data %>%
  summarise(
    CH_R24_median = median(CH_R24, na.rm = TRUE),
    CH_APP_median = median(CH_METABOLIQUE, na.rm = TRUE),
    CH_REF = median(CH_COM_EST, na.rm = TRUE),

    PRO_R24_median = median(PROT_R24, na.rm = TRUE),
    PRO_APP_median = median(PROT_METABOLIQUE, na.rm = TRUE),
    PRO_REF = median(PROT_COM_EST, na.rm = TRUE),

    LIP_R24_median = median(LIPIDOS_R24, na.rm = TRUE),
    LIP_APP_median = median(LIPIDOS_METABOLIQUE, na.rm = TRUE),
    LIP_REF = median(LIPIDOS_COM_EST, na.rm = TRUE),

    CAL_R24_median = median(CALO_R24, na.rm = TRUE),
    CAL_APP_median = median(CALO_METABOLIQUE, na.rm = TRUE),
    CAL_REF = median(CALO_COM_EST, na.rm = TRUE)
  )

print(nutrition_summary)

# =========================
# 6. RMSE function
# =========================
rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

rmse_table <- tibble(
  Outcome = c("Carbohydrates", "Protein", "Lipids", "Calories"),
  RMSE_R24 = c(
    rmse(data$CH_COM_EST, data$CH_R24),
    rmse(data$PROT_COM_EST, data$PROT_R24),
    rmse(data$LIPIDOS_COM_EST, data$LIPIDOS_R24),
    rmse(data$CALO_COM_EST, data$CALO_R24)
  ),
  RMSE_App = c(
    rmse(data$CH_COM_EST, data$CH_METABOLIQUE),
    rmse(data$PROT_COM_EST, data$PROT_METABOLIQUE),
    rmse(data$LIPIDOS_COM_EST, data$LIPIDOS_METABOLIQUE),
    rmse(data$CALO_COM_EST, data$CALO_METABOLIQUE)
  )
)

print(rmse_table)

# =========================
# 7. Bland–Altman helper
# =========================
prep_ba <- function(df, reference, method) {
  avg <- (df[[reference]] + df[[method]]) / 2
  diff <- df[[reference]] - df[[method]]

  bias <- mean(diff, na.rm = TRUE)
  sd_diff <- sd(diff, na.rm = TRUE)
  loa_hi <- bias + 1.96 * sd_diff
  loa_lo <- bias - 1.96 * sd_diff

  tibble(
    average = avg,
    difference = diff,
    bias = bias,
    loa_hi = loa_hi,
    loa_lo = loa_lo
  )
}

plot_ba <- function(df, reference, method, title, xlab, ylab) {
  ba_df <- prep_ba(df, reference, method)

  ggplot(ba_df, aes(x = average, y = difference)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
    geom_hline(yintercept = ba_df$bias[1], color = "blue", linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = ba_df$loa_hi[1], color = "red", linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = ba_df$loa_lo[1], color = "red", linetype = "dashed", linewidth = 0.8) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 13)
}

# Example plots
plot_ba(
  data,
  reference = "CH_COM_EST",
  method = "CH_R24",
  title = "Bland–Altman: Standardized meal vs 24-hour recall (Carbohydrates)",
  xlab = "Average intake [g]",
  ylab = "Difference (Reference - R24) [g]"
)

plot_ba(
  data,
  reference = "CH_COM_EST",
  method = "CH_METABOLIQUE",
  title = "Bland–Altman: Standardized meal vs METAbolique (Carbohydrates)",
  xlab = "Average intake [g]",
  ylab = "Difference (Reference - App) [g]"
)

# =========================
# 8. Error scatterplots
# =========================
ggplot(data, aes(x = error_CAL_R24, y = error_CAL_App)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.5) +
  geom_point(size = 3, alpha = 0.7, color = "darkolivegreen4") +
  labs(
    title = "Error plot – Calories",
    x = "24-hour recall error (kcal)",
    y = "METAbolique error (kcal)"
  ) +
  theme_minimal(base_size = 13)

# =========================
# 9. Regression model for absolute caloric error
# =========================
model_data <- data %>%
  mutate(
    edad_z = as.numeric(scale(EDAD)),
    imc_z = as.numeric(scale(IMC)),
    cintura_z = as.numeric(scale(CINTURA_CM))
  )

model1 <- lm(error_cal_abs ~ edad_z + sexo + imc_z + cintura_z, data = model_data)

model_results <- tidy(model1, conf.int = TRUE)
print(model_results)

# =========================
# 10. SUS boxplot
# =========================
ggplot(data, aes(x = "", y = SUS)) +
  geom_boxplot(
    fill = "grey80",
    color = "grey20",
    width = 0.4,
    outlier.shape = 21,
    outlier.fill = "grey50"
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "SUS score distribution",
    x = NULL,
    y = "SUS score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
