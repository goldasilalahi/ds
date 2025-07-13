# LIBRARY
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
library(caret)
library(janitor)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(car)
library(tidyverse)

# UNIT 1 ----
df <- read_excel("~/Desktop/Golda/SOAL ADS/ds 2.xlsx", sheet = "DATABASE")
cat("Jumlah baris:", nrow(df), "\n")
cat("Jumlah kolom:", ncol(df), "\n")
str(df)
colnames(df)
head(df, 5)

# UNIT 2 ----
df_turn <- df$`Turning circle (ft)`
summary(df_turn)
sd(df_turn, na.rm = TRUE)

ggplot(df, aes(x = `Turning circle (ft)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribusi Turning Circle (ft)", x = "Turning circle (ft)", y = "Frekuensi")

ggplot(df, aes(y = `Turning circle (ft)`)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot Turning Circle (ft)", y = "Turning circle (ft)")

# UNIT 3 ----
sapply(df[c("Turning circle (ft)", "Length (in)", "Wheelbase (in)", "Width (in)", "Drive type")],
       function(x) sum(is.na(x)))

df %>%
  dplyr::select(`Turning circle (ft)`, `Length (in)`, `Wheelbase (in)`, `Width (in)`) %>%
  summarise(across(everything(), ~sum(. < 0, na.rm = TRUE)))

turning_clean <- df %>% filter(!is.na(`Turning circle (ft)`))
zscore <- scale(turning_clean$`Turning circle (ft)`)
outlier_rows <- turning_clean[abs(zscore) > 3, ]
cat("Jumlah outlier (Z > 3):", nrow(outlier_rows), "\n")
head(outlier_rows[c("ID", "Make", "Model", "Turning circle (ft)")])

# UNIT 4 ----
df_model <- df %>% filter(!is.na(`Turning circle (ft)`))
df_model <- df_model %>%
  dplyr::select(`Turning circle (ft)`,
                `Length (in)`,
                `Wheelbase (in)`,
                `Width (in)`,
                `Curb weight (lbs)`,
                `Drive type`,
                `Body type`,
                `Engine size (l)`)

# UNIT 5 ----
sapply(df_model, function(x) sum(is.na(x)))
df_clean <- df_model %>% na.omit()
df_clean <- df_clean %>% distinct()
cat("Jumlah baris setelah dibersihkan:", nrow(df_clean), "\n")

# UNIT 6 ----
numeric_cols <- c("Length (in)", "Wheelbase (in)", "Width (in)", "Curb weight (lbs)", "Engine size (l)")
df_norm <- df_clean
df_norm[numeric_cols] <- scale(df_clean[numeric_cols])

set.seed(123)
split <- createDataPartition(df_norm$`Turning circle (ft)`, p = 0.8, list = FALSE)
train_data <- df_norm[split, ]
test_data <- df_norm[-split, ]
cat("Train:", nrow(train_data), " | Test:", nrow(test_data), "\n")

# UNIT 7 ----
train_data$turn_label <- cut(train_data$`Turning circle (ft)`,
                             breaks = c(-Inf, 34, 38, Inf),
                             labels = c("Baik", "Sedang", "Kurang"))
test_data$turn_label <- cut(test_data$`Turning circle (ft)`,
                            breaks = c(-Inf, 34, 38, Inf),
                            labels = c("Baik", "Sedang", "Kurang"))
cat("Proporsi Training:\n"); print(prop.table(table(train_data$turn_label)))
cat("Proporsi Testing:\n"); print(prop.table(table(test_data$turn_label)))

# UNIT 8 ----
# Persiapan data
train_clean <- train_data %>% janitor::clean_names()
test_clean <- test_data %>% janitor::clean_names()

# (1) Linear Regression
model_lm <- lm(turning_circle_ft ~ length_in + wheelbase_in + width_in +
                 curb_weight_lbs + engine_size_l + drive_type + body_type,
               data = train_clean)
summary(model_lm)

# === UJI ASUMSI REGRESI LINEAR ===

# 1. Normalitas Residual
par(mfrow = c(1,2))
plot(model_lm, which = 2)  # QQ Plot
hist(residuals(model_lm), breaks = 30, main = "Histogram Residual", xlab = "Residuals")
shapiro.test(residuals(model_lm))  # Shapiro-Wilk Test

# 2. Homoskedastisitas (Breusch-Pagan Test)
library(lmtest)
bptest(model_lm)

# 3. Multikolinearitas (VIF)
library(car)
vif(model_lm)

# 4. Autokorelasi (Durbin-Watson Test)
dwtest(model_lm)

# 5. Plot diagnostik lengkap
par(mfrow = c(2,2))
plot(model_lm)


# (2) Random Forest Regression
library(randomForest)
model_rf <- randomForest(turning_circle_ft ~ ., data = train_clean, ntree = 100, importance = TRUE)
summary(model_rf)

# Feature Importance
importance_rf <- importance(model_rf)
print(importance_rf)

# === UNIT 9 ===
# Prediksi
pred_lm <- predict(model_lm, newdata = test_clean)
pred_rf <- predict(model_rf, newdata = test_clean)

# Evaluasi LM
mae_lm <- mean(abs(pred_lm - test_clean$turning_circle_ft))
rmse_lm <- sqrt(mean((pred_lm - test_clean$turning_circle_ft)^2))
r2_lm <- cor(pred_lm, test_clean$turning_circle_ft)^2

# Evaluasi RF
mae_rf <- mean(abs(pred_rf - test_clean$turning_circle_ft))
rmse_rf <- sqrt(mean((pred_rf - test_clean$turning_circle_ft)^2))
r2_rf <- cor(pred_rf, test_clean$turning_circle_ft)^2

cat("=== EVALUASI MODEL ===\n")
cat("Linear Regression:\n")
cat("MAE:", round(mae_lm, 3), "| RMSE:", round(rmse_lm, 3), "| R²:", round(r2_lm, 3), "\n\n")

cat("Random Forest:\n")
cat("MAE:", round(mae_rf, 3), "| RMSE:", round(rmse_rf, 3), "| R²:", round(r2_rf, 3), "\n\n")

# Residual plot RF
resid_rf <- test_clean$turning_circle_ft - pred_rf
plot(pred_rf, resid_rf, main = "Residual Plot - Random Forest", xlab = "Predicted", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Kesimpulan
if (r2_rf > r2_lm) {
  cat("Model terbaik: Random Forest\n")
} else {
  cat("Model terbaik: Linear Regression\n")
}
