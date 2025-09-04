# Library
library(readxl)
library(dplyr)

data <- read_excel("Downloads/Datapasi.xlsx", sheet = 'Sheet2')
data$RA <- as.numeric(data$RA)

# Variabel independen
vars <- c("X1", "X2", "X3")
combinations <- unlist(lapply(1:length(vars), function(x) combn(vars, x, simplify = FALSE)), recursive = FALSE)
model_results <- list()

# Loop semua kombinasi
for (i in seq_along(combinations)) {
  formula_text <- paste("Y ~", paste(combinations[[i]], collapse = " + "))
  model <- lm(as.formula(formula_text), data = data)
  summary_model <- summary(model)
  
  # Ambil koefisien lengkap (estimate + p-value)
  coef_table <- coef(summary_model)
  
  # Tambahkan tanda * kalau signifikan
  coef_str <- apply(coef_table, 1, function(row) {
    est <- round(row[1], 3)
    pval <- row[4]
    signif_mark <- ifelse(pval < 0.05, "*", "")
    paste0(names(row)[1], " ", est, signif_mark)
  })
  
  coef_str <- paste(names(coef_table[,1]), round(coef_table[,1],3), 
                    ifelse(coef_table[,4] < 0.05, "*", ""), collapse = "; ")
  
  # Hitung jumlah signifikan (exclude intercept)
  p_values <- coef_table[-1, 4]
  signif_count <- sum(p_values < 0.05)
  
  model_results[[i]] <- list(
    formula = formula_text,
    n_variables = length(combinations[[i]]),
    n_significant = signif_count,
    coefficients = coef_str
  )
}

model_summary <- do.call(rbind, lapply(model_results, as.data.frame))
model_summary <- model_summary %>%
  mutate(ratio_significant = n_significant / n_variables) %>%
  arrange(desc(n_significant), desc(ratio_significant))

print(model_summary)
