# Пакети ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mice)
library(psych)
library(corrplot)
library(factoextra)
library(ggplot2)

# 1. Читання CSV -------------------------------------------------------
df_raw <- read.csv("Raw_data.csv", sep = ";", dec = ",")

# 2. Робимо синтаксичні імена -----------------------------------------
names(df_raw) <- make.names(names(df_raw))

# 3. Визначаємо індикатори (прості назви під твої CSV) ----------------
vars <- c(
  "Ease.of.doing.business.score",
  "Procedures.Men",
  "Cost.Men",
  "Dealing.with.construction.permits",
  "Cost",
  "Getting.electricity"
)

# 4. Перевіримо числові типи ------------------------------------------
df_raw <- df_raw %>%
  mutate(across(all_of(vars), as.numeric))

# 5. Імпутація пропусків (mice) ----------------------------------------
df_mice <- df_raw %>%
  mutate(
    Country = as.factor(Country.name),
    Year = as.factor(Year)
  ) %>%
  select(Country, Year, all_of(vars))

df_imp <- df_raw %>%
  mutate(across(all_of(vars), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# 6. Нормалізація -------------------------------------------------------
minmax <- function(x) (x - min(x)) / (max(x) - min(x))

df_norm <- df_imp %>%
  mutate(across(all_of(vars), minmax))

# 7. PCA та ваги --------------------------------------------------------
pca_res <- prcomp(df_norm %>% select(all_of(vars)), scale. = TRUE)

loadings <- abs(pca_res$rotation[,1])
weights <- loadings / sum(loadings)
weights

# 8. Композитний індекс -------------------------------------------------
Composite_Index <- as.numeric(as.matrix(df_norm[, vars]) %*% weights)

df_results <- df_norm %>%
  mutate(Composite_Index = Composite_Index) %>%
  mutate(
    Composite_Index_Scaled =
      100 * (Composite_Index - min(Composite_Index)) /
      (max(Composite_Index) - min(Composite_Index))
  )

# Повертаємо Country та Year
df_results$Country <- df_imp$Country
df_results$Year <- df_imp$Year

# 9. Середній індекс по країнах (топ-10) ----------------------------------------
df_country_mean <- df_results %>%
  group_by(Country.name) %>%  # змінено на правильну колонку
  summarise(Mean_Index = mean(Composite_Index_Scaled, na.rm = TRUE)) %>%
  arrange(desc(Mean_Index)) %>%
  slice_head(n = 20)  # вибираємо топ-20

print(df_country_mean)

# 10. Графік топ-10 країн ----------------------------------------------------
ggplot(df_country_mean, aes(x = reorder(Country.name, Mean_Index), y = Mean_Index)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  # розмір тексту назв країн
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Топ-20 країн за композитним індексом (0–100)",
    y = "Індекс",
    x = ""
  )

# 11. Кореляційні матриці ----------------------------------------------------
vars_numeric <- df_norm %>% select(all_of(vars))  # вибираємо тільки числові змінні

cor_matrix <- cor(vars_numeric, use = "pairwise.complete.obs")  # кореляційна матриця
print(cor_matrix)

# Візуалізація кореляційної матриці
corrplot(cor_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# 12. Збереження ---------------------------------------------------------
write.csv(df_results, "Composite_Index_results.csv", row.names = FALSE)
write.csv(df_country_mean, "Composite_Index_country_mean.csv", row.names = FALSE)