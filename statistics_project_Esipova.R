library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(lme4)
library(performance)
library(effsize) 
library(ordinal)
library(emmeans)


df <- read.csv("C:/Users/79998/Desktop/statistics_project/Composition+and+projection+of+co-speech+gestures_+tidy+data.csv")


# ---- Check on missing data ----

total_missing <- sum(is.na(df))
total_missing


# ---- Data description ----

glimpse(df)


# Base descriptivve statistics (Min, Max, Mean, Median)
summary(df$Rating)

# Pivot table
descriptives <- df %>%
  group_by(Content, Context) %>%
  summarise(
    N = n(),
    Mean_Rating = mean(Rating, na.rm = TRUE),
    SD_Rating = sd(Rating, na.rm = TRUE),
    Median_Rating = median(Rating, na.rm = TRUE)
  )
print(descriptives)


# Descriptive statistics on Content type
descriptives_content_only <- df %>%
  group_by(Content) %>%
  summarise(
    N = n(),
    Mean_Rating = mean(Rating, na.rm = TRUE),
    SD_Rating = sd(Rating, na.rm = TRUE),
    Median_Rating = median(Rating, na.rm = TRUE)
  )
print(descriptives_content_only)


# Outliers
outliers <- df %>%
  group_by(Content, Context) %>%
  mutate(
    Q1 = quantile(Rating, 0.25, na.rm = TRUE),
    Q3 = quantile(Rating, 0.75, na.rm = TRUE),
    IQR_value = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR_value,
    Upper_Bound = Q3 + 1.5 * IQR_value,
    Is_Outlier = Rating < Lower_Bound | Rating > Upper_Bound
  ) %>%
  filter(Is_Outlier == TRUE) %>%
  select(Participant, Item, Content, Context, Rating)

print(nrow(outliers))


# ---- Visualisations ----

# histogram
plot_hist <- ggplot(df, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of Acceptability Ratings", x = "Rating (0-100)", y = "Density")
print(plot_hist)


# Boxplot
plot_box <- ggplot(df, aes(x = Content, y = Rating, fill = Content)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
  geom_jitter(color = "black", size = 1, alpha = 0.3, width = 0.2) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + # Приятная цветовая палитра
  labs(title = "Acceptability Ratings by Content Type", 
       subtitle = "Dots represent individual participant ratings",
       x = "Content Type", 
       y = "Rating (0-100)") +
  theme(legend.position = "none")

print(plot_box)

# 100-points gradient scale plot

# Remove Mismatch
df_clean <- df %>% filter(Content != "Mismatch")

# Medians for labels
medians <- df_clean %>%
  group_by(Content, Context) %>%
  summarise(Median_Val = round(median(Rating, na.rm = TRUE), 1), .groups = 'drop')

plot_interaction_bars_100 <- ggplot(df_clean, aes(x = Context, fill = Rating, group = Rating)) +
  geom_bar(position = "fill", width = 0.8, color = NA) + 
  facet_grid(~ Content) +
  scale_fill_gradient(low = "#f0f0f0", high = "#000000") +
  theme_bw() +
  labs(title = "Interaction of Context and Content on Ratings",
       subtitle = "Numbers represent the median rating",
       x = "Interpretation Context",
       y = "Proportion of responses",
       fill = "Rating\n(0-100)") +
  geom_text(data = medians, aes(x = Context, y = 0.5, label = Median_Val), 
            inherit.aes = FALSE, fontface = "bold", color = "red") +
  theme(strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(face = "bold", size = 12))

print(plot_interaction_bars_100)

# Heatmap

# Data preparation
heatmap_data <- df %>%
  filter(Content != "Mismatch") %>%
  group_by(Content, Context) %>%
  summarise(Mean_Rating = mean(Rating, na.rm = TRUE), .groups = 'drop')

plot_heatmap <- ggplot(heatmap_data, aes(x = Context, y = Content, fill = Mean_Rating)) +
  # Создаем сами плитки
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(Mean_Rating, 1)), 
            color = ifelse(heatmap_data$Mean_Rating > 60, "white", "black"), 
            fontface = "bold", size = 5) +
  scale_fill_gradient(low = "#e0f3f8", high = "#d73027", limits = c(0, 100)) +
  theme_minimal() +
  labs(title = "Heatmap: Interaction of Content and Context",
       subtitle = "Colors and numbers represent mean acceptability ratings",
       x = "Interpretation Context",
       y = "Content Type",
       fill = "Mean\nRating") +
  theme(panel.grid = element_blank(), # Убираем лишнюю сетку
        axis.text = element_text(size = 12))

print(plot_heatmap)

# ---- Checking assumptions ----

# Normality
qqnorm(df$Rating, main = "Q-Q Plot of Ratings")
qqline(df$Rating, col = "red", lwd = 2)

# shapiro
shapiro.test(df$Rating)

# Homogeneity of Variance
leveneTest(Rating ~ Content * Context, data = df)


# Unusual Patterns
ceiling_floor <- df %>%
  summarise(
    Total_Ratings = n(),
    Absolute_Zeroes = sum(Rating == 0, na.rm = TRUE),
    Absolute_Hundreds = sum(Rating == 100, na.rm = TRUE)
  ) %>%
  mutate(
    Percent_Zeroes = round((Absolute_Zeroes / Total_Ratings) * 100, 1),
    Percent_Hundreds = round((Absolute_Hundreds / Total_Ratings) * 100, 1)
  )

print(ceiling_floor)


# ---- Data Quality ----

# Suspicious participants based on high ratings of Mismatch
suspicious_mismatch <- df %>%
  filter(Content == "Mismatch" & Rating > 80) %>%
  select(Participant, Rating)

print(nrow(suspicious_mismatch))

# Straightlining (SD=0)
lazy_participants <- df %>%
  group_by(Participant) %>%
  summarise(Rating_SD = sd(Rating, na.rm = TRUE)) %>%
  filter(Rating_SD == 0)

print(nrow(lazy_participants))


# ---- Simple Hypothesis Tests ----

# Prepare data for Gestures
df_g <- subset(df, Content == "G")
df_g$Context <- droplevels(factor(df_g$Context))

# Test 1: Kruskal-Wallis (Omnibus test for 3 groups)
test_kw <- kruskal.test(Rating ~ Context, data = df_g)
print(test_kw)

# Effect size for Kruskal-Wallis (Epsilon-squared)
H_stat <- test_kw$statistic
n_g <- nrow(df_g)
eps2 <- unname(H_stat / (n_g - 1))
print(paste("Epsilon-squared:", round(eps2, 3)))

# Test 2: Mann-Whitney U test (Directional comparison: PNR vs R)
df_pnr_r <- subset(df_g, Context %in% c("PNR", "R"))
df_pnr_r$Context <- factor(df_pnr_r$Context, levels = c("PNR", "R"))

test_mw <- wilcox.test(Rating ~ Context, data = df_pnr_r, alternative = "greater")
print(test_mw)

eff_mw <- cliff.delta(Rating ~ Context, data = df_pnr_r)
print(eff_mw)

# ---- Dream model THAT WAS TOO COMPLEX ----
 df_reg <- subset(df, Content != "Mismatch")
 df_reg$Content <- droplevels(factor(df_reg$Content))
# model_clmm <- clmm(factor(Rating) ~ Content + Context + Content * Context 
#                   + (1|Participant) 
#                   + (1|Scenario), data = df_reg)

# ---- Model Binned ----
df_reg$Rating_cat <- cut(df_reg$Rating, breaks = 10, ordered_result = TRUE)

model_clmm_binned <- clmm(Rating_cat ~ Content * Context + (1|Participant) + (1|Scenario), data = df_reg)


# ---- Assumption Checks ----

# 1. Proportional Odds Assumption
# We check this on a clm model without random effects
model_clm <- clm(factor(Rating_cat) ~ Content * Context, data = df_reg)
nominal_test(model_clm)


# 2. Multicollinearity (VIF)
# We fit a standard linear model to check VIF for fixed effects
model_vif <- lm(Rating_cat ~ Content * Context, data = df_reg)
vif(model_vif)


# 3. Normal Distribution of Random Effects
# Extract random intercepts
rand_int <- ranef(model_clmm_binned)

# Q-Q plot for Participant random effects
qqnorm(rand_int$Participant$`(Intercept)`, main = "Normal Q-Q Plot: Participants")
qqline(rand_int$Participant$`(Intercept)`)

# Q-Q plot for Scenario random effects
qqnorm(rand_int$Scenario$`(Intercept)`, main = "Normal Q-Q Plot: Scenarios")
qqline(rand_int$Scenario$`(Intercept)`)

# ---- Model Interpretation ----

# Full summary with coefficients, standard errors, and p-values
summary(model_clmm_binned)

# Calculate 95% Confidence Intervals for the coefficients
confint(model_clmm_binned)


# Measure of Model Fit

AIC(model_clmm_binned)
BIC(model_clmm_binned)

r2(model_clmm_binned)

# Pairwise Comparisons (Post-Hoc Tests)

# 1. Compare Contexts INSIDE each Content Type 
# (This checks Esipova's PNR > R > NPNR specifically for Gestures, Adjectives, etc.)
emm_context <- emmeans(model_clmm_binned, ~ Context | Content)
pairs(emm_context, adjust = "holm")

# 2. Compare Content Types INSIDE each Context 
# (This checks if Gestures act differently from Adjectives in a specific context)
emm_content <- emmeans(model_clmm_binned, ~ Content | Context)
pairs(emm_content, adjust = "holm")

# 3. Overall comparison
emm_content <- emmeans(model_clmm_binned, ~ Content * Context)
print(pairs(emm_content, adjust = "holm"))

