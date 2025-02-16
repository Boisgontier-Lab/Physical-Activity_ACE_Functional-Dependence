# Load libraries
library(haven)
library(here)
library(dplyr)
library(effects)
library(TOSTER)
library(ggplot2)
library(broom)
library(viridis)
library(patchwork)

# Load data
data_iat <- read_sav(here("Disability_IAT.public.2022-2024.sav"))
country_continent <- read.csv("country-continent.csv", fileEncoding = "UTF-8")

# Data exploration
colnames(data_iat)
nrow(data_iat)

## Create the continent variable
data_iat$continent <- NA_character_ #Create the continent variable as an empty character column
data_iat <- merge(data_iat, country_continent, by.x = "countryres_num", 
                  by.y = "countryres_num", all.x = TRUE) # Merge continent data using base R
colnames(data_iat)[which(names(data_iat) == "continent.y")] <- "continent" # Rename merged column if needed
data_iat$continent[data_iat$countryres_num %in% c(NA, "") | data_iat$continent == ""] <- NA_character_ # Ensure continent is NA when countryres_num is NA or continent is empty

# Create age variable
data_iat$age <- data_iat$year - data_iat$birthyear

# Data filtering
data_iat <- data_iat %>%
  filter(pct_300 < 10, age >= 20, age <= 70)

# Recode occuSelfDetail
data_iat$occuSelfDetail <- ifelse(data_iat$occuSelfDetail == "29-1000", "clin",
                                  ifelse(data_iat$occuSelfDetail == "31-2000", "rehab",
                                         ifelse(data_iat$occuSelfDetail == "", NA_character_, 
                                                "other")))

# Create the new variable occuSelfDetail_clinrehab merging clin and rehab values in clinrehab
data_iat$occuSelfDetail_clinrehab <- ifelse(data_iat$occuSelfDetail %in% c("clin", "rehab"), "clin_rehab", data_iat$occuSelfDetail)

# Group disabled variables
data_iat$disabled_newvar <- ifelse(
  data_iat$disabled_001 == 1 | data_iat$disabledfamilymem_001 == 1, 1L,
  ifelse(
    data_iat$disabled_001 == 2 & data_iat$disabledfamilymem_001 == 2, 2L,
    NA_integer_
  )
)

# map edu_14 to new 3-level classification edu_3
data_iat <- data_iat %>%
  mutate(edu_3 = case_when(
    edu_14 %in% 1:4  ~ "Primary & Secondary", 
    edu_14 %in% 5:7  ~ "College / Undergraduate", 
    edu_14 %in% 8:14 ~ "Graduate & Postgraduate", 
    TRUE ~ NA_character_
  ))

# Convert relevant columns to factors
data_iat$occuSelfDetail <- as.factor(data_iat$occuSelfDetail)
data_iat$occuSelfDetail_clinrehab <- as.factor(data_iat$occuSelfDetail_clinrehab)
data_iat$genderIdentity <- as.factor(data_iat$genderIdentity)
data_iat$birthSex <- factor(ifelse(data_iat$birthSex == 2, 0, 1), levels = c(0, 1))  # 0 = Female, 1 = Male
data_iat$disabled_newvar  <- as.factor(data_iat$disabled_newvar) # 1 = yes; 2 = no
data_iat$countryres_num_f <- as.factor(data_iat$countryres_num)
data_iat$raceomb_002 <- as.factor(data_iat$raceomb_002)
data_iat$continent <- as.factor(data_iat$continent)
data_iat$edu_3 <- as.factor(data_iat$edu_3)

# Convert continuous variables to numeric
data_iat$age <- as.numeric(data_iat$age)
data_iat$att_7 <- as.numeric(data_iat$att_7) # 1 = strongly prefer disabled; 7 = strongly prefer abled

# Remove rows with NA or empty strings
data_iat_clean <- data_iat %>%
  filter(
    complete.cases(D_biep.PhysAbled_Good_all, occuSelfDetail, occuSelfDetail_clinrehab, birthSex, age, att_7, disabled_newvar, continent, raceomb_002, edu_3),
    occuSelfDetail != "", 
    birthSex != "", 
    disabled_newvar != "", 
    continent != "", 
    raceomb_002 != ""
  )

# Standardization of continuous variables
data_iat_clean$age_c <- scale (data_iat_clean$age, center = TRUE, scale = TRUE)
data_iat_clean$att_7_c <- scale (data_iat_clean$att_7, center = TRUE, scale = TRUE)
data_iat_clean$D_biep.PhysAbled_Good_all_c <- scale (data_iat_clean$D_biep.PhysAbled_Good_all, center = TRUE, scale = TRUE)

# Relevel factors
data_iat_clean$occuSelfDetail <- relevel(data_iat_clean$occuSelfDetail, ref = "other")
data_iat_clean$occuSelfDetail_clinrehab <- relevel(data_iat_clean$occuSelfDetail_clinrehab, ref = "other")
data_iat_clean$raceomb_002 <- relevel(data_iat_clean$raceomb_002, ref = "6") #white
data_iat_clean$continent <- relevel(data_iat_clean$continent, ref = "North America")
data_iat_clean$edu_3 <- relevel(data_iat_clean$"edu_3", ref = "Primary & Secondary")

## Descriptive results
# Age and explicit attitudes as a function of occupation category
data_iat_clean %>%
  group_by(occuSelfDetail) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    
    mean_att_7 = mean(att_7, na.rm = TRUE),
    sd_att_7 = sd(att_7, na.rm = TRUE),
    min_att_7 = min(att_7, na.rm = TRUE),
    max_att_7 = max(att_7, na.rm = TRUE),
    median_att_7 = median(att_7, na.rm = TRUE),
    
    mean_D_biep.PhysAbled_Good_all = mean(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    sd_D_biep.PhysAbled_Good_all = sd(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    min_D_biep.PhysAbled_Good_all = min(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    max_D_biep.PhysAbled_Good_all = max(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    median_D_biep.PhysAbled_Good_all = median(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    
n = n()  # Number of participants per occupation category
  ) %>%
  arrange(mean_age) %>%
  print(width = Inf)  # Forces full column display

# number of observations per value of the variables sex, disabled, geographic region, and race
# Function to count and calculate percentages
count_percentage <- function(data, group_var) {
  data %>%
    group_by(occuSelfDetail, {{ group_var }}) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(occuSelfDetail) %>%
    mutate(perc = round(100 * n / sum(n), 2)) %>%
    arrange(occuSelfDetail, desc(n))  # Optional: Order by occupation and count
}

# Count and percentage for each variable
birthSex_counts <- count_percentage(data_iat_clean, birthSex)
disabled_counts <- count_percentage(data_iat_clean, disabled_newvar)
continent_counts <- count_percentage(data_iat_clean, continent)
race_counts <- count_percentage(data_iat_clean, raceomb_002)
edu_3_counts <- count_percentage(data_iat_clean, edu_3)

# Display results
list(
  birthSex = birthSex_counts,
  disabled_newvar = disabled_counts,
  continent = continent_counts,
  raceomb_002 = race_counts,
  edu_3 = edu_3_counts
)


### STATISTICAL MODELS ###
## Implicit attitudes
# Implicit: main model
lm1.implicit <- lm(D_biep.PhysAbled_Good_all ~ occuSelfDetail + birthSex + age_c + att_7_c + disabled_newvar + continent + raceomb_002 + edu_3, 
          data = data_iat_clean, na.action=na.omit)
summary(lm1.implicit )
confint(lm1.implicit )

# For Figures and interpretation of continuous variables
lm1.implicit_fig <- lm(D_biep.PhysAbled_Good_all ~ occuSelfDetail + birthSex + age + att_7 + disabled_newvar + continent + raceomb_002 + edu_3, 
                       data = data_iat_clean, na.action=na.omit)
summary(lm1.implicit_fig)

# Equivalence testing for lm1.implicit: Clinicians vs. other
TOSTtwo(m1 = 0.003365,               # Estimate for occuSelfDetailclin
        m2 = 0,                       # Reference group (Effect is 0 for reference)
        sd1 = 0.007936,               # Standard Error for occuSelfDetailclin
        sd2 = 0.4277,                 # Residual Standard Error for occuSelfDetailother_not-health
        n1 = 3109,                    # Sample size for occuSelfDetailclin
        n2 = 92350,                   # Sample size for occuSelfDetailother_not-health
        low_eqbound = -0.0642,         # Lower equivalence bound
        high_eqbound = 0.0642,         # Upper equivalence bound
        alpha = 0.05)

# Equivalence testing for lm1.implicit: Rehab assistants vs. other
TOSTtwo(m1 = -0.00003404,            # Estimate for occuSelfDetailrehab
        m2 = 0,                       # Reference group (Effect is 0 for reference)
        sd1 = 0.01071,                # Standard Error for occuSelfDetailrehab
        sd2 = 0.4277,                 # Residual Standard Error for occuSelfDetailother_not-health
        n1 = 1636,                    # Sample size for occuSelfDetailrehab
        n2 = 92350,                   # Sample size for occuSelfDetailother_not-health
        low_eqbound = -0.0642,         # Lower equivalence bound
        high_eqbound = 0.0642,         # Upper equivalence bound
        alpha = 0.05)

# Subset clincians
lm1_implicit.clin <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age_c + att_7_c + disabled_newvar + continent + raceomb_002 + edu_3, 
                        data = data_iat_clean, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_implicit.clin)
confint(lm1_implicit.clin)

# For Figures
lm1_implicit.clin_fig <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age + att_7 + disabled_newvar + continent + raceomb_002 + edu_3, 
                        data = data_iat_clean, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_implicit.clin_fig)

# Subset rehab
lm1_implicit.rehab <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age_c + att_7_c + disabled_newvar + continent + raceomb_002 + edu_3, 
                         data = data_iat_clean, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_implicit.rehab)
confint(lm1_implicit.rehab)

# For Figures
lm1_implicit.rehab_fig <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age + att_7 + disabled_newvar + continent + raceomb_002 + edu_3, 
                         data = data_iat_clean, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_implicit.rehab)

## Explicit attitudes
# Explicit: main model
lm1.explicit <- lm(att_7 ~ occuSelfDetail + birthSex + age_c + D_biep.PhysAbled_Good_all_c + disabled_newvar + continent + raceomb_002 + edu_3, 
                   data = data_iat_clean, na.action=na.omit)
summary(lm1.explicit)
confint(lm1.explicit)

# For Figures
lm1.explicit_fig <- lm(att_7 ~ occuSelfDetail + birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + continent + raceomb_002 + edu_3, 
                       data = data_iat_clean, na.action=na.omit)
summary(lm1.explicit_fig)

## subset clin
lm1_explicit.clin <- lm(att_7 ~ birthSex + age_c + D_biep.PhysAbled_Good_all_c + disabled_newvar + continent + raceomb_002 + edu_3, 
                        data = data_iat_clean, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_explicit.clin)
confint(lm1_explicit.clin)

# For Figures
lm1_explicit.clin_fig <- lm(att_7 ~ birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + continent + raceomb_002 + edu_3, 
                        data = data_iat_clean, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_explicit.clin_fig)

## Subset rehab
lm1_explicit.rehab <- lm(att_7 ~ birthSex + age_c + D_biep.PhysAbled_Good_all_c + disabled_newvar + continent + raceomb_002 + edu_3, 
                         data = data_iat_clean, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_explicit.rehab)
confint(lm1_explicit.rehab)

# For Figures
lm1_explicit.rehab_fig <- lm(att_7 ~ birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + continent + raceomb_002 + edu_3, 
                         data = data_iat_clean, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_explicit.rehab_fig)

### FIGURES ###
## Figure 1A
# Compute effects
effects_list <- allEffects(lm1.implicit_fig)

# Convert to a data frame for ggplot
effects_df <- as.data.frame(effects_list$occuSelfDetail)  # Change variable name as needed

# Create plot
ggplot(effects_df, aes(x = occuSelfDetail, y = fit)) +
  geom_point(size = 3, color = "blue") +  # Effect estimates
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Confidence intervals
  labs(title = "Effect of Occupation on D_biep.PhysAbled_Good_all",
       x = "Occupation",
       y = "Estimated Effect",
       caption = "Based on lm1 model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## Figure 1B
# Compute effects
effects_list <- allEffects(lm1.explicit_fig)

# Convert to a data frame for ggplot
effects_df <- as.data.frame(effects_list$occuSelfDetail)  # Change variable name as needed

# Create the plot
ggplot(effects_df, aes(x = occuSelfDetail, y = fit)) +
  geom_point(size = 3, color = "red") +  # Effect estimates
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Confidence intervals
  labs(title = "Effect of Occupation on D_biep.PhysAbled_Good_all",
       x = "Occupation",
       y = "Estimated Effect",
       caption = "Based on lm1 model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## Figure 2A
# Extract coefficients with confidence intervals
coefs <- tidy(lm1.implicit_fig, conf.int = TRUE)  
coefs <- coefs[coefs$term != "(Intercept)", ]  # Exclude intercept  

# Reorder terms by estimate value (smallest to largest)
coefs$term <- reorder(coefs$term, coefs$estimate)

# Plot coefficients
ggplot(coefs, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Regression Coefficients for lm1 (Ordered)",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Error bars show 95% confidence intervals") +
  coord_flip() +  # Flip to horizontal for better readability
  theme_minimal()

## Figure 2B
# Extract coefficients with confidence intervals
coefs <- tidy(lm1.explicit_fig, conf.int = TRUE)  
coefs <- coefs[coefs$term != "(Intercept)", ]  # Exclude intercept  

# Reorder terms by estimate value (smallest to largest)
coefs$term <- reorder(coefs$term, coefs$estimate)

# Plot coefficients
ggplot(coefs, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Regression Coefficients for lm1 (Ordered)",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Error bars show 95% confidence intervals") +
  coord_flip() +  # Flip to horizontal for better readability
  theme_minimal()

## Figure 3A: implicit & clinicians
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_implicit.clin_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_implicit.clin",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3B: explicit & clinicians
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_explicit.clin_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_explicit.clin",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3C: implicit & rehabilitation assistants
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_implicit.rehab_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_implicit.rehab",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3D: explicit & rehabilitation assistants
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_explicit.rehab_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_explicit.rehab",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 4
# Extract the specific predictor effects as data frames
eff_clin_age <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 2)$age)  
eff_rehab_age <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 2)$age)  
eff_clin_likert <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 3)$att_7)  
eff_rehab_likert <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 3)$att_7)  

# Print structure to verify correct extraction
str(eff_clin_age)

# Define a function to plot effects
plot_effect <- function(data, title, color) {
  ggplot(data, aes(x = data[[1]], y = fit)) +  # First column = predictor
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = color, alpha = 0.3) +
    geom_line(color = color, size = 1.2) +
    labs(title = title, x = names(data)[1], y = "Effect Size") +
    theme_minimal()
}

# Generate plots
p1 <- plot_effect(eff_clin_age, "Implicit (Clinicians) - Age", viridis(4)[1])
p2 <- plot_effect(eff_rehab_age, "Implicit (Rehab) - Age", viridis(4)[2])
p3 <- plot_effect(eff_clin_likert, "Implicit (Clinicians) - Likert", viridis(4)[3])
p4 <- plot_effect(eff_rehab_likert, "Implicit (Rehab) - Likert", viridis(4)[4])

# Extract the specific predictor effects as data frames
eff_clin_age <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 2)$age)  
eff_rehab_age <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 2)$age)  
eff_clin_likert <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 3)$att_7)  
eff_rehab_likert <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 3)$att_7)  

# Add a column to differentiate the models
eff_clin_age$model <- "Clinicians"
eff_rehab_age$model <- "Rehab"
eff_clin_likert$model <- "Clinicians"
eff_rehab_likert$model <- "Rehab"

# Combine datasets
eff_age <- rbind(eff_clin_age, eff_rehab_age)
eff_likert <- rbind(eff_clin_likert, eff_rehab_likert)

# Extract different viridis colors for each set
scales::show_col(viridis(10, option = "viridis"))
custom_colors_p1_p2 <- c("#440154FF", "#9C179EFF")
custom_colors_p3_p4 <- c("#26828EFF", "#6DCD59FF")  

# Compute the actual min/max from CI to ensure full coverage
ymin_p1_p2 <- min(eff_age$lower, 0.4)  
ymax_p1_p2 <- max(eff_age$upper, 0.9)  
ymin_p3_p4 <- min(eff_likert$lower, 0.2)  
ymax_p3_p4 <- max(eff_likert$upper, 0.7)  

# Superimposed plot for Age effect
p1_p2 <- ggplot(eff_age, aes(x = age, y = fit, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  
  geom_line(size = 1.2) +  
  scale_color_manual(values = custom_colors_p1_p2, name = "Model") +  
  scale_fill_manual(values = custom_colors_p1_p2, name = "Model") +  
  scale_y_continuous(
    breaks = seq(0.4, 0.9, by = 0.2),  # Set fixed breaks
    limits = c(ymin_p1_p2, ymax_p1_p2),  # Enforce range but allow expansion for CI
    expand = c(0, 0)
  ) +
  labs(title = "Implicit Attitudes - Age (Clinicians vs. Rehab)", x = "Age", y = "Effect Size") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Superimposed plot for Likert effect
p3_p4 <- ggplot(eff_likert, aes(x = att_7, y = fit, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  
  geom_line(size = 1.2) +  
  scale_color_manual(values = custom_colors_p3_p4, name = "Model") +  
  scale_fill_manual(values = custom_colors_p3_p4, name = "Model") +  
  scale_y_continuous(
    breaks = seq(0.2, 0.7, by = 0.2),
    limits = c(ymin_p3_p4, ymax_p3_p4),
    expand = c(0, 0)
  ) +
  labs(title = "Implicit Attitudes - Likert (Clinicians vs. Rehab)", x = "Attitude", y = "Effect Size") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Arrange plots
final_plot <- p1_p2 / p3_p4
print(final_plot)

