# Linear models

library(readr)
library(dplyr)

person <- read_csv(
  file = 'data/census_pums/sample.csv',
  col_types = cols_only(
    AGEP = 'i',  # Age
    WAGP = 'd',  # Wages or salary income past 12 months
    SCHL = 'i',  # Educational attainment
    SEX = 'f',   # Sex
    OCCP = 'f',  # Occupation recode based on 2010 OCC codes
    WKHP = 'i')) # Usual hours worked per week past 12 months

person <- within(person, {
  SCHL <- factor(SCHL)
  levels(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16,
    'College Credit' = 17:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  filter(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Formula Notation

fit <- lm(
  formula = WAGP ~ SCHL,
  data = person)

fit <- lm(
  log(WAGP) ~ SCHL,
  person)

# Metadata matters

fit <- lm(
  log(WAGP) ~ AGEP,
  person)

# GLM families

fit <- glm(log(WAGP) ~ SCHL,
           family = gaussian,
           data = person)

# Logistic Regression

fit <- glm(SEX ~ WAGP,
  family = binomial,
  data = person)

anova(fit, update(fit, SEX ~ 1), test = 'Chisq')

# Random Intercept

library(lme4)
fit <- lmer(
  log(WAGP) ~ (1| OCCP) + SCHL,
  data = person)

# Random Slope

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person)

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person,
  control = lmerControl(optimizer="bobyqa"))

library(ggplot2)

ggplot(person,
  aes(x = WKHP, y = log(WAGP), color = SCHL)) +
  geom_point() +
  geom_line(aes(y=predict(fit))) +
  labs(title = 'Random intercept and slope with lmer')

##HOMEWORK EXERCISE 1
fit <- lm(
  WKHP ~ AGEP + I(AGEP^2),
  person)

ggplot(person,
       aes(x = AGEP, y = WKHP)) +
  geom_point(shape = 'o') +
  geom_line(aes(y = predict(fit)))

##HOMEWORK EXERCISE 2
fit <- lm(
  WAGP ~ SCHL, 
  person)

##TRYING/STRUGGLING TO REMEMBER STATS...
