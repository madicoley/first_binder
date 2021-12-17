library(tidyverse)
library(Hmisc)
library(performance)
library(MASS)
library(car)
library(olsrr)
library(patchwork)

coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee_ratings_filtered <- filter(coffee_ratings, lot_number != 103)

plot_aroma <- ggplot(coffee_ratings_filtered, aes(x = aroma, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_aftertaste <- ggplot(coffee_ratings_filtered, aes(x = aftertaste, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_acidity <- ggplot(coffee_ratings_filtered, aes(x = acidity, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_body <- ggplot(coffee_ratings_filtered, aes(x = body, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_balance <- ggplot(coffee_ratings_filtered, aes(x = balance, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_sweetness <- ggplot(coffee_ratings_filtered, aes(x = sweetness, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_uniformity <- ggplot(coffee_ratings_filtered, aes(x = uniformity, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_clean_cup <- ggplot(coffee_ratings_filtered, aes(x = clean_cup, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

plot_moisture <- ggplot(coffee_ratings_filtered, aes(x = moisture, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

correlation_plots <- (plot_aroma + plot_aftertaste + plot_acidity + plot_balance) / (plot_body + plot_clean_cup + plot_moisture + plot_uniformity)

model0 <- lm(flavor ~ 1, data = coffee_ratings_filtered)
model1 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + clean_cup + uniformity + moisture, data = coffee_ratings_filtered)
model2 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + clean_cup + uniformity, data = coffee_ratings_filtered)

check_model(model1)
check_model(model2)
anova(model0, model2)

summary(model2)

steplimitsboth <- step(model0, scope = list (upper = model2), direction ="both")

summary(steplimitsboth)

model3 <- lm(flavor ~ aftertaste + acidity + aroma + uniformity + body + balance, data = coffee_ratings_filtered)

summary(model3)

flavor.score <- data.frame(aftertaste = 5, acidity = 7, aroma = 8, uniformity = 6, body = 5, balance = 9)
predict(steplimitsboth, newdata=flavor.score)

model4 <- lm(flavor ~ aroma + acidity + body + balance + uniformity + clean_cup + aftertaste, data = coffee_ratings_filtered)

flavor.score <- data.frame(aroma = 8, acidity = 7, body = 5, balance = 9, uniformity = 6, clean_cup = 9, aftertaste=5)
predict(model4, newdata = flavor.score)
