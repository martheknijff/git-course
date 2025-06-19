### This is a demo file with R code ###

# Load packages
library(tidyverse)
library(palmerpenguins)
library(wesanderson)


# Read data
df_penguins <- penguins 


# Check out data
summary(df_penguins)
str(df_penguins)
glimpse(df_penguins) #view the first few rows


# Clean data
df_penguins_clean <- df_penguins %>% drop_na()


# Calculate summary statistics by species
summary_statistics <- df_penguins_clean %>%
  group_by(species) %>%
  summarize(
    count = n(),
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    mean_body_mass = mean(body_mass_g, na.rm = TRUE)
  )
print(summary_statistics)


# Create scatter plot of bill length vs. flipper length
wes_palette("Zissou1")
zissou_colors <- wes_palette("Zissou1", n = 5)
colors <- c(zissou_colors[1], zissou_colors[4], zissou_colors[5])

plot_bill_flipper <- df_penguins_clean %>%
  ggplot(aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = colors, name = "Penguin species") +
  labs(x = "Bill length (mm)",
       y = "Flipper length (mm)") +
  theme_light()

plot(plot_bill_flipper)


# Create boxplot of body mass by species and sex
colors2 <- c(zissou_colors[1], zissou_colors[5])

plot_mass_species_sex <- df_penguins_clean %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = colors2, name = "Sex") +
  labs(x = "",
       y = "Body mass (g)") +
  theme_light()

plot(plot_mass_species_sex)


# Predict body mass based on flipper length and species:
model <- lm(body_mass_g ~ flipper_length_mm + species, data = df_penguins_clean)
summary(model)
