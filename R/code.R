### This is a demo file with R code ###

#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(wesanderson)
library(gtsummary)
library(officer)
library(flextable)


#-------------------------------------------------------------------------------
# Load paths
#-------------------------------------------------------------------------------
source("R/paths.R")


#-------------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------------
df_penguins <- penguins

#-------------------------------------------------------------------------------
# Check out data
#-------------------------------------------------------------------------------
summary(df_penguins)
str(df_penguins)
glimpse(df_penguins) #view the first few rows


#-------------------------------------------------------------------------------
# Clean data
#-------------------------------------------------------------------------------
df_penguins_clean <- df_penguins %>% drop_na()


#-------------------------------------------------------------------------------
# Summary statistics
#-------------------------------------------------------------------------------
## Calculate summary statistics by species
summary_statistics <- df_penguins_clean %>%
  group_by(species) %>%
  summarize(
    count = n(),
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    mean_body_mass = mean(body_mass_g, na.rm = TRUE)
  )
print(summary_statistics)

## Export to CSV
write.table(summary_statistics,
            file = file.path(paths$outdir, "PENG_summ_stats_species_v01.csv"),
            sep = ";",            #use ";" as delimiter
            dec = ".",            #use "." as decimal point
            na = "",              #replace NA with empty string
            row.names = FALSE)    #exclude row names


#-------------------------------------------------------------------------------
# Bill length vs. flipper length
#-------------------------------------------------------------------------------
## Create scatter plot of bill length vs. flipper length
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

## Save plot
ggsave(file.path(paths$outdir, "PENG_plot_bill_flipper_v01.pdf"), plot_bill_flipper)


#-------------------------------------------------------------------------------
# Body mass by species and sex
#-------------------------------------------------------------------------------
## Create boxplot of body mass by species and sex
colors2 <- c(zissou_colors[1], zissou_colors[5])

plot_mass_species_sex <- df_penguins_clean %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = colors2, name = "Sex") +
  labs(x = "",
       y = "Body mass (g)") +
  theme_light()
plot(plot_mass_species_sex)

## Save plot
ggsave(file.path(paths$outdir, "PENG_plot_mass_species_sex_v01.pdf"), plot_mass_species_sex)


#-------------------------------------------------------------------------------
# Predict whether a penguin is an Adelie species or not based on flipper length and bill length
#-------------------------------------------------------------------------------
## Create a binary outcome: is the species Adelie?
df_penguins_clean <- df_penguins_clean %>%
  mutate(is_adelie = ifelse(species == "Adelie", 1, 0))

## Fit logistic regression model
model_adelie <- glm(is_adelie ~ flipper_length_mm + bill_length_mm,
                   data = df_penguins_clean,
                   family = binomial)

## Create a gtsummary table
model_table_adelie <- tbl_regression(model_adelie, 
                              exponentiate = TRUE, #exponentiate to get odds ratios
                              label = list(
                                flipper_length_mm ~ "Flipper length (mm)",
                                bill_length_mm ~ "Bill length (mm)")) %>%  
  bold_labels()

## View the table
model_table_adelie

## Convert to flextable
ft_adelie <- as_flex_table(model_table_adelie)

## Create Word document
doc_adelie <- read_docx() %>%
  body_add_par("Logistic regression model for Adelie penguins", style = "heading 1") %>%
  body_add_flextable(ft_adelie)

## Save to Word
print(doc_adelie, target = file.path(paths$outdir, "PENG_log_reg_adelie_v01.docx"))

gc()