#' #################################################################
#' load the libraries that will give us extra functions for our work
#' #################################################################

library(tidyverse)


#' #################################################################
#' load the penguin data (via another library)
#' #################################################################

library(palmerpenguins)
data("penguins")


#' #################################################################
#' examine the penguin data
#' #################################################################

penguins

penguins %>%
  glimpse()

penguins %>%
  View()



#' #################################################################
#' manipulate the penguin data
#' #################################################################

# operations on rows include `filter` & `slice`

penguins

penguins %>%
  filter(body_mass_g > 4500)

penguins %>%
  filter(body_mass_g > median(body_mass_g, na.rm = TRUE))

penguins %>%
  arrange(desc(flipper_length_mm)) %>%
  slice(1:10)


# operations on columns include `select`, `mutate`, & `summarize`

# select: subset columns
penguins %>%
  select(species, year, body_mass_g)


# mutate: create new columns
penguins %>%
  mutate(big_body = body_mass_g > median(body_mass_g, na.rm = TRUE))

penguins %>%
  mutate(big_body = body_mass_g > median(body_mass_g, na.rm = TRUE)) %>%
  glimpse()

penguins %>%
  mutate(big_body = body_mass_g > median(body_mass_g, na.rm = TRUE),
         little_flipper = flipper_length_mm < median(flipper_length_mm, na.rm = TRUE)) %>%
  glimpse()


# summarize: create new columns that summarize row groups
penguins %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE), .by = species)

penguins %>%
  group_by(species) %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))

penguins %>%
  group_by(species) %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE),
            sd_mass = sd(body_mass_g, na.rm = TRUE),
            median_mass = median(body_mass_g, na.rm = TRUE),
            iqr_mass = IQR(body_mass_g, na.rm = TRUE))



#' #################################################################
#' plot the penguin data
#' #################################################################

penguins %>% 
  ggplot(data = .) +
  geom_boxplot(aes(y = species, x = flipper_length_mm, fill = species)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal()


penguins %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_light()


penguins %>% 
  ggplot(data = ., aes(y = body_mass_g, x = flipper_length_mm, color = species)) +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "Spectral") +
  theme_bw()


penguins %>% 
  ggplot(data = ., aes(y = body_mass_g, x = flipper_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Spectral") +
  theme_classic()



#' #################################################################
#' manipulate & plot the penguin data
#' #################################################################

penguins %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE),
            sd_mass = sd(body_mass_g, na.rm = TRUE),
            median_mass = median(body_mass_g, na.rm = TRUE),
            iqr_mass = IQR(body_mass_g, na.rm = TRUE),
            lower_quartile = quantile(body_mass_g, na.rm = TRUE, probs = 0.25),
            upper_quartile = quantile(body_mass_g, na.rm = TRUE, probs = 0.75),
            .by = species) %>%
  ggplot(data = .) +
  geom_linerange(aes(y = species, xmin = lower_quartile, xmax = upper_quartile, color = species)) +
  geom_point(aes(y = species, x = median_mass, color = species)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Big Penguins & Little Penguins", subtitle = "(from the `palmerpenguin` R package)", x = "Median & IQR Body Mass (g)", y = "", color = "")
  


#' #################################################################
#' saving new data
#' #################################################################

# name it!

penguin_species_summary <- penguins %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE),
            sd_mass = sd(body_mass_g, na.rm = TRUE),
            median_mass = median(body_mass_g, na.rm = TRUE),
            iqr_mass = IQR(body_mass_g, na.rm = TRUE),
            lower_quartile = quantile(body_mass_g, na.rm = TRUE, probs = 0.25),
            upper_quartile = quantile(body_mass_g, na.rm = TRUE, probs = 0.75),
            .by = species)

penguin_species_summary

# save it!

penguin_species_summary %>%
  write_csv("tables/penguin_species_summary.csv")

penguin_species_summary %>%
  writexl::write_xlsx("tables/penguin_species_summary.xlsx")



#' #################################################################
#' saving plots
#' #################################################################

# name it!

penguin_flipper_boxplot <- penguins %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_light()

penguin_flipper_boxplot


# save it!

penguin_flipper_boxplot %>%
  ggsave(filename = "figures/penguin_flipper_boxplot.pdf", plot = ., height = 6, width = 8, units = "in")

penguin_flipper_boxplot %>%
  ggsave(filename = "figures/penguin_flipper_boxplot.png", plot = ., height = 6, width = 8, units = "in", dpi = 300)



