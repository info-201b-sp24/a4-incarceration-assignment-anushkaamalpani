library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
jail_state_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

# Clean the data by filtering out rows with missing year values
jail_state_data_clean <- jail_state_data %>%
  filter(!is.na(year))

# Reshape the data from wide to long format for the selected races
jail_long <- jail_state_data_clean %>%
  select(year, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  pivot_longer(cols = starts_with("aapi_jail_pop") | starts_with("black_jail_pop") | 
                 starts_with("latinx_jail_pop") | starts_with("native_jail_pop") | 
                 starts_with("white_jail_pop"),
               names_to = "race",
               values_to = "population",
               names_prefix = "")

# Adjust race names for better readability
jail_long <- jail_long %>%
  mutate(race = recode(race,
                       aapi_jail_pop = "Asian American/Pacific Islander",
                       black_jail_pop = "Black",
                       latinx_jail_pop = "Latin",
                       native_jail_pop = "Native",
                       white_jail_pop = "White"))

# Plot the data
graph <- ggplot(jail_long, aes(x = year, y = population, color = race)) +
  geom_line(stat = 'summary', fun = 'mean') +
  labs(title = "Jail Population Trends by Race Over Time",
       x = "Year",
       y = "Jail Population",
       color = "Race") +
  theme_minimal()

