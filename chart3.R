
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)

# Load the dataset
jail_state_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

# Function to convert state abbreviations to full names
get_full_state_name <- function(state_abbr) {
  state_names <- data.frame(
    state_abbr = state.abb,
    state_full = state.name
  )
  return(state_names$state_full[match(state_abbr, state_names$state_abbr)])
}

# Filter data for the year 2018
jail_data_2018 <- jail_state_data %>%
  filter(year == 2018)

# Filter and clean the data to remove rows with missing values
cleaned_data_2018 <- jail_data_2018 %>%
  filter(!is.na(white_jail_pop) & !is.na(black_jail_pop))

# Calculate the ratio of black to white jail population by state
jail_data_aggregated <- cleaned_data_2018 %>%
  group_by(state) %>%
  summarize(white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(black_to_white_ratio = black_jail_pop / white_jail_pop)

# Convert state abbreviations to full state names
jail_data_aggregated$state_full <- get_full_state_name(jail_data_aggregated$state)

# Get the map data
us_map <- map_data("state")

# Merge the jail population data with the map data
jail_data_aggregated$region <- tolower(jail_data_aggregated$state_full)
merged_data <- merge(us_map, jail_data_aggregated, by = "region", all.x = TRUE)
merged_data <- merged_data[order(merged_data$order), ]

# Plot the map using ggplot2
mapplot <- ggplot(data = merged_data, aes(x = long, y = lat, group = group, fill = black_to_white_ratio)) +
  geom_polygon(color = "black") +
  scale_fill_viridis_c(option = "C", name = "Black to White Jail Pop. Ratio") +
  labs(title = "Geographic Distribution of Black to White Jail Population Ratio by State in 2018") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_map()
