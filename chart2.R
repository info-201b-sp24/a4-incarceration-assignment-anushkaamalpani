library(ggplot2)
library(dplyr)

# Assuming `jail_state_data` is the dataset containing the relevant data
# Load the dataset
jail_state_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

# Filter and clean the data to remove rows with missing values
cleaned_data <- jail_state_data %>%
  filter(!is.na(total_jail_pop) & !is.na(black_jail_pop))

# Create the scatter plot
scatter_plot <- ggplot(cleaned_data, aes(x = total_jail_pop, y = black_jail_pop, color = state)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "black") +
  labs(
    title = "Total Jail Population vs. Black Jail Population",
    x = "Total Jail Population",
    y = "Black Jail Population",
    color = "State"
  ) +
  theme_minimal()
