
library(dplyr)
jail_state_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

#1. The average total jail population across all counties in 2018

# 2018 data
jail_data_2018 <- filter(jail_state_data, year == 2018)

avg_jail_pop_2018 <- summarise(jail_data_2018, avg_total_jail_pop = mean(total_jail_pop, na.rm = TRUE))

avg_jail_pop_2018_value <- avg_jail_pop_2018$avg_total_jail_pop #248.945

#2. The county with the highest jail population in 2018.

max_jail_pop <- max(jail_data_2018$total_jail_pop, na.rm = TRUE)

# Filter the data to get the row(s) with the maximum jail population
county_max_jail_pop <- filter(jail_data_2018, total_jail_pop == max_jail_pop)

# Display the county name and the number of inmates
county_name <- county_max_jail_pop$county #Los Angeles County
number_of_inmates <- county_max_jail_pop$total_jail_pop #17208

#3. The average jail population for Black individuals in 2018 

avg_black_jail_pop_2018 <- summarise(jail_data_2018, avg_black_jail_pop = mean(black_jail_pop, na.rm = TRUE))

avg_black_jail_pop_2018_value <- avg_black_jail_pop_2018$avg_black_jail_pop #83.57426

# approximately 33.6% of the jail population is Black

#4. By how much did the Black individuals jail population increase from 1990 to 2018 (in percentage)?

# Filter data for the years 1990 and 2018
jail_data_filtered <- filter(jail_state_data, year %in% c(1990, 2018))

# Summarize the total jail population for each year
black_jail_pop <- jail_data_filtered %>%
  group_by(year) %>%
  summarise(black_population = sum(black_jail_pop, na.rm = TRUE))

# Calculate the percentage increase
pop_1990 <- black_jail_pop$black_population[black_jail_pop$year == 1990]
pop_2018 <- black_jail_pop$black_population[black_jail_pop$year == 2018]

black_percentage_increase <- ((pop_2018 - pop_1990) / pop_1990) * 100 #44.64293% increase


#5. In 2018, the male jail population was C times larger than the female jail population.

# Summarize the total male and female jail populations
total_gender_pop <- jail_data_2018 %>%
  summarise(
    total_male_pop = sum(male_jail_pop, na.rm = TRUE),
    total_female_pop = sum(female_jail_pop, na.rm = TRUE)
  )

# Calculate the ratio of male to female jail population
male_to_female_ratio <- total_gender_pop$total_male_pop / total_gender_pop$total_female_pop #5.972268
# approx 6 times more males than females


#6. By how much did the White individuals jail population increase from 1990 to 2018 (in percentage)?

# Summarize the total jail population for each year
white_jail_pop <- jail_data_filtered %>%
  group_by(year) %>%
  summarise(white_population = sum(white_jail_pop, na.rm = TRUE))

# Calculate the percentage increase
white_pop_1990 <- white_jail_pop$white_population[white_jail_pop$year == 1990]
white_pop_2018 <- white_jail_pop$white_population[white_jail_pop$year == 2018]

white_percentage_increase <- ((white_pop_2018 - white_pop_1990) / white_pop_1990) * 100 #108.2094%


summary <- list(
  avg_total_jail_pop_2018 = avg_jail_pop_2018_value,
  county_with_max_jail_pop_2018 = county_name,
  max_jail_pop_2018 = number_of_inmates,
  avg_black_jail_pop_2018_value = avg_black_jail_pop_2018_value,
  black_percentage_increase = black_percentage_increase,
  white_percentage_increase = white_percentage_increase,
  male_to_female_ratio_2018 = male_to_female_ratio
)
