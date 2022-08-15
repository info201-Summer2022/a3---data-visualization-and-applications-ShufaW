install.packages("plotly")

library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(usmap)

trend_of_inc <- read.csv('/Users/karen/Desktop/SU22/INFO 201/a3---data-visualization-and-applications-ShufaW/A3_data/incarceration_trends.csv')
pop_of_jail_df <- read.csv('/Users/karen/Desktop/SU22/INFO 201/a3---data-visualization-and-applications-ShufaW/A3_data/year-end-prison-2021.csv')

# For each race in 2017, in which state do they have the highest prison population?
state_2017<- trend_of_inc %>%
  filter(year == "2017") %>%
  select(state, total_pop_15to64:white_pop_15to64, total_jail_pop:white_jail_pop) %>%
  group_by(state)

sum_new_df <- summarise(
    state_2017,
    'aapi_prison' = sum(aapi_jail_pop),
    'black_prison' = sum(black_jail_pop),
    'latin_prison' = sum(latinx_jail_pop),
    'native_prison' = sum(native_jail_pop),
    'white_prison' = sum(white_jail_pop),
  )

sum_new_df_tailor <- filter(sum_new_df, !is.na(sum_new_df$`aapi_prison`))

aapi_2017 <- unlist(sum_new_df_tailor[sum_new_df_tailor$aapi_prison == max(sum_new_df_tailor$aapi_prison), 'state'])
black_2017 <- unlist(sum_new_df_tailor[sum_new_df_tailor$black_prison == max(sum_new_df_tailor$black_prison), 'state'])
latinx_2017 <- unlist(sum_new_df_tailor[sum_new_df_tailor$latin_prison == max(sum_new_df_tailor$latin_prison), 'state'])
native_2017 <- unlist(sum_new_df_tailor[sum_new_df_tailor$native_prison == max(sum_new_df_tailor$native_prison), 'state'])
white_2017 <- unlist(sum_new_df_tailor[sum_new_df_tailor$white_prison == max(sum_new_df_tailor$white_prison), 'state'])

# Which is the change of population in prison from 2019 to 2021?
pop_change_df <- pop_of_jail_df %>%
  filter(region == "US Total") %>%
  select(total_prison_pop_2019,total_prison_pop_2021)

pop_change <- pop_change_df$total_prison_pop_2021 - pop_change_df$total_prison_pop_2019
print(pop_change)

# Which state has the most male in jail in 2017?
gender_jail_df <- state_2017 %>%
  select(state, female_jail_pop, male_jail_pop)

gender_jail_df_tailor <- na.omit(gender_jail_df)

max_male_2017 <- unlist(gender_jail_df_tailor[gender_jail_df_tailor$male_jail_pop == max(gender_jail_df_tailor$male_jail_pop), 'state'])
print(max_male_2017)

# Which state has the most female in prison in 2017?
max_female_2017 <- unlist(gender_jail_df_tailor[gender_jail_df_tailor$female_jail_pop == max(gender_jail_df_tailor$female_jail_pop), 'state'])
print(max_female_2017)

# Which state has the least and most population in prison in 2019?
jail_population <- pop_of_jail_df[8:57, 2:5]
min_pop_2019 <- jail_population[jail_population$total_prison_pop_2019 == min(jail_population$total_prison_pop_2019), 'state_name']
max_pop_2019 <- jail_population[jail_population$total_prison_pop_2019 == max(jail_population$total_prison_pop_2019), 'state_name']

# Which state has the least and most population in prison in 2020?
min_pop_2020 <- jail_population[jail_population$total_prison_pop_2020 == min(jail_population$total_prison_pop_2020), 'state_name']
max_pop_2020 <- jail_population[jail_population$total_prison_pop_2020 == max(jail_population$total_prison_pop_2020), 'state_name']

# Which state has the least and most population in prison in 2021?
min_pop_2021 <- jail_population[jail_population$total_prison_pop_2021 == min(jail_population$total_prison_pop_2021), 'state_name']
max_pop_2021 <- jail_population[jail_population$total_prison_pop_2021 == max(jail_population$total_prison_pop_2021), 'state_name']

# Trend over time chart
# How the total population in prison at King County in WA changed from 1970 to 2018?
# Create a data frame that only include the information that I need.
population_df <- trend_of_inc %>%
  filter(state =="WA") %>%
  filter(county_name == "King County") %>%
  select(year, state, total_pop, total_jail_pop, female_jail_pop, male_jail_pop)

# Use ggplot2 to create a line chart that shows the trend.
line_plot <- ggplot(data = population_df, aes(x = year)) +
  geom_line(aes(y = total_jail_pop, color = "Total population"), group = 1) +
  geom_line(aes(y = female_jail_pop, color = "Female population"), group = 1) +
  geom_line(aes(y = male_jail_pop, color = "Male population"), group = 1)
line_plot <- line_plot + ggtitle("Population in jail at King County in WA change form 1970 to 2018") + xlab("Time(year)") + ylab("Population in the jail")
line_plot

# A chart that compares two variables to one another.
# Compare how the native_jail_pop_rate related to total_jail_pop_rate
pop_rate_df <- trend_of_inc %>%
  select(year, state, native_jail_pop_rate, total_jail_pop_rate) %>%
  na.omit()
options(scipen = 60000)
pop_plot <- ggplot(data = pop_rate_df, aes(x = native_jail_pop_rate))
pop_plot <- pop_plot + geom_line(aes(y = total_jail_pop_rate, color = "relationship"), group = 1)
pop_plot <- pop_plot + ggtitle("How native jail population rate related to total jail population rate") + xlab("the native jail population rate") + ylab("the total jail population rate")
pop_plot

# Map
geo_df <- trend_of_inc %>%
  select(state, total_jail_pop)
geo_df <- geo_df %>% group_by(state) %>%
  summarise(
    "Total_pop_in_jail" = sum(total_jail_pop, na.rm=TRUE)
  )

plot_usmap(regions = "state", data = geo_df, values = "Total_pop_in_jail", color = "white") +
  scale_fill_continuous(name = "Population in jail in each state") + labs(title = "Population in jail of each state in the US distribution")

