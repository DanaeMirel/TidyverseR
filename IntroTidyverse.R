
#---------------------------------------#
#---# Introduction to the Tidyverse #---#
#---------------------------------------#

library(gapminder)
library(dplyr)

str(gapminder)
gapminder # Look at the gapminder dataset

#------------------------#
#---# Data wrangling #---#
#------------------------#

# Filter the gapminder dataset for the year 1957
gapminder %>% 
  filter(year==1957)

# Filter for China in 2002
gapminder %>% 
  filter(year=='2002'& country=='China')

# Sort in ascending order of lifeExp
gapminder %>%
  arrange(lifeExp)

# Sort in descending order of lifeExp
gapminder %>%
  arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder %>%
  filter(year=='1957') %>%
  arrange(desc(pop))

# Use mutate to change lifeExp to be in months
gapminder %>%
  mutate(lifeExp = lifeExp*12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>% 
  mutate(lifeExpMonths = lifeExp*12)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>% 
  filter(year=='2007') %>%  
  mutate(lifeExpMonths = 12 * lifeExp) %>%  
  arrange(desc(lifeExpMonths))

#----------------------------#
#---# Data visualization #---#
#----------------------------#
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
gapminder_1952 %>% 
ggplot(aes(x = pop, y = gdpPercap)) +
  geom_point()

gapminder_1952 %>% 
  ggplot(aes(x=pop, y=lifeExp)) + 
  geom_point()

# Change this plot to put the x-axis on a log scale
gapminder_1952 %>% 
ggplot(aes(x = pop, y = lifeExp)) +
  geom_point() + 
  scale_x_log10()

# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
gapminder_1952 %>% 
  ggplot(aes(x=pop, y=gdpPercap)) +
  geom_point() +
  scale_x_log10()+ 
  scale_y_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
gapminder_1952 %>% 
  ggplot(aes(x=pop, y=lifeExp, color=continent)) +
  geom_point() +
  scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
gapminder_1952 %>% 
ggplot(aes(x = pop, y = lifeExp, color = continent, size=gdpPercap)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, faceted by continent
gapminder_1952 %>% 
  ggplot(aes(x=pop, y=lifeExp)) +
  geom_point() + 
  scale_x_log10() +
  facet_wrap(~continent)


# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
  geom_point() + 
  scale_x_log10()+
  facet_wrap(~year)

#----------------------------------#
#---# Grouping and summarizing #---#
#----------------------------------#

# Summarize to find the median life expectancy
gapminder %>% 
  summarise(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>% 
  filter(year==1957) %>% 
  summarize(medianLifeExp = median(lifeExp)) 

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>% 
  filter(year==1957) %>% 
  summarize(medianLifeExp= median(lifeExp),
            maxGdpPercap = max(gdpPercap)) 

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>% 
  group_by(year) %>% 
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpPercap= max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% 
  filter(year==1957) %>%  
  group_by(continent) %>% 
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpPercap= max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent/year combination
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
by_year %>%
  ggplot(aes(x = year, y = medianLifeExp)) +
  geom_point() +
  expand_limits(y=0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
by_year_continent %>%
  ggplot(aes(x = year, y = medianGdpPercap, color=continent)) +
  geom_point() +
  expand_limits(y=0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year==2007) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp),
            medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
by_continent_2007 %>%
  ggplot(aes(x = medianGdpPercap, y = medianLifeExp,  color=continent)) +
  geom_point() +
  expand_limits(y=0)

#---------------------------------#
#---# Types of visualizations #---#
#---------------------------------#

#---# Line plots #---#

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>% 
  group_by(year) %>%
  summarise(medianGdpPercap=median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
by_year %>% 
  ggplot(aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0) 

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>% 
  group_by(year, continent) %>%
  summarise(medianGdpPercap=median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
by_year_continent %>% 
  ggplot(aes(x = year, y = medianGdpPercap, color=continent)) +
  geom_line() +
  expand_limits(y = 0)  

#---# bar plots #---# 

# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
  filter(year==1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
by_continent %>% 
  ggplot(aes(x=continent, y=medianGdpPercap)) +
  geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(continent=='Oceania' & year=='1952') 

# Create a bar plot of gdpPercap by country
oceania_1952 %>% 
  ggplot(aes(x=country, y=gdpPercap)) +
  geom_col()

#---# histograms #---#

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
gapminder_1952 %>% 
  ggplot(aes(x=pop_by_mil)) + 
  geom_histogram(bins=50)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
gapminder_1952 %>% 
  ggplot(aes(x=pop)) + 
  geom_histogram() +
  scale_x_log10()

#---# boxplot #---#
