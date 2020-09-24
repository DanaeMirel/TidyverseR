
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
gapminder %>% 
  group_by(continent, year) %>% 
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpPercap = max(gdpPercap))



#---------------------------------#
#---# Types of visualizations #---#
#---------------------------------#

