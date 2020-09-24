
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


#----------------------------#
#---# Data visualization #---#
#----------------------------#

#----------------------------------#
#---# Grouping and summarizing #---#
#----------------------------------#

#---------------------------------#
#---# Types of visualizations #---#
#---------------------------------#

