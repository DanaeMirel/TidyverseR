
#--------------------------------------------#
#---# Working with Data in the Tidyverse #---#
#--------------------------------------------#

#---------------------------#
#---# Explore your data #---#
#---------------------------#

library(readr)
library(dplyr)
library(skimr)

# Create bakeoff but skip first row
bakeoff <- read_csv("messy_baker_results.csv", skip=1)
#bakeoff <- read_csv("baker_results.csv", skip=1)

# Print bakeoff
bakeoff

# Filter rows where showstopper is UNKNOWN 
bakeoff %>% 
  filter(showstopper == "UNKNOWN")

# Edit to add list of missing values
bakeoff <- read_csv("bakeoff.csv", skip = 1,
                    na = c("", "NA", "UNKNOWN"))

# Filter rows where showstopper is NA 
bakeoff %>%
  filter(is.na(showstopper))

bakeoff %>% 
  arrange(age) %>% 
  glimpse() # no argument needed here

# Edit to filter, group by, and skim
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season) %>% 
  skim()

bakeoff %>% 
  skim() %>%  # no argument needed here
  summary()

# View distinct results
bakeoff %>% 
  distinct(result)

# Count rows for each result
bakeoff %>% 
  count(result)

# Count whether or not star baker
bakeoff %>% 
  count(result=='SB')

# Add second count by series
bakeoff %>% 
  count(series, episode) %>%
  count(series)

# Count the number of rows by series and baker
bakers_by_series <- bakeoff %>% 
  count(series, baker)

# Print to view
bakers_by_series

# Count again by series
bakers_by_series %>% 
  count(series)

# Count again by baker
bakers_by_series %>%
  count(baker, sort=TRUE)

ggplot(bakeoff, aes(x=episode)) + 
  geom_bar() + 
  facet_wrap(~series)

#------------------------#
#---# Tame your data #---#
#------------------------#

#------------------------#
#---# Tidy your data #---#
#------------------------#

#-----------------------------#
#---# Transform your data #---#
#-----------------------------#

