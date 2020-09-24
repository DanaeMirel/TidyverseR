
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

#------------------------#
#---# Tame your data #---#
#------------------------#

#------------------------#
#---# Tidy your data #---#
#------------------------#

#-----------------------------#
#---# Transform your data #---#
#-----------------------------#

