
#--------------------------------------------#
#---# Working with Data in the Tidyverse #---#
#--------------------------------------------#

#---------------------------#
#---# Explore your data #---#
#---------------------------#

library(readr)
library(dplyr)
library(skimr)
library(ggplot2)

# Create bakeoff but skip first row
bakeoff <- read_csv("https://assets.datacamp.com/production/repositories/1613/datasets/53cf6583aa659942b787897319a1ac053cbcfa5a/bakeoff.csv")

# Print bakeoff
bakeoff

# Filter rows where showstopper is UNKNOWN 
bakeoff %>% filter(showstopper == "UNKNOWN")

# Edit to add list of missing values
bakeoff <- read_csv("bakeoff.csv", skip = 1, na = c("", "NA", "UNKNOWN"))

# Filter rows where showstopper is NA 
bakeoff %>% filter(is.na(showstopper))

bakeoff %>% 
  arrange(uk_airdate) %>% 
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

#bakeoff <- read_csv("messy_baker_results.csv", skip=1)
#bakeoff <- read_csv("baker_results.csv", skip=1)

#------------------------#
#---# Tame your data #---#
#------------------------#

#---# Cast column types #---#

# Find format to parse uk_airdate 
parse_date("17 August 2010", format = "%d %B %Y")

# Edit to cast uk_airdate
desserts <- read_csv("desserts.csv")

desserts <- read_csv("desserts.csv", 
                     col_types = cols(uk_airdate = col_date(format = "%d %B %Y")))

# Arrange by descending uk_airdate
desserts %>% 
  arrange(desc(uk_airdate))

# Try to cast technical as a number
desserts <- read_csv("desserts.csv", col_types = cols(
                          uk_airdate = col_date(format = "%d %B %Y"),
                          technical = col_number()))
# View parsing problems
problems(desserts)

# Edit code to fix the parsing error 
desserts <- read_csv("desserts.csv", col_types = cols(
                          uk_airdate = col_date(format = "%d %B %Y"),
                          technical = col_number()), 
                          na = c("", "NA", "N/A"))

# Cast result a factor
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y"),
                       technical = col_number(),                       
                       result = col_factor(levels=NULL)))
# Glimpse to view
glimpse(desserts)

#---# Recode values #---#

names(desserts)

# Count rows grouping by nut variable
desserts %>% 
  count(signature_nut, sort = TRUE)

# Edit code to recode "no nut" as missing
desserts_2 <- desserts %>% 
  mutate(nut = recode(signature_nut, "filbert" = "hazelnut", 
                      "no nut" = NA_character_))
# Count rows again 
desserts_2 %>% 
  count(nut, sort = TRUE)
 
glimpse(desserts) 
 
# Edit to recode tech_win as factor
desserts <- desserts %>% 
  mutate(tech_win = recode_factor(technical, `1` = 1,
                                  .default = 0))
# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)

#---# Select variables #---# 

ratings <- read_csv('02.03_messy_ratings.csv')

# Recode channel as factor: bbc (1) or not (0)
ratings <- ratings %>% 
  mutate(bbc = recode_factor(channel, 
                             "Channel 4" = 0,
                             .default = 1))

# Select to look at variables to plot next
ratings %>% 
  select(series, channel, bbc)

# Make a filled bar chart
ggplot(ratings, aes(x = series, y = bbc, fill = bbc)) +
  geom_col()

# Move channel to front and drop 7-/28-day episode ratings
ratings %>% 
  select(channel, everything(), -ends_with("day"))

#---# Tame variable names #---#

library(janitor)

messy_ratings <- read_csv('02.03_messy_ratings.csv')

# Glimpse to see variable names
glimpse(messy_ratings)

# Reformat to snake case
ratings <- messy_ratings %>%  
  clean_names("lower_camel")

# Glimpse cleaned names
glimpse(ratings)

# Adapt code to also rename 7-day viewer data
viewers_7day <- ratings %>% 
  select(series, viewers_7day_ = ends_with("7day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to keep original order
viewers_7day <- ratings %>% 
  select(everything(),
         viewers_7day_ = ends_with("7day"), 
         -ends_with("28day"))


# Glimpse
glimpse(viewers_7day)

#------------------------#
#---# Tidy your data #---#
#------------------------#

ratings <- read_csv('messy_ratings.csv')
head(ratings)

# Plot of episode 1 viewers by series
ratings %>% 
  ggplot(aes(x=series, y=e1)) + geom_col()

# Adapt code to plot episode 2 viewers by series
ggplot(ratings, aes(x = series, y = e2)) +
  geom_col()


x <- c(5, 1, 3, 2, 2, NA)
row_number(x)

tidy_ratings <- ratings %>%
  # Gather and convert episode to factor
  gather(key = "episode", value = "viewers_7day", -series, 
         factor_key = TRUE, na.rm = TRUE) %>% 
  # Sort in ascending order by series and episode
  arrange(series, episode)

#-----------------------------#
#---# Transform your data #---#
#-----------------------------#

