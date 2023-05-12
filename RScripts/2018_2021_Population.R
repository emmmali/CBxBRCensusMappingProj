#### Clear Workspace ###
cat("\014")
rm(list=ls())
set.seed(18552)

### Set working directory ###
setwd("~/Desktop/Community_Bridges/BakerRipley/Census_Data_Mapping")
# setwd("<insert path to your desired directory>")

### Set up necessary libraries ###
# Install packages first if not already installed
# install.packages(dplyr)
# install.packages(tidycensus)
# install.packages(tidyverse)
# install.packages(mapview)

library(dplyr)
library(tidycensus)
library(tidyverse)
library(ggplot2)

### Read in BakerRipley tract list and neighborhood names ###
BR_tracts = read.csv("TractList_BR_All.csv")

# Get all census tracts
BR_tracts = subset(BR_tracts, select=c("Neighborhood", "GEOID", "Tract", "Tractce"))

### Read in the desired Census Data Variables ###
ACS_Vars_21_With_BR = read.csv("ACSVariables2021CondensedModVars.csv")

ACS_Vars_21 = subset(ACS_Vars_21_With_BR, ACS_Label != 'BakerRipley Calculation')

pop_race_vars = c('DP05_0071',
                  'DP05_0077',
                  'DP05_0078',
                  'DP05_0079',
                  'DP05_0080',
                  'DP05_0081',
                  'DP05_0082',
                  'DP05_0083')

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2021_Pop_Race = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = pop_race_vars, # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2021 #Desired end year for 5-yr ACS range
)

View(ACS_Harris_2021_Pop_Race)

# Merge with tract names
ACS_BR_2021_Pop_Race = merge(x = BR_tracts, y = ACS_Harris_2021_Pop_Race, by.x = "GEOID", by.y = "GEOID")

# Merge with variable descriptions
ACS_BR_2021_Pop_Race = merge(x = ACS_Vars_21, y = ACS_BR_2021_Pop_Race, by.x = "Var_2021", by.y = "variable")

# Convert merged data to dataframe
ACS_BR_2021_Pop_Race = data.frame(ACS_BR_2021_Pop_Race)

ACS_BR_2021_Pop_Race = unique(ACS_BR_2021_Pop_Race)

View(ACS_BR_2021_Pop_Race)

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2020_Pop_Race = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = pop_race_vars, # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2020 #Desired end year for 5-yr ACS range
)

View(ACS_Harris_2020_Pop_Race)

# Merge with tract names
ACS_BR_2020_Pop_Race = merge(x = BR_tracts, y = ACS_Harris_2020_Pop_Race, by.x = "GEOID", by.y = "GEOID")

# Merge with variable descriptions
ACS_BR_2020_Pop_Race = merge(x = ACS_Vars_21, y = ACS_BR_2020_Pop_Race, by.x = "Var_2021", by.y = "variable")

# Convert merged data to dataframe
ACS_BR_2020_Pop_Race = data.frame(ACS_BR_2020_Pop_Race)

ACS_BR_2020_Pop_Race = unique(ACS_BR_2020_Pop_Race)

View(ACS_BR_2020_Pop_Race)

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2019_Pop_Race = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = pop_race_vars, # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2019 #Desired end year for 5-yr ACS range
)

View(ACS_Harris_2019_Pop_Race)

# Merge with tract names
ACS_BR_2019_Pop_Race = merge(x = BR_tracts, y = ACS_Harris_2019_Pop_Race, by.x = "GEOID", by.y = "GEOID")

# Merge with variable descriptions
ACS_BR_2019_Pop_Race = merge(x = ACS_Vars_21, y = ACS_BR_2019_Pop_Race, by.x = "Var_2021", by.y = "variable")

# Convert merged data to dataframe
ACS_BR_2019_Pop_Race = data.frame(ACS_BR_2019_Pop_Race)

ACS_BR_2019_Pop_Race = unique(ACS_BR_2019_Pop_Race)

View(ACS_BR_2019_Pop_Race)


# Create one dataframe for each neighborhood
ACS_Harris_2018_Pop_Race = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = pop_race_vars, # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2018 #Desired end year for 5-yr ACS range
)

View(ACS_Harris_2018_Pop_Race)

# Merge with tract names
ACS_BR_2018_Pop_Race = merge(x = BR_tracts, y = ACS_Harris_2018_Pop_Race, by.x = "GEOID", by.y = "GEOID")

# Merge with variable descriptions
ACS_BR_2018_Pop_Race = merge(x = ACS_Vars_21, y = ACS_BR_2018_Pop_Race, by.x = "Var_2021", by.y = "variable")

# Convert merged data to dataframe
ACS_BR_2018_Pop_Race = data.frame(ACS_BR_2018_Pop_Race)

ACS_BR_2018_Pop_Race = unique(ACS_BR_2018_Pop_Race)

View(ACS_BR_2018_Pop_Race)

gulfton_2018 = subset(ACS_BR_2018_Pop_Race, Neighborhood == "Gulfton/Sharpstown")
View(gulfton_2018)

sum(gulfton_2018[gulfton_2018$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate)


gulfton_2019 = subset(ACS_BR_2019_Pop_Race, Neighborhood == "Gulfton/Sharpstown")
View(gulfton_2019)

gulfton_2020 = subset(ACS_BR_2020_Pop_Race, Neighborhood == "Gulfton/Sharpstown")
View(gulfton_2020)

gulfton_2021 = subset(ACS_BR_2021_Pop_Race, Neighborhood == "Gulfton/Sharpstown")

gulfton_2021_native = subset(gulfton_2021, Variable_Name == 'American Indian and Alaska Native alone')
View(gulfton_2021_native)

gulfton_2021_hawaiian = subset(gulfton_2021, Variable_Name == 'Native Hawaiian and Other Pacific Islander')
View(gulfton_2021_hawaiian)





Year = c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
         2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
         2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 
         2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021)

Race = c('Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races')

sum(gulfton_2021[gulfton_2021$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate)


Population = c(sum(gulfton_2018[gulfton_2018$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'White alone', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'Asian alone', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'Some other race alone', ]$estimate),
               sum(gulfton_2018[gulfton_2018$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'White alone', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Asian alone', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Some other race alone', ]$estimate),
               sum(gulfton_2019[gulfton_2019$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'White alone', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Asian alone', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Some other race alone', ]$estimate),
               sum(gulfton_2020[gulfton_2020$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'White alone', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Asian alone', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Some other race alone', ]$estimate),
               sum(gulfton_2021[gulfton_2021$Variable_Name == 'Two or more races', ]$estimate)
              )

gulfton_2018_2021_pop_race = data.frame(Year, Race, Population)

View(gulfton_2018_2021_pop_race)

write.csv(gulfton_2018_2021_pop_race, "gulfton_2018_2021_pop_race.csv")

blah = read.csv("gulfton_2018_2021_pop_race.csv")

View(blah)

east_end_2018 = subset(ACS_BR_2018_Pop_Race, Neighborhood == "East End")
View(east_end_2018)


east_end_2019 = subset(ACS_BR_2019_Pop_Race, Neighborhood == "East End")
View(east_end_2019)

east_end_2020 = subset(ACS_BR_2020_Pop_Race, Neighborhood == "East End")
View(east_end_2020)

east_end_2021 = subset(ACS_BR_2021_Pop_Race, Neighborhood == "East End")
View(east_end_2021)




Year = c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
         2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
         2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 
         2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021)

Race = c('Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races',
         
         'Hispanic or Latino (of any race)',
         'White alone',
         'Black or African American alone',
         'American Indian and Alaska Native alone',
         'Asian alone',
         'Native Hawaiian and Other Pacific Islander',
         'Some other race alone',
         'Two or more races')


Population = c(sum(east_end_2018[east_end_2018$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'White alone', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'Asian alone', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'Some other race alone', ]$estimate),
               sum(east_end_2018[east_end_2018$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(east_end_2019[east_end_2019$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'White alone', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'Asian alone', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'Some other race alone', ]$estimate),
               sum(east_end_2019[east_end_2019$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(east_end_2020[east_end_2020$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'White alone', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'Asian alone', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'Some other race alone', ]$estimate),
               sum(east_end_2020[east_end_2020$Variable_Name == 'Two or more races', ]$estimate),
               
               sum(east_end_2021[east_end_2021$Variable_Name == 'Hispanic or Latino (of any race)', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'White alone', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'Black or African American alone', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'American Indian and Alaska Native alone', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'Asian alone', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'Native Hawaiian and Other Pacific Islander', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'Some other race alone', ]$estimate),
               sum(east_end_2021[east_end_2021$Variable_Name == 'Two or more races', ]$estimate)
)

east_end_2018_2021_pop_race = data.frame(Year, Race, Population)

View(east_end_2018_2021_pop_race)


write.csv(east_end_2018_2021_pop_race, "east_end_2018_2021_pop_race.csv")

ggplot(gulfton_2018_2021_pop_race) + 
  geom_line(mapping = aes(x = Year, y = Population, 
                          group = Race, 
                          color = Race))+
  ggtitle("Gulfton/Sharptown: Population of Each Race, 2018-2021")+
  labs(y= "Population", x = "Year")

ggplot(east_end_2018_2021_pop_race) + 
  geom_line(mapping = aes(x = Year, y = Population, 
                          group = Race, 
                          color = Race))+
  ggtitle("East End: Population of Each Race, 2018-2021")+
  labs(y= "Population", x = "Year")




  