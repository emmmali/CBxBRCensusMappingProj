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
library(mapview)
library(sf)

# Use cache for mapping
options(tigris_use_cache = TRUE)

# Load unique US Census API key (request here if needed: https://api.census.gov/data/key_signup.html ).
# Note: Setting install = TRUE will save the key for all future uses.
# census_api_key("47923ba1e02501406dc71b4ded6fed44e70c6b04", install = TRUE)
# census_api_key("<insert API key here>", install = TRUE)

# Load in the ACS Variables for BakerRipley Neighborhoods
BR_VARS_ALL_2021 = read.csv("BR_VARS_ALL_2021.csv")

### Read in the desired Census Data Variables ###
ACS_Vars_21_With_BR = read.csv("ACSVariables2021CondensedModVars.csv")

ACS_Vars_21 = subset(ACS_Vars_21_With_BR, ACS_Label != 'BakerRipley Calculation')

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_Housing_2021 = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = c('DP04_0050',
                'DP04_0051'), # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2021 #Desired end year for 5-yr ACS range
)


# Merge with variable descriptions
ACS_Harris_Housing_2021 = merge(x = ACS_Vars_21, y = ACS_Harris_Housing_2021, by.x = "Var_2021", by.y = "variable")

View(ACS_Harris_Housing_2021)

# Convert merged data to dataframe
ACS_Harris_Housing_2021 = data.frame(ACS_Harris_Housing_2021)

ACS_Harris_Housing_2021 = unique(ACS_Harris_Housing_2021)

### Category: Housing Tenure ###

# Create % of Population Column
ACS_Harris_Housing_2021['Percent_Of_Population'] = NA
View(ACS_Harris_Housing_2021)

# Create Total Population Column (initially NA)
ACS_Harris_Housing_2021['Total_Population'] = NA
View(ACS_Harris_Housing_2021)

UNIQUE_GEOIDS = unique(ACS_Harris_Housing_2021$GEOID)
View(UNIQUE_GEOIDS)
UNIQUE_GEOIDS = data.frame(UNIQUE_GEOIDS)
View(UNIQUE_GEOIDS)

### Set the total population for each tract ###
for (GEO_ID in ACS_Harris_Housing_2021$GEOID){
  
  ACS_Harris_Housing_2021[ACS_Harris_Housing_2021$GEOID == GEO_ID,]$Total_Population = 
    ACS_Harris_Housing_2021[ACS_Harris_Housing_2021$GEOID == GEO_ID
                          &ACS_Harris_Housing_2021$Variable_Name == 
                            'Total Occupied housing units',]$estimate
  
}


### Get the percentages ###
ACS_Harris_Housing_2021$Percent_Of_Population = ACS_Harris_Housing_2021$estimate /
  ACS_Harris_Housing_2021$Total_Population

### Round + multiply percent by 100 ###

ACS_Harris_Housing_2021$Percent_Of_Population = 
  round(ACS_Harris_Housing_2021$Percent_Of_Population * 100, 2)


Harris_After_2017 = subset(ACS_Harris_Housing_2021, Var_2021 == 'DP04_0051')

Harris_After_2017 = subset(ACS_Harris_Housing_2021, select = -c(ACS_Label))


st_geometry(Harris_After_2017) = "geometry"

mapview(Harris_After_2017, zcol = "Percent_Of_Population")

help(mapview)

