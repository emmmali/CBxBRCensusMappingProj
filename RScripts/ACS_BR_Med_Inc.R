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
library(mapview)
library(sf)

# Use cache for mapping
options(tigris_use_cache = TRUE)

# Load unique US Census API key (request here if needed: https://api.census.gov/data/key_signup.html ).
# Note: Setting install = TRUE will save the key for all future uses.
# census_api_key("47923ba1e02501406dc71b4ded6fed44e70c6b04", install = TRUE)
# census_api_key("<insert API key here>", install = TRUE)

### Read in BakerRipley tract list and neighborhood names ###
BR_tracts = read.csv("TractList_BR_All.csv")

# Include only relevant columns
# Get all census tracts
BR_tracts = subset(BR_tracts, select=c("Neighborhood", "GEOID", "Tract", "Tractce"))

# Load in the ACS Variables for BakerRipley Neighborhoods
BR_VARS_ALL_2021 = read.csv("BR_VARS_ALL_2021.csv")

### Read in the desired Census Data Variables ###
ACS_Vars_21_With_BR = read.csv("ACSVariables2021CondensedModVars.csv")

ACS_Vars_21 = subset(ACS_Vars_21_With_BR, ACS_Label != 'BakerRipley Calculation')

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2021 = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = 'DP03_0062', # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2021 #Desired end year for 5-yr ACS range
)

# Merge with tract names
ACS_BR_2021 = merge(x = BR_tracts, y = ACS_Harris_2021, by.x = "GEOID", by.y = "GEOID")

# Merge with variable descriptions
ACS_BR_2021 = merge(x = ACS_Vars_21, y = ACS_BR_2021, by.x = "Var_2021", by.y = "variable")

# Convert merged data to dataframe
ACS_BR_2021 = data.frame(ACS_BR_2021)

ACS_BR_2021 = unique(ACS_BR_2021)


# Make a dataframe with geo info (tract, GEOID, geometry), then merge with calculations after
ACS_GEO_INFO = subset(ACS_BR_2021, select=c(GEOID, Neighborhood, Tract, geometry))
ACS_GEO_INFO = unique(ACS_GEO_INFO)

# Merge GEO INFO with BR Variables
BR_VARS_ALL_2021_GEO = merge(x = BR_VARS_ALL_2021, y = ACS_GEO_INFO, by.x = "Tract", by.y = "Tract")

ACS_BR_2021_Med_Inc = subset(BR_VARS_ALL_2021_GEO, Var_2021 == 'DP03_0062')

st_geometry(ACS_BR_2021_Med_Inc) = "geometry"

mapview(ACS_BR_2021_Med_Inc, zcol = "Neighborhood.x")

