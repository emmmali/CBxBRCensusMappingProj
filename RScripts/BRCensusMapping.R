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
View(BR_VARS_ALL_2021)

### Read in the desired Census Data Variables ###
ACS_Vars_21_With_BR = read.csv("ACSVariables2021CondensedModVars.csv")
View(ACS_Vars_21_With_BR)

ACS_Vars_21 = subset(ACS_Vars_21_With_BR, ACS_Label != 'BakerRipley Calculation')
View(ACS_Vars_21)

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2021 = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = 'DP03_0062', # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2021 #Desired end year for 5-yr ACS range
)

# View county ACS data
View(ACS_Harris_2021)

# Merge with tract names
ACS_BR_2021 = merge(x = BR_tracts, y = ACS_Harris_2021, by.x = "GEOID", by.y = "GEOID")
View(ACS_BR_2021)

# Merge with variable descriptions
ACS_BR_2021 = merge(x = ACS_Vars_21, y = ACS_BR_2021, by.x = "Var_2021", by.y = "variable")
View(ACS_BR_2021)

# Convert merged data to dataframe
ACS_BR_2021 = data.frame(ACS_BR_2021)

ACS_BR_2021 = unique(ACS_BR_2021)

# View newly created datatframe
View(ACS_BR_2021)

# Make a dataframe with geo info (tract, GEOID, geometry), then merge with calculations after
ACS_GEO_INFO = subset(ACS_BR_2021, select=c(GEOID, Neighborhood, Tract, geometry))
ACS_GEO_INFO = unique(ACS_GEO_INFO)

View(ACS_GEO_INFO)

# Merge GEO INFO with BR Variables
BR_VARS_ALL_2021_GEO = merge(x = BR_VARS_ALL_2021, y = ACS_GEO_INFO, by.x = "Tract", by.y = "Tract")
View(BR_VARS_ALL_2021_GEO)

### Create a chloropleth map of all the neighborhoods, based on a single variable ###
# Create a dataframe that subsets the original for a single variable

# Example: Median Family Income, All Neighborhoods
ACS_BR_2021_Med_Inc = subset(BR_VARS_ALL_2021_GEO, Var_2021 == 'DP03_0062')
View(ACS_BR_2021_Med_Inc)

# View the new dataframe
View(ACS_BR_2021_Med_Inc)

View(BR_VARS_ALL_2021_GEO)

# Example: Median Family Income, Pasadena

ACS_BR_PAS_2021_Med_Inc = subset(ACS_BR_2021_Med_Inc, Neighborhood.x == "Pasadena")
View(ACS_BR_PAS_2021_Med_Inc)

# Example: Median Family Income, East End
ACS_BR_EA_END_2021_Med_Inc = subset(ACS_BR_EA_END_2021, Var_2021 == 'DP03_0062')

# Example: Renter-occupied Housng Units, East End
View(BR_VARS_ALL_2021_GEO)
ACS_BR_EA_END = subset(BR_VARS_ALL_2021_GEO, Neighborhood.x == "East End")
View(ACS_BR_EA_END)

ACS_BR_EA_END_RENTER = subset(ACS_BR_EA_END, Variable_Name == 'Renter-occupied housing units')
View(ACS_BR_EA_END_RENTER)

# View the new dataframe
View(ACS_BR_PAS_2021_Med_Inc)

# Example: Total Population, All Neighborhoods
ACS_BR_2021_Total_Pop = subset(ACS_BR_2021, Var_2021 == 'DP05_0001')

# View the new dataframe
View(ACS_BR_2021_Total_Pop)

# Example: Total Population, Pasadena
ACS_BR_PAS_2021_Total_Pop = subset(ACS_BR_PAS_2021, Var_2021 == 'DP05_0001')

# View the new dataframe
View(ACS_BR_PAS_2021_Total_Pop)

# Set the geometry of the newly created dataframe to be our geometry column
st_set_geometry(ACS_BR_2021_Med_Inc, "geometry")
st_geometry(ACS_BR_2021_Med_Inc) = "geometry"
st_geometry(ACS_BR_2021_Total_Pop) = "geometry"

st_geometry(ACS_BR_PAS_2021_Med_Inc) = "geometry"

st_geometry(ACS_BR_PAS_2021_Total_Pop) = "geometry"
st_geometry(ACS_BR_EA_END_2021_Med_Inc) = "geometry"

st_geometry(ACS_BR_EA_END_RENTER) = "geometry"

# Create the map

# Colors by neighborhood

# Add row names for feature title

mapview(ACS_BR_2021_Med_Inc, zcol = "Neighborhood.x")

mapview(ACS_BR_2021_Med_Inc, zcol = "estimate")

mapview(ACS_BR_EA_END_RENTER, zcol = "Percent_Of_Population")

# Census variable, then geography

# Show me all Harris County, Show me by neighborhood, Show me specific Baker-Ripley neighborhood


# Make this column a percentage
ACS_BR_PAS_2021_Med_Inc = unique(ACS_BR_PAS_2021_Med_Inc)
View(ACS_BR_PAS_2021_Med_Inc)
row.names(ACS_BR_PAS_2021_Med_Inc) = ACS_BR_PAS_2021_Med_Inc$Tract
View(ACS_BR_PAS_2021_Med_Inc)
mapview(ACS_BR_PAS_2021_Med_Inc, zcol = "estimate")

mapview(ACS_BR_EA_END_2021_Med_Inc, zcol = "estimate")

mapview(ACS_BR_PAS_2021_Total_Pop, zcol = "estimate")


# Median Income: All Neighborhoods
ACS_BR_2021_Med_Inc %>%
  ggplot(aes(fill = estimate))+
  labs(fill="Estimated Median Family Income ($)")+
  geom_sf(color = NA)+
  ggtitle("BakerRipley Neighborhoods by Median Family Income")+
  scale_fill_viridis_c(option = "magma")

# Median Income: Pasadena
ACS_BR_PAS_2021_Med_Inc %>%
  ggplot(aes(fill = estimate))+
  labs(fill="Estimated Median Family Income ($)")+
  geom_sf(color = NA)+
  ggtitle("Pasadena by Median Family Income")+
  scale_fill_viridis_c(option = "magma")

# Total Population
ACS_BR_2021_Total_Pop %>%
  ggplot(aes(fill = estimate))+
  labs(fill="Estimated Total Population")+
  geom_sf(color = NA)+ 
  ggtitle("BakerRipley Neighborhoods by Total Population")+
  scale_fill_viridis_c(option = "magma")

