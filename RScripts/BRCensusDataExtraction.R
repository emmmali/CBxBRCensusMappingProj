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
# View the tract list
View(BR_tracts)

# Get Pasadena census tracts
BR_tracts_pasadena = subset(BR_tracts, Neighborhood == 'Pasadena')
# View the tract list
View(BR_tracts_pasadena)

# Get Gulfton/Sharpstown Census Tracts
BR_tracts_gulfton_sharpstown = subset(BR_tracts, Neighborhood == 'Gulfton/Sharpstown')
# View the tract list
View(BR_tracts_gulfton_sharpstown)

# Get Hobby/Harbach Census Tracts
BR_tracts_hobby_harbach = subset(BR_tracts, Neighborhood == 'Hobby/Harbach')
# View the tract list
View(BR_tracts_hobby_harbach)

# Get East Aldine Census Tracts
BR_tracts_east_aldine = subset(BR_tracts, Neighborhood == 'EastAldine')
# View the tract list
View(BR_tracts_east_aldine)

# Get East End Census Tracts
BR_tracts_east_end = subset(BR_tracts, Neighborhood == 'East End')
# View the tract list
View(BR_tracts_east_end)

### Read in the desired Census Data Variables ###
ACS_Vars_21_With_BR = read.csv("ACSVariables2021CondensedModVars.csv")
View(ACS_Vars_21_With_BR)

ACS_Vars_21 = subset(ACS_Vars_21_With_BR, ACS_Label != 'BakerRipley Calculation')
View(ACS_Vars_21)

### Get ACS data ###

# Create a dataframe for the county ACS data at census-tract level, showing the corresponding geometries
ACS_Harris_2021 = get_acs(
  geography = "tract", #Display tract-level geographies
  state = "TX", #Select state
  county = "Harris County", #Select county
  variables = ACS_Vars_21$Var_2021, # Show read in desired variables
  geometry = TRUE, #Show geometric information
  year = 2021 #Desired end year for 5-yr ACS range
)

# View county ACS data
View(ACS_Harris_2021)

# Merge county-level with variable descriptions
ACS_Harris_Vars_2021 = merge(x = ACS_Vars_21, y = ACS_Harris_2021, by.x = "Var_2021", by.y = "variable")
View(ACS_Harris_Vars_2021)

# Make a dataframe with var id, tract, demo category, var name, neighborhood, estimate for calculations
ACS_Vars_Calc_Harris = 
  subset(ACS_Harris_Vars_2021, select = -c(geometry))

write.csv(ACS_Vars_Calc_Harris, "ACS_Vars_HARRIS_2021.csv", row.names = FALSE)

new_df = read.csv("ACS_Vars_HARRIS_2021.csv")
View(new_df)

### Merge BR tract list and county ACS data by GEOID to get data specific to BakerRipley census tracts ###

# All neighborhoods (all tracts)

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

# Make a dataframe with var id, tract, demo category, var name, neighborhood, estimate for calculations
ACS_Vars_Calc = 
  subset(ACS_BR_2021, select=c(Neighborhood, Tract, Demographic_Category, Var_2021, Variable_Name, estimate))

ACS_Vars_Calc = unique(ACS_Vars_Calc)

View(ACS_Vars_Calc)

# Create Percent Column (initially NA)

ACS_Vars_Calc['Percent_Of_Population'] = NA
View(ACS_Vars_Calc)

# Create Total Population Column (initially NA)
ACS_Vars_Calc['Total_Population'] = NA
View(ACS_Vars_Calc)

### Perform Baker-Ripley Calculations ###

# Subset into categories & perform calculations for those categories individually

### Categories: Total Population, Age, Gender, Race ###

### Get the subsetted dataframe ###
ACS_Vars_Age_Sex_Race = subset(ACS_Vars_Calc, Demographic_Category %in% c('Total Population', 'Age', 'Gender', 'Race / Ethnicity'))

View(ACS_Vars_Age_Sex_Race)

### Add BakerRipley-Calculated Variables ###

### Add in "Working Age Population (20-59 years old)" ###
for (tract in BR_tracts$Tract){
  
  working_age_set = ACS_Vars_Age_Sex_Race[ACS_Vars_Age_Sex_Race$Tract == tract &
                                            ACS_Vars_Age_Sex_Race$Variable_Name %in% 
                                            c('20 to 24 years',
                                              '25 to 34 years',
                                              '35 to 44 years',
                                              '45 to 54 years',
                                              '55 to 59 years'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Age'
  
  var_2021 = NA
  
  working_age_est = sum(working_age_set$estimate)
  
  ACS_Vars_Age_Sex_Race = ACS_Vars_Age_Sex_Race %>% 
    add_row(Variable_Name = "Working Age Population (20-59 years old)", estimate = working_age_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Age_Sex_Race = unique(ACS_Vars_Age_Sex_Race)

# Check if the newly added variable was actually added
working_age_pop = 
  subset(ACS_Vars_Age_Sex_Race, Variable_Name == "Working Age Population (20-59 years old)")

# Check if the total makes sense
View(sum(working_age_pop$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Age_Sex_Race[ACS_Vars_Age_Sex_Race$Tract == tract,]$Total_Population = 
    ACS_Vars_Age_Sex_Race[ACS_Vars_Age_Sex_Race$Tract == tract
                          &ACS_Vars_Age_Sex_Race$Variable_Name == 
                            'Total Population: General',]$estimate
  
}

View(ACS_Vars_Age_Sex_Race)

### Get the percentages ###
ACS_Vars_Age_Sex_Race$Percent_Of_Population = ACS_Vars_Age_Sex_Race$estimate /
  ACS_Vars_Age_Sex_Race$Total_Population

View(ACS_Vars_Age_Sex_Race)

### Round + multiply percent by 100 ###

ACS_Vars_Age_Sex_Race$Percent_Of_Population = 
  round(ACS_Vars_Age_Sex_Race$Percent_Of_Population * 100, 2)

View(ACS_Vars_Age_Sex_Race)

# Check to see if the percentages make sense 
View(subset(ACS_Vars_Age_Sex_Race, Variable_Name == "Working Age Population (20-59 years old)"))

# Check the total population
total_pop = subset(ACS_Vars_Age_Sex_Race, Variable_Name == 'Total Population: General')
View(total_pop)

total_pop = unique(total_pop)
View(total_pop)

View(sum(total_pop$estimate))


### Repeat that process for each category ###

### Category: Household Income & Benefits ###

### Get the subsetted dataframe ###
ACS_Vars_Inc_Benefits = subset(ACS_Vars_Calc, 
                               Demographic_Category %in% c('Household Income and Benefits'))

View(ACS_Vars_Inc_Benefits)

ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)

View(ACS_Vars_Inc_Benefits)

### Add BakerRipley-Calculated Variables ###

### Add in "Subtotal less than $25,000 income" ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Subtotal less than $25,000 income"
  
  new_subset = ACS_Vars_Inc_Benefits[ACS_Vars_Inc_Benefits$Tract == tract &
                                       ACS_Vars_Inc_Benefits$Variable_Name %in% 
                                            c('Less than $10,000',
                                              '$10,000 to $14,999',
                                              '$15,000 to $24,999'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Household Income and Benefits'
  
  var_2021 = NA
  
  new_est = sum(new_subset$estimate)
  
  ACS_Vars_Inc_Benefits = ACS_Vars_Inc_Benefits %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

# Check if the newly added variable was actually added

### Make sure to call unique to get only unique values ###
ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)

less_than_25000 = subset(ACS_Vars_Inc_Benefits, Variable_Name == "Subtotal less than $25,000 income")
View(less_than_25000)

# Check if the total amount is reasonable

View(sum(less_than_25000$estimate))


### Add in "Subtotal less than $50,000 income" ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Subtotal less than $50,000 income"
  
  new_subset = ACS_Vars_Inc_Benefits[ACS_Vars_Inc_Benefits$Tract == tract &
                                       ACS_Vars_Inc_Benefits$Variable_Name %in% 
                                       c('Less than $10,000',
                                         '$10,000 to $14,999',
                                         '$15,000 to $24,999',
                                         '$25,000 to $34,999',
                                         '$35,000 to $49,999'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Household Income and Benefits'
  
  var_2021 = NA
  
  new_est = sum(new_subset$estimate)
  
  ACS_Vars_Inc_Benefits = ACS_Vars_Inc_Benefits %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}
### Make sure to call unique to get only unique values ###

ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)

# Check if the newly added variable was actually added

less_than_50000 = subset(ACS_Vars_Inc_Benefits, Variable_Name == "Subtotal less than $50,000 income")
View(less_than_50000)

# Check if the total amount is reasonable

View(sum(less_than_50000$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Inc_Benefits[ACS_Vars_Inc_Benefits$Tract == tract,]$Total_Population = 
    ACS_Vars_Inc_Benefits[ACS_Vars_Inc_Benefits$Tract == tract
                          &ACS_Vars_Inc_Benefits$Variable_Name == 
                            'Total households',]$estimate
  
}
# Make unique
ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)
View(ACS_Vars_Inc_Benefits)

### Get the percentages ###
ACS_Vars_Inc_Benefits$Percent_Of_Population = ACS_Vars_Inc_Benefits$estimate /
  ACS_Vars_Inc_Benefits$Total_Population

ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)

View(ACS_Vars_Inc_Benefits)

# Check to see if the percentages make sense 
ACS_Vars_Inc_Benefits = unique(ACS_Vars_Inc_Benefits)
View(subset(ACS_Vars_Inc_Benefits, Variable_Name == "Subtotal less than $50,000 income"))

### Round + multiply percent by 100 ###

ACS_Vars_Inc_Benefits$Percent_Of_Population = 
  round(ACS_Vars_Inc_Benefits$Percent_Of_Population * 100, 2)

View(ACS_Vars_Inc_Benefits)

### Check # of total households ###
total_households = subset(ACS_Vars_Inc_Benefits, Variable_Name == 'Total households')
View(total_households)

total_households = unique(total_households)
View(total_households)

View(sum(total_households$estimate))


### TESTING: Income ###

# Merge and map the income variables for testing
ACS_BR_Inc_Benefits_With_Geo = merge(x = ACS_Vars_Inc_Benefits, y = ACS_GEO_INFO, by.x = "Tract", by.y = "Tract")
View(ACS_BR_Inc_Benefits_With_Geo)

ACS_BR_Inc_Benefits_Under_50000 = subset(ACS_BR_Inc_Benefits_With_Geo, Variable_Name == "Subtotal less than $50,000 income")

View(ACS_BR_Inc_Benefits_Under_50000)

st_geometry(ACS_BR_Inc_Benefits_Under_50000) = "geometry"

mapview(ACS_BR_Inc_Benefits_Under_50000, zcol = "Percent_Of_Population")

ACS_BR_Inc_Total_Households = subset(ACS_BR_Inc_Benefits_With_Geo, Variable_Name == "Total households")
View(ACS_BR_Inc_Total_Households)

st_geometry(ACS_BR_Inc_Total_Households) = "geometry"

mapview(ACS_BR_Inc_Total_Households, zcol = "estimate")


### END OF TESTING: Income ###

### Category: Poverty ###

### Get the subsetted dataframe ###
ACS_Vars_Poverty = subset(ACS_Vars_Calc, 
                               Demographic_Category %in% c('Poverty'))

View(ACS_Vars_Poverty)

ACS_Vars_Poverty = unique(ACS_Vars_Poverty)

View(ACS_Vars_Poverty)

### Add in variable - Over 200% FPL ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Over 200% FPL"
  
  total_pop_poverty = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                  ACS_Vars_Poverty$Variable_Name %in% 
                                       c('Total Population: Poverty'), ]
  
  below_200 = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                 ACS_Vars_Poverty$Variable_Name %in% 
                                 c('Below 200 % FPL'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Poverty'
  
  var_2021 = NA
  
  new_est = sum(total_pop_poverty$estimate) - sum(below_200$estimate)
  
  ACS_Vars_Poverty = ACS_Vars_Poverty %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}
### Make sure to call unique to get only unique values ###

ACS_Vars_Poverty = unique(ACS_Vars_Poverty)

# Check if the newly added variable was actually added

over_200_fpl = subset(ACS_Vars_Poverty, Variable_Name == "Over 200% FPL")
View(over_200_fpl)

# Check if the total amount is reasonable

View(sum(over_200_fpl$estimate))


### Add in variable - 100% - 200% FPL ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "100% - 200% FPL"
  
  below_200 = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                 ACS_Vars_Poverty$Variable_Name %in% 
                                 c('Below 200 % FPL'), ]
  
  below_fpl = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                         ACS_Vars_Poverty$Variable_Name %in% 
                                         c('Below Poverty Level'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Poverty'
  
  var_2021 = NA
  
  new_est = sum(below_200$estimate) - sum(below_fpl$estimate)
  
  ACS_Vars_Poverty = ACS_Vars_Poverty %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}
### Make sure to call unique to get only unique values ###

ACS_Vars_Poverty = unique(ACS_Vars_Poverty)

# Check if the newly added variable was actually added

fpl_100_to_200 = subset(ACS_Vars_Poverty, Variable_Name == "100% - 200% FPL")
View(fpl_100_to_200)

# Check if the total amount is reasonable

View(sum(fpl_100_to_200$estimate))


### Add in variable - 50% - 100% FPL ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "50% - 100% FPL"
  
  below_fpl = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                 ACS_Vars_Poverty$Variable_Name %in% 
                                 c('Below Poverty Level'), ]
  
  below_50 = ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract &
                                 ACS_Vars_Poverty$Variable_Name %in% 
                                 c('Below 50% FPL'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Poverty'
  
  var_2021 = NA
  
  new_est = sum(below_fpl$estimate) - sum(below_50$estimate)
  
  ACS_Vars_Poverty = ACS_Vars_Poverty %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}
### Make sure to call unique to get only unique values ###

ACS_Vars_Poverty = unique(ACS_Vars_Poverty)

# Check if the newly added variable was actually added

fpl_50_to_100 = subset(ACS_Vars_Poverty, Variable_Name == "50% - 100% FPL")
View(fpl_50_to_100)

# Check if the total amount is reasonable

View(sum(fpl_50_to_100$estimate))


### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract,]$Total_Population = 
    ACS_Vars_Poverty[ACS_Vars_Poverty$Tract == tract
                          &ACS_Vars_Poverty$Variable_Name == 
                            'Total Population: Poverty',]$estimate
  
}
# Make unique
ACS_Vars_Poverty = unique(ACS_Vars_Poverty)
View(ACS_Vars_Poverty)

### Get the percentages ###
ACS_Vars_Poverty$Percent_Of_Population = ACS_Vars_Poverty$estimate /
  ACS_Vars_Poverty$Total_Population

ACS_Vars_Poverty = unique(ACS_Vars_Poverty)

View(ACS_Vars_Poverty)

### Round + multiply percent by 100 ###

ACS_Vars_Poverty$Percent_Of_Population = round(ACS_Vars_Poverty$Percent_Of_Population * 100, 2)

View(ACS_Vars_Poverty)

# Check to see if the percentages make sense 
ACS_Vars_Poverty = unique(ACS_Vars_Poverty)
View(subset(ACS_Vars_Poverty, Variable_Name == "50% - 100% FPL"))

### Check # of total surveyed for poverty ###
total_poverty = subset(ACS_Vars_Poverty, Variable_Name == 'Total Population: Poverty')
View(total_poverty)

total_poverty = unique(total_poverty)
View(total_poverty)

View(sum(total_poverty$estimate))


### Category: Unemployment ###

### Get the subsetted dataframe ###
ACS_Vars_Unemployment = subset(ACS_Vars_Calc, 
                          Demographic_Category %in% c('Unemployment'))

View(ACS_Vars_Unemployment)

ACS_Vars_Unemployment = unique(ACS_Vars_Unemployment)

View(ACS_Vars_Unemployment)


### Add in variable - # Unemployed - ACS ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "# Unemployed - ACS"
  
  total_pop_unempl = ACS_Vars_Unemployment[ACS_Vars_Unemployment$Tract == tract &
                                ACS_Vars_Unemployment$Variable_Name %in% 
                                 c('Population 16 years and over'), ]
  
  unempl_rate = ACS_Vars_Unemployment[ACS_Vars_Unemployment$Tract == tract &
                                ACS_Vars_Unemployment$Variable_Name %in% 
                                c('Unemployment Rate'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Unemployment'
  
  var_2021 = NA
  
  new_est = round(((total_pop_unempl$estimate*unempl_rate$estimate)/100), 0)
  
  ACS_Vars_Unemployment = ACS_Vars_Unemployment %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Unemployment = unique(ACS_Vars_Unemployment)

# Check if the newly added variable was actually added

num_unempl = subset(ACS_Vars_Unemployment, Variable_Name == "# Unemployed - ACS")
View(num_unempl)

num_unempl = num_unempl[!(is.na(num_unempl$estimate)), ]

# Check if the total amount is reasonable

View(sum(num_unempl$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Unemployment[ACS_Vars_Unemployment$Tract == tract,]$Total_Population = 
    ACS_Vars_Unemployment[ACS_Vars_Unemployment$Tract == tract
                     &ACS_Vars_Unemployment$Variable_Name == 
                       'Population 16 years and over',]$estimate
  
}
# Make unique
ACS_Vars_Unemployment = unique(ACS_Vars_Unemployment)
View(ACS_Vars_Unemployment)

### Get the percentages ###
ACS_Vars_Unemployment$Percent_Of_Population = ACS_Vars_Unemployment$estimate /
  ACS_Vars_Unemployment$Total_Population

ACS_Vars_Unemployment = unique(ACS_Vars_Unemployment)

View(ACS_Vars_Unemployment)

### Round + multiply percent by 100 ###

ACS_Vars_Unemployment$Percent_Of_Population = round(ACS_Vars_Unemployment$Percent_Of_Population * 100, 2)

View(ACS_Vars_Unemployment)

# Check to see if the percentages make sense 
ACS_Vars_Unemployment = unique(ACS_Vars_Unemployment)
View(subset(ACS_Vars_Unemployment, Variable_Name == "Unemployment Rate"))
summary(subset(ACS_Vars_Unemployment, Variable_Name == "Unemployment Rate")$estimate)

### Check # of total surveyed for unemployment ###
total_unempl = subset(ACS_Vars_Unemployment, Variable_Name == 'Population 16 years and over')

total_unempl = unique(total_unempl)
View(total_unempl)

View(sum(total_unempl$estimate))


### Category: Educational Attainment ###

### Get the subsetted dataframe ###
ACS_Vars_Edu = subset(ACS_Vars_Calc, 
                               Demographic_Category %in% c('Educational Attainment'))

View(ACS_Vars_Edu)

ACS_Vars_Edu = unique(ACS_Vars_Edu)

View(ACS_Vars_Edu)


### Add in variable - Subtotal - Less than highschool ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Subtotal - Less than highschool"
  
  less_than_hs = ACS_Vars_Edu[ACS_Vars_Edu$Tract == tract &
                                ACS_Vars_Edu$Variable_Name %in% 
                                             c('Less than 9th grade',
                                              '9th to 12th grade, no diploma'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Educational Attainment'
  
  var_2021 = NA
  
  new_est = sum(less_than_hs$estimate)
  
  ACS_Vars_Edu = ACS_Vars_Edu %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Edu = unique(ACS_Vars_Edu)

# Check if the newly added variable was actually added

less_than_hs = subset(ACS_Vars_Edu, Variable_Name == "Subtotal - Less than highschool")
View(less_than_hs)

less_than_hs = less_than_hs[!(is.na(less_than_hs$estimate)), ]

# Check if the total amount is reasonable

View(sum(less_than_hs$estimate))



### Add in variable - Subtotal - No college degree ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Subtotal - No college degree"
  
  no_college = ACS_Vars_Edu[ACS_Vars_Edu$Tract == tract &
                                ACS_Vars_Edu$Variable_Name %in% 
                                c('Less than 9th grade',
                                  '9th to 12th grade, no diploma',
                                  'High school graduate (includes equivalency)',
                                  'Some college, no degree'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Educational Attainment'
  
  var_2021 = NA
  
  new_est = sum(no_college$estimate)
  
  ACS_Vars_Edu = ACS_Vars_Edu %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Edu = unique(ACS_Vars_Edu)

# Check if the newly added variable was actually added

no_college = subset(ACS_Vars_Edu, Variable_Name == "Subtotal - No college degree")
View(no_college)

# Check if the total amount is reasonable

View(sum(no_college$estimate))


### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Edu[ACS_Vars_Edu$Tract == tract,]$Total_Population = 
    ACS_Vars_Edu[ACS_Vars_Edu$Tract == tract
                          &ACS_Vars_Edu$Variable_Name == 
                            'Population 25 years old and over',]$estimate
  
}
# Make unique
ACS_Vars_Edu = unique(ACS_Vars_Edu)
View(ACS_Vars_Edu)

### Get the percentages ###
ACS_Vars_Edu$Percent_Of_Population = ACS_Vars_Edu$estimate /
  ACS_Vars_Edu$Total_Population

ACS_Vars_Edu = unique(ACS_Vars_Edu)

View(ACS_Vars_Edu)

### Round + multiply percent by 100 ###

ACS_Vars_Edu$Percent_Of_Population = round(ACS_Vars_Edu$Percent_Of_Population * 100, 2)

View(ACS_Vars_Edu)

# Check to see if the percentages make sense 
ACS_Vars_Edu = unique(ACS_Vars_Edu)
View(subset(ACS_Vars_Edu, Variable_Name == 'Some college, no degree'))
sum(subset(ACS_Vars_Edu, Variable_Name == 'Some college, no degree')$estimate)
summary(subset(ACS_Vars_Edu, Variable_Name == 'Some college, no degree')$estimate)

### Check # of total surveyed for educational attainment ###
total_edu_attainment = subset(ACS_Vars_Edu, Variable_Name == 'Population 25 years old and over')
total_edu_attainment = unique(total_edu_attainment)
View(total_edu_attainment)

View(sum(total_edu_attainment$estimate))


### Category: Opportunity Youth ###

### Get the subsetted dataframe ###
ACS_Vars_Opp_Youth = subset(ACS_Vars_Calc, 
                      Demographic_Category %in% c('Opportunity Youth'))

View(ACS_Vars_Opp_Youth)

ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)

View(ACS_Vars_Opp_Youth)


### Add in variable - Youth not in school, unemployed ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Youth not in school, unemployed"
  
  youth_no_school_unempl = ACS_Vars_Opp_Youth[ACS_Vars_Opp_Youth$Tract == tract &
                              ACS_Vars_Opp_Youth$Variable_Name %in% 
                              c('Male not enrolled, high school graduate, unemployed',
                                'Male not enrolled, not high school graduate, unemployed',
                                'Female not enrolled, high school graduate, unemployed',
                                'Female not enrolled, not high school, graduate unemployed'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Opportunity Youth'
  
  var_2021 = NA
  
  new_est = sum(youth_no_school_unempl$estimate)
  
  ACS_Vars_Opp_Youth = ACS_Vars_Opp_Youth %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)


# Check if the newly added variable was actually added

youth_no_school_unempl = subset(ACS_Vars_Opp_Youth, Variable_Name == "Youth not in school, unemployed")
View(youth_no_school_unempl)

# Check if the total amount is reasonable

View(sum(youth_no_school_unempl$estimate))

### Add in variable - Youth not in school, unemployed, graduated high school ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Youth not in school, unemployed, graduated high school"
  
  youth_unempl_hs_grad = ACS_Vars_Opp_Youth[ACS_Vars_Opp_Youth$Tract == tract &
                                          ACS_Vars_Opp_Youth$Variable_Name %in% 
                                   c('Male not enrolled, high school graduate, unemployed',
                                     'Female not enrolled, high school graduate, unemployed'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Opportunity Youth'
  
  var_2021 = NA
  
  new_est = sum(youth_unempl_hs_grad$estimate)
  
  ACS_Vars_Opp_Youth = ACS_Vars_Opp_Youth %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)


# Check if the newly added variable was actually added

youth_unempl_hs_grad = subset(ACS_Vars_Opp_Youth, Variable_Name == "Youth not in school, unemployed, graduated high school")
View(youth_unempl_hs_grad)

# Check if the total amount is reasonable
View(sum(youth_unempl_hs_grad$estimate))


### Add in variable - Youth not in school, unemployed, did not graduate high school ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Youth not in school, unemployed, did not graduate high school"
  
  youth_unempl_no_hs_grad = ACS_Vars_Opp_Youth[ACS_Vars_Opp_Youth$Tract == tract &
                                                ACS_Vars_Opp_Youth$Variable_Name %in% 
                                c('Male not enrolled, not high school graduate, unemployed',
                                  'Female not enrolled, not high school, graduate unemployed'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Opportunity Youth'
  
  var_2021 = NA
  
  new_est = sum(youth_unempl_no_hs_grad$estimate)
  
  ACS_Vars_Opp_Youth = ACS_Vars_Opp_Youth %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)


# Check if the newly added variable was actually added

youth_unempl_no_hs_grad = subset(ACS_Vars_Opp_Youth, Variable_Name == "Youth not in school, unemployed, did not graduate high school")
View(youth_unempl_no_hs_grad)

# Check if the total amount is reasonable

View(sum(youth_unempl_no_hs_grad$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Opp_Youth[ACS_Vars_Opp_Youth$Tract == tract,]$Total_Population = 
    ACS_Vars_Opp_Youth[ACS_Vars_Opp_Youth$Tract == tract
                 &ACS_Vars_Opp_Youth$Variable_Name == 
                   'Population 16-19 years old',]$estimate
  
}
# Make unique
ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)
View(ACS_Vars_Opp_Youth)

### Get the percentages ###
ACS_Vars_Opp_Youth$Percent_Of_Population = ACS_Vars_Opp_Youth$estimate /
  ACS_Vars_Opp_Youth$Total_Population

ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)

View(ACS_Vars_Opp_Youth)

### Round + multiply percent by 100 ###

ACS_Vars_Opp_Youth$Percent_Of_Population = round(ACS_Vars_Opp_Youth$Percent_Of_Population * 100, 2)

View(ACS_Vars_Opp_Youth)

# Check to see if the percentages make sense 
ACS_Vars_Opp_Youth = unique(ACS_Vars_Opp_Youth)

male_hsgrad_unempl = subset(ACS_Vars_Opp_Youth, Variable_Name == 
              'Male not enrolled, high school graduate, unemployed')

sum(male_hsgrad_unempl$estimate)

summary(male_hsgrad_unempl)

### Check # of total surveyed for opportunity youth ###
total_opp_youth = subset(ACS_Vars_Opp_Youth, Variable_Name == 'Population 16-19 years old')
total_opp_youth = unique(total_opp_youth)
View(total_opp_youth)

View(sum(total_opp_youth$estimate))


### Category: Place of Birth ###

### Get the subsetted dataframe ###
ACS_Vars_Birthplace = subset(ACS_Vars_Calc, 
                            Demographic_Category %in% c('Place of Birth'))

ACS_Vars_Birthplace = unique(ACS_Vars_Birthplace)

View(ACS_Vars_Birthplace)

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Birthplace[ACS_Vars_Birthplace$Tract == tract,]$Total_Population = 
    ACS_Vars_Birthplace[ACS_Vars_Birthplace$Tract == tract
                       &ACS_Vars_Birthplace$Variable_Name == 
                         'Total Population: Birthplace',]$estimate
  
}
# Make unique
ACS_Vars_Birthplace = unique(ACS_Vars_Birthplace)
View(ACS_Vars_Birthplace)

### Get the percentages ###
ACS_Vars_Birthplace$Percent_Of_Population = ACS_Vars_Birthplace$estimate /
  ACS_Vars_Birthplace$Total_Population

ACS_Vars_Birthplace = unique(ACS_Vars_Birthplace)

View(ACS_Vars_Birthplace)

### Round + multiply percent by 100 ###

ACS_Vars_Birthplace$Percent_Of_Population = round(ACS_Vars_Birthplace$Percent_Of_Population * 100, 2)

View(ACS_Vars_Birthplace)

# Check to see if the percentages make sense 
ACS_Vars_Birthplace = unique(ACS_Vars_Birthplace)

native_born = subset(ACS_Vars_Birthplace, Variable_Name == 
                              'Native')

sum(native_born$estimate)

summary(native_born$Percent_Of_Population)

### Check # of total surveyed for birthplace ###
total_birthplace = subset(ACS_Vars_Birthplace, Variable_Name == 'Total Population: Birthplace')
total_birthplace = unique(total_birthplace)
View(total_birthplace)

View(sum(total_birthplace$estimate))



### Categories: Language Spoken at Home + English Fluency ###

### Get the subsetted dataframe ###
ACS_Vars_Lang_Eng_Fluency = subset(ACS_Vars_Calc, 
                             Demographic_Category %in% 
                               c('Lanuage Spoken at Home',
                                 'English Fluency'))

ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)

View(ACS_Vars_Lang_Eng_Fluency)


### Add in variable - All Other Languages ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "All Other Languages"
  
  all_other_langs = ACS_Vars_Lang_Eng_Fluency[ACS_Vars_Lang_Eng_Fluency$Tract == tract &
                                                ACS_Vars_Lang_Eng_Fluency$Variable_Name %in% 
                                  c('Other Indo-European languages',
                                    'Asian and Pacific Islander languages',
                                    'Other languages'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Lanuage Spoken at Home'
  
  var_2021 = NA
  
  new_est = sum(all_other_langs$estimate)
  
  ACS_Vars_Lang_Eng_Fluency = ACS_Vars_Lang_Eng_Fluency %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)


# Check if the newly added variable was actually added

all_other_langs = subset(ACS_Vars_Lang_Eng_Fluency, Variable_Name == "All Other Languages")
View(all_other_langs)

# Check if the total amount is reasonable

View(sum(all_other_langs$estimate))


### Add in variable - English fluent but not primary ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "English fluent but not primary"
  
  primary_noneng = ACS_Vars_Lang_Eng_Fluency[ACS_Vars_Lang_Eng_Fluency$Tract == tract &
                                                ACS_Vars_Lang_Eng_Fluency$Variable_Name %in% 
                                                c('Primarily non-English '), ]
  
  eng_nonfluent = ACS_Vars_Lang_Eng_Fluency[ACS_Vars_Lang_Eng_Fluency$Tract == tract &
                                               ACS_Vars_Lang_Eng_Fluency$Variable_Name %in% 
                                c('English not fluent (Speak English less than "very well")'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'English Fluency'
  
  var_2021 = NA
  
  new_est = sum(primary_noneng$estimate) - sum(eng_nonfluent$estimate)
  
  ACS_Vars_Lang_Eng_Fluency = ACS_Vars_Lang_Eng_Fluency %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)


# Check if the newly added variable was actually added
eng_fluent_nonprimary = 
  subset(ACS_Vars_Lang_Eng_Fluency, Variable_Name == "English fluent but not primary")
View(eng_fluent_nonprimary)

# Check if the total amount is reasonable

View(sum(eng_fluent_nonprimary$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Lang_Eng_Fluency[ACS_Vars_Lang_Eng_Fluency$Tract == tract,]$Total_Population = 
    ACS_Vars_Lang_Eng_Fluency[ACS_Vars_Lang_Eng_Fluency$Tract == tract
                        &ACS_Vars_Lang_Eng_Fluency$Variable_Name == 
                          'Total Population: Language Spoken (5 years and over)',]$estimate
  
}
# Make unique
ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)
View(ACS_Vars_Lang_Eng_Fluency)

### Get the percentages ###
ACS_Vars_Lang_Eng_Fluency$Percent_Of_Population = ACS_Vars_Lang_Eng_Fluency$estimate /
  ACS_Vars_Lang_Eng_Fluency$Total_Population

ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)

View(ACS_Vars_Lang_Eng_Fluency)

### Round + multiply percent by 100 ###

ACS_Vars_Lang_Eng_Fluency$Percent_Of_Population = 
  round(ACS_Vars_Lang_Eng_Fluency$Percent_Of_Population * 100, 2)

View(ACS_Vars_Lang_Eng_Fluency)

# Check to see if the percentages make sense 
ACS_Vars_Lang_Eng_Fluency = unique(ACS_Vars_Lang_Eng_Fluency)

eng_primary = subset(ACS_Vars_Lang_Eng_Fluency, Variable_Name == 
                       'Primarily English')

sum(eng_primary$estimate)

summary(eng_primary$Percent_Of_Population)

### Check # of total surveyed for language spoken ###
total_lang_spoken = 
  subset(ACS_Vars_Lang_Eng_Fluency, 
         Variable_Name == 'Total Population: Language Spoken (5 years and over)')
total_lang_spoken = unique(total_lang_spoken)
View(total_lang_spoken)

View(sum(total_lang_spoken$estimate))


### Category: Health Insurance ###

### Get the subsetted dataframe ###
ACS_Vars_Health_Ins = subset(ACS_Vars_Calc, 
                                   Demographic_Category %in% 
                                     c('Health Insurance'))

ACS_Vars_Health_Ins = unique(ACS_Vars_Health_Ins)

View(ACS_Vars_Health_Ins)

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Health_Ins[ACS_Vars_Health_Ins$Tract == tract,]$Total_Population = 
    ACS_Vars_Health_Ins[ACS_Vars_Health_Ins$Tract == tract
                              &ACS_Vars_Health_Ins$Variable_Name == 
                                'Total population: Health Insurance',]$estimate
  
}
# Make unique
ACS_Vars_Health_Ins = unique(ACS_Vars_Health_Ins)
View(ACS_Vars_Health_Ins)

### Get the percentages ###
ACS_Vars_Health_Ins$Percent_Of_Population = ACS_Vars_Health_Ins$estimate /
  ACS_Vars_Health_Ins$Total_Population

ACS_Vars_Health_Ins = unique(ACS_Vars_Health_Ins)

View(ACS_Vars_Health_Ins)

### Round + multiply percent by 100 ###

ACS_Vars_Health_Ins$Percent_Of_Population = 
  round(ACS_Vars_Health_Ins$Percent_Of_Population * 100, 2)

View(ACS_Vars_Health_Ins)

# Check to see if the percentages make sense 
ACS_Vars_Health_Ins = unique(ACS_Vars_Health_Ins)

with_health_insurance = subset(ACS_Vars_Health_Ins, Variable_Name == 
                       'With health insurance coverage')

sum(with_health_insurance$estimate)

summary(with_health_insurance$Percent_Of_Population)

### Check # of total surveyed for language spoken ###
total_lang_spoken = 
  subset(ACS_Vars_Lang_Eng_Fluency, 
         Variable_Name == 'Total Population: Language Spoken (5 years and over)')
total_lang_spoken = unique(total_lang_spoken)
View(total_lang_spoken)

View(sum(total_lang_spoken$estimate))

View(ACS_Vars_Health_Ins)


### Category: Digital Access ###

### Get the subsetted dataframe ###
ACS_Vars_Dig_Access = subset(ACS_Vars_Calc, 
                             Demographic_Category %in% 
                               c('Digital Access'))

ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)

View(ACS_Vars_Dig_Access)

### Add in variable - No Device or Smartphone Only ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "No Device or Smartphone Only"
  
  no_dev_smartphone_only = ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract &
                                                 ACS_Vars_Dig_Access$Variable_Name %in% 
                                                c('Smartphone Only',
                                                  'No Device'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Digital Access'
  
  var_2021 = NA
  
  new_est = sum(no_dev_smartphone_only$estimate)
  
  ACS_Vars_Dig_Access = ACS_Vars_Dig_Access %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)


# Check if the newly added variable was actually added

no_dev_smartphone_only = 
  subset(ACS_Vars_Dig_Access, Variable_Name == "No Device or Smartphone Only")

View(no_dev_smartphone_only)

# Check if the total amount is reasonable

View(sum(no_dev_smartphone_only$estimate))



### Add in variable - Internet Subscription (other than cellular data) ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Internet Subscription (other than cellular data)"
  
  internet_sub = ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract &
                                 ACS_Vars_Dig_Access$Variable_Name %in% 
                                 c('Having Any Internet Subscription (including Cell Data)'), ]
  
  cellular_only = ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract &
                                        ACS_Vars_Dig_Access$Variable_Name %in% 
                                        c('Data Plan Only'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Digital Access'
  
  var_2021 = NA
  
  new_est = sum(internet_sub$estimate) - sum(cellular_only$estimate)
  
  ACS_Vars_Dig_Access = ACS_Vars_Dig_Access %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)


# Check if the newly added variable was actually added

internet_noncellular = 
  subset(ACS_Vars_Dig_Access, Variable_Name == "Internet Subscription (other than cellular data)")

View(internet_noncellular)

# Check if the total amount is reasonable

View(sum(internet_noncellular$estimate))



### Add in variable - No Internet Subscription (includes those only having cellular data) ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "No Internet Subscription (includes those only having cellular data)"
  
  no_internet_sub = ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract &
                                                 ACS_Vars_Dig_Access$Variable_Name %in% 
                                                 c('Data Plan Only',
                                                   'No Internet'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Digital Access'
  
  var_2021 = NA
  
  new_est = sum(no_internet_sub$estimate)
  
  ACS_Vars_Dig_Access = ACS_Vars_Dig_Access %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)


# Check if the newly added variable was actually added

no_internet_sub = 
  subset(ACS_Vars_Dig_Access, 
         Variable_Name == "No Internet Subscription (includes those only having cellular data)")

View(no_internet_sub)

# Check if the total amount is reasonable

View(sum(no_internet_sub$estimate))

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract,]$Total_Population = 
    ACS_Vars_Dig_Access[ACS_Vars_Dig_Access$Tract == tract
                        &ACS_Vars_Dig_Access$Variable_Name == 
                          'Total Households',]$estimate
  
}
# Make unique
ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)
View(ACS_Vars_Dig_Access)

### Get the percentages ###
ACS_Vars_Dig_Access$Percent_Of_Population = ACS_Vars_Dig_Access$estimate /
  ACS_Vars_Dig_Access$Total_Population

ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)

View(ACS_Vars_Dig_Access)

### Round + multiply percent by 100 ###

ACS_Vars_Dig_Access$Percent_Of_Population = 
  round(ACS_Vars_Dig_Access$Percent_Of_Population * 100, 2)

View(ACS_Vars_Dig_Access)

# Check to see if the percentages make sense 
ACS_Vars_Dig_Access = unique(ACS_Vars_Dig_Access)

desktop_or_laptop = subset(ACS_Vars_Dig_Access, Variable_Name == 
                                 'Desktop or Laptop')

sum(desktop_or_laptop$estimate)

summary(desktop_or_laptop$Percent_Of_Population)

### Check # of total surveyed for language spoken ###
total_households = 
  subset(ACS_Vars_Dig_Access, 
         Variable_Name == 'Total Households')
total_households = unique(total_households)
View(total_households)

View(sum(total_households$estimate))

View(ACS_Vars_Dig_Access)



### Category: Family Size ###

### Get the subsetted dataframe ###
ACS_Vars_Family_Size = subset(ACS_Vars_Calc, 
                             Demographic_Category %in% 
                               c('Family Size'))

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)

View(ACS_Vars_Family_Size)


### Add in variable - Total: 2-person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 2-person households"
  
  total_2_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                          c('Family households: 2-person',
                                            'Nonfamily households: 2-person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_2_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_2_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 2-person households")

View(total_2_person)

# Check if the total amount is reasonable

View(sum(total_2_person$estimate))


### Add in variable - Total: 3-person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 3-person households"
  
  total_3_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                          c('Family households: 3-person',
                                            'Nonfamily households: 3-person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_3_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_3_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 3-person households")

View(total_3_person)

# Check if the total amount is reasonable

View(sum(total_3_person$estimate))


### Add in variable - Total: 4-person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 4-person households"
  
  total_4_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                          c('Family households: 4-person',
                                            'Nonfamily households: 4-person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_4_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_4_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 4-person households")

View(total_4_person)

# Check if the total amount is reasonable

View(sum(total_4_person$estimate))


### Add in variable - Total: 5-person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 5-person households"
  
  total_5_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                          c('Family households: 5-person',
                                            'Nonfamily households: 5-person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_5_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_5_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 5-person households")

View(total_5_person)

# Check if the total amount is reasonable

View(sum(total_5_person$estimate))



### Add in variable - Total: 6-person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 6-person households"
  
  total_6_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                          c('Family households: 6-person',
                                            'Nonfamily households: 6-person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_6_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_6_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 6-person households")

View(total_6_person)

# Check if the total amount is reasonable

View(sum(total_6_person$estimate))



### Add in variable - Total: 7-or-more person households ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Total: 7-or-more person households"
  
  total_7_more_person = ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract &
                                          ACS_Vars_Family_Size$Variable_Name %in% 
                                         c('Family households: 7-or-more person',
                                           'Nonfamily households: 7-or-more person'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Family Size'
  
  var_2021 = NA
  
  new_est = sum(total_7_more_person$estimate)
  
  ACS_Vars_Family_Size = ACS_Vars_Family_Size %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)


# Check if the newly added variable was actually added

total_7_more_person = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == "Total: 7-or-more person households")

View(total_7_more_person)

# Check if the total amount is reasonable

View(sum(total_7_more_person$estimate))


### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract,]$Total_Population = 
    ACS_Vars_Family_Size[ACS_Vars_Family_Size$Tract == tract
                        &ACS_Vars_Family_Size$Variable_Name == 
                          'Total Occupied Housing Units',]$estimate
  
}
# Make unique
ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)
View(ACS_Vars_Family_Size)

### Get the percentages ###
ACS_Vars_Family_Size$Percent_Of_Population = ACS_Vars_Family_Size$estimate /
  ACS_Vars_Family_Size$Total_Population

ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)

View(ACS_Vars_Family_Size)

### Round + multiply percent by 100 ###

ACS_Vars_Family_Size$Percent_Of_Population = 
  round(ACS_Vars_Family_Size$Percent_Of_Population * 100, 2)

View(ACS_Vars_Family_Size)

# Check to see if the percentages make sense 
ACS_Vars_Family_Size = unique(ACS_Vars_Family_Size)

one_or_more_18_under = subset(ACS_Vars_Family_Size, Variable_Name == 
                             'Households with one or more people under 18 years old')

View(one_or_more_18_under)

sum(one_or_more_18_under$estimate)

summary(one_or_more_18_under$Percent_Of_Population)

### Check # of total surveyed for family size ###
total_family_size = 
  subset(ACS_Vars_Family_Size, 
         Variable_Name == 'Total Occupied Housing Units')
total_family_size = unique(total_family_size)
View(total_family_size)

View(sum(total_family_size$estimate))

View(ACS_Vars_Family_Size)


### Category: Housing Occupancy ###

### Get the subsetted dataframe ###
ACS_Vars_Housing_Occupancy = subset(ACS_Vars_Calc, 
                                 Demographic_Category %in% 
                                   c('Housing Occupancy'))

ACS_Vars_Housing_Occupancy = unique(ACS_Vars_Housing_Occupancy)

View(ACS_Vars_Housing_Occupancy)

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Housing_Occupancy[ACS_Vars_Housing_Occupancy$Tract == tract,]$Total_Population = 
    ACS_Vars_Housing_Occupancy[ACS_Vars_Housing_Occupancy$Tract == tract
                            &ACS_Vars_Housing_Occupancy$Variable_Name == 
                              'Total Housing Units',]$estimate
  
}

# Make unique
ACS_Vars_Housing_Occupancy = unique(ACS_Vars_Housing_Occupancy)
View(ACS_Vars_Housing_Occupancy)

### Get the percentages ###
ACS_Vars_Housing_Occupancy$Percent_Of_Population = ACS_Vars_Housing_Occupancy$estimate /
  ACS_Vars_Housing_Occupancy$Total_Population

ACS_Vars_Housing_Occupancy = unique(ACS_Vars_Housing_Occupancy)

View(ACS_Vars_Housing_Occupancy)

### Round + multiply percent by 100 ###

ACS_Vars_Housing_Occupancy$Percent_Of_Population = 
  round(ACS_Vars_Housing_Occupancy$Percent_Of_Population * 100, 2)

View(ACS_Vars_Housing_Occupancy)

# Check to see if the percentages make sense 
ACS_Vars_Housing_Occupancy = unique(ACS_Vars_Housing_Occupancy)

renter_occupied = subset(ACS_Vars_Housing_Occupancy, Variable_Name == 
                              'Renter-occupied housing units')

sum(renter_occupied$estimate)

summary(renter_occupied$Percent_Of_Population)

### Check # of total surveyed for housing tenure ###
total_housing_units = 
  subset(ACS_Vars_Housing_Occupancy, 
         Variable_Name == 'Total Housing Units')
total_housing_units = unique(total_housing_units)
View(total_housing_units)

View(sum(total_housing_units$estimate))

View(ACS_Vars_Housing_Occupancy)



### Category: Housing Tenure ###

### Get the subsetted dataframe ###
ACS_Vars_Housing_Tenure = subset(ACS_Vars_Calc, 
                             Demographic_Category %in% 
                               c('Housing Tenure'))

ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)

View(ACS_Vars_Housing_Tenure)

### Add in variable - Moved in 2015 or later ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Moved in 2015 or later"
  
  movedin_2015_later = ACS_Vars_Housing_Tenure[ACS_Vars_Housing_Tenure$Tract == tract &
                                                 ACS_Vars_Housing_Tenure$Variable_Name %in% 
                                              c('Moved in 2017 or later',
                                                'Moved in 2015-2016'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Housing Tenure'
  
  var_2021 = NA
  
  new_est = sum(movedin_2015_later$estimate)
  
  ACS_Vars_Housing_Tenure = ACS_Vars_Housing_Tenure %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)


# Check if the newly added variable was actually added

movedin_2015_later = 
  subset(ACS_Vars_Housing_Tenure, 
         Variable_Name == "Moved in 2015 or later")

View(movedin_2015_later)

# Check if the total amount is reasonable

View(sum(movedin_2015_later$estimate))



### Add in variable - Moved in 1999 or earlier ###
for (tract in BR_tracts$Tract){
  
  new_var_name = "Moved in 1999 or earlier"
  
  movedin_1999_earlier = ACS_Vars_Housing_Tenure[ACS_Vars_Housing_Tenure$Tract == tract &
                                                 ACS_Vars_Housing_Tenure$Variable_Name %in% 
                                                 c('Moved in 1990-1999',
                                                   'Moved in 1989 and earlier'), ]
  
  neighborhood = subset(BR_tracts, Tract == tract)$Neighborhood
  
  demo_category = 'Housing Tenure'
  
  var_2021 = NA
  
  new_est = sum(movedin_1999_earlier$estimate)
  
  ACS_Vars_Housing_Tenure = ACS_Vars_Housing_Tenure %>% 
    add_row(Variable_Name = new_var_name, estimate = new_est, 
            Tract = tract, Neighborhood = neighborhood, Demographic_Category = demo_category, 
            Var_2021 = var_2021, Total_Population = NA, Percent_Of_Population = NA)
  
}

### Make sure to call unique to get only unique values ###

ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)


# Check if the newly added variable was actually added

movedin_1999_earlier = 
  subset(ACS_Vars_Housing_Tenure, 
         Variable_Name == "Moved in 1999 or earlier")

View(movedin_1999_earlier)

# Check if the total amount is reasonable

View(sum(movedin_1999_earlier$estimate))


### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Housing_Tenure[ACS_Vars_Housing_Tenure$Tract == tract,]$Total_Population = 
    ACS_Vars_Housing_Tenure[ACS_Vars_Housing_Tenure$Tract == tract
                        &ACS_Vars_Housing_Tenure$Variable_Name == 
                          'Total Occupied housing units',]$estimate
  
}
# Make unique
ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)
View(ACS_Vars_Housing_Tenure)

### Get the percentages ###
ACS_Vars_Housing_Tenure$Percent_Of_Population = ACS_Vars_Housing_Tenure$estimate /
  ACS_Vars_Housing_Tenure$Total_Population

ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)

View(ACS_Vars_Housing_Tenure)

### Round + multiply percent by 100 ###

ACS_Vars_Housing_Tenure$Percent_Of_Population = 
  round(ACS_Vars_Housing_Tenure$Percent_Of_Population * 100, 2)

View(ACS_Vars_Housing_Tenure)

# Check to see if the percentages make sense 
ACS_Vars_Housing_Tenure = unique(ACS_Vars_Housing_Tenure)

movedin_2017_later = subset(ACS_Vars_Housing_Tenure, Variable_Name == 
                                 'Moved in 2017 or later')

sum(movedin_2017_later$estimate)

summary(movedin_2017_later$Percent_Of_Population)

### Check # of total surveyed for housing tenure ###
total_occupied_units = 
  subset(ACS_Vars_Housing_Tenure, 
         Variable_Name == 'Total Occupied housing units')
total_occupied_units = unique(total_occupied_units)
View(total_occupied_units)

View(sum(total_occupied_units$estimate))

View(ACS_Vars_Housing_Tenure)


### Category: Veterans ###

### Get the subsetted dataframe ###
ACS_Vars_Veterans = subset(ACS_Vars_Calc, 
                                    Demographic_Category %in% 
                                      c('Veterans'))

ACS_Vars_Veterans = unique(ACS_Vars_Veterans)

View(ACS_Vars_Veterans)

### Set the total population for each tract ###
for (tract in BR_tracts$Tract){
  
  ACS_Vars_Veterans[ACS_Vars_Veterans$Tract == tract,]$Total_Population = 
    ACS_Vars_Veterans[ACS_Vars_Veterans$Tract == tract
                               &ACS_Vars_Veterans$Variable_Name == 
                                 'Total population - Civilian 18 years and over',]$estimate
  
}

# Make unique
ACS_Vars_Veterans = unique(ACS_Vars_Veterans)
View(ACS_Vars_Veterans)

### Get the percentages ###
ACS_Vars_Veterans$Percent_Of_Population = ACS_Vars_Veterans$estimate /
  ACS_Vars_Veterans$Total_Population

ACS_Vars_Veterans = unique(ACS_Vars_Veterans)

View(ACS_Vars_Veterans)

### Round + multiply percent by 100 ###

ACS_Vars_Veterans$Percent_Of_Population = 
  round(ACS_Vars_Veterans$Percent_Of_Population * 100, 2)

View(ACS_Vars_Veterans)

# Check to see if the percentages make sense 
ACS_Vars_Veterans = unique(ACS_Vars_Veterans)

civilian_veterans = subset(ACS_Vars_Veterans, Variable_Name == 
                           'Civilian Veterans')

sum(civilian_veterans$estimate)

summary(civilian_veterans$Percent_Of_Population)

### Check # of total surveyed for housing tenure ###
total_over_18 = 
  subset(ACS_Vars_Veterans, 
         Variable_Name == 'Total population - Civilian 18 years and over')
total_over_18 = unique(total_over_18)
View(total_over_18)

View(sum(total_over_18$estimate))

View(ACS_Vars_Veterans)


BR_VARS_ALL_2021 = bind_rows(ACS_Vars_Age_Sex_Race, ACS_Vars_Inc_Benefits, ACS_Vars_Poverty, ACS_Vars_Unemployment, ACS_Vars_Edu, ACS_Vars_Opp_Youth, ACS_Vars_Birthplace, ACS_Vars_Lang_Eng_Fluency, ACS_Vars_Health_Ins, ACS_Vars_Dig_Access, ACS_Vars_Family_Size, ACS_Vars_Housing_Occupancy, ACS_Vars_Housing_Tenure, ACS_Vars_Veterans)
BR_VARS_ALL_2021 = unique(BR_VARS_ALL_2021)
View(BR_VARS_ALL_2021)

BR_VARS_ALL_2021 <- BR_VARS_ALL_2021 %>%
  mutate(Var_2021 = ifelse(is.na(Var_2021), 'BakerRipley Calculation', Var_2021))

testing = subset(BR_VARS_ALL_2021, Var_2021 == 'BakerRipley Calculation')
View(testing)

# Remove percentages for the variables it doesn't make sense for
BR_VARS_ALL_2021 <- BR_VARS_ALL_2021 %>%
  mutate(Percent_Of_Population = ifelse(Percent_Of_Population == 100.00, NA, Percent_Of_Population))

View(BR_VARS_ALL_2021)

BR_VARS_ALL_2021 <- BR_VARS_ALL_2021 %>%
  mutate(Percent_Of_Population = 
           ifelse(Variable_Name %in% 
                    c('Median household income ($)', 
                      'Unemployment Rate', 
                      'Average household size', 
                      'Average family size'), NA, Percent_Of_Population))

View(BR_VARS_ALL_2021)

total_pop = subset(BR_VARS_ALL_2021, Variable_Name == "Total Population: General")
View(sum(total_pop$estimate))

write.csv(BR_VARS_ALL_2021, "BR_VARS_ALL_2021.csv", row.names=FALSE)

hello = read.csv("BR_VARS_ALL_2021.csv")

View(hello)

#### yayyyyy ###

total_under_5 = subset(BR_VARS_ALL_2021, Variable_Name == "Under 5 years")
View(sum(total_under_5$estimate))
summary(total_under_5$Percent_Of_Population)





# TODO: Make 100% total population be NA, make percent name include a (%), also make it * 100 and round


### Merge all categories together into one dataframe ###

### Export new dataframe to CSV in the current directory ###
# Note: geometry column ommitted for conversion compatability
ACS_BR_2021_No_Geo = subset(ACS_BR_2021, select = -c(geometry))

### Create a subsetted dataframe for each neighborhood ###

# Pasadena
ACS_BR_PAS_2021 = subset(ACS_BR_2021_With_Names, Neighborhood == 'Pasadena')
# View newly created datatframe
View(ACS_BR_PAS_2021)

# Gulfton/Sharpstown
ACS_BR_GUL_SHA_2021 = subset(ACS_BR_2021_With_Names, Neighborhood == 'Gulfton/Sharpstown')
# View newly created datatframe
View(ACS_BR_GUL_SHA_2021)

# Hobby/Harbach
ACS_BR_HOB_HAR_2021 = subset(ACS_BR_2021_With_Names, Neighborhood == 'Hobby/Harbach')
# View newly created datatframe
View(ACS_BR_HOB_HAR_2021)

# East Aldine
ACS_BR_EA_AL_2021 = subset(ACS_BR_2021_With_Names, Neighborhood == 'EastAldine')
# View newly created dataframe
View(ACS_BR_EA_AL_2021)

# East End
ACS_BR_EA_END_2021 = subset(ACS_BR_2021_With_Names, Neighborhood == 'East End')
# View newly created datatframe
View(ACS_BR_EA_END_2021)

# Create summaries off of this (make the datasets cleaner later)

### Export new dataframe to CSV in the current directory ###
# Note: geometry column ommitted for conversion compatability
ACS_BR_2021_No_Geo = subset(ACS_BR_2021, select = -c(geometry))

# View dataframe with geometry omitted
View(ACS_BR_2021_No_Geo)

write.csv(ACS_BR_2021_No_Geo, "median_income_2021.csv", row.names=FALSE)

# Repeat the process for each neighborhood's dataframe

# Check that the CSV was exported correctly
new_data = read.csv("median_income_2021.csv")
View(new_data)

### Create a chloropleth map of all the neighborhoods, based on a single variable ###
# Create a dataframe that subsets the original for a single variable

# Example: Median Family Income, All Neighborhoods
ACS_BR_2021_Med_Inc = subset(ACS_BR_2021, Var_2021 == 'DP03_0062')

# View the new dataframe
View(ACS_BR_2021_Med_Inc)

# Example: Median Family Income, Pasadena
View(ACS_BR_PAS_2021)
ACS_BR_PAS_2021_Med_Inc = subset(ACS_BR_PAS_2021, Var_2021 == 'DP03_0062')

# Example: Median Family Income, East End
ACS_BR_EA_END_2021_Med_Inc = subset(ACS_BR_EA_END_2021, Var_2021 == 'DP03_0062')

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
st_geometry(ACS_BR_2021_Med_Inc) = "geometry"
st_geometry(ACS_BR_2021_Total_Pop) = "geometry"
st_geometry(ACS_BR_PAS_2021_Med_Inc) = "geometry"
st_geometry(ACS_BR_PAS_2021_Total_Pop) = "geometry"
st_geometry(ACS_BR_EA_END_2021_Med_Inc) = "geometry"

# Create the map

# Colors by neighborhood

# Add row names for feature title

mapview(ACS_BR_2021_Med_Inc, zcol = "Neighborhood")

mapview(ACS_BR_2021_Med_Inc, zcol = "estimate")

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

# Total Population: Pasadena

# Repeat the process for each neighborhood's map

# Can subset for that neighborhood + change title to be that neighborhood

### Display key information for each neighborhood ###

### Create Summary Tables ###

# Drop-down menu: Age is a header, then they can select within that

# That should feed into Tidycensus

