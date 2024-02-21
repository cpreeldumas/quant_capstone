install.packages("tidycensus")
install.packages("tidyverse")


census_api_key("d6995d87df335fc958097461151c236674041776", install = TRUE)
readRenviron("~/.Renviron")`




v20 <- load_variables(2020, "acs5", cache = TRUE)

#new metadata: 
#B06009_005, Estimate!!Total:!!Bachelor's degree, PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract
#B06009_002, Estimate!!Total:!!Less than high school graduate, PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract
#B06009_003, Estimate!!Total:!!High school graduate (includes equivalency), PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract
#B25008_003, Estimate!!Total:!!Renter occupied, TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE, block group
#B01002_001,Estimate!!Median age --!!Total: MEDIAN AGE BY SEX, block group
#B06011_001, Estimate!!Median income in the past 12 months --!!Total:MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION ADJUSTED DOLLARS)BYPLACE OF BIRTH IN THE UNITED STATES; tract
#B07411_002, Estimate!!Median income in the past 12 months --!!Total living in area 1 year ago:!!Same house, MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS) BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES
#B01003_001, Estimate!!Total, TOTAL POPULATION, block group
#B01001A_001, Estimate!!Total:SEX BY AGE (WHITE ALONE), tract
#B01001B_001, Estimate!!Total:SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE), tract
#B01001C_001, Estimate!!Total:SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE), tract
#B01001D_001, Estimate!!Total: SEX BY AGE (ASIAN ALONE), tract
#B01001E_001, Estimate!!Total: SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLAND, tract
#B01001I_001, Estimate!!Total:SEX BY AGE (HISPANIC OR LATINO), tract
#B01001F_001, Estimate!!Total:SEX BY AGE (SOME OTHER RACE ALONE), tract
#B01001G_001, Estimate!!Total: SEX BY AGE (TWO OR MORE RACES), tract
#B01001H_001, Estimate!!Total:SEX BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO), tractWhite
#Another population measure= B01001_001, Estimate!!Total: SEX BY AGE, block group 

#more on age from Denise 6 Feb   


covariate_data <- get_acs(geography = c("county"),
                          variables = c(Bachelors="B06009_005",
                                        Below_highschool="B06009_002",
                                        Highschool_graduate="B06009_003",
                                        Renter_occupied="B25008_003",
                                        Median_age="B01002_001",
                                        Median_income="B06011_001",
                                        Total_population="B01003_001", 
                                        White_alone_not_hispanic_or_latino="B01001H_001", 
                                        White_alone= "B01001A_001", 
                                        Black_or_African_American= "B01001B_001", 
                                        American_Indian_and_Alaska_Native= "B01001C_001", 
                                        Asian="B01001D_001", 
                                        Native_Hawaiian_and_other_Pacific_Island= "B01001E_001", 
                                        Hispanic_or_Latino= "B01001I_001", 
                                        Some_other_race_alone= "B01001F_001", 
                                        Two_or_more_races= "B01001G_001"), 
                          year = 2020,
                          sumfile = "dhc")
  covariate_data$moe<- NULL
  covariate_data2<-covariate_data %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate,
    values_fill = 0
  )





# Create percentages:
covariate_data2$Renters_occupied_percentage <- covariate_data2$Renter_occupied/ covariate_data2$Total_population*100
covariate_data2$Bachelors_percentage <- covariate_data2$Bachelors/ covariate_data2$Total_population*100
covariate_data2$Highschool_graduate_percentage <- covariate_data2$Highschool_graduate/ covariate_data2$Total_population*100
covariate_data2$Below_highschool_percentage <- covariate_data2$Below_highschool/ covariate_data2$Total_population*100
covariate_data2$White_alone_percentage<- covariate_data2$White_alone/ covariate_data2$Total_population*100
covariate_data2$Black_or_African_American_percentage<- covariate_data2$Black_or_African_American/ covariate_data2$Total_population*100
covariate_data2$Hispanic_or_Latino_percentage<- covariate_data2$Hispanic_or_Latino/ covariate_data2$Total_population*100
covariate_data2$American_Indian_and_Alaska_Native_percentage<- covariate_data2$American_Indian_and_Alaska_Native/ covariate_data2$Total_population*100
covariate_data2$Asian_percentage<- covariate_data2$Asian/ covariate_data2$Total_population*100
covariate_data2$Native_Hawaiian_and_other_Pacific_Island_percentage<- covariate_data2$Native_Hawaiian_and_other_Pacific_Island/ covariate_data2$Total_population*100
covariate_data2$Some_other_race_alone_percentage<- covariate_data2$Some_other_race_alone/ covariate_data2$Total_population*100
covariate_data2$Two_or_more_races_percentage<- covariate_data2$Two_or_more_races/ covariate_data2$Total_population*100
covariate_data2$White_alone_not_hispanic_or_latino_percentage<- covariate_data2$Hispanic_or_Latino/ covariate_data2$Total_population*100


covariate_data2$Renter_occupied<- NULL
covariate_data2$Bachelors<- NULL
covariate_data2$Highschool_graduate<- NULL
covariate_data2$Below_highschool<-NULL
covariate_data2$White_alone<- NULL
covariate_data2$Black_or_African_American<- NULL
covariate_data2$Hispanic_or_Latino<-NULL
covariate_data2$American_Indian_and_Alaska_Native<-NULL
covariate_data2$Asian<- NULL
covariate_data2$Native_Hawaiian_and_other_Pacific_Island<- NULL
covariate_data2$Some_other_race_alone<- NULL
covariate_data2$Two_or_more_races<- NULL
covariate_data2$White_alone_not_hispanic_or_latino<- NULL

#get_acs for state level population then merge 
state_population <- get_acs(geography = c("state"),
                          variables = c(Total_population="B01003_001"), 
                          year = 2020,
                          sumfile = "dhc")
state_population$moe<- NULL

state_population2<-state_population %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate,
    values_fill = 0
  )



# seperate function on GEOID
library(tidyverse)

covariate_data3<- separate(
  data= covariate_data2,
  col= "GEOID",
  into= c("GEOID", "GEOID2"),
  sep = +2,
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
)

covariates_joined<-full_join(
  covariate_data3,
  state_population2,
  by = "GEOID",
  copy = FALSE,
  suffix = c(". Country_Covariate_Data3", ". State_Population2"),
  keep = NULL)

covariates_joined$EDUC_PERCENT_SUM<-NULL
covariates_joined$RACE_PERCENT_SUM<-NULL


# why wouldn't above high school + below high school = 100%?? 
covariate_data2$EDUC_PERCENT_SUM<-rowSums(covariate_data2[8:9])
covariate_data2$RACE_PERCENT_SUM<-rowSums(covariate_data2[10:18])
    #actually the percentage of at high school graduates seems so low!


setwd("C:/Users/User/OneDrive/Desktop/New folder/NYU Classes/Quantitative Capstone/Covariates/2020")
library(readr)
write_csv(covariate_data2, 'output 13 Feb')
    


