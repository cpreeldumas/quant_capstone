install.packages("tidycensus")
install.packages("tidyverse")
library(tidycensus)
library(tidyverse)


#check if you might need a new census API key:
census_api_key("d6995d87df335fc958097461151c236674041776", install = TRUE)
readRenviron("~/.Renviron")`




v20 <- load_variables(2020, "acs5", cache = TRUE)

#new metadata: 
#B06009_005, Estimate!!Total:!!Bachelor's degree, PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract
#B06009_002, Estimate!!Total:!!Less than high school graduate, PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract
#B06009_003, Estimate!!Total:!!High school graduate (includes equivalency), PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES, tract

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
#B07013_002, Estimate!!Total:!!Householder lived in owner-occupied housing, GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES, tract
#B07013_003, Estimate!!Total:!!Householder lived in renter-occupied housing unit, GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES, tract


#Now excluding
#B08537_002
#B25008_003, Estimate!!Total:!!Renter occupied, TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE, block group


#Another population measure= B01001_001, Estimate!!Total: SEX BY AGE, block group 

#more on age from Denise 6 Feb   



covariate_data <- get_acs(geography = c("county"),
                          variables = c(Bachelors="B06009_005",
                                        
                                        Below_highschool="B06009_002",
                                        
                                        Highschool_graduate="B06009_003",
                                        Renter_occupied="B07013_003",
                                        Owner_occupied="B07013_002",
                                        Median_age="B01002_001",
                                        Females="B01001_026",
                                        Males= "B01001_002",
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

#to check validity of education variables 
covariate_data <- get_acs(geography = c("county"),
                          variables = c(Bachelors="B06009_005",Bachelors_2= "B15011_001", Below_highschool="B06009_002", Below_highschool2="B07009_002",
                                       Total_population="B01003_001",
                                        Highschool_graduate="B06009_003"
                                      ), 
                          year = 2020,
                          sumfile = "dhc")
#the two codes for below high school return similar results, meaning B06009_002 was always correct to use. Assuming that is true, then we can assume B06009_003 is accurate. And if that is true then we shall still stick with B06009_005 for Bachelors because for the first result (Antigua County), the Bachelors cannot logically be more than the high school graduates. 

#To check other possibly useful variables
B09001_002- Estimate!!Total:!!In households:POPULATION UNDER 18 YEARS BY AGE: Tract

B01001_026- the female one
B01001_002- the male one
#add the two above and compare with total population 


B98012_001
C15002I_010
C15002I_005






#continuation from detour 
  covariate_data$moe<- NULL
  
  library(tidyverse)
  
  covariate_data2<-covariate_data %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate,
    values_fill = 0
  )




covariate_data2$Owners_plus_renters<-rowSums(covariate_data2[19:20])

# Create fractions
covariate_data2$Renters_occupied_percentage <- covariate_data2$Renter_occupied/ covariate_data2$Owners_plus_renters*100
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
covariate_data2$Male_female_ratio<-covariate_data2$Males/covariate_data2$Females


covariate_data2$Renter_occupied<- NULL
covariate_data2$Owner_occupied<- NULL
covariate_data2$Owners_plus_renters<- NULL
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
covariate_data2$Males<-NULL
covariate_data2$Females<-NULL
#Enter Land Area from Cencus

install.packages("tidyr")
library(tidyr)

install.packages("stringr")
library(stringr)

setwd("C:/Users/mandy/OneDrive/Desktop/New folder/NYU Classes/Quantitative Capstone/Covariates")
dir()

LandArea <- read.csv("LandArea2.csv",stringsAsFactors=TRUE)
View(LandArea)


LandArea$LND010190F<-NULL
LandArea$LND010190D<-NULL
LandArea$LND010190N1<-NULL
LandArea$LND010190N2<-NULL
LandArea$LND010200F<-NULL
LandArea$LND010200D<-NULL
LandArea$LND010200N1<-NULL
LandArea$LND010200N2<-NULL
LandArea$LND110180F<-NULL
LandArea$LND110180D<-NULL
LandArea$LND110180N1<-NULL
LandArea$LND110180N2<-NULL
LandArea$LND110190F<-NULL
LandArea$LND110190D<-NULL
LandArea$LND110190N1<-NULL
LandArea$LND110190N2<-NULL
LandArea$LND110200F<-NULL
LandArea$LND110200N1<-NULL
LandArea$LND110200N2<-NULL
LandArea$LND110200D<-NULL
LandArea$LND110210F<-NULL
LandArea$LND110210N1<-NULL
LandArea$LND110210N2<-NULL
LandArea$LND210190F<-NULL
LandArea$LND210190D<-NULL
LandArea$LND210190N1<-NULL
LandArea$LND210190N2<-NULL
LandArea$LND210200F<-NULL
LandArea$LND210200N2<-NULL
LandArea$LND210200N1<-NULL
LandArea$LND210200D<-NULL




#JOIN both datasets
names(LandArea)[1:3]<-c('Areaname','GEOID','Land_Area')

LandArea$GEOID<- as.numeric(LandArea$GEOID)
class(LandArea$GEOID)

covariate_data2$GEOID<-as.numeric(covariate_data2$GEOID)
class(covariate_data2$GEOID)



covariate_data5<-left_join(
  covariate_data2,
  LandArea,
  by = "GEOID",
  copy = FALSE,
  keep = NULL)

covariate_data5$popul_density<-covariate_data5$Total_population/covariate_data5$Land_Area
covariate_data5$Land_Area<-NULL



#Try out geometry method for land area
#https://www.census.gov/quickfacts/fact/note/US/POP060210

library(tidycensus)
library(tidyverse)

options(tigris_use_cache = TRUE)
install.packages(c("tidycensus", "sf", "dplyr"))
library(tidycensus)
library(sf)
library(dplyr)

orange <- get_acs(
  geography = "county",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2022
)

view(orange)

orange <- orange %>%
  mutate(area = st_area(.))

view(orange)



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
  data= covariate_data5,
  col= "GEOID",
  into= c("GEOID", "GEOID2"),
  sep = +1,
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
)


#Now Join with state population

state_population2$GEOID<-as.numeric(state_population2$GEOID)
covariate_data3$GEOID<-as.numeric(covariate_data3$GEOID)


covariates_joined2<-full_join(
  covariate_data3,
  state_population2,
  by = "GEOID",
  copy = FALSE,
  suffix = c(". County_Covariate_Data3", ". State_Population2"),
  keep = NULL)







setwd("C:/Users/mandy/OneDrive/Desktop/New folder/NYU Classes/Quantitative Capstone/Covariates/2020")
library(readr)
write_csv(covariate_data5, 'covariates_census_v10')

%USERPROFILE%\AppData\Local\RStudio-Desktop\sources
history(Inf)




#cutting some columns for descriptive statistics
covariates_joined$GEOID<-NULL
covariates_joined$GEOID2<-NULL
covariates_joined$Highschool_graduate_percentage<-NULL
covariates_joined$Below_highschool_percentage<-NULL
covariates_joined$American_Indian_and_Alaska_Native_percentage<-NULL
covariates_joined$Asian_percentage<-NULL
covariates_joined$Native_Hawaiian_and_other_Pacific_Island_percentage<-NULL
covariates_joined$Some_other_race_alone_percentage<-NULL
covariates_joined$Two_or_more_races_percentage<-NULL
covariates_joined$White_alone_not_hispanic_or_latino_percentage<-NULL
covariates_joined$`NAME. State_Population2`<-NULL

#preparing mean and median for a small output table
Mean_Age<-mean(covariates_joined$Median_age)
Median_Age<-median(covariates_joined$Median_age)

Mean_M.Income<-median(covariates_joined$Median_income, na.rm=TRUE)
Median_M.Income<-median(covariates_joined$Median_income, na.rm=TRUE)

Mean_Population<-mean(covariates_joined$`Total_population. County_Covariate_Data3`)
Median_Population<-median(covariates_joined$`Total_population. County_Covariate_Data3`)

Mean_Renters<-mean(covariates_joined$Renters_occupied_percentage,na.rm=TRUE)
Median_Renters<-median(covariates_joined$Renters_occupied_percentage, na.rm=TRUE)
Mean_Bachelors<-mean(covariates_joined$Bachelors_percentage, na.rm=TRUE)
Median_Bachelors<-median(covariates_joined$Bachelors_percentage, na.rm=TRUE)
Mean_White<-mean(covariates_joined$White_alone_percentage)
Median_White<-median(covariates_joined$White_alone_percentage)


Measure<-c("Median","Mean")
Age<-c(Median_Age,Mean_Age)
Median_Income<-c(Median_M.Income,Mean_M.Income)
Renters_Share<-c(Median_Renters, Mean_Renters)
Bachelors<-c(Median_Bachelors,Mean_Bachelors)
White_Percentage<-(Median_White,Mean_White)
Population<-c(Median_Population, Mean_Population)


df1<- data_frame(Measure,Age,Median_Income,Renters_Share,Bachelors,Population)
  
df2<- print.data.frame(df1)
library(gridExtra)
pdf(file="myfirstpdf2.pdf")
grid.table(df1)
dev.off()

# why wouldn't above high school + below high school = 100%?? 
covariate_data2$EDUC_PERCENT_SUM<-rowSums(covariate_data2[8:9])
covariate_data2$RACE_PERCENT_SUM<-rowSums(covariate_data2[10:18])
    #actually the percentage of at high school graduates seems so low!

covariates_joined$EDUC_PERCENT_SUM<-NULL
covariates_joined$RACE_PERCENT_SUM<-NULL


