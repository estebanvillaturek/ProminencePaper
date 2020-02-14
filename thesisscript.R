#-----
# Master Thesis script
# Author: Esteban Villa-Turek
# Hertie School of Governance Berlin
#-----
  
  
# install required packages
library(cjoint)
#install.packages('kableExtra')
library(plyr)
library(tidyverse)
library(knitr)
library(stargazer)
library(kableExtra)



# import raw data
dat1 <- read_csv2("lastquestionsfinal.csv" )
dat2 <- read_csv2("conjointfinaldata.csv")

#merge datasets
dat <- merge(dat2, dat1, by = 'Response ID')

# rename columns
colnames(dat)[4] <- "Partyid"
colnames(dat)[5] <- "Ethnicity"
colnames(dat)[6] <- "Incumbency"
colnames(dat)[7] <- "Gender"
colnames(dat)[8] <- "Occupation"
colnames(dat)[9] <- "Age"
colnames(dat)[10] <- "Income"
colnames(dat)[11] <- "Pub_prominence"
colnames(dat)[12] <- "Pol_prominence"
colnames(dat)[13] <- "Elected"
colnames(dat)[14] <- "Resp_leftright"
colnames(dat)[15] <- "Resp_race"
colnames(dat)[16] <- "Resp_age"
colnames(dat)[17] <- "Resp_partisanship"
colnames(dat)[18] <- "Resp_pol_int"
colnames(dat)[19] <- "Resp_gender"
colnames(dat)[20] <- "Resp_educ"

# recode partyid levels 3-6 as 3
dat$Partyid[dat$Partyid >= 3] <- 3
table(dat$Partyid)

#convert variables to factors and rename levels
dat$Resp_leftright <- as.factor(dat$Resp_leftright)

dat$Partyid <- as.factor(dat$Partyid)
dat$Partyid <- revalue(dat$Partyid, c('1' = 'Republican', '2' = 'Democrat', '3' = 'No Party'))

dat$Ethnicity <- as.factor(dat$Ethnicity)
dat$Ethnicity <- revalue(dat$Ethnicity, c('1' = 'African American', '2' = 'Hispanic/ Latino',
                                          '3' = 'White', '4'= 'Native American', '5' = 'Asian'))

dat$Incumbency <- as.factor(dat$Incumbency)
dat$Incumbency <- revalue(dat$Incumbency, c('1' = 'Incumbent', '2' = 'Challenger'))

dat$Gender <- as.factor(dat$Gender)
dat$Gender <- revalue(dat$Gender, c('1' = 'Female', '2' = 'Male'))

dat$Occupation <- as.factor(dat$Occupation)
dat$Occupation <- revalue(dat$Occupation, c('1' = 'Lawyer', '2' = 'Military Officer',
                                          '3' = 'Teacher', '4'= 'Farmer', '5' = 'Business owner',
                                          '6'= 'Athlete', '7' = 'Actor', '8' = 'Banker', '9' = 'Journalist', '10' = 'Union Leader'))

dat$Age <- as.factor(dat$Age)
dat$Age <- revalue(dat$Age, c('1' = '31 years old', '2' = '38 years old',
                                            '3' = '45 years old', '4'= '52 years old', '5' = '59 years old',
                                            '6'= '66 years old', '7' = '73 years old'))

dat$Income <- as.factor(dat$Income)
dat$Income <- revalue(dat$Income, c('1' = 'Annual income $32.000', '2' = 'Annual income $54.000',
                              '3' = 'Annual income $75.000', '4'= 'Annual income $92.000', '5' = 'Annual income $140.000',
                              '6'= 'Annual income $360.000', '7' = 'Annual income $840.000'))


dat$Pub_prominence <- as.factor(dat$Pub_prominence)
dat$Pub_prominence <- revalue(dat$Pub_prominence, c('1' = '210 Twitter followers', '2' = '2.400 Twitter followers',
                                          '3' = '23.700 Twitter followers', '4'= '315.000 Twitter followers',
                                          '5' = '1.3 Million Twitter followers'))

dat$Pol_prominence <- as.factor(dat$Pol_prominence)
dat$Pol_prominence <- revalue(dat$Pol_prominence, c('1' = 'No Major Role in Party', '2' = 'Locally Renowned Party Member',
                                                    '3' = 'Statewide Renowned Party Member', '4'= 'Nationally Renowned Party Member'))

dat$Resp_race <- as.factor(dat$Resp_race)
dat$Resp_race <- revalue(dat$Resp_race, c('1' = 'White', '2' = 'African American',
                                                    '3' = 'Hispanic/ Latino', '4'= 'Asian'))

dat$Resp_age <- as.factor(dat$Resp_age)
dat$Resp_age <- revalue(dat$Resp_age, c('2' = '20 - 30 years old',
                                        '3' = '31 - 40 years old', '4'= '41 - 50 years old', '5' = '51 - 60 years old',
                                        '6'= '61- 70 years old'))

dat$Resp_partisanship <- as.factor(dat$Resp_partisanship)
dat$Resp_partisanship <- revalue(dat$Resp_partisanship, c('1' = 'Republican', '2' = 'Democrat',
                                          '3' = 'Independent', '4'= 'Something else'))


dat$Resp_pol_int <- as.factor(dat$Resp_pol_int)
dat$Resp_pol_int <- revalue(dat$Resp_pol_int, c('1' = 'Not interested at all', '2' = 'Slightly interested',
                                                    '3' = 'Moderately interested', '4'= 'Rather interested',
                                                    '5' = 'Very interested'))


dat$Resp_gender <- as.factor(dat$Resp_gender)
dat$Resp_gender <- revalue(dat$Resp_gender, c('1' = 'Male', '2' = 'Female'))


dat$Resp_educ <- as.factor(dat$Resp_educ)
dat$Resp_educ <- revalue(dat$Resp_educ, c('2' = 'Highschool, no diploma',
                                        '3' = 'Highschool diploma or equivalent',
                                        '4'= 'Some college or university studies, not completed',
                                        '5' = 'College or university studies, completed',
                                        '6'= 'Graduate studies'))

# Change the baselines
baselines <- list()
baselines$Partyid <- "No Party"
baselines$Ethnicity <- "White"


### Run AMCE estimation

#view(amce)

results <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                  Occupation + Age + Income + Pub_prominence + Pol_prominence,
                data = dat, respondent.id = 'Response ID',
                na.ignore = TRUE, cluster = TRUE, baseline = baselines)



#Plot amce results
plot.amce(results)


### Run ACIE estimation

# interaction between political and public prominence
results_intprom <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                  Occupation + Age + Income + Pub_prominence + Pol_prominence + Pol_prominence:Pub_prominence,
                data = dat, respondent.id = 'Response ID',
                na.ignore = TRUE, cluster = TRUE, baseline = baselines)

summary.amce(results_intprom)

plot.amce(results_intprom)

# short interaction between political and public prominence and prominence and occupation (no custom baselines!)
results_shortintprom <- amce(Elected ~ 
                          Occupation + Pub_prominence + Pol_prominence +
                          Pol_prominence:Pub_prominence+
                          Pub_prominence:Occupation,
                        data = dat, respondent.id = 'Response ID',
                        na.ignore = TRUE, cluster = TRUE)

summary.amce(results_shortintprom)
plot.amce(results_shortintprom)


#all significant effect interactions
results_int <- amce(Elected ~   Gender +
                          Age + Income + Pub_prominence + Pol_prominence + Partyid+
                      Age:Pub_prominence+
                      Pol_prominence:Age+
                      Pub_prominence:Income +
                      Pol_prominence:Partyid+
                      Pub_prominence:Partyid,
                        data = dat, respondent.id = 'Response ID',
                        na.ignore = TRUE, cluster = TRUE, baseline = baselines)
summary.amce(results_int)
plot.amce(results_int)
stargazer(results_int$estimates, title="ACIE Results", align=TRUE, type = 'text', out = 'acieresults.txt')



# Conditional effects with respondent varying variables

cond_leftright_full <- amce(Elected ~ Partyid  + Ethnicity + Incumbency + Gender +
                              Occupation + Age + Income +  Pub_prominence + Pol_prominence + Resp_leftright +
                              Pol_prominence:Resp_leftright+
                              Pub_prominence:Resp_leftright+
                              Partyid:Resp_leftright+
                              Ethnicity:Resp_leftright+
                              Incumbency:Resp_leftright+
                              Gender:Resp_leftright+
                              Occupation:Resp_leftright+
                              Age:Resp_leftright+
                              Income:Resp_leftright,
                            data = dat, respondent.id = 'Response ID',
                            na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_leftright')

plot.amce(cond_leftright_full)
summary(cond_leftright_full)

#leftright

cond_leftright_promi <- amce(Elected ~ Partyid +  Pub_prominence + Pol_prominence + Resp_leftright +
                       Pol_prominence:Resp_leftright+
                       Pub_prominence:Resp_leftright+
                         Partyid:Resp_leftright,
                   data = dat, respondent.id = 'Response ID',
                   na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_leftright')

summary.amce(cond_leftright_promi)

plot.amce(cond_leftright_promi)


#partisanship
cond_partisanship <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                            Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_partisanship +
                            Pub_prominence:Resp_partisanship+
                            Pol_prominence:Resp_partisanship+
                            Income:Resp_partisanship+
                            Age:Resp_partisanship+
                            Occupation:Resp_partisanship+
                            Gender:Resp_partisanship+
                            Incumbency:Resp_partisanship+
                            Ethnicity:Resp_partisanship+
                            Partyid:Resp_partisanship,                  data = dat, respondent.id = 'Response ID',
                          na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_partisanship')
summary(cond_partisanship)
plot(cond_partisanship)

table(dat$Resp_partisanship)

# Respondent's race
cond_race <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                    Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_race +
                    Pub_prominence:Resp_race+
                    Pol_prominence:Resp_race+
                    Income:Resp_race+
                    Age:Resp_race+
                    Occupation:Resp_race+
                    Gender:Resp_race+
                    Incumbency:Resp_race+
                    Ethnicity:Resp_race+
                    Partyid:Resp_race,
                  data = dat, respondent.id = 'Response ID',
                  na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_race')
summary(cond_race)

plot(cond_race)



# Respondent's political interest
cond_polint <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                            Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_pol_int +
                            Pol_prominence:Resp_pol_int+
                            Pub_prominence:Resp_pol_int+
                            Income:Resp_pol_int+
                            Age:Resp_pol_int+
                            Occupation:Resp_pol_int+
                            Gender:Resp_pol_int+
                            Incumbency:Resp_pol_int+
                            Ethnicity:Resp_pol_int+
                            Partyid:Resp_pol_int,
                          data = dat, respondent.id = 'Response ID',
                          na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_pol_int')
summary(cond_polint)
plot(cond_polint)

#Respondent's gender

cond_gender <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                      Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_gender +
                      Pol_prominence:Resp_gender +
                      Pub_prominence:Resp_gender+
                      Income:Resp_gender+
                      Age:Resp_gender+
                      Occupation:Resp_gender+
                      Gender:Resp_gender+
                      Incumbency:Resp_gender+
                      Ethnicity:Resp_gender+
                      Partyid:Resp_gender,
                    data = dat, respondent.id = 'Response ID',
                    na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_gender')

summary(cond_gender)
plot(cond_gender)


#Respondent's education

cond_educ <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                      Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_educ +
                    Pol_prominence:Resp_educ +
                    Pub_prominence:Resp_educ+
                    Income:Resp_educ+
                    Age:Resp_educ+
                    Occupation:Resp_educ+
                    Gender:Resp_educ+
                    Incumbency:Resp_educ+
                    Ethnicity:Resp_educ+
                    Partyid:Resp_educ,
                    data = dat, respondent.id = 'Response ID',
                    na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_educ')

summary(cond_educ)
plot(cond_educ)
table(dat$Resp_educ)

#Respondent's age

cond_age <- amce(Elected ~ Partyid + Ethnicity + Incumbency + Gender +
                    Occupation + Age + Income + Pub_prominence + Pol_prominence + Resp_age +
                    Pol_prominence:Resp_age +
                    Pub_prominence:Resp_age+
                    Income:Resp_age+
                    Age:Resp_age+
                    Occupation:Resp_age+
                    Gender:Resp_age+
                    Incumbency:Resp_age+
                    Ethnicity:Resp_age+
                    Partyid:Resp_age,
                  data = dat, respondent.id = 'Response ID',
                  na.ignore = TRUE, cluster = TRUE, baseline = baselines, respondent.varying = 'Resp_age')

summary(cond_age)
plot(cond_age)
table(dat$Resp_age)
