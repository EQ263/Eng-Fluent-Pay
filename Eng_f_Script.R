library(readr)
library(dplyr)
library(fpp2)
library(GGally)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(fastDummies)
library(foreign)
library(plm)
library(psych)
##########################
# CLEANING DATA IN R 
#########################


# Goals is WAGE = ENG_F + ED + AGE + SEX + YSM(YEARS SINCE MIGRATION) + MARRIED + COO (COUNTRY OF ORIGIN) + RACE 

# Wage Log 
Eng$IWAGE <- log(Eng$WAGP)

#English Fluency Values --> zero value was eliminated to only look at individuals with a second language 
# 1 and 2 will be if the individual has a strong English fluency and 3 and 4 being weak level 0f English fluency 
Eng$ENG<- ifelse(Eng$ENG == 1| Eng$ENG == 2, 1,0)

# Education Attainment
# 01-15 -Did not complete high school. 
# 16-High school graduate - regular high school diploma; 
# 17-High school graduate - GED or alternative credential
# 18-19-Some college, no degree, 
# 20-Associate's degree; 
# 21-Bachelor's degree; 
# 22-24-Post graduate degree;
# High School & GED
Eng$HS<- ifelse(Eng$SCHL =="16"|Eng$SCHL=="17", 1,0)
# Some College & Associate 
Eng$SC<- ifelse(Eng$SCHL =="18"| Eng$SCHL == "19" | Eng$SCHL == "20", 1,0)
# Bachelor's Degree 
Eng$BACH <- ifelse(Eng$SCHL =="21",1,0)
# Post undergraduate 
Eng$GRAD <- ifelse(Eng$SCHL == "22" | Eng$SCHL == "23" | Eng$SCHL == "24", 1,0)

#AGE IS OKAY
Eng$AGE <- Eng$AGEP

#Gender code 
Eng$MALE <- ifelse(Eng$SEX== 1, 1,0)

#YSM DOES NOT NEED TO BE MODIFIED

#Martial Status 
#1 - married
#2 - widowed
#3 - divorced
#4 -separated 
#5 - Never married
Eng$MARRIED <- ifelse(Eng$MAR == "1", 1,0)

#Country of origin (WILL HAVE TOP CHANGE TO CONTIENT) 
# codes will go as follows,,,,,
# 1 - Europe 
# 2- Africa
# 3 - North America (Canada & Mexico)
# 4 - Central & South American 
# 5- Asia 
# 6 - Australia 
#Europe
Eng$Euro <- ifelse(Eng$MIGSP == "1" , 1,0)
#Africa 
Eng$Africa <- ifelse(Eng$MIGSP == "2" , 1,0)
#North America 
Eng$NOA <- ifelse(Eng$MIGSP == "3" , 1,0)
#Central/South America
Eng$SCA <- ifelse(Eng$MIGSP == "4" , 1,0)
#Asia 
Eng$Asia <- ifelse(Eng$MIGSP == "5" , 1,0)
#Australia
Eng$Aussie <- ifelse(Eng$MIGSP == "6" , 1,0)

#Hispanic Code 
#Since the variable has so many option of Hispanic and one option stating that u are not we will apply it to the variable that way 
Eng$NOT_HISP <- ifelse(Eng$HISP == "1", 1,0)

#Summary Stats 
summary(Eng)

#Regression
ML_ENG <- lm( IWAGE ~ ENG + BACH + GRAD + MALE + AGE + MARRIED + 
                NOT_HISP + RACBLK + RACWHT + YSM + Africa + Asia , data=Eng)

summary(ML_ENG)

#fitting the table 

