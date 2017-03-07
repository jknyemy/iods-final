#Janne Kirjasniemi 7.3.2017
#Creating the human aid dataset. Sources are:
#UNDP - Human Development Reports - Human Development Index 2015
#http://hdr.undp.org/en/data
#And Gapminder:
#https://www.gapminder.org/data/;
#For foreign aid data provided by World Bank
#For Child deaths data provided by Lance

library(dplyr)
library(xlsx)
 
setwd("C:/Users/Janne/Documents/GitHub/iods-final/data")
#Loading the HDI data
var_classes <- c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
hdi1 <- read.xlsx2("2015_Statistical_Annex_Table_1_1.xls", sheetIndex=1, startRow=6, endRow=194,
                  colClasses = var_classes)
hdi1 <- select(hdi1, -GNI.HDI.rank)#Eliminating an unneeded variable.
hdi1$Country <- as.character(hdi1$Country)
class(hdi1$Country)
str(hdi1)
head(hdi1)

var_classes2 <- c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                  , "numeric", "numeric", "numeric", "numeric")
hdi2 <- read.xlsx2("2015_Statistical_Annex_Table_4_1.xls", sheetIndex=1, startRow=6, endRow=194,
                           colClasses = var_classes2)
str(hdi2)
hdi2$Country <- as.character(hdi2$Country)

var_classes3 <- var_classes2[1:9]
hdi3 <- read.xlsx2("2015_Statistical_Annex_Table_5_1.xls", sheetIndex=1, startRow=6, endRow=194,
                      colClasses = var_classes3)
str(hdi3)
hdi3$Country <- as.character(hdi3$Country)

#Then to transmute the variables divided into genders in hdi2 and
#hdi3 into variables of rate between genders by dividin F by M
#so values <1 mean there are less females than men in for example
#Labour participation and >1 means there are more women than men
#and 1 naturally is a 50-50 split.

HDI.FM <- transmute(hdi2, HDI.FM = HDI.F / HDI.M)
Life.expFM <- transmute(hdi2, Life.expFM = Life.exp.F / Life.exp.M)
str(hdi2)
Exp.SchoolFM <- transmute(hdi2, Exp.SchoolFM = Exp.school.F/Exp.school.M)
Mean.SchoolFM <- transmute(hdi2, Mean.SchoolFM = Mean.School.F/MeanSchool.M)
