#Janne Kirjasniemi 7.3.2017
#Creating the human aid dataset. Sources are:
#UNDP - Human Development Reports - Human Development Index 2015
#http://hdr.undp.org/en/data
#And Gapminder:
#https://www.gapminder.org/data/;
#For foreign aid data provided by World Bank
#For Child deaths and women's fertility data provided by Gapminder

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

#This could be done more elegantly, but this gets the job done as well.
hdi2_1 <- transmute(hdi2, HDI.FM = HDI.F / HDI.M) 
hdi2_1 <- bind_cols(hdi2_1, transmute(hdi2, Life.expFM = Life.exp.F / Life.exp.M))
hdi2_1 <- bind_cols(hdi2_1, transmute(hdi2, Exp.SchoolFM = Exp.school.F/Exp.school.M))
hdi2_1 <- bind_cols(hdi2_1, transmute(hdi2, Mean.SchoolFM = Mean.School.F./Mean.School.M))
hdi2_1 <- bind_cols(hdi2_1, transmute(hdi2, Est.GNIFM = Est.GNI.F / Est.GNI.M))
head(hdi2_1)
hdi3_1 <- transmute(hdi3, Edu2.FM = Edu2.F/Edu2.M)
hdi3_1 <- bind_cols(hdi3_1, transmute(hdi3, Labour.FM = Labour.F/Labour.M))
str(hdi1)
hdi.aid <- bind_cols(hdi2_1, hdi3_1)
names(hdi1)[names(hdi1) == "HDI."] <- "HDI"
hdi.aid <- select(hdi.aid, -Country)

hdi.aid <- bind_cols(hdi1, hdi.aid)
str(hdi.aid)
#Sorting alphabetically by country name so it is easier to combine
#with other data.
hdi.aid2 <- hdi.aid[order(hdi.aid$Country),]

#Loading the Gapminder women's fertility data, selecting the year 20
#2014 and deleting missing values.
fert <- read.xlsx2("indicator undata total_fertility_1.xlsx", sheetIndex=1)
str(fert)
keep_columns <- c("Country", "X2014")
fert2014 <- select(fert, one_of(keep_columns))
fert2014$X2014 <- as.numeric(levels(fert2014$X2014))[fert2014$X2014]
fert2014$Country <- as.character(fert2014$Country)
fert2014$miss <- complete.cases(fert2014)
fert2014_1 <- filter(fert2014, miss==TRUE)
fert2014_1 <- select(fert2014_1, -miss)
names(hdi.aid3)[names(hdi.aid3) == "X2014"] <- "Fert"

#Combining the data
hdi.aid3 <- inner_join(hdi.aid2, fert2014_1, by = "Country")
View(hdi.aid3)

#Loading Gapminder's data on foreign aid received provided by World Bank
#The point of this data is to get an indication of aid received which 
#which would have had time to affect the country in question. 
#To this end I will calculate a mean of aid received a 20 year
#period to year 2004, which is 10 years before
#the other data. Also a column that tells the amount of years that mean is
#calculated from, to give an indication of how consistent the aid
#has been.

aid <- read.xlsx2("indicator_aid received % of gni_1.xlsx", sheetIndex=1)
aid2 <- select(aid, X1984:X2004)

indx <- sapply(aid2, is.factor)
aid2[indx] <- lapply(aid2[indx], function(x) as.numeric(as.character(x)))

str(aid2)
aid$mean04 <- rowMeans(aid2, na.rm=TRUE)
mean.length <- apply(aid2, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]) )
aid$mean.length <- mean.length
keep_columns2 <- c("Country", "mean04", "mean.length")
aid3 <- select(aid, one_of(keep_columns2))
View(aid3)

#Combining data.

hdi.aid4 <- inner_join(hdi.aid3, aid3, by = "Country")
View(hdi.aid4)

#Loading under 5 child mortality/1000 live births for the year
#2014 from Gapminder and combining it o the dataset.

mort <- read.xlsx2("indicator gapminder under5mortality_1.xlsx", sheetIndex = 1)
keep_columns3 <- c("Country", "X2014")
child.mort <- select(mort, one_of(keep_columns3))
names(child.mort)[names(child.mort) == "X2014"] <- "child.mort"
View(child.mort)
child.mort$Country <- as.character(child.mort$Country)
child.mort$child.mort <- as.numeric(child.mort$child.mort)
str(child.mort)

#Combining data

hdi.aid5 <- inner_join(hdi.aid4, child.mort, by = "Country")
View(hdi.aid5)
#Saving the dataset

write.csv2(hdi.aid5, "hdi_aid.csv")