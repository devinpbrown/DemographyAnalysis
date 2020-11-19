library(readxl)
library(WDI)
library(countrycode)
library(tidyverse)

#getting my data and cleaning it for analysis
setwd("~/Dropbox/PhDApps/WritingSample/data")

#getting Polity Data and cleaning
polity <- read_xls('p5v2018.xls')
politycut1 <- polity[polity$year >= 1960,] #dropping years before 1960 and after 2003 (lower bound because of data availability from WB, upper bound becuase of MAR data)
polity <- politycut1[politycut1$year <= 2003,]
unique(polity$year)
head(polity)
polity <- polity[, -2] #dropping country to make merge only on ccode and year

#importing MAR data
mardata <- read.csv('output2.csv')
mardata <- mardata[mardata$year >= 1960,] #dropping values from 1940-1960 since WB doesn't have data pre 1960
head(mardata)

#getting WB Data
wbdata <- WDI(country = 'all', indicator = c('NY.GDP.PCAP.CD', 'SP.POP.TOTL'), start=1960, end = 2003, extra=T) #getting gdp/capita and country pop
wbdata <- wbdata[c('iso3c', 'year', 'NY.GDP.PCAP.CD', 'SP.POP.TOTL')] #getting rid of extra columns the were downloaded in the first call


wbdata$ccode = countrycode(sourcevar = wbdata$iso3c, origin = "iso3c", destination = "cown", nomatch = NULL) #converting country code from WBdata to COW code
wbdata <- wbdata[, -1] #dropping old iso3c country code
wbdata <- wbdata[!is.na(wbdata$ccode),] #dropping missing data for aggregate groups dowloaded with wbdata
wbdata <- wbdata %>% rename(gdppc = 'NY.GDP.PCAP.CD', country_pop = 'SP.POP.TOTL')
head(wbdata)

#merging data 
first_merged <- full_join(mardata, polity)
final_df <- full_join(first_merged, wbdata)

final_df[final_df==-99] = NA #changing -99s to NA
final_df$rebellion = ifelse(final_df$year>=1985, final_df$reb, final_df$rebel) #creating a rebellion column with the combined coverage of both MAR's reb and rebellion score
final_df$gdppc = scale(final_df$gdppc) #scaling both continuous variables
final_df$country_pop = scale(final_df$country_pop) 
final_df <- final_df[-c(7, 11, 12, 13)] #dropping columns not necessary for the analysis
final_df = final_df[, c(1,2,3,4,5,6,9,13,7,8,10,11,12)] #reordering columns to have outcomes at the beginning
write_csv(final_df, '~/Dropbox/PhDApps/WritingSample/finaldata.csv')

