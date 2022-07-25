library(readxl)
library(tidyverse)
library(dplyr)
library(readr)
library(expss)
library(pastecs)
library(haven)

pop_70s <- read_excel("C:/Users/bigfl/OneDrive/Desktop/Econometrics II/Project/Population 70-80 Formatted.xlsx")
pop_80s <- read_excel("C:/Users/bigfl/OneDrive/Desktop/Econometrics II/Project/Population 80-89 Formatted.xlsx")
politic_data <- read_excel("C:/Users/bigfl/OneDrive/Desktop/Econometrics II/Project/StateElections_Gub_2012_09_06_Public_Version.xlsx")
productivity <- read_excel("C:/Users/bigfl/OneDrive/Desktop/Econometrics II/Project/productivity.xlsx")


#1970s formatting
#create an aggregated dataset for year by state
format70 <- aggregate(pop_70s$"1970",list(pop_70s$STATE),FUN=sum)
format71 <- aggregate(pop_70s$"1971",list(pop_70s$STATE),FUN=sum)
format72 <- aggregate(pop_70s$"1972",list(pop_70s$STATE),FUN=sum)
format73 <- aggregate(pop_70s$"1973",list(pop_70s$STATE),FUN=sum)
format74 <- aggregate(pop_70s$"1974",list(pop_70s$STATE),FUN=sum)
format75 <- aggregate(pop_70s$"1975",list(pop_70s$STATE),FUN=sum)
format76 <- aggregate(pop_70s$"1976",list(pop_70s$STATE),FUN=sum)
format77 <- aggregate(pop_70s$"1977",list(pop_70s$STATE),FUN=sum)
format78 <- aggregate(pop_70s$"1978",list(pop_70s$STATE),FUN=sum)
format79 <- aggregate(pop_70s$"1979",list(pop_70s$STATE),FUN=sum)

#rename the variables to merge the years back together
format70 <- rename(format70,"1970"=x)
format71 <- rename(format71,"1971"=x)
format72 <- rename(format72,"1972"=x)
format73 <- rename(format73,"1973"=x)
format74 <- rename(format74,"1974"=x)
format75 <- rename(format75,"1975"=x)
format76 <- rename(format76,"1976"=x)
format77 <- rename(format77,"1977"=x)
format78 <- rename(format78,"1978"=x)
format79 <- rename(format79,"1979"=x)

#merge everything back into a new data set
pop70_merged <- left_join(format70,format71,by="Group.1")
pop70_merged <- left_join(pop70_merged,format72,by="Group.1")
pop70_merged <- left_join(pop70_merged,format73,by="Group.1")
pop70_merged <- left_join(pop70_merged,format74,by="Group.1")
pop70_merged <- left_join(pop70_merged,format75,by="Group.1")
pop70_merged <- left_join(pop70_merged,format76,by="Group.1")
pop70_merged <- left_join(pop70_merged,format77,by="Group.1")
pop70_merged <- left_join(pop70_merged,format78,by="Group.1")
pop70_merged <- left_join(pop70_merged,format79,by="Group.1")
pop70_merged <- rename(pop70_merged,STATE=Group.1)

#remove the annual data sets created earlier 
remove(format70)
remove(format71)
remove(format72)
remove(format73)
remove(format74)
remove(format75)
remove(format76)
remove(format77)
remove(format78)
remove(format79)


#1980s formatting
#initiate a data set to import the date into
pop80_merged <- pop70_merged["STATE"]
pop80_merged$"1980" <- 0
pop80_merged$"1981" <- 0
pop80_merged$"1982" <- 0
pop80_merged$"1983" <- 0
pop80_merged$"1984" <- 0
pop80_merged$"1985" <- 0
pop80_merged$"1986" <- 0

#Filter out DC
pop80_merged <- filter(pop80_merged,pop80_merged$STATE!="DC")
pop_80s <- filter(pop_80s,pop_80s$STATE!="DC")
pop70_merged <- filter(pop70_merged,pop70_merged$STATE!="DC")

#create iterators to loop through the data sets and find the needed values
i <- 7
n <- 1
while(i<=306){
  
  pop80_merged$`1980`[n] <- pop_80s$`1980`[i]
  pop80_merged$`1981`[n] <- pop_80s$`1981`[i]
  pop80_merged$`1982`[n] <- pop_80s$`1982`[i]
  pop80_merged$`1983`[n] <- pop_80s$`1983`[i]
  pop80_merged$`1984`[n] <- pop_80s$`1984`[i]
  pop80_merged$`1985`[n] <- pop_80s$`1985`[i]
  pop80_merged$`1986`[n] <- pop_80s$`1986`[i]
  
  
  n <- n+1
  i <- i+6
}

#merge the 1970s and 1980s populations
merge_population <- left_join(pop70_merged,pop80_merged,by="STATE")
remove(pop_70s)
remove(pop_80s)
remove(pop70_merged)
remove(pop80_merged)

#Now we need to format this new data set so we can merge it with the productivity data
#create a variable for population
productivity$population <- 0

#Filter out any missing states from population data
merge_population <- filter(merge_population,merge_population$STATE!="ALASKA")
merge_population <- filter(merge_population,merge_population$STATE!="HAWAII")


#loop through both data sets and assign populations for each year
i <- 1
n <- 1
while(i <= nrow(productivity)){
  productivity$population[i] <- merge_population$`1970`[n]
  productivity$population[i+1] <- merge_population$`1971`[n]
  productivity$population[i+2] <- merge_population$`1972`[n]
  productivity$population[i+3] <- merge_population$`1973`[n]
  productivity$population[i+4] <- merge_population$`1974`[n]
  productivity$population[i+5] <- merge_population$`1975`[n]
  productivity$population[i+6] <- merge_population$`1976`[n]
  productivity$population[i+7] <- merge_population$`1977`[n]
  productivity$population[i+8] <- merge_population$`1978`[n]
  productivity$population[i+9] <- merge_population$`1979`[n]
  productivity$population[i+10] <- merge_population$`1980`[n]
  productivity$population[i+11] <- merge_population$`1981`[n]
  productivity$population[i+12] <- merge_population$`1982`[n]
  productivity$population[i+13] <- merge_population$`1983`[n]
  productivity$population[i+14] <- merge_population$`1984`[n]
  productivity$population[i+15] <- merge_population$`1985`[n]
  productivity$population[i+16] <- merge_population$`1986`[n]
  i <- i+17
  n <- n+1
}

#Format and merge the info we need for political leaning
politic <- select(politic_data,STATE,year,govparty_b)
politic <- rename(politic,YR=year)

#merge the data together for the finished data set
final <- left_join(productivity,politic,by=c("STATE","YR"))

#establish other needed variables
final$GSP_capita <- (final$GSP*1000000/final$population)
write_dta(final,"C:/Users/bigfl/OneDrive/Desktop/Econometrics II/Project/final_data.dta")  






write.xlsx

install.packages("writexl")

library(writexl)

write_xlsx(x = AnalyticData_final, path = "daily.xlsx", col_names = TRUE)

summary(AnalyticData_final)

sd(AnalyticData_final$crime)
