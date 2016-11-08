setwd("D:/tuition/")

# Canadian Undergraduate Student Tuition
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/04770021-eng.zip", 
              destfile = "Data/tuition.zip")
unzip("Data/tuition.zip", exdir = "Data")
file.rename("Data/04770021-eng.csv", "Data/CanadianTuition.csv")

CanadianTuition <- read.csv("Data/CanadianTuition.csv")
CanadianTuition$Ref_Date <- as.numeric(as.character(substr(CanadianTuition$Ref_Date, 1, 4)))

CanadianTuition <- CanadianTuition[CanadianTuition$GEO == "Canada", ]
CanadianTuition$Value <- as.numeric(as.character(CanadianTuition$Value))

Year <- seq(min(CanadianTuition$Ref_Date), max(CanadianTuition$Ref_Date), 1)
canadaTS <- sapply(Year, function(x) median(CanadianTuition$Value[CanadianTuition$Ref_Date == x]))
diffcanadaTS <- diff(canadaTS)
#acf(diffcanadaTS)

plot(canadaTS)

# International Undergraduate Student Tuition
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/04770023-eng.zip",
              destfile = "Data/tuitionInternational.zip")
unzip("Data/tuitionInternational.zip", exdir = "Data")
file.rename("Data/04770023-eng.csv", "Data/InternationalTuition.csv")

InternationalTuition <- read.csv("Data/InternationalTuition.csv")
InternationalTuition$Ref_Date <- as.numeric(as.character(substr(InternationalTuition$Ref_Date, 1, 4)))

InternationalTuition <- InternationalTuition[InternationalTuition$GEO == "Canada", ]
InternationalTuition$Value <- as.numeric(as.character(InternationalTuition$Value))

Year <- seq(min(InternationalTuition$Ref_Date), max(InternationalTuition$Ref_Date), 1)
internationalTS <- sapply(Year, function(x) median(InternationalTuition$Value[InternationalTuition$Ref_Date == x]))
diffinternationalTS <- diff(internationalTS)
#acf(diffinternationalTS)

library(dplyr)

canadaIncrease <- canadaTS / lag(canadaTS, 1) - 1
internationalIncrease <- internationalTS / lag(internationalTS, 1) - 1

plot(y = tail(canadaIncrease, -1), 
     type = 'l', 
     col = 'black', 
     ylim = c(min(c(tail(canadaIncrease, -1), tail(internationalIncrease, -1))),
              max(c(tail(canadaIncrease, -1), tail(internationalIncrease, -1)))),
     x = 2007:2016, xlab = "Year", ylab ="% Increase in Tuition")

lines(y = tail(internationalIncrease, -1), x = 2007:2016,
      type = 'l', col = 'red')

plot(x = 2006:2016, y = internationalTS/canadaTS, xlab = "Year",
     ylab = "Int / Domestic Student Tuition", type = 'l', col = 'blue')
