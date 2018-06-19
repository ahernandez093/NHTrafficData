library(Hmisc)
library(dplyr)
library(ggplot2)


#Designating 2 Variables I wished to work with.
CountyViolations <- NHData %>%
  select(county_name,violation) %>%
  group_by(county_name)

#Produces view of how many total violations were recorded in each county.
PerCounty <- summarise(number_violations = n(), CountyViolations)

#Bar plot showcasing distribution of violations across all counties.
barplot(table(NHData$county_name), col= "Blue", ylim = c(0, 75000), xlab = "County Name", ylab = "Number of Stops", main="Stops Per County")

#Analyzing Merrimack County because it has the highest number of violations.
MerrimackData <- NHData %>%
  filter(county_name == "Merrimack County")

#Shows Outcome of Stops in Merrimack County
MCStopResult <- table(MerrimackData$stop_outcome)
barplot(MCStopResult, col="light blue",ylim = c(0, 35000), xlab = "Result of Stop", ylab = "Number of Stops",
        main ="Merrimack County Stop Results", border=NA)

#Shows Outcome of Stops in New Hampshire
NHStopResult <- table(NHData$stop_outcome)
barplot(NHStopResult, col="blue", ylim = c(0, 200000), xlab = "Result of Stop", ylab = "Number of Stops",
        main ="New Hampshire Stop Results", border=NA)

MCvsNH <- rbind(NHStopResult, MCStopResult)

MCNHBar <- barplot(MCvsNH, col=c("blue", "light blue"),  ylim = c(0, 250000), xpd = FALSE, xlab = "Result of Stop", ylab = "Number of Stops",
        main ="New Hampshire vs Merrimack County Stop Results", legend=( c("New Hampshire", "Merrimack County")))




#GGPlot of Number of Violations per County, Showing whether driver was instate or out of state.
C <- ggplot(NHData, aes(x=county_name, fill=(out_of_state))) + geom_bar()
C <- C+scale_fill_manual(values = c("Blue", "Light Blue", "Grey"), labs("Out of State"),  labels=c("NH Resident","Non NH Resident","N/A"))
C <- C+ggtitle("Number of Violations Per County")+ xlab("County Name") + ylab("Number of Stops")
C <- C+theme_bw()
#Will add counts to bars, but looks a little overwhelming.
#C <- C+geom_text(stat='count', aes(label=..count..), vjust= -1)
C <- C+ theme(axis.text.x = element_text(angle = 30, hjust =1, face="bold", size=11))
C

#GGPlot of Outcome of Stops in Merrimack County Showing instate or out of state.
M <- ggplot(MerrimackData, aes(x=stop_outcome, fill=out_of_state))+ geom_bar()
M <- M+ggtitle("Results of Stops in Merrimack County")+ xlab("Result of Stop") + ylab("Number of Stops")
M <- M+scale_fill_manual(values = c("Blue", "Light Blue", "Yellow"), labs("Out of State"), labels=c("NH Resident","Non NH Resident","N/A"))
M <- M+ theme_bw()
M


#GGPlot of Outcome of Stops in NH showing instate or out of state.
N <- ggplot(NHData, aes(x=stop_outcome, fill=out_of_state)) + geom_bar()
N <- N+ggtitle("Results of Stops in State of New Hampshire")+ xlab("Result of Stop") + ylab("Number of Stops")
N <- N+scale_fill_manual(values = c("Blue", "Light Blue", "Grey"), labs("Out of State"), labels=c("NH Resident", "Non NH Resident", "N/A"))
N <- N+ theme_bw()
#Will add counts to bars, but looks a little overwhelming.
#N <- N+geom_text(stat='count', aes(label=..count..), vjust= -1, color = "red", size=5)
N
