#calling libraries
library(dplyr)
library(tidyr)

#set working directory
setwd("C:/Users/katie/Desktop/policing/Austin Dirty Datasets")

#Call datasets Austin (citations, use of force)
UOF15<-read.csv(file='R2R_2015.csv', stringsAsFactors = FALSE)
UOF14<-read.csv(file='R2R_2014.csv', stringsAsFactors = FALSE)
UOF16<-read.csv(file='R2R_2016.csv', stringsAsFactors = FALSE)
UOF17<-read.csv(file='R2R_2017.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='R2R_2018.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='Racial_Profiling_Dataset_2015-_Citations.csv', stringsAsFactors = FALSE)
citations14<-read.csv(file='2014_RP__Citations.csv', stringsAsFactors = FALSE)
citations16<-read.csv(file='2016_RP__Citations.csv', stringsAsFactors = FALSE)
citations17<-read.csv(file='2017_RP__Citations.csv', stringsAsFactors = FALSE)
Citations18<-read.csv(file='2018_RP__Citations.csv', stringsAsFactors = FALSE)
Shooting_Incidents<-read.csv(file='Officer_Involved_Shootings_2008-17_Incidents.csv', stringsAsFactors = FALSE)
Shooting_officers<-read.csv(file='Officer_Involved_Shootings_2008-17_Officers.csv', stringsAsFactor = FALSE)
Shooting_Subjects<-read.csv(file='2008-17_OIS_Subjects.csv', stringsAsFactors = FALSE)


#numerize the data
#splitting year and date into  different variables UOF15
SplitDateTime_UOF15<-strsplit(as.character(UOF15$Date.Occurred), "\\s")
SplitDateTime_UOF15<-do.call(rbind, SplitDateTime_UOF15)
colnames(SplitDateTime_UOF15)<-(c("date", "time"))
SplitDateTime_UOF15<-as.data.frame(SplitDateTime_UOF15, stringsAsFactors=FALSE)
#making a tables with relevant metadata
ALLMetadata_UOF15<-cbind.data.frame(UOF15$RIN,SplitDateTime_UOF15$date,UOF15$Time.Occurred,UOF15$Subject.Race,UOF15$Subject.Sex,UOF15$Officer.Yrs.of.Service,UOF15$Officer.Commission.Date,UOF15$Officer.Organization.Desc,UOF15$Area.Command, stringAsFactors=FALSE)
colnames(ALLMetadata_UOF15)<-(c("RIN", "date","time","subject race","subject sex","officer Yrs of service","officer commision date","officer organization Desc","Area Command"))

#make all null values = NA for UOF
ALLMetadata_UOF15_NA<-ALLMetadata_UOF15
ALLMetadata_UOF15_NA[ALLMetadata_UOF15=="u"]<-NA
ALLMetadata_UOF15_NA[ALLMetadata_UOF15==""]<-NA
ALLMetadata_UOF15_NA[ALLMetadata_UOF15=="-1"]<-NA

#changing race to be words instead of single letters & to match Citations
AllMetadata_UOF15_FixRace<-ALLMetadata_UOF15_NA
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="W"]<-("White")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="B"]<-("Black")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="A"]<-("Asian")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="H"]<-("Hispanic")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="I"]<-("Native American")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="M"]<-("Middle Eastern")
AllMetadata_UOF15_FixRace[ALLMetadata_UOF15=="P"]<-("Hawaiian or Pacific Islander")

#UOF14
SplitDateTime_UOF14<-strsplit(as.character(UOF14$Date.Occurred), "\\s")
SplitDateTime_UOF14<-do.call(rbind, SplitDateTime_UOF14)
colnames(SplitDateTime_UOF14)<-(c("date", "time"))
SplitDateTime_UOF14<-as.data.frame(SplitDateTime_UOF14, stringsAsFactors=FALSE)
#table with relevant data for 2014
ALLMetadata_UOF14<-cbind.data.frame(UOF14$RIN,SplitDateTime_UOF14$date,UOF14$Time.Occurred,UOF14$Subject.Race,UOF14$Subject.Sex,UOF14$Officer.Yrs.of.Service,UOF14$Officer.Commission.Date,UOF14$Officer.Organization.Desc,UOF14$Area.Command, stringsAsFactors=FALSE)
colnames(ALLMetadata_UOF14)<-(c("RIN","Date","Time","Subject Race","Subject Sex","Officer Yrs of Service","Officer Commision Date","Officer Organization Desc","Area Command"))



#separating date and time in citations to get rid of unnecessary 00:00
SplitDateTime_Citations<-strsplit(as.character(citations$OFF.FROM.DATE), "\\s")
SplitDateTime_Citations<-do.call(rbind, SplitDateTime_Citations)
colnames(SplitDateTime_Citations)<-(c("date","time"))
SplitDateTime_Citations<-cbind.data.frame(SplitDateTime_Citations, stringsAsFactors=FALSE)

#making a table with all relevant metadata for citations
AllMetadata_Citations<-cbind.data.frame(SplitDateTime_Citations$date,citations$OFF.TIME,citations$RACE.ORIGIN.CODE,citations$CASE.PARTY.SEX, stringsAsFactors=FALSE)
colnames(AllMetadata_Citations)<-(c("date","time","Subject Race","Subject gender"))

#make all empty values = NA
ALLMetadata_Citations_NA<-AllMetadata_Citations
ALLMetadata_Citations_NA[AllMetadata_Citations==""]<-NA
ALLMetadata_Citations_NA[AllMetadata_Citations=="O"]<-NA

#changing race to be words
AllMetadata_citations_FixRace<-ALLMetadata_Citations_NA
AllMetadata_citations_FixRace[AllMetadata_Citations=="W"]<-("White")
AllMetadata_citations_FixRace[AllMetadata_Citations=="B"]<-("Black")
AllMetadata_citations_FixRace[AllMetadata_Citations=="A"]<-("Asian")
AllMetadata_citations_FixRace[AllMetadata_Citations=="H"]<-("Hispanic")
AllMetadata_citations_FixRace[AllMetadata_Citations=="ME"]<-("Middle Eastern")
AllMetadata_citations_FixRace[AllMetadata_Citations=="N"]<-("Native American")

