#calling libraries
library(dplyr)
library(tidyr)

#turn off scientific notation
options(scipen = 999)

#set working directory
setwd("C:/Users/katie/Desktop/policing/dirty data/Austin")
source("Austin Functions File.R")

#Calling two datasets
UOF15<-read.csv(file='R2R_2015.csv', stringsAsFactors = FALSE)
Citations15<-read.csv(file='Racial_Profiling_Dataset_2015-_Citations.csv', stringsAsFactors = FALSE)


#splitting year and date into  different variables
SplitDateTime_UOF<-strsplit(as.character(UOF15$Date.Occurred), "\\s")
SplitDateTime_UOF<-do.call(rbind, SplitDateTime_UOF)
colnames(SplitDateTime_UOF)<-(c("date", "time"))
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)
#making a tables with relevant metadata
ALLMetadata_UOF<-cbind.data.frame(UOF15$RIN,SplitDateTime_UOF$date,UOF15$Time.Occurred,UOF15$Subject.Race,UOF15$Subject.Ethnicity,UOF15$Subject.Sex,UOF15$Officer.Yrs.of.Service,UOF15$Officer.Commission.Date,UOF15$Officer.Organization.Desc,UOF15$Area.Command, stringAsFactors=FALSE)
colnames(ALLMetadata_UOF)<-(c("RIN", "date","time","subject race","Subject Ethnicity","subject sex","officer Yrs of service","officer commision date","officer organization Desc","Area Command"))

#make null values = NA
AllMetadata_UOF_NA<-ALLMetadata_UOF
AllMetadata_UOF_NA[ALLMetadata_UOF=="U"]<-NA
AllMetadata_UOF_NA[ALLMetadata_UOF==""]<-NA

#fix race to words not letters
AllMetadata_UOF_FixRace<-ALLMetadata_UOF_NA
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="W"]<-("White")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="B"]<-("Black")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="A"]<-("Asian")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="H"]<-("Hispanic")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="I"]<-("Native American")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="M"]<-("Middle Eastern")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="P"]<-("Hawaiian or Pacific Islander")


#combining
createUniqueIdentifierCode_UOF<-unite(AllMetadata_UOF_FixRace, dpsrg, sep=",")


#separating date and time in citations to get rid of unnecessary 00:00
SplitDateTime_Citations<-strsplit(as.character(citations15$OFF.FROM.DATE), "\\s")
SplitDateTime_Citations<-do.call(rbind, SplitDateTime_Citations)
colnames(SplitDateTime_Citations)<-(c("date","time"))
SplitDateTime_Citations<-cbind.data.frame(SplitDateTime_Citations, stringsAsFactors=FALSE)

#making a table with all relevant metadata for citations
AllMetadata_Citations<-cbind.data.frame(SplitDateTime_Citations$date,Citations15$OFF.TIME,Citations15$RACE.ORIGIN.CODE,Citations15$CASE.PARTY.SEX, stringsAsFactors=FALSE)
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

#combine all columns
createUniqueIdentifierCode_Citations<-unite(AllMetadata_citations_FixRace, dpsrg, sep=",")


#inner join the two data sets to see if there are any rows with identical values
TestOverLapInUniqueID<-inner_join(createUniqueIdentifierCode_UOF, createUniqueIdentifierCode_Citations)

#look at it
print(TestOverLapInUniqueID)

UOF<-createNAChart(AllMetadata_UOF_FixRace)
Citations<-createNAChart(AllMetadata_citations_FixRace)





