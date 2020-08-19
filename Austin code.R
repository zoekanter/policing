#calling libraries
library(dplyr)
library(tidyr)
library(gtools)

#set working directory
setwd("C:/Users/katie/Desktop/policing/Austin Dirty Datasets")

#Call datasets Austin (citations, use of force)
UOF15<-read.csv(file='R2R_2015.csv', stringsAsFactors = FALSE)
UOF14<-read.csv(file='R2R_2014.csv', stringsAsFactors = FALSE)
UOF16<-read.csv(file='R2R_2016.csv', stringsAsFactors = FALSE)
UOF17<-read.csv(file='R2R_2017.csv', stringsAsFactors = FALSE)
UOF18<-read.csv(file='R2R_2018.csv', stringsAsFactors = FALSE)
citations15<-read.csv(file='Racial_Profiling_Dataset_2015-_Citations.csv', stringsAsFactors = FALSE)
citations14<-read.csv(file='2014_RP__Citations.csv', stringsAsFactors = FALSE)
citations16<-read.csv(file='2016_RP__Citations.csv', stringsAsFactors = FALSE)
citations17<-read.csv(file='2017_RP__Citations.csv', stringsAsFactors = FALSE)
Citations18<-read.csv(file='2018_RP__Citations.csv', stringsAsFactors = FALSE)
Shooting_Incidents<-read.csv(file='Officer_Involved_Shootings_2008-17_Incidents.csv', stringsAsFactors = FALSE)
Shooting_officers<-read.csv(file='Officer_Involved_Shootings_2008-17_Officers.csv', stringsAsFactor = FALSE)
Shooting_Subjects<-read.csv(file='2008-17_OIS_Subjects.csv', stringsAsFactors = FALSE)


#making columns the same so they merge easily
UOF14_Fix_Col<-subset(UOF14, select=-c(Subject.Effects))
UOF15_Fix_COl<-subset(UOF15, select=-c(Subject.Effects))
UOF16_Fix_Col<-subset(UOF16, select=-c(Subject.Effects))
UOF17_Fix_Col<-UOF17[, -c(29:32)]
UOF18_Fix_Col<-UOF18[, -c(29:32)]

#changing column names to match
UOF16_Fix_Col<-UOF16_Fix_Col %>% rename(Date.Occurred = Date..Occurred,
                                        Time.Occurred = Time..Occurred,
                                        Area.Command = Area..Command,
                                        Subject.Sex = Subject..Sex,
                                        Subject.Race = Subject..Race,
                                        Subject.Ethnicity = Subject..Ethnicity,
                                        Number.Shots = Number..Shots,
                                        Officer.Organization.Desc = Officer..Organization.Desc,
                                        Officer.Commission.Date = Officer..Commission.Date)
UOF17_Fix_Col<-UOF17_Fix_Col %>% rename(Date.Occurred = Date..Occurred,
                         Time.Occurred = Time..Occurred,
                         Area.Command = Area..Command,
                         Subject.Sex = Subject..Sex,
                         Subject.Race = Subject..Race,
                         Subject.Ethnicity = Subject..Ethnicity,
                         Number.Shots = Number..Shots,
                         Officer.Organization.Desc = Officer..Organization.Desc,
                         Officer.Commission.Date = Officer..Commission.Date)


UOF18_Fix_Col<-UOF18_Fix_Col%>% rename(Date.Occurred = Date..Occurred,
                        Time.Occurred = Time..Occurred,
                        Area.Command = Area..Command,
                        Subject.Sex = Subject..Sex,
                        Subject.Race = Subject..Race,
                        Subject.Ethnicity = Subject..Ethnicity,
                        Number.Shots = Number..Shots,
                        Officer.Organization.Desc = Officer..Organization.Desc,
                        Officer.Commission.Date = Officer..Commission.Date)


#bind them all together
UOF_ALL<-smartbind(UOF14_Fix_Col,UOF15_Fix_COl,UOF16_Fix_Col,UOF17_Fix_Col,UOF18_Fix_Col)


#numerize the data
#splitting year and date into  different variables
SplitDateTime_UOF_ALL<-strsplit(as.character(UOF_ALL$Date.Occurred), "\\s")
SplitDateTime_UOF_ALL<-do.call(rbind, SplitDateTime_UOF_ALL)
colnames(SplitDateTime_UOF_ALL)<-(c("date", "time"))
SplitDateTime_UOF_ALL<-as.data.frame(SplitDateTime_UOF_ALL, stringsAsFactors=FALSE)
#making a tables with relevant metadata
ALLMetadata_UOF<-cbind.data.frame(UOF_ALL$RIN,SplitDateTime_UOF_ALL$date,UOF_ALL$Time.Occurred,UOF_ALL$Subject.Race,UOF_ALL$Subject.Ethnicity,UOF_ALL$Subject.Sex,UOF_ALL$Officer.Yrs.of.Service,UOF_ALL$Officer.Commission.Date,UOF_ALL$Officer.Organization.Desc,UOF_ALL$Area.Command, stringAsFactors=FALSE)
colnames(ALLMetadata_UOF)<-(c("RIN", "date","time","subject race","Subject Ethnicity","subject sex","officer Yrs of service","officer commision date","officer organization Desc","Area Command"))

#make all null values = NA for UOF
ALLMetadata_UOF_NA<-ALLMetadata_UOF_ALL
ALLMetadata_UOF_NA[ALLMetadata_UOF_ALL=="u"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF_ALL==""]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF_ALL=="-1"]<-NA
ALLMetadata_UOF_NA[ALLMetadata_UOF_ALL=="88"]<-NA

#changing race to be words instead of single letters & to match Citations
AllMetadata_UOF_FixRace<-ALLMetadata_UOF_NA
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="W"]<-("White")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="B"]<-("Black")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="A"]<-("Asian")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="H"]<-("Hispanic")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="I"]<-("Native American")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="M"]<-("Middle Eastern")
AllMetadata_UOF_FixRace[ALLMetadata_UOF_ALL=="P"]<-("Hawaiian or Pacific Islander")


#fixing column names to match for easy combining
Citations18<-Citations18%>% rename(OFF.FROM.DATE = OffenseDate,
                                   CITATION.NUMBER = Citation.Number,
                                     OFF.TIME = OffenseTime,
                                     RACE.ORIGIN.CODE = Race,
                                     CASE.PARTY.SEX = Sex,
                                     RACE.KNOWN = Race_Known,)


#combining all citations data
Citations_ALL<-smartbind(citations14,citations15,citations16,citations17,Citations18)


#separating date and time in citations to get rid of unnecessary 00:00
SplitDateTime_Citations<-strsplit(as.character(Citations_ALL$OFF.FROM.DATE), "\\s")
SplitDateTime_Citations<-do.call(rbind, SplitDateTime_Citations)
colnames(SplitDateTime_Citations)<-(c("date","time"))
SplitDateTime_Citations<-cbind.data.frame(SplitDateTime_Citations, stringsAsFactors=FALSE)

#making a table with all relevant metadata for citations
AllMetadata_Citations<-cbind.data.frame(SplitDateTime_Citations$date,Citations_ALL$OFF.TIME,Citations_ALL$RACE.ORIGIN.CODE,Citations_ALL$CASE.PARTY.SEX, stringsAsFactors=FALSE)
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


#combining shootings into one database with all info together (multiple entries for single case numbers problem, also definitely a better way to do this)
OIS_ALL<-merge(Shooting_officers,Shooting_Subjects)
OIS_ALL<-merge(OIS_ALL,Shooting_Incidents)

#make a table with available data
AllMetadata_Shootings<-cbind.data.frame(OIS_ALL$Case..,OIS_ALL$Date,OIS_ALL$Time,OIS_ALL$Officer.Name,OIS_ALL$Rank,OIS_ALL$Officer.Race.Ethnicity,OIS_ALL$Officer.Gender,OIS_ALL$Prev.OIS,OIS_ALL$Subject.Race.Ethnicity,OIS_ALL$Subject.Gender, stringsAsFactors = FALSE)
colnames(AllMetadata_Shootings)<-(c("Case","Date","Time","Officer Name","Officer Rank","Officer Race","Officer Gender","Officer Prev. OIS","Subject Race","Subject Gender"))

