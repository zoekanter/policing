#first I want to call libraries
library(dplyr)
library(tidyr)

#set working directory
setwd("C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/dirty data")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='use_of_force_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_seattle.csv', stringsAsFactors = FALSE)
shootings<-read.csv(file='Officer_involved_shooting_Seattle.csv', stringsAsFactors = FALSE)

#numericize the data
#first split year/date into two separate variables
SplitDateTime_UOF<-strsplit(as.character(UOF$Occured_date_time),"\\s")
SplitDateTime_UOF<-do.call(rbind, SplitDateTime_UOF)
colnames(SplitDateTime_UOF)<-(c("date","time"))
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)
#making a table with all relevant metadata for UOF
AllMetadata_UOF<-cbind.data.frame(SplitDateTime_UOF$date,SplitDateTime_UOF$time,UOF$Precinct,UOF$Sector,UOF$Beat,UOF$Subject_Gender,UOF$Subject_Race, stringsAsFactors=FALSE)
colnames(AllMetadata_UOF)<-(c("date","time","precinct","sector","beat","gender","race"))

#make all null values = NA for UOF
AllMetadata_UOF_NA<-AllMetadata_UOF
AllMetadata_UOF_NA[AllMetadata_UOF=="Not Specified"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="-"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="X"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="2300 BLOCK CALIFORNIA AV SW"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="2700 BLOCK UTAH AV S"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="4200 BLOCK E MADISON ST"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="500 BLOCK S MAIN ST"]<-NA
AllMetadata_UOF_NA[AllMetadata_UOF=="5200 BLOCK UTAH AV S"]<-NA

#Change race to be consisted with citations
AllMetadata_UOF_FixRace<-AllMetadata_UOF_NA
AllMetadata_UOF_FixRace[AllMetadata_UOF=="American Indian/Alaska Native"]<-("American Indian or Alaska Native")
AllMetadata_UOF_FixRace[AllMetadata_UOF=="Nat Hawaiian/Oth Pac Islander"]<-("Native Hawaiian or Other Pacific Islander")
AllMetadata_UOF_FixRace[AllMetadata_UOF=="Hispanic or Latino"]<-"Hispanic"

#There are two of each letter in sector. Here I'm trying to make all the letters the same. 
AllMetadata_UOF_FixSector<-AllMetadata_UOF_FixRace
AllMetadata_UOF_FixSector<-as.data.frame(substr(AllMetadata_UOF_NA$sector, 0,1),stringsAsFactors = FALSE)
colnames(AllMetadata_UOF_FixSector)<-c("sector")
AllMetadata_UOF_Standardized<-cbind.data.frame(AllMetadata_UOF_FixRace$date,AllMetadata_UOF_FixRace$precinct,AllMetadata_UOF_FixSector$sector, AllMetadata_UOF_FixRace$gender, AllMetadata_UOF_FixRace$race)
colnames(AllMetadata_UOF_Standardized)<-c("date","precinct", "sector", "gender","race")

#need to reorder date for citations to follow m/d/y format
FixDate_Citations<-strsplit(as.character(citations$Reported.Date),"T")
FixDate_Citations<-do.call(rbind, FixDate_Citations)
FixDate_Citations<-FixDate_Citations[,1]
FixDate_Citations<-strsplit(as.character(FixDate_Citations),"-")
FixDate_Citations<-do.call(rbind, FixDate_Citations)
FixDate_Citations<-FixDate_Citations[,c(2,3,1)]
colnames(FixDate_Citations)<-(c("1","2","3"))
FixDate_Citations<-as.data.frame(FixDate_Citations)
FixDate_Citations<-unite(FixDate_Citations,year,sep="/")
colnames(FixDate_Citations)<-c("date")
FixTime_Citations<-strsplit(as.character(citations$Reported.Time),":")
FixTime_Citations<-do.call(rbind,FixTime_Citations)
FixTime_Citations<-as.data.frame(FixTime_Citations)
FixTime_Citations<-unite(FixTime_Citations,FixTime_Citations,(V1:V2),sep=":")
colnames(FixTime_Citations)<-c("time")



#make all null/error values = NA for citations
AllMetadata_Citations_NA<-citations
AllMetadata_Citations_NA[citations=="FK ERROR"]<-NA
AllMetadata_Citations_NA[citations=="OOJ"]<-NA
AllMetadata_Citations_NA[citations=="Unknown"]<-NA
AllMetadata_Citations_NA[citations=="Other"]<-NA
AllMetadata_Citations_NA[citations=="Unable to Determine"]<-NA
AllMetadata_Citations_NA[citations=="-"]<-NA


#formatting of 99 and sector is weird. Fix by only taking the first value.
AllMetadata_Citations_NA$sector<-substr(AllMetadata_Citations_NA$sector,0,1)

#Back to NA replace
AllMetadata_Citations_NA[AllMetadata_Citations=="9"]<-NA


#sometimes spelled Southwest, sometimes SouthWest
AllMetadata_Citations_CleanPrecinct<-AllMetadata_Citations_NA
AllMetadata_Citations_CleanPrecinct[AllMetadata_Citations_CleanPrecinct=="SouthWest"]<-"Southwest"

#Make dataset with all updated data
AllMetadata_Citations_NA$Reported.Date<-FixDate_Citations$date
AllMetadata_Citations_NA$Reported.Time<-FixTime_Citations[,1]
AllMetadata_Citations_NA$Precinct<-AllMetadata_Citations_CleanPrecinct$Precinct

#change WD
write.csv(AllMetadata_Citations_NA,"C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data\\citations_Seattle.csv",row.names = FALSE)
