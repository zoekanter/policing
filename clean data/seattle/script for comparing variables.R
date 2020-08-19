#first I want to call libraries
library(dplyr)
library(tidyr)

#turn off scientific notation
options(scipen=999)

#set working directory
setwd("C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing")
source("functions file.R")

#I want to call in my datasets (citations, use of force).
UOF<-read.csv(file='use_of_force_Seattle.csv', stringsAsFactors = FALSE)
citations<-read.csv(file='citations_seattle.csv', stringsAsFactors = FALSE)


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
AllMetadata_UOF_FixSector[AllMetadata_UOF_FixSector==""]<-NA
AllMetadata_UOF_Standardized<-cbind.data.frame(AllMetadata_UOF_FixRace$date,AllMetadata_UOF_FixRace$precinct,AllMetadata_UOF_FixSector$sector, AllMetadata_UOF_FixRace$gender, AllMetadata_UOF_FixRace$race)
colnames(AllMetadata_UOF_Standardized)<-c("date","precinct", "sector", "gender","race")

#combine date, precinct, race, gender
CreateUniqueIdentifierCode_UOF<-unite(AllMetadata_UOF_Standardized, dpsrg, sep=",")

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
#making a table with all relevant metadata for citations
AllMetadata_Citations<-cbind.data.frame(FixDate_Citations$date,FixTime_Citations$time,citations$Precinct,citations$Sector,citations$Beat,citations$Subject.Perceived.Gender,citations$Subject.Perceived.Race, stringsAsFactors=FALSE)
colnames(AllMetadata_Citations)<-(c("date","time","precinct","sector","beat","gender","race"))

#make all null/error values = NA for citations
AllMetadata_Citations_NA<-AllMetadata_Citations
AllMetadata_Citations_NA[AllMetadata_Citations=="FK ERROR"]<-NA
AllMetadata_Citations_NA[AllMetadata_Citations=="OOJ"]<-NA
AllMetadata_Citations_NA[AllMetadata_Citations=="Unknown"]<-NA

#formatting of 99 and sector is weird. Fix by only taking the first value.
AllMetadata_Citations_NA$sector<-substr(AllMetadata_Citations_NA$sector,0,1)

#Back to NA replace
AllMetadata_Citations_NA[AllMetadata_Citations=="9"]<-NA
AllMetadata_Citations_NA[AllMetadata_Citations=="-"]<-NA
AllMetadata_Citations_NA[AllMetadata_Citations=="Unable to Determine"]<-NA


#sometimes spelled Southwest, sometimes SouthWest
AllMetadata_Citations_CleanPrecinct<-AllMetadata_Citations_NA
AllMetadata_Citations_CleanPrecinct[AllMetadata_Citations_CleanPrecinct=="SouthWest"]<-"Southwest"

#Make dataset with just date, precinct, sector, gender, race
Metadata_Citations_Standardized<-cbind.data.frame(AllMetadata_Citations_CleanPrecinct$date,AllMetadata_Citations_CleanPrecinct$precinct,AllMetadata_Citations_CleanPrecinct$sector,AllMetadata_Citations_CleanPrecinct$gender,AllMetadata_Citations_CleanPrecinct$race)
colnames(Metadata_Citations_Standardized)<-(c("date","precinct","sector","gender","race"))

#combine date, precinct, race, gender
CreateUniqueIdentifierCode_Citations<-unite(Metadata_Citations_Standardized, dpsrg, sep=",")



#inner_join the two datasets to see if there are any rows with identical values
TestOverlapInUniqueID<-inner_join(CreateUniqueIdentifierCode_UOF, CreateUniqueIdentifierCode_Citations)

#look at it
print(TestOverlapInUniqueID)
#how many repeat cases?
unique(TestOverlapInUniqueID)

#look at how many have only 1 case
OneCaseOverlapUniqueID <-  TestOverlapInUniqueID %>% group_by(dpsrg) %>% filter(n()==1) #
#There are not enough cases that overlap, so I need to look at the datasets and see what columns contribute most to our overlap ID


#Now I want to use a function I made that will spit out for each column: 1) how many NAs and 2) how many of those NAs are unique to the column (i.e. do not overlap with another column's NA)
UOF<-createNAChart(AllMetadata_UOF_Standardized)
Citations<-createNAChart(Metadata_Citations_Standardized)
                   