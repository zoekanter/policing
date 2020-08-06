#first I want to call libraries
library(dplyr)
library(tidyr)
library(stringr)

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
SplitDateTime_UOF<-as.data.frame(SplitDateTime_UOF, stringsAsFactors=FALSE)
colnames(SplitDateTime_UOF)<-(c("date","time"))

#making a table with all relevant metadata for UOF
AllMetadata_UOF<-cbind.data.frame(SplitDateTime_UOF[,1:2],UOF[,1:3],UOF[,5:11], stringsAsFactors=FALSE)

#look at the table
View(AllMetadata_UOF)

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
AllMetadata_UOF_FixSector<-as.data.frame(substr(AllMetadata_UOF_NA$Sector, 0,1),stringsAsFactors = FALSE)
colnames(AllMetadata_UOF_FixSector)<-c("sector")
AllMetadata_UOF_Standardized<-cbind.data.frame(AllMetadata_UOF_FixRace[,1:6],AllMetadata_UOF_FixSector[,1], AllMetadata_UOF_FixRace[,8:12])
colnames(AllMetadata_UOF_Standardized)[7]<-c("sector")

#okay, now we'll export into a new dataset in a clean data folder
write.csv(AllMetadata_UOF_Standardized,"C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data\\UseOfForce_Seattle.csv",row.names = FALSE)


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

#write the file to a new location in clean data folder!
write.csv(AllMetadata_Citations_NA,"C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data\\citations_Seattle.csv",row.names = FALSE)


#finally fix the officer involved shooting dataset. I need to standardize times and races
AllMetadata_shootings<-shootings
AllMetadata_shootings[AllMetadata_shootings=="American Indian/Alaska Native"]<-("American Indian or Alaska Native")
AllMetadata_shootings[AllMetadata_shootings=="Nat Hawaiian/Oth Pac Islander"]<-("Native Hawaiian or Other Pacific Islander")
AllMetadata_shootings[AllMetadata_shootings=="Hispanic or Latino"]<-"Hispanic"
AllMetadata_shootings[AllMetadata_shootings=="Hispanic/Latino"]<-"Hispanic"
AllMetadata_shootings[AllMetadata_shootings=="AI/AN"]<-"American Indian or Alaska Native"
AllMetadata_shootings[AllMetadata_shootings=="Asian/Pacific Islander"]<-"Asian"
AllMetadata_shootings[AllMetadata_shootings=="Black"]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="Black "]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="\nBlack"]<-"Black or African American"
AllMetadata_shootings[AllMetadata_shootings=="Native American"]<-"American Indian or Alaska Native"
AllMetadata_shootings[AllMetadata_shootings=="\nMale"]<-"Male"
AllMetadata_shootings[AllMetadata_shootings=="\nYes"]<-"Yes"
AllMetadata_shootings[AllMetadata_shootings=="\nNo"]<-"No"
AllMetadata_shootings[AllMetadata_shootings=="On"]<-"No"
AllMetadata_shootings[AllMetadata_shootings=="Within Policy "]<-"Justified"
AllMetadata_shootings[AllMetadata_shootings=="Within Policy"]<-"Justified"
AllMetadata_shootings[AllMetadata_shootings=="Out of Policy"]<-"Not Justified"

#add a colon 2 spaces into time
shooting_hour<-as.numeric(substr(AllMetadata_shootings$Time,1,nchar(AllMetadata_shootings$Time)-2))
shooting_minute<-as.numeric(substr(AllMetadata_shootings$Time,nchar(AllMetadata_shootings$Time)-1,nchar(AllMetadata_shootings$Time)))
shooting_time<-as.data.frame(cbind(shooting_hour,shooting_minute))

#add 0s to time
shooting_time$shooting_hour<-str_pad(shooting_time$shooting_hour, 2, pad = "0")
shooting_time$shooting_minute<-str_pad(shooting_time$shooting_minute, 2, pad = "0")
shooting_time<-unite(shooting_time,time,sep=":")

#now make all missings = NA
AllMetadata_shootings[AllMetadata_shootings=="MISSING"]<-NA
AllMetadata_shootings[AllMetadata_shootings==""]<-NA
AllMetadata_shootings[AllMetadata_shootings=="\nUnknown"]<-NA
AllMetadata_shootings[AllMetadata_shootings=="Missing"]<-NA


#combine data into one dataset
AllMetadata_shootings_clean<-cbind.data.frame(AllMetadata_shootings[,1:3],shooting_time,AllMetadata_shootings[,5:28])

#save!
write.csv(AllMetadata_shootings_clean,"C://Users/Katherine Manbeck/Desktop/Everything clinical psych/research related/Police Accountability Folder/policing/clean data\\shootings_Seattle.csv",row.names = FALSE)
