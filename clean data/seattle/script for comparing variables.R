

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
                   