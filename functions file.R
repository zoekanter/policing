NA_count<-function(metadata){
  
  count_temp<-data.frame(matrix(NA,nrow=nrow(metadata),ncol=ncol(metadata)))
  
  #look for number of NAs in each column
  for (j in (1:ncol(metadata)))
  {
    for (i in 1:length(metadata[,2])){
      
      if(is.na(metadata[,j][i])){
        count_temp[,j][i]<-1
      } 
      else{
        count_temp[,j][i]<-0
      }
    }
  }
    SummaryofNA_count<-rbind(colSums(count_temp))
    rownames(SummaryofNA_count)<-"NumberofNA"
    colnames(SummaryofNA_count)<-colnames(metadata)
    return(SummaryofNA_count)
}
  


  
  #look for numbers of distinct values in each column
LevelsOfValues<-function(metadata) { 
  Vector_length<-as.data.frame(metadata)
  SummaryofLevels<-data.frame(matrix(NA,nrow=1,ncol=ncol(Vector_length)))
  
for (j in (1:ncol(metadata)))
  {
    SummaryofLevels[,j]<-length(unique(metadata[,j][!is.na(metadata[,j])]))
}
rownames(SummaryofLevels)<-"NumberofLevels"
colnames(SummaryofLevels)<-colnames(metadata)
return(SummaryofLevels)
}
  

  
  
  
#look for numbers of cases where NA is unique in the column
#Where NumTotalNAsAllowable = 0 if only looking for unique NAs, = 1 if looking for NAs with overlap with only one other variable, etc.
PercentNAUnique<-function(metadata, NumTotalNAsAllowableInRow){
  Vector_length<-as.data.frame(metadata)
  PercentNAvsNonNA<-as.data.frame(matrix(NA,nrow=nrow(Vector_length),ncol=(ncol(Vector_length))))
  NAOverlap<-as.data.frame(matrix(NA,nrow=1,ncol=ncol(Vector_length)))

  for (j in (1:ncol(metadata))){
    for (i in (1:nrow(metadata))){
      
      if(is.na(metadata[,j][i])==TRUE){
        
        PercentNAvsNonNA[,j][i]<-rowSums(is.na(metadata[i,]))
      } 
      else{
        PercentNAvsNonNA[,j][i]<-0
      }
    }
    if(NumTotalNAsAllowableInRow !=0){ 
     NAOverlap[,j]<-((sum(PercentNAvsNonNA[,j]==NumTotalNAsAllowableInRow))/(sum(PercentNAvsNonNA[,j]!=0)))
    }
    else 
      {
      return("Error: please input a non zero number into function call line - 1 means the variable is distinct as a NA")
      }
  }
  rownames(NAOverlap)<-"NumberUniqueNAs"
  colnames(NAOverlap)<-colnames(metadata)
  return(NAOverlap)
}
  
#make a chart that will do all 5 columns at the same time!
  createNAChart<-function(metadata){
    NAChart<-as.data.frame(matrix(NA,nrow=nrow(metadata),ncol=5))
    NAChart<-rbind(nrow(metadata),NA_count(metadata), LevelsOfValues(metadata), PercentNAUnique(metadata,1),PercentNAUnique(metadata,2))
   rownames(NAChart)<-cbind('N',"Number of NAs", "Number of Levels", "Number of only NA", "Number of 1 of 2 NAs")
    return(NAChart)
  }
  