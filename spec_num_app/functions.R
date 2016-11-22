#functions
##basic utilities
substrRight <- function(x, n){
    substr(x, (nchar(x)-n+1), nchar(x))
}
trimWhitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
##extractor 
extractSpecNos<-function(x){
  fillers<-c("No.","cat.", "no.","number","numbers",",","and","-")
  result<-data.frame(docid=character(),sentid=numeric(),inst=character(),specnos=character(),stringsAsFactors=F)
  insts<-unlist(strsplit(as.character(x["inst"])," "))
  insts<-insts[order(nchar(insts),decreasing = T)]
  #acquire, clean, and split words
  words<-strsplit(gsub(",,,",",COMMA,",gsub("[(\"){}]","",x["words"])),",")[[1]]
  instPlusString<-paste0("(.*)(",insts[1],")(.+)")
  instLoc<-which(grepl(instPlusString,words))
  if(length(instLoc)>0) {words<-gsub(instPlusString,paste0("\\1","\\2",",","\\3"),words)}
  for (k in 2:length(insts)){ #split words after institute abbreviations
    instPlusString<-paste0("(.*)(",insts[k],")(.+)")
    longerInstNames<-paste(insts[1:k][-k],collapse = "|")
    instLoc<-which(!grepl(longerInstNames,words)&grepl(instPlusString,words))
    if(length(instLoc)>0) {words<-gsub(instPlusString,paste0("\\1","\\2",",","\\3"),words)}
  }
  words<-unlist(strsplit(words,","))
  words[which(words=="COMMA")]<-","
  #extract tuples {docid, sentid, inst, specnos}
  inSentence<-data.frame(docid=numeric(),sentid=numeric(),inst=character(),specnos=character(),stringsAsFactors=F)
  for (k in 1:length(insts)){
    findAbbr<-grep(insts[k],words,fixed=T) #find all instances of this institution abbreviation
    speclocs<-specnos<-numeric() #zero speclocs and specnos for each institution abbreviation
    for(j in 1:length(findAbbr)){ #catch all the number-like strings associated with each instance
      start<-findAbbr[j]
      end<-start+1
      while (grepl("[[:digit:]]",words[end])==T|words[end]%in%fillers) {
        if (words[end]%in%fillers) {end<-end+1}
        if (grepl("[[:digit:]]",words[end])==T) {
          speclocs<-c(speclocs,end)
          end<-end+1
          }
        }
      }
    if(length(speclocs)>0){ #if any instance of this abbreviation is associated with any numbers
      specnos<-words[speclocs][grepl("[[:digit:]]",words[speclocs])] #assign the ones with numbers in them to specnos
    }
    if(length(specnos)>0){
      inSentence[(nrow(inSentence)+1):(nrow(inSentence)+length(specnos)),]<-cbind(
        x["docid"],x["sentid"],rep(insts[k],length(specnos)),specnos,row.names=NULL,stringsAsFactors=F)
      }
    }
  if (nrow(inSentence)>0) {result<-inSentence} else {
    result<-cbind(docid=x["docid"],sentid=x["sentid"],inst=NA_character_,specnos=NA_character_)
    }
  return(data.frame(result))
  }
##wrapper applying extractSpecNos to table of sentences + "inst" column
extractMultiple<-function(x){
  specList<-suppressWarnings(apply(x,1,extractSpecNos))
  hasSpecs<-sapply(lapply(specList,function(x) !is.na(x$inst)&!is.na(x$specnos)),any)
  specsMatrixNoNA<-do.call("rbind",specList[hasSpecs])
  colnames(specsMatrixNoNA)[4]<-"specno"
  duplicates<-duplicated(apply(specsMatrixNoNA,1,paste,collapse=","))
  return(specsMatrixNoNA[!duplicates,])
}
##function to combine duplicated sentences
combineDuplicates<-function(x){
  isDuplicate<-duplicated(paste(x[,"docid"],x[,"sentid"]))
  x.unique<-x[!isDuplicate,]
  duplicates<-x[isDuplicate,]
  for(i in 1:nrow(duplicates)){
    uniqueNo<-which(x.unique[,"docid"]==duplicates[i,"docid"] & x.unique[,"sentid"]==duplicates[i,"sentid"])
    x.unique[uniqueNo,"inst"]<-as.character(paste(x.unique[uniqueNo,"inst"],duplicates[i,"inst"],collapse=","))
  }
  return(x.unique)
}
