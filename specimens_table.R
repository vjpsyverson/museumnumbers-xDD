#----------------------FUNCTIONS--------------------------#
##basic utilities
substrRight <- function(x, n){
  substr(x, (nchar(x)-n+1), nchar(x))
}
trimWhitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
cleanWords<-function(x,abbrs){
  #acquire, clean, and split words
  result<-gsub("[\\\"{}]","",gsub("-",",-,",gsub("-RRB-",")",gsub("-LRB-","(",x)))) #remove punctuation rubble
  result<-strsplit(gsub(",,,",",COMMA,",result),",")[[1]] #clean up real commas and split
  result[which(result=="COMMA")]<-"," #re-insert real commas
  abbrs<-abbrs[order(nchar(abbrs),decreasing = T)]
  for (i in 1:length(abbrs)){ #iterate through 
    loc<-which(grepl(abbrs[i],result)&!grepl(paste0(abbrs[i],"$",collapse=""),result)&!grepl(paste0(abbrs[1:(i-1)],collapse="|"),result))
    result[loc]<-gsub(abbrs[i],paste0(abbrs[i]," "),result[loc])
  } #if this is stupid move it into extractFromSentence() so it executes with the other greps there
  return(paste0(result,collapse=' ')) #put it back together with spaces instead
}
##the real functions
extractFromList<-function(instSpecList,data){
  instNames<-names(instSpecList)
  specsByInst<-lapply(instNames,function(x,d) extractFromNamedVector(instSpecList[[x]],d,x),data)
  result<-do.call(rbind.data.frame,specsByInst)
  row.names(result)<-NULL
  return(result)
}

extractFromNamedVector<-function(instRow,data,name){
  if (length(unlist(instRow))==0) {
    result<-NULL
    print(paste("No specimens found for",name))
  } else {
    specsByRow<-apply(data[instRow,],1,extractFromSentence,name)
    if(length(specsByRow)==0){
      result<-NULL
      print(paste("No specimens found for",name))
    } else {
      result<-do.call(rbind.data.frame,specsByRow)
      print(paste(nrow(result),"specimens found for",name))
    }
  }
  return(result)
}

extractFromSentence<-function(sentence,abbr){
  words<-unlist(strsplit(as.character(sentence["words"])," "))
  findAbbr<-which(grepl(abbr,words,fixed=T)&!grepl(paste0(abbr,"[A-Z]"),words)) #find all instances of this institution abbreviation
  specnos<-result<-numeric() #zero speclocs and specnos for each institution abbreviation
  speclocs<-do.call(rbind.data.frame,lapply(findAbbr,getNumbersAfter,words)) #catch all the number-like strings associated with each instance
  if(length(speclocs)>0){ #if any instance of this abbreviation is associated with any numbers
    numbers<-words[speclocs$speclocs][grepl("[[:digit:]]",words[speclocs$speclocs])] #assign the ones with numbers in them to "numbers"
    specnos<-gsub("^[-/=\\.[:blank:]]","",numbers) #parse out any initial junk (-,/,=,., ) and send to "specnos"
  }
  if(length(specnos)>0){
    result<-data.frame(sentrow=as.numeric(sentence["rownum"]),docid=sentence["docid"],
                       sentid=as.numeric(sentence["sentid"]),abbr=rep(abbr,length(specnos)),
                       specno=specnos,abbrloc=speclocs[,1],specloc=speclocs[,2],row.names=NULL,stringsAsFactors=F)
  }
  if (length(result)>0){return(result)}
}

getNumbersAfter<-function(abbrLoc,words){
  fillers<-c("Catalog","catalog","Catalogue","catalogue","Cat.","cat.","Number","number","Numbers","numbers","No.","no.",",","and","&","-","through")
  speclocs<-numeric()
  start<-abbrLoc
  end<-start+1
  while (grepl("[[:digit:]]",words[end])==T|words[end]%in%fillers) {
    if (words[end]%in%fillers) {end<-end+1} #move on
    if (grepl("[[:digit:]]{2,}",words[end]) & !grepl("(^[[:digit:]]*x[[:digit:]])|(^0\\.[[:digit:]]+$)|(^[[:digit:]]+[c|d|k|m]?m$)|(^d[0-9]{2}[A-Z]{1}$)",words[end])) {
          #not: dimensions eg "10x12"; a decimal beginning with "0."; a measurement in cm/dm/km/mm; an isotopic ratio 
      speclocs<-c(speclocs,end)
      end<-end+1
    } else { break }
  }
  if(length(speclocs)>0){return(cbind(rep(abbrLoc,length(speclocs)),speclocs))}
}


#----------------------SETUP: INSTALL LIBRARIES, FIND DIRECTORY, CONNECT TO POSTGRES----------------------------#
if (require("RPostgreSQL",warn.conflicts=FALSE)==FALSE) {
  install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
  library("RPostgreSQL");
}
if (require("rjson",warn.conflicts=FALSE)==FALSE) {
  install.packages("rjson",repos="http://cran.cnr.berkeley.edu/");
  library("rjson");
}
options(stringsAsFactors = FALSE)

#connect to PostgreSQL
#Credentials<-as.matrix(read.table(file.path(this.dir,"credentials.yaml"),row.names=1,fill=TRUE))
Credentials<-as.matrix(read.table(file.path(getwd(),"credentials.yaml"),row.names=1,fill=TRUE))
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])

#----------------------GET REDUCED TABLE FROM POSTGRES----------------------------#
#get museum abbreviations from local SQL db (change this when the GDD or PBDB API is reporting them better)
#print("Getting institution names from local database...")
#museumAbbrs<-dbGetQuery(con,"SELECT * FROM mus_abbrs")
museumAbbrs<-read.csv(file="mus_abbrs.csv")
museumAbbrs<-museumAbbrs[order(sapply(museumAbbrs$abbr,nchar),decreasing=T),]
print(paste("Got",nrow(museumAbbrs),"institution names"))
#get sentences from SQL containing at least one abbreviation from museumAbbrs and the string "specimen*"
query<-paste0("SELECT sentences_nlp352.docid,sentences_nlp352.sentid,sentences_nlp352.words FROM sentences_nlp352 
              JOIN (SELECT DISTINCT docid FROM sentences_nlp352 WHERE array_to_string(words,' ') ~ 'specimen' 
              AND array_to_string(words,' ') ~ ' ", paste(museumAbbrs$fullname,sep="",collapse=" | "), " ') a 
              ON a.docid=sentences_nlp352.docid WHERE array_to_string(sentences_nlp352.words,' ') ~ '",
              paste(museumAbbrs$abbr,sep="",collapse="|")," ';")
print("Querying database for sentences...")
time<-system.time(mm<-dbGetQuery(con,query))
print(paste("Got",nrow(mm),"sentences in",signif(unname(time[3]),3),"seconds."))
#label, number, and clean result
mus<-data.frame(docid=mm[,"docid"],sentid=mm[,"sentid"],
                words=unname(sapply(mm$words,cleanWords,museumAbbrs$abbr)),
                rownum=as.numeric(rownames(mm)),stringsAsFactors = F)
rm(mm)

#grep all museumAbbrs against all sentences, with spaces added to avoid the "UCMP"/"UCM"/"CM" problem
print("Finding all museum abbreviations...")
time<-system.time(instRows<-sapply(sort(unique(museumAbbrs$abbr)),function(x,y) grep(paste0(" ",x," "),y),mus$words))
print(paste("Found",length(unlist(instRows)),"instances of",length(instRows),"abbreviations in",signif(unname(time[3]),3),"seconds."))
names(instRows)<-sort(unique(museumAbbrs$abbr))
#find all specimen numbers in each sentence associated with each instance of each abbreviation in museumAbbrs
#(this is a wrapper function three apply()s deep, be warned)
print("Extracting specimen numbers...")
time<-system.time(specimens<-extractFromList(instRows,mus))
print(paste("Got",nrow(specimens),"specimen numbers in",signif(unname(time[3]),3),"seconds. Results in output/specimens.csv"))

#----------------------OUTPUT RESULTS------------------------#
write.csv(specimens,"output/specimens.csv",row.names = F)
