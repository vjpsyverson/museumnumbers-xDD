#----------------------FUNCTIONS--------------------------#
##basic utilities
substrRight <- function(x, n){
  substr(x, (nchar(x)-n+1), nchar(x))
}
trimWhitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
cleanWords<-function(x,abbrs){
  #acquire, clean, and split words
  result<-gsub("[\\\"{}]","",gsub("-",",-,",gsub("-RRB-",")",gsub("-LRB-","(",x)))) #remove punctuation rubble
  result<-strsplit(gsub(",,","",gsub(",,,",",COMMA,",result)),",")[[1]] #clean up real commas and split
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
  print(paste0("docid=",sentence["docid"],", sentid=",sentence["sentid"],", abbr=",abbr,collapse=""))
  words<-unlist(strsplit(as.character(sentence["words"])," "))
  findAbbr<-which(grepl(abbr,words,fixed=T)&!grepl(paste0(abbr,"[A-Z]"),words)) #find all instances of this institution abbreviation
  specnos<-speclocs<-nextWords<-result<-numeric() #zero speclocs and specnos for each institution abbreviation
  speclocs<-do.call(rbind.data.frame,lapply(findAbbr,getNumbersAfter,words)) #catch all the number-like strings associated with each instance
  nextWords<-""
  if(!any(grepl("[Ll]ocality|[Ll]ocalities",unlist(lapply(findAbbr,function(x) return(words[max(c(0,x-3)):min(c(length(words),x+3))])))))) {
    #skip locality numbers
    speclocs<-do.call(rbind.data.frame,lapply(findAbbr,getNumbersAfter,words)) #catch all the number-like strings associated with each instance
    if(any(as.logical(speclocs$sentEnded))){
      nextSent<-dbGetQuery(con,paste0("SELECT docid,sentid,wordidx,words FROM sentences_nlp352 WHERE docid='",sentence["docid"],"' AND sentid=",as.numeric(sentence["sentid"])+1,";"))
      if(ncol(nextSent)>0) {
        nextWords<-unlist(strsplit(cleanWords(as.character(nextSent$words),abbr)," "))
        speclocs<-do.call(rbind.data.frame,lapply(findAbbr,getNumbersAfter,c(words,nextWords)))
      }
    }
  } else { speclocs<-matrix(0,nrow=0,ncol=0) }
  if(nrow(speclocs)>0){ #if any instance of this abbreviation is associated with any numbers
    speclocs<-apply(speclocs[,1:3],c(1,2),as.numeric)
    if(nrow(speclocs)>1) {
      numberRanges<-lapply(apply(unname(speclocs),1,function(x) list(seq(x[2],x[3]))),"[[",1)
      numbers<-unlist(lapply(numberRanges,function(x,y) (paste0(y[x],collapse=" ")),c(words,nextWords)))
    } else {
      numbers<-paste0(c(words,nextWords)[speclocs[,2]:speclocs[,3]],collapse=" ")
    }
    numbers<-numbers[grepl("[[:digit:]]",numbers)] #assign the ones with numbers in them to "numbers"
    specnos<-gsub("[-/=\\.[:blank:]]+$","",gsub("^[-/=\\.[:blank:]]+","",numbers)) #parse out any initial or final junk (-,/,=,., ) and send to "specnos"
  }
  if(length(specnos)>0){
    result<-data.frame(sentrow=as.numeric(sentence["rownum"]),docid=sentence["docid"],sentid=sentence["sentid"],
                       abbr=abbr,specno=specnos,abbrloc=speclocs[,1],
                       specloc=apply(speclocs,1,function(x) paste0(seq(x[2],x[3]),collapse = ",")),
                       row.names=NULL,stringsAsFactors=F)
  }
  if (length(result)>0){return(result)}
}

getNumbersAfter<-function(abbrLoc,words){
  fillers<-c("Catalog","catalog","Catalogue","catalogue","Cat.","cat.","Number","number","Numbers","numbers","No.","no.","and","&","-",".",",")
  speclocs<-character()
  start<-abbrLoc
  end<-start+1
  sentEnded<-F
  while (grepl("[[:digit:]]|^[IVXLC]+$",words[end])==T|words[end]%in%fillers) {
    if (words[end]%in%fillers | grepl("^[IVXLC]+$",words[end])) { 
      if (words[end] == "." & end==length(words)){
        sentEnded <- T
        break
      } else {
        if (grepl("^[[:punct:]]+$|^[IVXLC]+$",words[end]) & words[end]!="," & words[end]!=";") {
          speclocs<-c(speclocs,end)
        }
        end<-end+1 #move on
      }
    } else if (grepl("[[:digit:]]",words[end]) & !grepl("(^[[:digit:]]*x[[:digit:]])|(^[[:digit:]]+\\.[[:digit:]]+$)|(^[[:digit:]]+[c|d|k|m]?m$)|(^d[0-9]{2}[A-Z]{1}$)|([I,C,PM,P,M]{1}[[:digit:]]{1}$)",words[end])) {
      #not: dimensions eg "10x12"; a measurement in cm/dm/km/mm; an isotopic ratio; a tooth 
      speclocs<-c(speclocs,end)
      end<-end+1
    } else { break }
  }
  if(length(grep("[[:digit:]]",words[range(as.numeric(speclocs))]))>0){
    result<-cbind(abbrloc=rep(abbrLoc,length(speclocs)),speclocs.start=min(as.numeric(speclocs)),speclocs.end=max(as.numeric(speclocs)),sentEnded)
    return(unique(result))
  }
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
options(stringsAsFactors = FALSE,encoding="UTF-8")

#if running remotely: first do "ssh -L 5432:localhost:5432 substrata"
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
print(paste("Got",nrow(museumAbbrs),"institution names."))
dbSendQuery(con,"DROP TABLE IF EXISTS mus_abbrs;DROP TABLE IF EXISTS sentences_temp;")
dbWriteTable(con,"mus_abbrs",museumAbbrs,row.names=F)
query<-"SELECT sentences_nlp352.docid,sentences_nlp352.sentid,sentences_nlp352.words INTO sentences_temp FROM sentences_nlp352 JOIN (SELECT DISTINCT sentences_nlp352.docid,mus_abbrs.abbr FROM sentences_nlp352 JOIN mus_abbrs ON array_to_string(sentences_nlp352.words,' ') ~ mus_abbrs.fullname) a ON a.docid=sentences_nlp352.docid AND array_to_string(sentences_nlp352.words,' ') ~ a.abbr;"
print("Getting sentences...")
dbSendQuery(con,query)
mm<-dbGetQuery(con,"SELECT * FROM sentences_temp;")
#label, number, and clean result
time<-system.time(mus<-data.frame(docid=mm[,"docid"],sentid=mm[,"sentid"],
                                  words=unname(sapply(mm$words,cleanWords,museumAbbrs$abbr)),
                                  rownum=as.numeric(rownames(mm)),stringsAsFactors = F))
print(paste("Got",nrow(mus),"sentences in",signif(unname(time[3]),3),"seconds."))
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
dbSendQuery(con,"DROP TABLE IF EXISTS specimens;")
dbWriteTable(con,"specimens",specimens,row.names=F)
print(paste("Got",nrow(specimens),"specimen numbers in",signif(unname(time[3]),3),"seconds."))

#----------------------OUTPUT RESULTS------------------------#
write.csv(specimens,"output/specimens.csv",row.names = F)
print("Results saved to output/specimens.csv and in database as table 'specimens'.")

#optional stats output
#ndocs<-dbGetQuery(con,"SELECT DISTINCT concat_ws(' ', a.abbr, a.specno), count(*) FROM (SELECT DISTINCT docid,abbr,specno FROM specimens AS a) a GROUP BY concat_ws(' ', a.abbr, a.specno);")
#ndocs<-ndocs[order(ndocs$count,ndocs$concat_ws,decreasing=T),]
#write.csv(ndocs,"n_docs_mentioning_specimen.csv",row.names = F)
