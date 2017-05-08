#----------------------FUNCTIONS--------------------------#
##basic utilities
substrRight <- function(x, n){
  substr(x, (nchar(x)-n+1), nchar(x))
}
trimWhitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
cleanWords<-function(x){
  #acquire, clean, and split words
  result<-strsplit(gsub(",,,",",COMMA,",gsub("[(\"){}]","",x)),",")[[1]] #remove punctuation rubble, clean up real commas
  result<-unlist(strsplit(result,",")) #split on nlp352's inserted commas
  result[which(result=="COMMA")]<-"," #re-insert real commas
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
  } else {
    specsByRow<-apply(data[instRow,],1,extractFromSentence,name)
    if(length(specsByRow)==0){
      result<-NULL
    } else {
      result<-do.call(rbind.data.frame,specsByRow)
    }
  }
  return(result)
}

extractFromSentence<-function(sentence,abbr){
  words<-unlist(strsplit(as.character(sentence["words"])," "))
  findAbbr<-which(grepl(abbr,words,fixed=T)&!grepl(paste0(abbr,"[A-Z]"),words)) #find all instances of this institution abbreviation
  specnos<-result<-numeric() #zero speclocs and specnos for each institution abbreviation
  speclocs<-unlist(sapply(findAbbr,getNumbersAfter,words)) #catch all the number-like strings associated with each instance
  if(length(speclocs)>0){ #if any instance of this abbreviation is associated with any numbers
    numbers<-words[speclocs][grepl("[[:digit:]]",words[speclocs])] #assign the ones with numbers in them to "numbers"
    specnos<-gsub("^[-/=\\.[:blank:]]","",numbers) #parse out any initial junk (-,/,=,., ) and send to "specnos"
  }
  if(length(specnos)>0){
    result<-data.frame(sentrow=as.numeric(sentence["rownum"]),docid=sentence["docid"],
                       sentid=as.numeric(sentence["sentid"]),abbr=rep(abbr,length(specnos)),
                       specno=specnos,row.names=NULL,stringsAsFactors=F)
  }
  if (length(result)>0){return(result)}
}

getNumbersAfter<-function(abbrLoc,words){
  fillers<-c("No.","cat.", "no.","number","numbers",",","and","-")
  speclocs<-numeric()
  start<-abbrLoc
  end<-start+1
  while (grepl("[[:digit:]]",words[end])==T|words[end]%in%fillers) {
    if (words[end]%in%fillers) {end<-end+1}
    if (grepl("[1-9]",words[end])
        & !grepl("^[[:digit:]]*x[[:digit:]]",words[end])
        & !(grepl("^0\\.",words[end]) 
            & !grepl("(\\.)(.*)(\\.)",words[end]))) {
      speclocs<-c(speclocs,end)
      end<-end+1
    } else { break }
  }
  if(length(speclocs)>0){return(speclocs)}
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

#locate correct directory -- run this if you're running line by line rather than from console
#this is super ugly but there is, alas, currently no good way to do it
#if("specimens.R"%in%dir()){
#  this.dir<-getwd()} else {
#    print("Please locate this script in your directory structure.")
#    this.dir<-dirname(file.choose())
#  }

#connect to PostgreSQL
#Credentials<-as.matrix(read.table(file.path(this.dir,"credentials.yaml"),row.names=1,fill=TRUE))
Credentials<-as.matrix(read.table(file.path(getwd(),"credentials.yaml"),row.names=1,fill=TRUE))
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])

#----------------------GET REDUCED TABLE FROM POSTGRES----------------------------#
#pull museum abbreviations from geodeepdive dictionary via API
url<-"https://geodeepdive.org/api/dictionaries?dict=museums&show_terms"
museumAllNames<-names(rjson::fromJSON(paste(suppressWarnings(readLines(url)),collapse = ""))$success$data[[1]]$term_hits)
museumAbbrs<-museumAllNames[!grepl("[[:lower:]]",museumAllNames)]
#add any other abbreviations stored locally
if("additional_abbrs.txt"%in%dir()){
  museumAbbrs<-unique(c(museumAbbrs,unname(unlist(read.table("additional_abbrs.txt",sep="\r",stringsAsFactors=F)))))
  }

#get sentences from SQL containing at least one abbreviation from museumAbbrs
dbColNames<-"docid,sentid,words"
query<-paste0("SELECT ", dbColNames , ",regexp_matches(array_to_string(words,' '),' ",
              paste(museumAbbrs,sep="",collapse=" | ")," ') FROM sentences_nlp352;")
mm<-dbGetQuery(con,query)[,c('docid','sentid','words')] #searched 524255 rows, got 14080 rows, 43.3 seconds
#label, number, and clean result
mus<-data.frame(docid=mm[,"docid"],sentid=mm[,"sentid"],
                words=unname(sapply(mm$words,cleanWords)),
                rownum=as.numeric(rownames(mm)),stringsAsFactors = F)
rm(mm)

#grep all museumAbbrs against all sentences, with spaces added to avoid the "UCMP"/"UCM"/"CM" problem
instRows<-sapply(museumAbbrs,function(x,y) grep(paste0(" ",x," "),y),mus$words) #14080 sentences*131 museum abbreviations, 12.1 seconds
#find all specimen numbers in each sentence associated with each instance of each abbreviation in museumAbbrs
#(this is a wrapper function three apply()s deep, be warned)
specimens<-extractFromList(instRows,mus) #15820 sent+abbr tuples parsed to 15236 specimen numbers, 17.4 seconds


#----------------------OUTPUT RESULTS------------------------#
write.csv(specimens,"output/specimens.csv")

#optional stats output
#dbSendQuery(con,"DROP TABLE IF EXISTS specimens;")
#dbWriteTable(con,"specimens",specimens,row.names=F)
#ndocs<-dbGetQuery(con,"SELECT DISTINCT concat_ws(' ', a.abbr, a.specno), count(*) FROM (SELECT DISTINCT docid,abbr,specno FROM specimens AS a) a GROUP BY concat_ws(' ', a.abbr, a.specno);")
#ndocs<-ndocs[order(ndocs$count,ndocs$concat_ws,decreasing=T),]
#write.csv(ndocs,"n_docs_mentioning_specimen.csv",row.names = F)