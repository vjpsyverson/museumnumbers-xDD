#----------------------SETUP: INSTALL LIBRARIES, FIND DIRECTORY, CONNECT TO POSTGRES----------------------------#
if (require("RPostgreSQL",warn.conflicts=FALSE)==FALSE) {
  install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
  library("RPostgreSQL");
}
if (require("rjson",warn.conflicts=FALSE)==FALSE) {
  install.packages("rjson",repos="http://cran.cnr.berkeley.edu/");
  library("rjson");
}

#locate correct directory
#this is super ugly but there is, alas, currently no good way to do it
if("spec_num.R"%in%dir()){
  this.dir<-getwd()} else {
  print("Please locate this script in your directory structure.")
  this.dir<-dirname(file.choose())
  }
source(file.path(this.dir,"functions.R"))
 
#connect to PostgreSQL
Credentials<-as.matrix(read.table(file.path(dirname(this.dir),"credentials.yml"),row.names=1,fill=TRUE))
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])

#----------------------GET REDUCED TABLE FROM POSTGRES----------------------------#
#pull museum abbreviations from geodeepdive dictionary via API
url<-"https://geodeepdive.org/api/dictionaries?dict=museums&show_terms"
museumAllNames<-names(rjson::fromJSON(paste(suppressWarnings(readLines(url)),collapse = ""))$success$data[[1]]$term_hits)
museumAbbrs<-museumAllNames[!grepl("[[:lower:]]",museumAllNames)]

#create table in SQL to hold abbreviation matches
dbColNames<-"docid,sentid,words"
dbSendQuery(con,paste0("DROP TABLE IF EXISTS mus_abbr_matches;
            SELECT ",dbColNames," INTO mus_abbr_matches FROM sentences WHERE 1=0;
            ALTER TABLE mus_abbr_matches ADD inst varchar;"))

#match each abbreviation in museums against sentence table in db
for (i in 1:length(museumAbbrs)) {
  query<-paste0("INSERT INTO mus_abbr_matches (",dbColNames,",inst) SELECT ",dbColNames,",'",museumAbbrs[i],"' FROM sentences WHERE array_to_string(words, ' ') ~'",museumAbbrs[i],"';")
  dbSendQuery(con,query)
  }

#get resulting table
mm<-dbReadTable(con,c("public","mus_abbr_matches"))

#parse out duplicates
mus<-combineDuplicates(mm)

#----------------------EXTRACT NUMBERS FROM DATAFRAME IN R----------------------------#
specs<-extractMultiple(mus)

#----------------------EXPORT RESULTS TO POSTGRES----------------------------#
dbSendQuery(con,"DROP TABLE IF EXISTS specimens;")
dbWriteTable(con,"specimens",specs,row.names=F)
