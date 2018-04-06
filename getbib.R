options(stringsAsFactors = F)
require(RPostgreSQL)
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, dbname = "vsyverson", host = "localhost", port = "5432", user = "vsyverson")

uniquedocIDs<-unname(unlist(dbGetQuery(con,"SELECT DISTINCT docid FROM specimens_full;")))
bibpb<-txtProgressBar(min=0,max=length(uniquedocIDs))
dbSendQuery(con,"DROP TABLE IF EXISTS bib;")
bib.line<-jsonlite::fromJSON(paste0("https://geodeepdive.org/api/articles?docid=",uniquedocIDs[1]))$success$data
bib<-bib.line[c("_gddid","author","title","journal","year")]
for(i in 2:length(uniquedocIDs)){
  bib.line<-try(jsonlite::fromJSON(paste0("https://geodeepdive.org/api/articles?docid=",uniquedocIDs[i]))$success$data)
  if (!is.null(bib.line)) {
    bib<-rbind(bib,bib.line[c("_gddid","author","title","journal","year")])
  }
  if(nrow(bib)%%10==0) setTxtProgressBar(bibpb,nrow(bib))
}
remainingdocIDs<-subset(uniquedocIDs,!uniquedocIDs%in%bib$`_gddid`)
nremaining<-length(remainingdocIDs)
paste(nremaining,"documents left to fetch. Starting second loop...")
while(nremaining>0){
  for (i in 1:length(remainingdocIDs)){
    bib.line<-try(jsonlite::fromJSON(paste0("https://geodeepdive.org/api/articles?docid=",remainingdocIDs[i]))$success$data)
    if (!is.null(bib.line)) {
      bib<-rbind(bib,bib.line[c("_gddid","author","title","journal","year")])
    }
  }
  remainingdocIDs<-subset(uniquedocIDs,!uniquedocIDs%in%bib$`_gddid`)
  setTxtProgressBar(bibpb,nrow(bib))
  if(length(remainingdocIDs)==nremaining) {
    print(paste("Exiting with",nremaining,"documents still missing."))
    break
  }
}
names(bib)[1]<-"gddid"
dbWriteTable(con,"bib",bib)
