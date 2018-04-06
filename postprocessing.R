#rep.data.frame <- function(x, times) {
#  rnames <- attr(x, "row.names")
#  x <- lapply(x, rep.int, times = times)
#  class(x) <- "data.frame"
#  if (!is.numeric(rnames))
#    attr(x, "row.names") <- make.unique(rep.int(rnames, times))
#  else
#    attr(x, "row.names") <- .set_row_names(length(rnames) * times)
#  x
#}


fixCommas<-function(specs){
  commas<-grepl(" , ",specs$specno)
  test<-subset(specs,commas)
  fixedRows<-do.call(rbind.data.frame,apply(test,1,splitByString,string=" , ",names=colnames(test)))
  specimensFixed<-rbind(specs[commas==F,],subset(fixedRows,fixedRows$fixed==T)[,-8])
  ands<-grepl("and ",specimensFixed$specno)
  test<-subset(specimensFixed,ands)
  fixedRows<-do.call(rbind.data.frame,apply(test,1,splitByString,string="and ",names=colnames(test)))
  specimensFixed<-rbind(specimensFixed[ands==F,],subset(fixedRows,fixedRows$fixed==T)[,-8])
  specimensFixed<-subset(specimensFixed,specimensFixed$specno!="")
  return(specimensFixed)
}

splitByString<-function(specrow,string,names){
  numbers<-unlist(strsplit(unlist(specrow["specno"]),string))
  columns<-length(specrow)
  specrow.df<-as.data.frame(t(specrow))
  if(abs(max(nchar(numbers))-min(nchar(numbers)))<3){
    temp<-setNames(data.frame(matrix(nrow=length(numbers),ncol=columns+1)),c(names,"fixed"))
    temp[,1:columns]<-specrow.df
    temp$specno<-numbers
    temp$fixed<-T
  } else {
    temp<-setNames(data.frame(matrix(nrow=2,ncol=columns+1)),c(names,"fixed"))
    temp[,1:columns]<-specrow.df
    temp$fixed<-F
  }
  return(temp)
}

fixSequences<-function(specs){
  dashes<-grepl(" - ",specs$specno)
  test<-subset(specs,dashes)
  fixedRows<-do.call(rbind.data.frame,apply(test,1,expandSeq,names=colnames(test)))
  specimensFixed<-rbind(specs[dashes==F,],subset(fixedRows,fixedRows$fixed==T)[,-8])
  return(specimensFixed)
}
expandSeq<-function(specrow,names){
  ends.char<-unlist(strsplit(specrow["specno"]," - "))
  ends<-suppressWarnings(as.numeric(ends.char))
  columns<-length(specrow)
  specrow.df<-as.data.frame(t(specrow))
  if(!any(is.na(ends)) & abs(max(ends)-min(ends))<200){
    numbers<-seq(ends[1],ends[2])
    #temp<-rep.data.frame(specrow,length(numbers))
    temp<-setNames(data.frame(matrix(nrow=length(numbers),ncol=columns+1)),c(names,"fixed"))
    temp[,1:columns]<-specrow.df
    temp$specno<-numbers
    temp$fixed<-T
  } else {
    temp<-setNames(data.frame(matrix(nrow=2,ncol=columns+1)),c(names,"fixed"))
    temp[,1:columns]<-specrow.df
    temp$fixed<-F
  }
  return(temp)
}