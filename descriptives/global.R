
## Does the fraction of non-photos in a group effect the communicativity of photoed PPTs
rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()))])
#rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()) | !(ls(all=TRUE) %in% c("global.vars",global.vars())) )])
assign("last.warning",NULL,envir=baseenv())

#.ts.data <- data.table(read.csv("./data/datakind_extract_keyfields.csv",stringsAsFactors=FALSE))
.ts.data <- data.table(read.csv("./data/sample_dk_extract.csv",stringsAsFactors=FALSE))
.ts.data[,Transaction_Date:=as.POSIXct(Transaction_Date)]
.ts.data <- .ts.data[!is.na(Budget)]
