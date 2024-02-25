library(tidyverse)
library(mice)
start_time <- Sys.time()

vars <- read_csv("data/variables.csv", col_type="cfc")

cols <- paste(vars$type, collapse='')


insts <- read_csv("insts.csv", col_types=cols,na=c("NA","NULL", "PrivacySuppressed"))

insts$C100_L4 <- as.numeric(insts$C100_L4)
insts$OMACHT8_FTFT <- as.integer(insts$OMACHT8_FTFT)


library(data.table)


## drop city, addr, INSTURL, NPCURL, ALIAS


# remove columns with all NA's
insts <- insts[,which(unlist(lapply(insts, function(x)!all(is.na(x))))),with=F]

## remove columns with more than 70% NAs
too_many_na <- c()
threshold <- 0.70
c <- nrow(insts)
for (i in 1:ncol(insts)) {
    n <- sum(is.na(insts[,i]))
    if ( (n/c) >= threshold) {
        ##print (paste0(i, " - " , round(n/c,2)))
        too_many_na <- c(too_many_na, i)
    }
}
insts <- insts[-c(too_many_na)]

dim(insts)
insts$OPEID6 <- as.numeric(insts$OPEID6)
insts <- insts[, !sapply(insts, is.character)]


cnames <- colnames(insts)
for (n in cnames) {
  if (sum(is.na(insts[,n]) > 0)) {
    na_name <- paste0(n, '_NA')
    insts <- insts %>% mutate (!!na_name := ifelse(is.na(!!as.name(n)), 1, 0 ))
  }
}


# remove factors
insts <- insts[, !sapply(insts, is.factor)]
insts[is.na(insts)] <- 0


end_time <- Sys.time()
print(end_time - start_time)
