library(tidyverse)

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

end_time <- Sys.time()
print(end_time - start_time)






vars <- read_csv("data/variables.csv", col_type="cfc")
diffs <- vars %>% filter(v1 != v2)
for (x in diffs$v2) {
    print(paste(x, diffs[diffs$v1 == x,'type'], diffs[diffs$v2 == x,'type']))
    vars[vars$v2 == x,'type'] <-  diffs[diffs$v1 == x,'type']
}
