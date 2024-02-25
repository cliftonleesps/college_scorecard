setwd('/Users/clee/Documents/home/cuny/data_698/')

library(tidyverse)
library(data.table)

print("beginning of determine_data_types")

insts <- read_csv("data/Most-Recent-Cohorts-Institution.csv", na=c("NULL", "PrivacySuppressed"))
headers <- data.frame(name = colnames(insts),
                      type = 'c')
rm(insts)

data_types <- read_csv('data_types.csv', col_type="cc", show_col_types = FALSE)

for (vn in data_types$name) {
##    vars[vars$v2 == x,'type'] <-  diffs[diffs$v1 == x,'type']
    headers[headers$name == vn, 'type'] <- data_types[data_types$name == vn,'type']
    ## print(paste0(vn, ' ' , data_types[data_types$name == vn,'type'], headers[headers$name == vn, 'type']))
}


cols <- paste(headers$type, collapse='')
insts <- read_csv("data/Most-Recent-Cohorts-Institution.csv", col_type=cols, na=c("NULL", "PrivacySuppressed"))

print("finished reading insts")
dim(insts)
#summary(insts$BBRR1_FED_IND_DFLT)

## reduce dimensions

# remove columns with all NA's
insts <- insts[,which(unlist(lapply(insts, function(x)!all(is.na(x))))),with=F]

## remove columns with more than 70% NAs
too_many_na <- c()
threshold <- 0.60
c <- nrow(insts)
for (i in 1:ncol(insts)) {
    n <- sum(is.na(insts[,i]))
    if ( (n/c) >= threshold) {
        ##print (paste0(i, " - " , round(n/c,2)))
        too_many_na <- c(too_many_na, i)
    }
}
insts <- insts[-c(too_many_na)]


## remove feature with character type
is_chars <- c('CITY','STABBR','ZIP','INSTURL','NPCURL','ACCREDAGENCY','REPAY_DT_MDN','SEPAR_DT_MDN','ALIAS','T4APPROVALDATE','ACCREDCODE','CIPCODE1','CIPTITLE1','FEDSCHCD','CONTROL_PEPS','ADDR')
##is_chars <- c('UNITID','OPEID','OPEID6','CITY','STABBR','ZIP','INSTURL','NPCURL','ACCREDAGENCY','REPAY_DT_MDN','SEPAR_DT_MDN','ALIAS','T4APPROVALDATE','ACCREDCODE','CIPCODE1','CIPTITLE1','FEDSCHCD','CONTROL_PEPS','ADDR')
insts <- insts[ , -which(names(insts) %in% is_chars)]


## Remove features dealing with graduate students
inst <- insts[!names(insts) %like% '_GR']


## only keep rows with BBRR2_FED_UGCOMP_N,BBRR2_FED_UGCOMP_DFLT,BBRR2_FED_UGCOMP_DLNQ,BBRR2_FED_UGCOMP_FBR,BBRR2_FED_UGCOMP_DFR,BBRR2_FED_UGCOMP_NOPROG,BBRR2_FED_UGCOMP_MAKEPROG,BBRR2_FED_UGCOMP_PAIDINFULL,BBRR2_FED_UGCOMP_DISCHARGE

targets <- c('BBRR2_FED_UGCOMP_N','BBRR2_FED_UGCOMP_DFLT','BBRR2_FED_UGCOMP_DLNQ','BBRR2_FED_UGCOMP_FBR','BBRR2_FED_UGCOMP_DFR','BBRR2_FED_UGCOMP_NOPROG','BBRR2_FED_UGCOMP_MAKEPROG','BBRR2_FED_UGCOMP_PAIDINFULL','BBRR2_FED_UGCOMP_DISCHARGE')
insts <- insts %>% filter(!is.na(BBRR2_FED_UGCOMP_N) & !is.na(BBRR2_FED_UGCOMP_DFLT) & !is.na(BBRR2_FED_UGCOMP_DLNQ) & !is.na(BBRR2_FED_UGCOMP_FBR) & !is.na(BBRR2_FED_UGCOMP_DFR) & !is.na(BBRR2_FED_UGCOMP_NOPROG) & !is.na(BBRR2_FED_UGCOMP_MAKEPROG) & !is.na(BBRR2_FED_UGCOMP_PAIDINFULL) & !is.na(BBRR2_FED_UGCOMP_DISCHARGE))

## Replace NA values with Median in All Numeric Columns
insts <- insts %>% mutate(across(where(is.integer), ~replace_na(., round(median(., na.rm=TRUE)))))
insts <- insts %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))


## Drop the factors for now
## https://stackoverflow.com/questions/20943317/how-to-find-out-whether-a-variable-is-a-factor-or-continuous-in-r
f <- sapply(insts, is.factor)
insts <- insts[,!f]


print("end of determine_data_types")
dim(insts)

