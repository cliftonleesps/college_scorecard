library(tidyverse)

cohorts <- read_csv('data/Most-Recent-Cohorts-Field-of-Study.csv', na=c("NULL", "PrivacySuppressed"))


# remove columns with all NA's
cohorts <- cohorts[,which(unlist(lapply(cohorts, function(x)!all(is.na(x))))),with=F]

print ("read cohorts")
dim(cohorts)

## remove columns with more than 70% NAs
too_many_na <- c()
threshold <- 0.75
c <- nrow(cohorts)
for (i in 1:ncol(cohorts)) {
    n <- sum(is.na(cohorts[,i]))
    if ( (n/c) >= threshold) {
        ##print (paste0(i, " - " , round(n/c,2)))
        too_many_na <- c(too_many_na, i)
    }
}
cohorts <- cohorts[-c(too_many_na)]


dim(cohorts)

print ("finished reading cohorts")
