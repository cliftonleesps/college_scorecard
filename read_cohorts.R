library(tidyverse)

start_time <- Sys.time()

cohorts <- read_csv('data/Most-Recent-Cohorts-Field-of-Study.csv', na=c("NULL", "PrivacySuppressed"), show_col_types=FALSE)


# remove columns with all NA's
cohorts <- cohorts[,which(unlist(lapply(cohorts, function(x)!all(is.na(x))))),with=F]

print ("read cohorts")
dim(cohorts)


# remove row with too many NA's
too_many_na <- c()
threshold <- 0.75
#threshold <- 0.50
c <- ncol(cohorts)
for (i in 1:nrow(cohorts)) {
    n <- sum(is.na(cohorts[i,]))
    if ( (n/c) >= threshold) {
        ##print (paste0(i, " - " , round(n/c,2)))
        too_many_na <- c(too_many_na, i)
    }
}
cohorts <- cohorts[-c(too_many_na),]
print("removed rows with too many na's")

cohorts$CIPCODE <- as.numeric(cohorts$CIPCODE)
cohorts$OPEID6 <- as.numeric(cohorts$OPEID6)


## remove characters
###cohorts <- select(cohorts, !matches("BBRR"))
cohorts <- cohorts[, !sapply(cohorts, is.character)]

## drop duplicate columns that appear in institutions
cohorts <- subset(cohorts, select=-c(UNITID))

## remove columns with too many NAs
too_many_na <- c()
threshold <- 0.70
#threshold <- 0.50
c <- nrow(cohorts)
for (i in 1:ncol(cohorts)) {
    n <- sum(is.na(cohorts[,i]))
    if ( (n/c) >= threshold) {
        ##print (paste0(i, " - " , round(n/c,2)))
        too_many_na <- c(too_many_na, i)
    }
}
cohorts <- cohorts[-c(too_many_na)]


## foreach column with na's create an indicator column showing we had to account for it
cnames <- colnames(cohorts)
for (n in cnames) {
    if (sum(is.na(cohorts[,n]) > 0)) {
      na_name <- paste0(n, '_NA')
      cohorts <- cohorts %>% mutate (!!na_name := ifelse(is.na(!!as.name(n)), 1, 0 ))
  }
}

# replace NAs with 0
cohorts[is.na(cohorts)] <- 0
summary(cohorts)


cipcode <- read_csv('cipcode.csv', col_types='nnc')
cipcode <- cipcode %>% mutate (STEM = ifelse(STEM == 0, 0, 1))
cipcode <- cipcode[,-3]
cohorts <- merge(cipcode, cohorts, by="CIPCODE")



tier <- read_csv('tiers.csv', col_types='cnn')
tier <- tier[,-c(1)]
cohorts <- merge(tier, cohorts, by="OPEID6")

dim(cohorts)

print ("finished reading cohorts")
end_time <- Sys.time()
print(end_time - start_time)


