library(tidyverse)

insts <- read_csv("sample.csv", col_types="cc", na=c("NULL", "PrivacySuppressed"))

target_1 <- "<=0.05"
target_2 <- "0.03-0.04"


## average_range <- function(value) {
##     if (str_detect( str_replace_all(value, "<=", "0-"), '-')) {
##         s <- as.numeric(unlist(str_split(value, "-")))
##         return (mean(s))
##     }
##     return (value)
## }

average_range <- function(values) {
    values <- str_replace_all(values, "<=", "0-")
    c <- length(values)+1
    for (i in 1:c) {
        if (!is.na(values[i]) & str_detect(values[i] , '-')) {
            s <- as.numeric(unlist(str_split(values[i], '-')))
            values[i] <- mean(s)
        }
    }

    return(unlist(values))
}


#print(average_range(target_2))

#target_1 <- str_replace_all(target_1, "<=", "0-")
#print(average_range(target_1))

#insts <- insts %>% mutate(
#BBRR1_FED_UG_DLNQ = unlist(lapply(BBRR1_FED_UG_DLNQ, average_range))
#                       )

insts <- data.frame(sapply(insts, average_range))
