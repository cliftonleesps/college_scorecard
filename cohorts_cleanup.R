library(mice)

resource("read_cohorts.R")
colnames(cohorts)


## try to impute data


start_time <- Sys.time()

num_multiple_iterations <- 5
num_multiple_iterations <- 2
num_iterations <- 50
num_iterations <- 10
num_iterations <- 3

impute_cohorts <- mice(cohorts,m=num_multiple_iterations,maxit=num_iterations,meth='pmm',seed=500)
impute_cohorts <- complete(impute_cohorts, 1)
summary(impute_cohorts)

end_time <- Sys.time()
print(end_time - start_time)


###saveRDS(impute_cohorts, file="impute_cohorts.3.Rda")
###saveRDS(undergrads, file="undergrads.rds")

impute_cohorts$CIPCODE <- as.numeric(impute_cohorts$CIPCODE)
impute_cohorts$OPEID6 <- as.numeric(impute_cohorts$OPEID6)
impute_cohorts$CIPDESC <- as.factor(impute_cohorts$CIPDESC)

