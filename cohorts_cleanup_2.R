load('impute_cohorts.2.Rda')
cohorts <- impute_cohorts; rm(impute_cohorts)
cohorts$CIPCODE <- as.numeric(cohorts$CIPCODE)
cohorts$OPEID6 <- as.numeric(cohorts$OPEID6)
cohorts$CONTROL <- as.numeric(cohorts$CONTROL)
cohorts$CIPDESC <- as.factor(cohorts$CIPDESC)
