undergrads <- readRDS('undergrads.rds')

dim(undergrads)

# remove characters & factors
undergrads <- undergrads[, !sapply(undergrads, is.character)]
dim(undergrads)

undergrads <- undergrads[, !sapply(undergrads, is.factor)]
dim(undergrads)

## remove any BBRR repayment columns
undergrads <- undergrads %>% select(-contains('BBRR'))
dim(undergrads)

## remove duplicate MAIN.y
dim(undergrads)
undergrads <- undergrads %>% select(-contains('MAIN.y'))
dim(undergrads)



## select school columns
school_columns <- variables2 %>% filter(category == 'school') %>% select(name)
temp_columns <- intersect(school_columns$name, colnames(undergrads))
school_columns <- undergrads %>% select(all_of(temp_columns))

impute_undergrads <- mice(undergrads,m=num_multiple_iterations,maxit=num_iterations,meth='pmm',seed=500)
undergrads <- complete(impute_undergrads, 1)

