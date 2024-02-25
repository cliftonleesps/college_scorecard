library(tidyverse)
load('2024-02-22.Rdata')

start_time <- Sys.time()

threshold_correlation <- 10
threshold_variance <- 10

final_columns <- data.frame()

## get admission columns
variables <- read_csv('variables2.csv', show_col_types=FALSE)
target_column <- cohorts %>% select('OPEID6','EARN_NE_MDN_3YR')
categories <- variables %>% filter(category != 'programs') %>% select(category) %>% arrange() %>% unique()
#categories <- variables  %>% select(category) %>% arrange() %>% unique()

## get the top predictors at the institution level
for (c in categories$category) {
    print(c)
    #admission_columns <- variables %>% filter(category == c) %>%  filter(!str_detect(name, 'EARN_')) %>% select(name)
    admission_columns <- variables %>% filter(category == c) %>% select(name)
    #print(admission_columns)
    temp_columns <- intersect(admission_columns$name, colnames(insts))
    temp_columns <- c(temp_columns, 'OPEID6')
    admission_columns <- insts %>% select(starts_with(temp_columns))
    admission_columns <- merge(admission_columns, target_column, by='OPEID6')
    ##admission_columns %>% head()


    ## calc the variance and correlation
    ## variances = data.frame(c = cor_names)
    temp_dataframe <- data.frame()
    cor_names <- setdiff(colnames(admission_columns),c('EARN_NE_MDN_3YR', 'OPEID6','OPEID6_NA'))
    for (cname in cor_names) {
        ##print(paste0(c, ' ', cor(admission_columns$EARN_NE_MDN_3YR, admission_columns[[c]])))
        #print(paste0(c, ' ', var(admission_columns[[c]])))
        temp_row <- data.frame(column = cname,
                               variance = var(admission_columns[[cname]]),
                               correlation = cor(admission_columns$EARN_NE_MDN_3YR, admission_columns[[cname]])
                               )
        temp_dataframe <- rbind(temp_dataframe, temp_row)
    }

    ## reduce by dropping lowest variance and keeping top correlation
    top_correlation <- temp_dataframe %>% arrange(desc(abs(correlation))) %>% head(threshold_correlation) %>% select(column)
    top_variance <- temp_dataframe %>% arrange(desc(variance)) %>% head(threshold_variance) %>% select(column)
    final_columns <- rbind(final_columns, top_correlation)
    final_columns <- rbind(final_columns, top_variance)
    print(final_columns)
}


## now get the top predictors at the cohort level
temp_cohorts <- cohorts %>% select(!starts_with('EARN'))
EARN_NE_MDN_3YR <- cohorts$EARN_NE_MDN_3YR
temp_cohorts <- cbind (temp_cohorts, EARN_NE_MDN_3YR)
temp_dataframe <- data.frame()
cnames <- temp_cohorts %>% select(!starts_with('EARN_')) %>% colnames()
for (cname in cnames) {
#    print(cname)
#    print(paste0(cname, ' ', cor(temp_cohorts$EARN_NE_MDN_3YR, temp_cohorts[[cname]])))
    print(paste0(cname, ' ', var(temp_cohorts[[cname]])))
    temp_row <- data.frame(column = cname,
                           variance = var(temp_cohorts[[cname]]),
                           correlation = cor(temp_cohorts$EARN_NE_MDN_3YR, temp_cohorts[[cname]])
                           )
    temp_dataframe <- rbind(temp_dataframe, temp_row)
}
top_correlation <- temp_dataframe %>% arrange(desc(abs(correlation))) %>% head(threshold_correlation) %>% select(column)
top_variance <- temp_dataframe %>% arrange(desc(variance)) %>% head(threshold_variance) %>% select(column)
final_columns <- rbind(final_columns, top_correlation)
final_columns <- rbind(final_columns, top_variance)

## add TIER AND STEM
final_columns <- rbind(final_columns, data.frame(column = 'OPEID6'))
final_columns <- rbind(final_columns, data.frame(column = 'TIER'))
final_columns <- rbind(final_columns, data.frame(column = 'STEM'))

final_columns


df <- insts %>% select(intersect(colnames(insts), final_columns$column))
#cohorts_df <- cohorts %>% select('OPEID6', 'EARN_NE_MDN_3YR')
df <- merge(df, temp_cohorts, by='OPEID6')
linear_model <- lm(EARN_NE_MDN_3YR ~., df)
summary(linear_model)


end_time <- Sys.time()
print(end_time - start_time)
