library(dplyr)
library(bit64)

fintech <- readRDS('C:/Users/Rudolfs/Desktop/grand_datasets/credit_Datasets/processed_data.rds')
fintech$loan_id <- as.integer.integer64(fintech$loan_id)
fintech$application_status <- ifelse(fintech$application_status == 'LOAN_ISSUED', 'ISSUED', fintech$application_status)

source('R/contingency_tables.R')
source('R/loandf.R')
source('R/stats.R')
source('R/stats_functions.R')
source('R/get_PSI.R')
source('R/utils.R')
source('R/cross_validation.R')

library(loan)

data(fintech)
df <- fintech

df$city <- NULL
df$district <- NULL
df$contact_relationship <- NULL

## TO DO: create target variable with pre-defined classes
df$class <- ifelse(df$fpd >= 15, 'BAD', 'GOOD')



###################### CONTINGENCY TABLES

## simple contingency table 
produce_contingency_table(df$gender, df$class)

## multiple classes target
produce_contingency_table(df$gender, df$marriage_status)

## with application statuses
produce_contingency_table(df$gender, df$class, application_status = df$application_status) %>%
  attributes()

## with template provided: TO DO

## many, many factors
produce_contingency_table(df$bank_name, df$class)
produce_contingency_table(df$bank_name, df$class, classes_limit = 10)

## character variable
produce_contingency_table(as.character(df$bank_name), df$class)

## data frame
produce_contingency_table(df, 'bank_name', 'class', classes_limit = 5)
produce_contingency_table(df, 'gender', 'class', 'application_status')

## numeric variable
produce_contingency_table(df$client_age, df$class)
produce_contingency_table(df$client_age, df$class, breaks = 10)

## template provided
c1 <- produce_contingency_table(df[df$loan_seq_number == 1, ], 'client_age', 'class')
c2 <- produce_contingency_table(df[df$loan_seq_number > 1, ], 'client_age', 'class')
c1; c2 #different the_var group ranges
c3 <- produce_contingency_table(df[df$loan_seq_number > 1, ], 'client_age', 'class', template_mat = c1)
c1; c3 #same intervals
c4 <- produce_contingency_table(df[df$loan_seq_number == 1, ], 'client_age', 'class', template_mat = c1)
c1; c4 #slight rounding differences. Shall be fixed but not the priority

###################### STATS

## provided as vectors
get_group_stats(df$client_age, df$class, stats = 'fisher_p_val')
get_group_stats(df$client_age, df$class, application_status = df$application_status, stats = 'fisher_p_val')

## provided as data
get_group_stats(df, 'client_age', binary_outcome = 'class', breaks = 10, stats = 'fisher_p_val')
get_group_stats(df[!is.na(df$fpd), ], 'loan_seq_number', binary_outcome = 'class', breaks = 10, stats = 'fisher_p_val')

get_group_stats(df[!is.na(df$fpd), ], 'client_age', binary_outcome = 'class', application_status = 'application_status',
                breaks = 10, stats = 'fisher_p_val')

## woe
get_group_stats(df, 'client_age', binary_outcome = 'class', breaks = 10, stats = c('woe'))
get_group_stats(df, 'client_age', binary_outcome = 'class', application_status = 'application_status', 
                breaks = 10, stats = c('woe'))

## fisher test
get_group_stats(df, 'client_age', binary_outcome = 'class', breaks = 10, stats = c('woe', 'fisher_p_val'))
get_group_stats(df, 'client_age', binary_outcome = 'class', breaks = 10, stats = c('woe', 'fisher_p_val'),
                p.adjust_func = NULL)


## plot
get_group_stats(df, 'employment_category', binary_outcome = 'class', stats = c('woe', 'fisher_p_val')) %>%
  plot()


###################### PSI
## low level
cont_table_prev <- produce_contingency_table(df[as.Date(df$app_created_at) < as.Date('2023-01-01'), ], 'client_age', 'class', 'application_status')
cont_table <- produce_contingency_table(df[as.Date(df$app_created_at) > as.Date('2023-06-01'), ], 'client_age', 'class', 'application_status', template_mat = cont_table_prev)

calculate_PSI_table(cont_table_prev, cont_table)
calculate_PSI_table(cont_table_prev, cont_table, param = 'applications_total')
calculate_PSI_table(cont_table_prev, cont_table, param = 'count_CANCELLED')


## higher level
calculate_PSI(df$gender, df$class, 
              time_split_base = as.Date(df$app_created_at) < as.Date('2023-01-01'),
              time_split_comparison = as.Date(df$app_created_at) > as.Date('2023-06-01'))
calculate_PSI(df$client_age, df$class, param_name = 'applications_total',
              time_split_base = as.Date(df$app_created_at) < as.Date('2023-01-01'),
              time_split_comparison = as.Date(df$app_created_at) > as.Date('2023-06-01')) #shalll trigger an R
calculate_PSI(df$client_age, df$class, param_name = 'applications_total', application_status = df$application_status,
              time_split_base = as.Date(df$app_created_at) < as.Date('2023-01-01'),
              time_split_comparison = as.Date(df$app_created_at) > as.Date('2023-06-01')) 

## df input
calculate_PSI(df, 'gender', 'class',
              time_split_base = as.Date(df$app_created_at) < as.Date('2023-01-01'),
              time_split_comparison = as.Date(df$app_created_at) > as.Date('2023-06-01'))


###################### dynamic_stability
## data.frame
ss <- get_dynamic_stats(df[df$loan_seq_number == 1, ], variables = c('client_age', 'gender', 'utm_source', 'gross_salary',
                                                                     'major_email_domain', 'marriage_status'),
                  time_splits = 'month',
                  base_period = as.Date(df$app_created_at[df$loan_seq_number == 1]) < as.Date('2023-01-01'),
                  application_created_at = 'app_created_at',
                  target = 'class',
                  application_status = NULL
                )




###################### PSI
get_PSI(x)
 get_PSI(x, variables = 'client_age')
## TO DO: lot's of customization

 usethis::use_data()
###################### dynamic stability


######################
## TO DO: Marginal effect plot (what are the highest profits for each score threshold).
# Option to adjust bad rates / lifetime values for score cutoffs (in case of economic condition changes or other factors not accounted in the scorecard)

######################
## TO DO: Survival analysis: LGD by score buckets (page 474)

######################
## TO DO: PSI with some kind of traffic lights (page 488)

######################
## TO DO: RETENTION RATES FROM one loan seq to next - loans issued and applications created
 
