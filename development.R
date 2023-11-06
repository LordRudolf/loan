library(dplyr)
library(bit64)

fintech <- readRDS('C:/Users/Rudolfs/Desktop/grand_datasets/credit_Datasets/processed_data.rds')
fintech$loan_id <- as.integer.integer64(fintech$loan_id)
fintech$application_status <- ifelse(fintech$application_status == 'LOAN_ISSUED', 'ISSUED', fintech$application_status)

source('R/loandf.R')
source('R/woe.R')
source('R/get_PSI.R')
source('R/utils.R')


df <- fintech[1:200000, c(1:27, which(colnames(fintech) %in% c('fpd15',
                                                               'open_loans',
                                                               'nic_in_ext_blocklist',
                                                               'marriage_status',
                                                               'gross_salary',
                                                               'education_level',
                                                               'employment_category',
                                                               'length_of_income',
                                                               'utm_source',
                                                               'loan_seq_number',
                                                               'loans_repaid_30_days',
                                                               'client_age',
                                                               'apps_total_apps_installed',
                                                               'juicy_antifraud_score',
                                                               'seon_phone_score')))]


df$city <- NULL
df$district <- NULL
df$contact_relationship <- NULL

## TO DO: create target variable with pre-defined classes
df$class <- ifelse(df$fpd15 == 1, 'BAD', 'GOOD')

client_id <- 'client_id'

x <- loan_df(df,
             application_id = 'application_id',
             loan_id = 'loan_id',
             client_id = 'client_id',
             application_created_at = 'app_created_at',
             application_status = 'application_status',
             loan_status = 'loan_status',
             target = 'class',
             data_downloaded_at = as.Date('2021-10-30'),
             workflow = list()
             )
print(x)
View(attributes(x))


###################### CONTINGENCY TABLES
get_group_stats(x, 'client_age')
get_group_stats(df, 'client_age')
get_group_stats(x, 'client_age', breaks = 6)
cont_table <- get_group_stats(x, 'employment_category', unique_val = 3)

get_group_stats(x, 'client_age', breaks = 6, stats = 'fisher_p_val')

get_fisher_p_val(cont_table)
## TO DO: p-value, woe accepting other classes than cont_table


###################### PSI
get_PSI(x)
get_PSI(x, variables = 'client_age')
## TO DO: lot's of customization


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
