# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

# 2.0 DATA IMPORT ----

# 2.1 Loan Acquisitions Data ----

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))

acquisition_data <- vroom(
  file       = "data-science/00_data/loan_data/Acquisition_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_acq),
  col_types  = col_types_acq,
  na         = c("", "NA", "NULL"))
acquisition_data %>% glimpse()

# 2.2 Performance Data ----
col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "data-science/00_data/loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))

performance_data %>% glimpse()