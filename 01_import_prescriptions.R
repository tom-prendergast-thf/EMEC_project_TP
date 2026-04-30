# 01 Loading prescriptions data ################################################
# updated 31/03/2026 to take August 24 - August 25 matching FP data ############


library(openxlsx)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)

# Note: dates covered: July 2024 - June 2025

# Cat 1: inhalers
# Cat 2: Anti-anxiety meds
# Cat 3: Anti-depressants


####################
########### INHALERS
####################


#Dates
date <- c('-04-01', '-05-01', '-06-01', '-07-01', '-08-01', 
              '-09-01', '-10-01', '-11-01', '-12-01', '-01-01', '-02-01', '-03-01')

years <- c('2021', '2022', '2023', '2024', '2025')

all_dates <- lapply(1:4, function(i){
  list <- c(paste0(years[i], date[1]), paste0(years[i], date[2]), paste0(years[i], date[3]),
    paste0(years[i], date[4]), paste0(years[i], date[5]), paste0(years[i], date[6]),
    paste0(years[i], date[7]), paste0(years[i], date[8]), paste0(years[i], date[9]),
    paste0(years[i+1], date[10]), paste0(years[i+1], date[11]), paste0(years[i+1], date[12]))
  return(list)
})

full_date_list <- c(all_dates[[1]], all_dates[[2]], all_dates[[3]], all_dates[[4]])

# Codes for respiratory drugs
resp_codes <- list('0301', '0302', '0303')


# Function for loading prescriptions
read_presc_function <- function(codes){
  
  full_list <- list()
  
  for (i in 1:length(codes)){
    
    minilist <- list()
    
    for (x in 1:length(full_date_list)){
      
      single_df <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_org/?org_type=practice&code=", codes[[i]], "&date=", full_date_list[[x]],"&format=csv"),
                            col_types = list(ccg = col_character(),
                                             row_id = col_character(),
                                             row_name = col_character(),
                                             actual_cost = col_double(),
                                             items = col_double(),
                                             quantity = col_double(),
                                             setting = col_character(),
                                             date = col_date()
                            ))
      
      single_df$bnf_code <- codes[[i]]
      
      minilist <- append(minilist, list(single_df))
      
    }
    
    combined_df <- bind_rows(minilist)
    
    full_list <- append(full_list, list(combined_df))
    
  }
  
  return(full_list)
  
}

full_inhaler_list <- read_presc_function(codes = resp_codes)

# Create df

full_inhaler_df <- bind_rows(full_inhaler_list)

full_inhaler_df <- full_inhaler_df %>% select(!(X1:X8))

# Group df

inhalers_grouped <- full_inhaler_df %>%
  group_by(bnf_code, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


#################################################################
######### FORGET ALL THE ABOVE
################################################################

# Load in numbered prescription sheets

numbers <- c(1:48)

all_prescriptions <- lapply(1:length(numbers), function(i){
  df <- read_csv(paste0('Data/Prescriptions/spending-by-practice-0301-0302-0303 (', numbers[i], ').csv'))
})

all_prescriptions <- do.call('rbind', all_prescriptions)

all_prescriptions$date <- lubridate::as_date(all_prescriptions$date)

all_prescriptions <- all_prescriptions %>%
  mutate(year = case_when((date >= '2021-04-01') & (date < '2022-04-01') ~ '2022',
                        (date >= '2022-04-01' & date < '2023-04-01') ~ '2023',
                        (date >= '2023-04-01' & date < '2024-04-01') ~ '2024',
                        (date >= '2024-04-01' & date < '2025-04-01') ~ '2025'))


prescriptions_grouped <- all_prescriptions %>%
  group_by(row_id, year) %>%
  summarise(cost = sum(actual_cost), items = sum(items), quantity = sum(quantity)) %>%
  rename(PRACTICE_CODE = row_id)

write.csv(prescriptions_grouped, 'Data/Prescriptions/all_prescriptions_grouped.csv')
