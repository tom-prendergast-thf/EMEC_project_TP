
#Load and process data

setwd("C:\\Users\\tom.prendergast\\OneDrive - The Health Foundation\\Desktop\\Econometrics project")

# Modal function
get_mode <- function(x) {
  unique_vals <- unique(x)
  unique_vals[which.max(tabulate(match(x, unique_vals)))]
}

# LSOA to MSOA lookup

LSOA_lookup <- read_csv('Data/Lookups/LSOA_MSOA_Lookup.csv') %>%
  select(lsoa21cd, msoa21cd, ladcd) %>% 
  group_by(lsoa21cd) %>%
  summarise(MSOA_code = get_mode(msoa21cd), LA_code = get_mode(ladcd))

# MSOA 11 to 21 lookup
MSOA_lookup <- read_csv('Data/Lookups/MSOA_11_to_21_lookup.csv')

# LSOA 11 to 21 lookup

LSOA_11_21_lookup <- read_csv('Data/Lookups/LSOA11_to_21_lookup.csv')

# MSOA to LA lookup 

MSOA_to_LA <- LSOA_lookup %>%
  group_by(MSOA_code) %>%
  summarise(LA_code = get_mode(LA_code)) %>%
  filter(grepl('E', MSOA_code))

###############################################
#### MSOA LEVEL DATA ####
###############################################

# EPC by MSOA

EPC_years <- c('2022', '2023', '2024', '2025')

EPC_data <- lapply(1:4, function(i){
  if (i < 3){
  df <- read_csv(paste0('Data/EPC/Clean/EPC_OverC_', EPC_years[i], '_clean.csv')) %>%
        mutate(year = EPC_years[i]) %>%
    select(year, MSOA_code = `Middle super output layer (MSOA) code`, MSOA_name = `Middle super output layer (MSOA) name`, EPC_prop_C = `All dwellings`) %>%
    left_join(MSOA_lookup, .,  by = join_by(MSOA11CD == MSOA_code)) %>%
    group_by(MSOA21CD, year) %>%
    summarise(MSOA_name = get_mode(MSOA_name), EPC_prop_C = mean(EPC_prop_C)) %>%
    rename(MSOA_code = MSOA21CD)
  } else {
    df <- read_csv(paste0('Data/EPC/Clean/EPC_OverC_', EPC_years[i], '_clean.csv')) %>%
      mutate(year = EPC_years[i]) %>%
      select(year, MSOA_code = `Middle super output layer (MSOA) code`, MSOA_name = `Middle super output layer (MSOA) name`, EPC_prop_C = `All dwellings`)
  }
  })

EPC_data <- do.call('rbind', EPC_data)

# Age of housing by MSOA

housing_age_data <- lapply(1:4, function(i){
  
  if (i < 3) {
    df <- read_csv(paste0('Data/Housing/Housing_age_MSOA_', EPC_years[i], '.csv')) %>%
      filter(geography == 'MSOA' & band == 'All') %>%
      mutate(year = EPC_years[i]) %>%
      left_join(MSOA_lookup, .,  by = join_by(MSOA21CD == ecode)) %>%
      mutate(bp_pre_1900 = case_when(bp_pre_1900 == '-' ~ '0',
                                     TRUE ~ bp_pre_1900)) %>%
      mutate(bp_1900_1918 = case_when(bp_1900_1918 == '-' ~ '0',
                                      TRUE ~ bp_1900_1918)) %>%
      group_by(MSOA21CD, year) %>%
      summarise(MSOA_name = get_mode(area_name), bp_pre_1900 = max(as.numeric(bp_pre_1900)), bp_1900_1918 = max(as.numeric(bp_1900_1918)),
                all_properties = max(as.numeric(all_properties))) %>%
      rename(MSOA_code = MSOA21CD)
    
  } else {df <- read_csv(paste0('Data/Housing/Housing_age_MSOA_', EPC_years[i], '.csv')) %>%
    mutate(year = EPC_years[i]) %>%
    filter(geography == 'MSOA' & band == 'All') %>%
    mutate(bp_pre_1900 = case_when(bp_pre_1900 == '-' ~ 0,
                                   TRUE ~ as.numeric(bp_pre_1900))) %>%
    mutate(bp_1900_1918 = case_when(bp_1900_1918 == '-' ~ 0,
                                    TRUE ~ as.numeric(bp_1900_1918))) %>%
    mutate(all_properties = as.numeric(all_properties)) %>%
    select(MSOA_code = ecode, year, MSOA_name = area_name, bp_pre_1900, bp_1900_1918, all_properties)
    
    }
  
  })


housing_age_data <- do.call('rbind', housing_age_data) %>%
  mutate(all_pre_1919 = bp_pre_1900 + bp_1900_1918) %>%
  mutate(perc_pre_1919 = all_pre_1919/as.numeric(all_properties)) %>%
  select(year, MSOA_code, perc_pre_1919)

# Age distribution by MSOA

var_years <- c('2022', '2023', '2024', '2025')

age_data <- lapply(1:length(var_years), function(i){
  if (i < 1) {
    df <- read_csv(paste0('Data/Age/Clean/Age_clean_', var_years[i], '.csv')) %>%
      mutate(year = var_years[i]) %>%
      left_join(MSOA_lookup, .,  by = join_by(MSOA11CD == MSOA_code)) %>%
      group_by(MSOA21CD, year) %>%
      summarise(MSOA_name = get_mode(MSOA_name), perc_over_65 = mean(perc_over_65),
                Total = mean(Total)) %>%
      rename(MSOA_code = MSOA21CD)
    
  } else {df <- read_csv(paste0('Data/Age/Clean/Age_clean_', var_years[i], '.csv')) %>%
    mutate(year = var_years[i])}
  
  }) 

age_data <- do.call('rbind', age_data) %>%
  select(year, MSOA_code, MSOA_name, total_pop = Total, perc_over_65)

# Income by MSOA

##### Looks like we can't include this...


# Air quality by LA

air_data <- lapply(1:length(var_years), function(i){
  
  if (i == 1){
    df <- read_csv(paste0('Data/PM2/Air_quality_', var_years[i], '.csv')) %>%
      group_by(LA_code) %>%
      summarise(PM25 = mean(PM25)) %>%
      mutate(year = var_years[i])
  } else{
    df <- read_csv(paste0('Data/PM2/Air_quality_', var_years[i], '.csv')) %>%
    mutate(year = var_years[i])}
  
  })

air_data <- do.call('rbind', air_data) %>%
  filter(grepl('E', LA_code)) %>%
  rename(old_LA_code = LA_code) %>%
  mutate(LA_code = case_when(old_LA_code == 'E08000016' ~ 'E08000038',
                                old_LA_code == 'E08000019' ~ 'E08000039',
                               old_LA_code %in% c('E07000004', 'E07000005', 
                                                  'E07000006', 'E07000007') ~ 'E06000060',
                             old_LA_code %in% c('E07000150', 'E07000152',
                                                'E07000153', 'E07000156') ~ 'E06000061',
                             old_LA_code %in% c('E07000151', 'E07000154',
                                                'E07000155') ~ 'E06000062',
                             old_LA_code %in% c('E07000026',
                                                'E07000028', 'E07000029') ~ 'E06000063',
                             old_LA_code %in% c('E07000027', 'E07000030',
                                                'E07000031') ~ 'E06000064',
                             old_LA_code %in% c('E07000163', 'E07000164', 'E07000165', 
                                                'E07000166', 'E07000167', 'E07000168', 
                                                'E07000169') ~ 'E06000065',
                             old_LA_code %in% c('E07000187', 'E07000188',
                                                'E07000189', 'E07000246') ~ 'E06000066',
                                TRUE ~ old_LA_code)) 

## Combine MSOA level data

all_msoa_data <- left_join(EPC_data, housing_age_data, by = join_by(MSOA_code, year)) %>%
  left_join(., age_data, by = join_by(MSOA_code, year)) %>%
  filter(grepl('E', MSOA_code)) %>%
  left_join(., MSOA_to_LA, by = 'MSOA_code') %>%
  left_join(., air_data, by = join_by(LA_code, year)) %>%
  select(MSOA_code, LA_code, year, MSOA_name = MSOA_name.y, EPC_prop_C, perc_pre_1919,
         perc_over_65, total_pop, PM25)

###############################################
#### PRACTICE LEVEL DATA ####
###############################################

# Patients registered in GP practice

gp_patients_data <- lapply(1:length(var_years), function(i){
  df <- read_csv(paste0('Data/GP/GP_Patients_', var_years[i], '.csv')) %>%
    mutate(year = var_years[i]) %>%
    filter(SEX == 'ALL') %>%
    group_by(PRACTICE_CODE) %>%
    mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
    dplyr::ungroup() %>%
    mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS)}) 


# Smoking rates by GP practice

smoking_data <- lapply(1:length(var_years), function(i){
  df <- read_csv(paste0('Data/Smoking/Clean/QOF', var_years[i], '_clean.csv')) %>%
    mutate(year = var_years[i]) %>%
  mutate(List_size_15plus = as.numeric(List_size_15plus)) %>%
    mutate_at(c('List_size_15plus'), ~replace_na(.,0))
  
  Smokers <- as.numeric(df[[4]])
  
  df <- df %>%       
          mutate(smoking_prev = Smokers/List_size_15plus) %>%
    mutate(smoking_prev = case_when(smoking_prev == Inf ~ 0,
                                    TRUE ~ smoking_prev)) %>%
    select(PRACTICE_CODE = Practice_code, smoking_prev
           ) })

#  Profile to LSOAs and sum to MSOA

join_practices_smoking <- function(practices_df, smoking_df){ 
  practices_joined <- left_join(practices_df, smoking_df, by = join_by(PRACTICE_CODE)) %>%
    mutate_at(c('smoking_prev'), ~replace_na(.,0)) %>%
    mutate(smokerate_per_LSOA = smoking_prev*PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(smoking_prev = sum(smokerate_per_LSOA), NUMBER_OF_PATIENTS = sum(NUMBER_OF_PATIENTS))
  
  return(LSOA_summed)
}


smoking_by_lsoa <- lapply(1:length(var_years), function(i){
  df <- join_practices_smoking(practices_df = gp_patients_data[[i]], smoking_df = as.data.frame(smoking_data[[i]])) %>%
    mutate(year = var_years[[i]])
})

smoking_msoa <- do.call('rbind', smoking_by_lsoa) %>%
  left_join(., LSOA_11_21_lookup, by = join_by(LSOA_CODE == LSOA11CD)) %>%
  group_by(LSOA21CD, year) %>%
  mutate(lsoa21_sum = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(lsoa_weight = NUMBER_OF_PATIENTS/lsoa21_sum,
         weighted_smokprev = smoking_prev*lsoa_weight) %>%
  group_by(LSOA21CD, year) %>%
  summarise(lsoa21_smokprev = sum(weighted_smokprev), NUMBER_OF_PATIENTS = mean(NUMBER_OF_PATIENTS)) %>%
  left_join(., LSOA_lookup, by = join_by(LSOA21CD == lsoa21cd)) %>%
  group_by(MSOA_code, year) %>%
  mutate(msoa_sum = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(lsoa_weight = NUMBER_OF_PATIENTS/msoa_sum,
         weighted_smokprev = lsoa21_smokprev*lsoa_weight) %>%
  group_by(MSOA_code, year) %>%
  summarise(msoa_smokprev = sum(weighted_smokprev)) %>%
  filter(grepl('E', MSOA_code))
  


# Incorporate prescriptions

prescriptions_grouped <- read_csv('Data/Prescriptions/all_prescriptions_grouped.csv')

join_practices_prescriptions <- function(practices_df, prescription_df){ 
  practices_joined <- left_join(practices_df, prescription_df, by = join_by(PRACTICE_CODE)) %>%
    mutate_at(c('items', 'quantity', 'cost'), ~replace_na(.,0)) %>%
    mutate(items_per_LSOA = items*PATIENT_PROPORTION) %>%
    mutate(quantity_per_LSOA = quantity*PATIENT_PROPORTION) %>%
    mutate(cost_per_LSOA = cost*PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(items = sum(items_per_LSOA), quantity = sum(quantity_per_LSOA), cost = sum(cost_per_LSOA))
  
  return(LSOA_summed)
}


prescriptions_by_lsoa <- lapply(1:length(var_years), function(i){
  df <- join_practices_prescriptions(practices_df = gp_patients_data[[i]], prescription_df = prescriptions_grouped[prescriptions_grouped$year == var_years[[i]],]) %>%
    mutate(year = var_years[[i]])
})

prescriptions_msoa <- do.call('rbind', prescriptions_by_lsoa) %>%
  left_join(., LSOA_11_21_lookup, by = join_by(LSOA_CODE == LSOA11CD)) %>%
  group_by(LSOA21CD, year) %>%
  mutate(lsoa_items = sum(items),
         lsoa_quantity = sum(quantity),
         lsoa_cost = sum(cost)) %>%
  left_join(., LSOA_lookup, by = join_by(LSOA21CD == lsoa21cd)) %>%
  group_by(MSOA_code, year) %>%
  summarise(msoa_items = sum(lsoa_items),
         msoa_quantity = sum(lsoa_quantity),
         msoa_cost = sum(lsoa_cost)) %>%
  filter(grepl('E', MSOA_code)) %>%
  filter(year %in% c('2022', '2023', '2024', '2025'))


##################################################
###### COLLATE ALL DATA
#################################################

# Deflator

deflator <- read_csv('Data/Deflator/gdp_deflator_March_2026.csv')

deflator$year <- c('2020', '2021', '2022', '2023', '2024', '2025')

deflator <- deflator %>% select(year, deflator)

current_val <- deflator$deflator[deflator$year == '2025']

# Create full panel

full_panel_df <- all_msoa_data %>%
  left_join(., smoking_msoa, by = join_by(MSOA_code, year)) %>%
  filter(year %in% c('2022', '2023', '2024', '2025')) %>%
  left_join(., prescriptions_msoa, by = join_by(MSOA_code, year)) %>%
  mutate(perc_pre_1919 = perc_pre_1919*100,
         perc_over_65 = perc_over_65*100,
         msoa_smokprev = msoa_smokprev*100,
    cost_per_1000head = (msoa_cost/total_pop)*1000,
         items_per_1000head = (msoa_items/total_pop)*1000,
         quant_per_1000head = (msoa_quantity/total_pop)*1000) %>%
  left_join(., deflator, by = 'year') %>%
  mutate(msoa_cost_real = msoa_cost/deflator*current_val,
         cost_per_1000head_real = cost_per_1000head/deflator*current_val)
  
write.csv(full_panel_df, 'Data/full_panel_df.csv')
