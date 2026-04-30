# 3. Diagnostic tests

full_panel_df <- read_csv('Data/full_panel_df.csv')

full_panel_df <- full_panel_df %>%
  filter(!(MSOA_code == 'E02005646'))

plot(full_panel_df$EPC_prop_C, full_panel_df$cost_per_1000head_real) 


###########################
### Balanced panel?
###########################

full_panel_df %>% group_by(MSOA_code) %>%
  summarise(n_years = n_distinct(year)) %>% count(n_years)

na <- full_panel_df |>
  summarise(across(everything(), ~sum(is.na(.))))


########################################
###### Distribution of outcome
########################################

hist(full_panel_df$cost_per_1000head_real)

# Overall distribution
ggplot(full_panel_df, aes(x = cost_per_1000head_real)) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  facet_wrap(~year) +
  labs(x = "Respiratory prescribing cost (£)", y = "MSOAs")

var(full_panel_df$cost_per_1000head_real)/mean(full_panel_df$cost_per_1000head_real)

mean(full_panel_df$cost_per_1000head_real)/var(full_panel_df$cost_per_1000head_real)

ggplot(full_panel_df |> filter(cost_per_1000head_real > 0), aes(x = log(cost_per_1000head_real))) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  facet_wrap(~year)

###################################
#### Within-MSOA variation 
###################################

# Compute within-MSOA standard deviation of exposure
within_variation <- full_panel_df |>
  group_by(MSOA_code) |>
  summarise(
    within_sd = sd(EPC_prop_C, na.rm = TRUE),
    mean_epc = mean(EPC_prop_C, na.rm = TRUE),
    range_epc = max(EPC_prop_C, na.rm = TRUE) - 
      min(EPC_prop_C, na.rm = TRUE)
  )

summary(within_variation$within_sd)
summary(within_variation$range_epc)

# What proportion of MSOAs show essentially no variation?
mean(within_variation$within_sd < 0.01, na.rm = TRUE)



#######################################
### Instrumentation needed? 
#######################################

# First stage: direct entry of instrument (no year interaction needed)
first_stage <- feols(
  EPC_prop_C ~ perc_pre_1919 + msoa_smokprev + 
    perc_over_65 + PM25 |
    MSOA_code + year,
  cluster = ~LA_code,    # THIS IS SOMETHING TO CHECK
  data = full_panel_df         # Cluster at MSOA?
)

summary(first_stage)
fitstat(first_stage, type = "ivf")

model_iv <- feols(
  log(cost_per_1000head_real) ~ msoa_smokprev + perc_over_65 + PM25 |
    MSOA_code + year |
    EPC_prop_C ~ perc_pre_1919,
  cluster = ~LA_code,             # AGAIN, CHECK THIS
  data = full_panel_df
)

##############################
## Endogeneity? Wu-Hausman
##############################

# Full IV model
model_iv <- feols(
  log(cost_per1000head_real) ~ msoa_smokprev + perc_over_65 + PM25 |
    MSOA_code + year |
    EPC_prop_C ~ i(year, perc_pre_1919),
  cluster = ~LA_code,
  data = full_panel_df
)

summary(model_iv)

# Wu-Hausman endogeneity test
fitstat(model_iv, type = "wuh")

# First-stage F-statistic
fitstat(model_iv, type = "ivf")


#################################
### Multicollinearity?
#################################

library(car)

# Pooled OLS as a collinearity diagnostic proxy
# (VIF is not defined for FE models but pooled gives a useful approximation)
ols_proxy <- lm(log(cost_per_1000head_real) ~ EPC_prop_C + msoa_smokprev +
                  perc_over_65 + PM25 + factor(MSOA_code) + factor(year),
                data = full_panel_df)

# VIF on substantive variables only (omit MSOA dummies)
vif_vals <- vif(ols_proxy)
vif_vals[c("EPC_prop_C", "msoa_smokprev", "perc_over_65", "PM25")]

# Correlation matrix of time-varying covariates
full_panel_df |>
  select(EPC_prop_C, msoa_smokprev, perc_over_65, PM25) |>
  cor(use = "complete.obs") |>
  round(3)


################################
### Serial autocorrelation?
################################

library(plm)

pdata <- pdata.frame(full_panel_df, index = c("MSOA_code", "year"))

wooldridge_test <- pbgtest(
  cost_per_1000head_real ~ EPC_prop_C + msoa_smokprev + perc_over_65 + PM25,
  data = pdata
)
print(wooldridge_test)



#########################################
## Spatial autocorrelation?
#########################################

library(sf)
library(spdep)

# Load MSOA boundaries
msoa_sf <- st_read("Data/Lookups/MSOA_boundaries.shp")

# Construct queen contiguity weights matrix
nb <- poly2nb(msoa_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Estimate baseline FE model and extract residuals
model_base <- feols(
  cost_per_1000head_real ~ EPC_prop_C + msoa_smokprev + perc_over_65 + PM25 |
    MSOA_code + year,
  cluster = ~LA_code,
  data = full_panel_df
)

resids <- residuals(model_base)

# Moran's I test on residuals for one year (repeat per year)
resids_2024 <- resids[full_panel_df$year == 2024]
moran.test(resids_2024, lw, zero.policy = TRUE)

# Plot spatial distribution of residuals
msoa_sf$resid_2024 <- resids_2024
ggplot(msoa_sf) +
  geom_sf(aes(fill = resid_2024), colour = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(title = "Spatial distribution of model residuals, 2024")



#############################
## Functional form (?)
#############################



##############################
## Influential observations?
##############################



##########################################
### Fixed effects or random effects?
##########################################





