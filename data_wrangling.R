#Set R package location and load packages
.libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.3")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#Define data sources
census_file <- "2010_Census_Demographics_CA.csv"
econ_surv_file <- "ACS_2014_Economic_Estimates_CA.csv"
facilities_file <- "CA_ED_Encounters_by_Expected_Payer.csv"

#Read in census demographic data into dataframe
census <- read_csv(census_file, skip = 2, col_names = FALSE, na = c("", NA, "(X)"))
census <- census %>% select(geo = X3, 
                            tot_pop = X4, 
                            med_age = X42, 
                            pct_over65 = X53, 
                            pct_white = X159,
                            pct_black = X161,
                            pct_hisp = X231,
                            pct_wh_hisp = X233,
                            pct_nonhisp_wh = X249,
                            pct_married_houses = X309,
                            pct_single_moth_houses = X319) %>%
  mutate(geo = tolower(geo))
  census <-  mutate(census, geo = sub(" county, california$", "", x = census$geo))
  census <-  mutate(census, geo = sub("^zcta5 ", "", x = census$geo))


#Read in expected payer data
facilities_columns <- c("year", "id", "facility", "MSSA_desig", "MSSA_name", "county", "address", "city", "zip", "ownership", "EMS_level", "trauma_desig", "payor", "volume", "location")
facilities <- read_csv(facilities_file, col_names = payors_columns, col_types = cols(zip = col_character()), skip = 1)

#Filter records to year with latest data available
facilities <- facilities %>% 
  group_by(facility) %>% 
  mutate(max_year = max(year)) %>% 
  filter(year == max_year) %>% 
  spread(payor, volume) %>%
  mutate(tot_volume = `Medi-Cal` + Medicare + Other + `Private Coverage` + `Self Pay`, 
         pct_comm = `Private Coverage`/tot_volume) %>%
  mutate()

#Read in ACS economic survey data
econ_surv <- read_csv(econ_surv_file, skip = 2, col_names = FALSE)
econ_surv <- econ_surv %>% select(geo = X3,
                                  pct_labor_force = X10,
                                  pct_unemployed = X38,
                                  median_household_income = X248,
                                  mean_household_income = X252,
                                  pct_SNAP = X298,
                                  median_fam_income = X344,
                                  mean_fam_income = X348,
                                  per_cap_income = X352,
                                  median_worker_earnings = X368,
                                  pct_private_ins = X390,
                                  pct_public_ins = X394,
                                  pct_no_ins = X398,
                                  pct_poverty = X514) %>%
  mutate(geo = tolower(geo))
econ_surv <-  mutate(econ_surv, geo = sub(" county, california$", "", x = econ_surv$geo))
econ_surv <-  mutate(econ_surv, geo = sub("^zcta5 ", "", x = econ_surv$geo))

#Join datasets
all_data <- left_join(facilities, left_join(econ_surv, census), by = c("zip" = "geo"))
all_data$per_cap_income <- as.numeric(all_data$per_cap_income)


#Make plots
pct_table <- select(all_data, year:location, starts_with("pct_"))
pct_table_long <- gather(pct_table, "stat", "value", pct_labor_force:pct_single_moth_houses)
ggplot(pct_table_long, aes(x = value, y = pct_comm, col = stat, fill = stat)) + geom_point(alpha = 0.4) + geom_smooth(se = FALSE, size = 2, method = lm) + xlab("value (as % of population)") + ylab("percent commercial patients") + facet_grid(. ~ owner)
ggplot(all_data, aes(x = per_cap_income, y = pct_comm)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)
