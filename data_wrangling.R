#Set R package location and load packages
.libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.3")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)


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
                            pct_black = X161,
                            pct_hisp = X231,
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
econ_surv <- read_csv(econ_surv_file, skip = 2, col_names = FALSE, na = c("", NA, "(X)", "-", "N"))
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
econ_surv$median_worker_earnings <- as.numeric(econ_surv$median_worker_earnings)

#Join datasets
all_data <- left_join(facilities, left_join(econ_surv, census), by = c("zip" = "geo"))
all_data <- separate(all_data, ownership, c("owner", "owner_type"), sep = " - ")
all_data$MSSA_desig <- factor(all_data$MSSA_desig)
all_data$owner <- factor(all_data$owner)


#Make plots
pct_table <- select(all_data, year:location, starts_with("pct_"))
pct_table_long <- gather(pct_table, "stat", "value", pct_labor_force:pct_single_moth_houses)
ggplot(pct_table_long, aes(x = value, y = pct_comm, col = stat, fill = stat)) + geom_point(alpha = 0.4) + geom_smooth(se = FALSE, size = 2, method = lm) + xlab("value (as % of population)") + ylab("percent commercial patients") + facet_grid(. ~ owner)
ggplot(all_data, aes(x = per_cap_income, y = pct_comm)) + geom_point(alpha = 0.3) + geom_smooth(method = lm)


### caret modeling attempts

#prepare data for training

model_data <- all_data %>% ungroup() %>% select(-(year:tot_volume))
model_data <- na.omit(model_data)

#80% of data for training too high?
inTraining <- createDataPartition(model_data$pct_comm, p = .8, list = FALSE)
training <- model_data[inTraining,]
testing <- model_data[-inTraining,]

#preprocess? do this in the train function or before?
preProcValues <- preProcess(training, method = c("center", "scale", "pca"))

trainingTransformed <- predict(preProcValues, training)
testingTransformed <- predict(preProcValues, testing)

#how to choose number and repeats for repeated cross validation?
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

## set.seed causes you to get same results each time? necessary?
#Use PCA to transform data to a smaller sub-space where the new variables are
#uncorrelated with one another. Forces scaling of the predictors. Changes
#column names to PC1, PC2, etc. How do I interpret these variables?
#What does increasing or decreasing thresh do? 
#

#Use data = training or or model_data? Define preProcess again? 
#add a metric to minimize or maximize?
Fit1 <- train(pct_comm ~ ., data = training,
              method = "lm",
              trControl = fitControl,
              preProcess = c("center", "scale", "pca"))



##from https://rpubs.com/njvijay/27823 - a function to display all PCA related plots in 2X2 grid
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(prcomp(trainingTransformed, center = FALSE))

#test model
testPred <- predict(Fit1, testing)
postResample(testPred, testing$pct_comm)

