#If code is running in author's working directory, set R package location 
if(getwd() == "C:/Users/emily_rinaldi/Desktop/R Projects/SB Capstone"){
  .libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.3")
  }

#Load all required packages
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#Define data sources
census_file <- "2010_Census_Demographics_CA.csv"
econ_surv_file <- "ACS_2014_Economic_Estimates_CA.csv"
facilities_file <- "CA_ED_Encounters_by_Expected_Payer.csv"

#Read in census demographic data into dataframe
census <- read_csv(census_file, skip = 2, 
                   col_names = FALSE, 
                   na = c("", NA, "(X)"))

census <- census %>% select(geo = X3, 
                            tot_pop = X4, 
                            med_age = X42, 
                            pct_over65 = X53, 
                            pct_black = X161,
                            pct_hisp = X231,
                            pct_nonhisp_wh = X249,
                            pct_married_houses = X309,
                            pct_sing_mother_houses = X319) %>%
  mutate(geo = tolower(geo))
  census <-  mutate(census, geo = sub(" county, california$", "", x = census$geo))
  census <-  mutate(census, geo = sub("^zcta5 ", "", x = census$geo))


#Read in expected payer data into dataframe
facilities_columns <- c("year", 
                        "id", 
                        "facility", 
                        "MSSA_desig", 
                        "MSSA_name", 
                        "county", 
                        "address", 
                        "city", 
                        "zip", 
                        "ownership", 
                        "EMS_level", 
                        "trauma_desig", 
                        "payor", 
                        "volume", 
                        "location")

facilities <- read_csv(facilities_file, 
                       col_names = facilities_columns, 
                       col_types = cols(zip = col_character()), 
                       skip = 1)

#Filter records to year with latest data available and transform into "tidy" format
facilities <- facilities %>% 
  group_by(facility) %>% 
  dplyr::mutate(max_year = max(year)) %>% 
  filter(year == max_year) %>% 
  spread(payor, volume) %>%
  mutate(tot_volume = `Medi-Cal` + Medicare + Other + `Private Coverage` + `Self Pay`, 
         pct_comm = `Private Coverage`/tot_volume) %>%
  #Remove four facilities for which not enough data is available to calculate 
  #percent of patients with commercial insurance
  filter(!is.na(pct_comm))

#Read in ACS economic survey data
econ_surv <- read_csv(econ_surv_file, 
                      skip = 2, 
                      col_names = FALSE, 
                      na = c("", NA, "(X)", "-", "N", "2,500-"))
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

#Change class of MSSA_desig and owner variables to factor
all_data$MSSA_desig <- factor(all_data$MSSA_desig)
all_data$owner <- factor(all_data$owner)

#Remove four facilities located in zip codes for which economic survey data is not available
all_data <- filter(all_data, !is.na(pct_labor_force))

#Plot all percent variables vs pct_comm faceted by facility ownership
pct_table <- select(all_data, year:location, starts_with("pct_"))
pct_table_long <- gather(pct_table, "stat", "value", pct_labor_force:pct_sing_mother_houses)
ggplot(pct_table_long, aes(x = value, y = pct_comm, col = stat, fill = stat)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE, size = 2, method = lm) + 
  xlab("value (as % of population)") + 
  ylab("percent commercial patients") + 
  facet_grid(. ~ owner)

#Plot all income or earnings variables vs pct_comm faceted by facility ownership
inc_table <- select(all_data, year:location, pct_comm, contains("income"), contains("earnings"))
inc_table_long <- gather(inc_table, "stat", "value", median_household_income:median_worker_earnings)
ggplot(inc_table_long, aes(x = value, y = pct_comm, col = stat, fill = stat)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE, size = 2, method = lm) + 
  xlab("value") + ylab("percent commercial patients") + 
  facet_grid(. ~ owner)

# MODELING

library(caret)
library(leaps)

#Prepare data for training

model_data <- all_data %>% 
  ungroup() %>% 
  select(pct_comm, 
         MSSA_desig, 
         owner, 
         pct_labor_force:pct_sing_mother_houses)

#Create dummy variables for MSSA_design and owner categories -- why is intercept column created?
model_data <- tbl_df(model.matrix( ~ ., model.frame(~., data = model_data, na.action = na.pass)))

#remove highly correlated variables, excluding dummy variables
modelCor <- cor(model_data[, -(1:6)])
summary(modelCor[upper.tri(modelCor)])

highlyCorVar <- findCorrelation(modelCor, cutoff = 0.75) + 6
model_data <- model_data[, -highlyCorVar]


#Create training and testing data sets
set.seed(50)
inTraining <- createDataPartition(model_data$pct_comm, p = .7, list = FALSE)
training <- model_data[inTraining,]
testing <- model_data[-inTraining,]

#Use bagged trees preprocessing method to impute NAs found in MSSA_desig and owner variables
impute_NAs <- preProcess(training[,-1], method = "bagImpute")

trainingTransformed <- predict(impute_NAs, training)
testingTransformed <- predict(impute_NAs, testing)

# FEATURE SELECTION

# Can regsubsets consider interaction terms?
regfit <- regsubsets(pct_comm ~ ., trainingTransformed[,-1], nvmax = 16)
summary(regfit)
par(mfrow = c(2,2))
plot(regfit, scale = "r2")
plot(regfit, scale = "adjr2")
plot(regfit, scale = "Cp")
plot(regfit, scale = "bic")
par(mfrow = c(1,1))

reg_sum <- summary(regfit)
plot(reg_sum$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg_sum$cp)
points(4, reg_sum$cp[4], pch = 20, col = "red")

coef(regfit, 4)

#Set resampling method to repeated cross validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

#Train linear model
## Selected variables using regsubsets. Tested all interactions but none were
## significant. Also tested these methods: 
## leapForward, leapBackward, lmStepAIC

set.seed(50) #Set seed again?
Fit1 <- train(pct_comm ~ ownerNonprofit + 
                ownerPublic + 
                median_worker_earnings + 
                pct_sing_mother_houses, 
              data = trainingTransformed,
              method = "lm",
              trControl = fitControl)

## Adding pct_labor_force, top_pop, pct_married_houses increases R-squared,
## but the p-values for added variables are all between .06 and .10

set.seed(50) #Set seed again?
Fit2 <- train(pct_comm ~ ownerNonprofit + 
                ownerPublic + 
                median_worker_earnings + 
                pct_sing_mother_houses + 
                pct_labor_force + 
                tot_pop + 
                pct_married_houses, 
              data = trainingTransformed,
              method = "lm",
              trControl = fitControl)




#GBM Model attempt
gbmGrid <-  expand.grid(interaction.depth = seq(1, 7, by = 2),
                        n.trees = seq(100, 1000, by = 50),
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = 5)

set.seed(50) #Set seed again?
Fit_gbm <- train(pct_comm ~ .,
                 data = trainingTransformed,
                 method = "gbm",
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = gbmGrid)
#Test linear model Fit1
testFit1 <- predict(Fit1, testingTransformed)
postResample(testFit1, testingTransformed$pct_comm)

plot(testFit1, testingTransformed$pct_comm)

results <- bind_cols(tbl_df(testPred), tbl_df(testing$pct_comm))
names(results) <- c("predicted", "actual")
ggplot(results, aes(x = predicted, y = actual)) + geom_point() + geom_abline()

#Test linear model Fit2
testFit2 <- predict(Fit2, testingTransformed)
postResample(testFit2, testingTransformed$pct_comm)

plot(testFit2, testingTransformed$pct_comm)

#Test GBM model Fit_gbm
testGBM <- predict(Fit_gbm, testingTransformed)
postResample(testGBM, testingTransformed$pct_comm)

plot(testGBM, testingTransformed$pct_comm)
plot(Fit_gbm$finalModel)


