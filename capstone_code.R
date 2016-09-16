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
                            pct_over18 = X47, 
                            pct_over65 = X53, 
                            pct_black = X161,
                            pct_asian = X165,
                            pct_hisp = X231,
                            pct_nonhisp_wh = X249,
                            pct_house_wchildren = X273,
                            pct_extfamily_houses = X275,
                            pct_nonrelative_houses =  X281,
                            pct_group_qrts = X289,
                            pct_married_houses = X309,
                            pct_sing_mother_houses = X319,
                            avg_household_size = X336,
                            pct_vacant_houses =  X345) %>%
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
                                  pct_armed_forces = X26,
                                  pct_unemployed = X38,
                                  pct_female_labforce = X46,
                                  pct_pub_trans = X86,
                                  pct_service_ind = X114,
                                  pct_sales_office = X118,
                                  pct_construction = X122,
                                  pct_transport_ind = X126,
                                  pct_under10K = X210,
                                  pct_10to15K = X214,
                                  pct_15to25K = X218,
                                  pct_25to35K = X222,
                                  pct_35to50K = X226,
                                  pct_50to75K = X230,
                                  pct_75to100K = X234,
                                  med_househ_income = X248,
                                  mn_househ_income = X252,
                                  pct_wSSI = X282,
                                  pct_wcash_assist = X290,
                                  pct_SNAP = X298,
                                  per_cap_income = X352,
                                  med_worker_earnings = X368,
                                  med_male_earnings = X372,
                                  pct_private_ins = X390,
                                  pct_public_ins = X394,
                                  pct_no_ins = X398,
                                  pct_poverty = X514) %>%
  mutate(geo = tolower(geo))

econ_surv <-  mutate(econ_surv, geo = sub(" county, california$", "", x = econ_surv$geo))
econ_surv <-  mutate(econ_surv, geo = sub("^zcta5 ", "", x = econ_surv$geo))
econ_surv$med_worker_earnings <- as.numeric(econ_surv$med_worker_earnings)

#Join datasets
all_data <- left_join(facilities, left_join(econ_surv, census), by = c("zip" = "geo"))
all_data <- separate(all_data, ownership, c("owner", "owner_type"), sep = " - ")

#Change class of MSSA_desig and owner variables to factor
all_data$MSSA_desig <- factor(all_data$MSSA_desig)
all_data$owner <- factor(all_data$owner)

#Remove four facilities located in zip codes for which economic survey data is not available
all_data <- filter(all_data, !is.na(pct_labor_force))

# MODELING

library(caret)
library(leaps)

#Prepare data for modeling
#eliminate any unneeded columns that will not be used as predictors
model_data <- all_data %>% 
  ungroup() %>% 
  select(pct_comm, 
         MSSA_desig, 
         owner, 
         pct_labor_force:pct_vacant_houses)

##FEATURE SELECTION
#narrow down variables using regsubsets from leaps package
regfit <- regsubsets(pct_comm ~ ., model_data, nvmax = 16)
reg_sum <- summary(regfit)

#Use Mallow's Cp to choose the best regsubsets result, which is a model evaluation metric that is 
#not as biased to adding more predictors as Rsquared
plot(reg_sum$cp, xlab = "Number of Variables", ylab = "Cp")
best_num <- which.min(reg_sum$cp)
points(best_num, reg_sum$cp[best_num], pch = best_num, col = "red")
coef(regfit, best_num)
best_vars <- names(coef(regfit, best_num))

#Filter model_data to include only those variables that appeared in the optimum regsubsets model
model_filtered <- tbl_df(cbind(pct_comm = model_data$pct_comm, 
                               owner = model_data$owner, 
                               model_data[best_vars[4:14]]))


#Create dummy variables for MSSA_design and owner categories
model_filtered2 <- tbl_df(model.matrix( ~ ., model.frame(~., data = model_filtered, na.action = na.pass)))

#remove highly correlated variables, excluding dummy variables
modelCor <- cor(model_filtered2[, -(1:4)])
summary(modelCor[upper.tri(modelCor)])

highlyCorVar <- findCorrelation(modelCor, cutoff = 0.75) + 4
model_filtered3 <- model_filtered2[, -highlyCorVar]

#Create training and testing data sets
set.seed(50)
inTraining <- createDataPartition(model_filtered3$pct_comm, p = .7, list = FALSE)
training <- model_filtered3[inTraining,]
testing <- model_filtered3[-inTraining,]

#Use bagged trees preprocessing method to impute NAs found in owner variable
impute_NAs <- preProcess(training[,-1], method = "bagImpute")

set.seed(50)
trainingTransformed <- predict(impute_NAs, training)
testingTransformed <- predict(impute_NAs, testing)

#Set resampling method to repeated cross validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

#Train linear model
## Tested all interactions but none were significant

set.seed(50)
Fit1 <- train(pct_comm ~ ., 
              data = trainingTransformed,
              method = "lm",
              trControl = fitControl)
summary(Fit1)
Fit1$results

#Test linear model Fit1
testFit1 <- predict(Fit1, testingTransformed)
postResample(testFit1, testingTransformed$pct_comm)

#Remove insignificant variables and train a second model
set.seed(50)
Fit2 <- train(pct_comm ~ ownerPublic + pct_service_ind + pct_75to100K + med_male_earnings + pct_asian, 
              data = trainingTransformed,
              method = "lm",
              trControl = fitControl)
summary(Fit2)
Fit2$results

testFit2 <- predict(Fit2, testingTransformed)
postResample(testFit2, testingTransformed$pct_comm)


#GBM Model attempt to automate interactions
gbmGrid <-  expand.grid(interaction.depth = seq(1, 7, by = 2),
                        n.trees = seq(100, 1000, by = 50),
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = 5)

set.seed(50)
FitGBM1 <- train(pct_comm ~ .,
                 data = trainingTransformed,
                 method = "gbm",
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

#Test FitGBM1 model
testGBM1 <- predict(FitGBM1, testingTransformed)
postResample(testGBM1, testingTransformed$pct_comm)

#Try GBM Model again with the most important variables from FitGBM1
varImp(FitGBM1)

set.seed(50)
FitGBM2 <- train(pct_comm ~ med_male_earnings + pct_public_ins + pct_black + pct_service_ind + pct_hisp,
                 data = trainingTransformed,
                 method = "gbm",
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

#Test FitGBM2
testGBM2 <- predict(FitGBM2, testingTransformed)
postResample(testGBM2, testingTransformed$pct_comm)
