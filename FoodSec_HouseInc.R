### Notice that HouseInc is dominating the model
## More income = better chances of Food Secure
# Split the data based on HouseInc and rerun

library(readr)
library(tidyverse)
library(forcats)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
set.seed(42)

### Load the data
raw.foodsec1 <- read.csv('/Users/jacobmorgan/Desktop/Jobs/2021/Analyses/FoodSecurity/cchs-82M0013-E-2015-2016-Annual-component_F1.csv')
raw.foodsec2 <- read.csv('/Users/jacobmorgan/Desktop/Jobs/2021/Analyses/FoodSecurity/cchs-82M0013-E-2017-2018-Annual-component_F1.csv')

### Pre-processing
## Rename Columns
# Select columns of interest
# Ensure both datasets have the same column names

raw.foodsec1 <- raw.foodsec1 %>%
  rename(
    Province = GEO_PRV,
    Sex = DHH_SEX,
    Age = DHHGAGE,
    LiveArrange = DHHDGLVG,
    Education = EHG2DVH3,
    PerceivedHealth = GEN_005,
    PerceivedMental = GENDVMHI,
    SmokesDaily = SMK_015,
    DrinkingRisk = ALWDVLTR,
    HealthCare = PHC_020,
    OwnHouse = DHH_OWN,
    BirthCountry = SDCDGCB,
    White = SDCDGCGT,
    FoodSec = FSCDVHFS,
    HouseInc = INCDGHH,
    Weed = DRGDVYCM,
    Drugs = DRGDVYAC,
  )

raw.foodsec2 <- raw.foodsec2 %>%
  rename(
    Province = GEO_PRV,
    Sex = DHH_SEX,
    Age = DHHGAGE,
    LiveArrange = DHHDGLVG,
    Education = EHG2DVH3,
    PerceivedHealth = GEN_005,
    PerceivedMental = GENDVMHI,
    SmokesDaily = SMK_015,
    DrinkingRisk = ALWDVLTR,
    HealthCare = PHC_020,
    OwnHouse = DHH_OWN,
    BirthCountry = SDCDGCB,
    White = SDCDGCGT,
    FoodSec = FSCDVHF2,
    HouseInc = INCDGHH,
    Weed = DRGDVYCM,
    Drugs = DRGDVYAC,
  )

## Drop and keep columns
raw.foodsec1 <-select(raw.foodsec1, c(FoodSec, Province, Sex, Age, LiveArrange, Education, PerceivedHealth, 
                                      PerceivedMental, SmokesDaily, Weed, Drugs, DrinkingRisk, HealthCare, 
                                      White, BirthCountry, OwnHouse, HouseInc))         

raw.foodsec2 <-select(raw.foodsec2, c(FoodSec, Province, Sex, Age, LiveArrange, Education, PerceivedHealth, 
                                      PerceivedMental, SmokesDaily, Weed, Drugs, DrinkingRisk, HealthCare, 
                                      White, BirthCountry, OwnHouse, HouseInc)) 

## Combine dataframes by row
foodsec <- rbind(raw.foodsec1, raw.foodsec2)

## Create factors of categorical variables
# Household food security status
foodsec$FoodSec <- foodsec$FoodSec %>%
  as.factor() %>% 
  fct_collapse(FoodSecure = '0', FoodInsecure = c('1', '2', '3'))
# Province
foodsec$Province <- foodsec$Province %>%
  as.factor() %>%
  fct_collapse(ND = '10', PEI = '11', NS = '12', NB = '13', QB = '24', 
               ON = '35', MT = '46', SA = '47', AB = '48', BC = '59', YK = '60',
               NW = '61', NV = '62')
# Sex of respondent
foodsec$Sex <- foodsec$Sex %>%
  as.factor() %>%
  fct_collapse(Male = '1', Female = '2')
# Age group
foodsec$Age <- foodsec$Age %>%
  as.factor() %>%
  fct_collapse('Under 20' = c('1','2','3'), '20 - 40' = c('4','5','6','7'), '40 - 65' = c('8','9','10','11','12'), '65+' = c('13','14','15','16'))
# Living Arrangement
foodsec$LiveArrange <- foodsec$LiveArrange %>%
  as.factor() %>%
  fct_collapse(Single = '1', SingleRoomies = '2', Partnered = '3', ThreeGenFam = '4', SingleParent = c('5','6'), MultiParent = '7')
# Education
foodsec$Education <- foodsec$Education %>%
  as.factor() %>%
  fct_collapse(Junior = '1', Secondary = '2', PostSecondary = '3')
# Perceived Health
foodsec$PerceivedHealth <- foodsec$PerceivedHealth %>%
  as.factor() %>%
  fct_collapse(Poor = '5', Fair = '4', Good = c('1','2','3'))
# Perceived Mental Health
foodsec$PerceivedMental <- foodsec$PerceivedMental %>%
  as.factor() %>%
  fct_collapse(Poor = '0', Fair = '1', Good = c('2', '3', '4'))
# Smokes Daily
foodsec$SmokesDaily <- foodsec$SmokesDaily %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Drinking Risk
foodsec$DrinkingRisk <- foodsec$DrinkingRisk %>%
  as.factor() %>%
  fct_collapse(Increased = '1', None = '2')
# Weed in the last year
foodsec$Weed <- foodsec$Weed %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Used drugs in the last year
foodsec$Drugs <- foodsec$Drugs %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Healthcare Provider
foodsec$HealthCare <- foodsec$HealthCare %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# White
foodsec$White <- foodsec$White %>%
  as.factor() %>%
  fct_collapse(White = '1', BIPOC = '2')
# Owns House
foodsec$OwnHouse <- foodsec$OwnHouse %>%
  as.factor() %>%
  fct_collapse(Owns = '1', Rents = '2')
# Birth Country
foodsec$BirthCountry <- foodsec$BirthCountry %>%
  as.factor() %>%
  fct_collapse(Canada = '1', Other = '2')
# Total Household Income bracket
foodsec$HouseInc <- foodsec$HouseInc %>%
  as.factor() %>%
  fct_collapse('<60k' = c('1','2','3'), '>60k' = c('4','5'))

## Filter out by missing / unused observations

foodsec <- foodsec %>%
  filter(
    (foodsec$FoodSec == 'FoodSecure' | foodsec$FoodSec == 'FoodInsecure') &
      (foodsec$Education == 'Junior' | foodsec$Education == 'Secondary' | foodsec$Education == 'PostSecondary') &
      (foodsec$LiveArrange == 'Single' | foodsec$LiveArrange == 'SingleRoomies' | foodsec$LiveArrange == 'Partnered' | foodsec$LiveArrange == 'ThreeGenFam' | foodsec$LiveArrange == 'SingleParent' | foodsec$LiveArrange == 'MultiParent') & 
      (foodsec$PerceivedHealth == 'Poor' | foodsec$PerceivedHealth == 'Fair' | foodsec$PerceivedHealth == 'Good') &
      (foodsec$PerceivedMental == 'Poor' | foodsec$PerceivedMental == 'Fair' | foodsec$PerceivedMental == 'Good') &
      (foodsec$SmokesDaily == 'Yes' | foodsec$SmokesDaily == 'No') &
      (foodsec$Weed == 'Yes' | foodsec$Weed == 'No') &
      (foodsec$Drugs == 'Yes' | foodsec$Drugs == 'No') &
      (foodsec$DrinkingRisk == 'Increased' | foodsec$DrinkingRisk == 'None' ) &
      (foodsec$HealthCare == 'Yes' | foodsec$HealthCare == 'No') &
      (foodsec$White == 'White' | foodsec$White == 'BIPOC') &
      (foodsec$BirthCountry == 'Canada' | foodsec$BirthCountry == 'Other') &
      (foodsec$OwnHouse == 'Owns' | foodsec$OwnHouse == 'Rents') &
      (foodsec$HouseInc == '<60k' | foodsec$HouseInc == '>60k')
  ) %>%
  droplevels() # Remove unused factor levels

## Limit dataset to residents of Ontario, and then drop the variable

foodsec <- filter(foodsec, Province == 'ON') %>%
  droplevels()
foodsec <- select(foodsec, -Province)

## Separate dataset by HouseInc
# Split at 60k

foodsec.u60k <- filter(foodsec, HouseInc == '<60k') 
foodsec.o60k <- filter(foodsec, HouseInc == '>60k')

summary(foodsec.u60k)
summary(foodsec.o60k)

### The data is now grouped by HouseInc split at 60k
## From here, each dataset will be analyzed separately

### Over 60k

## Balance the categories
# Randomly select 300 of the FoodSecure to compare against all (762) FoodInsecure

set.seed(42)
o60.foodsec_FS <- foodsec.o60k %>%
  filter(FoodSec == 'FoodSecure') %>%
  sample_n(., 300) # n == 300

o60.foodsec_FIs <- foodsec.o60k %>%
  filter(FoodSec == 'FoodInsecure') # n == 217

o60.foodsec.pre <- rbind(o60.foodsec_FS, o60.foodsec_FIs)

summary(o60.foodsec.pre)

## Split the data into training and testing sets
o60.foodsec_split <- rsample::initial_split(o60.foodsec.pre, prop = 0.7)
o60.foodsec_train <- rsample::training(o60.foodsec_split)
o60.foodsec_test <- rsample::testing(o60.foodsec_split)

## Build the classification tree

o60.foodsec_cart <- rpart(
  formula = FoodSec ~ ., 
  data = o60.foodsec_train, 
  method = 'class',
  control = list(cp = 0.001)
)

printcp(o60.foodsec_cart)
plotcp(o60.foodsec_cart, upper = 'splits')
rpart.plot(o60.foodsec_cart, yesno= T)

## Tune the model
# Create a tuning grid

tuninggrid <- expand.grid(
  minsplit = seq(4,20,1),
  maxdepth = seq(5,15,1)
)

# Create models according to the parameter values specified in the tuning grid
tuningmodels <- list()

for (i in 1:nrow(tuninggrid)) {
  # get the minsplit and maxdepth values of each row
  minsplit <- tuninggrid$minsplit[i]
  maxdepth <- tuninggrid$maxdepth[i]
  
  # train models based on minsplit and maxdepth, storing the values in the models list created globally
  tuningmodels[[i]] <- rpart(
    formula = FoodSec ~ .,
    data = o60.foodsec_train,
    method = 'class',
    control = list(minsplit = minsplit, maxdepth = maxdepth, cp = 0.001)
  )
}

## Retrieve tuning values

# Optimal CP
get_cp <- function(x) {
  min <- which.min(x$cptable[, 'xerror'])
  cp <- x$cptable[min, 'CP']
}
# Minimum error
min_err <- function(x) {
  min <- which.min(x$cptable[ , 'xerror'])
  xerror <- x$cptable[min, 'xerror']
}

# Organize top 5 model parameters

tuninggrid %>%
  mutate(
    cp = purrr::map_dbl(tuningmodels, get_cp),
    error = purrr::map_dbl(tuningmodels, min_err)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

## Note:
# minsplit = 16
# maxdepth = 14
# cp = 0.001
# error = 0.875

o60.foodsec_cart.prn <- prune(
 o60.foodsec_cart,
  cp = o60.foodsec_cart$cptable[o60.foodsec_cart$cptable[ ,2] == 16, 'CP']
)

# rpart.plot(foodsec_cart.prn$finalmodel, yesno = T)
print(o60.foodsec_cart.prn)

## Evaluate the model
## Measure training predictions against testing data set

# Variable Importance Plot
o60.foodsec_cart.prn$variable.importance %>%
  data.frame() %>%
  rownames_to_column(var = 'Feature') %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = 'cadetblue', size = 0.3) +
  theme_minimal() +
  coord_flip() +
  labs(x = '', y = '', title = 'Variable Importance from Single Classification Tree')

# Note: House Income is a very heavy variable, will be 'greedily' selected by the model
# A random forest model will explore the value of other variables

# Confusion Matrix
o60.foodsec.preds <- bind_cols(
  predict(o60.foodsec_cart.prn, newdata = o60.foodsec_test, type = 'prob'),
  Predicted = predict(o60.foodsec_cart.prn, newdata = o60.foodsec_test, type = 'vector'),
  Actual = o60.foodsec_test$FoodSec
)

o60.foodsec.preds$Predicted <- o60.foodsec.preds$Predicted %>%
  as.factor() %>% 
  fct_collapse(FoodSecure = '1', FoodInsecure = '2')

o60.foodsec.cm <- confusionMatrix(o60.foodsec.preds$Predicted, o60.foodsec.preds$Actual)
o60.foodsec.cm

### Random Forest Model

## Define the training control parameters
# 10-fold cross validation using grid search

trctrl <- trainControl(
  method = 'cv',
  number = 10,
  search = 'grid'
)

## Train a basic model on the data using the default parameters
set.seed(42)
rf.o60 <- train(
  FoodSec ~ .,
  data = o60.foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  trControl = trctrl
)

print(rf.o60)

# mtry = 2
# Accuracy = .59

### Revise the basic model
## Find optimal mtry

set.seed(42)
tunegrid <- expand.grid(.mtry = c(1:10))

rf.o60.mtry <- train(
  FoodSec ~ .,
  data = o60.foodsec_train,
  method = 'rf',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  nodesize = 14,
  ntree = 400
)
print(rf.o60.mtry)

best_mtry <- rf.o60.mtry$bestTune$mtry
best_mtry

# Optimal mtry is 7

## Find the optimal number of nodes

store_maxnode <- list()

tunegrid <- expand.grid(.mtry = best_mtry)

for (i in c(3:30)) {
  set.seed(42)
  rf.o60.maxnode <- train(
    FoodSec ~ .,
    data = o60.foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    tuneGrid = tunegrid,
    trControl = trctrl,
    importance = T,
    nodesize = 14,
    maxnodes = i,
    ntree = 300)
  current_iteration <- toString(i)
  store_maxnode[[current_iteration]] <- rf.o60.maxnode
}

results.maxnodes <- resamples(store_maxnode)
summary(results.maxnodes)

# Optimal number of max nodes is 23

## Find the optimal number of trees

store_ntrees <-list()

for (i in c(250,300,450,500,550,600,700,800,1000,2000)) {
  set.seed(42)
  rf.o60.ntrees <- train(
    FoodSec ~ .,
    data = o60.foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    trControl = trctrl,
    tuneGrid = tunegrid,
    importance = T,
    nodesize = 14,
    maxnodes = 23,
    ntree = i)
  key <- toString(i)
  store_ntrees[[key]] <- rf.o60.ntrees
}

rf.o60.ntrees <- resamples(store_ntrees)
summary(rf.o60.ntrees)

## Optimal number of trees is 300

### Build the final model based on the tuning parameters

rf.o60.foodsec.tuned <- train(
  FoodSec ~ .,
  data = o60.foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  nodesize = 14,
  ntree = 300,
  maxnodes = 23
)

### Evaluate the model

o60.predictions <- predict(
  rf.o60.foodsec.tuned,
  o60.foodsec_test
)

o60.foodsec.cm <- confusionMatrix(
  o60.predictions,
  o60.foodsec_test$FoodSec
)
print(o60.foodsec.cm)

plot(varImp(rf.o60.foodsec.tuned))

varImp(rf.o60.foodsec.tuned)

### Under 60k

## Balance the categories
# Randomly select 900 of the FoodSecure (1464) to compare against all (856) FoodInsecure

set.seed(42)
u60.foodsec_FS <- foodsec.u60k %>%
  filter(FoodSec == 'FoodSecure') %>%
  sample_n(., 900) # n == 900

u60.foodsec_FIs <- foodsec.u60k %>%
  filter(FoodSec == 'FoodInsecure') # n == 856

u60.foodsec.pre <- rbind(u60.foodsec_FS, u60.foodsec_FIs)

summary(u60.foodsec.pre)

## Split the data into training and testing sets
u60.foodsec_split <- rsample::initial_split(u60.foodsec.pre, prop = 0.7)
u60.foodsec_train <- rsample::training(u60.foodsec_split)
u60.foodsec_test <- rsample::testing(u60.foodsec_split)

## Build the classification tree

u60.foodsec_cart <- rpart(
  formula = FoodSec ~ ., 
  data = u60.foodsec_train, 
  method = 'class',
  control = list(cp = 0.001)
)

printcp(u60.foodsec_cart)
plotcp(u60.foodsec_cart, upper = 'splits')
rpart.plot(u60.foodsec_cart, yesno= T)

## Tune the model
# Create a tuning grid

tuninggrid <- expand.grid(
  minsplit = seq(4,35,1),
  maxdepth = seq(3,15,1)
)

# Create models according to the parameter values specified in the tuning grid
tuningmodels <- list()

for (i in 1:nrow(tuninggrid)) {
  # get the minsplit and maxdepth values of each row
  minsplit <- tuninggrid$minsplit[i]
  maxdepth <- tuninggrid$maxdepth[i]
  
  # train models based on minsplit and maxdepth, storing the values in the models list created globally
  tuningmodels[[i]] <- rpart(
    formula = FoodSec ~ .,
    data = u60.foodsec_train,
    method = 'class',
    control = list(minsplit = minsplit, maxdepth = maxdepth, cp = 0.001)
  )
}

## Retrieve tuning values

# Optimal CP
get_cp <- function(x) {
  min <- which.min(x$cptable[, 'xerror'])
  cp <- x$cptable[min, 'CP']
}
# Minimum error
min_err <- function(x) {
  min <- which.min(x$cptable[ , 'xerror'])
  xerror <- x$cptable[min, 'xerror']
}

# Organize top 5 model parameters

tuninggrid %>%
  mutate(
    cp = purrr::map_dbl(tuningmodels, get_cp),
    error = purrr::map_dbl(tuningmodels, min_err)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

## Note:
# minsplit = 24
# maxdepth = 4
# cp = 0.003466205
# error = 0.6759099

u60.foodsec_cart.prn <- prune(
  u60.foodsec_cart,
  cp = u60.foodsec_cart$cptable[u60.foodsec_cart$cptable[ ,2] == 24, 'CP']
)

# rpart.plot(foodsec_cart.prn$finalmodel, yesno = T)
print(u60.foodsec_cart.prn)

## Evaluate the model
## Measure training predictions against testing data set

# Variable Importance Plot
u60.foodsec_cart.prn$variable.importance %>%
  data.frame() %>%
  rownames_to_column(var = 'Feature') %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = 'cadetblue', size = 0.3) +
  theme_minimal() +
  coord_flip() +
  labs(x = '', y = '', title = 'Variable Importance from Single Classification Tree')

# Note: House Income is a very heavy variable, will be 'greedily' selected by the model
# A random forest model will explore the value of other variables

# Confusion Matrix
u60.foodsec.preds <- bind_cols(
  predict(u60.foodsec_cart.prn, newdata = u60.foodsec_test, type = 'prob'),
  Predicted = predict(u60.foodsec_cart.prn, newdata = u60.foodsec_test, type = 'vector'),
  Actual = u60.foodsec_test$FoodSec
)

u60.foodsec.preds$Predicted <- u60.foodsec.preds$Predicted %>%
  as.factor() %>% 
  fct_collapse(FoodSecure = '1', FoodInsecure = '2')

u60.foodsec.cm <- confusionMatrix(u60.foodsec.preds$Predicted, u60.foodsec.preds$Actual)
u60.foodsec.cm

### Random Forest Model

## Define the training control parameters
# 10-fold cross validation using grid search

trctrl <- trainControl(
  method = 'cv',
  number = 10,
  search = 'grid'
)

## Train a basic model on the data using the default parameters
set.seed(42)
rf.u60 <- train(
  FoodSec ~ .,
  data = u60.foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  trControl = trctrl
)

print(rf.u60)

# mtry = 2
# Accuracy = .59

### Revise the basic model
## Find optimal mtry

set.seed(42)
tunegrid <- expand.grid(.mtry = c(1:10))

rf.u60.mtry <- train(
  FoodSec ~ .,
  data = u60.foodsec_train,
  method = 'rf',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  nodesize = 14,
  ntree = 400
)
print(rf.u60.mtry)

best_mtry <- rf.u60.mtry$bestTune$mtry
best_mtry

# Optimal mtry is 7

## Find the optimal number of nodes

store_maxnode <- list()

tunegrid <- expand.grid(.mtry = best_mtry)

for (i in c(3:30)) {
  set.seed(42)
  rf.u60.maxnode <- train(
    FoodSec ~ .,
    data = u60.foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    tuneGrid = tunegrid,
    trControl = trctrl,
    importance = T,
    nodesize = 14,
    maxnodes = i,
    ntree = 300)
  current_iteration <- toString(i)
  store_maxnode[[current_iteration]] <- rf.u60.maxnode
}

results.maxnodes <- resamples(store_maxnode)
summary(results.maxnodes)

# Optimal number of max nodes is 23

## Find the optimal number of trees

store_ntrees <-list()

for (i in c(250,300,450,500,550,600,700,800,1000,2000)) {
  set.seed(42)
  rf.u60.ntrees <- train(
    FoodSec ~ .,
    data = u60.foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    trControl = trctrl,
    tuneGrid = tunegrid,
    importance = T,
    nodesize = 14,
    maxnodes = 23,
    ntree = i)
  key <- toString(i)
  store_ntrees[[key]] <- rf.u60.ntrees
}

rf.u60.ntrees <- resamples(store_ntrees)
summary(rf.u60.ntrees)

## Optimal number of trees is 300

### Build the final model based on the tuning parameters

rf.u60.foodsec.tuned <- train(
  FoodSec ~ .,
  data = u60.foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  nodesize = 14,
  ntree = 300,
  maxnodes = 23
)

### Evaluate the model

u60.predictions <- predict(
  rf.u60.foodsec.tuned,
  u60.foodsec_test
)

u60.foodsec.cm <- confusionMatrix(
  u60.predictions,
  u60.foodsec_test$FoodSec
)
print(u60.foodsec.cm)

plot(varImp(rf.u60.foodsec.tuned))

varImp(rf.u60.foodsec.tuned)
























