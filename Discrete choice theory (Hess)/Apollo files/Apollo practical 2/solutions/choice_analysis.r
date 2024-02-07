# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  indivID         = "ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = apollo_modeChoiceData
### for data dictionary, use ?apollo_modeChoiceData

### Use only SP data
database = subset(database,database$SP==1)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

### Define settings for analysis of choice data to be conducted prior to model estimation
choiceAnalysis_settings <- list(
  alternatives = c(car=1, bus=2, air=3, rail=4),
  avail        = list(car=database$av_car, bus=database$av_bus, air=database$av_air, rail=database$av_rail),
  choiceVar    = database$choice,
  explanators  = database[,c("female","business","income")],
  rows         = (database$SP==1)
)

### Run function to analyse choice data
apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### ANALYSIS OF CHOICES ON DUMMY OUTCOMES                       ####
# ################################################################# #

### create temporary copy of database
database_temp=database

### replace costs for unavailable alternatives by NA
database_temp[database_temp$cost_car==0,"cost_car"]=NA
database_temp[database_temp$cost_bus==0,"cost_bus"]=NA
database_temp[database_temp$cost_air==0,"cost_air"]=NA
database_temp[database_temp$cost_rail==0,"cost_rail"]=NA

### find minimum available cost for each row
database_temp$minimum_cost=apply(database_temp[,c("cost_car",
                                                "cost_bus",
                                                "cost_air","cost_rail")],
                                 1,
                                 function(x) min(x,na.rm=TRUE))
database_temp$maximum_cost=apply(database_temp[,c("cost_car",
                                                  "cost_bus",
                                                  "cost_air","cost_rail")],
                                 1,
                                 function(x) max(x,na.rm=TRUE))

database_temp$chosen_cost=with(database,
                               (choice==1)*cost_car
                               +(choice==2)*cost_bus
                               +(choice==3)*cost_air
                               +(choice==4)*cost_rail)

### indicators for choosing cheapest and most expensive
database_temp$choose_cheapest=with(database_temp,(chosen_cost==minimum_cost))
database_temp$choose_expensive=with(database_temp,(chosen_cost==maximum_cost))
                                               
### Define settings for analysis of choice data to be conducted prior to model estimation
choiceAnalysis_settings <- list(
  alternatives = c(cheapest=1, expensive=2, other=3),
  choiceVar    = (1*database_temp$choose_cheapest
                  +2*database_temp$choose_expensive
                  +3*(database_temp$choose_cheapest==FALSE)*(database_temp$choose_expensive==FALSE)),
  explanators  = database[,c("female","business","income")],
  rows         = (database$SP==1)
)

### Run function to analyse choice data
apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)


