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
  modelName       = "MNL_modeChoice_SP_base_model",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "ID", 
  outputDirectory = "output"
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
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt      = 0,
              b_access  = 0,
              b_cost    = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt * time_car                           + b_cost * cost_car
  V[["bus"]]  = asc_bus  + b_tt * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[["air"]]  = asc_air  + b_tt * time_air  + b_access * access_air  + b_cost * cost_air   
  V[["rail"]] = asc_rail + b_tt * time_rail + b_access * access_rail + b_cost * cost_rail  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### ADDITIONAL RESULTS ANALYSIS AND DIAGNOSTICS                ####
# ################################################################# #

### Print the outputs of additional diagnostics to file (comment out sink to file command below if not desired)
### Remember to run the line closing any open sinks at the end of this file
apollo_sink()

# ----------------------------------------------------------------- #
#---- STANDARD ERRORS OF PARAMETER TRANSFORMATIONS               ----
# ----------------------------------------------------------------- #

apollo_deltaMethod(model,
                   deltaMethod_settings = list(
                     expression=c(VTT_per_minute="b_tt/b_cost",
                                  VTT_per_hour="60*b_tt/b_cost",
                                  b_tt_vs_access="b_tt-b_access"
                     )))

# ----------------------------------------------------------------- #
#---- GENERATE PREDICTIONS FROM THE MODEL                        ----
# ----------------------------------------------------------------- #

# ----------------------------------------------------------------- #
#---- PREDICTIONS                                                ----
# ----------------------------------------------------------------- #

### This uses the estimated model to make predictions
predictions_base = apollo_prediction(model,apollo_probabilities,apollo_inputs)

### keep only columns for four alternatives (removing ID, choice task and chosen)
predictions_base=predictions_base[,3:6]

### Can look at a summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 1%
database$cost_rail=1.01*database$cost_rail

### Revalidate data
apollo_inputs=apollo_validateInputs()

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model,apollo_probabilities,apollo_inputs)

### keep only columns for four alternatives (removing ID, choice task and chosen)
predictions_new=predictions_new[,3:6]

### Can look at a summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail=1/1.01*database$cost_rail
### Revalidate data
apollo_inputs=apollo_validateInputs()

predictions_overview=rbind(table(database$choice)/nrow(database),
                           colMeans(predictions_base),
                           colMeans(predictions_new))

rownames(predictions_overview)=c("Data",
                                 "Base predictions",
                                 "1% increase in rail costs")
print(round(predictions_overview,4))

### Computing elasticities
### Own elasticity for rail:
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)
### Cross-elasticities for other modes
log(sum(predictions_new[,1])/sum(predictions_base[,1]))/log(1.01)
log(sum(predictions_new[,2])/sum(predictions_base[,2]))/log(1.01)
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)
# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()