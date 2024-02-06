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
  modelName       = "MNL_modeChoice_SP_service_effects",
  modelDescr      = "MNL model on mode choice SP data with categorical service quality variable using effects coding",
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
apollo_beta=c(asc_car     = 0,
              asc_bus     = 0,
              asc_air     = 0,
              asc_rail    = 0,
              b_tt        = 0,
              b_access    = 0,
              b_cost      = 0,
              b_wifi      = 0,
              b_food      = 0)

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
  
  ### compute value for effects coded coefficient
  b_no_frills=-(b_wifi+b_food)

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt * time_car                           + b_cost * cost_car 
  V[["bus"]]  = asc_bus  + b_tt * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[["air"]]  = asc_air  + b_tt * time_air  + b_access * access_air  + b_cost * cost_air  + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[["rail"]] = asc_rail + b_tt * time_rail + b_access * access_rail + b_cost * cost_rail + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
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

