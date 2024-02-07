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
  modelName       = "MNL_modeChoice_SP_heterogeneity_asc_cost_v1",
  modelDescr      = "MNL model on mode choice SP data with heterogeneity in ASCs and cost (shift for business)",
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
apollo_beta=c(asc_car                 = 0,
              asc_bus_base            = 0,
              asc_bus_shift_female    = 0,
              asc_bus_shift_business  = 0,
              asc_air_base            = 0,
              asc_air_shift_female    = 0,
              asc_air_shift_business  = 0,
              asc_rail_base           = 0,
              asc_rail_shift_female   = 0,
              asc_rail_shift_business = 0,
              b_tt_car                = 0,
              b_tt_bus                = 0,
              b_tt_air                = 0,
              b_tt_rail               = 0,
              b_access                = 0,
              b_cost_base             = 0,
              b_cost_shift_business   = 0,
              b_no_frills             = 0,
              b_wifi                  = 0,
              b_food                  = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_no_frills")

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
  
  ### Create alternative specific constants and coefficients with interactions with socio-demographics
  b_cost   = b_cost_base + b_cost_shift_business * business
  asc_bus  = asc_bus_base + asc_bus_shift_female * female + asc_bus_shift_business * business
  asc_air  = asc_air_base + asc_air_shift_female * female + asc_air_shift_business * business
  asc_rail = asc_rail_base + asc_rail_shift_female * female + asc_rail_shift_business * business
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt_car  * time_car                           + b_cost * cost_car 
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air  + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    V             = V
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
#---- COST SENSITIVITY FOR BUSINESS TRAVELLERS                   ----
# ----------------------------------------------------------------- #

apollo_deltaMethod(model,
                   deltaMethod_settings = list(
                     expression=c(b_cost_business="b_cost_base+b_cost_shift_business")))

# ----------------------------------------------------------------- #
#---- LIKELIHOOD RATIO TEST AGAINST BASE MODEL                   ----
# ----------------------------------------------------------------- #

apollo_lrTest("MNL_modeChoice_SP_heterogeneity_ASC",model)

# ----------------------------------------------------------------- #
#---- CALCULATE WTP FOR DIFFERENT GROUPS AND TIME ATTRIBUTES     ----
# ----------------------------------------------------------------- #

### WTP for 2 groups of people (business and leisure)
wtp=matrix(0,nrow=7,ncol=2)
rownames(wtp)=c("VTT_car","VTT_bus","VTT_air","VTT_rail","VTT_access","WTP_wifi","WTP_food")
colnames(wtp)=c("leisure","business")
wtp[1,1]=60*model$estimate["b_tt_car"]/model$estimate["b_cost_base"]
wtp[2,1]=60*model$estimate["b_tt_bus"]/model$estimate["b_cost_base"]
wtp[3,1]=60*model$estimate["b_tt_air"]/model$estimate["b_cost_base"]
wtp[4,1]=60*model$estimate["b_tt_rail"]/model$estimate["b_cost_base"]
wtp[5,1]=60*model$estimate["b_access"]/model$estimate["b_cost_base"]
wtp[6,1]=-model$estimate["b_wifi"]/model$estimate["b_cost_base"]
wtp[7,1]=-model$estimate["b_food"]/model$estimate["b_cost_base"]
wtp[1,2]=60*model$estimate["b_tt_car"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[2,2]=60*model$estimate["b_tt_bus"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[3,2]=60*model$estimate["b_tt_air"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[4,2]=60*model$estimate["b_tt_rail"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[5,2]=60*model$estimate["b_access"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[6,2]=-model$estimate["b_wifi"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])
wtp[7,2]=-model$estimate["b_food"]/(model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"])

round(wtp,2)

# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()