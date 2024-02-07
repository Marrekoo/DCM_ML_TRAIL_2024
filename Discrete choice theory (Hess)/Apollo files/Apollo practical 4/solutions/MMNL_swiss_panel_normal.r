# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MMNL_swiss_panel_normal",
  modelDescr      = "MMNL model with Normal distributions for time and cost on Swiss route choice data",
  indivID         = "ID",
  nCores          = 3, 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = apollo_swissRouteChoiceData
### for data dictionary, use ?apollo_swissRouteChoiceData

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc1        = 0, 
                b_tt_mu     = 0,
                b_tt_sig    = 0, 
                b_tc_mu     = 0,
                b_tc_sig    = 0, 
                b_hw        = 0, 
                b_ch        = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_tt","draws_tc")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_tt"]] = b_tt_mu + b_tt_sig * draws_tt 
  randcoeff[["b_tc"]] = b_tc_mu + b_tc_sig * draws_tc 

  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["alt1"]] = asc1 + b_tt * tt1 + b_tc * tc1 + b_hw * hw1 + b_ch * ch1
  V[["alt2"]] =        b_tt * tt2 + b_tc * tc2 + b_hw * hw2 + b_ch * ch2
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(100, 200, 500),
#   nCoresTry = c(1,3,5,7),
#   nRep      = 30
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs)

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
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST MNL MODEL                                  ----
# ----------------------------------------------------------------- #

apollo_lrTest("MNL_swiss", model)

# ----------------------------------------------------------------- #
#---- BEN-AKIVA & SWAIT TEST AGAINST CROSS_SECTIONAL MMNL        ----
# ----------------------------------------------------------------- #

apollo_basTest("MMNL_swiss_CS_normal", model)

# ----------------------------------------------------------------- #
#---- SIMULATING RATIO OF COEFFICIENTS                           ----
# ----------------------------------------------------------------- #

b_tt=rnorm(10^6,model$estimate["b_tt_mu"],abs(model$estimate["b_tt_sig"]))
b_tc=rnorm(10^6,model$estimate["b_tc_mu"],abs(model$estimate["b_tc_sig"]))
ratio=b_tt/b_tc
mean(ratio)
sd(ratio)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()