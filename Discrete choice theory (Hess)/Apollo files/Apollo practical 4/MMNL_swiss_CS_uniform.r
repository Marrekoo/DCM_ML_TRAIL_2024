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
  modelName       = "MMNL_swiss_CS_uniform",
  modelDescr      = "MMNL model with Uniform distributions for time and cost on Swiss route choice data, cross-sectional spec",
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
                b_tt_a      = 0,
                b_tt_b      = 0, 
                b_tc_a      = 0,
                b_tc_b      = 0, 
                b_hw        = 0, 
                b_ch        = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  intraDrawsType = "halton",
  intraNDraws    = 100,
  intraUnifDraws = c("draws_tt","draws_tc")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_tt"]] = b_tt_a + b_tt_b * draws_tt 
  randcoeff[["b_tc"]] = b_tc_a + b_tc_b * draws_tc 

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
  
  ### Average across intra-individual draws
  P = apollo_avgIntraDraws(P, apollo_inputs, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

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
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()