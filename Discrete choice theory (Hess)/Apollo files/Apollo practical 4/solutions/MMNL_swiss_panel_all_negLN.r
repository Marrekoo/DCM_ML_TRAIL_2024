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
  modelName       = "MMNL_swiss_panel_all_negLN",
  modelDescr      = "MMNL model with negative Lognormal distributions for all four coefficients on Swiss route choice data",
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
                b_log_tt_mu     = -3,
                b_log_tt_sig    = 0, 
                b_log_tc_mu     = -3,
                b_log_tc_sig    = 0, 
                b_log_hw_mu     = -3, 
                b_log_hw_sig    = 0, 
                b_log_ch_mu     = -3,	
                b_log_ch_sig    = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_tt","draws_tc","draws_hw","draws_ch")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_tt"]] = -exp(b_log_tt_mu + b_log_tt_sig * draws_tt )
  randcoeff[["b_tc"]] = -exp(b_log_tc_mu + b_log_tc_sig * draws_tc )
  randcoeff[["b_hw"]] = -exp(b_log_hw_mu + b_log_hw_sig * draws_hw )
  randcoeff[["b_ch"]] = -exp(b_log_ch_mu + b_log_ch_sig * draws_ch )
  
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
#---- B&S TEST AGAINST MODEL WITH UNIFORMS                       ----
# ----------------------------------------------------------------- #

apollo_basTest("MMNL_swiss_panel_all_uniform", model)

# ----------------------------------------------------------------- #
#---- COMPARING ANALYTICAL AND SIMULATED DISTRIBUTIONS           ----
# ----------------------------------------------------------------- #

### analytical 
mean_tt=-exp(model$estimate["b_log_tt_mu"]+model$estimate["b_log_tt_sig"]^2/2)
sd_tt=abs(mean_tt*sqrt(exp(model$estimate["b_log_tt_sig"]^2)-1))
mean_tc=-exp(model$estimate["b_log_tc_mu"]+model$estimate["b_log_tc_sig"]^2/2)
sd_tc=abs(mean_tc*sqrt(exp(model$estimate["b_log_tc_sig"]^2)-1))
log_vtt_mu=model$estimate["b_log_tt_mu"]-model$estimate["b_log_tc_mu"]
log_vtt_sig=sqrt(model$estimate["b_log_tt_sig"]^2+model$estimate["b_log_tc_sig"]^2)
mean_vtt=exp(log_vtt_mu+log_vtt_sig^2/2)
sd_vtt=abs(mean_vtt*sqrt(exp(log_vtt_sig^2)-1))
mean_vtt=60*mean_vtt
sd_vtt=60*sd_vtt

### simulated
beta=apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

### summary
output=matrix(0,nrow=6,ncol=2)
colnames(output)=c("Analytical","Simulated")
rownames(output)=c("tt_mu","tt_sig","tc_mu","tc_sig","vtt_mu","vtt_sig")

output[1,1]=mean_tt
output[2,1]=sd_tt
output[3,1]=mean_tc
output[4,1]=sd_tc
output[5,1]=mean_vtt
output[6,1]=sd_vtt

output[1,2]=mean(beta[["b_tt"]])
output[2,2]=sd(beta[["b_tt"]])
output[3,2]=mean(beta[["b_tc"]])
output[4,2]=sd(beta[["b_tc"]])
output[5,2]=mean(beta[["b_tt"]]/beta[["b_tc"]])*60
output[6,2]=sd(beta[["b_tt"]]/beta[["b_tc"]])*60

round(output,2)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()