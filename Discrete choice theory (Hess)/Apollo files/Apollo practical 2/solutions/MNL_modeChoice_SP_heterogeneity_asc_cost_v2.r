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
  modelName       = "MNL_modeChoice_SP_heterogeneity_asc_cost_v2",
  modelDescr      = "MNL model on mode choice SP data with heterogeneity in ASCs and cost (shift for business and income)",
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

### Create new variable with average income
database$mean_income = mean(database$income)

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
              cost_income_elast       = 0,
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
  b_cost   = ( b_cost_base + b_cost_shift_business * business ) * ( income / mean_income ) ^ cost_income_elast
  asc_bus = asc_bus_base + asc_bus_shift_female * female + asc_bus_shift_business * business
  asc_air = asc_air_base + asc_air_shift_female * female + asc_air_shift_business * business
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
#---- LIKELIHOOD RATIO TEST AGAINST BASE MODEL                   ----
# ----------------------------------------------------------------- #

apollo_lrTest("MNL_modeChoice_SP_heterogeneity_ASC",model)
apollo_lrTest("MNL_modeChoice_SP_heterogeneity_ASC_cost_v1",model)

# ----------------------------------------------------------------- #
#---- CALCULATE WTP FOR DIFFERENT GROUPS AND TIME ATTRIBUTES     ----
# ----------------------------------------------------------------- #

### WTP calculations for 5 time components, 2 groups of people (leisure/business) and 5 income categories
income_group=quantile(database$income,c(0,0.25,0.5,0.75,1))
business_group=c(0,1)

vtt_car=matrix(0,nrow=2,ncol=5)
vtt_bus=matrix(0,nrow=2,ncol=5)
vtt_air=matrix(0,nrow=2,ncol=5)
vtt_rail=matrix(0,nrow=2,ncol=5)
vtt_access=matrix(0,nrow=2,ncol=5)
wtp_wifi=matrix(0,nrow=2,ncol=5)
wtp_food=matrix(0,nrow=2,ncol=5)

j=1
k=1
while(j<=5){
  k=1
  while(k<=2){
    vtt_car[k,j]=60*(model$estimate["b_tt_car"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    vtt_bus[k,j]=60*(model$estimate["b_tt_bus"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    vtt_air[k,j]=60*(model$estimate["b_tt_air"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    vtt_rail[k,j]=60*(model$estimate["b_tt_rail"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    vtt_access[k,j]=60*(model$estimate["b_access"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    wtp_wifi[k,j]=-(model$estimate["b_wifi"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    wtp_food[k,j]=-(model$estimate["b_food"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*business_group[k])*((income_group[j]/mean(database$income))^model$estimate["cost_income_elast"]))
    k=k+1
  }
  j=j+1
}

colnames(vtt_car)=paste("Income",income_group)
rownames(vtt_car)=c("Leisure","Business")
colnames(vtt_bus)=paste("Income",income_group)
rownames(vtt_bus)=c("Leisure","Business")
colnames(vtt_air)=paste("Income",income_group)
rownames(vtt_air)=c("Leisure","Business")
colnames(vtt_rail)=paste("Income",income_group)
rownames(vtt_rail)=c("Leisure","Business")
colnames(vtt_access)=paste("Income",income_group)
rownames(vtt_access)=c("Leisure","Business")
colnames(wtp_wifi)=paste("Income",income_group)
rownames(wtp_wifi)=c("Leisure","Business")
colnames(wtp_food)=paste("Income",income_group)
rownames(wtp_food)=c("Leisure","Business")

round(vtt_car,2)
round(vtt_bus,2)
round(vtt_air,2)
round(vtt_rail,2)
round(vtt_access,2)
round(wtp_wifi,2)
round(wtp_food,2)

### Now use sample enumeration to calculate the VTT measures for every observation in the data
N=nrow(database)
vtt_car_sample=rep(0,N)
vtt_bus_sample=rep(0,N)
vtt_air_sample=rep(0,N)
vtt_rail_sample=rep(0,N)
vtt_access_sample=rep(0,N)
wtp_wifi=rep(0,N)
wtp_food=rep(0,N)

n=1
while(n<=N){
  vtt_car_sample[n]=60*(model$estimate["b_tt_car"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  vtt_bus_sample[n]=60*(model$estimate["b_tt_bus"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  vtt_air_sample[n]=60*(model$estimate["b_tt_air"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  vtt_rail_sample[n]=60*(model$estimate["b_tt_rail"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  vtt_access_sample[n]=60*(model$estimate["b_access"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  wtp_wifi[n]=-(model$estimate["b_wifi"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  wtp_food[n]=-(model$estimate["b_food"])/((model$estimate["b_cost_base"]+model$estimate["b_cost_shift_business"]*database$business[n])*((database$income[n]/mean(database$income))^model$estimate["cost_income_elast"]))
  n=n+1}

summary(vtt_car_sample)
summary(vtt_bus_sample)
summary(vtt_air_sample)
summary(vtt_rail_sample)
summary(vtt_access_sample)
summary(wtp_wifi)
summary(wtp_food)


# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()