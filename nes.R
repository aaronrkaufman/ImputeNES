#setwd("/nfs/home/A/akaufman/Desktop/shared_space/cong_rec/NES")
setwd("D://Documents//Research//Imputation")

#######################################################################
############# Generic Modeling Functions  #############################
#######################################################################

#Vars to impute in round 1
# ident_whiteid, ident_blackid 
# budget_rdefctax, dhs_torture
# paprofile_aarp, discrim_gays, 
# prevote_regparty (unordered), gend_gendobs (good test case)

## Another task: find set of vars to use to impute
## Use L1, and every single variable: "bet on sparsity"; but if dense, use L2
# Update in blocks or update individually? Let's assume individual, and test it later
# Maybe start with gender so that I don't have to do any fake missingness stuff

b_ordered_logit = function(varname){
  out = gbm(varname~., family="multinomial", data = nes)
}

b_cat = function(varname){
  out = gbm(varname~., family="bernoulli", data = nes)
}

digits_to_dummies = function(x){
  s = as.character(x)
  if(nchar(s)==1){
    s = paste("0", s, sep="")
  }
  fd = substr(s, start=1, stop=1)
  ld = substr(s, start=2, stop=2)
  if(s=="100"){
    fd = "10"
    ld = "0"
  }
  fds = as.numeric(fd == as.character(0:10))
  lds = as.numeric(ld == as.character(0:10))
  out = c(fds, lds)
}

dummies = function(var){
  z = lapply(var, digits_to_dummies)
  out = do.call(rbind, z)
}

b_ft = function(varname){
  dum = dummies(nes[,"varname"])
  temp = cbind(nes, dum)
  #now run penalized least squares?
  
  #this will be weird
  ## Maybe bin the feeling thermometer first?
  ## Maybe fit a model for the first digit, then the second digit using the first
  ## Both 10-cat ordered logit
}

#######################################################################
############# Generic Training Functions  #############################
#######################################################################
ensemble = function(train, test, varID){
  # here is where I put the classifier ensemble
  # maybe just a boosted regression
  varname = colnames(test)[varID]
  m = gbm(varname ~ ., distribution = "gaussian", ntrees=100, shrinkage=0.1, interaction.depth=2, train.fraction=1,
          keep.data=F)
  out = predict(gbm, test)
}

ft_ensemble = function(train, test, varID){
  #the function I use for a thermometer
  # 0s are 0s, 3-4-5 in a bucket, then 2nd ordered logit
  # polr in MASS
}



#One function to impute a single variable
#maybe helpful"
#which(sort(table(var))==max(sort(table(var))))
onevar = function(varID, na.code){
  var = nes[,varID]
  c = class(var)
  
  # make more sophisticated: check range
  # for therms: round to 
    
  if(c=="numeric"|c=="integer"){
    var = as.numeric(as.character(var))
    train = nes[var!=na.code, ]
    test = nes[var==na.code,]
    fit = gbm(varname~., family="gaussian", data = train)
    prediction = predict(fit, test)
  }
  if(c=="character"){
    var = as.numeric(as.character(var))
    train = nes[var!=na.code, ]
    test = nes[var==na.code,]
    fit = gbm(as.factor(varname)~., family="multinomial", data = train)
    prediction = predict(fit, test)
  }
  if(c=="factor"){
    var = as.numeric(as.character(var))
    train = nes[var!=na.code, ]
    test = nes[var==na.code,]
    fit = gbm(varname~., family="multinomial", data = train)
    prediction = predict(fit, test)
  }
  return(prediction)
}

#One function to loop over the set of variables
one_iteration = function(nes, na.codes){
  prediction = list()
  for(i in 1:ncol(nes)){
    prediction[[i]] = onevar(i, na.codes[i])
  }
  iterates[,ncol(iterates)+1] <<- unlist(prediction)
  return(unlist(prediction))
}

#One function to iterate: print MSE?
impute_nes = function(nes, iterates, tol = 100){
  conv = 10000
  while(conv > tol){
    newpred = one_iteration(nes, na.codes)
    iterates[, (ncol(iterates)+1)] <<- newpred
    conv = check_conv(iterates)
  }
  return(iterates)
}

## Plug the new values into the NES
input_imputed_vals = function(iterates, nes){
  #here's where I loop through the rows of iterates and plug the values into the nes
  return(final_nes)
}

#######################################################################
############# Generic Error Checking Functions  #######################
#######################################################################


## One function to randomly induce missingness
induce_nas = function(nes, p = 0.001){
  n = ncol(nes)*nrow(nes)
  l = as.logical(rbinom(n, 1, p))
  #extract the values, store to FRONT of init_iterates
  #fill in the rest: col, row, firstguess
}


## One function to check the MSE
check_mse = function(iterates){
  subset = iterates[!is.na(iterates$truth),]
  se = (iterates$truth - iterates[,ncol(iterates)])^2
  mse = mean(se)
  print(mse)
}

## One function to store iterates of all the missing entries, so check for convergence
init_iterates = function(nes){
  na.values = list()
  for(i in 1:ncol(nes)){
    row = which(nes[,i]=="NA")
  }
  return(na.vaues)
}

## First iterate: mean/mode for each thing
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

modeval = function(ind){
  c = class(nes[,ind])
  # here is where I figure out whether to take the mean, mode, etc
  modes = Mode(nes[,ind])
}

## Check for convergence
check_conv = function(iterates){
  # this function normalizes all variables 0-1, then measures MSE
}

#######################################################################
############# Preprocessing  ##########################################
#######################################################################

### First, subset out the columns we don't want
clean_nes = function(nes){
  start = 13:ncol(nes)
  nes = nes[,start]
  c = sapply(1:ncol(nes), FUN=function(x) class(nes[,x]))
  nes = nes[,c!="character"]
  l = sapply(1:ncol(nes), FUN=function(x) length(levels(nes[,x])))
  nes = nes[,l!=1]
  nes = nes[,!grepl("randord", colnames(nes))]
  u = sapply(1:ncol(nes), FUN=function(x) length(unique(nes[,x])))
  nes = nes[,u!=1]
}


### Next, go through the remaining columns and clean them up so that they can go into a prediction function.
recode_values = function(ind){
  vec = nes[,ind]
  ls = levels(vec)
  levels(vec)[levels(vec)=='-1. Inapplicable'] <- "NA"
  levels(vec)[levels(vec)=='-2. Missing'] <- "NA"  
  levels(vec)[levels(vec)=='-2. Missing: R entered DK/RF for different address'] <- "NA"
  levels(vec)[levels(vec)=='-2. Missing; available in forthcoming release'] <- "NA"
  levels(vec)[levels(vec)=='-2. Available in future release'] <- "NA"
  levels(vec)[levels(vec)=='-3. Restricted Access'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error; FTF: pre reg in same county as residence but no ballot card use'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said voted for Senate but no Sen race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said preferred Senate but no Sen race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said voted for governor but no gov race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said preferred governor but no gov race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-6. Not asked, unit nonresponse (no post-election interview)'] <- "NA"
  levels(vec)[levels(vec)=='-6. Not asked, unit nonresponse'] <- "NA"
  levels(vec)[levels(vec)=='-6. Inapplicable'] <- "NA"
  levels(vec)[levels(vec)=='-7. Deleted due to partial (post-election) interview'] <- "NA"
  levels(vec)[levels(vec)== '-8. Don\'t know'] <- "NA"
  levels(vec)[levels(vec)=='-9. Refused'] <- "NA"
  return(vec)
}

recode_wrap = function(nes){
  nes2 = nes
  for(i in 1:ncol(nes2)){
    nes2[,i] = recode_values(i)
  }
  return(nes2)
}

warm_start = function(nes){
  mode.val = c()
  for(i in 1:ncol(nes)){
    mode.val[i] = Mode(nes[,i])
    if(mode.val[i]=="NA"){
      mode.val[i] = names(sort(table(nes[,i]), decreasing=T)[2])
    }
  }  
  return(mode.val) 
}

warm_start_wrap = function(nes, iterates){
  nes2 = nes
  modes = warm_start(nes2)
  #now go through every missing value and input the relevant mode...
  for(i in 1:ncol(nes2)){
    nes2[iterates[[i]],i] = modes[i]
  }
  return(nes2)
}

#######################################################################
############# Workflow  ###############################################
#######################################################################

library(foreign)
library(gbm)
library(glmnet)
library(car)

nes = read.dta("anes_timeseries_2012_stata12.dta")
nes = clean_nes(nes)
nes = recode_wrap(nes) # recodes miscellaneous error values to NA
iterates = init_iterates(nes) # collects the locations of all the missing values
nes = warm_start_wrap(nes, iterates) # initializes missing values at column mode (or second column mode if mode is NA)
#iterates = induce.nas(iterates, 0.001)   ### Uncomment to add false missingness for testing purposes
missing.vals = impute_nes(nes, iterates) # Runs the bulk of the imputation
final = input_imputed_vals(iterates, nes)
#mse = check_accuracy(missing.vals)


#######################################################################
############# Testing   ###############################################
#######################################################################
#nes = read.dta("anes_timeseries_2012_stata12.dta")
#save(nes, file="nes.RData")
load("nes.RData")
nes = clean_nes(nes)

nes2 = nes[,1:20]
nes2 = recode_wrap(nes2)
#Next, figure out which variables have any missingness
its = init.iterates(nes2)
#Next, warm start the imputation by putting in the most common non-missng value
modes = warm_start(nes2)
## Now input modes
nes2 = warm_start_wrap(nes2)

## Maybe also drop variables where there are more NA values than covariates, for singularity purposes?

fit = glmnet(y=as.numeric(train.y)-3, x=train.x, family="binomial")
fit = glm.fit(y=as.numeric(train.y)-3, x=train.x, family=binomial())


#preds = predict(fit, test.x)
#preds[preds==preds[[1]]] = 0
#preds[preds==preds[[2]]] = 1
#truth =  nes$gender_respondent_x[nes$gend_gendobs == "-1. Inapplicable"]
#truth = as.numeric(truth) - 1
#mse = sqrt((truth-preds)^2)
#mse


#######################################################################
############# FT ######################################################
#######################################################################
ft = nes$ft_rpc
ft = ft[ft!=-2 & ft!=-8 & ft!=-9]
hist(ft, breaks=0:100, main="Distribution of Feeling Thermometer Scores")
ld = ft %% 10
fd = round(ft)

par(mfrow=c(2,2))
hist(ft, breaks=0:100, main="Distribution of Feeling Thermometer Scores")
hist(ld, main="Distribution of Last Digits")
hist(fd, main="Distribution of First Digits")


#what if I have a latent preference and then a dummy variable for each first and last digit?
latent = sapply(ft, FUN=function(x) rnorm(1, x, 5))
latent[latent <= 0] = 0
latent[latent >= 100] = 100


## Derive empirical penalties for FT scores
### Aggregate all FT values
ft = c(nes$ft_rpc, nes$ft_dpc, nes$ft_dvpc, nes$ft_rvpc, nes$ft_hclinton, nes$ft_gwb, nes$ft_dem, nes$ft_rep,
       nes$ftpo_dpcsp, nes$ftpo_rpcsp, nes$ftpo_hdc, nes$ftpo_hrc, nes$ftpo_sdc, nes$ftpo_src,
       nes$ftpo_snsr, nes$ftpo_senjr, nes$ftpo_roberts, nes$ftgr_xfund, nes$ftgr_catholics,
       nes$ftgr_feminists, nes$ftgr_fedgov, nes$ftgr_liberals,nes$ftgr_middle, nes$ftgr_unions,
       nes$ftgr_poor, nes$ftgr_military, nes$ftgr_bigbus, nes$ftgr_welfare, nes$ftgr_cons, nes$ftgr_working,
       nes$ftgr_ussc, nes$ftgr_gay, nes$ftgr_congress, nes$ftgr_rich, nes$ftgr_muslims, nes$ftgr_xian, nes$ftgr_atheists,
       nes$ftgr_mormons, nes$ftgr_tea, nes$ftcasi_asian, nes$ftcasi_hisp, nes$ftcasi_black,
       nes$ftcasi_illegal, nes$ftcasi_white)
ft = ft[ft>=0]
hist(ft, breaks=0:100, main="Distribution of Feeling Thermometer Scores")
ld = ft %% 10
fd = (ft - (ft %% 10))/10

ldt = c(table(ld)/length(ld))
ld_penalties = (ldt^-1)/sum(ldt^-1)

fdt = c(table(fd)/length(fd))
fd_penalties = (fdt^-1)/sum(fdt^-1)