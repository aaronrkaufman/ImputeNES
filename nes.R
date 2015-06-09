setwd("/nfs/home/A/akaufman/Desktop/shared_space/cong_rec/NES")
#setwd("D://Documents//Research//Imputation")

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
  nes = nes[,-1535]
}


### Next, go through the remaining columns and clean them up so that they can go into a prediction function.
recode_values = function(ind){
  vec = nes[,ind]
  ls = levels(vec)
  levels(vec)[levels(vec)=='-1. Inapplicable'] <- "NA"
  levels(vec)[levels(vec)=='-1. Inap, no Democratic gubernatorial candidate; no governor race in state'] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, no independent/3rd party/other gubernatorial candidate; no governor race in state"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, Senate race in state"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, no independent/3rd party/other House candidate; district not identified (Web only)"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inapplicable (same date as du module or pre IWR obs not completed)"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, religion group not determinable at preload"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, R did not vote or DK/RF if voted; voted but not (or DK/RF if) for governor; no gov race in state of vote"] <- "NA"
  levels(vec)[levels(vec)=="-1. Inap, R did not vote or DK/RF if voted; voted but not (or DK/RF if) for us Senate; no us Sen race in state of vote"] <- "NA"
  
  levels(vec)[levels(vec)=='-1'] <- "NA"
  levels(vec)[levels(vec)=='-2'] <- "NA"
  levels(vec)[levels(vec)=='-2. Missing'] <- "NA"  
  levels(vec)[levels(vec)=="-2. Haven't thought much about this"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing, field left blank"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing, other not codeable to 1-5"] <- "NA"  
  
  levels(vec)[levels(vec)=="-2. Missing; R repeated existing race mention in other specify for race or other specify recoded to existing race"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing; R gave Native American/Alaskan identification in text of other specify for race"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing; R gave White identification in text of other specify for race"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing; IWR mistakenly entered '2' in place of DK code for total income"  ] <- "NA"  
  
  levels(vec)[levels(vec)=="-2. Text responses available in separate file"] <- "NA"  
  levels(vec)[levels(vec)=="-2. Missing; R gave Black identification in text of other specify for race" ] <- "NA"  
  levels(vec)[levels(vec)=='-2. Missing: R entered DK/RF for different address'] <- "NA"
  levels(vec)[levels(vec)=='-2. Missing; available in forthcoming release'] <- "NA"
  levels(vec)[levels(vec)=='-2. Available in future release'] <- "NA"
  levels(vec)[levels(vec)=='-2. State of registration asked, not recorded'] <- "NA"
  levels(vec)[levels(vec)=='-3. Restricted Access'] <- "NA"
  levels(vec)[levels(vec)=="-3. Restricted access"] <- "NA"
  levels(vec)[levels(vec)=='-3'] <- "NA"
  levels(vec)[levels(vec)=='-4'] <- "NA"
  levels(vec)[levels(vec)=="-4. Error; FTF: R said preferred Senate but no Sen race in reg state (not recorded correctly or error, no ballot card)"] <- "NA"
  
  levels(vec)[levels(vec)=='-4. Error; FTF: pre reg in same county as residence but no ballot card use'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said voted for Senate but no Sen race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said preferred Senate but no Sen race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said voted for governor but no gov race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-4. Error- R said preferred governor but no gov race in reg state (not recorded correctly or error, no ballot card)'] <- "NA"
  levels(vec)[levels(vec)=='-6. Not asked, unit nonresponse (no post-election interview)'] <- "NA"
  levels(vec)[levels(vec)=='-6. Not asked, unit nonresponse'] <- "NA"
  levels(vec)[levels(vec)=='-6. Unit nonresponse (no post-election interview)'] <- "NA"
  
  levels(vec)[levels(vec)=='-6. Inapplicable'] <- "NA"
  levels(vec)[levels(vec)=='-7. Deleted due to partial (post-election) interview'] <- "NA"
  levels(vec)[levels(vec)== '-8. Don\'t know'] <- "NA"
  levels(vec)[levels(vec)== '-8. Don\'t now'] <- "NA"
  
  levels(vec)[levels(vec)=='-9. Refused'] <- "NA"
  return(vec)
}

recode_values_numeric = function(ind){
  vec = nes[,ind]
  vec[vec==-1] = NA
  vec[vec==-2] = NA
  vec[vec==-3] = NA
  vec[vec==-4] = NA
  vec[vec==-5] = NA
  vec[vec==-6] = NA
  vec[vec==-7] = NA
  vec[vec==-8] = NA
  vec[vec==-9] = NA  
  return(vec)
}

recode_factor = function(nes){
  nes2 = nes
  for(i in 1:ncol(nes2)){
    nes2[,i] = recode_values(i)
  }
  return(nes2)
}

recode_numeric = function(nes){
  nes2 = nes
  for(i in 1:ncol(nes2)){
    nes2[,i] = recode_values_numeric(i)
  }
  return(nes2)
}

trim_na_cols = function(nes){
  idx = c()
  for(i in 1:ncol(nes)){
    if(all(nes[,i]=="NA")){
      idx = c(idx, i)
    }
  }
  sd = setdiff(1:ncol(nes), idx)
  nes2 = nes[,sd]
  return(nes2)
}

trim_singular_cols = function(nes){
  idx = c()
  for(i in 1:ncol(nes)){
    if(class(nes[,i])=="factor"){
      us = unique(nes[,i])
      observed = sum(us != "NA")      
      if(observed==1){
        idx = c(idx, i)
      }
    }
  }
  sd = setdiff(1:ncol(nes), idx)
  nes2 = nes[,sd]
  return(nes2)
}

trim_rand_cols = function(nes){
  idx = c()
  for(i in 1:ncol(nes)){
    if(grepl("rand", colnames(nes)[i])){
      idx = c(idx, i)
    }
  }
  sd = setdiff(1:ncol(nes), idx)
  nes2 = nes[,sd]
  return(nes2)
}

## One function to store iterates of all the missing entries, so check for convergence
init_iterates = function(nes){
  na.values = list()
  for(i in 1:ncol(nes)){
    temp1 = which(nes[,i]=="NA")
    temp2 = which(is.na(nes[,i]))
    na.values[[i]] = sort(c(temp1, temp2))
  }
  return(na.values)
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

warm_start = function(nes){
  mode.val = c()
  for(i in 1:ncol(nes)){
    mode.val[i] = Mode(nes[,i])
    if(is.na(mode.val[i])){
      mode.val[i] = names(sort(table(nes[,i]), decreasing=T)[2])
    }
  }  
  return(mode.val) 
}

warm_start_wrap = function(nes, iterates){
  nes2 = nes
  modes = warm_start(nes2)
  inputed_values = c()
  #now go through every missing value and input the relevant mode
  for(i in 1:ncol(nes2)){
  #for(i in 1000:1200){
    if(class(nes[,i])=="factor"){
      levs = levels(nes2[,i])
      m = levs[as.numeric(modes[i])]
      if(m=="NA"){
        m=levs[as.numeric(modes[i])+1]
      }
      #print(m)
      inputed_values[i] = m
      nes2[iterates[[i]],i] = m
    }
    if(class(nes[,i])=="numeric"){
      #print(modes[i])
      inputed_values[i] = as.numeric(as.character(modes[i]))
      nes2[iterates[[i]],i] = as.numeric(as.character(modes[i]))
    }

  }
  return(nes2)
}

#######################################################################
############# Generic Error Checking Functions  #######################
#######################################################################


## One function to randomly induce missingness
induce_nas = function(iterates, nes, pr = 0.001){
  n = ncol(nes)*nrow(nes)
  l = sample(1:n, n*pr)
  l = sort(l)
  r = l %% nrow(nes)
  c = floor(l/nrow(nes)) + 1
  iters2 = iterates
  
  induced_nas = list()
  length(induced_nas) = ncol(nes)
  truth = list()
  length(truth) = ncol(nes)
  
  for(i in 1:length(l)){
    iters2[[c[i]]] = c(iters2[[c[i]]], r[i])
    dup = !duplicated(iters2[[c[i]]]) #in case the randomly induced missing vals are already missing...
    iters2[[c[i]]] = iters2[[c[i]]][dup]
    
    induced_nas[[c[i]]] = c(induced_nas[[c[i]]], r[i]) 
    #truth[[c[i]]] = c(truth[[c[i]]], nes[r[i], c[[i]]]) ## change this: put it outside the loop:
  }
  
  for(i in 1:ncol(nes)){
    truth[[i]] = nes[induced_nas[[i]],i] #check this t make sure they're all factors that need to be
    idx = !is.na(truth[[i]]) & truth[[i]]!= "NA"
    truth[[i]] = truth[[i]][idx]
  }
  
  out = list(iters2, induced_nas, truth)
  return(out)
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



## Check for convergence
check_conv = function(iterates){
  # this function normalizes all variables 0-1, then measures MSE
}


## MICE comparison
require(mice)
check_mice = function(nes2, iterates, indices, truth, subset=NULL){
  nes3 = nes2
  for(1 in 1:ncol(nes3)){
    nes3[iterates[[i]],i] = NA
  }
  if(!is.null(subset)){
    nes3 = nes3[,subset]
  }
  nes_mice = mice(nes3, MaxNWts = 20000)
  mice_guess = list()
  correct = 0
  imp = complete(nes_mice)
  for(i in 2:ncol(nes3)){
    mice_guess[[i]] = imp[indices[[i]],i]
    correct = correct + sum(unlist(mice_guess[[i]])==unlist(truth[i]))
  }
  return(correct)
}

test = check_mice(nes2, iterates, indices, truth, subset=1:50)
test2 = one_iteration(nes[,1:50], nes2[,1:50])

#######################################################################
############# Iterating Training Functions  ###########################
#######################################################################

# try collapsing this function?
onevar = function(varID, nes, nes2){ #be careful here: the training set is from nes2, but the index is from nes1
  var = nes[,varID]
  varname = colnames(nes)[varID]
  c = class(var)
  print(varID)
  rows_missing = sort(iterates[[varID]])
  rows_present = setdiff(1:nrow(nes), rows_missing)
  if(length(iterates[[i]])!=0){
    if(c=="numeric"){
      train = nes2[rows_present, ]
      test = nes2[rows_missing,]
      if(nrow(train)==1){ # not much we can do about that...
        prediction = var
      } else if(nrow(train)<=  51){
        nm = floor(nrow(train)*.5) - 1
        fit = gbm.fit(y= var[rows_present], x = train[,-varID], distribution = "gaussian", 
                      bag.fraction=2, n.minobsinnode = nm, keep.data=F)
        prediction = predict(fit, test, n.trees=100, type="response")
        prediction = as.numeric(prediction)      
      } else if(nrow(train) > 51){
        fit = gbm.fit(y= var[rows_present], x = train[,-varID], distribution = "gaussian", keep.data=F)
        prediction = predict(fit, test, n.trees=100, type="response")
        if(!is.null(nrow(prediction))|length(dim(prediction))==3){
          prediction = sapply(1:nrow(prediction), FUN=function(x) which(prediction[x,,]==max(prediction[x,,])))
        }
        prediction = as.numeric(unlist(prediction))
      }

    }
    if(c=="factor"){
      train = nes2[rows_present, ]
      test = nes2[rows_missing,]
      if(nrow(train)==1){
        prediction = var
      } else if(nrow(train)<=  51){
        nm = floor(nrow(train)*.5) - 1
        fit = gbm.fit(y= var[rows_present], x = train[,-varID], distribution = "multinomial",
                      bag.fraction=2, n.minobsinnode = nm, keep.data=F)
        prediction = predict(fit, test, n.trees=100, type="response")
        prediction = as.factor(prediction)
      } else if(nrow(train)> 51){
        fit = gbm.fit(y= var[rows_present], x = train[,-varID], distribution = "multinomial", keep.data=F)
        prediction = predict(fit, test, n.trees=100, type="response")
        if(!is.null(nrow(prediction))|length(dim(prediction))==3){
          prediction = sapply(1:nrow(prediction), FUN=function(x) which(prediction[x,,]==max(prediction[x,,])))
        }
        prediction = as.factor(unlist(prediction))
        levels(prediction) = levels(factor(nes2[,varID]))
      }
    } 
  } else if(length(iterates[[i]])==0){
    prediction = c()
  }
  
  return(prediction)
}




#One function to loop over the set of variables
one_iteration = function(nes, nes2){
  prediction = lapply(1:(ncol(nes)-10), FUN=function(x) onevar(x, nes, nes2)) 
  
  nes3 = nes #storing a temp dataframe
  
  correct = 0
  for(i in 1:(ncol(nes)-10)){
    nes3[iterates[[i]],i] <- prediction[[i]]
    #correct = correct + sum(prediction[[i]][indices[[i]]]==truth[[i]]) ## note: doesn't quite work yet
  }
  
  #print accuracy here
  #print(paste("Accuracy: ", correct))
  
  return(nes3)
}

#One function to iterate: print MSE?
impute_nes = function(nes, iterates, tol = 100, false.missingness=T){
  conv = 10000
  while(conv > tol){
    newpred = one_iteration(nes, na.codes)
    iterates[, (ncol(iterates)+1)] <<- newpred
    conv = check_conv(iterates)
  }
  # have a thing which prints accuracy
  return(iterates)
}

## Plug the new values into the NES
input_imputed_vals = function(iterates, nes){
  #here's where I loop through the rows of iterates and plug the values into the nes
  return(final_nes)
}


#######################################################################
############# Workflow  ###############################################
#######################################################################
#setwd("D://Documents//Research//Imputation")
library(foreign)
library(gbm)
library(glmnet)
library(car)

#nes = read.dta("anes_timeseries_2012_stata12.dta")
load("nes.RData")
nes = clean_nes(nes)
nes = recode_factor(nes) # recodes miscellaneous error values to NA
nes = recode_numeric(nes)
nes = trim_na_cols(nes) # delete variables which are now all NAs
nes = trim_singular_cols(nes)
iterates = init_iterates(nes) # collects the locations of all the missing values
nes2 = warm_start_wrap(nes, iterates) # initializes missing values at column mode (or second column mode if mode is NA)

temp = induce_nas(iterates, nes, 0.001)
iterates = temp[[1]]   ### Adds false missingness for testing purposes and accuracy measurement
indices = temp[[2]]
truth = temp[[3]]
rm(temp)

#load("current.RData")
#start = Sys.time()
#test = one_iteration(nes, nes2)
#time_taken = Sys.time() - start

missing.vals = impute_nes(nes2, iterates) # Runs the bulk of the imputation; probably will have to turn some loops to applys
final = input_imputed_vals(iterates, nes)
mse = check_accuracy(missing.vals) # Mahalanobis?

#Also in clude the part where I run MICE and compare accuracy? Maybe for each variable one by one?

#######################################################################
############# Testing   ###############################################
#######################################################################

## Mahalanobis distance instead?

## Compare to MICE?

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