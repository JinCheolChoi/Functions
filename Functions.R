#*******************************
#
#
# [ Operational functions ] ----
#
#
#*******************************
# package check
#
#**************
checkpackages=function(package){
  # Checking the Availability of packages 
  # Installs them.  
  # Example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package, character.only =T)
}

#********
#
# rounds2
#
#********
round2=function(x, n) {
  posneg=sign(x)
  z=abs(x)*10^n
  z=z + 0.5
  z=trunc(z)
  z=z/10^n
  z*posneg
}

#***************
#
# elapsed_months
#
#***************
# calculate the difference between two timesteps in month
#********************************************************
elapsed_months=function(start_date, end_date){
  sd=as.POSIXlt(start_date)
  ed=as.POSIXlt(end_date)
  12*(ed$year-sd$year)+(ed$mon-sd$mon)
}

#************************************
# convert the class of *_DATE to date
#************************************
convert_date=function(data){
  date.ind=grep('DATE', names(data))
  data[, (date.ind):=lapply(.SD, as.Date), .SDcols=date.ind]
}

#*************
#
#
# [ GLM ] ----
#
#
#******************************
#
# GLM_NB_Bivariate_Personal_Jin
#
#******************************
# Run simple GLM models for each explanatory variable with negative binomial distribution 
# to account for zero-inflated data (i.e. the excessive number of 0s).
#********
# Example
#*****************
# require(geepack)
# require(dplyr)
# data("respiratory")
# Data=respiratory %>%
#   group_by(id) %>%
#   mutate(count_outcome=sum(outcome)) %>%
#   filter(visit==max(visit)) %>%
#   ungroup(id) %>%
#   dplyr::select(-outcome)
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline")
# Outcome_name="count_outcome"
# Offset_name="visit"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# GLM_NB_Bivariate_Personal_Jin(Data, ColumnsToUse, Outcome_name,
#                               Offset_name, vector.OF.classes.num.fact, levels.of.fact)
GLM_NB_Bivariate_Personal_Jin=function(Data, ColumnsToUse, Outcome_name, Offset_name, vector.OF.classes.num.fact, levels.of.fact){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert response variable to numeric
  Data[, Outcome_name]=as.numeric(as.character(Data[, Outcome_name]))
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[,ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[,ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[,ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
    
    # run model
    fullmod=as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i],"+offset(log(",Offset_name,"))"))
    GEE.m=glm.nb(fullmod, data=Data)
    
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    RR.CI=exp(cbind(RR=coef(GEE.m), confint(GEE.m, level=0.95)))
    colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
    est=cbind(summary(GEE.m)$coefficients, RR.CI)
    
    # output
    output=rbind(output,
                 data.frame(
                   Estimate=round2(est[-1, "Estimate"], 3),
                   Std.Error=round2(est[-1, "Std. Error"], 3),
                   `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001", 
                                    format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
                   OR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] ,2), nsmall=2),
                                    " (", format(round2(as.numeric(est[-1, "Lower RR"]),2), nsmall=2), " - ",
                                    format(round2(as.numeric(est[-1, "Upper RR"]),2), nsmall=2), ")"),
                   row.names=names(coef(GEE.m))[-1]
                 )
    )
    #print(paste0(i, " ", ColumnsToUse[i]))
  }
  return(output)
}

#**************************
#
# GLM_NB_Multi_Personal_Jin
#
#**************************
# Run a multiple GLM model with negative binomial distribution to account for zero-inflated data
# (i.e. the excessive number of 0s).
#********
# Example
#*****************
# require(geepack)
# require(dplyr)
# data("respiratory")
# Data=respiratory %>%
#   group_by(id) %>%
#   mutate(count_outcome=sum(outcome)) %>%
#   filter(visit==max(visit)) %>%
#   ungroup(id) %>%
#   dplyr::select(-outcome)
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline")
# Outcome_name="count_outcome"
# Offset_name="visit"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# GLM_NB_Multi_Personal_Jin(Data, ColumnsToUse, Outcome_name,
#                           Offset_name, vector.OF.classes.num.fact, levels.of.fact)
GLM_NB_Multi_Personal_Jin=function(Data, ColumnsToUse, Outcome_name, Offset_name, vector.OF.classes.num.fact, levels.of.fact){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert variable class
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name])) # response variable
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
  }
  
  # run model
  fullmod=as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+"), "+offset(log(", Offset_name, "))"))
  GEE.m=glm.nb(fullmod, data=Data)
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  RR.CI=exp(cbind(RR=coef(GEE.m), confint(GEE.m, level=0.95)))
  colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
  est=cbind(summary(GEE.m)$coefficients, RR.CI)
  
  data.frame(
    Estimate=round2(est[-1, "Estimate"], 3),
    Std.Error=round2(est[-1, "Std. Error"], 3),
    `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001", 
                     format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
    OR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] ,2), nsmall=2),
                     " (", format(round2(as.numeric(est[-1, "Lower RR"]),2), nsmall=2), " - ",
                     format(round2(as.numeric(est[-1, "Upper RR"]),2), nsmall=2), ")"),
    row.names=names(coef(GEE.m))[-1]
  )
  
  # output
  output=data.frame(Estimate=round2(est[-1, "Estimate"], 3),
                    Std.Error=round2(est[-1, "Std. Error"], 3),
                    `P-value`=ifelse(round2(est[-1, "Pr(>|z|)"], 3)<0.001, "<0.001", 
                                     format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
                    OR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] ,2), nsmall=2),
                                     " (", format(round2(as.numeric(est[-1, "Lower RR"]),2), nsmall=2), " - ",
                                     format(round2(as.numeric(est[-1, "Upper RR"]),2), nsmall=2), ")"),
                    row.names=names(coef(GEE.m))[-1]
  )
  return(output)
}


#*************
#
#
# [ GEE ] ----
#
#
#******************
#
# GEE_Bivariate_Jin
#
#******************
# Example
#******************
# require(geepack)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# GEE_Bivariate_Jin(Data, ColumnsToUse, Outcome_name, ID_name, which.family,
#               vector.OF.classes.num.fact, levels.of.fact)
GEE_Bivariate_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family="binomial", vector.OF.classes.num.fact, levels.of.fact){
  # check out packages
  lapply(c("geepack", "MESS", "doBy"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert response variable to numeric
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name]))
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[,ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[,ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[,ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
    
    # not consider data with missing value
    target.indx=which(is.na(Data[,ColumnsToUse[i]])==F) 
    
    # run model
    GEE.m=geeglm(as.formula(paste(Outcome_name, "~", ColumnsToUse[i])),
                 id=Data[target.indx, ID_name],  data=Data[target.indx, c(Outcome_name, ColumnsToUse)], family=which.family, corstr="exchangeable")
    
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    est=esticon(GEE.m, diag(length(coef(GEE.m))))[-1,]
    
    # output
    output=rbind(output,
                 data.frame(
                   Estimate=round2(est$Estimate, 3),
                   Std.Error=round2(est$Std.Error, 3),
                   `P-value`=ifelse(est$`Pr(>|X^2|)`<0.001, "<0.001", 
                                    format(round2(est$`Pr(>|X^2|)`, 3), nsmall=3)),
                   OR.and.CI=paste0(format(round2(exp(est$Estimate), 2), nsmall=2),
                                    " (", format(round2(exp(est$Lower), 2), nsmall=2), " - ",
                                    format(round2(exp(est$Upper), 2), nsmall=2), ")"),
                   row.names=names(coef(GEE.m))[-1]
                 )
    )
    #print(paste0(i, " ", ColumnsToUse[i]))
  }
  return(output)
}

#***********************
#
# GEE_Multivariable_Jin
#
#***********************
# Example
#****************
# require(geepack)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# GEE_Multivariable_Jin(Data, ColumnsToUse, Outcome_name, ID_name, which.family,
#                       vector.OF.classes.num.fact, levels.of.fact)
GEE_Multivariable_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact, levels.of.fact){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # delete data with missing value
  del=unique(which(is.na(Data[,c(Outcome_name,ColumnsToUse)]), arr.ind=T)[,1])
  
  if(length(del)>0){
    Data=Data[-del, ]}else{ Data=Data}
  
  # convert variable class
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name])) # response variable
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
  }
  
  # run model
  fullmod=as.formula( paste( Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+")))
  GEE.m=geeglm(fullmod, data=Data[,c(ColumnsToUse, ID_name, Outcome_name)], id=Data[,ID_name], family=which.family, corstr="exchangeable")
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(GEE.m, diag(length(coef(GEE.m))))[-1,]
  
  # output
  output=data.frame(Estimate=round2(est$Estimate, 3),
                    Std.Error=round2(est$Std.Error, 3),
                    `P-value`=ifelse(round2(est$`Pr(>|X^2|)`, 3)<0.001, "<0.001", 
                                     format(round2(est$`Pr(>|X^2|)`, 3), nsmall=3)),
                    OR.and.CI=paste0(format(round2(exp(est$Estimate), 2), nsmall=2),
                                     " (", format(round2(exp(est$Lower), 2), nsmall=2), " - ",
                                     format(round2(exp(est$Upper), 2), nsmall=2), ")"),
                    row.names=names(coef(GEE.m))[-1]
  )
  return(output)
}

#*************
#
#
# [ LMM ] ----
#
#
#******************
#
# LMM_Bivariate_Jin
#
#******************
# require(geepack)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# LMM_Bivariate_Jin(Data, ColumnsToUse, Outcome_name, ID_name, vector.OF.classes.num.fact, levels.of.fact)
LMM_Bivariate_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, vector.OF.classes.num.fact, levels.of.fact){
  # check out packages
  lapply(c("lme4", 
           "lmerTest"), # produce p-value of coefficient
         checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert response variable to numeric
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name]))
  
  # main algorithm
  coefficient=c()
  prediction=c()
  for(i in 1:length(ColumnsToUse)){
    #i=4
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
    # run model
    fullmod=as.formula(paste(Outcome_name, "~", ColumnsToUse[i], "+(1|", ID_name, ")", sep=""))
    myfit=lmer(fullmod, na.action=na.exclude, data=Data)
    # coefficient
    Coef=summary(myfit)$coefficients
    Coef.ind=which(grepl(ColumnsToUse[i], row.names(Coef)))
    # confidence interval (exponentiated)
    CI=confint(myfit, level=0.95,method="Wald")
    CI.ind=which(grepl(ColumnsToUse[i], row.names(CI)))
    
    # prediction
    if(vector.OF.classes.num.fact[i]=="num"){
      prediction[[i]]=Coef[1, 1]+Coef[2, 1]*sort(unique(Data[,ColumnsToUse[i]]))
    }else{
      prediction[[i]]=Coef[1, 1]+Coef[2, 1]*seq(0, 1)
    }
    
    # coefficient
    coefficient=rbind(coefficient, 
                      data.frame(
                        Estimate=round2(Coef[, "Estimate"][Coef.ind], 3),
                        Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3),
                        `P-value`=ifelse(Coef[, "Pr(>|t|)"][Coef.ind]<0.001, "<0.001", 
                                         format(round2(Coef[, "Pr(>|t|)"][Coef.ind], 3), nsmall=3)),
                        `95 CI`=paste0("(", format(round2(CI[CI.ind, 1], 3), nsmall=2), ", ",
                                         format(round2(CI[CI.ind, 2], 3), nsmall=2), ")")
                      )
    )
    #print(paste(i," ", ColumnsToUse[i],sep=""))
  }
  # name list
  names(prediction)=paste0("pred.val.", ColumnsToUse)
  
  # output
  output=list(coefficient, prediction)
  names(output)=c("Coefficient", "Prediction")
  return(output)
}


#**************
#
#
# [ GLMM ] ----
#
# * This can be used for GLM (glm()) as well provided that there is one observation for each individual.
#
#
#********************
#
# GLMM_Bivariate_Jin
#
#********************
# Example
#******************
# require(geepack)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# NAGQ=100
# GLMM_Bivariate_Jin(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact, levels.of.fact, NAGQ)
GLMM_Bivariate_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact, levels.of.fact, NAGQ=100){
  # check out packages
  lapply(c("lme4"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert response variable to numeric
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name]))
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
    # run model
    fullmod=as.formula(paste(Outcome_name, "~", ColumnsToUse[i], "+(1|", ID_name, ")", sep=""))
    myfit=glmer(fullmod, family=which.family,na.action=na.exclude, data=Data,nAGQ=NAGQ)
    # coefficient
    Coef=summary(myfit)$coefficients
    Coef.ind=which(grepl(ColumnsToUse[i], row.names(Coef)))
    # confidence interval (exponentiated)
    CI=exp(confint(myfit, level=0.95,method="Wald"))
    CI.ind=which(grepl(ColumnsToUse[i], row.names(CI)))
    
    # output
    output=rbind(output, 
                 data.frame(
                   Estimate=round2(Coef[, "Estimate"][Coef.ind], 3),
                   Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3),
                   `P-value`=ifelse(Coef[, "Pr(>|z|)"][Coef.ind]<0.001, "<0.001", 
                                    format(round2(Coef[, "Pr(>|z|)"][Coef.ind], 3), nsmall=3)),
                   OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2),
                                    " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ",
                                    format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
                 )
    )
    
    
    print(paste(i," ", ColumnsToUse[i],sep=""))
  }
  return(output)
}

#************************
#
# GLMM_Multivariable_Jin
#
#************************
# Example
#******************
# require(geepack)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# NAGQ=100
# GLMM_Multivariable_Jin(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact, levels.of.fact, NAGQ)
GLMM_Multivariable_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact, levels.of.fact, NAGQ=100){
  # check out packages
  lapply(c("lme4"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert variable class
  Data[,Outcome_name]=as.numeric(as.character(Data[,Outcome_name])) # response variable
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[,ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[,ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[,ColumnsToUse[i]], ref=levels.of.fact[i])
    }
  }
  # run model
  fullmod=as.formula(paste(Outcome_name, "~", paste(ColumnsToUse, collapse="+"),"+(1|", ID_name, ")", sep=""))
  myfit=glmer(fullmod, family=which.family, na.action=na.exclude, data=Data, nAGQ=NAGQ)
  # coefficient
  Coef=summary(myfit)$coefficients
  Coef.ind=c()
  for(i in 1:length(ColumnsToUse)){
    Coef.ind=c(Coef.ind, which(grepl(ColumnsToUse[i], row.names(Coef))))
  }
  Coef.ind=sort(unique(Coef.ind))
  
  # confidence interval (exponentiated)
  CI=exp(confint(myfit, level=0.95,method="Wald"))
  CI.ind=c()
  for(i in 1:length(ColumnsToUse)){
    CI.ind=c(CI.ind, which(grepl(ColumnsToUse[i], row.names(CI))))
  }
  CI.ind=sort(unique(CI.ind))
  
  # output
  output=data.frame(
    Estimate=round2(Coef[, "Estimate"][Coef.ind], 3),
    Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3),
    `P-value`=ifelse(Coef[, "Pr(>|z|)"][Coef.ind]<0.001, "<0.001", 
                     format(round2(Coef[, "Pr(>|z|)"][Coef.ind], 3), nsmall=3)),
    OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2),
                     " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ",
                     format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  )
  return(output)
}


#*****************
#
# Binomial_GLMM_CV
#
#*****************
# require(geepack)
# data("respiratory")
# Data=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# lambda=seq(0, 5, by=0.5)
# MSE=Binomial_GLMM_CV(data=Data,
#                      pred_vars,
#                      res_var,
#                      rand_var,
#                      vector.OF.classes.num.fact,
#                      levels.of.fact,
#                      k=4,
#                      lambda=lambda)
# # optimal lambda
# lambda[which.min(apply(MSE, 1, mean))]
Binomial_GLMM_CV=function(data, pred_vars, res_var, rand_var, vector.OF.classes.num.fact,
                          levels.of.fact, k=4, lambda=seq(0, 10, by=1)){
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr"), checkpackages)
  # convert data to data frame
  data=as.data.table(data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  missing_obs=unique(which(is.na(data[, 
                                      .SD, 
                                      .SDcols=c(res_var, pred_vars, rand_var)]), 
                           arr.ind=TRUE)[, 1])
  # 
  CV_data=data[!missing_obs, 
               .SD, 
               .SDcols=c(res_var, pred_vars, rand_var)]
  #***********************
  # convert variable types
  #***********************
  # response variable to numeric
  CV_data[, 
          (res_var):=lapply(.SD, function(x) as.numeric(as.character(x))), 
          .SDcols=res_var]
  # predictor variables
  for(i in 1:length(pred_vars)){
    #i=3
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      CV_data[, 
              (pred_vars[i]):=lapply(.SD, function(x) as.numeric(as.character(x))), 
              .SDcols=pred_vars[i]]
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      CV_data[, 
              (pred_vars[i]):=lapply(.SD, function(x) as.factor(as.character(x))), 
              .SDcols=pred_vars[i]]
      
      CV_data[, 
              (pred_vars[i]):=lapply(.SD, function(x) relevel(x, ref=levels.of.fact[i])),
              .SDcols=pred_vars[i]]
    }
  }
  #***
  # CV
  #***
  # specify GLMM model (before creating dummies)
  original.model=as.formula(paste(res_var, "~", paste(pred_vars, collapse="+"), sep=""))
  # generate design matrix (with dummies created for categorical variable with more than 2 levels)
  CV_data_X=model.matrix(original.model, CV_data) %>% 
    as.data.table()
  CV_data_X_names=colnames(CV_data_X[, -1])
  # response variable
  CV_data_y=CV_data[, .SD, .SDcols=res_var]
  # standardize predictor variables
  CV_data_X[, (CV_data_X_names):=lapply(.SD, function(x) scale(x, center=TRUE, scale=TRUE)), 
            .SDcols=CV_data_X_names]
  # grouping variable
  CV_data[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
  CV_data_ID=CV_data[, .SD, .SDcols=rand_var]
  # generate array containing fold-number for each sample (row)
  pass.ind=1
  while(sum(pass.ind)>0){
    folds=sample(rep_len(1:k, nrow(CV_data_y)), 
                   nrow(CV_data_y))
    for(k.ind in 1:k){
      #k.ind=1
      # actual split of the CV_data
      fold=which(folds == k.ind)
      
      # divide data into training and test sets
      CV_data_train=cbind(CV_data_y[-fold,], CV_data_ID[-fold,], CV_data_X[-fold,])
      
      if(sum((CV_data_train[, .SD, .SDcols=CV_data_X_names] %>% 
              lapply(function(x) length(unique(x))) %>% 
              unlist)==1)>0){
        
        print(which((CV_data_train[, .SD, .SDcols=CV_data_X_names] %>% 
                       lapply(function(x) length(unique(x))) %>% 
                       unlist)==1))
        #print(paste0("Re-diving data"))
        pass.ind[k.ind]=1
      }else{
        pass.ind[k.ind]=0
        #print(paste0("Divided data into train and test"))
      }
    }  
  }
  # speicfy GLMM model (with dummies)
  CV.model=as.formula(paste(res_var, "~", paste(CV_data_X_names, collapse="+"), sep=""))
  # generate empty matrix to save MSE
  MSE=matrix(NA, length(lambda), k)
  rownames(MSE)=c(paste0("lambda=", lambda))
  colnames(MSE)=c(paste0(1:k, "nd sub"))
  # run algorithm
  for(lambda.ind in 1:length(lambda)){
    # actual cross validation
    for(k.ind in 1:k) {
      # actual split of the CV_data
      fold=which(folds == k.ind)
      # divide data into training and test sets
      CV_data_train=cbind(CV_data_y[-fold,], CV_data_ID[-fold,], CV_data_X[-fold,])
      CV_data_test=cbind(CV_data_y[fold,], CV_data_ID[fold,], CV_data_X[fold,])
      # train and test your model with CV_data.train and CV_data.test
      # random effect
      random_effect=list(id=~1)
      names(random_effect)=rand_var
      ## fit adjacent category model
      glmmLasso.fit=glmmLasso(CV.model,
                                 rnd=random_effect, 
                                 family=binomial(link=logit), 
                                 data=CV_data_train, 
                                 lambda=lambda[lambda.ind],
                                 switch.NR=TRUE)
      # Make predictions and compute the R2, RMSE and MAE
      predictions=glmmLasso.fit %>% predict(CV_data_test)
      # MSE
      MSE[lambda.ind, k.ind]=mean(unlist(predictions - CV_data_test[, .SD, .SDcol=res_var])^2)
      if(k.ind == k){
        # print process
        print(paste0("k : ", k.ind, ", lambda : ", lambda[lambda.ind]))
      }
    }
  }
  return(MSE)
}


#***********
#
# GLMM_LASSO
#
#***********
# require(geepack)
# data("respiratory")
# Data=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# GLMM.LASSO.fit=GLMM_LASSO(data=Data,
#                      pred_vars,
#                      res_var,
#                      rand_var,
#                      vector.OF.classes.num.fact,
#                      levels.of.fact,
#                      lambda=10)
# summary(GLMM.LASSO.fit)
GLMM_LASSO=function(data, pred_vars, res_var, rand_var, vector.OF.classes.num.fact,
                          levels.of.fact, lambda=10){
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr"), checkpackages)
  # convert data to data frame
  data=as.data.table(data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  missing_obs=unique(which(is.na(data[, 
                                      .SD, 
                                      .SDcols=c(res_var, pred_vars, rand_var)]), 
                           arr.ind=TRUE)[, 1])
  # 
  data=data[!missing_obs, 
               .SD, 
               .SDcols=c(res_var, pred_vars, rand_var)]
  #***********************
  # convert variable types
  #***********************
  # response variable to numeric
  data[, 
          (res_var):=lapply(.SD, function(x) as.numeric(as.character(x))), 
          .SDcols=res_var]
  # predictor variables
  for(i in 1:length(pred_vars)){
    #i=3
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      data[, 
              (pred_vars[i]):=lapply(.SD, function(x) as.numeric(as.character(x))), 
              .SDcols=pred_vars[i]]
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      data[, 
              (pred_vars[i]):=lapply(.SD, function(x) as.factor(as.character(x))), 
              .SDcols=pred_vars[i]]
      
      data[, 
              (pred_vars[i]):=lapply(.SD, function(x) relevel(x, ref=levels.of.fact[i])),
              .SDcols=pred_vars[i]]
    }
  }
  # grouping variable
  data[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
  
  #*************
  # run algoritm
  #*************
  # specify GLMM model (before creating dummies)
  model=as.formula(paste(res_var, "~", paste(pred_vars, collapse="+"), sep=""))
  # random effect
  random_effect=list(id=~1)
  names(random_effect)=rand_var
  #
  glmmLasso.fit=glmmLasso(model,
                          rnd=random_effect, 
                          family=binomial(link=logit), 
                          data=data, 
                          lambda=lambda,
                          switch.NR=TRUE)
  
  return(glmmLasso.fit)
}



#*****************************************************
#
# [ Multiple Imputation Analytic Result Combine ] ----
#
#*************************
# Combine_Multiple_Results
#*************************
# Combine the results of analysis from multiple imputed data sets.
# The following is based on p.5 and p.6 in 'AMELIA II : A Program for Missing Data'
#**********************************************************************************
# Input :  A vector of data names
# Output : A combined result
# Result saved in each data name must be in the same size in a specific format where and row represents each variable (or its level) and 
# there are 5 columns, each of which represents (1) variable name, (2) estimate of coefficient, (3) Std error, (4) p.value, and (5) OR and CI.
#****************
# Example
#********
# lapply(c("geepack", "dplyr", "data.table", "Amelia"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Outcome_name="outcome"
# ID_name="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[,ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA",length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# # generate 100 missing data in outcome
# Data=as.data.table(Data)
# Data[sample(1:nrow(Data), 100), "outcome"]=NA
# # amelia
# set.seed(2019) # set seed in order to obtain the same imputed values for the future reproduction of analysis results
# m=2
# amelia.imp=amelia(Data[,
#                              .SD,
#                              .SDcols=c(ColumnsToUse, Outcome_name)],
#                   p2s=0,
#                   Mvalue="NA",
#                   m=m,
#                   noms=c(ColumnsToUse[vector.OF.classes.num.fact=="fact"], "outcome"), # all time-varying variates with missing data are nominal, so we need to specify them
#                   ts=c("visit"), # column number or variable name indicating the variable identifying time in time series data
#                   cs=c("id"), # column number or variable name indicating the cross section variable
#                   idvars=c(), # identification variable (variables that have no information except labels)
#                   polytime=1, # 0 under the assumption of no time effect
#                   interacs=FALSE # TRUE if time effects of polytime vary across the cross-section
# )
# GEE.result.1=GEE_Multivariable_Jin(amelia.imp$imputations$imp1, ColumnsToUse, Outcome_name, ID_name, which.family,
#                       vector.OF.classes.num.fact, levels.of.fact) %>% as.data.table(keep.rownames=TRUE)
# GEE.result.2=GEE_Multivariable_Jin(amelia.imp$imputations$imp2, ColumnsToUse, Outcome_name, ID_name, which.family,
#                       vector.OF.classes.num.fact, levels.of.fact) %>% as.data.table(keep.rownames=TRUE)
# # combine results
# GEE.combined.result=Combine_Multiple_Results(Input_Data_Names=c("GEE.result.1", "GEE.result.2"))
# GEE.combined.result
Combine_Multiple_Results=function(Input_Data_Names){
  library(data.table)
  library(magrittr)
  library(dplyr)
  Estimate_rbind=c()
  Std.Error_rbind=c()
  
  for(m.ind in 1:length(Input_Data_Names)){
    #m.ind=1
    Estimate_rbind=rbind(Estimate_rbind, get(paste0(Input_Data_Names[m.ind]))[, Estimate])
    Std.Error_rbind=rbind(Std.Error_rbind, get(paste0(Input_Data_Names[m.ind]))[, Std.Error])
  }
  # conver the results to the data table format
  Estimate_rbind %<>% as.data.table
  Std.Error_rbind %<>% as.data.table
  # name columns
  colnames(Estimate_rbind)=colnames(Std.Error_rbind)=get(paste0(Input_Data_Names[m.ind]))[, rn]
  # mean of parameter coefficient estimated
  est.coef.mean=sapply(Estimate_rbind, mean)
  # mean of parameter variance estimated
  est.var.mean=sapply(Std.Error_rbind^2, mean)
  # variance of parameter estimates
  par.var=sapply(Estimate_rbind, var)
  # compute P.value
  P.value=2*(1-pnorm(abs((est.coef.mean)/sqrt(est.var.mean+par.var*(1+1/m)))))
  # compute parameters
  output=data.table(
    rn=get(paste0(Input_Data_Names[m.ind]))[, rn],
    Estimate=round(est.coef.mean, 3), 
    Std.Error=round(sqrt(est.var.mean+par.var*(1+1/m)), 3), # standard error of the multiple imputation point estimate
    P.value=ifelse(P.value<0.001, "<0.001", 
                   format(round2(P.value, 3), nsmall=3)) # assuming that the sampling distribution of statistic is normal distribution
  )
  # compute OR and 95% CI
  output[, 
         OR.and.CI:=paste0(round(exp(Estimate), 2),
                           " (", round(exp(output[, Estimate]-qnorm(0.975)*output[, Std.Error]), 2), 
                           " - ", 
                           round(exp(output[, Estimate]+qnorm(0.975)*output[, Std.Error]), 2), ")")
         ]
  
  return(output)
}


#***************************
#
# [ Contribution Plot ] ----
#
#***************************
# Contribution plot
#******************
# Draw a contribution plot that displays the extent of contribution of predictor variables on the linear association with response variables,
# which is decomposed from the multivariate generalization of the squared Pearson correlation coefficient, called the RV cofficient.
# The contribution plot with shrinkage estimation can reveal truly-associated explanatory variables.
#****************************************************************************************************
# Note : Due to the nature of the Pearson correlation, all measured variables must be numeric.
#********
# Example
#********
# library(mvtnorm)
# n=100 # sample size
# p=4   # number of predictors
# q=2   # number of responses
# sigma=diag(1, p+q) # covariance matrix
# sigma[4, 2]=sigma[2, 4]=0.7
# D=rmvnorm(n=n, mean=rep(0,p+q), sigma=sigma) # data generated
# Contribution_plot(X=D[, 3:6], Y=D[, 1:2], alpha= c(1:5), nrep=500)
Contribution_plot=function(X, Y, alpha=1, level=0.95, nrep=100){
  # Input: Data matrices X and Y, the power alpha, significance level, and the number of
  # permutation replicates for the permutation test.
  # Output: The threshold.
  #
  # import aSPC
  library(aSPC)
  # output
  output=c()
  # run aSPC
  aSPC.result=aSPC(X, Y, pow=2*alpha, B=nrep)
  # optimal alpha
  output$opt.alpha=alpha[which.min(aSPC.result[names(aSPC.result)!="aSPC"])]
  # aSPC result
  output$aSPC=aSPC.result[names(aSPC.result)=="aSPC"]
  # Estimated contributions
  output$Est.Contribution=data.frame(Contribution=EstContribution(X, Y, alpha=output$opt.alpha))
  # Call to plot() to plot the contributions and threshold.
  plot(output$Est.Contribution$Contribution, type='l', main="X and Y (alpha=1)",
       xlab="Explanatory Variables", ylab="Contribution",
       cex=1.3, cex.lab=1.3, cex.axis=1.3, cex.main=2.5, cex.sub=1.3)
  # Calculate threshold
  output$threshold=Threshold(X, Y, alpha=output$opt.alpha, level=level, nrep=nrep)
  # Draw the threshold in the plot
  abline(a=output$threshold, b=0, col="blue")
  return(output)
}
EstContribution=function(X, Y, alpha=1){
  # Input: Data matrices X and Y, and the power alpha
  # Output: A vector of contribution of each explanatory variable
  # to the RV(X,Y| \alpha) statistic
  #
  # 1. Generate a matrix of powered covariances between columns of X and Y
  Cov=(cov(X,Y)^(2))^alpha
  # 2. For each explanatory variable, sum the powered correlations.
  Contr=apply(Cov, 1, sum)
  return(Contr)
}
Threshold=function(X, Y, alpha=1, level=0.95, nrep=100, p2s=0){
  # Input: Data matrices X and Y, the power alpha and the number of
  # permutation replicates for the permutation test.
  # Output: The threshold.
  #
  # Initialize a vector to hold max contribution for each permutation
  maxs=rep(NA,nrep)
  for(i in 1:nrep){
    # record all the maximum contributions based under the estimated
    # permutation distribution
    maxs[i]=max(EstContribution(X[sample(1:nrow(X)),], Y, alpha=alpha))
    
    if(p2s==0){
      # no print process
    }else{
      if(i%%(0.25*nrep)==0){ # print the process at every 25%
        print(paste0("<Obtaining Threshold> ", i/nrep*100, "% done"))
      }}
    
  }
  # obtain the threshold value
  return(quantile(maxs, level))
}


#******************************************************
#
# [ Markov chain Monte Carlo (MCMC) for sampling ] ----
#
#******************************************************
# Multivariate random-walk Metropolis sampling
#*********************************************
# Run multivariate random-walk Metro-polis sampling
# (I think this is just a regular Metropolis-Hastings sampling for sampling multiple variables)
#********
# Example
#********
# # Define arguments
# # x is a vector
# ring2D=function(x){exp(-5*abs(x[1]^2+x[2]^2-1))}  
# vcov2D=.01*diag(2)
# target=ring2D
# N=400
# x=c(0,0)
# VCOV=vcov2D
# # Call the sampling function with the arguments
# ringsample2D=rwmetro(ring2D,4000,c(0,0),vcov2D)
# # Use the sample
# plot(ringsample2D[,1], ringsample2D[,2], xlim=c(-1.5,1.5),ylim=c(-1.5,1.5), main='Metropolis-Hastings Sample',xlab='x', ylab='y', pch='.')
rwmetro=function(target,N,x,VCOV,burnin=0)
{
  require(MASS)   #requires package MASS for normal sampling
  samples=x
  for (i in 2:(burnin+N))
  {
    prop=mvrnorm(n=1, x, VCOV)
    if (runif(1) < min(1, target(prop)/target(x)))
      x=prop
    samples=rbind(samples,x)
  }
  samples[(burnin+1):(N+burnin),]
}


