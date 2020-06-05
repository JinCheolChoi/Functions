#***************************************
#
# [ --- Operational functions --- ] ----
#
#***************************************
# package check
#**************
# Example
#********
# lapply(c("geepack"), checkpackages)
checkpackages=function(package){
  # Checking the Availability of packages 
  # Installs them.  
  # Example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package, character.only=T)
}

#********
# rounds2
#********
round2=function(x, n){
  posneg=sign(x)
  z=abs(x)*10^n
  z=z+0.5
  z=trunc(z)
  z=z/10^n
  z*posneg
}

#***************
# elapsed_months
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

#***********************************
# see related vignettes of a package
#***********************************
#browseVignettes(package="simr")

#********************
# remove missing data
#********************
Remove_missing=function(Data, Columns){
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Columns)])
  return(Data)
}

#***************
# Format_Columns
#***************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data=Format_Columns(Data,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data
Format_Columns=function(Data, Res_Var, Pred_Vars, vector.OF.classes.num.fact, levels.of.fact){
  # as data frame
  Data=as.data.frame(Data)
  
  # convert variable class
  for(i in 1:length(Res_Var)){
    Data[, Res_Var[i]]=as.numeric(as.character(Data[, Res_Var[i]])) # response variable
  }
  
  for(i in 1:length(Pred_Vars)){
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, Pred_Vars[i]]=as.numeric(as.character(Data[, Pred_Vars[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, Pred_Vars[i]]=as.factor(Data[, Pred_Vars[i]])
      Data[, Pred_Vars[i]]=relevel(Data[, Pred_Vars[i]], ref=levels.of.fact[i])
    }
  }
  
  return(Data)
}

#*********************
#
# [ --- GLM --- ] ----
#
#*********************
# GLM_Bivariate
#**************
# lapply(c("stats", "geepack"), checkpackages)
# require(dplyr)
# data("respiratory")
# Data_to_use=respiratory %>%
#   group_by(id) %>%
#   filter(visit==min(visit))
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline")
# Res_Var="outcome"
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use$sex[sample(1:nrow(Data_to_use), 50)]="N"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLM_Bivariate(Data_to_use,
#               Pred_Vars,
#               Res_Var=Res_Var,
#               which.family="binomial")
GLM_Bivariate=function(Data, Pred_Vars, Res_Var, which.family){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    #i=1
    # run model
    Temp=GLM_Multivariable(Data=Data,
                           Pred_Vars=Pred_Vars[i],
                           Res_Var=Res_Var,
                           which.family=which.family)
    Output=rbind(Output,
                 cbind(Temp$summ_table,
                       Data_Used=Temp$N_data_used))
    
    # print progress
    print(paste0(Pred_Vars[i], " (", i, "/", length(Pred_Vars), ")"))
  }
  return(Output)
}

#******************
# GLM_Multivariable
#******************
# lapply(c("stats", "geepack"), checkpackages)
# require(dplyr)
# data("respiratory")
# Data_to_use=respiratory %>%
#   group_by(id) %>%
#   filter(visit==min(visit))
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline")
# Res_Var="outcome"
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use$sex[sample(1:nrow(Data_to_use), 50)]="N"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLM_Multivariable(Data=Data_to_use,
#                   Pred_Vars,
#                   Res_Var=Res_Var,
#                   which.family="gaussian")
GLM_Multivariable=function(Data, Pred_Vars, Res_Var, which.family){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # main algorithm
  Output=c()
  #i=1
  # run model
  fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+")))
  model_fit=glm(fullmod, family=which.family, na.action=na.exclude, data=Data)
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  
  # vif
  if(length(Pred_Vars)>=2){Output$vif=car::vif(model_fit)}else{Output$vif=""}
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  OR.CI=exp(cbind(OR=coef(model_fit), confint(model_fit, level=0.95)))
  colnames(OR.CI)=c("Odds Ratio", "Lower RR", "Upper RR")
  est=cbind(summary(model_fit)$coefficients, OR.CI)
  
  # Output
  if(which.family=="gaussian"){
    Output$summ_table=rbind(Output$summ_table,
                            data.frame(
                              Estimate=round2(est[-1, "Estimate"], 3),
                              Std.Error=round2(est[-1, "Std. Error"], 3),
                              `P-value`=ifelse(est[-1, "Pr(>|t|)"]<0.001, "<0.001",
                                               format(round2(est[-1, "Pr(>|t|)"], 3), nsmall=3)),
                              Estimate.and.CI=paste0(format(round2(est[-1, "Estimate"] , 2), nsmall=2),
                                                     " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ",
                                                     format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"),
                              row.names=names(coef(model_fit))[-1]
                            )
    )
  }else{
    Output$summ_table=rbind(Output$summ_table,
                            data.frame(
                              Estimate=round2(est[-1, "Estimate"], 3),
                              Std.Error=round2(est[-1, "Std. Error"], 3),
                              `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001",
                                               format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
                              OR.and.CI=paste0(format(round2(est[-1, "Odds Ratio"] , 2), nsmall=2),
                                               " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ",
                                               format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"),
                              row.names=names(coef(model_fit))[-1]
                            )
    )
  }
  return(Output)
}




#****************************************
#
# [ --- Descriptive Statistics --- ] ----
#
#****************************************
#
# Contingency_Table_Generator
#
#****************************
# Generate summary (contingency) table that outlines characteristics from dataset
# Row_Var : Non-continuous predictor variable
# Col_Var : Categorical response variable
#********
# Example
#****************
# lapply(c("dplyr",
#          "data.table",
# 
#          "lme4",
#          "epitools"
# ),
# checkpackages)
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# # randomly generate NAs in some variables
# Data_to_use$sex[sample(1:nrow(Data_to_use), 30)]=NA
# 
# Data_to_use$age[sample(1:nrow(Data_to_use), 30)]=NA
# Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]=NA
# #Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]=2
# 
# # Work on predictor with more than 2 levels
# Data_to_use=as.data.table(Data_to_use)
# Data_to_use[sample(nrow(Data_to_use), 50), sex:="N"]
# 
# Data_to_use[age<20, age_cat:="<20"]
# Data_to_use[20<=age & age<30, age_cat:="20<=age<30"]
# Data_to_use[30<=age & age<40, age_cat:="30<=age<40"]
# Data_to_use[40<=age & age<50, age_cat:="40<=age<50"]
# Data_to_use[50<=age & age<60, age_cat:="50<=age<60"]
# Data_to_use[60<=age, age_cat:="60<=age"]
# Data_to_use[, age_cat:=as.factor(age_cat)]
# 
# # Data at baseline
# BL_Data=Data_to_use %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Contingency_Table_Generator(Data=Data_to_use,
#                             Row_Var="sex",
#                             Col_Var="outcome",
#                             Ref_of_Row_Var="F",
#                             Missing="Include",
#                             Ind_P_Value=T)
Contingency_Table_Generator=function(Data, Row_Var, Col_Var, Ref_of_Row_Var, Missing="Not_Include", Ind_P_Value=F){
  # library
  library(epitools)
  
  # Data as data table
  Data=as.data.frame(Data)
  # 
  Col_Order=c()
  if(is.numeric(Data[, Col_Var])==T){
    Col_Order=sort(unique(Data[, Col_Var]))
  }else if(is.factor(Data[, Col_Var])){
    Col_Order=levels(Data[, Col_Var])
  }
  Data[, Col_Var]=as.character(Data[, Col_Var])
  Data[, Row_Var]=as.character(Data[, Row_Var])
  
  if(Missing=="Include"){
    Data[is.na(Data[, Col_Var]), Col_Var]="NA"
    Data[is.na(Data[, Row_Var]), Row_Var]="NA"
  }else if(Missing=="Not_Include"){
  }else(print("Options for Missing : (1) Not_Include (Default), or (2) Include"))
  
  #
  if(length(Col_Order)>0){
    Data[, Col_Var]=factor(Data[, Col_Var], levels=c(Col_Order))
  }else if(length(Col_Order)==0){
    Data[, Col_Var]=as.factor(Data[, Col_Var])
  }
  Data[, Row_Var]=as.factor(Data[, Row_Var])
  Data[, Row_Var]=relevel(Data[, Row_Var], ref=Ref_of_Row_Var)
  
  # Contingency Table
  Contingency_Table=Data %>% 
    dplyr::select(Row_Var, Col_Var) %>% 
    table(useNA="no")
  
  # Sum of values column-wise INCLUDING missing data in Row_Var
  Sum_Col_Wise=Data %>% 
    dplyr::select(Col_Var) %>% 
    table(useNA="no") %>% c
  
  # Sum of values row-wise EXCLUDING missing data in Col_Var
  Sum_Row_Wise=apply(Contingency_Table, 1, sum)
  
  # merge all results
  Merged=cbind(Row_Var, rownames(Contingency_Table), 
               cbind(
                 paste0(Contingency_Table, " (", round(t(t(Contingency_Table)/Sum_Col_Wise)*100, 2), "%)") %>% 
                   matrix(nrow(Contingency_Table), ncol(Contingency_Table)), 
                 paste0(Sum_Row_Wise, " (", round(Sum_Row_Wise/sum(Sum_Col_Wise)*100, 2), "%)")
               )
  ) %>% as.data.frame()
  
  # post-processing
  colnames(Merged)[1:2]=c("Predictor", "Value")
  colnames(Merged)[3:(3+ncol(Contingency_Table)-1)]=paste0(Col_Var, "=", colnames(Contingency_Table), " (n=", Sum_Col_Wise, ")")
  colnames(Merged)[3+ncol(Contingency_Table)]=paste0("Total (n=", sum(Sum_Col_Wise), ")")
  Out=Merged
  
  # calculate OR given a response variable of two levels
  if(ncol(Contingency_Table)==2){
    # compute odds ratio
    Odds_ratio=Contingency_Table %>% 
      oddsratio(method="wald")
    Odds_ratio_row=cbind(paste0(round(Odds_ratio$measure[, 1], 2), 
                                " (", 
                                round(Odds_ratio$measure[, 2], 2), 
                                " - ", 
                                round(Odds_ratio$measure[, 3], 2), 
                                ")"), 
                         ifelse(Odds_ratio$p.value[, "fisher.exact"]<0.001, "<0.001", round(Odds_ratio$p.value[, "fisher.exact"], 3)), 
                         ifelse(Odds_ratio$p.value[, "chi.square"]<0.001, "<0.001", round(Odds_ratio$p.value[, "chi.square"], 3)))
    colnames(Odds_ratio_row)=c("OR (95% CI)", "P-value (Fisher)", "P-value (Chi-square)")
    
    Out=cbind(Merged, Odds_ratio_row)
    
    Out$`OR (95% CI)`=as.character(Out$`OR (95% CI)`)
    if(Ind_P_Value==F){
      Out$`P-value (Fisher)`=""
      Out$`P-value (Chi-square)`=""
    }
    # 
    Out=as.data.table(Out)
    Out[Value==Ref_of_Row_Var, c("OR (95% CI)")]=""
    
    # delete OR for NA
    Out[Value=="NA", c("OR (95% CI)")]=""
  }
  
  # 
  Out=as.data.table(Out)
  Out[Value==Ref_of_Row_Var, c("P-value (Fisher)")]=ifelse(fisher.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ], simulate.p.value=TRUE)$p.value<0.001,
                                                           "<0.001 (*Ind Test)",
                                                           paste0(round(fisher.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ], simulate.p.value=TRUE)$p.value, 3), " (*Ind Test)"))
  Out[Value==Ref_of_Row_Var, c("P-value (Chi-square)")]=ifelse(chisq.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ])$p.value<0.001,
                                                               "<0.001 (*Ind Test)",
                                                               paste0(round(chisq.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ])$p.value, 3), " (*Ind Test)"))  # return
  
  return(Out)
}



#************************************
#
# Contingency_Table_Generator_Conti_X
#
#************************************
# Generate summary (contingency) table that outlines characteristics from dataset
# Row_Var : Non-continuous predictor variable
# Col_Var : Categorical response variable
#********
# Example
#****************
# lapply(c("dplyr",
#          "data.table",
# 
#          "lme4",
#          "epitools",
#          "doBy" # for esticon function
# ),
# checkpackages)
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# # randomly generate NAs in some variables
# Data_to_use$age[sample(1:nrow(Data_to_use), 30)]=NA
# Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]=NA
# #Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]="2"
# 
# # Data at baseline
# BL_Data=Data_to_use %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Contingency_Table_Generator_Conti_X(Data=BL_Data,
#                                     Row_Var="age",
#                                     Col_Var="outcome",
#                                     Missing="Include")
# Contingency_Table_Generator_Conti_X(Data=BL_Data,
#                                     Row_Var="age",
#                                     Col_Var="outcome",
#                                     Missing="Not_Include")
Contingency_Table_Generator_Conti_X=function(Data, Row_Var, Col_Var, Ref_of_Row_Var, Missing="Not_Include"){
  # library
  library(doBy)
  
  # Data as data table
  Data=as.data.frame(Data)
  
  # If response variable is binary, perform GLM to compute P.value and OR.and.CI
  if(length(unique(Data[, Col_Var][!is.na(Data[, Col_Var])]))==2){
    #
    Levels=levels(as.factor(Data[, Col_Var]))
    Data[, Col_Var]=as.numeric(as.character(Data[, Col_Var]))
    Data[, Row_Var]=as.numeric(as.character(Data[, Row_Var]))
    #
    if(Missing=="Include"){
      useNA="always"
    }else if(Missing=="Not_Include"){
      useNA="no"
    }else(print("Options for Missing : (1) Not_Include (Default), or (2) Include"))
    
    # Sum of values column-wise INCLUDING missing data in Row_Var
    Sum_Col_Wise=as.data.frame(Data) %>% 
      dplyr::select(Col_Var) %>% 
      table(useNA=useNA) %>% c
    names(Sum_Col_Wise)=Levels
    
    GLM_Result=glm(as.formula(paste(Col_Var, "~", Row_Var)), data=na.omit(Data[, c(Row_Var, Col_Var)]), binomial(logit))
    est=esticon(GLM_Result, diag(length(coef(GLM_Result))))[-1, ]
    OR.and.CI=paste0(round(exp(est$estimate), 2), " (", round(exp(est$estimate-qnorm(0.975)*est$std.error), 2), " - ", round(exp(est$estimate+qnorm(0.975)*est$std.error), 2), ")")
    P.value=ifelse(est$estimate<0.001, "<0.001", round(est$estimate, 3)) 
    
    # compute P.values
    unique_outcome_value=unique(Data[, Col_Var])[!is.na(unique(Data[, Col_Var]))]
    Mann_Whitney_test=wilcox.test(Data[which(Data[, Col_Var]==unique_outcome_value[1]), Row_Var], 
                                  Data[which(Data[, Col_Var]==unique_outcome_value[2]), Row_Var])
    P.value_Mann_Whitney=ifelse(Mann_Whitney_test$p.value<0.001, "<0.001", round(Mann_Whitney_test$p.value, 3))
    T.test=t.test(Data[which(Data[, Col_Var]==unique_outcome_value[1]), Row_Var], 
                  Data[which(Data[, Col_Var]==unique_outcome_value[2]), Row_Var], alternative="two.sided")
    P.value_T_test=ifelse(T.test$p.value<0.001, "<0.001", round(T.test$p.value, 3))
    P.value_ANOVA="Y is binary"
  }else{
    #
    Data[, Col_Var]=as.factor(Data[, Col_Var])
    Data[, Row_Var]=as.numeric(Data[, Row_Var])
    #
    if(Missing=="Include"){
      useNA="always"
    }else if(Missing=="Not_Include"){
      useNA="no"
    }else(print("Options for Missing : (1) Not_Include (Default), or (2) Include"))
    
    # Sum of values column-wise INCLUDING missing data in Row_Var
    Sum_Col_Wise=as.data.frame(Data) %>% 
      dplyr::select(Col_Var) %>% 
      table(useNA=useNA) %>% c
    
    # compute P.values
    OR.and.CI="Y is not binary"
    P.value="Y is not binary"
    P.value_Mann_Whitney="Y is not binary"
    P.value_T_test="Y is not binary"
    
    ANOVA=summary(aov(as.formula(paste(Row_Var, "~", Col_Var)), data=Data, na.action=na.omit))
    P.value_ANOVA=ANOVA[[1]][["Pr(>F)"]][1]
    P.value_ANOVA=ifelse(P.value_ANOVA<0.001, "<0.001", round(P.value_ANOVA, 3))
    
  }
  
  #
  Data=as.data.table(Data)
  # summary statistics
  if(Missing=="Include"){
    Sum_Stat=round(t(rbind(
      with(Data, do.call(rbind, by(eval(parse(text=Row_Var)), eval(parse(text=Col_Var)), summary)))[, 1:6], # excluding NA's
      summary(Data[is.na(eval(parse(text=Col_Var))), eval(parse(text=Row_Var))])[1:6], # missing in outcome
      summary(as.data.frame(Data)[, Row_Var])[1:6] # total
    )), 2)
  }else if(Missing=="Not_Include"){
    Sum_Stat=round(t(rbind(
      with(Data, do.call(rbind, by(eval(parse(text=Row_Var)), eval(parse(text=Col_Var)), summary)))[, 1:6], # excluding NA's
      summary(as.data.frame(Data[!is.na(eval(parse(text=Col_Var))), ])[, Row_Var])[1:6] # total
    )), 2)
  }else(print("Options for Missing : (1) Not_Include (Default), or (2) Include"))
  
  # merge all results
  Out=c()
  Out=cbind(
    rbind(
      Sum_Stat
    ), 
    # GLM to compute P.value and OR.and.CI
    OR.and.CI, 
    P.value, 
    P.value_Mann_Whitney, 
    P.value_T_test,
    P.value_ANOVA
  )
  
  # post-processing
  Out=cbind(Row_Var, 
            c("Min", "Q1", "Median", "Mean", "Q3", "Max"), 
            Out) %>% as.data.frame
  Out$`OR.and.CI`=as.character(Out$`OR.and.CI`)
  Out$`P.value`=as.character(Out$`P.value`)
  Out$`P.value_Mann_Whitney`=as.character(Out$`P.value_Mann_Whitney`)
  Out$`P.value_T_test`=as.character(Out$`P.value_T_test`)
  Out$`P.value_ANOVA`=as.character(Out$`P.value_ANOVA`)
  Out[-1, c("OR.and.CI", "P.value", "P.value_Mann_Whitney", "P.value_T_test", "P.value_ANOVA")]=""
  colnames(Out)[1:2]=c("Predictor", "Value")
  
  #
  colnames(Out)[3:(3+length(Sum_Col_Wise)-1)]=paste0(Col_Var, "=", names(Sum_Col_Wise), " (n=", Sum_Col_Wise, ")")
  colnames(Out)[(3+length(Sum_Col_Wise))]=paste0("Total (n=", sum(Sum_Col_Wise), ")")
  colnames(Out)[(3+length(Sum_Col_Wise)+1):(3+length(Sum_Col_Wise)+5)]=c("OR (95% CI)", "P-value (GLM)", "P-value (Mann_Whitney)", "P-value (T_test)", "P-value (ANOVA)")
  
  # return
  return(Out)
}
