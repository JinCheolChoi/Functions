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
  z=z + 0.5
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
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data
Format_Columns=function(Data, Outcome_name, ColumnsToUse, vector.OF.classes.num.fact, levels.of.fact){
  # as data frame
  Data=as.data.frame(Data)
  
  # convert variable class
  for(i in 1:length(Outcome_name)){
    Data[, Outcome_name[i]]=as.numeric(as.character(Data[, Outcome_name[i]])) # response variable
  }
  
  for(i in 1:length(ColumnsToUse)){
    # convert variable class
    if(vector.OF.classes.num.fact[i]=="num"){
      Data[, ColumnsToUse[i]]=as.numeric(as.character(Data[, ColumnsToUse[i]]))
    }
    if(vector.OF.classes.num.fact[i]=="fact"){
      Data[, ColumnsToUse[i]]=as.factor(Data[, ColumnsToUse[i]])
      Data[, ColumnsToUse[i]]=relevel(Data[, ColumnsToUse[i]], ref=levels.of.fact[i])
    }
  }
  
  return(Data)
}

#**************************************************
#
# [ --- Interrupted Time Series Analysis --- ] ----
#
#**************************************************
# Segmented_Regression_Model
#***************************
# Example
#********
# int=85
# set.seed(42)
# df=data.table(
#   count=as.integer(rpois(132, 9) + rnorm(132, 1, 1)),
#   time=1:132,
#   at_risk=rep(
#     c(4305, 4251, 4478, 4535, 4758, 4843, 4893, 4673, 4522, 4454, 4351),
#     each =12
#   ),
#   month=rep(factor(month.name, levels=month.name), length=132)
# )
# df[, intv:=ifelse(time >= int, 1, 0)]
# df[, intv_trend:=c(rep(0, (int - 1)), 1:(length(unique(time)) - (int - 1)))]
# # Add a grouping variable manually
# df[, group:=ifelse(intv==1, "Intervention", "Control")]
# Segmented_Regression_Model(Data=df,
#                            Res_Var="count",
#                            Time_Var="time",
#                            Int_Var="intv")
# Segmented_Regression_Model_Plot(Data=df,
#                                 X_Var="time",
#                                 Y_Var="count",
#                                 X_Lab="Time",
#                                 Group_Var="group",
#                                 Y_Lab="Frequency")
Segmented_Regression_Model=function(Data, Res_Var, Time_Var, Int_Var){
  # 
  #Data=Long_Table
  Data=as.data.frame(Data)
  
  # 
  #Data=na.omit(Data[, c(Outcome_name, ColumnsToUse)])
  
  Data[, Res_Var]=as.numeric(as.character(Data[, Res_Var]))
  
  Data[, Time_Var]=as.factor(Data[, Time_Var])
  Data$Time_Order=as.numeric(Data[, Time_Var])
  
  # fit model
  model_fit=lm(as.formula(paste(Res_Var, " ~ ", Int_Var, "*Time_Order")), data=Data)
  
  # output
  output=c()
  output$model_fit=model_fit
  output$summ_table=as.data.frame(summary(model_fit)$coefficients)
  colnames(output$summ_table)=c("Estimate", "Std.Error", "T-value", "P-value")
  output$summ_table$Estimate=round(output$summ_table$Estimate, 3)
  output$summ_table$Std.Error=round(output$summ_table$Std.Error, 3)
  output$summ_table$`T-value`=round(output$summ_table$`T-value`, 3)
  output$summ_table$`P-value`=ifelse(output$summ_table$`P-value`<0.001, "<0.001", round(output$summ_table$`P-value`, 3)) 
  
  return(output)
}

#********************************
# Segmented_Regression_Model_Plot
#********************************
Segmented_Regression_Model_Plot=function(Data,
                                         X_Var,
                                         Y_Var,
                                         Group_Var,
                                         X_Lab="Time",
                                         Y_Lab="Frequency"){
  
  # plot
  Trend_Plot=ggplot(Data, aes(x=eval(parse(text=X_Var)), y=eval(parse(text=Y_Var)))) +
    geom_line()+
    geom_point()+
    geom_smooth(method="lm", se=T, aes(colour=eval(parse(text=Group_Var)))) +
    theme_bw()+
    xlab(X_Lab)+
    ylab(Y_Lab)+
    labs(colour="", size=16)+
    theme(axis.title.x=element_text(size=rel(1.8)),
          axis.title.y=element_text(size=rel(1.8)),
          axis.text.x=element_text(size=rel(1.8)),
          legend.text=element_text(size=rel(1.5)))
  
  Trend_Plot
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
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline")
# Outcome_name="outcome"
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use$sex[sample(1:nrow(Data_to_use), 50)]="N"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Outcome_name="outcome",
#                            ColumnsToUse,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLM_Bivariate(Data_to_use,
#               ColumnsToUse,
#               Outcome_name=Outcome_name,
#               which.family="binomial")
# GLM.fit=GLM_Multivariable(Data_to_use,
#                           ColumnsToUse,
#                           Outcome_name=Outcome_name,
#                           which.family="binomial")
# Confounder_Steps=GLM_Confounder_Selection(Full_Model=GLM.fit$model_fit,
#                                           Main_Pred_Var="sex",
#                                           Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"],
#                                           which.family="binomial", # distribution of the response variable
#                                           Min.Change.Percentage=5,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# Main_Pred_Var="sex"
# Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"]
# GLM_Confounder=GLM_Confounder_Model(Data<-Data_to_use,
#                                     Main_Pred_Var=Main_Pred_Var,
#                                     Potential_Con_Vars=ColumnsToUse[ColumnsToUse!=Main_Pred_Var],
#                                     Outcome_name="outcome",
#                                     which.family="binomial", # gaussian, binomial, poisson
#                                     Min.Change.Percentage=5,
#                                     Estimate="raw_estimate") # raw_estimate, converted_estimate
# GLM_Confounder$Full_Multivariable_Model$summ_table
# GLM_Confounder$Confounder_Steps$Confounders
# GLM_Confounder$Confounder_Model$summ_table
GLM_Bivariate=function(Data, ColumnsToUse, Outcome_name, which.family){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Outcome_name, ColumnsToUse)])
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # run model
    fullmod=as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i]))
    model.fit=glm(fullmod, family=which.family, na.action=na.exclude, data=Data)
    
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    OR.CI=exp(cbind(OR=coef(model.fit), confint(model.fit, level=0.95)))
    colnames(OR.CI)=c("Odds Ratio", "Lower RR", "Upper RR")
    est=cbind(summary(model.fit)$coefficients, OR.CI)
    
    # output
    output=rbind(output, 
                 data.frame(
                   Estimate=round2(est[-1, "Estimate"], 3), 
                   Std.Error=round2(est[-1, "Std. Error"], 3), 
                   `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001", 
                                    format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)), 
                   OR.and.CI=paste0(format(round2(est[-1, "Odds Ratio"] , 2), nsmall=2), 
                                    " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ", 
                                    format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"), 
                   row.names=names(coef(model.fit))[-1]
                 )
    )
    #print(paste0(i, " ", ColumnsToUse[i]))
  }
  return(output)
}

#******************
# GLM_Multivariable
#******************
GLM_Multivariable=function(Data, ColumnsToUse, Outcome_name, which.family){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Outcome_name, ColumnsToUse)])
  
  # main algorithm
  output=c()
  #i=1
  # run model
  fullmod=as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+")))
  model.fit=glm(fullmod, family=which.family, na.action=na.exclude, data=Data)
  output$model_fit=model.fit
  
  # vif
  if(length(ColumnsToUse)>=2){output$vif=car::vif(model.fit)}else{output$vif=""}
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  OR.CI=exp(cbind(OR=coef(model.fit), confint(model.fit, level=0.95)))
  colnames(OR.CI)=c("Odds Ratio", "Lower RR", "Upper RR")
  est=cbind(summary(model.fit)$coefficients, OR.CI)
  
  # output
  output$summ_table=rbind(output$summ_table, 
                          data.frame(
                            Estimate=round2(est[-1, "Estimate"], 3), 
                            Std.Error=round2(est[-1, "Std. Error"], 3), 
                            `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001", 
                                             format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)), 
                            OR.and.CI=paste0(format(round2(est[-1, "Odds Ratio"] , 2), nsmall=2), 
                                             " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ", 
                                             format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"), 
                            row.names=names(coef(model.fit))[-1]
                          )
  )
  
  return(output)
}

#*************************
# GLM_Confounder_Selection
#*************************
GLM_Confounder_Selection=function(Full_Model, 
                                  Main_Pred_Var, 
                                  Potential_Con_Vars, 
                                  which.family="binomial",
                                  Min.Change.Percentage=5,
                                  Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  
  # Full_Model=GLM.example$model_fit
  # Main_Pred_Var="sex"
  # Potential_Con_Vars=c("center", "treat", "age", "baseline", "visit")
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Potential_Con_Vars=Potential_Con_Vars
  Include_Index=c(1:length(Potential_Con_Vars))
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    Fixed_Effects_Current_Full_Model=Current_Full_Model$coefficients
    #Fixed_Effects_Current_Full_Model=fixef(Current_Full_Model)
    Main_Effects_Current_Full_Model=Fixed_Effects_Current_Full_Model[grep(Main_Pred_Var, names(Fixed_Effects_Current_Full_Model))]
    
    # [ IMPORTANT ] - When indep_var is a factor with more than two levels, we pick max coef of its levels.
    Main_Cov_Level=names(which.max(abs(Main_Effects_Current_Full_Model)))
    Main_Effect_Current_Full_Model=Main_Effects_Current_Full_Model[Main_Cov_Level]
    Main_Effect_Current_Reduced_Model=c()
    
    # run GLM excluding one variable at once
    for(i in 1:length(Potential_Con_Vars[Include_Index])){
      #i=1
      Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[Include_Index][i], collapse="-"))))
      Fixed_Effects_Current_Reduced_Model=Current_Reduced_Model$coefficients
      #Fixed_Effects_Current_Reduced_Model=fixef(Current_Reduced_Model)
      Main_Effect_Current_Reduced_Model[i]=Fixed_Effects_Current_Reduced_Model[Main_Cov_Level]
      
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Potential_Con_Vars[Include_Index])))
    }
    
    if(Estimate=="raw_estimate"){
      #**** refer to the raw coefficient estimate ****
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
        Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }else if(Estimate=="converted_estimate"){
      # summary table
      if(which.family=="gaussian"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="binomial"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="poisson"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_RR=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }
    }
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than 10, terminate the while loop
      loop.key=1
    }else{
      # decide the variable to remove
      Var_to_Remove=Temp_Table[Rank==min(Rank, na.rm=T), Removed_Var]
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Potential_Con_Vars==Var_to_Remove[1])]
      # update the current full model
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[setdiff(1:length(Potential_Con_Vars), Include_Index)], collapse="-"))))
      # increase step
      step=step+1
      
      # if there's no more variable left
      if(length(Include_Index)==0){
        Temp_Table=data.table(
          Removed_Var=c("Full", Potential_Con_Vars[Include_Index]),
          Estimate=c(fixef(Current_Full_Model)[-1]),
          Delta="",
          Rank=""
        )
        Out$summ_table[[step]]=Temp_Table
        loop.key=1
      }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Confounders=c(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var[-grep(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var)])
  
  return(Out)
}

#*********************
# GLM_Confounder_Model
#*********************
GLM_Confounder_Model=function(Data,
                              Main_Pred_Var,
                              Potential_Con_Vars,
                              Outcome_name,
                              which.family,
                              Min.Change.Percentage=5,
                              Estimate="raw_estimate"){
  Output=c()
  # Full multivariable model
  ColumnsToUse=c(Main_Pred_Var, Potential_Con_Vars)
  Output$Full_Multivariable_Model=GLM_Multivariable(Data,
                                                    ColumnsToUse=ColumnsToUse,
                                                    Outcome_name=Outcome_name,
                                                    which.family=which.family)
  
  # Confounder selection
  Confounder_Steps=GLM_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                            Main_Pred_Var=Main_Pred_Var,
                                            Potential_Con_Vars=ColumnsToUse[ColumnsToUse!=Main_Pred_Var],
                                            which.family=which.family, # distribution of the response variable
                                            Min.Change.Percentage=Min.Change.Percentage,
                                            Estimate=Estimate) # raw_estimate, converted_estimate
  
  # save all stepwise procedure
  Output$Confounder_Steps=Confounder_Steps
  
  # index of confounders
  Confounder_Ind=which(ColumnsToUse%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=GLM_Multivariable(Data,
                                            ColumnsToUse=ColumnsToUse[Confounder_Ind],
                                            Outcome_name=Outcome_name,
                                            which.family=which.family)
  return(Output)
}

#******************************
# GLM_NB_Bivariate_Personal_Jin
#******************************
# Run simple GLM models for each explanatory variable with negative binomial distribution 
# to account for zero-inflated data (i.e. the excessive number of 0s).
#********
# Example
#*****************
# lapply(c("geepack"), checkpackages)
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
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="count_outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GLM_NB_Bivariate_Personal_Jin(Data, ColumnsToUse, Outcome_name,
#                               Offset_name)
GLM_NB_Bivariate_Personal_Jin=function(Data, ColumnsToUse, Outcome_name, Offset_name){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    # run model
    fullmod=as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i], "+offset(log(", Offset_name, "))"))
    model.fit=glm.nb(fullmod, data=Data)
    
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    RR.CI=exp(cbind(RR=coef(model.fit), confint(model.fit, level=0.95)))
    colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
    est=cbind(summary(model.fit)$coefficients, RR.CI)
    
    # output
    output=rbind(output, 
                 data.frame(
                   Estimate=round2(est[-1, "Estimate"], 3), 
                   Std.Error=round2(est[-1, "Std. Error"], 3), 
                   `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001", 
                                    format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)), 
                   RR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] , 2), nsmall=2), 
                                    " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ", 
                                    format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"), 
                   row.names=names(coef(model.fit))[-1]
                 )
    )
    #print(paste0(i, " ", ColumnsToUse[i]))
  }
  return(output)
}

#**************************
# GLM_NB_Multi_Personal_Jin
#**************************
# Run a multiple GLM model with negative binomial distribution to account for zero-inflated data
# (i.e. the excessive number of 0s).
#********
# Example
#*****************
# lapply(c("geepack"), checkpackages)
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
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="count_outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GLM_NB_Multi_Personal_Jin(Data, ColumnsToUse, Outcome_name,
#                           Offset_name)
GLM_NB_Multi_Personal_Jin=function(Data, ColumnsToUse, Outcome_name, Offset_name){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # run model
  fullmod=as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+"), "+offset(log(", Offset_name, "))"))
  model.fit=glm.nb(fullmod, data=Data)
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  RR.CI=exp(cbind(RR=coef(model.fit), confint(model.fit, level=0.95)))
  colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
  est=cbind(summary(model.fit)$coefficients, RR.CI)
  
  # output
  output=data.frame(Estimate=round2(est[-1, "Estimate"], 3), 
                    Std.Error=round2(est[-1, "Std. Error"], 3), 
                    `P-value`=ifelse(round2(est[-1, "Pr(>|z|)"], 3)<0.001, "<0.001", 
                                     format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)), 
                    RR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] , 2), nsmall=2), 
                                     " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ", 
                                     format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"), 
                    row.names=names(coef(model.fit))[-1]
  )
  return(output)
}

#*******************
# GLM_Bivariate_Plot
#*******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# head(Data)
# GLM_Bivariate_Plot(Data=Data,
#                    Pred_Var="age",
#                    Res_Var="outcome",
#                    which.family="binomial",
#                    xlab="age",
#                    ylab="outcome",
#                    title="Title",
#                    x_breaks=seq(round(min(Data$age)-5, -1), round(max(Data$age)+5, -1), by=10))
GLM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, which.family, xlab="", ylab="", title="", x_breaks=0){
  # output
  output=c()
  
  # change data type & remove missing data 
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Res_Var, Pred_Var)])
  
  # fit GLMM
  fullmod=as.formula(paste(Res_Var, "~", Pred_Var, sep=""))
  Model=glm(fullmod, family=which.family, na.action=na.exclude, data=Data)
  output$glm=Model
  
  # prediction
  newdat=expand.grid(Pred_Var=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=50))
  colnames(newdat)=c(Pred_Var)
  fit=predict(Model, newdat, type="response", se.fit=T)
  newdat$fit=fit$fit
  colnames(newdat)=c(Pred_Var, Res_Var)
  
  # confidence interval
  newdat$plo=fit$fit-qnorm(0.975)*fit$se.fit
  newdat$pho=fit$fit+qnorm(0.975)*fit$se.fit
  
  if(which.family=="binomial"){ # code for 'binomial' family
    newdat[newdat$plo<0, "plo"]=0
    newdat[newdat$pho>1, "pho"]=1
    
    colnames(newdat)=c(Pred_Var, Res_Var, "CI_Lower", "CI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    output$plot=ggplot(data=newdat, aes(x=eval(parse(text=Pred_Var)),                          
                                        y=eval(parse(text=Res_Var))
    )) + 
      geom_ribbon(data=newdat, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10) +
      geom_hline(yintercept=0) +
      geom_hline(yintercept=1) + 
      scale_y_continuous(labels=scales::percent) + 
      scale_x_continuous(breaks=x_breaks) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.8, col="blue") + 
      theme_bw() + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }else if(which.family=="poisson"){ # code for 'poisson' family
    newdat[newdat$plo<0, "plo"]=0
    
    colnames(newdat)=c(Pred_Var, Res_Var, "CI_Lower", "CI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    output$plot=ggplot(data=newdat, aes(x=eval(parse(text=Pred_Var)),                          
                                        y=eval(parse(text=Res_Var))
    )) + 
      geom_ribbon(data=newdat, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10) +
      geom_hline(yintercept=0) +
      scale_x_continuous(breaks=x_breaks) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.8, col="blue") + 
      theme_bw() + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }
  
  
  return(output)
}


#*********************
#
# [ --- GEE --- ] ----
#
#*********************
# GEE_Bivariate_Jin
#******************
# Example
#******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GEE_Bivariate_Jin(Data,
#                   ColumnsToUse=ColumnsToUse,
#                   Outcome_name="outcome",
#                   ID_name="id",
#                   which.family="binomial")
GEE_Bivariate_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family="binomial"){
  # check out packages
  lapply(c("geepack", "MESS", "doBy"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # convert response variable to numeric
  Data[, Outcome_name]=as.numeric(as.character(Data[, Outcome_name]))
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    
    # not consider data with missing value
    target.indx=which(is.na(Data[, ColumnsToUse[i]])==F) 
    
    # run model
    GEE.m=geeglm(as.formula(paste(Outcome_name, "~", ColumnsToUse[i])), 
                 id=Data[target.indx, ID_name], data=Data[target.indx, c(Outcome_name, ColumnsToUse)], family=which.family, corstr="exchangeable")
    
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    est=esticon(GEE.m, diag(length(coef(GEE.m))))[-1, ]
    
    # output
    if(which.family=="gaussian"){
      output=rbind(output, 
                   data.frame(
                     Estimate=round2(est$estimate, 3), 
                     Std.Error=round2(est$std.error, 3), 
                     `P-value`=ifelse(est$p.value<0.001, "<0.001", 
                                      format(round2(est$p.value, 3), nsmall=3)), 
                     Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                            " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                            format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                     row.names=names(coef(GEE.m))[-1]
                   )
      )
    }else if(which.family=="binomial"){
      output=rbind(output, 
                   data.frame(
                     Estimate=round2(est$estimate, 3), 
                     Std.Error=round2(est$std.error, 3), 
                     `P-value`=ifelse(est$p.value<0.001, "<0.001", 
                                      format(round2(est$p.value, 3), nsmall=3)), 
                     OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                      " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                      format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                     row.names=names(coef(GEE.m))[-1]
                   )
      )
    }else if(which.family=="poisson"){
      output=rbind(output, 
                   data.frame(
                     Estimate=round2(est$estimate, 3), 
                     Std.Error=round2(est$std.error, 3), 
                     `P-value`=ifelse(est$p.value<0.001, "<0.001", 
                                      format(round2(est$p.value, 3), nsmall=3)), 
                     RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                      " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                      format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                     row.names=names(coef(GEE.m))[-1]
                   )
      )
    }
    
    #print(paste0(i, " ", ColumnsToUse[i]))
  }
  return(output)
}

#***********************
# GEE_Multivariable_Jin
#***********************
# Example
#*****************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# # generate missing data
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # run GEE_Multivariable_Jin
# GEE_Multivariable_Jin(Data,
#                       ColumnsToUse=ColumnsToUse,
#                       Outcome_name="outcome",
#                       ID_name="id",
#                       which.family="binomial")
GEE_Multivariable_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy", "HH"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # delete data with missing value
  Data=na.omit(Data[, c(ColumnsToUse, ID_name, Outcome_name)])
  
  # run model
  #fullmod=as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+")))
  GEE.m=geeglm(as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+"))), 
               data=Data[, c(ColumnsToUse, ID_name, Outcome_name)], 
               id=Data[, ID_name], 
               family=which.family, 
               corstr="exchangeable")
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(GEE.m, diag(length(coef(GEE.m))))[-1, ]
  
  # output
  output=c()
  output$model_fit=GEE.m
  #output$vif=HH::vif(GEE.m)
  
  if(which.family=="gaussian"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                                        " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                                        format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
  }else if(which.family=="binomial"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
    
  }else if(which.family=="poisson"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
  }
  return(output)
  
}

#*******************************
# GEE_Multivariable_with_vif_Jin
#*******************************
# Example
#**************************************************************************
# Note : 1. Input data must not have missing data at variables of interest!
#        2. Argument must be declared with '<-' in a function for HH::vif!
#**************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_original=respiratory
# # generate missing data
# Data_original$outcome[1:5]=NA
# Data_original$treat[c(3, 7, 35, 74)]=NA
# Data_original$id[c(6, 25, 45, 98)]=NA
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_original[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # run GEE_Multivariable_with_vif_Jin
# GEE_Multivariable_with_vif_Jin(Remove_missing(Data_original, # remove missing data
#                                               c(ColumnsToUse<-ColumnsToUse,
#                                                 Outcome_name<-"outcome",
#                                                 ID_name<-"id")),
#                                ColumnsToUse<-ColumnsToUse,
#                                Outcome_name<-Outcome_name,
#                                ID_name<-ID_name,
#                                which.family<-"binomial")
GEE_Multivariable_with_vif_Jin=function(Data, ColumnsToUse, Outcome_name, ID_name, which.family){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy", "HH"), checkpackages)
  
  # as data frame
  Data<<-as.data.frame(Data)
  
  # run model
  #fullmod=as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+")))
  GEE.m=geeglm(as.formula(paste(Outcome_name, " ~ ", paste(ColumnsToUse, collapse="+"))), 
               data=Data[, c(ColumnsToUse, ID_name, Outcome_name)], 
               id=Data[, ID_name], 
               family=which.family, 
               corstr="exchangeable")
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(GEE.m, diag(length(coef(GEE.m))))[-1, ]
  
  # output
  output=c()
  output$model_fit=GEE.m
  
  if(length(ColumnsToUse)>=2){output$vif=HH::vif(GEE.m)}
  if(length(ColumnsToUse)==1 & !is.numeric(Data[, ColumnsToUse])){output$vif=HH::vif(GEE.m)} # if the only variable is not numeric (that's, if it is categorical), compute vif
  
  if(which.family=="gaussian"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                                        " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                                        format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
  }else if(which.family=="binomial"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
  }else if(which.family=="poisson"){
    output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(GEE.m))[-1]
    )
  }
  return(output)
  
}

#*************************
# GEE_Confounder_Selection
#*************************
# Example
#******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# 
# Data$sex=as.character(Data$sex)
# Data[sample(nrow(Data), 30), "sex"]="N"
# Data[sample(nrow(Data), 30), "sex"]="P"
# Data$sex=as.factor(Data$sex)
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GEE.fit=GEE_Multivariable_with_vif_Jin(Remove_missing(Data, # remove missing data
#                                                       c(ColumnsToUse<-ColumnsToUse,
#                                                         Outcome_name<-"outcome",
#                                                         ID_name<-"id")),
#                                        ColumnsToUse<-ColumnsToUse,
#                                        Outcome_name<-Outcome_name,
#                                        ID_name<-ID_name,
#                                        which.family<-"binomial")
# Confounder_Steps=GEE_Confounder_Selection(Full_Model=GEE.fit$model_fit,
#                                           Main_Pred_Var="sex",
#                                           Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"],
#                                           which.family="binomial", # distribution of the response variable
#                                           Min.Change.Percentage=30,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Ind=which(ColumnsToUse%in%Confounder_Steps$Confounders)
# GEE.confound.fit=GEE_Multivariable_with_vif_Jin(Remove_missing(Data, # remove missing data
#                                                                c(ColumnsToUse<-ColumnsToUse,
#                                                                  Outcome_name<-"outcome",
#                                                                  ID_name<-"id")),
#                                                 ColumnsToUse<-ColumnsToUse[Confounder_Ind],
#                                                 Outcome_name<-Outcome_name,
#                                                 ID_name<-ID_name,
#                                                 which.family<-"binomial")
# GEE.fit$summ_table
# GEE.confound.fit$summ_table
GEE_Confounder_Selection=function(Full_Model, 
                                  Main_Pred_Var, 
                                  Potential_Con_Vars, 
                                  which.family="binomial",
                                  Min.Change.Percentage=5,
                                  Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  
  # Full_Model=GEE.example$model_fit
  # Main_Pred_Var="sex"
  # Potential_Con_Vars=c("center", "treat", "age", "baseline", "visit")
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Potential_Con_Vars=Potential_Con_Vars
  Include_Index=c(1:length(Potential_Con_Vars))
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    Fixed_Effects_Current_Full_Model=coef(Current_Full_Model)
    Main_Effects_Current_Full_Model=Fixed_Effects_Current_Full_Model[grep(Main_Pred_Var, names(Fixed_Effects_Current_Full_Model))]
    
    # when indep_var is factor, we pick max coef of its levels
    Main_Cov_Level=names(which.max(abs(Main_Effects_Current_Full_Model)))
    Main_Effect_Current_Full_Model=Main_Effects_Current_Full_Model[Main_Cov_Level]
    Main_Effect_Current_Reduced_Model=c()
    
    # run GEE excluding one variable at once
    for(i in 1:length(Potential_Con_Vars[Include_Index])){
      #i=1
      Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[Include_Index][i], collapse="-"))))
      Fixed_Effects_Current_Reduced_Model=coef(Current_Reduced_Model)
      Main_Effect_Current_Reduced_Model[i]=Fixed_Effects_Current_Reduced_Model[Main_Cov_Level]
      
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Potential_Con_Vars[Include_Index])))
    }
    
    if(Estimate=="raw_estimate"){
      #**** refer to the raw coefficient estimate ****
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
        Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }else if(Estimate=="converted_estimate"){
      # summary table
      if(which.family=="gaussian"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="binomial"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="poisson"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_RR=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }
    }
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than 10, terminate the while loop
      loop.key=1
      
    }else{
      # decide the variable to remove
      Var_to_Remove=Temp_Table[Rank==min(Rank, na.rm=T), Removed_Var]
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Potential_Con_Vars==Var_to_Remove)]
      # update the current full model
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[setdiff(1:length(Potential_Con_Vars), Include_Index)], collapse="-"))))
      # increase step
      step=step+1
      
      # if there's no more variable left
      if(length(Include_Index)==0){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", names(Current_Full_Model$coefficients)[-1], ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(coef(Current_Full_Model)[-1]),
          Delta="",
          Rank=""
        )
        Out$summ_table[[step]]=Temp_Table
        loop.key=1
      }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Confounders=c(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var[-grep(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var)])
  
  return(Out)
}

#**********************
# GEE_Confounder_Model
#**********************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Outcome_name="outcome",
#                            ColumnsToUse,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# Main_Pred_Var="sex"
# Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"]
# GEE_Confounder=GEE_Confounder_Model(Data=Data_to_use,
#                                     Main_Pred_Var<-Main_Pred_Var,
#                                     Potential_Con_Vars<-Potential_Con_Vars,
#                                     Outcome_name<-"outcome",
#                                     ID_name<-"id",
#                                     which.family<-"gaussian", # gaussian, binomial, poisson
#                                     Min.Change.Percentage=15,
#                                     Estimate="raw_estimate") # raw_estimate, converted_estimate
# GEE_Confounder$Full_Multivariable_Model$summ_table
# GEE_Confounder$Confounder_Steps$Confounders
# GEE_Confounder$Confounder_Model$summ_table
GEE_Confounder_Model=function(Input_Data,
                              Main_Pred_Var,
                              Potential_Con_Vars,
                              Outcome_name,
                              ID_name,
                              which.family,
                              Min.Change.Percentage=5,
                              Estimate="raw_estimate"){
  
  Output=c()
  
  # Full multivariable model
  ColumnsToUse=c(Main_Pred_Var, Potential_Con_Vars)
  
  Output$Full_Multivariable_Model=GEE_Multivariable_with_vif_Jin(Data=Remove_missing(Input_Data, # remove missing data
                                                                                     c(ColumnsToUse,
                                                                                       Outcome_name,
                                                                                       ID_name)),
                                                                 ColumnsToUse<<-ColumnsToUse,
                                                                 Outcome_name<<-Outcome_name,
                                                                 ID_name<<-ID_name,
                                                                 which.family<<-which.family)
  
  # Confounder selection
  Confounder_Steps=GEE_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                            Main_Pred_Var=Main_Pred_Var,
                                            Potential_Con_Vars=ColumnsToUse[ColumnsToUse!=Main_Pred_Var],
                                            which.family=which.family, # distribution of the response variable
                                            Min.Change.Percentage=Min.Change.Percentage,
                                            Estimate=Estimate) # raw_estimate, converted_estimate
  Output$Confounder_Steps=Confounder_Steps
  Confounder_Ind=which(ColumnsToUse%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=GEE_Multivariable_with_vif_Jin(Data=Remove_missing(Input_Data, # remove missing data
                                                                             c(ColumnsToUse[Confounder_Ind],
                                                                               Outcome_name,
                                                                               ID_name)),
                                                         ColumnsToUse<<-ColumnsToUse[Confounder_Ind],
                                                         Outcome_name<<-Outcome_name,
                                                         ID_name<<-ID_name,
                                                         which.family<<-which.family)
  return(Output)
}



#**********************
#
# [ --- GLMM --- ] ----
#
#**********************
# GLMM_Bivariate
#***************
# Example
#************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Bivariate(Data,
#                    ColumnsToUse,
#                    Outcome_name="outcome",
#                    ID_name="id",
#                    which.family<-"binomial", # gaussian, binomial, poisson
#                    NAGQ<-1,
#                    Compute.Power=T, # power can be computed for a non-gaussian distribution
#                    nsim=5)
GLMM_Bivariate=function(Data,
                        ColumnsToUse,
                        Outcome_name,
                        ID_name,
                        which.family,
                        NAGQ=100,
                        Compute.Power=FALSE,
                        nsim=1000){
  # check out packages
  lapply(c("lme4"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    #i=1
    # run model
    #fullmod=as.formula(paste(Outcome_name, "~", ColumnsToUse[i], "+(1|", ID_name, ")", sep=""))
    if(which.family=="gaussian"){
      myfit=lmer(as.formula(paste(Outcome_name, "~", ColumnsToUse[i], "+(1|", ID_name, ")", sep="")), 
                 na.action=na.exclude, 
                 data=Data, 
                 control=lmerControl(optimizer=c("bobyqa"))) # the other optimizer : "Nelder_Mead"
    }else{
      myfit=glmer(as.formula(paste(Outcome_name, "~", ColumnsToUse[i], "+(1|", ID_name, ")", sep="")), 
                  family=which.family, 
                  na.action=na.exclude, 
                  data=Data, 
                  nAGQ=NAGQ, 
                  control=glmerControl(optimizer=c("bobyqa"))) # the other optimizer : "Nelder_Mead"
    }
    
    # coefficient
    Coef=summary(myfit)$coefficients
    Coef.ind=which(grepl(ColumnsToUse[i], row.names(Coef)))
    # confidence interval (exponentiated)
    CI.raw=confint(myfit, level=0.95, method="Wald")
    CI.raw.ind=which(grepl(ColumnsToUse[i], row.names(CI.raw)))
    CI=exp(CI.raw)
    CI.ind=which(grepl(ColumnsToUse[i], row.names(CI)))
    # power
    if(Compute.Power==T){Var.Power=powerSim(myfit, fixed(ColumnsToUse[i], "lr"), nsim=nsim, progress=F)}
    # output
    temp_out=c()
    temp_out$Estimate=round2(Coef[, "Estimate"][Coef.ind], 3)
    temp_out$Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3)
    temp_out$`P-value`=ifelse(Coef[, ncol(Coef)][Coef.ind]<0.001, "<0.001", 
                              format(round2(Coef[, ncol(Coef)][Coef.ind], 3), nsmall=3))
    if(which.family=="gaussian"){
      temp_out$Estimate.and.CI=paste0(format(round2(Coef[, "Estimate"][Coef.ind], 2), nsmall=2), 
                                      " (", format(round2(CI.raw[CI.raw.ind, 1], 2), nsmall=2), " - ", 
                                      format(round2(CI.raw[CI.raw.ind, 2], 2), nsmall=2), ")")
    }else if(which.family=="binomial"){
      temp_out$OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
    }else if(which.family=="poisson"){
      temp_out$RR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
    }
    # power
    if(Compute.Power==T){
      # import the package
      lapply(c("simr"), checkpackages)
      temp_out$power=paste0(
        paste0(round(summary(Var.Power)["mean"]*100, 2), "%"),
        " (",
        round(summary(Var.Power)["lower"]*100, 2),
        ", ",
        round(summary(Var.Power)["upper"]*100, 2),
        ")"
      )
    }
    
    output=rbind(output, data.frame(temp_out))
    #print(paste(i, " ", ColumnsToUse[i], sep=""))
  }
  output=as.data.frame(output)
  return(output)
}


#***********************
# GLMM_Multivariable
#***********************
# Example
#******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Multivariable(Data,
#                        ColumnsToUse,
#                        Outcome_name="outcome",
#                        ID_name="id",
#                        which.family<-"binomial", # gaussian, binomial, poisson
#                        NAGQ<-1,
#                        Compute.Power=F, # power can be computed for a non-gaussian distribution
#                        nsim=5)
GLMM_Multivariable=function(Data,
                            ColumnsToUse,
                            Outcome_name,
                            ID_name, which.family,
                            NAGQ=100,
                            Compute.Power=FALSE,
                            nsim=1000){
  # check out packages
  lapply(c("lme4", "simr", "sjPlot"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # run model
  #fullmod=as.formula(paste(Outcome_name, "~", paste(ColumnsToUse, collapse="+"), "+(1|", ID_name, ")", sep=""))
  if(which.family=="gaussian"){
    myfit=lmer(as.formula(paste(Outcome_name, "~", paste(ColumnsToUse, collapse="+"), "+(1|", ID_name, ")", sep="")), 
               na.action=na.exclude, 
               data=Data, 
               control=lmerControl(optimizer=c("bobyqa"), optCtrl=list(maxfun=1e7)))
  }else{
    myfit=glmer(as.formula(paste(Outcome_name, "~", paste(ColumnsToUse, collapse="+"), "+(1|", ID_name, ")", sep="")), 
                family=which.family, 
                na.action=na.exclude, 
                data=Data, nAGQ=NAGQ, 
                control=glmerControl(optimizer=c("bobyqa"), optCtrl=list(maxfun=1e7))) # try "bobyqa" or "Nelder_Mead" if the algorithm fails to converge.
  }
  
  # coefficient
  Coef=summary(myfit)$coefficients
  Coef.ind=c()
  # confidence interval (raw)
  CI.raw=confint(myfit, level=0.95, method="Wald")
  CI.raw.ind=c()
  # confidence interval (exponentiated)
  CI=exp(confint(myfit, level=0.95, method="Wald"))
  CI.ind=c()
  # power
  Var.Power=list()
  for(i in 1:length(ColumnsToUse)){
    Coef.ind=c(Coef.ind, which(grepl(ColumnsToUse[i], row.names(Coef))))
    CI.raw.ind=c(CI.raw.ind, which(grepl(ColumnsToUse[i], row.names(CI.raw))))
    CI.ind=c(CI.ind, which(grepl(ColumnsToUse[i], row.names(CI))))
    if(Compute.Power==T){
      lapply(c("simr"), checkpackages)
      Var.Power[[i]]=powerSim(myfit, fixed(ColumnsToUse[i], "lr"), nsim=nsim, progress=F)}
  }
  
  Coef.ind=sort(unique(Coef.ind))
  CI.raw.ind=sort(unique(CI.raw.ind))
  CI.ind=sort(unique(CI.ind))
  #*******
  # output
  #*******
  output=c()
  # random effect plot (exponentiated)
  output$re_plot=plot_model(myfit, type="re")
  # info of model fit
  output$model_fit=myfit
  # vif
  if(length(ColumnsToUse)>=2){output$vif=car::vif(myfit)}else{output$vif=""}
  
  # summary table
  output$summ_table$Estimate=round2(Coef[, "Estimate"][Coef.ind], 3)
  output$summ_table$Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3)
  output$summ_table$`P-value`=ifelse(Coef[, ncol(Coef)][Coef.ind]<0.001, "<0.001", 
                                     format(round2(Coef[, ncol(Coef)][Coef.ind], 3), nsmall=3))
  if(which.family=="gaussian"){
    output$summ_table$Estimate.and.CI=paste0(format(round2(Coef[, "Estimate"][Coef.ind], 2), nsmall=2), 
                                             " (", format(round2(CI.raw[CI.raw.ind, 1], 2), nsmall=2), " - ", 
                                             format(round2(CI.raw[CI.ind, 2], 2), nsmall=2), ")")
  }else if(which.family=="binomial"){
    output$summ_table$OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }else if(which.family=="poisson"){
    output$summ_table$RR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }
  # power
  if(Compute.Power==T){
    output$summ_table$power=sapply(Var.Power, function(x) paste0(
      paste0(round(summary(x)["mean"]*100, 2), "%"),
      " (",
      round(summary(x)["lower"]*100, 2),
      ", ",
      round(summary(x)["upper"]*100, 2),
      ")"
    ))
  }
  output$summ_table=as.data.frame(output$summ_table)
  return(output)
}

#***************************
# GLMM_Multinomial_Bivariate
#***************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Multinomial_Bivariate_Format_1(Data,
#                                     ColumnsToUse,
#                                     Outcome_name="outcome",
#                                     ID_name="id")
# GLMM_Multinomial_Bivariate_Format_2(Data,
#                                     ColumnsToUse,
#                                     Outcome_name="outcome",
#                                     ID_name="id")
#************************************
# GLMM_Multinomial_Bivariate_Format_1
GLMM_Multinomial_Bivariate_Format_1=function(Data,
                                             ColumnsToUse,
                                             Outcome_name,
                                             ID_name,
                                             k=2,
                                             maxit=500,
                                             tol=1e-04){
  # check out packages
  lapply(c("mixcat"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Outcome_name]=as.factor(Data[, Outcome_name])
  Y_Levels=levels(Data[, Outcome_name])
  Y=factor(Data[, Outcome_name], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, ID_name]
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    #i=1
    X=Data[, ColumnsToUse[i]]
    
    # run algorithm until convergence
    while_trs=0
    count=0
    temp_k=k
    temp_maxit=maxit
    temp_tol=tol
    while(while_trs==0){
      count=count+1
      
      # run model
      myfit=npmlt(Y~X,
                  formula.npo=~X,
                  random=~1,
                  id=ID,
                  k=temp_k,
                  link="blogit", # specify that the model is a baseline logit random effects model
                  EB=T,
                  maxit=temp_maxit,
                  tol=temp_tol)
      
      #
      while_trs=1
      if(is.na(myfit$coefficients[1])){
        # temp_k=temp_k+ifelse(sample(c(0,1), 1)==0, 1, -1)
        # if(temp_k==1){temp_k=2}else if(temp_k>=length(ColumnsToUse)-1){temp_k=2}
        print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Fail to converge. Try k=", temp_k))
        while_trs=0 # re-run algorithm
        Sys.sleep(0.5)
      }else{
        if(myfit$flagcvm>0 | myfit$flaginfo>0){
          print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Reduce 'tol' from ", temp_tol, " to ", temp_tol/100))
          temp_tol=temp_tol/100
          while_trs=0 # re-run algorithm
        }
        if(myfit$iter==myfit$maxit){
          print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Increase 'maxit' from ", temp_maxit, " to ", temp_maxit+10000))
          temp_maxit=temp_maxit+10000
          while_trs=0 # re-run algorithm
        }
      }
    }
    print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Algorithm converged (k=", temp_k, ", maxit=", temp_maxit, ", tol=", temp_tol, ")"))
    
    # coefficient
    Coef=myfit$coefficients
    Coef.ind=which(grepl("X", row.names(Coef)))
    # standard error
    SE.Coef=myfit$SE.coefficients
    # confidence interval (exponentiated)
    Raw_Upper_Bound=Coef[Coef.ind]+qnorm(0.975)*SE.Coef[Coef.ind]
    Raw_Lower_Bound=Coef[Coef.ind]-qnorm(0.975)*SE.Coef[Coef.ind]
    # p-values
    Z_value=Coef/SE.Coef # Wald test statistic
    P_values=(1-pnorm(abs(Z_value), 0, 1))*2
    # CI (upper and lower bounds)
    Upper_Bound=exp(Raw_Upper_Bound)
    Lower_Bound=exp(Raw_Lower_Bound)
    
    # output
    temp_out=c()
    temp_out$Estimate=round2(Coef[Coef.ind], 3)
    temp_out$Std.Error=round2(SE.Coef[Coef.ind], 3)
    temp_out$`P-value`=ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                              format(round2(P_values[Coef.ind], 3), nsmall=3))
    temp_out$OR.and.CI=paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
                              " (",
                              format(round(Lower_Bound, 2), nsmall=2),
                              " - ",
                              format(round(Upper_Bound, 2), nsmall=2),
                              ")")
    
    #
    temp_out=data.frame(temp_out)
    
    if(is.factor(X)){
      X_Levels=levels(X)
      Temp_Name=expand.grid(Y_Levels[-1], X_Levels[-1])
      rownames(temp_out)=paste0(ColumnsToUse[i], " ",Temp_Name[, 2], " / ", Temp_Name[, 1])
    }else if(is.numeric(X)){
      Temp_Name=expand.grid(Y_Levels[-1], ColumnsToUse[i])
      rownames(temp_out)=paste0(Temp_Name[, 2], " / ", Temp_Name[, 1])
    }
    
    output=rbind(output, temp_out)
    #print(paste(i, " ", ColumnsToUse[i], sep=""))
  }
  return(output)
}

#************************************
# GLMM_Multinomial_Bivariate_Format_2
GLMM_Multinomial_Bivariate_Format_2=function(Data,
                                             ColumnsToUse,
                                             Outcome_name,
                                             ID_name,
                                             k=2,
                                             maxit=500,
                                             tol=1e-04){
  # check out packages
  lapply(c("mixcat"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Outcome_name]=as.factor(Data[, Outcome_name])
  Y_Levels=levels(Data[, Outcome_name])
  Y=factor(Data[, Outcome_name], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, ID_name]
  
  # main algorithm
  output=c()
  for(i in 1:length(ColumnsToUse)){
    #i=17
    X=Data[, ColumnsToUse[i]]
    
    # run algorithm until convergence
    while_trs=0
    count=0
    temp_k=k
    temp_maxit=maxit
    temp_tol=tol
    while(while_trs==0){
      count=count+1
      
      # run model
      myfit=npmlt(Y~X,
                  formula.npo=~X,
                  random=~1,
                  id=ID,
                  k=temp_k,
                  link="blogit", # specify that the model is a baseline logit random effects model
                  EB=FALSE,
                  maxit=temp_maxit,
                  tol=temp_tol)
      
      #
      while_trs=1
      if(is.na(myfit$coefficients[1])){
        temp_k=temp_k+ifelse(sample(c(0,1), 1)==0, 1, -1)
        if(temp_k==1){temp_k=2}else if(temp_k>=length(ColumnsToUse)-1){temp_k=2}
        print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Fail to converge. Try k=", temp_k))
        while_trs=0 # re-run algorithm
        Sys.sleep(0.5)
      }else{
        if(myfit$flagcvm>0 | myfit$flaginfo>0){
          print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Reduce 'tol' from ", temp_tol, " to ", temp_tol/100))
          temp_tol=temp_tol/100
          while_trs=0 # re-run algorithm
        }
        if(myfit$iter==myfit$maxit){
          print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Increase 'maxit' from ", temp_maxit, " to ", temp_maxit+10000))
          temp_maxit=temp_maxit+10000
          while_trs=0 # re-run algorithm
        }
      }
    }
    print(paste0("[ ", ColumnsToUse[i], " : ", count, "th run ] - Algorithm converged (k=", temp_k, ", maxit=", temp_maxit, ", tol=", temp_tol, ")"))
    
    # coefficient
    Coef=myfit$coefficients
    Coef.ind=which(grepl("X", row.names(Coef)))
    # standard error
    SE.Coef=myfit$SE.coefficients
    # confidence interval (exponentiated)
    Raw_Upper_Bound=Coef[Coef.ind]+qnorm(0.975)*SE.Coef[Coef.ind]
    Raw_Lower_Bound=Coef[Coef.ind]-qnorm(0.975)*SE.Coef[Coef.ind]
    # p-values
    Z_value=Coef/SE.Coef # Wald test statistic
    P_values=(1-pnorm(abs(Z_value), 0, 1))*2
    # CI (upper and lower bounds)
    Upper_Bound=exp(Raw_Upper_Bound)
    Lower_Bound=exp(Raw_Lower_Bound)
    
    # output
    temp_out=c()
    
    # record 
    if(is.factor(X)){
      temp_out$Estimate=matrix(round2(Coef[Coef.ind], 3), 
                               ncol=length(levels(X))-1, 
                               nrow=length(levels(Y))-1)
      temp_out$Std.Error=matrix(round2(SE.Coef[Coef.ind], 3), 
                                ncol=length(levels(X))-1, 
                                nrow=length(levels(Y))-1)
      temp_out$`P-value`=matrix(
        ifelse(P_values[Coef.ind]<0.001, "<0.001", 
               format(round2(P_values[Coef.ind], 3), nsmall=3)), 
        ncol=length(levels(X))-1, 
        nrow=length(levels(Y))-1)
      temp_out$OR.and.CI=matrix(
        paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
               " (",
               format(round(Lower_Bound, 2), nsmall=2),
               " - ",
               format(round(Upper_Bound, 2), nsmall=2),
               ")"), 
        ncol=length(levels(X))-1, 
        nrow=length(levels(Y))-1)
      # names for row and column
      X_Levels=levels(X)
      Temp_Row_Names=Y_Levels[-1]
      Temp_Column_Names=paste0(ColumnsToUse[i], " / ", X_Levels[-1])
    }else if(is.numeric(X)){
      temp_out$Estimate=data.frame(X=round2(Coef[Coef.ind], 3))
      temp_out$Std.Error=data.frame(X=round2(SE.Coef[Coef.ind], 3))
      temp_out$`P-value`=data.frame(
        X=ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                 format(round2(P_values[Coef.ind], 3), nsmall=3))
      )
      temp_out$OR.and.CI=data.frame(
        X=paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
                 " (",
                 format(round(Lower_Bound, 2), nsmall=2),
                 " - ",
                 format(round(Upper_Bound, 2), nsmall=2),
                 ")")
      )
      # names for row and column
      Temp_Row_Names=Y_Levels[-1]
      Temp_Column_Names=ColumnsToUse[i]
    }
    
    rownames(temp_out$Estimate)=Temp_Row_Names
    colnames(temp_out$Estimate)=Temp_Column_Names
    rownames(temp_out$Std.Error)=Temp_Row_Names
    colnames(temp_out$Std.Error)=Temp_Column_Names
    rownames(temp_out$`P-value`)=Temp_Row_Names
    colnames(temp_out$`P-value`)=Temp_Column_Names
    rownames(temp_out$OR.and.CI)=Temp_Row_Names
    colnames(temp_out$OR.and.CI)=Temp_Column_Names
    
    output$Estimate=rbind(output$Estimate, t(temp_out$Estimate))
    output$Std.Error=rbind(output$Std.Error, t(temp_out$Std.Error))
    output$`P-value`=rbind(output$`P-value`,t(temp_out$`P-value`))
    output$OR.and.CI=rbind(output$OR.and.CI, t(temp_out$OR.and.CI))
    #print(paste(i, " ", ColumnsToUse[i], sep=""))
  }
  return(output)
}

#******************************
# GLMM_Multinomial_Multivariate
#******************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Multinomial_Multivariate_Format_1(Data,
#                                        ColumnsToUse,
#                                        Outcome_name="outcome",
#                                        ID_name="id")
# GLMM_Multinomial_Multivariate_Format_2(Data,
#                                        ColumnsToUse,
#                                        Outcome_name="outcome",
#                                        ID_name="id",
#                                        maxit=10,
#                                        par.update=T)
#***************************************
# GLMM_Multinomial_Multivariate_Format_1
GLMM_Multinomial_Multivariate_Format_1=function(Data,
                                                ColumnsToUse,
                                                Outcome_name,
                                                ID_name,
                                                k=2,
                                                maxit=500,
                                                tol=1e-04){
  # check out packages
  lapply(c("mixcat"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Outcome_name]=as.factor(Data[, Outcome_name])
  Y_Levels=levels(Data[, Outcome_name])
  Y=factor(Data[, Outcome_name], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, ID_name]
  
  # assign predictors to values
  for(i in 1:length(ColumnsToUse)){
    assign(paste0("X_", i), Data[, ColumnsToUse[i]])
  }
  
  # run algorithm until convergence
  while_trs=0
  count=0
  while(while_trs==0){
    count=count+1
    
    # run model
    myfit=npmlt(formula(paste0("Y~", paste0("X_", c(1:length(ColumnsToUse)), collapse="+"))),
                formula.npo=formula(paste0("~", paste0("X_", c(1:length(ColumnsToUse)), collapse="+"))),
                random=~1,
                id=ID,
                k=k,
                link="blogit", # specify that the model is a baseline logit random effects model
                EB=FALSE,
                maxit=maxit,
                tol=tol)
    
    #
    while_trs=1
    if(is.na(myfit$coefficients[1])){
      k=k+ifelse(sample(c(0,1), 1)==0, 1, -1)
      if(k==1){k=2}else if(k>=length(ColumnsToUse)-1){k=2}
      print(paste0("[ ", count, "th run ] - Fail to converge. Try k=", k))
      while_trs=0 # re-run algorithm
      Sys.sleep(0.5)
    }else{
      if(myfit$flagcvm>0 | myfit$flaginfo>0){
        print(paste0("[ ", count, "th run ] - Reduce 'tol' from ", tol, " to ", tol/100))
        tol=tol/100
        while_trs=0 # re-run algorithm
      }
      if(myfit$iter==myfit$maxit){
        print(paste0("[ ", count, "th run ] - Increase 'maxit' from ", maxit, " to ", maxit+10000))
        maxit=maxit+10000
        while_trs=0 # re-run algorithm
      }
    }
  }
  print(paste0("[ ", count, "th run ] - Algorithm converged (k=", k, ", maxit=", maxit, ", tol=", tol, ")"))
  
  
  # coefficient
  Coef=myfit$coefficients
  Coef.ind=which(grepl("X_", row.names(Coef)))
  # standard error
  SE.Coef=myfit$SE.coefficients
  # confidence interval (exponentiated)
  Raw_Upper_Bound=Coef[Coef.ind]+qnorm(0.975)*SE.Coef[Coef.ind]
  Raw_Lower_Bound=Coef[Coef.ind]-qnorm(0.975)*SE.Coef[Coef.ind]
  # p-values
  Z_value=Coef/SE.Coef # Wald test statistic
  P_values=(1-pnorm(abs(Z_value), 0, 1))*2
  # CI (upper and lower bounds)
  Upper_Bound=exp(Raw_Upper_Bound)
  Lower_Bound=exp(Raw_Lower_Bound)
  
  # output
  output=c()
  output$Estimate=round2(Coef[Coef.ind], 3)
  output$Std.Error=round2(SE.Coef[Coef.ind], 3)
  output$`P-value`=ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                          format(round2(P_values[Coef.ind], 3), nsmall=3))
  output$OR.and.CI=paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
                          " (",
                          format(round(Lower_Bound, 2), nsmall=2),
                          " - ",
                          format(round(Upper_Bound, 2), nsmall=2),
                          ")")
  output=data.frame(output)
  
  # name output rows
  row_names=c()
  for(i in 1:length(ColumnsToUse)){
    X=get(paste0("X_", i))
    if(is.factor(X)){
      X_Levels=levels(X)
      Temp_Name=expand.grid(Y_Levels[-1], X_Levels[-1])
      row_names=c(row_names, paste0(ColumnsToUse[i], " ", Temp_Name[, 2], " / ", Temp_Name[, 1]))
    }else if(is.numeric(X)){
      Temp_Name=expand.grid(Y_Levels[-1], ColumnsToUse[i])
      row_names=c(row_names, paste0(Temp_Name[, 2], " / ", Temp_Name[, 1]))
    }
  }
  rownames(output)=row_names
  
  #
  output_to_return=c()
  output_to_return$converged.set=data.table(count=count, k=k, maxit=maxit, tol=tol)
  output_to_return$summ_table=output
  # return output
  return(output_to_return)
}

#***************************************
# GLMM_Multinomial_Multivariate_Format_2
GLMM_Multinomial_Multivariate_Format_2=function(Data,
                                                ColumnsToUse,
                                                Outcome_name,
                                                ID_name,
                                                k=2,
                                                maxit=500,
                                                tol=1e-04,
                                                par.update=FALSE){
  # check out packages
  lapply(c("mixcat"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Outcome_name]=as.factor(Data[, Outcome_name])
  Y_Levels=levels(Data[, Outcome_name])
  Y=factor(Data[, Outcome_name], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, ID_name]
  
  # assign predictors to values
  for(i in 1:length(ColumnsToUse)){
    assign(paste0("X_", i), Data[, ColumnsToUse[i]])
  }
  
  # run algorithm until convergence
  while_trs=0
  count=0
  while(while_trs==0){
    count=count+1
    
    # run model
    myfit=npmlt(formula(paste0("Y~", paste0("X_", c(1:length(ColumnsToUse)), collapse="+"))),
                formula.npo=formula(paste0("~", paste0("X_", c(1:length(ColumnsToUse)), collapse="+"))),
                random=~1,
                id=ID,
                k=k,
                link="blogit", # specify that the model is a baseline logit random effects model
                EB=FALSE,
                maxit=maxit,
                tol=tol)
    #
    while_trs=1
    if(is.na(myfit$coefficients[1])){
      k=k+ifelse(sample(c(0,1), 1)==0, 1, -1)
      if(k==1){k=2}else if(k>=length(ColumnsToUse)-1){k=2}
      print(paste0("[ ", count, "th run ] - Fail to converge. Try k=", k))
      while_trs=0 # re-run algorithm
      Sys.sleep(0.5)
    }else{
      if(par.update==T){
        if(myfit$flagcvm>0 | myfit$flaginfo>0){
          print(paste0("[ ", count, "th run ] - Reduce 'tol' from ", tol, " to ", tol/100))
          tol=tol/100
          while_trs=0 # re-run algorithm
        }
        if(myfit$iter==myfit$maxit){
          print(paste0("[ ", count, "th run ] - Increase 'maxit' from ", maxit, " to ", maxit+10000))
          maxit=maxit+10000
          while_trs=0 # re-run algorithm
        }
      }
    }
  }
  print(paste0("[ ", count, "th run ] - Algorithm converged (k=", k, ", maxit=", maxit, ", tol=", tol, ")"))
  
  # coefficient
  Coef=myfit$coefficients
  Coef.ind=which(grepl("X_", row.names(Coef)))
  # standard error
  SE.Coef=myfit$SE.coefficients
  # confidence interval (exponentiated)
  Raw_Upper_Bound=Coef[Coef.ind]+qnorm(0.975)*SE.Coef[Coef.ind]
  Raw_Lower_Bound=Coef[Coef.ind]-qnorm(0.975)*SE.Coef[Coef.ind]
  # p-values
  Z_value=Coef/SE.Coef # Wald test statistic
  P_values=(1-pnorm(abs(Z_value), 0, 1))*2
  # CI (upper and lower bounds)
  Upper_Bound=exp(Raw_Upper_Bound)
  Lower_Bound=exp(Raw_Lower_Bound)
  
  # output
  output=c()
  output$converged.set=data.table(count=count, k=k, maxit=maxit, tol=tol)
  output$Estimate=t(matrix(round2(Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  output$Std.Error=t(matrix(round2(SE.Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  output$`P-value`=t(matrix(ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                                   format(round2(P_values[Coef.ind], 3), nsmall=3)), nrow=length(levels(Y))-1))
  output$OR.and.CI=t(matrix(paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
                                   " (",
                                   format(round(Lower_Bound, 2), nsmall=2),
                                   " - ",
                                   format(round(Upper_Bound, 2), nsmall=2),
                                   ")"), nrow=length(levels(Y))-1))
  
  # name output rows
  row_names=c()
  for(i in 1:length(ColumnsToUse)){
    X=get(paste0("X_", i))
    if(is.factor(X)){
      X_Levels=levels(X)
      row_names=c(row_names, paste0(ColumnsToUse[i], " / ", X_Levels[-1]))
    }else if(is.numeric(X)){
      row_names=c(row_names, ColumnsToUse[i])
    }
  }
  colnames(output$Estimate)=levels(Y)[-length(levels(Y))]
  colnames(output$Std.Error)=levels(Y)[-length(levels(Y))]
  colnames(output$`P-value`)=levels(Y)[-length(levels(Y))]
  colnames(output$OR.and.CI)=levels(Y)[-length(levels(Y))]
  rownames(output$Estimate)=row_names
  rownames(output$Std.Error)=row_names
  rownames(output$`P-value`)=row_names
  rownames(output$OR.and.CI)=row_names
  
  return(output)
}

#**************************
# GLMM_Confounder_Selection
#**************************
# Example
#******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# 
# Data$sex=as.character(Data$sex)
# Data[sample(nrow(Data), 30), "sex"]="N"
# Data[sample(nrow(Data), 30), "sex"]="P"
# Data$sex=as.factor(Data$sex)
# 
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM.fit=GLMM_Multivariable(Data,
#                                 ColumnsToUse,
#                                 Outcome_name="outcome",
#                                 ID_name="id",
#                                 which.family="binomial", # gaussian, binomial, poisson
#                                 NAGQ<-1,
#                                 Compute.Power=F,
#                                 nsim=30)
# Confounder_Steps=GLMM_Confounder_Selection(Full_Model=GLMM.fit$model_fit,
#                                            Main_Pred_Var="sex",
#                                            Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"],
#                                            which.family="binomial", # distribution of the response variable
#                                            Min.Change.Percentage=5,
#                                            Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
GLMM_Confounder_Selection=function(Full_Model, 
                                   Main_Pred_Var, 
                                   Potential_Con_Vars, 
                                   which.family="binomial",
                                   Min.Change.Percentage=5,
                                   Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  
  # Full_Model=GLMM.example$model_fit
  # Main_Pred_Var="sex"
  # Potential_Con_Vars=c("center", "treat", "age", "baseline", "visit")
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Potential_Con_Vars=Potential_Con_Vars
  Include_Index=c(1:length(Potential_Con_Vars))
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    Fixed_Effects_Current_Full_Model=fixef(Current_Full_Model)
    Main_Effects_Current_Full_Model=Fixed_Effects_Current_Full_Model[grep(Main_Pred_Var, names(Fixed_Effects_Current_Full_Model))]
    
    # [ IMPORTANT ] - When indep_var is a factor with more than two levels, we pick max coef of its levels.
    Main_Cov_Level=names(which.max(abs(Main_Effects_Current_Full_Model)))
    Main_Effect_Current_Full_Model=Main_Effects_Current_Full_Model[Main_Cov_Level]
    Main_Effect_Current_Reduced_Model=c()
    
    # run GLMM excluding one variable at once
    for(i in 1:length(Potential_Con_Vars[Include_Index])){
      #i=1
      Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[Include_Index][i], collapse="-"))))
      Fixed_Effects_Current_Reduced_Model=fixef(Current_Reduced_Model)
      Main_Effect_Current_Reduced_Model[i]=Fixed_Effects_Current_Reduced_Model[Main_Cov_Level]
      
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Potential_Con_Vars[Include_Index])))
    }
    
    if(Estimate=="raw_estimate"){
      #**** refer to the raw coefficient estimate ****
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
        Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }else if(Estimate=="converted_estimate"){
      # summary table
      if(which.family=="gaussian"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="binomial"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(which.family=="poisson"){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_RR=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }
    }
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than 10, terminate the while loop
      loop.key=1
    }else{
      # decide the variable to remove
      Var_to_Remove=Temp_Table[Rank==min(Rank, na.rm=T), Removed_Var]
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Potential_Con_Vars==Var_to_Remove[1])]
      # update the current full model
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[setdiff(1:length(Potential_Con_Vars), Include_Index)], collapse="-"))))
      # increase step
      step=step+1
      
      # if there's no more variable left
      if(length(Include_Index)==0){
        Temp_Table=data.table(
          Removed_Var=c("Full", Potential_Con_Vars[Include_Index]),
          Estimate=c(fixef(Current_Full_Model)[-1]),
          Delta="",
          Rank=""
        )
        Out$summ_table[[step]]=Temp_Table
        loop.key=1
      }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Confounders=c(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var[-grep(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var)])
  
  return(Out)
}

#**********************
# GLMM_Confounder_Model
#**********************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# ColumnsToUse=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Outcome_name="outcome",
#                            ColumnsToUse,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# # Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# Main_Pred_Var="sex"
# Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"]
# 
# GLMM_Confounder=GLMM_Confounder_Model(Data=Data_to_use,
#                                       Main_Pred_Var=Main_Pred_Var,
#                                       Potential_Con_Vars=ColumnsToUse[ColumnsToUse!=Main_Pred_Var],
#                                       Outcome_name="outcome",
#                                       ID_name="id",
#                                       which.family="binomial", # gaussian, binomial, poisson
#                                       NAGQ=1,
#                                       Min.Change.Percentage=30,
#                                       Estimate="raw_estimate") # raw_estimate, converted_estimate
# GLMM_Confounder$Full_Multivariable_Model$summ_table
# GLMM_Confounder$Confounder_Steps$Confounders
# GLMM_Confounder$Confounder_Model$summ_table
GLMM_Confounder_Model=function(Data,
                               Main_Pred_Var,
                               Potential_Con_Vars,
                               Outcome_name,
                               ID_name,
                               which.family,
                               NAGQ=100,
                               Min.Change.Percentage=5,
                               Estimate="raw_estimate"){
  Output=c()
  # Full multivariable model
  ColumnsToUse=c(Main_Pred_Var, Potential_Con_Vars)
  Output$Full_Multivariable_Model=GLMM_Multivariable(Data,
                                                     ColumnsToUse=ColumnsToUse,
                                                     Outcome_name=Outcome_name,
                                                     ID_name=ID_name,
                                                     which.family=which.family,
                                                     NAGQ=NAGQ)
  
  # Confounder selection
  Confounder_Steps=GLMM_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                             Main_Pred_Var=Main_Pred_Var,
                                             Potential_Con_Vars=ColumnsToUse[ColumnsToUse!=Main_Pred_Var],
                                             which.family=which.family, # distribution of the response variable
                                             Min.Change.Percentage=Min.Change.Percentage,
                                             Estimate=Estimate) # raw_estimate, converted_estimate
  
  # save all stepwise procedure
  Output$Confounder_Steps=Confounder_Steps
  
  # index of confounders
  Confounder_Ind=which(ColumnsToUse%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=GLMM_Multivariable(Data,
                                             ColumnsToUse=ColumnsToUse[Confounder_Ind],
                                             Outcome_name=Outcome_name,
                                             ID_name=ID_name,
                                             which.family=which.family,
                                             NAGQ=NAGQ)
  return(Output)
}

#********
#
# GLMM_CV
#
#*****************
# For some reason, the function in library(lmmen) that conducts a cross-validation for the optimal tuning parameter does not work.
#cv.glmmLasso(initialize_example(seed=1))
# Thus, I wrote my own code that perform a k-fold cross-validation as below.
#***************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="binomial(link=logit)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse=pred_vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# Data$sex=as.character(Data$sex)
# Data[sample(nrow(Data), 150), "sex"]="N"
# lambda=seq(0, 5, by=0.5)
# # GLMM_CV_Out
# GLMM_CV_Out=GLMM_CV(data=Data,
#                     pred_vars,
#                     res_var,
#                     rand_var,
#                     which.family,
#                     k=6,
#                     lambda=lambda)
# # train error
# GLMM_CV_Out$Train_Error
# # cv error
# GLMM_CV_Out$CV_Error
# # plot
# GLMM_CV_Out$CV_plot
# # optimal lambda
# GLMM_CV_Out$Optimal_Lambda

# # There's a function that performs a CV for GLMM, called cv.glmmLasso, in lmmen package.
# # However, there appears to be some issues when the function is excuted (fun a code below). I googled to find how to troubleshoot, but there's even not
# # a single example that shows the use of the function.
# # https://raw.githubusercontent.com/cran/glmmLasso/master/demo/glmmLasso-soccer.r
# library(lmmen)
# cv.glmmLasso(dat=Data,
#              form.fixed=outcome ~ center + as.factor(treat) + as.factor(sex) + age + baseline + visit,
#              form.rnd=list(id=~1),
#              family=binomial(link=logit),
#              lambda=seq(0, 20, by=5))
GLMM_CV=function(data,
                 pred_vars,
                 res_var,
                 rand_var,
                 which.family,
                 k=4,
                 lambda=seq(0, 10, by=1)){
  #data=data[sample(1:nrow(data), 5000), ]
  
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr", "ggplot2"), checkpackages)
  # convert data to data frame
  data=as.data.table(data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  CV_data=na.omit(data[, 
                       .SD, 
                       .SDcols=c(res_var, pred_vars, rand_var)])
  
  # grouping variable
  CV_data[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
  
  
  #***
  # CV
  #***
  # generate array containing fold-number for each sample (row)
  pass.ind=1
  while(sum(pass.ind)>0){
    folds=sample(rep_len(1:k, nrow(CV_data)), nrow(CV_data))
    for(k.ind in 1:k){
      #k.ind=1
      # actual split of the CV_data
      fold=which(folds == k.ind)
      
      # divide data into training and test sets
      CV_data_train=CV_data[-fold, ]
      CV_data_test=CV_data[fold, ]
      
      if(sum((CV_data_train[, .SD, .SDcols=pred_vars] %>% 
              lapply(function(x) length(unique(x))) %>% 
              unlist)==1)>0){
        print(which((CV_data_train[, .SD, .SDcols=pred_vars] %>% 
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
  CV.model=as.formula(paste(res_var, "~", paste(ifelse(!unlist(data[, lapply(.SD, is.numeric), .SDcols=pred_vars]), paste0("as.factor(", pred_vars, ")"), pred_vars), collapse="+"), sep=""))
  
  # generate empty matrix to save Train_Error
  Train_Error=matrix(NA, length(lambda), k)
  rownames(Train_Error)=c(paste0("lambda=", lambda))
  colnames(Train_Error)=c(paste0(1:k, "nd sub"))
  # generate empty matrix to save CV_Error
  CV_Error=matrix(NA, length(lambda), k)
  rownames(CV_Error)=c(paste0("lambda=", lambda))
  colnames(CV_Error)=c(paste0(1:k, "nd sub"))
  # generate empty matrix to save CV_AIC
  CV_AIC=matrix(NA, length(lambda), k)
  rownames(CV_AIC)=c(paste0("lambda=", lambda))
  colnames(CV_AIC)=c(paste0(1:k, "nd sub"))
  # generate empty matrix to save CV_BIC
  CV_BIC=matrix(NA, length(lambda), k)
  rownames(CV_BIC)=c(paste0("lambda=", lambda))
  colnames(CV_BIC)=c(paste0(1:k, "nd sub"))
  
  # run algorithm
  for(lambda.ind in 1:length(lambda)){
    #lambda.ind=1
    # actual cross validation
    for(k.ind in 1:k) {
      #k.ind=2
      # actual split of the CV_data
      fold=which(folds == k.ind)
      # random effect
      random_effect=list(id=~1)
      names(random_effect)=rand_var
      ## fit adjacent category model
      glmmLasso.fit=glmmLasso(CV.model, 
                              rnd=random_effect, 
                              family=eval(parse(text=which.family)), 
                              data=CV_data_train, 
                              lambda=lambda[lambda.ind])
      
      # Make predictions and compute the R2, RMSE and MAE
      predictions_train=glmmLasso.fit %>% predict(CV_data_train)
      predictions_test=glmmLasso.fit %>% predict(CV_data_test)
      
      # Train_Error and CV_Error
      Train_Error[lambda.ind, k.ind]=mean(unlist(predictions_train - CV_data_train[, .SD, .SDcol=res_var])^2)
      CV_Error[lambda.ind, k.ind]=mean(unlist(predictions_test - CV_data_test[, .SD, .SDcol=res_var])^2)
      
      CV_AIC[lambda.ind, k.ind]=glmmLasso.fit$aic
      CV_BIC[lambda.ind, k.ind]=glmmLasso.fit$bic
      
      if(k.ind == k){
        # print process
        print(paste0("k : ", k.ind, ", lambda : ", lambda[lambda.ind]))
      }
    }
  }
  
  # combine Train_Error and CV_Error
  out=list()
  out$Train_Error=Train_Error
  out$CV_Error=CV_Error
  out$CV_AIC=CV_AIC
  out$CV_BIC=CV_BIC
  
  # optimal lambda
  out$Optimal_Lambda=lambda[which.min(apply(CV_Error, 1, mean))]
  
  # plot
  Error_by_Lambda=data.frame(
    lambda=lambda,
    Error=c(apply(out$Train_Error, 1, mean), apply(out$CV_Error, 1, mean)),
    Label=c(rep("Train", length(lambda)), rep("CV", length(lambda)))
  )
  out$CV_plot=Error_by_Lambda %>%
    ggplot(aes(x=lambda, y=Error, group=Label)) +
    geom_line(aes(color=Label)) +
    geom_point(aes(color=Label)) +
    scale_color_brewer(palette="Dark2") +
    theme_set(theme_bw())
  
  return(out)
}

#***********
#
# GLMM_LASSO
#
#***********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="binomial(link=logit)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse=pred_vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# GLMM.LASSO.fit=GLMM_LASSO(data=Data,
#                           pred_vars,
#                           res_var,
#                           rand_var,
#                           which.family,
#                           lambda=10)
# summary(GLMM.LASSO.fit)
GLMM_LASSO=function(data, pred_vars, res_var, rand_var, which.family="binomial(link=logit)", lambda=10){
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr"), checkpackages)
  # convert data to data frame
  data=as.data.table(data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  data=na.omit(data[, 
                    .SD, 
                    .SDcols=c(res_var, pred_vars, rand_var)])
  
  #*************
  # run algoritm
  #*************
  # grouping variable
  data[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
  # random effect
  random_effect=list(id=~1)
  names(random_effect)=rand_var
  
  # specify GLMM model
  GLMM.model=as.formula(paste(res_var, "~", paste(ifelse(!unlist(data[, lapply(.SD, is.numeric), .SDcols=pred_vars]), paste0("as.factor(", pred_vars, ")"), pred_vars), collapse="+"), sep=""))
  
  # run glmm Lasso
  glmmLasso.fit=glmmLasso(GLMM.model, 
                          rnd=random_effect, 
                          family=eval(parse(text=which.family)), 
                          data=data, 
                          lambda=lambda,
                          switch.NR=TRUE)
  return(glmmLasso.fit)
}


#********************
#
# GLMM_Bivariate_Plot
#
#********************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="binomial(link=logit)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse=pred_vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# head(Data)
# GLMM_Bivariate_Plot(Data<-Data,
#                     Pred_Var<-"age",
#                     Res_Var<-"outcome",
#                     Group_Var<-"id",
#                     which.family<-"binomial",
#                     NAGQ<-100,
#                     xlab<-"age",
#                     ylab<-"outcome",
#                     title<-"Title",
#                     x_breaks<-seq(round(min(Data$age)-5, -1), round(max(Data$age)+5, -1), by=10))
GLMM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, Group_Var, which.family, NAGQ=100, xlab="", ylab="", title="", x_breaks=0){
  lapply(c("sjPlot", "simr"), checkpackages)
  
  # output
  output=c()
  
  # change data type & remove missing data
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Res_Var, Pred_Var, Group_Var)])
  
  # fit GLMM
  fullmod=as.formula(paste(Res_Var, "~", Pred_Var, "+(1|", Group_Var, ")", sep=""))
  
  Model=glmer(fullmod, family=which.family, na.action=na.exclude, data=Data, nAGQ=NAGQ)
  output$glmer=Model
  #output$power=powerSim(Model)
  
  # save random effect plot
  output$re_plot=plot_model(Model, type="re")
  
  # generate newdat for prediction
  newdat=expand.grid(Pred_Var=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=50), 
                     Group_Var=unique(Data[, Group_Var]))
  colnames(newdat)=c(Pred_Var, Group_Var)
  
  newdat$conditional_fit=predict(Model, newdat, type="response")
  colnames(newdat)=c(Pred_Var, Group_Var, Res_Var)
  
  # generate marginal effect prediction line and its CIs
  newdat.margin=data.frame(X=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=50))
  mm=model.matrix(~X, newdat.margin)
  raw_fit=mm%*%fixef(Model)
  pvar1=diag(mm%*%tcrossprod(vcov(Model), mm))
  VarCorr_out=unlist(VarCorr(Model))
  tvar1=pvar1+VarCorr_out[names(VarCorr_out)==Group_Var]
  
  if(which.family=="poisson"){ # code for 'poisson' family
    newdat.margin=data.frame(
      Pred_Var=newdat.margin$X, 
      Res_Var=exp(raw_fit), 
      Group_Var="Fixed Effect", 
      plo=exp(raw_fit-1.96*sqrt(pvar1)), 
      pho=exp(raw_fit+1.96*sqrt(pvar1)), 
      tlo=exp(raw_fit-1.96*sqrt(tvar1)), 
      tho=exp(raw_fit+1.96*sqrt(tvar1))
    )
    newdat.margin[newdat.margin$plo<0, "plo"]=0
    newdat.margin[newdat.margin$tlo<0, "tlo"]=0
    
    colnames(newdat.margin)=c(Pred_Var, Res_Var, Group_Var, "CI_Lower", "CI_Upper", "PI_Lower", "PI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    newdat[, Group_Var]=as.factor(newdat[, Group_Var])
    newdat.margin[, Group_Var]=as.factor(newdat.margin[, Group_Var])
    
    # plot with only pure fixed effect
    output$plot=ggplot(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
                                               y=eval(parse(text=Res_Var)), 
                                               group=eval(parse(text=Group_Var))
    )) + 
      geom_ribbon(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10) +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=x_breaks) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.8, col="blue") + 
      theme_bw() + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
    
    # plot with random effect
    output$plot_w_re=ggplot(newdat, aes(x=eval(parse(text=Pred_Var)), 
                                        y=eval(parse(text=Res_Var)), 
                                        #col=eval(parse(text=Group_Var)), 
                                        group=eval(parse(text=Group_Var))
    )) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.5, alpha=5/10, linetype="longdash") +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=x_breaks) +
      #geom_ribbon(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10, colour=NA) +
      geom_line(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
                                        y=eval(parse(text=Res_Var)), 
                                        group=eval(parse(text=Group_Var))
      ), size=0.8, col="blue") +
      theme_bw() +
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }else if(which.family=="binomial"){ # code for 'binomial' family
    newdat.margin=data.frame(
      Pred_Var=newdat.margin$X, 
      Res_Var=1/(1+exp(-raw_fit)), 
      Group_Var="Fixed Effect", 
      plo=1/(1+exp(-(raw_fit-1.96*sqrt(pvar1)))), 
      pho=1/(1+exp(-(raw_fit+1.96*sqrt(pvar1)))), 
      tlo=1/(1+exp(-(raw_fit-1.96*sqrt(tvar1)))), 
      tho=1/(1+exp(-(raw_fit+1.96*sqrt(tvar1))))
    )
    newdat.margin[newdat.margin$plo<0, "plo"]=0
    newdat.margin[newdat.margin$pho>1, "pho"]=1
    newdat.margin[newdat.margin$tlo<0, "tlo"]=0
    newdat.margin[newdat.margin$tho>1, "tho"]=1
    
    colnames(newdat.margin)=c(Pred_Var, Res_Var, Group_Var, "CI_Lower", "CI_Upper", "PI_Lower", "PI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    newdat[, Group_Var]=as.factor(newdat[, Group_Var])
    newdat.margin[, Group_Var]=as.factor(newdat.margin[, Group_Var])
    
    # plot with only pure fixed effect
    output$plot=ggplot(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
                                               y=eval(parse(text=Res_Var)), 
                                               group=eval(parse(text=Group_Var))
    )) + 
      geom_ribbon(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10) +
      geom_hline(yintercept=0) + 
      geom_hline(yintercept=1) + 
      scale_y_continuous(labels=scales::percent) + 
      scale_x_continuous(breaks=x_breaks) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.8, col="blue") + 
      theme_bw() + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
    # plot with random effect
    output$plot_w_re=ggplot(newdat, aes(x=eval(parse(text=Pred_Var)), 
                                        y=eval(parse(text=Res_Var)), 
                                        #col=eval(parse(text=Group_Var)), 
                                        group=eval(parse(text=Group_Var))
    )) +
      geom_line(aes(y=eval(parse(text=Res_Var))), size=0.5, alpha=5/10, linetype="longdash") +
      geom_hline(yintercept=0) + 
      geom_hline(yintercept=1) + 
      scale_y_continuous(labels=scales::percent) + 
      scale_x_continuous(breaks=x_breaks) +
      #geom_ribbon(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), ymax=CI_Upper, ymin=CI_Lower), fill="blue", alpha=2/10, colour=NA) +
      geom_line(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
                                        y=eval(parse(text=Res_Var)), 
                                        group=eval(parse(text=Group_Var))
      ), size=0.8, col="blue") +
      theme_bw() +
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }else{print("Currently available families include : 'binomial' and 'poisson' ")}
  
  return(output)
}

#*************************
#
# GLMM_Overdispersion_Test
#
#*************************
# Test overdispersion with null that the data are not overdispersed.
# So, rejecting H0 suggests the statistical evidence of overdispersion.
#**********************************************************************
# require(lme4)
# data("respiratory")
# Data=respiratory
# myfit=glmer(outcome~treat+age+(1|id), family=binomial, na.action=na.exclude, data=Data, nAGQ=100)
# GLMM_Overdispersion_Test(myfit)
GLMM_Overdispersion_Test=function(model){
  # number of variance parameters in an n-by-n variance-covariance matrix
  vpars=function(m){
    nrow(m)*(nrow(m)+1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df=sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf=nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp=residuals(model, type="pearson")
  Pearson.chisq=sum(rp^2)
  prat=Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval=pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}


#**********************
#
# [ --- CLMM --- ] ----
#
#**********************
# CLMM_Ordinal_Bivariate
#***********************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data$outcome=as.factor(Data$outcome)
# Data$id=as.factor(Data$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data<-Data,
#                                          ColumnsToUse<-ColumnsToUse,
#                                          Outcome_name<-"outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(ColumnsToUse))
# Type_Odds[ColumnsToUse%in%Output$sig_vars]="Non_Prop"
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# CLMM_Ordinal_Bivariate_Format_1(Data,
#                                 Pred_Vars<-ColumnsToUse,
#                                 Type_Odds<-Type_Odds,
#                                 Res_Var<-"outcome",
#                                 ID_name<-"id",
#                                 NAGQ=3)
# CLMM_Ordinal_Bivariate_Format_2(Data,
#                                 Pred_Vars<-ColumnsToUse,
#                                 Type_Odds<-Type_Odds,
#                                 Res_Var<-"outcome",
#                                 ID_name<-"id",
#                                 NAGQ=3)
#********************************
# CLMM_Ordinal_Bivariate_Format_1
CLMM_Ordinal_Bivariate_Format_1=function(Data,
                                         Pred_Vars,
                                         Type_Odds,
                                         Res_Var,
                                         ID_name,
                                         NAGQ=3){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, ID_name]=factor(Data[, ID_name], order=F)
  
  # main algorithm
  output=c()
  
  for(i in 1:length(Pred_Vars)){
    #i=4
    i<<-i
    if(Type_Odds[i]=="Prop"){
      model.fit=clmm2(as.formula(paste(Res_Var, "~ ", Pred_Vars[i])),
                      #nominal=as.formula(paste("~ ", Nom_Vars[i])),
                      random=eval(parse(text=ID_name)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }else if(Type_Odds[i]=="Non_Prop"){
      model.fit=clmm2(as.formula(paste(Res_Var, "~ 1")),
                      nominal=as.formula(paste("~ ", Pred_Vars[i])),
                      random=eval(parse(text=ID_name)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }
    
    model.fit.summ=summary(model.fit)$coefficients
    Coef.ind=which(grepl(ColumnsToUse[i], row.names(model.fit.summ)))
    # coefficient
    Coef=model.fit.summ[Coef.ind, 1]
    # standard error
    SE.Coef=model.fit.summ[Coef.ind, 2]
    # confidence interval (exponentiated)
    Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
    Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
    # CI (upper and lower bounds)
    Upper_Bound=exp(Raw_Upper_Bound)
    Lower_Bound=exp(Raw_Lower_Bound)
    
    # output
    temp_out=c()
    temp_out$Estimate=round2(Coef, 3)
    temp_out$Std.Error=round2(SE.Coef, 3)
    temp_out$`P-value`=ifelse(model.fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                              format(round2(model.fit.summ[Coef.ind, 4], 3), nsmall=3))
    temp_out$COR.and.CI=paste0(format(round(exp(Coef), 2), nsmall=2), 
                               " (",
                               format(round(Lower_Bound, 2), nsmall=2),
                               " - ",
                               format(round(Upper_Bound, 2), nsmall=2),
                               ")")
    
    #
    temp_out=data.frame(temp_out)
    if(Type_Odds[i]=="Prop"){
      if(is.factor(Data[, ColumnsToUse[i]])){
        name_temp=expand.grid(levels(Data[, ColumnsToUse[i]])[-1])
        row.names(temp_out)=paste0(ColumnsToUse[i], " ", unlist(name_temp))
      }else if(is.numeric(Data[, ColumnsToUse[i]])){
        row.names(temp_out)=paste0(ColumnsToUse[i])
      }
    }else if(Type_Odds[i]=="Non_Prop"){
      # blank
    }
    
    output=rbind(output, temp_out)
    
    #print(paste(i, " ", ColumnsToUse[i], sep=""))
  }
  return(output)
}

#********************************
# CLMM_Ordinal_Bivariate_Format_2
CLMM_Ordinal_Bivariate_Format_2=function(Data,
                                         Pred_Vars,
                                         Type_Odds,
                                         Res_Var,
                                         ID_name,
                                         NAGQ=3){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, ID_name]=factor(Data[, ID_name], order=F)
  
  # main algorithm
  output=c()
  
  for(i in 1:length(Pred_Vars)){
    #i=7
    i<<-i
    if(Type_Odds[i]=="Prop"){
      model.fit=clmm2(as.formula(paste(Res_Var, "~ ", Pred_Vars[i])),
                      #nominal=as.formula(paste("~ ", Nom_Vars[i])),
                      random=eval(parse(text=ID_name)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }else if(Type_Odds[i]=="Non_Prop"){
      model.fit=clmm2(as.formula(paste(Res_Var, "~ 1")),
                      nominal=as.formula(paste("~ ", Pred_Vars[i])),
                      random=eval(parse(text=ID_name)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }
    
    model.fit.summ=summary(model.fit)$coefficients
    Coef.ind=which(grepl(ColumnsToUse[i], row.names(model.fit.summ)))
    # coefficient
    Coef=model.fit.summ[Coef.ind, 1]
    # standard error
    SE.Coef=model.fit.summ[Coef.ind, 2]
    # confidence interval (exponentiated)
    Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
    Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
    # CI (upper and lower bounds)
    Upper_Bound=exp(Raw_Upper_Bound)
    Lower_Bound=exp(Raw_Lower_Bound)
    
    #
    if(Type_Odds[i]=="Prop"){
      # output
      temp_out=c()
      temp_out$Estimate=round2(Coef, 3)
      temp_out$Std.Error=round2(SE.Coef, 3)
      temp_out$`P-value`=ifelse(model.fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                                format(round2(model.fit.summ[Coef.ind, 4], 3), nsmall=3))
      temp_out$COR.and.CI=paste0(format(round(exp(Coef), 2), nsmall=2), 
                                 " (",
                                 format(round(Lower_Bound, 2), nsmall=2),
                                 " - ",
                                 format(round(Upper_Bound, 2), nsmall=2),
                                 ")")
      temp_out=data.frame(temp_out)
      
      if(is.factor(Data[, ColumnsToUse[i]])){
        name_temp=expand.grid(levels(Data[, ColumnsToUse[i]])[-1])
        row.names(temp_out)=paste0(ColumnsToUse[i], " ", unlist(name_temp))
      }else if(is.numeric(Data[, ColumnsToUse[i]])){
        row.names(temp_out)=paste0(ColumnsToUse[i])
      }
      # output
      output$Prop_Odds=rbind(output$Prop_Odds, temp_out)
    }else if(Type_Odds[i]=="Non_Prop"){
      # output
      temp_out=c()
      # record 
      if(is.factor(Data[, ColumnsToUse[i]])){
        X_Levels=levels(Data[, ColumnsToUse[i]])
        Y_Levels=levels(Data[, Outcome_name])
        temp_out$Estimate=matrix(round2(Coef, 3), 
                                 ncol=length(X_Levels)-1, 
                                 nrow=length(Y_Levels)-1)
        temp_out$Std.Error=matrix(round2(SE.Coef, 3), 
                                  ncol=length(X_Levels)-1, 
                                  nrow=length(Y_Levels)-1)
        temp_out$`P-value`=matrix(
          ifelse(model.fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                 format(round2(model.fit.summ[Coef.ind, 4], 3), nsmall=3)), 
          ncol=length(X_Levels)-1, 
          nrow=length(Y_Levels)-1)
        temp_out$COR.and.CI=matrix(
          paste0(format(round(exp(Coef), 2), nsmall=2), 
                 " (",
                 format(round(Lower_Bound, 2), nsmall=2),
                 " - ",
                 format(round(Upper_Bound, 2), nsmall=2),
                 ")"), 
          ncol=length(X_Levels)-1, 
          nrow=length(Y_Levels)-1)
        # names for row and column
        Temp_Row_Names=Y_Levels[-1]
        Temp_Column_Names=paste0(ColumnsToUse[i], " / ", X_Levels[-1])
      }else if(is.numeric(Data[, ColumnsToUse[i]])){
        temp_out$Estimate=data.frame(X=round2(Coef, 3))
        temp_out$Std.Error=data.frame(X=round2(SE.Coef, 3))
        temp_out$`P-value`=data.frame(
          X=ifelse(model.fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                   format(round2(model.fit.summ[Coef.ind, 4], 3), nsmall=3))
        )
        temp_out$COR.and.CI=data.frame(
          X=paste0(format(round(exp(Coef), 2), nsmall=2), 
                   " (",
                   format(round(Lower_Bound, 2), nsmall=2),
                   " - ",
                   format(round(Upper_Bound, 2), nsmall=2),
                   ")")
        )
        # names for row and column
        Temp_Row_Names=levels(Data[, Outcome_name])[-1]
        Temp_Column_Names=ColumnsToUse[i]
      }
      
      rownames(temp_out$Estimate)=Temp_Row_Names
      colnames(temp_out$Estimate)=Temp_Column_Names
      rownames(temp_out$Std.Error)=Temp_Row_Names
      colnames(temp_out$Std.Error)=Temp_Column_Names
      rownames(temp_out$`P-value`)=Temp_Row_Names
      colnames(temp_out$`P-value`)=Temp_Column_Names
      rownames(temp_out$COR.and.CI)=Temp_Row_Names
      colnames(temp_out$COR.and.CI)=Temp_Column_Names
      
      # output
      output$Non_Prop_Odds$Estimate=rbind(output$Non_Prop_Odds$Estimate, t(temp_out$Estimate))
      output$Non_Prop_Odds$Std.Error=rbind(output$Non_Prop_Odds$Std.Error, t(temp_out$Std.Error))
      output$Non_Prop_Odds$`P-value`=rbind(output$Non_Prop_Odds$`P-value`,t(temp_out$`P-value`))
      output$Non_Prop_Odds$COR.and.CI=rbind(output$Non_Prop_Odds$COR.and.CI, t(temp_out$COR.and.CI))
    }
    #print(paste(i, " ", ColumnsToUse[i], sep=""))
  }
  return(output)
}

#***************************
# CLMM_Ordinal_Multivariable
#***************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data$outcome=as.factor(Data$outcome)
# Data$id=as.factor(Data$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data<-Data,
#                                          ColumnsToUse<-ColumnsToUse,
#                                          Outcome_name<-"outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(ColumnsToUse))
# Type_Odds[ColumnsToUse%in%Output$sig_vars]="Non_Prop"
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# CLMM_Ordinal_Multivariable_Format_1(Data,
#                                     Pred_Vars<-ColumnsToUse,
#                                     Type_Odds<-Type_Odds,
#                                     Res_Var<-"outcome",
#                                     ID_name<-"id",
#                                     NAGQ=3)
# CLMM_Ordinal_Multivariable_Format_2(Data,
#                                     Pred_Vars<-ColumnsToUse,
#                                     Type_Odds<-Type_Odds,
#                                     Res_Var<-"outcome",
#                                     ID_name<-"id",
#                                     NAGQ=3)
#************************************
# CLMM_Ordinal_Multivariable_Format_1
CLMM_Ordinal_Multivariable_Format_1=function(Data,
                                             Pred_Vars,
                                             Type_Odds,
                                             Res_Var,
                                             ID_name,
                                             NAGQ=3){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, ID_name]=factor(Data[, ID_name], order=F)
  
  # main algorithm
  output=c()
  Loc_Vars<<-Pred_Vars[Type_Odds=="Prop"]
  Nom_Vars<<-Pred_Vars[Type_Odds=="Non_Prop"]
  
  model.fit=clmm2(as.formula(paste(Res_Var, "~ ", paste(Loc_Vars, collapse="+"))),
                  nominal=as.formula(paste("~ ", paste(Nom_Vars, collapse="+"))),
                  random=eval(parse(text=ID_name)),
                  data=Data,
                  Hess=TRUE,
                  nAGQ=NAGQ,
                  link="logistic")
  # save model fit
  output$model_fit=model.fit
  model.fit.summ=summary(model.fit)$coefficients[-c(1:(length(levels(Data[, Res_Var]))-1)), ]
  
  # coefficient
  Coef=model.fit.summ[, 1]
  SE.Coef=model.fit.summ[, 2]
  # confidence interval (exponentiated)
  Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
  Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
  # CI (upper and lower bounds)
  Upper_Bound=exp(Raw_Upper_Bound)
  Lower_Bound=exp(Raw_Lower_Bound)
  
  # output
  temp_out=c()
  temp_out$Estimate=round2(Coef, 3)
  temp_out$Std.Error=round2(SE.Coef, 3)
  temp_out$`P-value`=ifelse(model.fit.summ[, 4]<0.001, "<0.001", 
                            format(round2(model.fit.summ[, 4], 3), nsmall=3))
  temp_out$COR.and.CI=paste0(format(round(exp(Coef), 2), nsmall=2), 
                             " (",
                             format(round(Lower_Bound, 2), nsmall=2),
                             " - ",
                             format(round(Upper_Bound, 2), nsmall=2),
                             ")")
  
  #
  
  temp_out=data.frame(temp_out)
  for(i in 1:length(Pred_Vars)){
    if(Type_Odds[i]=="Prop"){
      if(is.factor(Data[, Pred_Vars[i]])){
        name_temp=expand.grid(levels(Data[, Pred_Vars[i]])[-1])
        rownames(temp_out)[grep(ColumnsToUse[i], rownames(temp_out))]=paste0(Pred_Vars[i], " ", unlist(name_temp))
        
      }else if(is.numeric(Data[, ColumnsToUse[i]])){
        rownames(temp_out)[grep(ColumnsToUse[i], rownames(temp_out))]=paste0(ColumnsToUse[i])
      }
    }else if(Type_Odds[i]=="Non_Prop"){
      # blank
    }
  }
  
  output=temp_out
  return(output)
  #print(paste(i, " ", ColumnsToUse[i], sep=""))
}

#************************************
# CLMM_Ordinal_Multivariable_Format_2
CLMM_Ordinal_Multivariable_Format_2=function(Data,
                                             Pred_Vars,
                                             Type_Odds,
                                             Res_Var,
                                             ID_name,
                                             NAGQ=3){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, ID_name]=factor(Data[, ID_name], order=F)
  
  # main algorithm
  output=c()
  Loc_Vars<<-Pred_Vars[Type_Odds=="Prop"]
  Nom_Vars<<-Pred_Vars[Type_Odds=="Non_Prop"]
  
  model.fit=clmm2(as.formula(paste(Res_Var, "~ ", paste(Loc_Vars, collapse="+"))),
                  nominal=as.formula(paste("~ ", paste(Nom_Vars, collapse="+"))),
                  random=eval(parse(text=ID_name)),
                  data=Data,
                  Hess=TRUE,
                  nAGQ=NAGQ,
                  link="logistic")
  
  # save model fit
  output$model_fit=model.fit
  model.fit.summ=summary(model.fit)$coefficients[-c(1:(length(levels(Data[, Res_Var]))-1)), ]
  
  # coefficient
  Coef=model.fit.summ[, 1]
  SE.Coef=model.fit.summ[, 2]
  # confidence interval (exponentiated)
  Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
  Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
  # CI (upper and lower bounds)
  Upper_Bound=exp(Raw_Upper_Bound)
  Lower_Bound=exp(Raw_Lower_Bound)
  
  #
  output$Prop_Odds=c()
  output$Non_Prop_Odds=c()
  for(i in 1:length(Pred_Vars)){
    Target_Ind=grep(Pred_Vars[i], names(Coef))
    
    if(Type_Odds[i]=="Prop"){
      # output
      temp_out=c()
      
      temp_out$Estimate=round2(Coef[Target_Ind], 3)
      temp_out$Std.Error=round2(SE.Coef[Target_Ind], 3)
      temp_out$`P-value`=ifelse(model.fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                                format(round2(model.fit.summ[Target_Ind, 4], 3), nsmall=3))
      temp_out$COR.and.CI=paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                                 " (",
                                 format(round(Lower_Bound[Target_Ind], 2), nsmall=2),
                                 " - ",
                                 format(round(Upper_Bound[Target_Ind], 2), nsmall=2),
                                 ")")
      temp_out=data.frame(temp_out)
      
      if(is.factor(Data[, Pred_Vars[i]])){
        name_temp=expand.grid(levels(Data[, Pred_Vars[i]])[-1])
        row.names(temp_out)=paste0(Pred_Vars[i], " ", unlist(name_temp))
      }else if(is.numeric(Data[, Pred_Vars[i]])){
        row.names(temp_out)=paste0(Pred_Vars[i])
      }
      # output
      output$Prop_Odds=rbind(output$Prop_Odds, temp_out)
    }else if(Type_Odds[i]=="Non_Prop"){
      # output
      temp_out=c()
      # record 
      if(is.factor(Data[, Pred_Vars[i]])){
        X_Levels=levels(Data[, Pred_Vars[i]])
        Y_Levels=levels(Data[, Outcome_name])
        temp_out$Estimate=matrix(round2(Coef[Target_Ind], 3), 
                                 ncol=length(X_Levels)-1, 
                                 nrow=length(Y_Levels)-1)
        temp_out$Std.Error=matrix(round2(SE.Coef[Target_Ind], 3), 
                                  ncol=length(X_Levels)-1, 
                                  nrow=length(Y_Levels)-1)
        temp_out$`P-value`=matrix(
          ifelse(model.fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                 format(round2(model.fit.summ[Target_Ind, 4], 3), nsmall=3)), 
          ncol=length(X_Levels)-1, 
          nrow=length(Y_Levels)-1)
        temp_out$COR.and.CI=matrix(
          paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                 " (",
                 format(round(Lower_Bound[Target_Ind], 2), nsmall=2),
                 " - ",
                 format(round(Upper_Bound[Target_Ind], 2), nsmall=2),
                 ")"), 
          ncol=length(X_Levels)-1, 
          nrow=length(Y_Levels)-1)
        # names for row and column
        Temp_Row_Names=Y_Levels[-1]
        Temp_Column_Names=paste0(Pred_Vars[i], " / ", X_Levels[-1])
      }else if(is.numeric(Data[, Pred_Vars[i]])){
        temp_out$Estimate=data.frame(X=round2(Coef[Target_Ind], 3))
        temp_out$Std.Error=data.frame(X=round2(SE.Coef[Target_Ind], 3))
        temp_out$`P-value`=data.frame(
          X=ifelse(model.fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                   format(round2(model.fit.summ[Target_Ind, 4], 3), nsmall=3))
        )
        temp_out$COR.and.CI=data.frame(
          X=paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                   " (",
                   format(round(Lower_Bound[Target_Ind], 2), nsmall=2),
                   " - ",
                   format(round(Upper_Bound[Target_Ind], 2), nsmall=2),
                   ")")
        )
        # names for row and column
        Temp_Row_Names=levels(Data[, Outcome_name])[-1]
        Temp_Column_Names=Pred_Vars[i]
      }
      
      rownames(temp_out$Estimate)=Temp_Row_Names
      colnames(temp_out$Estimate)=Temp_Column_Names
      rownames(temp_out$Std.Error)=Temp_Row_Names
      colnames(temp_out$Std.Error)=Temp_Column_Names
      rownames(temp_out$`P-value`)=Temp_Row_Names
      colnames(temp_out$`P-value`)=Temp_Column_Names
      rownames(temp_out$COR.and.CI)=Temp_Row_Names
      colnames(temp_out$COR.and.CI)=Temp_Column_Names
      
      # output
      output$Non_Prop_Odds$Estimate=rbind(output$Non_Prop_Odds$Estimate, t(temp_out$Estimate))
      output$Non_Prop_Odds$Std.Error=rbind(output$Non_Prop_Odds$Std.Error, t(temp_out$Std.Error))
      output$Non_Prop_Odds$`P-value`=rbind(output$Non_Prop_Odds$`P-value`,t(temp_out$`P-value`))
      output$Non_Prop_Odds$COR.and.CI=rbind(output$Non_Prop_Odds$COR.and.CI, t(temp_out$COR.and.CI))
    }
  }
  
  #print(paste(i, " ", ColumnsToUse[i], sep=""))
  return(output)
}

#**************************
# CLMM_Confounder_Selection
#**************************
# Example
#***************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data$outcome=as.factor(Data$outcome)
# Data$id=as.factor(Data$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data<-Data,
#                                          ColumnsToUse<-ColumnsToUse,
#                                          Outcome_name<-"outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(ColumnsToUse))
# Type_Odds[ColumnsToUse%in%Output$sig_vars]="Non_Prop"
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# CLMM.fit=CLMM_Ordinal_Multivariable_Format_2(Data,
#                                              Pred_Vars<-ColumnsToUse,
#                                              Type_Odds<-Type_Odds,
#                                              Res_Var<-"outcome",
#                                              ID_name<-"id",
#                                              NAGQ<-3)
# 
# Confounder_Steps=CLMM_Confounder_Selection(Full_Model=CLMM.fit$model_fit,
#                                            Main_Pred_Var="sex",
#                                            Main_Pred_Var_Type_Odds<-Type_Odds[ColumnsToUse=="sex"],
#                                            Potential_Con_Vars=ColumnsToUse[ColumnsToUse!="sex"],
#                                            Potential_Con_Vars_Type_Odds<-Type_Odds[ColumnsToUse!="sex"],
#                                            Min.Change.Percentage=5,
#                                            Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
CLMM_Confounder_Selection=function(Full_Model,
                                   Main_Pred_Var,
                                   Main_Pred_Var_Type_Odds,
                                   Potential_Con_Vars,
                                   Potential_Con_Vars_Type_Odds,
                                   Min.Change.Percentage=5,
                                   Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  
  # Full_Model=CLMM.example$model_fit
  # Main_Pred_Var="sex"
  # Potential_Con_Vars=c("center", "treat", "age", "baseline", "visit")
  # check out packages
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Potential_Con_Vars=Potential_Con_Vars
  Include_Index=c(1:length(Potential_Con_Vars))
  
  Loc_Vars<<-Potential_Con_Vars[Potential_Con_Vars_Type_Odds=="Prop"]
  Nom_Vars<<-Potential_Con_Vars[Potential_Con_Vars_Type_Odds=="Non_Prop"]
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    #Fixed_Effects_Current_Full_Model=fixef(Current_Full_Model)
    Fixed_Effects_Current_Full_Model=Current_Full_Model$coefficients
    Main_Effects_Current_Full_Model=Fixed_Effects_Current_Full_Model[grep(Main_Pred_Var, names(Fixed_Effects_Current_Full_Model))]
    
    # [ IMPORTANT ] - When indep_var is a factor with more than two levels, we pick max coef of its levels.
    Main_Cov_Level=names(which.max(abs(Main_Effects_Current_Full_Model)))
    Main_Effect_Current_Full_Model=Main_Effects_Current_Full_Model[Main_Cov_Level]
    Main_Effect_Current_Reduced_Model=c()
    
    # run CLMM excluding one variable at once
    for(i in 1:length(Potential_Con_Vars[Include_Index])){
      #i=1
      Removed_Var_Temp=Potential_Con_Vars[Include_Index][i]
      
      if(Removed_Var_Temp%in%Loc_Vars){ # if removed variable is proportional odds
        if(Main_Pred_Var_Type_Odds=="Prop"){
          Current_Reduced_Model=update(Current_Full_Model, 
                                       location=formula(paste0("~.+", Main_Pred_Var, "-", Removed_Var_Temp)))
        }
        if(Main_Pred_Var_Type_Odds=="Non_Prop"){
          Current_Reduced_Model=update(Current_Full_Model, 
                                       location=formula(paste0("~.-", Removed_Var_Temp)),
                                       nominal=formula(paste0("~.+", Main_Pred_Var)))
        }
      }
      if(Removed_Var_Temp%in%Nom_Vars){ # if removed variable is non-proportional odds
        if(Main_Pred_Var_Type_Odds=="Prop"){
          Current_Reduced_Model=update(Current_Full_Model, 
                                       location=formula(paste0("~.+", Main_Pred_Var)), 
                                       nominal=formula(paste0("~.-", Removed_Var_Temp)))
        }
        if(Main_Pred_Var_Type_Odds=="Non_Prop"){
          Current_Reduced_Model=update(Current_Full_Model, 
                                       nominal=formula(paste0("~.+", Main_Pred_Var, "-", Removed_Var_Temp)))
        }
      }
      
      #Fixed_Effects_Current_Reduced_Model=fixef(Current_Reduced_Model)
      Fixed_Effects_Current_Reduced_Model=Current_Reduced_Model$coefficients
      Main_Effect_Current_Reduced_Model[i]=Fixed_Effects_Current_Reduced_Model[Main_Cov_Level]
      
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Potential_Con_Vars[Include_Index])))
    }
    
    if(Estimate=="raw_estimate"){
      #**** refer to the raw coefficient estimate ****
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
        Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }else if(Estimate=="converted_estimate"){
      # summary table
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Est_Cum_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
        Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than 10, terminate the while loop
      loop.key=1
    }else{
      # decide the variable to remove
      Var_to_Remove=Temp_Table[Rank==min(Rank, na.rm=T), Removed_Var]
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Potential_Con_Vars==Var_to_Remove[1])]
      # update the current full model
      #Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[setdiff(1:length(Potential_Con_Vars), Include_Index)], collapse="-"))))
      if(Var_to_Remove%in%Loc_Vars){ # if removed variable is proportional odds
        if(Main_Pred_Var_Type_Odds=="Prop"){
          Current_Full_Model=update(Current_Full_Model, 
                                    location=formula(paste0("~.+", Main_Pred_Var, "-", Var_to_Remove)))
        }
        if(Main_Pred_Var_Type_Odds=="Non_Prop"){
          Current_Full_Model=update(Current_Full_Model, 
                                    location=formula(paste0("~.-", Var_to_Remove)),
                                    nominal=formula(paste0("~.+", Main_Pred_Var)))
        }
      }
      if(Var_to_Remove%in%Nom_Vars){ # if removed variable is non-proportional odds
        if(Main_Pred_Var_Type_Odds=="Prop"){
          Current_Full_Model=update(Current_Full_Model, 
                                    location=formula(paste0("~.+", Main_Pred_Var)), 
                                    nominal=formula(paste0("~.-", Var_to_Remove)))
        }
        if(Main_Pred_Var_Type_Odds=="Non_Prop"){
          Current_Full_Model=update(Current_Full_Model, 
                                    nominal=formula(paste0("~.+", Main_Pred_Var, "-", Var_to_Remove)))
        }
      }
      
      # increase step
      step=step+1
      
      # if there's no more variable left
      if(length(Include_Index)==0){
        Temp_Table=data.table(
          Removed_Var=c("Full", Potential_Con_Vars[Include_Index]),
          Estimate=c(fixef(Current_Full_Model)[-1]),
          Delta="",
          Rank=""
        )
        Out$summ_table[[step]]=Temp_Table
        loop.key=1
      }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Confounders=c(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var[-grep(Main_Pred_Var, Out$summ_table[[step]]$Removed_Var)])
  
  return(Out)
}

#**********************************
# Proportional_Odds_Assumption_Test
#**********************************
# # The most popular form of this model (which we will focus on exclusively in this course) is
# # the proportional odds model. In this model, the linear predictor x is restricted so that the
# # intercept may depend on j, but the effects of the other predictor variables are constant
# # across response categories:
# # The assumption is that the effects of any explanatory variables are consistent (proportional)
# # across the different thresholds (by thresholds we mean the splits between each pair of categories
# # of your ordinal outcome variable).
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# ColumnsToUse=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data$outcome=as.factor(Data$outcome)
# Data$id=as.factor(Data$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data<-Data,
#                                          ColumnsToUse<-ColumnsToUse,
#                                          Outcome_name<-"outcome")
# # covariates that violate the assumption
# Output$sig_vars
Proportional_Odds_Assumption_Test=function(Data,
                                           ColumnsToUse,
                                           Outcome_name){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Outcome_name]=factor(Data[, Outcome_name], order=T)
  
  # main algorithm
  output=c()
  lr_test=list()
  p_value=c()
  for(i in 1:length(ColumnsToUse)){
    #i=13
    Covariate<<-ColumnsToUse[i]
    model.fit=clm(as.formula(paste0(Outcome_name, " ~ ", Covariate)),
                  #nominal=as.formula(paste("~ ", ColumnsToUse[i])),
                  data=Data)
    lr_test[[i]]=nominal_test(model.fit)
    p_value[i]=lr_test[[i]]$`Pr(>Chi)`[-1]
  }
  p_value[is.na(p_value)]=1
  output$lr_test=lr_test
  output$p_value=p_value
  output$sig_vars=ColumnsToUse[p_value<0.05]
  
  return(output)
}

#**********************
#
# [ --- GAMM --- ] ----
#
#**********************
#
# GAMM_Bivariate_Plot
#
#********************
# Note that for count data (poisson distribution), the currently offset term is not included
#*******************************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# head(Data)
# GAMM_Bivariate_Plot(Data=Data,
#                     Pred_Var="age",
#                     Res_Var="outcome",
#                     Group_Var="id",
#                     #Group_Var=NA,
#                     which.family="binomial",
#                     xlab="age",
#                     ylab="outcome",
#                     title="Title",
#                     x_breaks=seq(round(min(Data$age)-5, -1), round(max(Data$age)+5, -1), by=10))
GAMM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, Group_Var=NA, which.family, xlab="", ylab="", title="", x_breaks=0){
  # result
  Out=c()
  
  # check out packages
  lapply(c("mgcv", "ggplot2", "sjPlot"), checkpackages)
  
  # Data as data frame
  Data=as.data.frame(Data)
  
  # gamm model
  gamm_model=as.formula(paste(Res_Var, "~ s(", Pred_Var, ")"))
  
  # random effect
  random_effect=list(id=~1)
  names(random_effect)=Group_Var
  
  # run gamm
  if(is.na(Group_Var)){
    gaml=gamm(gamm_model, data=Data, family=which.family)
  }else{
    gaml=gamm(gamm_model, random=random_effect, data=Data, family=which.family)
  }
  Out$gaml=gaml
  
  # save random effect
  Out$re_plot=plot_model(gaml$gam)
  
  # generate predicted data and confidence interval
  pdat=with(Data, data.frame(Pred_Var=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=100)))
  colnames(pdat)=Pred_Var
  predicted_df=data.frame(Res_Var=predict(gaml$gam, newdata=pdat, type="response", se=T)$fit, 
                          Res_Var_sd=predict(gaml$gam, newdata=pdat, type="response", se=T)$se.fit, 
                          Pred_Var=pdat[, Pred_Var])
  colnames(predicted_df)=c(Res_Var, paste0(Res_Var, "_sd"), Pred_Var)
  predicted_df$upper=predicted_df[, Res_Var]+qnorm(0.975)*predicted_df[, paste0(Res_Var, "_sd")]
  predicted_df$lower=predicted_df[, Res_Var]-qnorm(0.975)*predicted_df[, paste0(Res_Var, "_sd")]
  
  # If binomial distribution, maximum=1; minimum=0
  if(which.family=="binomial"){ # code for 'binomial' family
    predicted_df[which(predicted_df[, Res_Var]>1), Res_Var]=1
    predicted_df[which(predicted_df[, Res_Var]<0), Res_Var]=0
    
    predicted_df[which(predicted_df[, "upper"]>1), "upper"]=1
    predicted_df[which(predicted_df[, "upper"]<0), "upper"]=0
    
    predicted_df[which(predicted_df[, "lower"]>1), "lower"]=1
    predicted_df[which(predicted_df[, "lower"]<0), "lower"]=0
    
    #*****
    # plot
    Out$plot=ggplot(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), y=eval(parse(text=Res_Var)), group=1)) + 
      geom_ribbon(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), ymax=upper, ymin=lower), fill="blue", alpha=2/10) +
      geom_line(color='blue', size=0.8) + 
      geom_hline(yintercept=0) + 
      geom_hline(yintercept=1) + 
      scale_y_continuous(labels=scales::percent) + 
      #geom_point(color='black') +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=upper)) +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=lower)) +
      scale_x_continuous(breaks=x_breaks) + 
      theme_set(theme_bw()) + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }else if(which.family=="poisson"){ # code for 'poisson' family
    predicted_df[which(predicted_df[, Res_Var]<0), Res_Var]=0
    
    predicted_df[which(predicted_df[, "upper"]<0), "upper"]=0
    
    predicted_df[which(predicted_df[, "lower"]<0), "lower"]=0
    
    #*****
    # plot
    Out$plot=ggplot(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), y=eval(parse(text=Res_Var)), group=1)) + 
      geom_ribbon(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), ymax=upper, ymin=lower), fill="blue", alpha=2/10) +
      geom_line(color='blue', size=0.8) + 
      geom_hline(yintercept=0) + 
      #geom_point(color='black') +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=upper)) +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=lower)) +
      scale_x_continuous(breaks=x_breaks) +
      theme_set(theme_bw()) + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }
  
  return(Out)
  
  # Or the following simple code can be used to generate a estimated smoothing function
  #plot_model(gaml$gam, type="pred")
}

#*********************
#
# [ --- GAM --- ] ----
#
#*********************
# GAM_Bivariate_Plot
#*******************
# Note that for count data (poisson distribution), the currently offset term is not included
#*******************************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# head(Data)
# GAM_Bivariate_Plot(Data=Data,
#                    Pred_Var="age",
#                    Res_Var="outcome",
#                    which.family="binomial",
#                    xlab="age",
#                    ylab="outcome",
#                    title="Title",
#                    x_breaks=seq(round(min(Data$age)-5, -1), round(max(Data$age)+5, -1), by=10))
GAM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, which.family, xlab="", ylab="", title="", x_breaks=0){
  # check out packages
  lapply(c("mgcv", "ggplot2"), checkpackages)
  
  # Data as data frame
  Data=as.data.frame(Data)
  
  # gamm model
  gamm_model=as.formula(paste(Res_Var, "~ s(", Pred_Var, ")"))
  
  # run gamm
  gaml=gam(gamm_model, data=Data, family=which.family)
  
  # generate predicted data and confidence interval
  pdat=with(Data, data.frame(Pred_Var=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=100)))
  colnames(pdat)=Pred_Var
  predicted_df=data.frame(Res_Var=predict(gaml, newdata=pdat, type="response", se=T)$fit, 
                          Res_Var_sd=predict(gaml, newdata=pdat, type="response", se=T)$se.fit, 
                          Pred_Var=pdat[, Pred_Var])
  colnames(predicted_df)=c(Res_Var, paste0(Res_Var, "_sd"), Pred_Var)
  predicted_df$upper=predicted_df[, Res_Var]+qnorm(0.975)*predicted_df[, paste0(Res_Var, "_sd")]
  predicted_df$lower=predicted_df[, Res_Var]-qnorm(0.975)*predicted_df[, paste0(Res_Var, "_sd")]
  
  # If binomial distribution, maximum=1; minimum=0
  if(which.family=="binomial"){ # code for 'binomial' family
    predicted_df[which(predicted_df[, Res_Var]>1), Res_Var]=1
    predicted_df[which(predicted_df[, Res_Var]<0), Res_Var]=0
    
    predicted_df[which(predicted_df[, "upper"]>1), "upper"]=1
    predicted_df[which(predicted_df[, "upper"]<0), "upper"]=0
    
    predicted_df[which(predicted_df[, "lower"]>1), "lower"]=1
    predicted_df[which(predicted_df[, "lower"]<0), "lower"]=0
    
    # gaml result
    Out=c()
    Out$gaml=gaml
    # plot
    Out$plot=ggplot(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), y=eval(parse(text=Res_Var)), group=1)) + 
      geom_ribbon(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), ymax=upper, ymin=lower), fill="blue", alpha=2/10) +
      geom_line(color='blue', size=0.8) + 
      geom_hline(yintercept=0) +
      geom_hline(yintercept=1) + 
      scale_y_continuous(labels=scales::percent) + 
      #geom_point(color='black') +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=upper)) + 
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=lower)) + 
      scale_x_continuous(breaks=x_breaks) + 
      theme_set(theme_bw()) + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }else if(which.family=="poisson"){ # code for 'poisson' family
    predicted_df[which(predicted_df[, Res_Var]<0), Res_Var]=0
    
    predicted_df[which(predicted_df[, "upper"]<0), "upper"]=0
    
    predicted_df[which(predicted_df[, "lower"]<0), "lower"]=0
    
    # gaml result
    Out=c()
    Out$gaml=gaml
    # plot
    Out$plot=ggplot(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), y=eval(parse(text=Res_Var)), group=1)) + 
      geom_ribbon(data=predicted_df, aes(x=eval(parse(text=Pred_Var)), ymax=upper, ymin=lower), fill="blue", alpha=2/10) +
      geom_line(color='blue', size=0.8) + 
      geom_hline(yintercept=0) +
      #geom_point(color='black') +
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=upper)) + 
      # geom_line(color='black', data=predicted_df, linetype="dotted", aes(x=eval(parse(text=Pred_Var)), y=lower)) + 
      scale_x_continuous(breaks=x_breaks) + 
      theme_set(theme_bw()) + 
      labs(
        x=xlab, 
        y=ylab, 
        title=title
      )
  }
  
  return(Out)
}

#*************************************************************
#
# [ --- Multiple Imputation Analytic Result Combine --- ] ----
#
#*************************************************************
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
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, ColumnsToUse], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(ColumnsToUse=="treat")]="P"
# levels.of.fact[which(ColumnsToUse=="sex")]="F"
# 
# Data=Format_Columns(Data,
#                     Outcome_name="outcome",
#                     ColumnsToUse,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# # generate 100 missing data in outcome
# Data=as.data.table(Data)
# Data[sample(1:nrow(Data), 100), "outcome"]=NA
# # multiple imputation using Amelia
# set.seed(2019) # set seed in order to obtain the same imputed values for the future reproduction of analysis results
# m=2 # the number of imputed datasets to creates
# amelia.imp=amelia(Data[,
#                        .SD,
#                        .SDcols=c(ColumnsToUse, Outcome_name)],
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
# # GEE using the 1st imputed data set
# GEE.result.1=GEE_Multivariable_Jin(amelia.imp$imputations$imp1, ColumnsToUse, Outcome_name, ID_name, which.family)$summ_table %>%
#   as.data.table(keep.rownames=TRUE)
# # GEE using the 2nd imputed data set
# GEE.result.2=GEE_Multivariable_Jin(amelia.imp$imputations$imp2, ColumnsToUse, Outcome_name, ID_name, which.family)$summ_table %>%
#   as.data.table(keep.rownames=TRUE)
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
  # convert the results to the data table format
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


#***********************************
#
# [ --- Contribution Plot --- ] ----
#
#***********************************
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
# D=rmvnorm(n=n, mean=rep(0, p+q), sigma=sigma) # data generated
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
  # to the RV(X, Y| \alpha) statistic
  #
  # 1. Generate a matrix of powered covariances between columns of X and Y
  Cov=(cov(X, Y)^(2))^alpha
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
  maxs=rep(NA, nrep)
  for(i in 1:nrep){
    # record all the maximum contributions based under the estimated
    # permutation distribution
    maxs[i]=max(EstContribution(X[sample(1:nrow(X)), ], Y, alpha=alpha))
    
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
# Data=respiratory
# 
# # randomly generate NAs in some variables
# Data$sex[sample(1:nrow(Data), 30)]=NA
# Data$age[sample(1:nrow(Data), 30)]=NA
# Data$outcome[sample(1:nrow(Data), 30)]=NA
# #Data$outcome[sample(1:nrow(Data), 30)]=2
# 
# # Work on predictor with more than 2 levels
# Data=as.data.table(Data)
# Data[age<20, age_cat:="<20"]
# Data[20<=age & age<30, age_cat:="20<=age<30"]
# Data[30<=age & age<40, age_cat:="30<=age<40"]
# Data[40<=age & age<50, age_cat:="40<=age<50"]
# Data[50<=age & age<60, age_cat:="50<=age<60"]
# Data[60<=age, age_cat:="60<=age"]
# Data[, age_cat:=as.factor(age_cat)]
# 
# # Data at baseline
# BL_Data=Data %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Contingency_Table_Generator(Data=Data,
#                             Row_Var="sex",
#                             Col_Var="outcome",
#                             Ref_of_Row_Var="F",
#                             Missing="Not_Include")
# Contingency_Table_Generator(Data=Data,
#                             Row_Var="age_cat",
#                             Col_Var="outcome",
#                             Ref_of_Row_Var="<20",
#                             Missing="Include") # independence test does not account for missing data
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
# Data=respiratory
# 
# # randomly generate NAs in some variables
# Data$age[sample(1:nrow(Data), 30)]=NA
# Data$outcome[sample(1:nrow(Data), 30)]=NA
# #Data$outcome[sample(1:nrow(Data), 30)]="2"
# 
# # Data at baseline
# BL_Data=Data %>%
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

#******************************
#
# Contingency_Table_Univariable
#
#************************************
# Generate summary (contingency) table that outlines characteristics from dataset
# Var : Categorical variable
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
# Data=respiratory
# 
# # randomly generate NAs in some variables
# Data$age[sample(1:nrow(Data), 30)]=NA
# Data$outcome[sample(1:nrow(Data), 30)]=NA
# #Data$outcome[sample(1:nrow(Data), 30)]="2"
# 
# # Data at baseline
# BL_Data=Data %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Contingency_Table_Univariable(Data=BL_Data,
#                Var="sex")
Contingency_Table_Univariable=function(Data, Var, Missing="Not_Include"){
  Data=as.data.table(Data)
  
  Table_with_missing=table(Data[, .SD, .SDcols=Var], useNA="always")
  
  if(Missing=="Include"){
    Table=table(Data[, .SD, .SDcols=Var], useNA="always")
  }else if(Missing=="Not_Include"){
    Table=table(Data[, .SD, .SDcols=Var])
  }
  
  CT=t(as.matrix(paste0(Table, " (", round(Table/sum(Table_with_missing)*100, 2), "%)")))
  
  Out=cbind(Var, 
            names(Table), 
            t(CT))
  colnames(Out)=c("Variable", "Value", paste0("Total (n=", sum(Table_with_missing), ")"))
  
  return(as.data.table(Out))
}

#**************************************
#
# Contingency_Table_Univariable_Conti_X
#
#************************************
# Generate summary (contingency) table that outlines characteristics from dataset
# Var : Continuous variable
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
# Data=respiratory
# 
# # randomly generate NAs in some variables
# Data$age[sample(1:nrow(Data), 30)]=NA
# Data$outcome[sample(1:nrow(Data), 30)]=NA
# #Data$outcome[sample(1:nrow(Data), 30)]="2"
# 
# # Data at baseline
# BL_Data=Data %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Contingency_Table_Univariable_Conti_X(Data=BL_Data,
#                Var="age")
Contingency_Table_Univariable_Conti_X=function(Data, Var){
  Data=as.data.frame(Data)
  
  Table=round(summary(Data[, Var]), 2)
  
  Out=cbind(Var, paste0(Table[3], " (", Table[5]-Table[2], ")"))
  colnames(Out)=c("Variable", "Median (IQR)")
  return(as.data.table(Out))
}


#******************************************************************************
#
# Example of combining contingency tables of categorical and continuous variables
#
#******************************************************************************
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
# Data=respiratory
# 
# #
# Combined_CT=rbind(
#   Contingency_Table_Generator(Data=Data,
#                               Row_Var="sex",
#                               Col_Var="outcome",
#                               Ref_of_Row_Var="F",
#                               Missing="Include"),
#   Contingency_Table_Generator_Conti_X(Data=Data,
#                                       Row_Var="age",
#                                       Col_Var="outcome",
#                                       Missing="Include"),
#   fill=TRUE
#   )
# Combined_CT


#********************************
# Raw_Contingency_Table_Generator
#********************************
# lapply(c("dplyr",
#          "data.table",
# 
#          "lme4",
#          "epitools"
# ),
# checkpackages)
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# 
# # randomly generate NAs in some variables
# Data$sex[sample(1:nrow(Data), 30)]=NA
# Data$age[sample(1:nrow(Data), 30)]=NA
# Data$outcome[sample(1:nrow(Data), 30)]=NA
# #Data$outcome[sample(1:nrow(Data), 30)]=2
# 
# # Work on predictor with more than 2 levels
# Data=as.data.table(Data)
# Data[age<20, age_cat:="<20"]
# Data[20<=age & age<30, age_cat:="20<=age<30"]
# Data[30<=age & age<40, age_cat:="30<=age<40"]
# Data[40<=age & age<50, age_cat:="40<=age<50"]
# Data[50<=age & age<60, age_cat:="50<=age<60"]
# Data[60<=age, age_cat:="60<=age"]
# Data[, age_cat:=as.factor(age_cat)]
# 
# # Data at baseline
# BL_Data=Data %>%
#   group_by(id) %>%
#   filter(visit==min(visit)) %>%
#   ungroup()
# #
# Table_Data=Raw_Contingency_Table_Generator(Data=Data,
#                                            Row_Var="outcome",
#                                            Col_Var="age",
#                                            Value="Frequency")
# Raw_Contingency_Table_Generator(Data=Data,
#                                 Row_Var="outcome",
#                                 Col_Var="age",
#                                 Value="Percentage")
# Line_Graph_Generator(Table_Data[2, 1:(ncol(Table_Data)-1)],
#                      Y_lab="Outcome",
#                      X_lab="Age",
#                      Y_breaks=2)
Raw_Contingency_Table_Generator=function(Data, 
                                         Row_Var, 
                                         Col_Var, 
                                         Value="Frequency"){
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
  if(Value=="Frequency"){
    Merged=cbind(Row_Var, rownames(Contingency_Table), 
                 cbind(
                   paste0(Contingency_Table) %>% 
                     matrix(nrow(Contingency_Table), ncol(Contingency_Table)), 
                   paste0(Sum_Row_Wise)
                 )
    ) %>% as.data.frame()
  }
  if(Value=="Percentage"){
    Merged=cbind(Row_Var, rownames(Contingency_Table), 
                 cbind(
                   paste0(round(t(t(Contingency_Table)/Sum_Col_Wise)*100, 2)) %>% 
                     matrix(nrow(Contingency_Table), ncol(Contingency_Table)),
                   paste0(round(Sum_Row_Wise/sum(Sum_Col_Wise)*100, 2))
                 )
    ) %>% as.data.frame()
  }
  
  # post-processing
  colnames(Merged)[1:2]=c("Predictor", "Value")
  colnames(Merged)[1:2]=c("Predictor", "Value")
  colnames(Merged)[3:(3+ncol(Contingency_Table)-1)]=paste0(names(Sum_Col_Wise))
  colnames(Merged)[3+ncol(Contingency_Table)]=paste0(sum(Sum_Col_Wise))
  Out=Merged
  
  # return
  return(Out)
}

#*********************
# Line_Graph_Generator
#*********************
Line_Graph_Generator=function(Table_Data, 
                              Y_lab="Y",
                              X_lab="X",
                              Y_breaks=100){
  # make plot
  Long_Table_Data=melt(Table_Data, id=c("Predictor", "Value"))
  colnames(Long_Table_Data)=c("Group", "Nothing", "X", "Y")
  Long_Table_Data=as.data.table(Long_Table_Data)
  Long_Table_Data[, Y:=as.numeric(Y)]
  # plot - all colored
  Line_Chart=ggplot(Long_Table_Data, aes(x=X, y=Y, group=Group)) +
    geom_line(aes(color=Group))+
    geom_point(aes(color=Group))+
    ylab(Y_lab)+
    xlab(X_lab)+
    ylim(0, max(Long_Table_Data[!is.na(Y), Y]))
  scale_y_continuous(breaks=seq(0,
                                max(Long_Table_Data[!is.na(Y), Y]),
                                Y_breaks))
  
  Line_Chart+theme(axis.text=element_text(size=20),
                   axis.title=element_text(size=20),
                   legend.text=element_text(size=15))
}


#**************************************************************
#
# [ --- Markov chain Monte Carlo (MCMC) for sampling --- ] ----
#
#**************************************************************
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
# x=c(0, 0)
# VCOV=vcov2D
# # Call the sampling function with the arguments
# ringsample2D=rwmetro(ring2D, 4000, c(0, 0), vcov2D)
# # Use the sample
# plot(ringsample2D[, 1], ringsample2D[, 2], xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), main='Metropolis-Hastings Sample', xlab='x', ylab='y', pch='.')
rwmetro=function(target, N, x, VCOV, burnin=0){
  require(MASS)   #requires package MASS for normal sampling
  samples=x
  for(i in 2:(burnin+N)){
    prop=mvrnorm(n=1, x, VCOV)
    if(runif(1) < min(1, target(prop)/target(x)))
      x=prop
    samples=rbind(samples, x)
  }
  samples[(burnin+1):(N+burnin), ]
}


#*********************
# 
# [ --- Etc --- ] ---- 
# 
#*********************
# Mortgage_Calculator 
#******************** 
# Example
#********
# Mortgage_Calculator(HP=1000000, DP=200000,
#                     ANIR=0.04, AP=25,
#                     ID="2020-02-01", PF="Monthly")
Mortgage_Calculator=function(HP=500000,          # House Price 
                             DP=100000,          # Down Payment 
                             ANIR=0.04,          # Annual Nominal Interest Rate 
                             AP=15,              # Amortization Period (years) 
                             ID="2020-02-01",    # Initial Date of Amortization 
                             PF="Monthly"){      # "Monthly"|"Bi-Weekly" 
  
  IP=HP-DP # Initial Principal 
  
  if(PF=="Monthly"){ 
    AA=12 # Annual_Amortization 
  }else if(PF=="Bi-Weekly"){ 
    AA=26 
  } 
  IR=((1+(ANIR/2))^2)^(1/AA)-1 # Interest Rate 
  PP=(IP*IR)/(1-(1+IR)^(-AP*AA)) # Periodic Payment 
  
  # generate Amortization Schedule Worksheet  
  Periodic_Table=data.table( 
  ) 
  if(PF=="Monthly"){Periodic_Table[, Date:=seq(as.Date(ID), by="1 month", length=(AP*AA)+1)]} 
  if(PF=="Bi-Weekly"){Periodic_Table[, Date:=seq(as.Date(ID), by="2 weeks", length=(AP*AA)+1)]} 
  Periodic_Table[, Payment:=c(0, rep(PP, AP*AA))] 
  for(i in 1:(AA*AP+1)){ 
    if(i==1){ 
      Periodic_Table[i, Interest:=0] 
      Periodic_Table[i, Principal:=0] 
      Periodic_Table[i, Balance:=IP] # Remaining Principal 
    }else{ 
      Periodic_Table[i, Interest:=IR*Periodic_Table[i-1, Balance]] 
      Periodic_Table[i, Principal:=round(Payment-Interest, 2)] 
      Periodic_Table[i, Balance:=round(Periodic_Table[i-1, Balance]-Principal)] 
    } 
  } 
  return(as.data.table(Periodic_Table)) 
}












