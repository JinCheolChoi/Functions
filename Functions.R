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

#***********
# cbind.fill
#***********
cbind.fill=function(...){
  nm=list(...) 
  dfdetect=grepl("data.frame|matrix", unlist(lapply(nm, function(cl) paste(class(cl), collapse = " ") )))
  # first cbind vectors together 
  vec=data.frame(nm[!dfdetect])
  n=max(sapply(nm[dfdetect], nrow)) 
  vec=data.frame(lapply(vec, function(x) rep(x, n)))
  if(nrow(vec)>0) nm <- c(nm[dfdetect], list(vec))
  nm=lapply(nm, as.data.frame)
  
  do.call(cbind, lapply(nm, function (df1) 
    rbind(df1, as.data.frame(matrix(NA, ncol=ncol(df1), nrow=n-nrow(df1), dimnames=list(NULL, names(df1))))) )) 
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


#*************************************************
#
# [ --- ITS ( Interrupted Time Series ) --- ] ----
#
#*************************************************
# Segmented_Regression_Model
#***************************
# Example
#********
# lapply(c("ggplot2"), checkpackages)
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
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  Data[, Res_Var]=as.numeric(as.character(Data[, Res_Var]))
  
  Data[, Time_Var]=as.factor(Data[, Time_Var])
  Data$Time_Order=as.numeric(Data[, Time_Var])
  
  # fit model
  model_fit=lm(as.formula(paste(Res_Var, "~", Int_Var, "*Time_Order")), data=Data)
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  
  # Output
  Output=c()
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  Output$summ_table=as.data.frame(summary(model_fit)$coefficients)
  colnames(Output$summ_table)=c("Estimate", "Std.Error", "T-value", "P-value")
  Output$summ_table$Estimate=round(Output$summ_table$Estimate, 3)
  Output$summ_table$Std.Error=round(Output$summ_table$Std.Error, 3)
  Output$summ_table$`T-value`=round(Output$summ_table$`T-value`, 3)
  Output$summ_table$`P-value`=ifelse(Output$summ_table$`P-value`<0.001, "<0.001", round(Output$summ_table$`P-value`, 3)) 
  
  return(Output)
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

#************************
#
# [ --- COX PH --- ] ----
#
#************************
# COX_Bivariate
#**************
# Example
#********
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,1,1,0,1),
#                  x3=c(0,1,2,2,2,0,1,0,1,0,2,2,0,1,0,0,1,1,0,2))
# Data_to_use$x3=as.factor(Data_to_use$x3)
# #
# COX_Bivariate(Data=Data_to_use,
#               # Pred_Vars=list("x1",
#               #                c("x2", "x3", "x2:x3")), #!!! for now, algorithm works the best with no interaction term (PH_assumption_P.value needs to be further touched for merging in output)
#               Pred_Vars=list("x1", "x2"),
#               Res_Var="event",
#               Group_Vars="id",
#               Strat_Vars="x3",
#               Start_Time="start",
#               Stop_Time="stop")
COX_Bivariate=function(Data,
                       Pred_Vars,
                       Res_Var,
                       Group_Vars=NULL,
                       Strat_Vars=NULL,
                       Start_Time=NULL,
                       Stop_Time,
                       Message="Yes"){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    #i=1
    if(i>=2){
      Message="No"
    }
    # run model
    Temp=COX_Multivariable(Data=Data,
                           Pred_Vars=unlist(Pred_Vars[i]),
                           Res_Var=Res_Var,
                           Group_Vars=Group_Vars,
                           Strat_Vars=Strat_Vars,
                           Start_Time=Start_Time,
                           Stop_Time=Stop_Time,
                           Message=Message)
    Output=rbind(Output,
                 cbind(Temp$summ_table,
                       N_events=Temp$N_events,
                       PH_assumption_P.value=Temp$cox.zph$table[1, "p"],
                       N_non_missing_data=Temp$N_non_missing_data))
  }
  return(Output)
}


#******************
# COX_Multivariable
#******************
# Example
#********
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,1,1,0,1),
#                  x3=c(0,1,2,2,2,0,1,0,1,0,2,2,0,1,0,0,1,1,0,2))
# Data_to_use$x3=as.factor(Data_to_use$x3)
# 
# COX_Multivariable(Data=Data_to_use,
#                   # Pred_Vars=c("x1", "x2", "x3", "x2:x3"), #!!!! for now, algorithm works the best with no interaction term (PH_assumption_P.value needs to be further touched for merging in output)
#                   Pred_Vars=c("x1", "x2"),
#                   Res_Var="event",
#                   Group_Vars="id",
#                   Strat_Vars="x3",
#                   Start_Time="start",
#                   Stop_Time="stop")
COX_Multivariable=function(Data,
                           Pred_Vars,
                           Res_Var,
                           Group_Vars=NULL,
                           Strat_Vars=NULL,
                           Start_Time=NULL,
                           Stop_Time,
                           Message="Yes"){
  # check out packages
  lapply(c("survival", "data.table", "riskRegression"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # main algorithm
  Output=c()
  #i=1
  # run model
  if(!is.null(Group_Vars)){
    Group_Parts=paste0("+", paste0(" cluster(", Group_Vars, ")", collapse=" +"))
  }else{Group_Parts=""}
  if(!is.null(Strat_Vars)){
    Strat_Parts=paste0("+", paste0(" strata(", Strat_Vars, ")", collapse=" +"))
  }else{Strat_Parts=""}
  
  if(is.null(Start_Time)){
    fullmod=as.formula(paste("Surv(", Stop_Time, ",", Res_Var, ") ~", paste(Pred_Vars, collapse="+"), Group_Parts, Strat_Parts))
  }else{
    fullmod=as.formula(paste("Surv(", Start_Time, ",", Stop_Time, ",", Res_Var, ") ~", paste(Pred_Vars, collapse="+"), Group_Parts, Strat_Parts))
  }
  
  model_fit=coxph(fullmod, na.action=na.exclude, data=Data)
  
  # number of events from a model fit
  Used_N_Rows=nobs(model_fit)
  
  # number of non-missing observations
  Used_Non_Missing_N=coxN(model_fit)
  
  Output$N_events=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$N_non_missing_data=paste0(Used_Non_Missing_N, "/", Origin_N_Rows, " (", round(Used_Non_Missing_N/Origin_N_Rows*100, 2), "%)") 
  Output$fullmod=fullmod
  Output$model_fit=model_fit
  
  # Examine the proportional hazards assumption that hazard ratio is constant over time, 
  # or equivalently, that the hazard for one individual is proportional to the hard for any other individual, 
  # where the proportionality constant is independent of time.
  # In other words, the hazard ratio comparing any two specifications of predictors is constant over time.
  # Rejecting 'age' means that there is strong evidence of non-proportional hazards for age.
  # cox.zph : Test the assumption based on Schoenfeld residuals
  Output$cox.zph=cox.zph(model_fit)
  Output$cox.zph$table[, "p"]=ifelse(Output$cox.zph$table[, "p"]<0.001, "<0.001", 
                                     Output$cox.zph$table[, "p"])
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  Fit.Summary=summary(model_fit)
  HR.Coefficients=Fit.Summary$coefficients
  HR.Conf.int=Fit.Summary$conf.int
  
  # Output
  if(is.null(Group_Vars)){ # if cluster (group) variable is not specified, report regular standard errors
    Std.Error=round2(HR.Coefficients[, "se(coef)"], 3)
  }else{ # if cluster (group) is specified, report robust standard errors
    Std.Error=round2(HR.Coefficients[, "robust se"], 3)  }
  
  Output$summ_table=rbind(Output$summ_table, 
                          data.frame(
                            Estimate=round2(HR.Coefficients[, "coef"], 3),
                            Std.Error=Std.Error,
                            `P-value`=ifelse(HR.Coefficients[, "Pr(>|z|)"]<0.001, "<0.001", 
                                             format(round2(HR.Coefficients[, "Pr(>|z|)"], 3), nsmall=3)),
                            HR.and.CI=paste0(format(round2(HR.Conf.int[, "exp(coef)"] , 2), nsmall=2),
                                             " (", format(round2(as.numeric(HR.Conf.int[, "lower .95"]), 2), nsmall=2), " - ", 
                                             format(round2(as.numeric(HR.Conf.int[, "upper .95"]), 2), nsmall=2), ")"),
                            row.names=names(coef(model_fit))
                          )
  )
  
  # print a note
  if(Message=="Yes"){
    print("Note : PH Assumption is not necessary for the extended Cox model that includes time-variant variables (so, the data is formatted in the counting process layout).")
  }
  return(Output)
}


#**************************
# COX_Confounder_Selection
#**************************
# Example
#********
# for now, algorithm works with no interaction term
#**************************************************
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,1,1,0,1),
#                  x3=c(0,1,2,2,2,0,1,0,1,0,2,2,0,1,0,0,1,1,0,2))
# Data_to_use$x3=as.factor(Data_to_use$x3)
# 
# COX.fit=COX_Multivariable(Data<-Data_to_use,
#                           Pred_Vars<-c("x1", "x2"),
#                           Res_Var="event",
#                           # Group_Vars="id",
#                           Strat_Vars=c("x3"),
#                           Start_Time="start",
#                           Stop_Time="stop")
# Confounder_Steps=COX_Confounder_Selection(Full_Model=COX.fit$model_fit,
#                                           Main_Pred_Var="x1",
#                                           Potential_Con_Vars=Pred_Vars[Pred_Vars!="x1"], # for now, algorithm works with no interaction term
#                                           Min.Change.Percentage=5,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
COX_Confounder_Selection=function(Full_Model, 
                                  Main_Pred_Var, 
                                  Potential_Con_Vars, 
                                  Min.Change.Percentage=5,
                                  Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
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
    
    # run COX_PH model excluding one variable at once
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
      Temp_Table=data.table(
        Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
        Est_HR=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
        Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
        Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
      )
    }
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than Min.Change.Percentage, terminate the while loop
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
# COX_Confounder_Model
#**********************
# Example
#**************
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,1,1,0,1),
#                  x3=c(0,1,2,2,2,0,1,0,1,0,2,2,0,1,0,0,1,1,0,2))
# Data_to_use$x3=as.factor(Data_to_use$x3)
# 
# Pred_Vars=c("x1", "x2")
# Potential_Con_Vars=Pred_Vars[Pred_Vars!="x1"]
# COX_Confounder=COX_Confounder_Model(Data=Data_to_use,
#                                     Main_Pred_Var="x1",
#                                     Potential_Con_Vars=Pred_Vars[Pred_Vars!="x1"], # for now, algorithm works with no interaction term
#                                     Res_Var="event",
#                                     Group_Vars="id",
#                                     Strat_Vars="x3",
#                                     Start_Time="start",
#                                     Stop_Time="stop",
#                                     Min.Change.Percentage=5,
#                                     Estimate="raw_estimate") # raw_estimate, converted_estimate
# COX_Confounder$Full_Multivariable_Model$summ_table
# COX_Confounder$Confounder_Steps$Confounders
# COX_Confounder$Confounder_Model$summ_table
COX_Confounder_Model=function(Data,
                              Main_Pred_Var,
                              Potential_Con_Vars,
                              Res_Var,
                              Group_Vars=NULL,
                              Strat_Vars=NULL,
                              Start_Time=NULL,
                              Stop_Time="End_Time",
                              Min.Change.Percentage=5,
                              Estimate="raw_estimate"){
  # check out packages
  lapply(c("data.table"), checkpackages)
  
  # Output
  Output=c()
  
  # convert Data to data frame
  Data=as.data.frame(Data)
  
  # Full multivariable model
  Pred_Vars=c(Main_Pred_Var, Potential_Con_Vars)
  
  Output$Full_Multivariable_Model=COX_Multivariable(Data<<-Data,
                                                    Pred_Vars<<-Pred_Vars,
                                                    Res_Var<<-Res_Var,
                                                    Group_Vars<<-Group_Vars,
                                                    Strat_Vars<<-Strat_Vars,
                                                    Start_Time<<-Start_Time,
                                                    Stop_Time<<-Stop_Time)
  
  # Confounder selection
  Confounder_Steps=COX_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                            Main_Pred_Var=Main_Pred_Var,
                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var],
                                            Min.Change.Percentage=Min.Change.Percentage,
                                            Estimate=Estimate) # raw_estimate, converted_estimate
  Output$Confounder_Steps=Confounder_Steps
  Confounder_Ind=which(Pred_Vars%in%Output$Confounder_Steps$Confounders)
  
  # Data=Remove_missing(Data, # remove missing data
  #                     c(Pred_Vars[Confounder_Ind],
  #                       Res_Var,
  #                       Group_Vars))
  
  # Multivariable model with confounders
  Output$Confounder_Model=COX_Multivariable(Data<<-Data,
                                            Pred_Vars<<-Pred_Vars[Confounder_Ind],
                                            Res_Var<<-Res_Var,
                                            Group_Vars<<-Group_Vars,
                                            Strat_Vars<<-Strat_Vars,
                                            Start_Time<<-Start_Time,
                                            Stop_Time<<-Stop_Time)
  
  return(Output)
}


#********
# KM_Plot
#********
# Example
#********
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,1,0,1,0,0,1,1,0,1,1,1,1,0,1))
# 
# KM_Plot(Data=Data_to_use,
#         Stop_Time="stop",
#         Res_Var="event",
#         Pred_Vars="x2",
#         xlab="xlab",
#         ylab="ylab",
#         title="title",
#         font.x=25,
#         font.y=25,
#         font.tickslab=25,
#         font.legend=25,
#         font.title=25,
#         conf.int=T,
#         palette=c("red3", "green3"))
KM_Plot=function(Data,
                 Start_Time=NULL,
                 Stop_Time,
                 Res_Var,
                 Pred_Vars="1",
                 ...){
  if(!is.null(Start_Time)){
    fullmod=as.formula(paste("Surv(", Start_Time, ", ", Stop_Time, ",", Res_Var, ")", "~", paste(Pred_Vars, collapse="+")))
  }else{
    fullmod=as.formula(paste("Surv(", Stop_Time, ",", Res_Var, ")", "~", paste(Pred_Vars, collapse="+")))
  }
  
  # conduct Log-rank test if the model is not a null model
  if(sum(Pred_Vars!="1")>0){
    surv_diff=do.call(survdiff, args=list(formula=fullmod, data=Data))
    p.val=max(0.001, round(1-pchisq(surv_diff$chisq, length(surv_diff$n)-1), 3))
    p.val=ifelse(p.val=="0.001", "< 0.001", p.val)
    
    if(p.val=="< 0.001"){
      Log_rank_test=paste0("Log rank p ", p.val)
    }else{
      Log_rank_test=paste0("Log rank p = ", p.val)
    }
    
    # pairwise comparisons between group levels in case there are more than 2 groups
    pairwise_test=pairwise_survdiff(formula=fullmod, data=Data)
    
  }else{ # if the model is a null model
    Log_rank_test=""
    pairwise_test=""
  }
  
  # survfit_output
  survfit_output=do.call(survfit, args=list(formula=fullmod, data=Data))
  
  # ggsurvplot
  (ggsurv_plot=ggsurvplot(
    fit=survfit_output,
    ...
  )$plot+
      annotate("text", x=0, y=0.2, 
               label=Log_rank_test,
               cex=15, col="black", 
               vjust=0, hjust=0,
               fontface=4)+
      guides(color=guide_legend(override.aes=list(size=1),
                                keywidth=3,
                                keyheight=3)))
  
  # ggsurvplot(
  #   fit=do.call(survfit, args=list(formula=fullmod, data=Data)),
  #   xlab="Days",
  #   ylab="Retention Rate",
  #   title="Kaplan-Meier curve for retention rate in assigned treatment on all randomized participants (mITT==1), stratified by fentanyl use at SCR",
  #   font.x=25,
  #   font.y=25,
  #   font.tickslab=25,
  #   font.legend=25,
  #   font.title=25
  # )$plot+
  #   annotate("text", x=0, y=0.2, 
  #            label=Log_rank_test,
  #            cex=10, col="black", 
  #            vjust=0, hjust=0,
  #            fontface=4)
  
  # output
  output=c()
  output$survfit_output=survfit_output
  output$ggsurv_plot=ggsurv_plot
  output$pairwise_test=pairwise_test
  
  return(output)
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
# GLM_Bivariate(Data=Data_to_use,
#               Pred_Vars=list("center",
#                              c("sex", "age", "sex:age")),
#               Res_Var=Res_Var,
#               which.family="binomial (link='logit')")
GLM_Bivariate=function(Data, Pred_Vars, Res_Var, which.family){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    #i=1
    # run model
    Temp=GLM_Multivariable(Data=Data,
                           Pred_Vars=unlist(Pred_Vars[i]),
                           Res_Var=Res_Var,
                           which.family=which.family)
    Output=rbind(Output,
                 cbind(Temp$summ_table[, c(1, 2, 3, 5)],
                       Data_Used=Temp$N_data_used))
    
    # print out progress
    if(sum(grepl(":", Pred_Vars[i]))>0){
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", unlist(Pred_Vars[i])[grepl(":", unlist(Pred_Vars[i]))], " (", i ," out of ", length(Pred_Vars), ")"))
    }else{
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
    }
  }
  
  return(Output)
}


#******************
# GLM_Multivariable
#******************
# lapply(c("stats", "geepack", "doBy"), checkpackages)
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
#                   Pred_Vars=c("center", "sex", "age", "sex:age"),
#                   Res_Var=Res_Var,
#                   which.family="binomial (link='logit')")
GLM_Multivariable=function(Data, Pred_Vars, Res_Var, which.family){
  # check out packages
  lapply(c("MASS", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  #Data<<-na.exclude(Data[, c(Pred_Vars, Res_Var)])
  
  # main algorithm
  Output=c()
  #i=1
  # run model
  fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+")))
  model_fit=glm(fullmod, family=eval(parse(text=which.family)), na.action=na.exclude, data=Data)
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  
  # vif
  if(length(Pred_Vars)>=2){Output_vif=car::vif(model_fit)}else{Output_vif=""}
  
  # Output
  if(grepl("gaussian", which.family)){
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    Est.CI=cbind(OR=coef(model_fit), confint(model_fit, level=0.95))
    colnames(Est.CI)=c("Odds Ratio", "Lower RR", "Upper RR")
    est=cbind(summary(model_fit)$coefficients, Est.CI)
    
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
  }else if(grepl("binomial", which.family)){ # default with the logit link function
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    OR.CI=exp(cbind(OR=coef(model_fit), confint(model_fit, level=0.95)))
    colnames(OR.CI)=c("Odds Ratio", "Lower OR", "Upper OR")
    est=cbind.fill(summary(model_fit)$coefficients, OR.CI)
    
    Output$summ_table=rbind(Output$summ_table,
                            data.frame(
                              Estimate=round2(est[-1, "Estimate"], 3),
                              Std.Error=round2(est[-1, "Std. Error"], 3),
                              `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001",
                                               format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
                              OR.and.CI=paste0(format(round2(est[-1, "Odds Ratio"] , 2), nsmall=2),
                                               " (", format(round2(as.numeric(est[-1, "Lower OR"]), 2), nsmall=2), " - ",
                                               format(round2(as.numeric(est[-1, "Upper OR"]), 2), nsmall=2), ")"),
                              row.names=names(coef(model_fit))[-1]
                            )
    )
    # }else if(which.family=="binomial (link='log')"){ # log-binomial regression with the log link function
    #   # IndivID_vecual Wald test and confID_vecence interval for each parameter
    #   RR.CI=exp(cbind(OR=coef(model_fit), confint(model_fit, level=0.95)))
    #   colnames(RR.CI)=c("Odds Ratio", "Lower RR", "Upper RR")
    #   est=cbind(summary(model_fit)$coefficients, RR.CI)
    #   
    #   Output$summ_table=rbind(Output$summ_table,
    #                           data.frame(
    #                             Estimate=round2(est[-1, "Estimate"], 3),
    #                             Std.Error=round2(est[-1, "Std. Error"], 3),
    #                             `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001",
    #                                              format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
    #                             RR.and.CI=paste0(format(round2(est[-1, "Odds Ratio"] , 2), nsmall=2),
    #                                              " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ",
    #                                              format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"),
    #                             row.names=names(coef(model_fit))[-1]
    #                           )
    #   )
  }else if(grepl("poisson", which.family)){
    # IndivID_vecual Wald test and confID_vecence interval for each parameter
    RR.CI=exp(cbind(OR=coef(model_fit), confint(model_fit, level=0.95)))
    colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
    est=cbind(summary(model_fit)$coefficients, RR.CI)
    
    Output$summ_table=rbind(Output$summ_table,
                            data.frame(
                              Estimate=round2(est[-1, "Estimate"], 3),
                              Std.Error=round2(est[-1, "Std. Error"], 3),
                              `P-value`=ifelse(est[-1, "Pr(>|z|)"]<0.001, "<0.001",
                                               format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)),
                              RR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] , 2), nsmall=2),
                                               " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ",
                                               format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"),
                              row.names=names(coef(model_fit))[-1]
                            )
    )
  }
  
  Output$summ_table=as.data.table(Output$summ_table, keep.rownames=TRUE)
  
  if(!is.null(dim(Output_vif))){
    Output$summ_table=cbind(Output$summ_table[, c(1, 5, 4)],
                            GVIF=rep(Output_vif[, 3], Output_vif[, 2]),
                            N_data_used=N_data_used)
  }else{
    Output$summ_table=cbind(Output$summ_table[, c(1, 5, 4)],
                            VIF=Output_vif,
                            N_data_used=N_data_used)
  }
  
  return(Output)
}

#*************************
# GLM_Confounder_Selection
#*************************
# # for now, algorithm works with no interaction term
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
# GLM.fit=GLM_Multivariable(Data=Data_to_use,
#                           Pred_Vars=Pred_Vars,
#                           Res_Var=Res_Var,
#                           which.family="binomial (link='logit')")
# # The value 'Data' must be defined!
# Data=Data_to_use
# Confounder_Steps=GLM_Confounder_Selection(Full_Model=GLM.fit$model_fit,
#                                           Main_Pred_Var="sex",
#                                           Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                           which.family="binomial (link='logit')", # distribution of the response variable
#                                                                                   # this must be identical to that used in GLM_Multivariable
#                                           Min.Change.Percentage=5,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
GLM_Confounder_Selection=function(Full_Model, 
                                  Main_Pred_Var, 
                                  Potential_Con_Vars, 
                                  which.family="binomial",
                                  Min.Change.Percentage=5,
                                  Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
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
      if(grepl("gaussian", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("binomial", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("poisson", which.family)){
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
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than Min.Change.Percentage, terminate the while loop
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
# GLM.fit=GLM_Multivariable(Data=Data_to_use,
#                           Pred_Vars=Pred_Vars,
#                           Res_Var=Res_Var,
#                           which.family="binomial (link='logit')")
# # 'Data' argument must be declared with '<-'!
# Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"]
# GLM_Confounder=GLM_Confounder_Model(Data<-Data_to_use,
#                                     Main_Pred_Var="sex",
#                                     Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                     Res_Var="outcome",
#                                     which.family="binomial (link='logit')", # gaussian, binomial, poisson
#                                     Min.Change.Percentage=5,
#                                     Estimate="raw_estimate") # raw_estimate, converted_estimate
# GLM_Confounder$Full_Multivariable_Model$summ_table
# GLM_Confounder$Confounder_Steps$Confounders
# GLM_Confounder$Confounder_Model$summ_table
GLM_Confounder_Model=function(Data,
                              Main_Pred_Var,
                              Potential_Con_Vars,
                              Res_Var,
                              which.family,
                              Min.Change.Percentage=5,
                              Estimate="raw_estimate"){
  Output=c()
  # Full multivariable model
  Pred_Vars=c(Main_Pred_Var, Potential_Con_Vars)
  Output$Full_Multivariable_Model=GLM_Multivariable(Data,
                                                    Pred_Vars=Pred_Vars,
                                                    Res_Var=Res_Var,
                                                    which.family=which.family)
  
  # Confounder selection
  Confounder_Steps=GLM_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                            Main_Pred_Var=Main_Pred_Var,
                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var],
                                            which.family=which.family, # distribution of the response variable
                                            Min.Change.Percentage=Min.Change.Percentage,
                                            Estimate=Estimate) # raw_estimate, converted_estimate
  
  # save all stepwise procedure
  Output$Confounder_Steps=Confounder_Steps
  
  # index of confounders
  Confounder_Ind=which(Pred_Vars%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=GLM_Multivariable(Data,
                                            Pred_Vars=Pred_Vars[Confounder_Ind],
                                            Res_Var=Res_Var,
                                            which.family=which.family)
  return(Output)
}

#*****************
# GLM_NB_Bivariate
#*****************
# Run simple GLM models for each explanatory variable with negative binomial distribution 
# to account for zero-inflated data (i.e. the excessive number of 0s).
#********
# Example
#*****************
# lapply(c("geepack"), checkpackages)
# require(dplyr)
# data("respiratory")
# Data_to_use=respiratory %>%
#   group_by(id) %>%
#   mutate(count_outcome=sum(outcome)) %>%
#   filter(visit==max(visit)) %>%
#   ungroup(id) %>%
#   dplyr::select(-outcome)
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="count_outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLM_NB_Bivariate(Data=Data_to_use,
#                  Pred_Vars=list("center",
#                                 c("sex", "age", "sex:age")),
#                  Res_Var="count_outcome",
#                  Offset_name="visit")
GLM_NB_Bivariate=function(Data, Pred_Vars, Res_Var, Offset_name){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    # run model
    Temp=GLM_NB_Multivariable(Data=Data,
                              Pred_Vars=unlist(Pred_Vars[i]),
                              Res_Var=Res_Var,
                              Offset_name=Offset_name)
    Output=rbind(Output,
                 cbind(Temp$summ_table,
                       Data_Used=Temp$N_data_used))
    
    # print out progress
    if(sum(grepl(":", Pred_Vars[i]))>0){
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", unlist(Pred_Vars[i])[grepl(":", unlist(Pred_Vars[i]))], " (", i ," out of ", length(Pred_Vars), ")"))
    }else{
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
    }
  }
  return(Output)
}

#*********************
# GLM_NB_Multivariable
#*********************
# Run a multiple GLM model with negative binomial distribution to account for zero-inflated data
# (i.e. the excessive number of 0s).
#********
# Example
#*****************
# lapply(c("geepack"), checkpackages)
# require(dplyr)
# data("respiratory")
# Data_to_use=respiratory %>%
#   group_by(id) %>%
#   mutate(count_outcome=sum(outcome)) %>%
#   filter(visit==max(visit)) %>%
#   ungroup(id) %>%
#   dplyr::select(-outcome)
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="count_outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GLM_NB_Multivariable(Data=Data_to_use,
#                      Pred_Vars=c("center", "sex", "age", "sex:age"),
#                      Res_Var="count_outcome",
#                      Offset_name="visit")
GLM_NB_Multivariable=function(Data, Pred_Vars, Res_Var, Offset_name){
  # check out packages
  lapply(c("MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # run model
  fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), "+offset(log(", Offset_name, "))"))
  model_fit=glm.nb(fullmod, data=Data, control=glm.control(maxit=25)) # maxit=25 is default
  
  # this is to avoide an error "profiling has found a better solution, so original fit had not converged"
  class(model_fit)=c("myglm", class(model_fit))
  confint.myglm=confint.default
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  RR.CI=exp(cbind(RR=coef(model_fit), confint(model_fit, level=0.95)))
  colnames(RR.CI)=c("Rate Ratio", "Lower RR", "Upper RR")
  est=cbind(summary(model_fit)$coefficients, RR.CI)
  
  # Output
  Output=c()
  
  # vif
  if(length(Pred_Vars)>=2){Output$vif=car::vif(model_fit)}else{Output$vif=""}
  
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  Output$summ_table=data.frame(Estimate=round2(est[-1, "Estimate"], 3), 
                               Std.Error=round2(est[-1, "Std. Error"], 3), 
                               `P-value`=ifelse(round2(est[-1, "Pr(>|z|)"], 3)<0.001, "<0.001", 
                                                format(round2(est[-1, "Pr(>|z|)"], 3), nsmall=3)), 
                               RR.and.CI=paste0(format(round2(est[-1, "Rate Ratio"] , 2), nsmall=2), 
                                                " (", format(round2(as.numeric(est[-1, "Lower RR"]), 2), nsmall=2), " - ", 
                                                format(round2(as.numeric(est[-1, "Upper RR"]), 2), nsmall=2), ")"), 
                               row.names=names(coef(model_fit))[-1]
  )
  return(Output)
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
  # Output
  Output=c()
  
  # change data type & remove missing data 
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Res_Var, Pred_Var)])
  
  # fit GLMM
  fullmod=as.formula(paste(Res_Var, "~", Pred_Var, sep=""))
  Model=glm(fullmod, family=which.family, na.action=na.exclude, data=Data)
  Output$glm=Model
  
  # prediction
  newdat=expand.grid(Pred_Var=seq(min(Data[, Pred_Var]), max(Data[, Pred_Var]), length=50))
  colnames(newdat)=c(Pred_Var)
  fit=predict(Model, newdat, type="response", se.fit=T)
  newdat$fit=fit$fit
  colnames(newdat)=c(Pred_Var, Res_Var)
  
  # confidence interval
  newdat$plo=fit$fit-qnorm(0.975)*fit$se.fit
  newdat$pho=fit$fit+qnorm(0.975)*fit$se.fit
  
  if(grepl("binomial", which.family)){ # code for 'binomial' family
    newdat[newdat$plo<0, "plo"]=0
    newdat[newdat$pho>1, "pho"]=1
    
    colnames(newdat)=c(Pred_Var, Res_Var, "CI_Lower", "CI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    Output$plot=ggplot(data=newdat, aes(x=eval(parse(text=Pred_Var)),                          
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
  }else if(grepl("poisson", which.family)){ # code for 'poisson' family
    newdat[newdat$plo<0, "plo"]=0
    
    colnames(newdat)=c(Pred_Var, Res_Var, "CI_Lower", "CI_Upper")
    
    #*****
    # plot
    # The following code produces the same plot as plot_model(Model, type="pred")
    library(ggplot2)
    Output$plot=ggplot(data=newdat, aes(x=eval(parse(text=Pred_Var)),                          
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
  
  
  return(Output)
}


#*********************
#
# [ --- GEE --- ] ----
#
#*********************
# GEE_Bivariate
#******************
# Example
#******************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use[sample(1:nrow(Data_to_use), 20), "treat"]=NA
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GEE_Bivariate(Data=Data_to_use,
#               Pred_Vars=list("center",
#                              "treat",
#                              c("sex", "age", "sex:age")),
#               Res_Var="outcome",
#               Group_Var="id",
#               which.family="binomial (link='logit')")
GEE_Bivariate=function(Data, Pred_Vars, Res_Var, Group_Var, which.family="binomial"){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    Temp=GEE_Multivariable(Data=Data,
                           Pred_Vars=unlist(Pred_Vars[i]),
                           Res_Var=Res_Var,
                           Group_Var=Group_Var,
                           which.family=which.family)
    
    Output=rbind(Output,
                 cbind(Temp$summ_table,
                       Data_Used=Temp$N_data_used))
    
    # print out progress
    if(sum(grepl(":", Pred_Vars[i]))>0){
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", unlist(Pred_Vars[i])[grepl(":", unlist(Pred_Vars[i]))], " (", i ," out of ", length(Pred_Vars), ")"))
    }else{
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
    }
  }
  return(Output)
}

#******************
# GEE_Multivariable
#******************
# Example
#************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# # generate missing data
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use[sample(1:nrow(Data_to_use), 20), "treat"]=NA
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# # run GEE_Multivariable
# GEE_Multivariable(Data=Data_to_use,
#                   Pred_Vars=c("center", "treat", "sex", "age", "sex:age"),
#                   Res_Var="outcome",
#                   Group_Var=c("id"),
#                   which.family="binomial (link='logit')")
GEE_Multivariable=function(Data, Pred_Vars, Res_Var, Group_Var, which.family){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # Convert code to numeric/factor (This is very important when running gee! Whether it is numeric or factor doesn't matter. They produce the same result!)
  Data[, Group_Var]=as.numeric(as.factor(Data[, Group_Var]))
  
  # number of observations from a model fit
  if(sum(grepl(":", Pred_Vars))>0){
    Data=na.omit(Data[, c(Pred_Vars[!grepl(":", Pred_Vars)], Res_Var, Group_Var)])
    Used_N_Rows=nrow(Data)
  }else{
    Data=na.omit(Data[, c(Pred_Vars, Res_Var, Group_Var)])
    Used_N_Rows=nrow(Data)
  }
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+")))
  model_fit=geeglm(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"))),
                   data=Data, 
                   id=Data[, Group_Var],
                   family=eval(parse(text=which.family)),
                   corstr="exchangeable")
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(model_fit, diag(length(coef(model_fit))))[-1, ]
  
  # Output
  Output=c()
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  #Output$vif=HH::vif(model_fit)
  
  if(grepl("gaussian", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                                        " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                                        format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
  }else if(grepl("binomial", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
    
  }else if(grepl("poisson", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
  }
  Output$summ_table=as.data.table(Output$summ_table, keep.rownames=TRUE)
  return(Output)
}

#***************************
# GEE_Multivariable_with_vif
#***************************
# Example
#**************************************************************************
# Note : 1. Argument must be declared with '<-' in a function for HH::vif!
#**************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_original=respiratory
# # generate missing data
# Data_original$outcome[1:5]=NA
# Data_original$treat[c(3, 7, 35, 74)]=NA
# Data_original$id[c(6, 25, 45, 98)]=NA
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_original[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Res_Var="outcome"
# Group_Var="id"
# #
# Data_original=Format_Columns(Data_original,
#                              Res_Var="outcome",
#                              Pred_Vars,
#                              vector.OF.classes.num.fact,
#                              levels.of.fact)
# # run GEE_Multivariable
# GEE_Multivariable_with_vif(Data<-Data_original,
#                            Pred_Vars<-c("center", "sex", "age", "sex:age"),
#                            Res_Var<-Res_Var,
#                            Group_Var<-Group_Var,
#                            which.family<-"binomial (link='logit')")
# Data_original=as.data.table(Data_original)
GEE_Multivariable_with_vif=function(Data, Pred_Vars, Res_Var, Group_Var, which.family){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy", "HH", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # as data frame
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  if(sum(grepl(":", Pred_Vars))>0){
    Non_Missing_Data<<-Remove_missing(Data, # remove missing data
                                      c(Pred_Vars[!grepl(":", Pred_Vars)],
                                        Res_Var,
                                        Group_Var))
  }else{
    Non_Missing_Data<<-Remove_missing(Data, # remove missing data
                                      c(Pred_Vars,
                                        Res_Var,
                                        Group_Var))
  }
  
  # Convert code to numeric/factor (This is very important when running gee! Whether it is numeric or factor doesn't matter. They produce the same result!)
  Non_Missing_Data[, Group_Var]=as.numeric(as.factor(Non_Missing_Data[, Group_Var]))
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+")))
  model_fit=geeglm(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"))), 
                   data=Non_Missing_Data, 
                   id=Non_Missing_Data[, Group_Var], 
                   family=eval(parse(text=which.family)), 
                   corstr="exchangeable")
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(model_fit, diag(length(coef(model_fit))))[-1, ]
  
  # Output
  Output=c()
  N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  
  if(length(Pred_Vars)>=2){Output_vif=car::vif(model_fit)}else{Output_vif=""}
  # if(length(Pred_Vars)==1 & !is.numeric(Data[, Pred_Vars])){Output$vif=car::vif(model_fit)} # if the only variable is not numeric (that's, if it is categorical), compute vif
  # if(length(Pred_Vars)==1 & is.numeric(Data[, Pred_Vars])){Output$vif=""} # if the only variable is numeric, don't compute vif
  
  if(grepl("gaussian", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                                        " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                                        format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
  }else if(grepl("binomial", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
  }else if(grepl("poisson", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(model_fit))[-1]
    )
  }
  Output$summ_table=as.data.table(Output$summ_table, keep.rownames=TRUE)
  
  
  if(!is.null(dim(Output_vif))){
    Output$summ_table=cbind(Output$summ_table[, c(1, 5, 4)],
                            GVIF=rep(Output_vif[, 3], Output_vif[, 2]),
                            N_data_used=N_data_used)
  }else{
    Output$summ_table=cbind(Output$summ_table[, c(1, 5, 4)],
                            VIF=Output_vif,
                            N_data_used=N_data_used)
  }
  
  return(Output)
}

#*************************
# GEE_Confounder_Selection
#*************************
# Example
#******************
# # for now, algorithm works with no interaction term
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use[sample(1:nrow(Data_to_use), 20), "visit"]=NA
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# Res_Var="outcome"
# Group_Var="id"
# # All arguments must be declared with '<-'!
# GEE.fit=GEE_Multivariable_with_vif(Data<-Data_to_use,
#                                    Pred_Vars<-Pred_Vars,
#                                    Res_Var<-Res_Var,
#                                    Group_Var<-Group_Var,
#                                    which.family<-"binomial (link='logit')")
# Confounder_Steps=GEE_Confounder_Selection(Full_Model=GEE.fit$model_fit,
#                                           Main_Pred_Var="sex",
#                                           Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                           which.family="binomial (link='logit')", # distribution of the response variable
#                                           Min.Change.Percentage=5,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Ind=which(Pred_Vars%in%Confounder_Steps$Confounders)
# GEE.confound.fit=GEE_Multivariable_with_vif(Data<-Data_to_use,
#                                             Pred_Vars<-Pred_Vars[Confounder_Ind],
#                                             Res_Var<-Res_Var,
#                                             Group_Var<-Group_Var,
#                                             which.family<-"binomial (link='logit')")
# GEE.fit$summ_table
# GEE.confound.fit$summ_table
GEE_Confounder_Selection=function(Full_Model, 
                                  Main_Pred_Var, 
                                  Potential_Con_Vars, 
                                  which.family="binomial",
                                  Min.Change.Percentage=5,
                                  Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
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
      if(grepl("gaussian", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("binomial", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("poisson", which.family)){
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
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than Min.Change.Percentage, terminate the while loop
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

#*********************
# GEE_Confounder_Model
#*********************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# Main_Pred_Var="sex"
# Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"]
# GEE_Confounder=GEE_Confounder_Model(Data=Data_to_use,
#                                     Main_Pred_Var=Main_Pred_Var,
#                                     Potential_Con_Vars=Potential_Con_Vars, # for now, algorithm works with no interaction term
#                                     Res_Var="outcome",
#                                     Group_Var="id",
#                                     which.family="binomial (link='logit')", # gaussian, binomial, poisson
#                                     Min.Change.Percentage=15,
#                                     Estimate="raw_estimate") # raw_estimate, converted_estimate
# GEE_Confounder$Full_Multivariable_Model$summ_table
# GEE_Confounder$Confounder_Steps$Confounders
# GEE_Confounder$Confounder_Model$summ_table
# GEE_Confounder$Confounder_Model$N_data_used
GEE_Confounder_Model=function(Data,
                              Main_Pred_Var,
                              Potential_Con_Vars,
                              Res_Var,
                              Group_Var,
                              which.family,
                              Min.Change.Percentage=5,
                              Estimate="raw_estimate"){
  # check out packages
  lapply(c("data.table"), checkpackages)
  
  # Output
  Output=c()
  
  # convert Data to data frame
  Data=as.data.frame(Data)
  
  # Full multivariable model
  Pred_Vars=c(Main_Pred_Var, Potential_Con_Vars)
  
  Output$Full_Multivariable_Model=GEE_Multivariable_with_vif(Data<<-Data,
                                                             Pred_Vars<<-Pred_Vars,
                                                             Res_Var<<-Res_Var,
                                                             Group_Var<<-Group_Var,
                                                             which.family<<-which.family)
  
  # Confounder selection
  Confounder_Steps=GEE_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                            Main_Pred_Var=Main_Pred_Var,
                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var],
                                            which.family=which.family, # distribution of the response variable
                                            Min.Change.Percentage=Min.Change.Percentage,
                                            Estimate=Estimate) # raw_estimate, converted_estimate
  Output$Confounder_Steps=Confounder_Steps
  Confounder_Ind=which(Pred_Vars%in%Output$Confounder_Steps$Confounders)
  
  # Data=Remove_missing(Data, # remove missing data
  #                     c(Pred_Vars[Confounder_Ind],
  #                       Res_Var,
  #                       Group_Var))
  
  # Multivariable model with confounders
  Output$Confounder_Model=GEE_Multivariable_with_vif(Data<<-Data,
                                                     Pred_Vars<<-Pred_Vars[Confounder_Ind],
                                                     Res_Var<<-Res_Var,
                                                     Group_Var<<-Group_Var,
                                                     which.family<<-which.family)
  
  return(Output)
}



#********************
#
# GEE_Backward_by_QIC
#
#*******************************
# QIC-based backward elimination
#********
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GEE.fit=GEE_Multivariable_with_vif(Data=Data_to_use,
#                                    Pred_Vars=Pred_Vars,
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial")
# QIC_Selection_Steps=GEE_Backward_by_QIC(Full_Model=GEE.fit$model_fit,
#                                                      Pred_Vars=Pred_Vars)
GEE_Backward_by_QIC=function(Full_Model,
                             Pred_Vars){ # minimum percentage of change-in-estimate to terminate the algorithm
  # Full_Model=GEE.fit$model_fit
  # Pred_Vars
  
  
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
  # Full_Model=GEE.example$model_fit
  # Main_Pred_Var="sex"
  # Pred_Vars=c("center", "treat", "age", "baseline", "visit")
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Pred_Vars=Pred_Vars
  Include_Index=c(1:length(Pred_Vars))
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    Current_Full_Model_QIC=QIC(Current_Full_Model)["QIC"]
    
    Reduced_Model_QICs=c()
    
    # run GEE excluding one variable at once
    for(i in 1:length(Pred_Vars[Include_Index])){
      #i=1
      Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[Include_Index][i], collapse="-"))))
      Reduced_Model_QICs[i]=QIC(Current_Reduced_Model)["QIC"]
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Pred_Vars[Include_Index])))
    }
    
    # QIC of multivariable model excluding each variable
    Temp_Table=data.table(
      Inclusion="-",
      Var=c("(none)", Pred_Vars[Include_Index]),
      QIC=c(Current_Full_Model_QIC, Reduced_Model_QICs))
    
    # order by QIC
    Temp_Table=Temp_Table[order(QIC), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(Temp_Table$QIC[1]>=Current_Full_Model_QIC){
      loop.key=1
    }else{
      # if there is a variable whose exclusion leads to an improvement of the model (deacresed QIC)
      # remove the variable
      Var_to_Remove=Temp_Table[1, Var]
      
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Pred_Vars==Var_to_Remove)]
      # update the current full model
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[setdiff(1:length(Pred_Vars), Include_Index)], collapse="-"))))
      # increase step
      step=step+1
      
      # # if there's no more variable left
      # if(length(Include_Index)==0){
      #   Temp_Table=data.table(
      #     Removed_Var=c("Full", Pred_Vars[Include_Index]),
      #     Estimate=c(QIC(Current_Full_Model)[-1]),
      #     Delta="",
      #     Rank=""
      #   )
      #   Out$summ_table[[step]]=Temp_Table
      #   loop.key=1
      # }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Selected_Vars=Pred_Vars[Include_Index]
  
  return(Out)
}



#*******************
#
# GEE_Backward_by_P
#
#*******************
# Example
#************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# # generate missing data
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use[sample(1:nrow(Data_to_use), 20), "treat"]=NA
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GEE.fit=GEE_Multivariable_with_vif(Data=Data_to_use,
#                                    Pred_Vars=Pred_Vars,
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial")
# Backward_Elimination_Steps=GEE_Backward_by_P(GEE.fit$model_fit,
#                                              Data=Data_to_use,
#                                              Pred_Vars=Pred_Vars)
# # run GEE_Multivariable
# Backward_Elimination_Steps_Katya=GEE_Backward_by_P_Katya(Data=Data_to_use,
#                                                          Pred_Vars=Pred_Vars,
#                                                          Res_Var="outcome",
#                                                          Group_Var=c("id"),
#                                                          which.family="binomial")
# Backward_Elimination_Steps$Selected_Vars
# Backward_Elimination_Steps_Katya
GEE_Backward_by_P=function(Full_Model,
                           Data,
                           Pred_Vars){ # minimum percentage of change-in-estimate to terminate the algorithm
  # Full_Model=GEE.fit$model_fit
  # Pred_Vars
  # Data=Data_to_use
  
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
  # Full_Model=GLMM.example$model_fit
  # Main_Pred_Var="sex"
  # Pred_Vars=c("center", "treat", "age", "baseline", "visit")
  
  Data=as.data.table(Data)
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Pred_Vars=Pred_Vars
  Include_Index=c(1:length(Pred_Vars))
  
  Name_Dictionary=list()
  for(i in 1:length(Pred_Vars)){
    Name_Dictionary[[Pred_Vars[i]]]=paste0(colnames(Data[, .SD, .SDcols=Pred_Vars[i]]), Data[, levels(unlist(.SD)), .SDcols=Pred_Vars[i]])
  }
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step, " of ", length(Pred_Vars)))
    
    # update the current full model
    if(length(Pred_Vars)!=length(Include_Index)){
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[setdiff(1:length(Pred_Vars), Include_Index)], collapse="-"))))
    }
    
    # save QIC of the current model
    Out$Model_QIC[step]=QIC(Current_Full_Model)["QIC"]
    
    # p-values of multivariable model excluding each variable
    Temp_Summary=summary(Current_Full_Model)
    Temp_Table=data.table(
      Var=rownames(Temp_Summary$coefficients),
      Pvalue=Temp_Summary$coefficients[, "Pr(>|W|)"])
    
    # order by Pvalue
    Temp_Table=Temp_Table[Var!="(Intercept)", ]
    Temp_Table=Temp_Table[order(Pvalue, decreasing=T), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(nrow(Temp_Table)==1){
      loop.key=1
    }else{
      # if there is a variable whose exclusion leads to an improvement of the model (deacresed QIC)
      # remove the variable
      Temp_Var_to_Remove=Temp_Table[1, Var]
      
      Var_to_Remove=c()
      for(i in 1:length(Pred_Vars)){
        if(sum(Temp_Var_to_Remove==Name_Dictionary[[Pred_Vars[[i]]]])>0){
          Var_to_Remove=Pred_Vars[[i]]
        }
      }
      
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Pred_Vars==Var_to_Remove)]
      # increase step
      step=step+1
      
      # # if there's no more variable left
      # if(length(Include_Index)==0){
      #   Temp_Table=data.table(
      #     Removed_Var=c("Full", Pred_Vars[Include_Index]),
      #     Estimate=c(QIC(Current_Full_Model)[-1]),
      #     Delta="",
      #     Rank=""
      #   )
      #   Out$summ_table[[step]]=Temp_Table
      #   loop.key=1
      # }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Temp_Vars=Out$summ_table[[which(Out$Model_QIC==min(Out$Model_QIC))]]$Var
  Selected_Vars=c()
  for(j in 1:length(Temp_Vars)){
    for(i in 1:length(Pred_Vars)){
      if(sum(Temp_Vars[j]==Name_Dictionary[[Pred_Vars[[i]]]])>0){
        Selected_Vars[j]=Pred_Vars[[i]]
      }
    }
  }
  Out$Selected_Vars=Selected_Vars
  
  return(Out)
}



#************************
#
# GEE_Backward_by_P_Katya
#
#************************
GEE_Backward_by_P_Katya=function(Data, Pred_Vars, Res_Var, Group_Var, which.family){ ## names of people should be numeric
  Data=as.data.frame(Data)
  Data[, Group_Var]=gsub("A", 999, Data[, Group_Var])
  Data[, Group_Var]=as.numeric(as.factor(Data[, Group_Var]))
  
  # as data frame
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  if(sum(grepl(":", Pred_Vars))>0){
    Non_Missing_Data<<-Remove_missing(Data, # remove missing data
                                      c(Pred_Vars[!grepl(":", Pred_Vars)],
                                        Res_Var,
                                        Group_Var))
  }else{
    Non_Missing_Data<<-Remove_missing(Data, # remove missing data
                                      c(Pred_Vars,
                                        Res_Var,
                                        Group_Var))
  }
  Data=Non_Missing_Data
  
  ## check if some columns have a similar name
  
  N.of.similar.cols=array()
  for(i in 1:length(Pred_Vars)){
    N.of.similar.cols[i]=length(grep(Pred_Vars[i], Pred_Vars))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these=which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " , Pred_Vars[rename.these], sep=""))
  }else{
    
    
    library(geepack)
    library(MESS)
    library(doBy) 
    
    vars=Pred_Vars
    vars.ALL=Pred_Vars
    
    QIC.RES=REMOVE.RES=COL.NAMES=array()
    QIC.RES[1]=NA
    QIC.RES_p=NA
    
    ## while(QIC.RES[i]<QIC.RES[i-1]){
    for(i in 1:(length(vars)-1)){
      fullmod=as.formula( paste( Res_Var, " ~ ", paste(vars, collapse = "+")))
      r1=geeglm(fullmod, data=Data, id=Data[, Group_Var], family=which.family, corstr="exchangeable")
      QIC.RES[i+1]=QIC(r1)[1]
      remove.this.id.name=names(which(as.matrix(summary(r1)$coefficients)[-1, 4]==max(as.matrix(summary(r1)$coefficients)[-1, 4])))
      remove.this.id.name=remove.this.id.name[1]
      QIC.RES_p=c(QIC.RES_p, round2(max(as.matrix(summary(r1)$coefficients)[-1, 4]), 2))
      remove.this.id.name.clean=Pred_Vars[sapply(1:length(Pred_Vars), function(x) grepl(Pred_Vars[x], remove.this.id.name))]
      
      remove.this.id=sapply(1:length(vars), function(x) grepl(remove.this.id.name.clean, vars[x]))
      
      REMOVE.RES[i]=vars[remove.this.id]
      vars= setdiff(vars, vars[remove.this.id])
      #i=i+1
      print(i)
      print(QIC.RES)
      print(REMOVE.RES)
    }
    QIC.RES=QIC.RES[-1]
    QIC.RES_p=QIC.RES_p[-1]
    REMOVE.RES=REMOVE.RES
    
    RES=cbind(QIC.RES, QIC.RES_p, REMOVE.RES)
    colnames(RES)=c("QIC Before removal", "p Before removal", "Removed variable")
    RES.QIC=RES
    
    Top=(which.min(as.numeric(RES[, "QIC Before removal"]))-1)
    if(Top==0){vars.keep=vars.ALL}else{
      vars.keep=vars.ALL[which(vars.ALL%in%(RES[1:Top, "Removed variable"])==F)]
    }
    
    RES.QIC[, "Removed variable"]=gsub("as.factor", "", RES.QIC[, "Removed variable"])
    RES.QIC[, "Removed variable"]=gsub("as.numeric", "", RES.QIC[, "Removed variable"])
    RES.QIC[, "Removed variable"]=gsub(")1", "", RES.QIC[, "Removed variable"])
    RES.QIC[, "Removed variable"]=gsub(") 1", "", RES.QIC[, "Removed variable"])
    RES.QIC[, "Removed variable"]=gsub("[()]", "", RES.QIC[, "Removed variable"])
    
    return(vars.keep)
  }
} ## names of people should be numeric



#**********************
#
# [ --- GLMM --- ] ----
#
#**********************
# GLMM_Bivariate
#***************
# Example
# ************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Bivariate(Data=Data_to_use,
#                Pred_Vars=list("center",
#                               c("sex", "age", "sex:age")),
#                Res_Var="outcome",
#                Group_Var="id",
#                which.family<-"binomial (link='logit')", # gaussian, binomial, poisson
#                NAGQ<-1,
#                Compute.Power=F, # power can be computed for a non-gaussian distribution
#                nsim=5)
# 
# #**********************************
# # Example - (2) : negative binomial
# #**********************************
# set.seed(101)
# Data_to_use=expand.grid(f1 = factor(1:3),
#                         f2 = LETTERS[1:2], g=1:9, rep=1:15,
#                         KEEP.OUT.ATTRS=FALSE)
# summary(mu <- 5*(-4 + with(Data_to_use, as.integer(f1) + 4*as.numeric(f2))))
# Data_to_use$y <- rnbinom(nrow(Data_to_use), mu = mu, size = 0.5)
# Pred_Vars=c("f1", "f2")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="f1")]="1"
# levels.of.fact[which(Pred_Vars=="f2")]="A"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="y",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM_Mult_model=GLMM_Bivariate(Data=Data_to_use,
#                                Pred_Vars=list(c("f1" ,"f2", "f1:f2")),
#                                Res_Var="y",
#                                Group_Var="g",
#                                which.family<-"negative_binomial", # gaussian, binomial, poisson
#                                NAGQ<-1,
#                                Compute.Power=F, # power can be computed for a non-gaussian distribution
#                                nsim=5)
GLMM_Bivariate=function(Data,
                        Pred_Vars,
                        Res_Var,
                        Group_Var,
                        which.family,
                        NAGQ=100,
                        Compute.Power=FALSE,
                        nsim=1000){
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    #i=1
    Temp=GLMM_Multivariable(Data=Data,
                            Pred_Vars=unlist(Pred_Vars[i]),
                            Res_Var=Res_Var,
                            Group_Var=Group_Var,
                            which.family=which.family, # gaussian, binomial, poisson
                            NAGQ=NAGQ,
                            Compute.Power=Compute.Power, # power can be computed for a non-gaussian distribution
                            nsim=nsim)
    Output=rbind(Output,
                 cbind(Temp$summ_table[, c(1:5, 7)],
                       Data_Used=Temp$N_data_used))
    
    # print out progress
    if(sum(grepl(":", Pred_Vars[i]))>0){
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", unlist(Pred_Vars[i])[grepl(":", unlist(Pred_Vars[i]))], " (", i ," out of ", length(Pred_Vars), ")"))
    }else{
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
    }
  }
  
  return(Output)
}



#*******************
# GLMM_Multivariable
#*******************
# Example - (1)
#************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Mult_model=GLMM_Multivariable(Data=rbind(Data_to_use,Data_to_use),
#                                    Pred_Vars=c("center", "sex", "age", "sex:age"),
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial (link='logit')", # gaussian, binomial, poisson
#                                    NAGQ<-1,
#                                    Compute.Power=F, # power can be computed for a non-gaussian distribution
#                                    nsim=5)
# 
# #**********************************
# # Example - (2) : negative binomial
# #**********************************
# set.seed(101)
# Data_to_use=expand.grid(f1 = factor(1:3),
#                         f2 = LETTERS[1:2], g=1:9, rep=1:15,
#                         KEEP.OUT.ATTRS=FALSE)
# summary(mu <- 5*(-4 + with(Data_to_use, as.integer(f1) + 4*as.numeric(f2))))
# Data_to_use$y <- rnbinom(nrow(Data_to_use), mu = mu, size = 0.5)
# Pred_Vars=c("f1", "f2")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="f1")]="1"
# levels.of.fact[which(Pred_Vars=="f2")]="A"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="y",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM_Mult_model=GLMM_Multivariable(Data=Data_to_use,
#                                    Pred_Vars=c("f1" ,"f2", "f1:f2"),
#                                    Res_Var="y",
#                                    Group_Var="g",
#                                    which.family<-"negative_binomial", # gaussian, binomial, poisson
#                                    NAGQ<-1,
#                                    Compute.Power=T, # power can be computed for a non-gaussian distribution
#                                    nsim=5)
GLMM_Multivariable=function(Data,
                            Pred_Vars,
                            Res_Var,
                            Group_Var, 
                            which.family,
                            NAGQ=100,
                            Compute.Power=FALSE,
                            nsim=1000){
  # check out packages
  lapply(c("lme4", "simr", "sjPlot", "MASS", "data.table", "optimx"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep=""))
  if(grepl("gaussian", which.family)){
    myfit=lmer(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep="")), 
               na.action=na.exclude, 
               data=Data, 
               control=lmerControl(optimizer=c("bobyqa"), optCtrl=list(maxfun=1e09)))
  }else if(grepl("negative_binomial", which.family)){
    # estimate theta
    print("Distribution : Negative Binomial / Estimating the overdispersion parameter, theta")
    Overdispersion=GLMM_NB_Overdispersion_Estimator(Data=Data,
                                                    Pred_Vars=Pred_Vars,
                                                    Res_Var=Res_Var,
                                                    Group_Var=Group_Var)
    
    myfit=glmer(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep="")), 
                family=eval(parse(text=paste0("MASS::negative.binomial(theta=", Overdispersion$theta, ")"))), 
                na.action=na.exclude, 
                data=Data, nAGQ=NAGQ, 
                control=glmerControl(optimizer=c("bobyqa"), optCtrl=list(maxfun=1e09))) # try "bobyqa" or "Nelder_Mead" if the algorithm fails to converge.
  }else{
    myfit=glmer(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep="")), 
                family=eval(parse(text=which.family)), 
                na.action=na.exclude, 
                data=Data, nAGQ=NAGQ, 
                control=glmerControl(optimizer=c("bobyqa"), optCtrl=list(maxfun=1e09))) # try "bobyqa" or "Nelder_Mead" if the algorithm fails to converge.
    #control=glmerControl(optimizer=c("optimx"), optCtrl=list(method="nlminbwrap"))) # try "bobyqa" or "Nelder_Mead" if the algorithm fails to converge.
  }
  
  # number of observations from a model fit
  Used_N_Rows=nobs(myfit)
  
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
  Estimates=row.names(Coef)[-1]
  for(i in 1:length(Estimates)){
    Coef.ind=c(Coef.ind, which(grepl(Estimates[i], row.names(Coef))))
    CI.raw.ind=c(CI.raw.ind, which(grepl(Estimates[i], row.names(CI.raw))))
    CI.ind=c(CI.ind, which(grepl(Estimates[i], row.names(CI))))
    if(Compute.Power==T){
      lapply(c("simr"), checkpackages)
      Var.Power[[i]]=powerSim(myfit, fixed(Estimates[i], "lr"), nsim=nsim, progress=F)}
  }
  
  Coef.ind=sort(unique(Coef.ind))
  CI.raw.ind=sort(unique(CI.raw.ind))
  CI.ind=sort(unique(CI.ind))
  
  #*******
  # Output
  #*******
  Output=c()
  N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)")
  # random effect plot (exponentiated)
  Output$re_plot=plot_model(myfit, type="re")
  # info of model fit
  Output$model_fit=myfit
  # vif
  if(length(Pred_Vars)>=2){Output_vif=car::vif(myfit)}else{Output_vif=""}
  
  # summary table
  Output$summ_table$Estimate=round2(Coef[, "Estimate"][Coef.ind], 3)
  Output$summ_table$Std.Error=round2(Coef[, "Std. Error"][Coef.ind], 3)
  
  if(grepl("gaussian", which.family)){
    Output$summ_table$`P-value`=c()
    Output$summ_table$Estimate.and.CI=paste0(format(round2(Coef[, "Estimate"][Coef.ind], 2), nsmall=2), 
                                             " (", format(round2(CI.raw[CI.raw.ind, 1], 2), nsmall=2), " - ", 
                                             format(round2(CI.raw[CI.ind, 2], 2), nsmall=2), ")")
  }else if(grepl("poisson", which.family) | grepl("negative_binomial", which.family)){
    Output$summ_table$`P-value`=ifelse(Coef[, ncol(Coef)][Coef.ind]<0.001, "<0.001", 
                                       format(round2(Coef[, ncol(Coef)][Coef.ind], 7), nsmall=3))
    Output$summ_table$RR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }else if(grepl("binomial", which.family)){
    Output$summ_table$`P-value`=ifelse(Coef[, ncol(Coef)][Coef.ind]<0.001, "<0.001", 
                                       format(round2(Coef[, ncol(Coef)][Coef.ind], 7), nsmall=3))
    Output$summ_table$OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }
  
  
  Output$summ_table=as.data.frame(Output$summ_table) %>% as.data.table(keep.rownames=TRUE)
  
  
  if(!is.null(dim(Output_vif))){
    Output$summ_table=cbind(Output$summ_table,
                            GVIF=rep(Output_vif[, 3], Output_vif[, 2]),
                            N_data_used=N_data_used)
  }else{
    Output$summ_table=cbind(Output$summ_table,
                            VIF=Output_vif,
                            N_data_used=N_data_used)
  }
  
  # power
  if(Compute.Power==T){
    Output$summ_table$power=sapply(Var.Power, function(x) paste0(
      paste0(round(summary(x)["mean"]*100, 2), "%"),
      " (",
      round(summary(x)["lower"]*100, 2),
      ", ",
      round(summary(x)["upper"]*100, 2),
      ")"
    ))
  }
  
  # if the distribution is negative binomial, then include additional components in Output
  if(grepl("negative_binomial", which.family)){
    Output$Overdispersion=Overdispersion
  }
  return(Output)
}



#**************************
# GLMM_Confounder_Selection
#**************************
# Example - (1)
#******************
# for now, algorithm works with no interaction term
#**************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM.fit=GLMM_Multivariable(Data_to_use,
#                             Pred_Vars,
#                             Res_Var="outcome",
#                             Group_Var="id",
#                             which.family="gaussian", # gaussian, binomial, poisson
#                             NAGQ=100,
#                             Compute.Power=F,
#                             nsim=30)
# Confounder_Steps=GLMM_Confounder_Selection(Full_Model=GLMM.fit$model_fit,
#                                            Main_Pred_Var="sex",
#                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                            which.family="binomial (link='logit')", # distribution of the response variable
#                                            # !! Unlike GEE_Confounder_Selection, this which.family is not applied when the model is updated (update()) as the process of building the confounding model.
#                                            Min.Change.Percentage=5,
#                                            Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
# 
# #**********************************
# # Example - (2) : negative binomial
# #**********************************
# set.seed(101)
# Data_to_use=expand.grid(f1 = factor(1:3),
#                         f2 = LETTERS[1:2], 
#                         f3 = rep(2:5),
#                         g=1:5, rep=1:3,
#                         KEEP.OUT.ATTRS=FALSE)
# summary(mu <- 5*(-4 + with(Data_to_use, as.integer(f1) + 4*as.numeric(f2))))
# Data_to_use$y <- rnbinom(nrow(Data_to_use), mu = mu, size = 0.5)
# Pred_Vars=c("f1", "f2", "f3")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="f1")]="1"
# levels.of.fact[which(Pred_Vars=="f2")]="A"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="y",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM.fit=GLMM_Multivariable(Data=Data_to_use,
#                             Pred_Vars=c("f1" ,"f2", "f3", "f2:f3"),
#                             Res_Var="y",
#                             Group_Var="g",
#                             which.family<-"negative_binomial", # gaussian, binomial, poisson
#                             NAGQ<-1,
#                             Compute.Power=F, # power can be computed for a non-gaussian distribution
#                             nsim=5)
# Confounder_Steps=GLMM_Confounder_Selection(Full_Model=GLMM.fit$model_fit,
#                                            Main_Pred_Var="f1",
#                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!="f1"], # for now, algorithm works with no interaction term
#                                            which.family="negative_binomial", # distribution of the response variable
#                                            # !! Unlike GEE_Confounder_Selection, this which.family is not applied when the model is updated (update()) as the process of building the confounding model.
#                                            Min.Change.Percentage=5,
#                                            Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
GLMM_Confounder_Selection=function(Full_Model, 
                                   Main_Pred_Var, 
                                   Potential_Con_Vars, 
                                   which.family="binomial",
                                   Min.Change.Percentage=5,
                                   Estimate="raw_estimate"){ # minimum percentage of change-in-estimate to terminate the algorithm
  
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
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
      if(grepl("gaussian", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Estimate=c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model),
          Delta=c("", abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("binomial", which.family)){
        Temp_Table=data.table(
          Removed_Var=c(paste0("Full (", Main_Cov_Level, ")"), Potential_Con_Vars[Include_Index]),
          Est_Odds=exp(c(Main_Effect_Current_Full_Model, Main_Effect_Current_Reduced_Model)),
          Delta=c("", abs(exp(Main_Effect_Current_Reduced_Model)/exp(Main_Effect_Current_Full_Model)-1)*100),
          Rank=as.numeric(c("", rank(abs(Main_Effect_Current_Reduced_Model/Main_Effect_Current_Full_Model-1)*100)))
        )
      }else if(grepl("poisson", which.family)){
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
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than Min.Change.Percentage, terminate the while loop
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
# Example - (1)
#**************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# Main_Pred_Var="sex"
# Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"]
# 
# GLMM_Confounder=GLMM_Confounder_Model(Data=Data_to_use,
#                                       Main_Pred_Var=Main_Pred_Var,
#                                       Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var], # for now, algorithm works with no interaction term
#                                       Res_Var="outcome",
#                                       Group_Var="id",
#                                       which.family="binomial (link='logit')", # gaussian, binomial, poisson
#                                       NAGQ=1,
#                                       Min.Change.Percentage=5,
#                                       Estimate="raw_estimate") # raw_estimate, converted_estimate
# GLMM_Confounder$Full_Multivariable_Model$summ_table
# GLMM_Confounder$Confounder_Steps$Confounders
# GLMM_Confounder$Confounder_Model$summ_table
# 
#
# #**********************************
# # Example - (2) : negative binomial
# #**********************************
# set.seed(101)
# Data_to_use=expand.grid(f1 = factor(1:3),
#                         f2 = LETTERS[1:2],
#                         f3 = rep(2:5),
#                         g=1:5, rep=1:3,
#                         KEEP.OUT.ATTRS=FALSE)
# summary(mu <- 5*(-4 + with(Data_to_use, as.integer(f1) + 4*as.numeric(f2))))
# Data_to_use$y <- rnbinom(nrow(Data_to_use), mu = mu, size = 0.5)
# Pred_Vars=c("f1", "f2", "f3")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="f1")]="1"
# levels.of.fact[which(Pred_Vars=="f2")]="A"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="y",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# Main_Pred_Var="f1"
# GLMM_Confounder=GLMM_Confounder_Model(Data=Data_to_use,
#                                       Main_Pred_Var=Main_Pred_Var,
#                                       Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var], # for now, algorithm works with no interaction term
#                                       Res_Var="y",
#                                       Group_Var="g",
#                                       which.family="negative_binomial", # gaussian, binomial, poisson
#                                       NAGQ=1,
#                                       Min.Change.Percentage=5,
#                                       Estimate="raw_estimate") # raw_estimate, converted_estimate
# GLMM_Confounder$Full_Multivariable_Model$summ_table
# GLMM_Confounder$Confounder_Steps$Confounders
# GLMM_Confounder$Confounder_Model$summ_table
GLMM_Confounder_Model=function(Data,
                               Main_Pred_Var,
                               Potential_Con_Vars,
                               Res_Var,
                               Group_Var,
                               which.family,
                               NAGQ=100,
                               Min.Change.Percentage=5,
                               Estimate="raw_estimate"){
  Output=c()
  # Full multivariable model
  Pred_Vars=c(Main_Pred_Var, Potential_Con_Vars)
  Output$Full_Multivariable_Model=GLMM_Multivariable(Data,
                                                     Pred_Vars=Pred_Vars,
                                                     Res_Var=Res_Var,
                                                     Group_Var=Group_Var,
                                                     which.family=which.family,
                                                     NAGQ=NAGQ)
  
  # Confounder selection
  Confounder_Steps=GLMM_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                             Main_Pred_Var=Main_Pred_Var,
                                             Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var],
                                             which.family=which.family, # distribution of the response variable
                                             Min.Change.Percentage=Min.Change.Percentage,
                                             Estimate=Estimate) # raw_estimate, converted_estimate
  
  # save all stepwise procedure
  Output$Confounder_Steps=Confounder_Steps
  
  # index of confounders
  Confounder_Ind=which(Pred_Vars%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=GLMM_Multivariable(Data,
                                             Pred_Vars=Pred_Vars[Confounder_Ind],
                                             Res_Var=Res_Var,
                                             Group_Var=Group_Var,
                                             which.family=which.family,
                                             NAGQ=NAGQ)
  return(Output)
}

#**************
#
# GLMM_LASSO_CV
#
#*****************
# For some reason, the function in library(lmmen) that conducts a cross-validation for the optimal tuning parameter does not work.
#cv.glmmLasso(initialize_example(seed=1))
# Thus, I wrote my own code that performs a k-fold cross-validation as below.
#****************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="gaussian(link=identity)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data=Format_Columns(Data=Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars=pred_vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# Data$sex=as.character(Data$sex)
# Data[sample(nrow(Data), 150), "sex"]="N"
# lambda=seq(0, 5, by=0.5)
# # GLMM_LASSO_CV_Out
# GLMM_LASSO_CV_Out=GLMM_LASSO_CV(Data=Data_to_use,
#                                 pred_vars,
#                                 res_var,
#                                 rand_var,
#                                 which.family,
#                                 k=6,
#                                 lambda=lambda)
# 
# # train error
# GLMM_LASSO_CV_Out$Train_Error
# # cv error
# GLMM_LASSO_CV_Out$CV_Error
# # plot
# GLMM_LASSO_CV_Out$CV_plot
# # optimal lambda
# GLMM_LASSO_CV_Out$Optimal_Lambda

# # There's a function that performs a CV for GLMM, called cv.glmmLasso, in lmmen package.
# # However, there appears to be some issues when the function is excuted (fun a code below). I googled to find how to troubleshoot, but there's even not
# # a single example that shows the use of the function.
# # https://raw.githubusercontent.com/cran/glmmLasso/master/demo/glmmLasso-soccer.r
# library(lmmen)
# cv.glmmLasso(dat=Data_to_use,
#              form.fixed=outcome~center + as.factor(treat) + as.factor(sex) + age + baseline + visit,
#              form.rnd=list(id=~1),
#              family=binomial(link=logit),
#              lambda=seq(0, 5, by=0.5))
GLMM_LASSO_CV=function(Data,
                       pred_vars,
                       res_var,
                       rand_var,
                       which.family,
                       k=4,
                       lambda=seq(0, 10, by=1)){
  #Data=Data[sample(1:nrow(Data), 5000), ]
  
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr", "ggplot2"), checkpackages)
  # convert data to data frame
  Data=as.data.table(Data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  CV_data=na.omit(Data[, 
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
  CV.model=as.formula(paste(res_var, "~", paste(ifelse(!unlist(Data[, lapply(.SD, is.numeric), .SDcols=pred_vars]), paste0("as.factor(", pred_vars, ")"), pred_vars), collapse="+"), sep=""))
  
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
      
      # divide data into training and test sets
      CV_data_train=CV_data[-fold, ]
      CV_data_test=CV_data[fold, ]
      
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
# Data_to_use=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="gaussian(link=identity)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data_to_use=rbind(Data_to_use)
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars=pred_vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# 
# # lambda
# lambda=seq(0, 5, by=0.5)
# # GLMM_LASSO_CV_Out
# GLMM_LASSO_CV_Out=GLMM_LASSO_CV(Data=Data_to_use,
#                                 pred_vars,
#                                 res_var,
#                                 rand_var,
#                                 which.family,
#                                 k=6,
#                                 lambda=lambda)
# 
# 
# GLMM_LASSO_CV_Out$Optimal_Lambda
# 
# GLMM.LASSO.fit=GLMM_LASSO(Data=Data_to_use,
#                           pred_vars,
#                           res_var,
#                           rand_var,
#                           which.family,
#                           lambda=0) # optimal lambda
# summary(GLMM.LASSO.fit)
GLMM_LASSO=function(Data, pred_vars, res_var, rand_var, which.family="binomial(link=logit)", lambda=10){
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr"), checkpackages)
  # convert data to data frame
  Data=as.data.table(Data)
  
  #****************************
  # exclude missing obsevations
  #****************************
  Data=na.omit(Data[, 
                    .SD, 
                    .SDcols=c(res_var, pred_vars, rand_var)])
  
  #*************
  # run algoritm
  #*************
  # grouping variable
  Data[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
  # random effect
  random_effect=list(id=~1)
  names(random_effect)=rand_var
  
  # specify GLMM model
  GLMM.model=as.formula(paste(res_var, "~", paste(ifelse(!unlist(Data[, lapply(.SD, is.numeric), .SDcols=pred_vars]), paste0("as.factor(", pred_vars, ")"), pred_vars), collapse="+"), sep=""))
  
  # run glmm Lasso
  glmmLasso.fit=glmmLasso(GLMM.model, 
                          rnd=random_effect, 
                          family=eval(parse(text=which.family)), 
                          data=Data, 
                          lambda=lambda,
                          switch.NR=TRUE)
  return(glmmLasso.fit)
}


#********************
#
# GLMM_Bivariate_Plot
#
#********************
#The blue curves represent the fixed effect of age on outcome, which corresponds to the center of subject-specific regression lines with varying intercepts (grey-dotted lines) on a log scale (A) and a logit scale (B and C).
#The blue-shaded bands represent 95% confidence intervals of prediction.
#***********************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# res_var="outcome"
# rand_var="id"
# which.family="binomial(link=logit)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(pred_vars=="treat")]="P"
# levels.of.fact[which(pred_vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars=pred_vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# head(Data_to_use)
# GLMM_Bivariate_Plot(Data=Data_to_use,
#                     Pred_Var="age",
#                     Res_Var="outcome",
#                     Group_Var="id",
#                     which.family="binomial (link='logit')",
#                     NAGQ=100,
#                     xlab="age",
#                     ylab="outcome",
#                     title="Title",
#                     x_breaks=seq(round(min(Data_to_use$age)-5, -1), round(max(Data_to_use$age)+5, -1), by=10))
GLMM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, Group_Var, which.family, NAGQ=100, xlab="", ylab="", title="", x_breaks=0){
  lapply(c("sjPlot", "simr"), checkpackages)
  
  # Output
  Output=c()
  
  # change data type & remove missing data
  Data=as.data.frame(Data)
  Data=na.omit(Data[, c(Res_Var, Pred_Var, Group_Var)])
  
  # fit GLMM
  fullmod=as.formula(paste(Res_Var, "~", Pred_Var, "+(1|", Group_Var, ")", sep=""))
  
  Model=glmer(fullmod, family=eval(parse(text=which.family)), na.action=na.exclude, data=Data, nAGQ=NAGQ)
  Output$glmer=Model
  #Output$power=powerSim(Model)
  
  # save random effect plot
  Output$re_plot=plot_model(Model, type="re")
  
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
  VarCorr_out=unlist(lme4::VarCorr(Model))
  tvar1=pvar1+VarCorr_out[names(VarCorr_out)==Group_Var]
  
  if(grepl("poisson", which.family)){ # code for 'poisson' family
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
    Output$plot=ggplot(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
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
    Output$plot_w_re=ggplot(newdat, aes(x=eval(parse(text=Pred_Var)), 
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
  }else if(grepl("binomial", which.family)){ # code for 'binomial' family
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
    Output$plot=ggplot(data=newdat.margin, aes(x=eval(parse(text=Pred_Var)), 
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
    Output$plot_w_re=ggplot(newdat, aes(x=eval(parse(text=Pred_Var)), 
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
  
  return(Output)
}

#**********
#
# GLMM_Plot
#
#**********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GLMM_Mult_model=GLMM_Multivariable(Data=Data_to_use,
#                                    Pred_Vars=Pred_Vars,
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family="binomial (link='cloglog')", # gaussian, binomial, poisson
#                                    NAGQ=1,
#                                    Compute.Power=F, # power can be computed for a non-gaussian distribution
#                                    nsim=5)
# GLMM_Plot(Model_Fit=GLMM_Mult_model$model_fit,
#           X_Var="center",
#           Group_Var="id",
#           Type="re")
GLMM_Plot=function(Model_Fit,
                   X_Var,
                   Group_Var,
                   Type="fe"){
  
  # check out packages
  lapply(c("ggeffects"), checkpackages)
  
  plot(ggpredict(Model_Fit,
                 terms=c(X_Var, paste0(Group_Var, "[sample=9]")), type=Type),
       add.data=T)
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
  model.df=sum(sapply(lme4::VarCorr(model), vpars)) + length(fixef(model))
  rdf=nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp=residuals(model, type="pearson")
  Pearson.chisq=sum(rp^2, na.rm=TRUE)
  prat=Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval=pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}



#**********************
#
# GLMM_Backward_by_AIC
#
#*******************************
# AIC-based backward elimination
#*******************************
# data(mtcars)
# MultiLinearReg=glm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars, family="gaussian")
# 
# stepAIC(MultiLinearReg, trace=TRUE)
# 
# AIC(MultiLinearReg)
# AIC(glm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars, family="gaussian"))
# AIC(glm(mpg~disp+hp+drat+wt+qsec+am+gear+carb, data=mtcars, family="gaussian"))
# 
# summary(MultiLinearReg)
#********
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM.fit=GLMM_Multivariable(Data=Data_to_use,
#                             Pred_Vars=Pred_Vars[Pred_Vars!="visit"],
#                             Res_Var="outcome",
#                             Group_Var=c("id"," visit"),
#                             which.family="binomial", # gaussian, binomial, poisson
#                             NAGQ=1,
#                             Compute.Power=F,
#                             nsim=30)
# AIC_Selection_Steps=GLMM_Backward_by_AIC(Full_Model=GLMM.fit$model_fit,
#                                                      Pred_Vars=Pred_Vars)
GLMM_Backward_by_AIC=function(Full_Model,
                              Pred_Vars){ # minimum percentage of change-in-estimate to terminate the algorithm
  # Full_Model=GLMM.fit$model_fit
  # Pred_Vars
  
  
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
  # Full_Model=GLMM.example$model_fit
  # Main_Pred_Var="sex"
  # Pred_Vars=c("center", "treat", "age", "baseline", "visit")
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Pred_Vars=Pred_Vars
  Include_Index=c(1:length(Pred_Vars))
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step))
    
    #
    Current_Full_Model_AIC=AIC(Current_Full_Model)
    
    Reduced_Model_AICs=c()
    
    # run GLMM excluding one variable at once
    for(i in 1:length(Pred_Vars[Include_Index])){
      #i=1
      Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[Include_Index][i], collapse="-"))))
      Reduced_Model_AICs[i]=AIC(Current_Reduced_Model)
      print(paste0("Step : ", step, " - Vars : ", i, "/", length(Pred_Vars[Include_Index])))
    }
    
    # AIC of multivariable model excluding each variable
    Temp_Table=data.table(
      Inclusion="-",
      Var=c("(none)", Pred_Vars[Include_Index]),
      AIC=c(Current_Full_Model_AIC, Reduced_Model_AICs))
    
    # order by AIC
    Temp_Table=Temp_Table[order(AIC), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(Temp_Table$AIC[1]>=Current_Full_Model_AIC){
      loop.key=1
    }else{
      # if there is a variable whose exclusion leads to an improvement of the model (deacresed AIC)
      # remove the variable
      Var_to_Remove=Temp_Table[1, Var]
      
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Pred_Vars==Var_to_Remove)]
      # update the current full model
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[setdiff(1:length(Pred_Vars), Include_Index)], collapse="-"))))
      # increase step
      step=step+1
      
      # # if there's no more variable left
      # if(length(Include_Index)==0){
      #   Temp_Table=data.table(
      #     Removed_Var=c("Full", Pred_Vars[Include_Index]),
      #     Estimate=c(AIC(Current_Full_Model)[-1]),
      #     Delta="",
      #     Rank=""
      #   )
      #   Out$summ_table[[step]]=Temp_Table
      #   loop.key=1
      # }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Out$Selected_Vars=Pred_Vars[Include_Index]
  
  return(Out)
}



#*******************
#
# GLMM_Backward_by_P
#
#*******************
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="N"
# Data_to_use[sample(nrow(Data_to_use), 30), "sex"]="P"
# Data_to_use$sex=as.factor(Data_to_use$sex)
# 
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# NAGQ=1
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM.fit=GLMM_Multivariable(Data=Data_to_use,
#                             Pred_Vars=Pred_Vars,
#                             Res_Var="outcome",
#                             Group_Var=c("id", "visit"),
#                             which.family="binomial", # gaussian, binomial, poisson
#                             NAGQ=1,
#                             Compute.Power=F,
#                             nsim=30)
# Backward_Elimination_Steps=GLMM_Backward_by_P(GLMM.fit$model_fit,
#                                               Data=Data_to_use,
#                                               Pred_Vars=Pred_Vars)
# Backward_Elimination_Steps_Katya=GLMM_Backward_by_P_Katya(Data=Data_to_use,
#                                                           Pred_Vars=Pred_Vars,
#                                                           Res_Var="outcome",
#                                                           Group_Var=c("id"," visit"),
#                                                           which.family="binomial",
#                                                           NAGQ=NAGQ)
# Backward_Elimination_Steps$Selected_Vars
# Backward_Elimination_Steps_Katya
GLMM_Backward_by_P=function(Full_Model,
                            Data,
                            Pred_Vars){ # minimum percentage of change-in-estimate to terminate the algorithm
  # Full_Model=GLMM.fit$model_fit
  # Pred_Vars
  # Data=Data_to_use
  
  # check packages
  lapply(c("dplyr", "data.table"), checkpackages)
  
  # Full_Model=GLMM.example$model_fit
  # Main_Pred_Var="sex"
  # Pred_Vars=c("center", "treat", "age", "baseline", "visit")
  
  Data=as.data.table(Data)
  
  # Out
  Out=c()
  
  # initial settings
  step=1
  loop.key=0
  Current_Full_Model=Full_Model
  #Current_Pred_Vars=Pred_Vars
  Include_Index=c(1:length(Pred_Vars))
  
  Name_Dictionary=list()
  for(i in 1:length(Pred_Vars)){
    Name_Dictionary[[Pred_Vars[i]]]=paste0(colnames(Data[, .SD, .SDcols=Pred_Vars[i]]), Data[, levels(unlist(.SD)), .SDcols=Pred_Vars[i]])
  }
  
  #***************
  # main algorithm
  #***************
  while(loop.key==0){ # while - start
    # indicate how many steps have been processed
    print(paste0("Step : ", step, " of ", length(Pred_Vars)))
    
    # update the current full model
    if(length(Pred_Vars)!=length(Include_Index)){
      Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[setdiff(1:length(Pred_Vars), Include_Index)], collapse="-"))))
    }
    
    # save AIC of the current model
    Out$Model_AIC[step]=AIC(Current_Full_Model)
    
    # p-values of multivariable model excluding each variable
    Temp_Summary=summary(Current_Full_Model)
    Temp_Table=data.table(
      Var=rownames(Temp_Summary$coefficients),
      Pvalue=Temp_Summary$coefficients[, "Pr(>|z|)"])
    
    # order by Pvalue
    Temp_Table=Temp_Table[Var!="(Intercept)", ]
    Temp_Table=Temp_Table[order(Pvalue, decreasing=T), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(nrow(Temp_Table)==1){
      loop.key=1
    }else{
      # if there is a variable whose exclusion leads to an improvement of the model (deacresed AIC)
      # remove the variable
      Temp_Var_to_Remove=Temp_Table[1, Var]
      
      Var_to_Remove=c()
      for(i in 1:length(Pred_Vars)){
        if(sum(Temp_Var_to_Remove==Name_Dictionary[[Pred_Vars[[i]]]])>0){
          Var_to_Remove=Pred_Vars[[i]]
        }
      }
      
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Pred_Vars==Var_to_Remove)]
      # increase step
      step=step+1
      
      # # if there's no more variable left
      # if(length(Include_Index)==0){
      #   Temp_Table=data.table(
      #     Removed_Var=c("Full", Pred_Vars[Include_Index]),
      #     Estimate=c(AIC(Current_Full_Model)[-1]),
      #     Delta="",
      #     Rank=""
      #   )
      #   Out$summ_table[[step]]=Temp_Table
      #   loop.key=1
      # }
    }
    
  } # while - end
  
  # get the list of primary predictor and confounders
  Temp_Vars=Out$summ_table[[which(Out$Model_AIC==min(Out$Model_AIC))]]$Var
  Selected_Vars=c()
  for(j in 1:length(Temp_Vars)){
    for(i in 1:length(Pred_Vars)){
      if(sum(Temp_Vars[j]==Name_Dictionary[[Pred_Vars[[i]]]])>0){
        Selected_Vars[j]=Pred_Vars[[i]]
      }
    }
  }
  Out$Selected_Vars=Selected_Vars
  
  return(Out)
}



#*************************
#
# GLMM_Backward_by_P_Katya
#
#*************************
GLMM_Backward_by_P_Katya=function(Data, Pred_Vars, Res_Var, Group_Var, which.family, NAGQ){
  
  library("lme4")
  
  vars=Pred_Vars
  vars.ALL=vars
  
  AIC.RES=REMOVE.RES=COL.NAMES=array()
  AIC.RES[1]=NA
  AIC.RES_p=NA
  
  #while(AIC.RES[i]<AIC.RES[i-1]){
  for(i in 1:(length(vars)-1)){
    
    fullmod=as.formula(paste(Res_Var, "~", paste(vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep=""))
    myfit=glmer(fullmod, family=which.family, na.action=na.exclude, data=Data, nAGQ=NAGQ)
    ss=as.matrix(summary(myfit)$coefficients)
    AIC.RES[i+1]=AIC(myfit)[1]
    
    remove.this.id.name=names(which(ss[-1, "Pr(>|z|)"]==max(ss[-1, "Pr(>|z|)"])))
    remove.this.id.name=remove.this.id.name[1]
    AIC.RES_p=c(AIC.RES_p, round2(max(ss[-1, "Pr(>|z|)"]), 2))
    remove.this.id.name.clean=Pred_Vars[sapply(1:length(Pred_Vars), function(x) grepl(Pred_Vars[x], remove.this.id.name))]
    remove.this.id=sapply(1:length(vars), function(x) grepl(remove.this.id.name.clean, vars[x]))
    REMOVE.RES[i]=vars[remove.this.id]
    vars= setdiff(vars, vars[remove.this.id])
    print(i)
    print(AIC.RES)
    print(REMOVE.RES)
  }
  AIC.RES=AIC.RES[-1]
  AIC.RES_p=AIC.RES_p[-1]
  
  RES=cbind(AIC.RES, AIC.RES_p, REMOVE.RES)
  colnames(RES)=c("AIC Before removal", "p Before removal", "Removed variable")
  RES.AIC=RES
  
  Top=(which.min(as.numeric(RES[, "AIC Before removal"]))-1)
  if(Top==0){vars.keep=vars.ALL}else{
    vars.keep=vars.ALL[which(vars.ALL%in%(RES[1:Top, "Removed variable"])==F)]}
  
  return(vars.keep)
}











#*********************************
#
# GLMM_NB_Overdispersion_Estimator
#
#********************************************
# Estimate the overdispersion parameter theta
#********************************************
# Example
#********
# set.seed(101)
# Data_to_use=expand.grid(f1 = factor(1:3),
#                         f2 = LETTERS[1:2], g=1:9, rep=1:15,
#                         KEEP.OUT.ATTRS=FALSE)
# summary(mu <- 5*(-4 + with(Data_to_use, as.integer(f1) + 4*as.numeric(f2))))
# Data_to_use$y <- rnbinom(nrow(Data_to_use), mu = mu, size = 0.5)
# Pred_Vars=c("f1", "f2", "g")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="f1")]="1"
# levels.of.fact[which(Pred_Vars=="f2")]="A"
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="y",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# Overdispersion_Theta=GLMM_NB_Overdispersion_Estimator(Data=Data_to_use,
#                                                       Pred_Vars=c("f1", "f2", "f1:f2"),
#                                                       Res_Var="y",
#                                                       Group_Var="g")
GLMM_NB_Overdispersion_Estimator=function(Data, Pred_Vars, Res_Var, Group_Var){
  # check out packages
  lapply(c("lme4", "MASS"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep=""))
  myfit=glmer.nb(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep="")), 
                 na.action=na.exclude, 
                 data=Data, 
                 nb.control=glmerControl(optimizer=c("Nelder_Mead"), optCtrl=list(maxfun=1e09)), # try "bobyqa" or "Nelder_Mead" if the algorithm fails to converge.
                 verbose=FALSE)
  
  Output=c()
  Output$model_fit=myfit
  Output$theta=getME(myfit, "glmer.nb.theta")
  
  return(Output)
}



#**************************************
#
# [ --- GLMM ( Multinomial ) --- ] ----
#
#***************************
# GLMM_Multinomial_Bivariate
#***************************
# # for now, algorithm works with no interaction term
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data=Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# GLMM_Multinomial_Bivariate(Data=Data_to_use,
#                            Pred_Vars=Pred_Vars, # for now, algorithm works with no interaction term
#                            Res_Var="outcome",
#                            Group_Var="id")
#***************************
# GLMM_Multinomial_Bivariate
GLMM_Multinomial_Bivariate=function(Data,
                                    Pred_Vars,
                                    Res_Var,
                                    Group_Var,
                                    k=2,
                                    maxit=500,
                                    tol=1e-04,
                                    par.update=F){
  # check out packages
  lapply(c("mixcat", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=as.factor(Data[, Res_Var])
  Y_Levels=levels(Data[, Res_Var])
  Y=factor(Data[, Res_Var], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, Group_Var]
  
  # main algorithm
  Output=c()
  for(i in 1:length(Pred_Vars)){
    Temp=GLMM_Multinomial_Multivariate(Data=Data,
                                       Pred_Vars=unlist(Pred_Vars[i]),
                                       Res_Var=Res_Var,
                                       Group_Var=Group_Var,
                                       k=k,
                                       maxit=maxit,
                                       par.update=par.update)
    
    Output$N_data_used=rbind(Output$N_data_used, Temp$N_data_used)
    Output$Estimate=rbind(Output$Estimate, Temp$Estimate)
    Output$Std.Error=rbind(Output$Std.Error, Temp$Std.Error)
    Output$`P-value`=rbind(Output$`P-value`, Temp$`P-value`)
    Output$OR.and.CI=rbind(Output$OR.and.CI, Temp$OR.and.CI)
  }
  
  return(Output)
}



#******************************
# GLMM_Multinomial_Multivariate
#******************************
# Note : The function (nobs) obtaining the number of observations used doesn't work for 'npmlt'. Hence, it's calculated by deleting data with missing value from the input data.
# 
#         ------ Current
#         # delete data with missing value
#         Data=na.omit(Data[, c(Pred_Vars, Res_Var, Group_Var)])
#         Used_N_Rows=nrow(Data)
# 
#         ------ Ideal code (but, not working)
#         #number of observations from a model fit
#         Used_N_Rows=nobs(myfit)
#
# Also, for now, algorithm works with no interaction term
#********************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data$sex=as.character(Data$sex) # make sex categorical
# Data$sex[sample(1:length(Data$sex), 100)]="N"
# Data$sex=factor(Data$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data$outcome[sample(1:length(Data$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data=Format_Columns(Data,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# GLMM_Multinomial_Multivariate(Data,
#                               Pred_Vars,
#                               Res_Var="outcome",
#                               Group_Var="id",
#                               maxit=10,
#                               par.update=T)
#********************************************
# GLMM_Multinomial_Multivariate
GLMM_Multinomial_Multivariate=function(Data,
                                       Pred_Vars,
                                       Res_Var,
                                       Group_Var,
                                       k=2,
                                       maxit=500,
                                       tol=1e-04,
                                       par.update=FALSE){
  # check out packages
  lapply(c("mixcat", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # delete data with missing value
  Data=na.omit(Data[, c(Pred_Vars, Res_Var, Group_Var)])
  Used_N_Rows=nrow(Data)
  
  # values
  Data[, Res_Var]=as.factor(Data[, Res_Var])
  Y_Levels=levels(Data[, Res_Var])
  Y=factor(Data[, Res_Var], levels=c(Y_Levels[-1], Y_Levels[1])) # more the first level to the last that is going to be the baseline level
  ID=Data[, Group_Var]
  
  # assign predictors to values
  for(i in 1:length(Pred_Vars)){
    assign(paste0("X_", i), Data[, Pred_Vars[i]])
  }
  
  # run algorithm until convergence
  while_trs=0
  count=0
  while(while_trs==0){
    count=count+1
    
    # run model
    myfit=npmlt(formula(paste0("Y~", paste0("X_", c(1:length(Pred_Vars)), collapse="+"))),
                formula.npo=formula(paste0("~", paste0("X_", c(1:length(Pred_Vars)), collapse="+"))),
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
      if(k==1){k=2}else if(k>=length(Pred_Vars)-1){k=2}
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
  
  # Output
  Output=c()
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=myfit
  
  Output$converged.set=data.table(count=count, k=k, maxit=maxit, tol=tol)
  Output$Estimate=t(matrix(round2(Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  Output$Std.Error=t(matrix(round2(SE.Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  Output$`P-value`=t(matrix(ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                                   format(round2(P_values[Coef.ind], 3), nsmall=3)), nrow=length(levels(Y))-1))
  Output$OR.and.CI=t(matrix(paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
                                   " (",
                                   format(round(Lower_Bound, 2), nsmall=2),
                                   " - ",
                                   format(round(Upper_Bound, 2), nsmall=2),
                                   ")"), nrow=length(levels(Y))-1))
  
  # name Output rows
  row_names=c()
  for(i in 1:length(Pred_Vars)){
    X=get(paste0("X_", i))
    if(is.factor(X)){
      X_Levels=levels(X)
      row_names=c(row_names, paste0(Pred_Vars[i], " / ", X_Levels[-1]))
    }else if(is.numeric(X)){
      row_names=c(row_names, Pred_Vars[i])
    }
  }
  colnames(Output$Estimate)=levels(Y)[-length(levels(Y))]
  colnames(Output$Std.Error)=levels(Y)[-length(levels(Y))]
  colnames(Output$`P-value`)=levels(Y)[-length(levels(Y))]
  colnames(Output$OR.and.CI)=levels(Y)[-length(levels(Y))]
  rownames(Output$Estimate)=row_names
  rownames(Output$Std.Error)=row_names
  rownames(Output$`P-value`)=row_names
  rownames(Output$OR.and.CI)=row_names
  
  return(Output)
}


#**********************************
#
# [ --- CLMM ( Ordinal ) --- ] ----
#
#**********************************
# Explanation on why the sign of beta for proportional odds is negative
#**********************************************************************
# Note that the use of a cdf of form G(y*-n). for the latent variable
# results in linear predictor a_{j}-B'x rather than a_{j}-B'x When B>0, as x j j
# increases each cumulative logit then decreases, so each cumulative probability
# decreases and relatively less probability mass falls at the low end of the Y
# scale. Thus, Y tends to be larger at higher values of x. With this parameterization
# the sign of B has the usual meaning. However, most software (e.g.,
# SAS) uses form (7.5).
# The explanation above is from Categorical Data Analysis, p.279

# For this reason, the estimate for scale effects (proportional odds) is obtained with the negative sign in the output of clmm2() and can be interpreted with the useful meaning.
# (see p.15 and 16 in Cumulative Link Models for Ordinal Regression with the R Package ordinal.pdf for the model formulation and an example of interpretation)
# On the other hand, the estimate for nomial effect (non-proportional odds) is (weirdly) produced in the output with the positive sign. This implies that the interpretation is bound to be the opposite.
# (see p.19)

#***********************
# CLMM_Ordinal_Bivariate
#***********************
# # for now, algorithm works with no interaction term
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data_to_use$outcome=as.factor(Data_to_use$outcome)
# Data_to_use$id=as.factor(Data_to_use$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data=Data_to_use,
#                                          Pred_Vars=Pred_Vars,
#                                          Res_Var="outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(Pred_Vars))
# Type_Odds[Pred_Vars%in%Output$sig_vars]="Non_Prop"
# Type_Odds[1:3]="Non_Prop"
# #Two arguments (Res_Var and Group_Var) must be declared with!
# CLMM_Ordinal_Bivariate(Data=Data_to_use,
#                        Pred_Vars<-Pred_Vars, # for now, algorithm works with no interaction term
#                        Type_Odds=Type_Odds,
#                        Res_Var<-"outcome",
#                        Group_Var<-"id",
#                        NAGQ=3)
#********************************
# CLMM_Ordinal_Bivariate
CLMM_Ordinal_Bivariate=function(Data,
                                Pred_Vars,
                                Type_Odds,
                                Res_Var,
                                Group_Var,
                                NAGQ=3){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, Group_Var]=factor(Data[, Group_Var], order=F)
  
  # main algorithm
  Output=c()
  
  for(i in 1:length(Pred_Vars)){
    #i=7
    i<<-i
    if(Type_Odds[i]=="Prop"){
      model_fit=clmm2(as.formula(paste(Res_Var, "~", Pred_Vars[i])),
                      #nominal=as.formula(paste("~", Nom_Vars[i])),
                      random=eval(parse(text=Group_Var)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }else if(Type_Odds[i]=="Non_Prop"){
      model_fit=clmm2(as.formula(paste(Res_Var, "~1")),
                      nominal=as.formula(paste("~", Pred_Vars[i])),
                      random=eval(parse(text=Group_Var)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic")
    }
    
    model_fit.summ=summary(model_fit)$coefficients
    Coef.ind=which(grepl(Pred_Vars[i], row.names(model_fit.summ)))
    # coefficient
    Coef=model_fit.summ[Coef.ind, 1]
    # standard error
    SE.Coef=model_fit.summ[Coef.ind, 2]
    
    #
    if(Type_Odds[i]=="Prop"){
      # confidence interval (exponentiated)
      Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
      Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
      # CI (upper and lower bounds)
      Upper_Bound=exp(Raw_Upper_Bound)
      Lower_Bound=exp(Raw_Lower_Bound)
      
      # Output
      temp_out=c()
      
      temp_out$Estimate=round2(Coef, 3)
      temp_out$Std.Error=round2(SE.Coef, 3)
      temp_out$`P-value`=ifelse(model_fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                                format(round2(model_fit.summ[Coef.ind, 4], 3), nsmall=3))
      temp_out$COR.and.CI=paste0(format(round(exp(Coef), 2), nsmall=2), 
                                 " (",
                                 format(round(Lower_Bound, 2), nsmall=2),
                                 " - ",
                                 format(round(Upper_Bound, 2), nsmall=2),
                                 ")")
      temp_out=as.data.frame(temp_out)
      colnames(temp_out)=c("Estimate", "Std.Error", "P-value", "COR.and.CI")
      
      if(is.factor(Data[, Pred_Vars[i]])){
        name_temp=expand.grid(levels(Data[, Pred_Vars[i]])[-1])
        row.names(temp_out)=paste0(Pred_Vars[i], " ", unlist(name_temp))
      }else if(is.numeric(Data[, Pred_Vars[i]])){
        row.names(temp_out)=paste0(Pred_Vars[i])
      }
      # Output
      # number of observations from a model fit
      Used_N_Rows=nobs(model_fit)
      temp_obs=as.matrix(paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)"))
      rownames(temp_obs)=Pred_Vars[i]
      Output$N_data_used=rbind(Output$N_data_used, temp_obs)
      # 
      Output$Prop_Odds=rbind(Output$Prop_Odds, temp_out)
    }else if(Type_Odds[i]=="Non_Prop"){
      # multiply -1 to change the sign to the negative
      Coef=Coef*(-1)
      
      # confidence interval (exponentiated)
      Raw_Upper_Bound=Coef+qnorm(0.975)*SE.Coef
      Raw_Lower_Bound=Coef-qnorm(0.975)*SE.Coef
      # CI (upper and lower bounds)
      Upper_Bound=exp(Raw_Upper_Bound)
      Lower_Bound=exp(Raw_Lower_Bound)
      
      # Output
      temp_out=c()
      # record 
      if(is.factor(Data[, Pred_Vars[i]])){
        X_Levels=levels(Data[, Pred_Vars[i]])
        Y_Levels=levels(Data[, Res_Var])
        temp_out$Estimate=matrix(round2(Coef, 3), 
                                 ncol=length(X_Levels)-1, 
                                 nrow=length(Y_Levels)-1)
        temp_out$Std.Error=matrix(round2(SE.Coef, 3), 
                                  ncol=length(X_Levels)-1, 
                                  nrow=length(Y_Levels)-1)
        temp_out$`P-value`=matrix(
          ifelse(model_fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                 format(round2(model_fit.summ[Coef.ind, 4], 3), nsmall=3)), 
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
        Temp_Column_Names=paste0(Pred_Vars[i], " / ", X_Levels[-1])
      }else if(is.numeric(Data[, Pred_Vars[i]])){
        temp_out$Estimate=data.frame(X=round2(Coef, 3))
        temp_out$Std.Error=data.frame(X=round2(SE.Coef, 3))
        temp_out$`P-value`=data.frame(
          X=ifelse(model_fit.summ[Coef.ind, 4]<0.001, "<0.001", 
                   format(round2(model_fit.summ[Coef.ind, 4], 3), nsmall=3))
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
        Temp_Row_Names=levels(Data[, Res_Var])[-1]
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
      
      # Output
      # number of observations from a model fit
      Used_N_Rows=nobs(model_fit)
      temp_obs=as.matrix(paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)"))
      rownames(temp_obs)=Pred_Vars[i]
      Output$N_data_used=rbind(Output$N_data_used, temp_obs)
      # 
      Output$Non_Prop_Odds$Estimate=rbind(Output$Non_Prop_Odds$Estimate, t(temp_out$Estimate))
      Output$Non_Prop_Odds$Std.Error=rbind(Output$Non_Prop_Odds$Std.Error, t(temp_out$Std.Error))
      Output$Non_Prop_Odds$`P-value`=rbind(Output$Non_Prop_Odds$`P-value`,t(temp_out$`P-value`))
      Output$Non_Prop_Odds$COR.and.CI=rbind(Output$Non_Prop_Odds$COR.and.CI, t(temp_out$COR.and.CI))
    }
    print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
  }
  return(Output)
}

#***************************
# CLMM_Ordinal_Multivariable
#***************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data_to_use$outcome=as.factor(Data_to_use$outcome)
# Data_to_use$id=as.factor(Data_to_use$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data=Data_to_use,
#                                          Pred_Vars=Pred_Vars,
#                                          Res_Var="outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(Pred_Vars))
# Type_Odds[Pred_Vars%in%Output$sig_vars]="Non_Prop"
# Type_Odds[1:3]="Non_Prop"
# #Two arguments (Res_Var and Group_Var) must be declared with '<-'!
# CLMM_Ordinal_Multivariable(Data=Data_to_use,
#                            Pred_Vars=Pred_Vars, # for now, algorithm works with no interaction term
#                            Type_Odds=Type_Odds,
#                            Res_Var<-"outcome",
#                            Group_Var<-"id",
#                            NAGQ=3)
#***************************
# CLMM_Ordinal_Multivariable
CLMM_Ordinal_Multivariable=function(Data,
                                    Pred_Vars,
                                    Type_Odds,
                                    Res_Var,
                                    Group_Var,
                                    NAGQ=3){
  # check out packages
  lapply(c("ordinal", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  Data[, Group_Var]=factor(Data[, Group_Var], order=F)
  
  # main algorithm
  Output=c()
  Loc_Vars<<-Pred_Vars[Type_Odds=="Prop"]
  Nom_Vars<<-Pred_Vars[Type_Odds=="Non_Prop"]
  
  if(length(Loc_Vars)==0){ # if all are not proportional odds
    model_fit=clmm2(as.formula(paste(Res_Var, "~1")),
                    nominal=as.formula(paste("~", paste(Nom_Vars, collapse="+"))),
                    random=eval(parse(text=Group_Var)),
                    data=Data,
                    Hess=TRUE,
                    nAGQ=NAGQ,
                    link="logistic")
  }else if(length(Nom_Vars)==0){ # if all are proportional odds
    model_fit=clmm2(as.formula(paste(Res_Var, "~", paste(Loc_Vars, collapse="+"))),
                    #nominal=as.formula(paste("~", paste(Nom_Vars, collapse="+"))),
                    random=eval(parse(text=Group_Var)),
                    data=Data,
                    Hess=TRUE,
                    nAGQ=NAGQ,
                    link="logistic")
  }else if(length(Loc_Vars)!=0 & length(Nom_Vars)!=0){ # mixed
    model_fit=clmm2(as.formula(paste(Res_Var, "~", paste(Loc_Vars, collapse="+"))),
                    nominal=as.formula(paste("~", paste(Nom_Vars, collapse="+"))),
                    random=eval(parse(text=Group_Var)),
                    data=Data,
                    Hess=TRUE,
                    nAGQ=NAGQ,
                    link="logistic")
  }
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  
  # save model fit
  Output$model_fit=model_fit
  Summary_coefficients=data.frame(summary(model_fit)$coefficients)
  model_fit.summ=Summary_coefficients[-c(1:(length(levels(Data[, Res_Var]))-1)), ]
  
  # coefficient
  Coef=model_fit.summ[, 1]
  SE.Coef=model_fit.summ[, 2]
  
  # names
  names(Coef)=rownames(model_fit.summ)
  names(SE.Coef)=rownames(model_fit.summ)
  
  #
  Output$Prop_Odds=c()
  Output$Non_Prop_Odds=c()
  for(i in 1:length(Pred_Vars)){
    Target_Ind=grep(Pred_Vars[i], names(Coef))
    
    if(Type_Odds[i]=="Prop"){
      # confidence interval (exponentiated)
      Raw_Upper_Bound=Coef[Target_Ind]+qnorm(0.975)*SE.Coef[Target_Ind]
      Raw_Lower_Bound=Coef[Target_Ind]-qnorm(0.975)*SE.Coef[Target_Ind]
      # CI (upper and lower bounds)
      Upper_Bound=exp(Raw_Upper_Bound)
      Lower_Bound=exp(Raw_Lower_Bound)
      
      # Output
      temp_out=c()
      
      temp_out$Estimate=round2(Coef[Target_Ind], 3)
      temp_out$Std.Error=round2(SE.Coef[Target_Ind], 3)
      temp_out$`P-value`=ifelse(model_fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                                format(round2(model_fit.summ[Target_Ind, 4], 3), nsmall=3))
      temp_out$COR.and.CI=paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                                 " (",
                                 format(round(Lower_Bound, 2), nsmall=2),
                                 " - ",
                                 format(round(Upper_Bound, 2), nsmall=2),
                                 ")")
      temp_out=data.frame(temp_out)
      colnames(temp_out)=c("Estimate", "Std.Error", "P-value", "COR.and.CI")
      
      if(is.factor(Data[, Pred_Vars[i]])){
        name_temp=expand.grid(levels(Data[, Pred_Vars[i]])[-1])
        row.names(temp_out)=paste0(Pred_Vars[i], " ", unlist(name_temp))
      }else if(is.numeric(Data[, Pred_Vars[i]])){
        row.names(temp_out)=paste0(Pred_Vars[i])
      }
      # Output
      Output$Prop_Odds=rbind(Output$Prop_Odds, temp_out)
    }else if(Type_Odds[i]=="Non_Prop"){
      # multiply -1 to change the sign to the negative
      Coef[Target_Ind]=Coef[Target_Ind]*(-1)
      
      # confidence interval (exponentiated)
      Raw_Upper_Bound=Coef[Target_Ind]+qnorm(0.975)*SE.Coef[Target_Ind]
      Raw_Lower_Bound=Coef[Target_Ind]-qnorm(0.975)*SE.Coef[Target_Ind]
      # CI (upper and lower bounds)
      Upper_Bound=exp(Raw_Upper_Bound)
      Lower_Bound=exp(Raw_Lower_Bound)
      
      # Output
      temp_out=c()
      # record 
      if(is.factor(Data[, Pred_Vars[i]])){
        X_Levels=levels(Data[, Pred_Vars[i]])
        Y_Levels=levels(Data[, Res_Var])
        temp_out$Estimate=matrix(round2(Coef[Target_Ind], 3), 
                                 ncol=length(X_Levels)-1, 
                                 nrow=length(Y_Levels)-1)
        temp_out$Std.Error=matrix(round2(SE.Coef[Target_Ind], 3), 
                                  ncol=length(X_Levels)-1, 
                                  nrow=length(Y_Levels)-1)
        temp_out$`P-value`=matrix(
          ifelse(model_fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                 format(round2(model_fit.summ[Target_Ind, 4], 3), nsmall=3)), 
          ncol=length(X_Levels)-1, 
          nrow=length(Y_Levels)-1)
        temp_out$COR.and.CI=matrix(
          paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                 " (",
                 format(round(Lower_Bound, 2), nsmall=2),
                 " - ",
                 format(round(Upper_Bound, 2), nsmall=2),
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
          X=ifelse(model_fit.summ[Target_Ind, 4]<0.001, "<0.001", 
                   format(round2(model_fit.summ[Target_Ind, 4], 3), nsmall=3))
        )
        temp_out$COR.and.CI=data.frame(
          X=paste0(format(round(exp(Coef[Target_Ind]), 2), nsmall=2), 
                   " (",
                   format(round(Lower_Bound, 2), nsmall=2),
                   " - ",
                   format(round(Upper_Bound, 2), nsmall=2),
                   ")")
        )
        # names for row and column
        Temp_Row_Names=levels(Data[, Res_Var])[-1]
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
      
      # Output
      Output$Non_Prop_Odds$Estimate=rbind(Output$Non_Prop_Odds$Estimate, t(temp_out$Estimate))
      Output$Non_Prop_Odds$Std.Error=rbind(Output$Non_Prop_Odds$Std.Error, t(temp_out$Std.Error))
      Output$Non_Prop_Odds$`P-value`=rbind(Output$Non_Prop_Odds$`P-value`,t(temp_out$`P-value`))
      Output$Non_Prop_Odds$COR.and.CI=rbind(Output$Non_Prop_Odds$COR.and.CI, t(temp_out$COR.and.CI))
    }
  }
  
  #print(paste(i, " ", Pred_Vars[i], sep=""))
  return(Output)
}

#**************************
# CLMM_Confounder_Selection
#**************************
# Example
#***************************
# # for now, algorithm works with no interaction term
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data_to_use$outcome=as.factor(Data_to_use$outcome)
# Data_to_use$id=as.factor(Data_to_use$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data=Data_to_use,
#                                          Pred_Vars=Pred_Vars,
#                                          Res_Var="outcome")
# Output$sig_vars # these variables are better to be assigned as nominal variables
# Type_Odds=rep("Prop", length=length(Pred_Vars))
# Type_Odds[Pred_Vars%in%Output$sig_vars]="Non_Prop"
# #Two arguments (Res_Var and Group_Var) must be declared with '<-'!
# CLMM.fit=CLMM_Ordinal_Multivariable(Data=Data_to_use,
#                                     Pred_Vars=Pred_Vars,
#                                     Type_Odds=Type_Odds,
#                                     Res_Var<-"outcome",
#                                     Group_Var<-"id",
#                                     NAGQ<-3)
# # The value 'Data' and 'NAGC' must be defined in the global environme!
# Data=Data_to_use
# Confounder_Steps=CLMM_Confounder_Selection(Full_Model=CLMM.fit$model_fit,
#                                            Main_Pred_Var="sex",
#                                            Main_Pred_Var_Type_Odds<-Type_Odds[Pred_Vars=="sex"],
#                                            Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                            Potential_Con_Vars_Type_Odds<-Type_Odds[Pred_Vars!="sex"],
#                                            Min.Change.Percentage=5,
#                                            Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Steps$Confounders
CLMM_Confounder_Selection=function(Full_Model,
                                   Main_Pred_Var,
                                   Main_Pred_Var_Type_Odds,
                                   Potential_Con_Vars,
                                   Potential_Con_Vars_Type_Odds,
                                   Min.Change.Percentage=5, # minimum percentage of change-in-estimate to terminate the algorithm
                                   Estimate="raw_estimate"){
  #*****
  # Note
  #*****
  # For now, algorithm works with no interaction term
  # Due to the nature of 'clmm2', parameters must be defined and assigned with values in the global environment, including location, random, data, and nAGQ.
  # The other parameters are not currently considered.
  
  # Full_Model : An existing fit from the cumulative link mixed model (clmm2) function
  # Main_Pred_Var_Type_Odds : type of cumulative odds for the primary predictor variable of interest
  # Potential_Con_Vars_Type_Odds : type of cumulative odds for potential confounders
  # "Prop" for proportional; "Non_Prop" for non-proportional
  
  # check packages
  lapply(c("data.table"), checkpackages)

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
      
      if(Removed_Var_Temp%in%Loc_Vars){ # if the variable to remove is proportional odds
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
      if(Removed_Var_Temp%in%Nom_Vars){ # if the variable to remove is non-proportional odds
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
    
    if(min(as.numeric(Temp_Table$Delta[-1]))>Min.Change.Percentage){ # if the minimum change-in-estimate is larger than Min.Change.Percentage, terminate the while loop
      loop.key=1
    }else{
      # decide the variable to remove
      Var_to_Remove=Temp_Table[Rank==min(Rank, na.rm=T), Removed_Var]
      # update Include_Index
      Include_Index=Include_Index[Include_Index!=which(Potential_Con_Vars==Var_to_Remove[1])]
      # update the current full model
      #Current_Full_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Potential_Con_Vars[setdiff(1:length(Potential_Con_Vars), Include_Index)], collapse="-"))))
      if(Var_to_Remove%in%Loc_Vars){ # if the variable to remove is proportional odds
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
      if(Var_to_Remove%in%Nom_Vars){ # if the variable to remove is non-proportional odds
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


#**********************
# CLMM_Confounder_Model
#**********************
# # for now, algorithm works with no interaction term
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data=Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars=Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# Data_to_use$outcome=as.factor(Data_to_use$outcome)
# Data_to_use$id=as.factor(Data_to_use$id)
# Main_Pred_Var="sex"
# #Some arguments (Data, Res_Var, Group_Var, and NAGQ) must be declared with '<-' in a function!
# CLMM_Confounder=CLMM_Confounder_Model(Data<-Data_to_use,
#                                       Main_Pred_Var=Main_Pred_Var,
#                                       Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var], # for now, algorithm works with no interaction term
#                                       Res_Var<-"outcome",
#                                       Group_Var<-"id",
#                                       NAGQ<-3,
#                                       Min.Change.Percentage=5,
#                                       Estimate<-"raw_estimate") # raw_estimate, converted_estimate
# CLMM_Confounder$Proportional_Assumption
# CLMM_Confounder$Full_Multivariable_Model$Prop_Odds
# CLMM_Confounder$Full_Multivariable_Model$Non_Prop_Odds
# CLMM_Confounder$Confounder_Steps$Confounders
# CLMM_Confounder$Confounder_Model$Prop_Odds
# CLMM_Confounder$Confounder_Model$Non_Prop_Odds
CLMM_Confounder_Model=function(Data,
                               Main_Pred_Var,
                               Potential_Con_Vars,
                               Res_Var,
                               Group_Var,
                               NAGQ=3,
                               Min.Change.Percentage=5,
                               Estimate="raw_estimate"){
  Output=c()
  # proportional odds assumption test
  Proportional_Assumption=Proportional_Odds_Assumption_Test(Data<-Data,
                                                            Pred_Vars<-c(Main_Pred_Var, Potential_Con_Vars),
                                                            Res_Var<-Res_Var)
  Proportional_Assumption$sig_vars # these variables are better to be assigned as nominal variables
  Type_Odds=rep("Prop", length=length(Pred_Vars))
  Type_Odds[Pred_Vars%in%Proportional_Assumption$sig_vars]="Non_Prop"
  
  #
  Output$Proportional_Assumption=Proportional_Assumption
  
  # Full multivariable model
  print(paste0("Full multivariable model begins."))
  Output$Full_Multivariable_Model=CLMM_Ordinal_Multivariable(Data=Data,
                                                             Pred_Vars=Pred_Vars,
                                                             Type_Odds=Type_Odds,
                                                             Res_Var=Res_Var,
                                                             Group_Var=Group_Var,
                                                             NAGQ=NAGQ)
  print(paste0("Full multivariable model completed."))
  # Confounder selection
  Confounder_Steps=CLMM_Confounder_Selection(Full_Model=Output$Full_Multivariable_Model$model_fit,
                                             Main_Pred_Var=Main_Pred_Var,
                                             Main_Pred_Var_Type_Odds=Type_Odds[Pred_Vars==Main_Pred_Var],
                                             Potential_Con_Vars=Pred_Vars[Pred_Vars!=Main_Pred_Var],
                                             Potential_Con_Vars_Type_Odds=Type_Odds[Pred_Vars!=Main_Pred_Var],
                                             Min.Change.Percentage=Min.Change.Percentage,
                                             Estimate=Estimate) # raw_estimate, converted_estimate
  
  
  # save all stepwise procedure
  Output$Confounder_Steps=Confounder_Steps
  
  # index of confounders
  Confounder_Ind=which(Pred_Vars%in%Output$Confounder_Steps$Confounders)
  
  # Multivariable model with confounders
  Output$Confounder_Model=CLMM_Ordinal_Multivariable(Data=Data,
                                                     Pred_Vars=Pred_Vars[Confounder_Ind],
                                                     Type_Odds=Type_Odds[Confounder_Ind],
                                                     Res_Var<-Res_Var,
                                                     Group_Var<-Group_Var,
                                                     NAGQ=NAGQ)
  return(Output)
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
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# 
# Data_to_use$sex=as.character(Data_to_use$sex) # make sex categorical
# Data_to_use$sex[sample(1:length(Data_to_use$sex), 100)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# 
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$outcome[sample(1:length(Data_to_use$outcome), 150)]=2 # make the outcome multinomial (categorical)
# Data_to_use=Format_Columns(Data_to_use,
#                     Res_Var="outcome",
#                     Pred_Vars,
#                     vector.OF.classes.num.fact,
#                     levels.of.fact)
# Data_to_use$outcome=as.factor(Data_to_use$outcome)
# Data_to_use$id=as.factor(Data_to_use$id)
# # proportional odds assumption test
# Output=Proportional_Odds_Assumption_Test(Data<-Data_to_use,
#                                          Pred_Vars<-Pred_Vars,
#                                          Res_Var<-"outcome")
# # covariates that violate the assumption
# Output$sig_vars
Proportional_Odds_Assumption_Test=function(Data,
                                           Pred_Vars,
                                           Res_Var){
  # check out packages
  lapply(c("ordinal"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # values
  Data[, Res_Var]=factor(Data[, Res_Var], order=T)
  
  # main algorithm
  Output=c()
  lr_test=list()
  p_value=c()
  for(i in 1:length(Pred_Vars)){
    #i=1
    Covariate<<-Pred_Vars[i]
    model_fit=clm(as.formula(paste0(Res_Var, "~", Covariate)),
                  #nominal=as.formula(paste("~", Pred_Vars[i])),
                  data=Data)
    lr_test[[i]]=nominal_test(model_fit)
    p_value[i]=lr_test[[i]]$`Pr(>Chi)`[-1]
  }
  p_value[is.na(p_value)]=1
  Output$lr_test=lr_test
  Output$p_value=p_value
  Output$sig_vars=Pred_Vars[p_value<0.05]
  
  return(Output)
}
# Proportional_Odds_Assumption_Test_CLMM=function(Data,
#                                                 Pred_Vars,
#                                                 Group_Var,
#                                                 Res_Var){
#   # check out packages
#   lapply(c("ordinal"), checkpackages)
#   
#   # Cat_Vars
#   Cat_Vars=Pred_Vars[sapply(Data[, .SD, .SDcols=Pred_Vars], class)=="factor"]
#   
#   # as data frame
#   Data=as.data.frame(Data)
#   
#   # Group_Var
#   Group_Var=Group_Var
# 
#   # values
#   Data[, Res_Var]=factor(Data[, Res_Var], order=T)
#   
#   # main algorithm
#   Output=c()
#   lr_test=list()
#   p_value=c()
#   for(i in 1:length(Cat_Vars)){
#     #i=19
#     Covariate<<-Cat_Vars[i]
#     prop_model_fit=clmm2(as.formula(paste0(Res_Var, "~", Covariate)),
#                          random=eval(parse(text=Group_Var)),
#                          #nominal=as.formula(paste("~", Cat_Vars[i])),
#                          data=Data)
#     non_prop_model_fit=clmm2(as.formula(paste0(Res_Var, "~ 1")),
#                              random=eval(parse(text=Group_Var)),
#                              nominal=as.formula(paste("~", Covariate)),
#                              data=Data)
#     lr_test[[i]]=anova(prop_model_fit, non_prop_model_fit)
#     p_value[i]=lr_test[[i]]$`Pr(Chi)`[-1]
#   }
#   p_value[is.na(p_value)]=1
#   Output$lr_test=lr_test
#   Output$p_value=p_value
#   Output$sig_vars=Cat_Vars[p_value<0.05]
#   
#   return(Output)
# }

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
# Data_to_use=respiratory
# head(Data)
# GAMM_Bivariate_Plot(Data=Data_to_use,
#                     Pred_Var="age",
#                     Res_Var="outcome",
#                     Group_Var="id",
#                     #Group_Var=NA,
#                     which.family="binomial (link='logit')",
#                     xlab="age",
#                     ylab="outcome",
#                     title="Title",
#                     x_breaks=seq(round(min(Data_to_use$age)-5, -1), round(max(Data_to_use$age)+5, -1), by=10))
GAMM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, Group_Var=NA, which.family, xlab="", ylab="", title="", x_breaks=0){
  # result
  Out=c()
  
  # check out packages
  lapply(c("mgcv", "ggplot2", "sjPlot"), checkpackages)
  
  # Data as data frame
  Data=as.data.frame(Data)
  
  # gamm model
  gamm_model=as.formula(paste(Res_Var, "~s(", Pred_Var, ")"))
  
  # random effect
  random_effect=list(id=~1)
  names(random_effect)=Group_Var
  
  # run gamm
  if(is.na(Group_Var)){
    gaml=gamm(gamm_model, data=Data, family=eval(parse(text=which.family)))
  }else{
    gaml=gamm(gamm_model, random=random_effect, data=Data, family=eval(parse(text=which.family)))
  }
  Out$gaml=gaml
  
  # save random effect (an error arises for an unknown reason, so let's comment this part for now)
  #Out$re_plot=plot_model(gaml$gam)
  
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
  if(grepl("binomial", which.family)){ # code for 'binomial' family
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
  }else if(grepl("poisson", which.family)){ # code for 'poisson' family
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
# Data_to_use=respiratory
# head(Data)
# GAM_Bivariate_Plot(Data=Data_to_use,
#                    Pred_Var="age",
#                    Res_Var="outcome",
#                    which.family="binomial (link='cloglog')",
#                    xlab="age",
#                    ylab="outcome",
#                    title="Title",
#                    x_breaks=seq(round(min(Data_to_use$age)-5, -1), round(max(Data_to_use$age)+5, -1), by=10))
GAM_Bivariate_Plot=function(Data, Pred_Var, Res_Var, which.family, xlab="", ylab="", title="", x_breaks=0){
  # check out packages
  lapply(c("mgcv", "ggplot2"), checkpackages)
  
  # Data as data frame
  Data=as.data.frame(Data)
  
  # gamm model
  gamm_model=as.formula(paste(Res_Var, "~s(", Pred_Var, ")"))
  
  # run gamm
  gaml=gam(gamm_model, data=Data, family=eval(parse(text=which.family)))
  
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
  if(grepl("binomial", which.family)){ # code for 'binomial' family
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
  }else if(grepl("poisson", which.family)){ # code for 'poisson' family
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

#****************************
#
# [ --- Imputation --- ] ----
#
#****************************
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
# Data_to_use=respiratory
# Pred_Vars=c("center", "id", "treat", "sex", "age", "baseline", "visit")
# Res_Var="outcome"
# Group_Var="id"
# which.family="binomial"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# # generate 100 missing data in outcome
# Data_to_use=as.data.table(Data_to_use)
# Data_to_use[sample(1:nrow(Data_to_use), 100), "outcome"]=NA
# # multiple imputation using Amelia
# set.seed(2019) # set seed in order to obtain the same imputed values for the future reproduction of analysis results
# m=2 # the number of imputed datasets to creates
# amelia.imp=amelia(Data_to_use[,
#                               .SD,
#                               .SDcols=c(Pred_Vars, Res_Var)],
#                   p2s=0,
#                   Mvalue="NA",
#                   m=m,
#                   noms=c(Pred_Vars[vector.OF.classes.num.fact=="fact"], "outcome"), # all time-varying variates with missing data are nominal, so we need to specify them
#                   ts=c("visit"), # column number or variable name indicating the variable identifying time in time series data
#                   cs=c("id"), # column number or variable name indicating the cross section variable
#                   idvars=c(), # identification variable (variables that have no information except labels)
#                   polytime=1, # 0 under the assumption of no time effect
#                   interacs=FALSE # TRUE if time effects of polytime vary across the cross-section
# )
# # GEE using the 1st imputed data set
# GEE.result.1=GEE_Multivariable(amelia.imp$imputations$imp1, Pred_Vars, Res_Var, Group_Var, which.family)$summ_table %>%
#   as.data.table(keep.rownames=TRUE)
# # GEE using the 2nd imputed data set
# GEE.result.2=GEE_Multivariable(amelia.imp$imputations$imp2, Pred_Vars, Res_Var, Group_Var, which.family)$summ_table %>%
#   as.data.table(keep.rownames=TRUE)
# # combine results
# GEE.combined.result=Combine_Multiple_Results(Input_Data_Names=c("GEE.result.1", "GEE.result.2"))
# GEE.combined.result
Combine_Multiple_Results=function(Input_Data_Names){
  lapply(c("data.table", "magrittr", "dplyr"), checkpackages)
  
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
  Output=data.table(
    rn=get(paste0(Input_Data_Names[m.ind]))[, rn],
    Estimate=round(est.coef.mean, 3),
    Std.Error=round(sqrt(est.var.mean+par.var*(1+1/m)), 3), # standard error of the multiple imputation point estimate
    P.value=ifelse(P.value<0.001, "<0.001",
                   format(round2(P.value, 7), nsmall=3)) # assuming that the sampling distribution of statistic is normal distribution
  )
  # compute OR and 95% CI
  Output[,
         OR.and.CI:=paste0(round(exp(Estimate), 2),
                           " (", round(exp(Output[, Estimate]-qnorm(0.975)*Output[, Std.Error]), 2),
                           " - ",
                           round(exp(Output[, Estimate]+qnorm(0.975)*Output[, Std.Error]), 2), ")")
  ]
  
  return(Output)
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
  lapply(c("aSPC"), checkpackages)
  # Output
  Output=c()
  # run aSPC
  aSPC.result=aSPC(X, Y, pow=2*alpha, B=nrep)
  # optimal alpha
  Output$opt.alpha=alpha[which.min(aSPC.result[names(aSPC.result)!="aSPC"])]
  # aSPC result
  Output$aSPC=aSPC.result[names(aSPC.result)=="aSPC"]
  # Estimated contributions
  Output$Est.Contribution=data.frame(Contribution=EstContribution(X, Y, alpha=Output$opt.alpha))
  # Call to plot() to plot the contributions and threshold.
  plot(Output$Est.Contribution$Contribution, type='l', main="X and Y (alpha=1)", 
       xlab="Explanatory Variables", ylab="Contribution", 
       cex=1.3, cex.lab=1.3, cex.axis=1.3, cex.main=2.5, cex.sub=1.3)
  # Calculate threshold
  Output$threshold=Threshold(X, Y, alpha=Output$opt.alpha, level=level, nrep=nrep)
  # Draw the threshold in the plot
  abline(a=Output$threshold, b=0, col="blue")
  return(Output)
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
#                             Ind_P_Value=F)
Contingency_Table_Generator=function(Data, Row_Var, Col_Var, Ref_of_Row_Var, Missing="Not_Include", Ind_P_Value=F){
  # library
  library(epitools)
  
  # Data as data table
  Data=as.data.frame(Data)
  
  # Row_levels
  Row_Levels=levels(Data[, Row_Var])
  
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
  if(nrow(Contingency_Table)>1 & ncol(Contingency_Table)>1){
    Out[Value==Ref_of_Row_Var, c("P-value (Fisher)")]=ifelse(fisher.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ], simulate.p.value=TRUE)$p.value<0.001,
                                                             "<0.001",
                                                             paste0(round(fisher.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ], simulate.p.value=TRUE)$p.value, 3)))
    Out[Value==Ref_of_Row_Var, c("P-value (Chi-square)")]=ifelse(chisq.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ])$p.value<0.001,
                                                                 "<0.001",
                                                                 paste0(round(chisq.test(Contingency_Table[!rownames(Contingency_Table)=="NA", ])$p.value, 3)))  # return
  }
  
  return(Out[order(match(Out$Value, Row_Levels)), ])
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
    if(length(unique(Data[, Col_Var][!is.na(Data[, Col_Var])]))==1){
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
      OR.and.CI=""
      P.value=""
      P.value_Mann_Whitney=""
      P.value_T_test=""
      P.value_ANOVA=""
      
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
      with(na.omit(Data[, .SD, .SDcols=c(Row_Var, Col_Var)]), do.call(rbind, by(eval(parse(text=Row_Var)), eval(parse(text=Col_Var)), summary)))[, 1:6], # excluding NA's
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
  return(as.data.table(Out))
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
# Contingency_Table_Univariable(Data=BL_Data, Var="sex")
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
# Contingency_Table_Univariable_Conti_X(Data=BL_Data, Var="age")
Contingency_Table_Univariable_Conti_X=function(Data, Var, Form=1){
  Data=as.data.frame(Data)
  
  Table=round(summary(Data[, Var]), 2)
  
  if(Form==1){
    Out=cbind(Var, 
              Value="Median (IQR)", 
              paste0(Table[3], " (", Table[5]-Table[2], ")"))
    
  }else if(Form==2){
    Out=cbind(Var,
              Value=c(names(Table)),
              data.table(unclass(Table)))
  }
  colnames(Out)=c("Variable", "Value", paste0("Total (n=", nrow(Data), ")")) 
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
# Data_to_use=respiratory
# 
# #
# Combined_CT=rbind(
#   Contingency_Table_Generator(Data=Data_to_use,
#                               Row_Var="sex",
#                               Col_Var="outcome",
#                               Ref_of_Row_Var="F",
#                               Missing="Include"),
#   Contingency_Table_Generator_Conti_X(Data=Data_to_use,
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
#          "epitools",
#          "geepack"
# ),
# checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# # randomly generate NAs in some variables
# Data_to_use$sex[sample(1:nrow(Data_to_use), 30)]=NA
# Data_to_use$age[sample(1:nrow(Data_to_use), 30)]=NA
# Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]=NA
# #Data_to_use$outcome[sample(1:nrow(Data_to_use), 30)]=2
# 
# # Work on predictor with more than 2 levels
# Data_to_use=as.data.table(Data_to_use)
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
# Table_Data=Raw_Contingency_Table_Generator(Data=Data_to_use,
#                                            Row_Var="outcome",
#                                            Col_Var="age",
#                                            Value="Frequency")
# Raw_Contingency_Table_Generator(Data=Data_to_use,
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
# Stepwise_AIC
#*************
# Example
#********
# lapply(c("stats", "geepack"), checkpackages)
# require(dplyr)
# data("respiratory")
# 
# # run GLM_Multivariable
# Full_Model=GLM_Multivariable(Data<-respiratory,
#                              Pred_Vars=c("center", "treat", "sex", "age", "baseline"),
#                              Res_Var="outcome",
#                              which.family<-"binomial (link='logit')")$model_fit
# Stepwise_AIC(Full_Model)
Stepwise_AIC=function(Full_Model, ...){ # names of people should be numeric
  lapply(c("MASS", "doBy"), checkpackages)
  
  # run model
  AIC_Results=stepAIC(Full_Model, ...)
  
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est=esticon(AIC_Results, diag(length(coef(AIC_Results))))[-1, ]
  
  # Output
  Output=c()
  Output$model_fit=AIC_Results
  Output$vif=car::vif(AIC_Results)
  
  if(grepl("gaussian", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 Estimate.and.CI=paste0(format(round2(est$estimate, 2), nsmall=2), 
                                                        " (", format(round2(est$estimate-qnorm(0.975)*est$std.error, 2), nsmall=2), " - ", 
                                                        format(round2(est$estimate+qnorm(0.975)*est$std.error, 2), nsmall=2), ")"), 
                                 row.names=names(coef(AIC_Results))[-1]
    )
  }else if(grepl("binomial", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 OR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(AIC_Results))[-1]
    )
    
  }else if(grepl("poisson", which.family)){
    Output$summ_table=data.frame(Estimate=round2(est$estimate, 3), 
                                 Std.Error=round2(est$std.error, 3), 
                                 `P-value`=ifelse(round2(est$p.value, 3)<0.001, "<0.001", 
                                                  format(round2(est$p.value, 3), nsmall=3)), 
                                 RR.and.CI=paste0(format(round2(exp(est$estimate), 2), nsmall=2), 
                                                  " (", format(round2(exp(est$estimate-qnorm(0.975)*est$std.error), 2), nsmall=2), " - ", 
                                                  format(round2(exp(est$estimate+qnorm(0.975)*est$std.error), 2), nsmall=2), ")"), 
                                 row.names=names(coef(AIC_Results))[-1]
    )
  }
  
  return(Output)
}


#******************** 
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


#*********
# Joy_Plot
#*********
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# 
# Joy_Plot(Data=Data_to_use,
#          X_Var="age",
#          Y_Var="sex",
#          scale=2,
#          rel_min_height=0.0001)
Joy_Plot=function(Data, X_Var, Y_Var, ...){
  lapply(c("ggridges",
           "ggplot2",
           "viridis",
           "hrbrthemes"), 
         checkpackages)
  
  Data=as.data.frame(Data)
  Data[, X_Var]=as.numeric(Data[, X_Var])
  Data[, Y_Var]=as.factor(Data[, Y_Var])
  
  ggplot(Data, aes(x=eval(parse(text=X_Var)), y=eval(parse(text=Y_Var)), fill=..x..))+
    geom_density_ridges_gradient(...)+ 
    xlab(X_Var)+
    ylab(Y_Var)+
    scale_fill_viridis(name="X_Var", option="C")
}


#**************
# mclapply.hack
#**************
# Example
#********
# lapply(c("parallel"), checkpackages)
# wait.longer.then.square=function(xx){
#   ## Wait for ten seconds
#   Sys.sleep(10);
#   ## Square the argument
#   xx^2}
# 
# (a.global.variable=diag(3))
# 
# system.time({serial.output=lapply(1:4,
#                                  function(xx){
#                                    return(wait.longer.then.square(xx)+a.global.variable)
#                                  })})
# 
# system.time({par.output=mclapply.hack(1:4,
#                                      function(xx){
#                                        return(wait.longer.then.square(xx)+a.global.variable)
#                                      })})
mclapply.hack=function(...){
  ## Create a cluster
  ## ... How many workers do you need?
  ## ... N.B. list(...)[[1]] returns the first 
  ##          argument passed to the function. In
  ##          this case it is the list to iterate over
  size.of.list=length(list(...)[[1]])
  cl=makeCluster(min(size.of.list, detectCores()))
  
  ## Find out the names of the loaded packages 
  loaded.package.names=c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names(sessionInfo()$otherPkgs))
  
  ## N.B. tryCatch()allows us to properly shut down the 
  ##      cluster if an error in our code halts execution
  ##      of the function. For details see: help(tryCatch)
  tryCatch({
    
    ## Copy over all of the objects within scope to
    ## all clusters. 
    ## 
    ## The approach is as follows: Beginning with the 
    ## current environment, copy over all objects within
    ## the environment to all clusters, and then repeat
    ## the process with the parent environment. 
    ##
    this.env=environment()
    while(identical(this.env, globalenv())==FALSE){
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env=parent.env(environment())
    }
    ## repeat for the global environment
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl)returns the number of clusters
    parLapply(cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy){
        ## N.B. the character.only option of 
        ##      require()allows you to give the 
        ##      name of a package as a string. 
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel 
    return(parLapply(cl, ...))
  }, finally={        
    ## Stop the cluster
    stopCluster(cl)
  })
}


#**********************
# Example of doParallel
#**********************
# lapply(c("foreach", "doParallel"), checkpackages)
# numCores=detectCores()
# registerDoParallel(numCores)  # use multicore, set to the number of our cores
# 
# # 1 - use lapply
# x=iris[which(iris[,5]!="setosa"), c(1,5)]
# trials=seq(1, 10000)
# boot_fx=function(trial){
#   ind=sample(100, 100, replace=TRUE)
#   result1=glm(x[ind,2]~x[ind,1], family=binomial(logit))
#   r=coefficients(result1)
#   res=rbind(data.frame(), r)
# }
# system.time({
#   results=lapply(trials, boot_fx)
# })
# 
# # 2 - use foreach - compare that to what it takes to do the same analysis in serial
# trials=10000
# system.time({
#   r=foreach(icount(trials), .combine=rbind)%do%{
#     ind=sample(100, 100, replace=TRUE)
#     result1=glm(x[ind,2]~x[ind,1], family=binomial(logit))
#     coefficients(result1)
#   }
# })
# 
# # 3 - use a parallel bootstrap
# # From the doParallel vignette, but slightly modified
# trials=10000
# system.time({
#   r.2=foreach(icount(trials), .combine=rbind)%dopar%{
#     ind=sample(100, 100, replace=TRUE)
#     result1=glm(x[ind,2]~x[ind,1], family=binomial(logit))
#     coefficients(result1)
#   }
# })
# 
# # When you're done, clean up the cluster
# stopImplicitCluster()

