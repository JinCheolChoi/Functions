#***************************************
#
# [ --- Operational Functions --- ] ----
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

#**********
# floor_dec
#**********
floor_dec=function(x, level=1){
  round(x-5*10^(-level-1), level) 
}

#************
# ceiling_dec
#************
ceiling_dec=function(x, level=1){
  round(x+5*10^(-level-1), level)
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
  # check out packages
  lapply(c("data.table"), checkpackages)
  
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

#**********************
# SURVEY_Number_Updater
#**********************
SURVEY_Number_Updater=function(Data,
                               Survey_Var,
                               Int_Date_Var){
  # check out packages
  lapply(c("data.table"), checkpackages)
  
  # data.table
  if(!is.data.table(Data)){
    print("Format Data into data.table")
  }else{
    # new Int_Date_Var
    # 1) Int_Date_Var_NEW=Int_Date_Var
    Data[, paste0(Survey_Var, "_NEW"):=eval(parse(text=Survey_Var))]
    
    # 2) conditional assignments to Int_Date_Var_NEW
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           eval(parse(text=Int_Date_Var))<"2006-06-01",
         paste0(Survey_Var, "_NEW"):=0]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2006-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2006-12-01",
         paste0(Survey_Var, "_NEW"):=1]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2006-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2007-06-01",
         paste0(Survey_Var, "_NEW"):=2]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2007-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2007-12-01", 
         paste0(Survey_Var, "_NEW"):=3]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2007-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2008-06-01", 
         paste0(Survey_Var, "_NEW"):=4]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2008-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2008-12-01", 
         paste0(Survey_Var, "_NEW"):=5]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2008-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2009-06-01", 
         paste0(Survey_Var, "_NEW"):=6]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2009-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2009-12-01", 
         paste0(Survey_Var, "_NEW"):=7]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2009-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2010-06-01", 
         paste0(Survey_Var, "_NEW"):=8]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2010-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2010-12-01", 
         paste0(Survey_Var, "_NEW"):=9]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2010-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2011-06-01", 
         paste0(Survey_Var, "_NEW"):=10]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2011-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2011-12-01", 
         paste0(Survey_Var, "_NEW"):=11]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2011-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2012-06-01", 
         paste0(Survey_Var, "_NEW"):=12]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2012-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2012-12-01", 
         paste0(Survey_Var, "_NEW"):=13]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2012-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2013-06-01", 
         paste0(Survey_Var, "_NEW"):=14]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2013-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2013-12-01", 
         paste0(Survey_Var, "_NEW"):=15]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2013-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2014-06-01", 
         paste0(Survey_Var, "_NEW"):=16]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2014-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2014-12-01", 
         paste0(Survey_Var, "_NEW"):=17]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2014-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2015-06-01", 
         paste0(Survey_Var, "_NEW"):=18]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2015-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2015-12-01", 
         paste0(Survey_Var, "_NEW"):=19]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2015-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2016-06-01", 
         paste0(Survey_Var, "_NEW"):=20]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2016-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2016-12-01", 
         paste0(Survey_Var, "_NEW"):=21]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2016-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2017-06-01", 
         paste0(Survey_Var, "_NEW"):=22]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2017-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2017-12-01", 
         paste0(Survey_Var, "_NEW"):=23]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2017-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2018-06-01", 
         paste0(Survey_Var, "_NEW"):=24]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2018-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2018-12-01", 
         paste0(Survey_Var, "_NEW"):=25]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2018-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2019-06-01", 
         paste0(Survey_Var, "_NEW"):=26]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2019-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2019-12-01", 
         paste0(Survey_Var, "_NEW"):=27]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2019-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=28]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2020-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=29]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2020-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=30]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2021-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=31]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2021-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=32]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2022-06-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=33]
    
    Data[eval(parse(text=Survey_Var))%in%c(0, 5) & 
           "2022-12-01"<=eval(parse(text=Int_Date_Var)) & eval(parse(text=Int_Date_Var))<"2020-06-01", 
         paste0(Survey_Var, "_NEW"):=34]
  }
  
  return(as.data.table(Data))
}


#************
# Align_plots
#************
Align_plots=function(...){
  pl=list(...)
  stopifnot(do.call(all, lapply(pl, inherits, "gg")))
  gl=lapply(pl, ggplotGrob)
  bind2=function(x, y) gtable:::rbind_gtable(x, y, "first")
  combined=Reduce(bind2, gl[-1], gl[[1]])
  wl=lapply(gl, "[[", "widths")
  combined$widths=do.call(grid::unit.pmax, wl)
  grid::grid.newpage()
  grid::grid.draw(combined)
}


#*********************************
#
# [ --- Marginal Effect --- ] ----
#
#*********************************
# Marginal_Effect
#
# (based on 'margins')
#*********************
# This function is based on 'margins' (Version 0.3.26, Date 2021-01-10), which is available for the following object classes:
#   
# "betareg", see betareg
# "glm", see glm, glm.nb
# "ivreg", see ivreg
# "lm", see lm
# "loess", see loess
# "merMod", see lmer, glmer
# "nnet", see nnet
# "polr", see polr
# "svyglm", see svyglm
#
# Unfortunately, there are model classes that 'margins' does not support (ex. geeglm), for which other packages need to be used.
# One good candidate is 'marginaleffects' that works on geeglm, based on which Marginal_Effect_2 is written.
#********
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$id=as.factor(Data_to_use$id)
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Mult_model=GLMM_Multivariable(Data=rbind(Data_to_use, Data_to_use),
#                                    Pred_Vars=c("center", "sex", "age", "sex:age"),
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial (link='logit')", # gaussian, binomial, poisson
#                                    NAGQ<-1,
#                                    Compute.Power=F, # power can be computed for a non-gaussian distribution
#                                    nsim=5)
# GLMM_Marginal_Effect=Marginal_Effect(Model_Fit=GLMM_Mult_model$model_fit,
#                                      Family="gaussian",
#                                      Var_1="sex",
#                                      Var_1_Levels=c("F", "M"),
#                                      Var_2="age",
#                                      Var_2_Levels=c(20, 30, 40, 50, 60))
Marginal_Effect=function(Model_Fit,
                         Family,
                         Var_1,
                         Var_1_Levels,
                         Var_2,
                         Var_2_Levels){
  # check out packages
  lapply(c("margins", "data.table"), checkpackages)
  
  #**********************************************
  # marginal effect of Var_1 conditional on Var_2
  List_Var_2=list(Var_2=Var_2_Levels)
  names(List_Var_2)=Var_2
  assign(paste0("Marginal_", Var_1),
         margins(Model_Fit,
                 type="link",
                 at=List_Var_2))
  Marginal_Summ_Var_1=summary(get(paste0("Marginal_", Var_1)))[grepl(Var_1, summary(get(paste0("Marginal_", Var_1)))$factor), ]
  
  #**********************************************
  # marginal effect of Var_2 conditional on Var_1
  List_Var_1=list(Var_1=Var_1_Levels)
  names(List_Var_1)=Var_1
  assign(paste0("Marginal_", Var_2),
         margins(Model_Fit,
                 type="link",
                 at=List_Var_1))
  Marginal_Summ_Var_2=summary(get(paste0("Marginal_", Var_2)))[grepl(Var_2, summary(get(paste0("Marginal_", Var_2)))$factor), ]
  
  # combine the marginal effect summaries
  Out=rbind(data.table(round(Marginal_Summ_Var_1[, c("AME", "SE", "p")], 4)),
            data.table(round(Marginal_Summ_Var_2[, c("AME", "SE", "p")], 4)))
  
  if(Family=="gaussian"){
    Out$Estimate.and.CI=c(paste0(round(Marginal_Summ_Var_1[, "AME"], 3), " (", 
                                 round(Marginal_Summ_Var_1[, "lower"], 3), " - ", 
                                 round(Marginal_Summ_Var_1[, "upper"], 3), ")"),
                          paste0(round(Marginal_Summ_Var_2[, "AME"], 3), " (", 
                                 round(Marginal_Summ_Var_2[, "lower"], 3), " - ", 
                                 round(Marginal_Summ_Var_2[, "upper"], 3), ")"))
    colnames(Out)=c("Estimate", "Std.Error", "P.value", "Estimate.and.CI")
    
    Out[, Variable:=c(paste0(Marginal_Summ_Var_1$factor, ":", Var_2, "=", Marginal_Summ_Var_1[, Var_2]),
                      paste0(Marginal_Summ_Var_2$factor, ":", Var_1, "=", Marginal_Summ_Var_2[, Var_1]))]
    
    setcolorder(Out,
                c("Variable",
                  "Estimate", "Std.Error", "P.value", "Estimate.and.CI"))
  }else if(Family=="binomial"){
    Out$OR.and.CI=c(paste0(round(exp(Marginal_Summ_Var_1[, "AME"]), 3), " (", 
                           round(exp(Marginal_Summ_Var_1[, "lower"]), 3), " - ", 
                           round(exp(Marginal_Summ_Var_1[, "upper"]), 3), ")"),
                    paste0(round(exp(Marginal_Summ_Var_2[, "AME"]), 3), " (", 
                           round(exp(Marginal_Summ_Var_2[, "lower"]), 3), " - ", 
                           round(exp(Marginal_Summ_Var_2[, "upper"]), 3), ")"))
    colnames(Out)=c("Estimate", "Std.Error", "P.value", "OR.and.CI")
    
    Out[, Variable:=c(paste0(Marginal_Summ_Var_1$factor, ":", Var_2, "=", Marginal_Summ_Var_1[, Var_2]),
                      paste0(Marginal_Summ_Var_2$factor, ":", Var_1, "=", Marginal_Summ_Var_2[, Var_1]))]
    
    setcolorder(Out,
                c("Variable",
                  "Estimate", "Std.Error", "P.value", "OR.and.CI"))
  }
  
  # P.value
  Out[, P.value:=ifelse(P.value<0.001, "<0.001", P.value)]
  
  # return
  return(as.data.table(Out[, c(1, 5, 4)]))
}


#******************
# Marginal_Effect_2
#
# (based on 'marginaleffects')
#*****************************
# Example
#********
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
# Pred_Vars=c("center", "treat", "sex", "age", "baseline", "visit")
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_Vars=="treat")]="P"
# levels.of.fact[which(Pred_Vars=="sex")]="F"
# Data_to_use$id=as.factor(Data_to_use$id)
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Mult_model=GLMM_Multivariable(Data=rbind(Data_to_use, Data_to_use),
#                                    Pred_Vars=c("center", "sex", "age", "sex:age"),
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial (link='logit')", # gaussian, binomial, poisson
#                                    NAGQ<-1,
#                                    Compute.Power=F, # power can be computed for a non-gaussian distribution
#                                    nsim=5)
# GLMM_Marginal_Effect_2=Marginal_Effect_2(Model_Fit=GLMM_Mult_model$model_fit,
#                                          Family="gaussian",
#                                          Var_1="sex",
#                                          Var_1_Levels=c("F", "M"),
#                                          Var_2="age",
#                                          Var_2_Levels=c(20, 30, 40, 50, 60))
Marginal_Effect_2=function(Model_Fit,
                           Family,
                           Var_1,
                           Var_1_Levels,
                           Var_2,
                           Var_2_Levels,
                           Model="GLMM"){
  # check out packages
  lapply(c("marginaleffects", "data.table"), checkpackages)
  
  # If Model==GEE (based on geeglm), bring the saved data in the model object to Non_Missing_Data
  # (ignore) It seems that this part doesn't matter as the actual data stored in the model object is used.
  # (ignore) Still (and weirdly), the name of data used when fitted by geeglm is checked if a value named the same exists in the global environment.
  # (ignore) If there is no value matched the same in the global environment, an error is entailed.
  if(Model=="GEE"){
    Non_Missing_Data<<-Model_Fit$data
    print("Marginal_Effect_2 / Model : GEE")
  }
  
  #**********************************************
  # marginal effect of Var_1 conditional on Var_2
  List_Var_2=list(x1=Model_Fit, x2=Var_2_Levels)
  names(List_Var_2)=c("model", Var_2)
  
  Temp_Var_1=marginaleffects(Model_Fit,
                             type="link",
                             newdata=do.call(datagrid, List_Var_2))
  setnames(Temp_Var_1,
           c("term", "dydx", "std.error"),
           c("factor", "AME", "SE"))
  Temp_Var_1$z=Temp_Var_1$AME/Temp_Var_1$SE
  Temp_Var_1$p=round(2*(1-pnorm(abs(Temp_Var_1$z))), 4)
  Temp_Var_1$lower=round(Temp_Var_1$AME-qnorm(0.975)*Temp_Var_1$SE, 4)
  Temp_Var_1$upper=round(Temp_Var_1$AME+qnorm(0.975)*Temp_Var_1$SE, 4)
  
  Temp_Var_1=Temp_Var_1[, !colnames(Temp_Var_1)%in%c("rowid", "type")]
  
  Marginal_Summ_Var_1=Temp_Var_1[grepl(Var_1, Temp_Var_1$factor), ]
  
  #**********************************************
  # marginal effect of Var_2 conditional on Var_1
  List_Var_1=list(x1=Model_Fit, x2=Var_1_Levels)
  names(List_Var_1)=c("model", Var_1)
  
  Temp_Var_2=marginaleffects(Model_Fit,
                             type="link",
                             newdata=do.call(datagrid, List_Var_1))
  setnames(Temp_Var_2,
           c("term", "dydx", "std.error"),
           c("factor", "AME", "SE"))
  Temp_Var_2$z=Temp_Var_2$AME/Temp_Var_2$SE
  Temp_Var_2$p=round(2*(1-pnorm(abs(Temp_Var_2$z))), 4)
  Temp_Var_2$lower=round(Temp_Var_2$AME-qnorm(0.975)*Temp_Var_2$SE, 4)
  Temp_Var_2$upper=round(Temp_Var_2$AME+qnorm(0.975)*Temp_Var_2$SE, 4)
  
  Temp_Var_2=Temp_Var_2[, !colnames(Temp_Var_2)%in%c("rowid", "type")]
  
  Marginal_Summ_Var_2=Temp_Var_2[grepl(Var_2, Temp_Var_2$factor), ]
  
  #**************************************
  # combine the marginal effect summaries
  Out=rbind(data.table(round(Marginal_Summ_Var_1[, c("AME", "SE", "p")], 4)),
            data.table(round(Marginal_Summ_Var_2[, c("AME", "SE", "p")], 4)))
  
  if(Family=="gaussian"){
    Out$Estimate.and.CI=c(paste0(round(Marginal_Summ_Var_1[, "AME"], 3), " (", 
                                 round(Marginal_Summ_Var_1[, "lower"], 3), " - ", 
                                 round(Marginal_Summ_Var_1[, "upper"], 3), ")"),
                          paste0(round(Marginal_Summ_Var_2[, "AME"], 3), " (", 
                                 round(Marginal_Summ_Var_2[, "lower"], 3), " - ", 
                                 round(Marginal_Summ_Var_2[, "upper"], 3), ")"))
    colnames(Out)=c("Estimate", "Std.Error", "P.value", "Estimate.and.CI")
    
    Out[, Variable:=c(paste0(paste0(Marginal_Summ_Var_1$factor, Marginal_Summ_Var_1$contrast), ":", Var_2, "=", Marginal_Summ_Var_1[, Var_2]),
                      paste0(paste0(Marginal_Summ_Var_2$factor, Marginal_Summ_Var_2$contrast), ":", Var_1, "=", Marginal_Summ_Var_2[, Var_1]))]
    
    setcolorder(Out,
                c("Variable",
                  "Estimate", "Std.Error", "P.value", "Estimate.and.CI"))
  }else if(Family=="binomial"){
    Out$OR.and.CI=c(paste0(round(exp(Marginal_Summ_Var_1[, "AME"]), 3), " (", 
                           round(exp(Marginal_Summ_Var_1[, "lower"]), 3), " - ", 
                           round(exp(Marginal_Summ_Var_1[, "upper"]), 3), ")"),
                    paste0(round(exp(Marginal_Summ_Var_2[, "AME"]), 3), " (", 
                           round(exp(Marginal_Summ_Var_2[, "lower"]), 3), " - ", 
                           round(exp(Marginal_Summ_Var_2[, "upper"]), 3), ")"))
    colnames(Out)=c("Estimate", "Std.Error", "P.value", "OR.and.CI")
    
    Out[, Variable:=c(paste0(paste0(Marginal_Summ_Var_1$factor, Marginal_Summ_Var_1$contrast), ":", Var_2, "=", Marginal_Summ_Var_1[, Var_2]),
                      paste0(paste0(Marginal_Summ_Var_2$factor, Marginal_Summ_Var_2$contrast), ":", Var_1, "=", Marginal_Summ_Var_2[, Var_1]))]
    
    setcolorder(Out,
                c("Variable",
                  "Estimate", "Std.Error", "P.value", "OR.and.CI"))
  }
  
  # P.value
  Out[, P.value:=ifelse(P.value<0.001, "<0.001", P.value)]
  
  # return
  return(as.data.table(Out[, c(1, 5, 4)]))
}


#*******************************************
#
# [ --- Marginal Structural Model --- ] ----
#
#*******************************************
# Inverse Probability Weighting (IPW)
#************************************
# Example
#********
# # (data simulation code source : https://rpubs.com/mbounthavong/IPTW_MSM_Tutorial)
# #set seed to replicate results
# set.seed(12345)
# 
# #define sample size
# n=2000
# #define confounder c1 (gender, male==1)
# male=rbinom(n,1,0.55)
# #define confounder c2 (age)
# age=exp(rnorm(n, 3, 0.5))
# #define treatment at time 1
# t_1=rbinom(n,1,0.20)
# #define treatment at time 2
# t_2=rbinom(n,1,0.20)
# #define treatment at time 3
# t_3=rbinom(n,1,0.20)
# #define depression at time 1 (prevalence=number per 100000 population)
# d_1=exp(rnorm(n, 0.001, 0.5))
# #define depression at time 2 (prevalence=number per 100000 population)
# d_2=exp(rnorm(n, 0.002, 0.5))
# #define depression at time 3 (prevalence=number per 100000 population)
# d_3=exp(rnorm(n, 0.004, 0.5))
# #define time-varying confounder v1 as a function of t1 and d1
# v_1=(0.4*t_1 + 0.80*d_1 + rnorm(n, 0, sqrt(0.99))) + 5
# #define time-varying confounder v2 as a function of t1 and d1
# v_2=(0.4*t_2 + 0.80*d_2 + rnorm(n, 0, sqrt(0.55))) + 5
# #define time-varying confounder v3 as a function of t1 and d1
# v_3=(0.4*t_3 + 0.80*d_3 + rnorm(n, 0, sqrt(0.33))) + 5
# #put all in a dataframe and write data to harddrive to use later in e.g. SPSS
# df1=data.frame(male, age, v_1, v_2, v_3, t_1, t_2, t_3, d_1, d_2, d_3)
# 
# #required packages
# lapply(c("geepack",
#          "survey",
#          "ipw",
#          "reshape",
#          "dplyr"), checkpackages)
# 
# # Data is readin long format
# data1=df1
# # Convert from wide to long format
# data_long<-reshape(data1, varying=c("v_1", "v_2", "v_3", "t_1","t_2","t_3", "d_1", "d_2", "d_3"), direction="long", idvar="id", sep="_")
# # Reshape from wide to long format
# data_long_sort=arrange(data_long, id, time)
# # Rearrange the dataset so that id is first
# data_long_sort=data_long_sort[c("id", "time", "age", "male", "t", "v", "d")]
# head(data_long_sort)
# 
# # male as factor
# data_long_sort$male=as.factor(data_long_sort$male)
# w=IPW(Exposure="t",
#       Time_Invariant_Covs=c("male", "age"),
#       Time_Varying_Covs=c("v"),
#       which.family="binomial",
#       Link="logit",
#       ID_Var="id",
#       Data="data_long_sort")
# head(w$unstab_IP_weights$ipw.weights, 10)
# head(w$basic_stab_IP_weights$ipw.weights, 10)
# head(w$adjusted_stab_IP_weights$ipw.weights, 10)
IPW=function(Exposure,
             Time_Invariant_Covs,
             Time_Varying_Covs,
             which.family="binomial",
             Link="logit",
             # Timevar,
             ID_Var,
             Data){
  # check out packages
  lapply(c("data.table",
           "ipw",
           "dplyr"),
         checkpackages)
  # For binary exposure, the inverse probability of treatment weights for patients are estimated using a (pooled) logistic regression
  # defined in a similar structure as the example in the book, Leite, W. L. (2016). Practical propensity score methods using R.
  
  # remove observations with missing data
  Data_to_use_No_Missing=as.data.table(na.omit(eval(parse(text=Data))))
  if(nrow(eval(parse(text=Data)))!=nrow(Data_to_use_No_Missing)){
    print("missing data removed.")
  }
  
  # Time_Point
  Data_to_use_No_Missing[, Time_Point:=0:(.N-1), by=ID_Var]
  
  # convert Exposure to numeric
  Data_to_use_No_Missing[, (Exposure):=as.numeric(as.character(eval(parse(text=Exposure))))]
  
  # generate lagged exposure with a lag of one period
  Data_to_use_No_Missing[,
                         (paste0("Lagged_", Exposure)):=lapply(.SD, function(x) lag(x, n=1)),
                         .SDcol=Exposure,
                         by=ID_Var]
  Data_to_use_No_Missing[is.na(eval(parse(text=paste0("Lagged_", Exposure)))), (paste0("Lagged_", Exposure)):=0]
  
  # cumsum lagged exposure (total number of previous measurement waves)
  Data_to_use_No_Missing[,
                         (paste0("Cumsum_Lagged_", Exposure)):=cumsum(eval(parse(text=paste0("Lagged_", Exposure)))),
                         by=ID_Var]
  
  # # generate lagged time-varying covariates with a lag of one period
  # Data_to_use_No_Missing[,
  #                        (paste0("Lagged_", Time_Varying_Covs)):=lapply(.SD, function(x) lag(x, n=1)),
  #                        .SDcol=Time_Varying_Covs,
  #                        by=ID_Var]
  
  #*****************
  # define numerator
  # 1. unstabilized
  unstab_numerator_formula=NULL
  
  # 2. basic stabilized
  basic_stab_numerator_formula=paste0("~ 1")
  
  # 3. stabilized
  # For the censoring weights, stabilized produces the same values as basic stabilized because Cumsum_Lagged_* variable has only 0 as its value.
  # This might be something to further ponder upon later.
  stab_numerator_formula=paste0("~ 1 + ",
                                paste0("Cumsum_Lagged_", Exposure))
  
  # 4. adjusted stabilized by time-invariant covariates
  adjusted_stab_numerator_formula=paste0("~ 1 + ",
                                         paste0("Cumsum_Lagged_", Exposure),
                                         " + ",
                                         paste0(Time_Invariant_Covs, collapse=" + "))
  
  # #*****************
  # # define numerator
  # # 1. unstabilized
  # unstab_numerator_formula=NULL
  # 
  # # 2. basic stabilized
  # basic_stab_numerator_formula_t0=1
  # basic_stab_numerator_formula_t=paste0("~ ",
  #                                       "Lagged_",
  #                                       Exposure)
  # 
  # # 3. adjusted stabilized by time-invariant covariates
  # adjusted_stab_numerator_formula_t0=paste0("~ ",
  #                                           paste0(Time_Invariant_Covs, collapse=" + "))
  # adjusted_stab_numerator_formula_t=paste0("~ ",
  #                                          "Lagged_",
  #                                          Exposure,
  #                                          " + ",
  #                                          paste0(Time_Invariant_Covs, collapse=" + "))
  # 
  #*******************
  # define denominator
  denominator_formula=paste0("~ ",
                             paste0("Cumsum_Lagged_", Exposure),
                             " + ",
                             paste0(Time_Invariant_Covs, collapse=" + "),
                             " + ",
                             paste0(Time_Varying_Covs, collapse=" + "))
  
  
  # #*****************
  # # define numerator
  # # 1. unstabilized
  # unstab_numerator_formula=NULL
  # 
  # # 2. basic stabilized
  # basic_stab_numerator_formula_t0=1
  # basic_stab_numerator_formula_t=paste0("~ ",
  #                                       "Lagged_",
  #                                       Exposure)
  # 
  # # 3. adjusted stabilized by time-invariant covariates
  # adjusted_stab_numerator_formula_t0=paste0("~ ",
  #                                           paste0(Time_Invariant_Covs, collapse=" + "))
  # adjusted_stab_numerator_formula_t=paste0("~ ",
  #                                          "Lagged_",
  #                                          Exposure,
  #                                          " + ",
  #                                          paste0(Time_Invariant_Covs, collapse=" + "))
  # 
  # #*******************
  # # define denominator
  # denominator_formula_t0=paste0("~ ",
  #                               paste0(Time_Invariant_Covs, collapse=" + "),
  #                               " + ",
  #                               paste0(Time_Varying_Covs, collapse=" + "))
  # 
  # denominator_formula_t=paste0("~ ",
  #                              "Lagged_",
  #                              Exposure,
  #                              " + ",
  #                              paste0(Time_Invariant_Covs, collapse=" + "),
  #                              " + ",
  #                              paste0(Time_Varying_Covs, collapse=" + "))
  
  #*********************
  # calculate IP weights
  # 1. unstabilized
  unstab_ipwtm_function=paste0(
    'ipwtm(exposure=', Exposure,',
           family="', which.family, '",
           link="', Link, '",
           timevar=Time_Point,
           numerator=', unstab_numerator_formula,',
           denominator=', denominator_formula, ',
           id=', ID_Var, ',
           type="first",
           data=Data_to_use_No_Missing)')
  # obtain unstabilized weight
  unstab_IP_weights=eval(parse(text=unstab_ipwtm_function))
  
  # 2. basic stabilized
  basic_stab_ipwtm_function=paste0(
    'ipwtm(exposure=', Exposure,',
           family="', which.family, '",
           link="', Link, '",
           timevar=Time_Point,
           numerator=', basic_stab_numerator_formula,',
           denominator=', denominator_formula, ',
           id=', ID_Var, ',
           type="all",
           data=Data_to_use_No_Missing)')
  # obtain unstabilized weight
  basic_stab_IP_weights=eval(parse(text=basic_stab_ipwtm_function))
  
  # 3. stabilized
  stab_ipwtm_function=paste0(
    'ipwtm(exposure=', Exposure,',
           family="', which.family, '",
           link="', Link, '",
           timevar=Time_Point,
           numerator=', stab_numerator_formula,',
           denominator=', denominator_formula, ',
           id=', ID_Var, ',
           type="all",
           data=Data_to_use_No_Missing)')
  # obtain unstabilized weight
  stab_IP_weights=eval(parse(text=stab_ipwtm_function))
  
  # 4. adjusted stabilized by time-invariant covariates
  adjusted_stab_ipwtm_function=paste0(
    'ipwtm(exposure=', Exposure,',
           family="', which.family, '",
           link="', Link, '",
           timevar=Time_Point,
           numerator=', adjusted_stab_numerator_formula,',
           denominator=', denominator_formula, ',
           id=', ID_Var, ',
           type="all",
           data=Data_to_use_No_Missing)')
  # obtain unstabilized weight
  adjusted_stab_IP_weights=eval(parse(text=adjusted_stab_ipwtm_function))
  
  # export
  Out=c()
  Out$unstab_IP_weights=unstab_IP_weights
  Out$basic_stab_IP_weights=basic_stab_IP_weights
  Out$stab_IP_weights=stab_IP_weights
  Out$adjusted_stab_IP_weights=adjusted_stab_IP_weights
  
  return(Out)
}


#**********************
# IP_Weights_Calculator
#**********************
# Example
#********
# # (data simulation code source : https://rpubs.com/mbounthavong/IPTW_MSM_Tutorial)
# #set seed to replicate results
# set.seed(12345)
# 
# #define sample size
# n=2000
# #define confounder c1 (gender, male==1)
# male=rbinom(n,1,0.55)
# #define confounder c2 (age)
# age=exp(rnorm(n, 3, 0.5))
# #define treatment at time 1
# t_1=rbinom(n,1,0.20)
# #define treatment at time 2
# t_2=rbinom(n,1,0.20)
# #define treatment at time 3
# t_3=rbinom(n,1,0.20)
# #define depression at time 1 (prevalence=number per 100000 population)
# d_1=exp(rnorm(n, 0.001, 0.5))
# #define depression at time 2 (prevalence=number per 100000 population)
# d_2=exp(rnorm(n, 0.002, 0.5))
# #define depression at time 3 (prevalence=number per 100000 population)
# d_3=exp(rnorm(n, 0.004, 0.5))
# #define time-varying confounder v1 as a function of t1 and d1
# v_1=(0.4*t_1 + 0.80*d_1 + rnorm(n, 0, sqrt(0.99))) + 5
# #define time-varying confounder v2 as a function of t1 and d1
# v_2=(0.4*t_2 + 0.80*d_2 + rnorm(n, 0, sqrt(0.55))) + 5
# #define time-varying confounder v3 as a function of t1 and d1
# v_3=(0.4*t_3 + 0.80*d_3 + rnorm(n, 0, sqrt(0.33))) + 5
# #put all in a dataframe and write data to harddrive to use later in e.g. SPSS
# df1=data.frame(male, age, v_1, v_2, v_3, t_1, t_2, t_3, d_1, d_2, d_3)
# 
# #required packages
# lapply(c("geepack",
#          "survey",
#          "ipw",
#          "reshape",
#          "dplyr"), checkpackages)
# 
# # Data is readin long format
# data1=df1
# # Convert from wide to long format
# data_long<-reshape(data1, varying=c("v_1", "v_2", "v_3", "t_1","t_2","t_3", "d_1", "d_2", "d_3"), direction="long", idvar="id", sep="_")
# # Reshape from wide to long format
# data_long_sort=arrange(data_long, id, time)
# # Rearrange the dataset so that id is first
# data_long_sort=data_long_sort[c("id", "time", "age", "male", "t", "v", "d")]
# head(data_long_sort)
# 
# # male as factor
# data_long_sort$male=as.factor(data_long_sort$male)
# w=ipwtm(exposure=t,
#         family="binomial",
#         link="logit",
#         numerator=~ male + age,
#         denominator=~ v + male + age,
#         id=id,
#         timevar=time,
#         type="all",
#         data=data_long_sort)
# w2=IP_Weights_Calculator(Exposure="t",
#                          Effect_Modifiers=c("male", "age"),
#                          Covariates="v",
#                          which.family="binomial",
#                          ID_Var="id",
#                          Data=data_long_sort)
# 
# # compare calculated weights
# head(w$ipw.weights, 10)
# head(w2$Time_Varying_Treatment_IP_weights, 10)
# # looks the same but weirdly R regards them as different numbers
# head(w$ipw.weights, 10)==head(w2$Time_Varying_Treatment_IP_weights, 10)
# # iptw
# iptw=w$ipw.weights
# iptw2=w2$Time_Varying_Treatment_IP_weights
# # Add the iptw variable onto a new dataframe = data2.
# data2=cbind(data_long_sort, iptw)
# # fit a weighted gee
# geeglm(d~t + time + factor(male) + age + cluster(id),
#        id=id,
#        data=data2,
#        family=gaussian("identity"),
#        corstr="ar1",
#        weights=iptw)
# geeglm(d~t + time + factor(male) + age + cluster(id),
#        id=id,
#        data=data2,
#        family=gaussian("identity"),
#        corstr="ar1",
#        weights=iptw2)
# IP_Weights_Calculator=function(Exposure,
#                                Effect_Modifiers=NULL, # usually time-invariant
#                                Covariates, # usually time-varying
#                                Censoring_Var=NULL,
#                                which.family, # distribution of Exposure (currently only binomial with logit link)
#                                # Timevar, # this is equivalent to timevar in ipwtm, but not applicable yet
#                                ID_Var,
#                                # Type, # currently, the algorithm runs as Type="all
#                                Data,
#                                Stabilized=TRUE,
#                                Effect_Modification=TRUE){
#   #*****
#   # Note
#   #*****
#   # Effect_Modification=TRUE allows the weights to take into account modifiers when calculating the numerator
#   # Whether Effect_Modification is TRUE, specify Effect_Modifiers that are considered a part of covariates that go into the denominator
#   
#   # Exposure="PRIMARY_CARE_Bi"
#   # Effect_Modifiers=Time_Invariant_Covs
#   # Covariates=Time_Varying_Covs
#   # Censoring_Var=Censor_Var
#   # # Censoring_Var=NULL
#   # which.family="binomial"
#   # # Timevar="Time"
#   # ID_Var="CODE"
#   # # Type="all"
#   # Data=Data_to_use_ER_L6M
#   # Stabilized=TRUE
#   # Effect_Modification=TRUE
#   
#   # check out packages
#   lapply(c("data.table", "magrittr", "dplyr"), checkpackages)
#   
#   # as data frame
#   Data=as.data.frame(Data)
#   # Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
#   # Data=Data[Non_Missing_Outcome_Obs, ]
#   Origin_N_Rows=nrow(Data)
#   
#   # Convert code to numeric/factor (This is very important when running gee! Whether it is numeric or factor doesn't matter. They produce the same result!)
#   Data[, ID_Var]=as.factor(Data[, ID_Var])
#   
#   # as data table
#   Data=as.data.table(Data)
#   
#   if(Stabilized==FALSE){
#     #*************
#     #
#     # unstabilized
#     #
#     #*****************
#     # treatment weight
#     #*****************
#     # denominator
#     unstabilized_trt_dnom_fullmod=as.formula(paste0(Exposure, "~",
#                                                     paste0(Effect_Modifiers, collapse="+"),
#                                                     "+",
#                                                     paste0(Covariates, collapse="+")))
#     
#     unstabilized_trt_dnom_fit=glm(unstabilized_trt_dnom_fullmod, 
#                                   family=eval(parse(text=which.family)),
#                                   data=Data)
#     
#     unstab.pd.qsmk=predict(unstabilized_trt_dnom_fit, type = "response")
#     
#     #**********
#     # numerator
#     unstab.pn.qsmk=rep(1, nrow(Data))
#     
#     #*********************************
#     # unstabilized IP treatment weight
#     usw_trt_weights=ifelse(Data[[Exposure]]==0,
#                            unstab.pn.qsmk/(1-unstab.pd.qsmk),
#                            unstab.pn.qsmk/unstab.pd.qsmk)
#     
#     #*****************
#     # censoring weight
#     #*****************
#     if(!is.null(Censoring_Var)){
#       #************
#       # denominator
#       unstabilized_censor_dnom_fullmod=as.formula(paste0(Censoring_Var, "~",
#                                                          paste0(Exposure),
#                                                          "+",
#                                                          paste0(Effect_Modifiers, collapse="+"),
#                                                          "+",
#                                                          paste0(Covariates, collapse="+")))
#       unstabilized_censor_dnom_fit=glm(unstabilized_censor_dnom_fullmod, 
#                                        family="binomial", # censoring variable is always binary
#                                        data=Data)
#       
#       unstab.pd.cens=1-predict(unstabilized_censor_dnom_fit, type = "response")
#       
#       #**********
#       # numerator
#       unstab.pn.cens=rep(1, nrow(Data))
#       
#       #*********************************
#       # unstabilized IP censoring weight
#       # Unstabilized IP censoring weights for the censored individuals are 0.
#       # (p.159, Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC)
#       usw_censor_weights=ifelse(Data[[Censoring_Var]]==1,
#                                 0,
#                                 unstab.pn.cens/unstab.pd.cens)
#     }else{
#       usw_censor_weights=rep(1, nrow(Data))
#     }
#     
#     #*****************************
#     # final unstabilized IP weight
#     #*****************************
#     usw_IP_weights=usw_censor_weights*usw_trt_weights
#     
#     # compute Time_Varying_Treatment_Weights
#     Data[, Point_Treatment_Weights:=usw_IP_weights]
#     Data %<>% 
#       group_by(eval(parse(text=ID_Var))) %>% 
#       mutate(Time_Varying_Treatment_Weights=cumprod(Point_Treatment_Weights)) %>% 
#       ungroup() %>% 
#       as.data.table
#   }else{
#     #***********
#     #
#     # stabilized
#     #
#     #*****************
#     # treatment weight
#     #*****************
#     # denominator
#     stabilized_trt_dnom_fullmod=as.formula(paste0(Exposure, "~",
#                                                   paste0(Effect_Modifiers, collapse="+"),
#                                                   "+",
#                                                   paste0(Covariates, collapse="+")))
#     
#     stabilized_trt_dnom_fit=glm(stabilized_trt_dnom_fullmod, 
#                                 family=eval(parse(text=which.family)),
#                                 data=Data)
#     
#     stab.pd.qsmk=predict(stabilized_trt_dnom_fit, type = "response")
#     
#     #**********
#     # numerator
#     if(Effect_Modification==TRUE){
#       stabilized_trt_num_fullmod=as.formula(paste0(Exposure, "~",
#                                                    paste0(Effect_Modifiers, collapse="+")))
#     }else{
#       stabilized_trt_num_fullmod=as.formula(paste0(Exposure, "~1"))
#     }
#     stabilized_trt_num_fit=glm(stabilized_trt_num_fullmod, 
#                                family=eval(parse(text=which.family)),
#                                data=Data)
#     
#     stab.pn.qsmk=predict(stabilized_trt_num_fit, type = "response")
#     
#     #*******************************
#     # stabilized IP treatment weight
#     sw_trt_weights=ifelse(Data[[Exposure]]==0,
#                           (1-stab.pn.qsmk)/(1-stab.pd.qsmk),
#                           stab.pn.qsmk/stab.pd.qsmk)
#     
#     #*****************
#     # censoring weight
#     #*****************
#     if(!is.null(Censoring_Var)){
#       #************
#       # denominator
#       stabilized_censor_dnom_fullmod=as.formula(paste0(Censoring_Var, "~",
#                                                        paste0(Exposure),
#                                                        "+",
#                                                        paste0(Effect_Modifiers, collapse="+"),
#                                                        "+",
#                                                        paste0(Covariates, collapse="+")))
#       stabilized_censor_dnom_fit=glm(stabilized_censor_dnom_fullmod, 
#                                      family="binomial", # censoring variable is always binary
#                                      data=Data)
#       
#       stab.pd.cens=1-predict(stabilized_censor_dnom_fit, type = "response")
#       
#       #**********
#       # numerator
#       if(Effect_Modification==TRUE){
#         stabilized_censor_num_fullmod=as.formula(paste0(Censoring_Var, "~",
#                                                         paste0(Exposure),
#                                                         "+",
#                                                         paste0(Effect_Modifiers, collapse="+")))
#       }else{
#         stabilized_censor_num_fullmod=as.formula(paste0(Censoring_Var, "~",
#                                                         paste0(Exposure)))
#       }
#       
#       stabilized_censor_num_fit=glm(stabilized_censor_num_fullmod, 
#                                     family="binomial", # censoring variable is always binary
#                                     data=Data)
#       
#       stab.pn.cens=1-predict(stabilized_censor_num_fit, type = "response")
#       
#       #*******************************
#       # stabilized IP censoring weight
#       # Stabilized IP censoring weights are not 0 regardless of censoring of individuals.
#       # (p.159, Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC)
#       sw_censor_weights=stab.pn.cens/stab.pd.cens
#     }else{
#       sw_censor_weights=rep(1, nrow(Data))
#     }
#     
#     #***************************
#     # final stabilized IP weight
#     #***************************
#     sw_IP_weights=sw_censor_weights*sw_trt_weights
#     
#     # compute Time_Varying_Treatment_Weights
#     # The general form of the (un)stabilized PI weights can be found on p.263 (Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC)
#     Data[, Point_Treatment_Weights:=sw_IP_weights]
#     Data %<>% 
#       group_by(eval(parse(text=ID_Var))) %>% 
#       mutate(Time_Varying_Treatment_Weights=cumprod(Point_Treatment_Weights)) %>% 
#       ungroup() %>% 
#       as.data.table
#   }
#   
#   # export weights
#   if(Stabilized==FALSE){
#     Data[, `:=`(Index=.I,
#                 censor_weights=usw_censor_weights,
#                 trt_weights=usw_trt_weights,
#                 Point_Treatment_IP_weights=usw_IP_weights,
#                 Time_Varying_Treatment_IP_weights=Data$Time_Varying_Treatment_Weights)]
#   }else{
#     Data[, `:=`(Index=.I,
#                 censor_weights=sw_censor_weights,
#                 trt_weights=sw_trt_weights,
#                 Point_Treatment_IP_weights=sw_IP_weights,
#                 Time_Varying_Treatment_IP_weights=Time_Varying_Treatment_Weights)]
#     
#   }
#   return(Data[, .SD, .SDcols=c(ID_Var,
#                                "Index",
#                                "censor_weights",
#                                "trt_weights",
#                                "Point_Treatment_IP_weights",
#                                "Time_Varying_Treatment_IP_weights")])
# }

# # Keep this function just in case
# IPW=function(Exposure,
#              Time_Invariant_Covs,
#              Time_Varying_Covs,
#              which.family,
#              Link=NULL,
#              Tstart=NULL,
#              Timevar=NULL,
#              ID_Var,
#              Type="first",
#              Data){
#   # check out packages
#   lapply(c("data.table",
#            "ipw",
#            "dplyr"),
#          checkpackages)
#   
#   # #
#   # Exposure="PRIMARY_CARE_Bi"
#   # Time_Invariant_Covs="ETHNIC_WHITE"
#   # Time_Varying_Covs="AGE"
#   # # Censoring_Var="Censor"
#   # which.family="survival"
#   # Link=NULL
#   # # Link="logit" # for family="survival" this argument is ignored
#   # Tstart="START_TIME"
#   # Timevar="STOP_TIME"
#   # ID_Var="CODE"
#   # Type="first"
#   # Data="Data_to_use"
#   
#   # remove observations with missing data
#   Data_to_use_No_Missing=na.omit(eval(parse(text=Data)))
#   if(nrow(eval(parse(text=Data)))!=nrow(Data_to_use_No_Missing)){
#     print("missing data removed.")
#   }
#   
#   #*********************************
#   # unstabilized IP treatment weight
#   # define the function
#   Usw_num=NULL
#   Usw_den=paste0("~",
#                  paste0(Time_Invariant_Covs, collapse="+"),
#                  "+",
#                  paste0(Time_Varying_Covs, collapse="+"))
#   
#   unstab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Usw_num,',
#            denominator=', Usw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # ipwtm_function=paste0(
#   #   'ipwtm(exposure=PRIMARY_CARE_Bi,
#   #          family="survival",
#   #          link=NULL,
#   #          tstart=START_TIME,
#   #          timevar=STOP_TIME,
#   #          numerator=NULL,
#   #          denominator=~ETHNIC_WHITE,
#   #          id=CODE,
#   #          type="first",
#   #          data=Data_to_use_No_Missing)')
#   
#   # obtain unstabilized weight
#   unstab_IP_weights=eval(parse(text=unstab.ipwtm_function))
#   
#   
#   #*******************************
#   # stabilized IP treatment weight
#   # define the function
#   Sw_num=paste0("~",
#                 paste0(Time_Invariant_Covs, collapse="+"))
#   Sw_den=paste0("~",
#                 paste0(Time_Invariant_Covs, collapse="+"),
#                 "+",
#                 paste0(Time_Varying_Covs, collapse="+"))
#   
#   stab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Sw_num,',
#            denominator=', Sw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # obtain stabilized weight
#   adjusted_stab_IP_weights=eval(parse(text=stab.ipwtm_function))
#   
#   
#   #*************************************
#   # basic stabilized IP treatment weight
#   # define the function
#   Bsw_num=paste0("~ 1")
#   Bsw_den=paste0("~",
#                  paste0(Time_Invariant_Covs, collapse="+"),
#                  "+",
#                  paste0(Time_Varying_Covs, collapse="+"))
#   
#   basic.stab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Bsw_num,',
#            denominator=', Bsw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # obtain stabilized weight
#   basic_stab_IP_weights=eval(parse(text=basic.stab.ipwtm_function))
#   
#   # export
#   Out=c()
#   Out$unstab_IP_weights=unstab_IP_weights
#   Out$adjusted_stab_IP_weights=adjusted_stab_IP_weights
#   Out$basic_stab_IP_weights=basic_stab_IP_weights
#   
#   return(Out)
# }

# # Keep this function just in case
# IPW_2=function(Exposure,
#                Time_Invariant_Covs,
#                Time_Varying_Covs,
#                which.family="binomial",
#                Link="logit",
#                ID_Var,
#                Data){
#   # check out packages
#   lapply(c("data.table",
#            "ipw",
#            "dplyr"),
#          checkpackages)
#   
#   #
#   Exposure="PRIMARY_CARE_Bi"
#   Time_Invariant_Covs="ETHNIC_WHITE"
#   Time_Varying_Covs="AGE"
#   # Censoring_Var="Censor"
#   which.family="survival"
#   Link=NULL
#   # Link="logit" # for family="survival" this argument is ignored
#   Tstart="START_TIME"
#   Timevar="STOP_TIME"
#   ID_Var="CODE"
#   Type="first"
#   Data="Data_to_use"
#   
#   # remove observations with missing data
#   Data_to_use_No_Missing=na.omit(eval(parse(text=Data)))
#   if(nrow(eval(parse(text=Data)))!=nrow(Data_to_use_No_Missing)){
#     print("missing data removed.")
#   }
#   
#   #*********************************
#   # unstabilized IP treatment weight
#   # define the function
#   Usw_num=NULL
#   Usw_den=paste0("~",
#                  paste0(Time_Invariant_Covs, collapse="+"),
#                  "+",
#                  paste0(Time_Varying_Covs, collapse="+"))
#   
#   unstab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Usw_num,',
#            denominator=', Usw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # ipwtm_function=paste0(
#   #   'ipwtm(exposure=PRIMARY_CARE_Bi,
#   #          family="survival",
#   #          link=NULL,
#   #          tstart=START_TIME,
#   #          timevar=STOP_TIME,
#   #          numerator=NULL,
#   #          denominator=~ETHNIC_WHITE,
#   #          id=CODE,
#   #          type="first",
#   #          data=Data_to_use_No_Missing)')
#   
#   # obtain unstabilized weight
#   unstab_IP_weights=eval(parse(text=unstab.ipwtm_function))
#   
#   
#   #*******************************
#   # stabilized IP treatment weight
#   # define the function
#   Sw_num=paste0("~",
#                 paste0(Time_Invariant_Covs, collapse="+"))
#   Sw_den=paste0("~",
#                 paste0(Time_Invariant_Covs, collapse="+"),
#                 "+",
#                 paste0(Time_Varying_Covs, collapse="+"))
#   
#   stab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Sw_num,',
#            denominator=', Sw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # obtain stabilized weight
#   adjusted_stab_IP_weights=eval(parse(text=stab.ipwtm_function))
#   
#   
#   #*************************************
#   # basic stabilized IP treatment weight
#   # define the function
#   Bsw_num=paste0("~ 1")
#   Bsw_den=paste0("~",
#                  paste0(Time_Invariant_Covs, collapse="+"),
#                  "+",
#                  paste0(Time_Varying_Covs, collapse="+"))
#   
#   basic.stab.ipwtm_function=paste0(
#     'ipwtm(exposure=', Exposure,',
#            family="', which.family, '",
#            link="', Link, '",
#            tstart=', Tstart,',
#            timevar=', Timevar, ',
#            numerator=', Bsw_num,',
#            denominator=', Bsw_den, ',
#            id=', ID_Var, ',
#            type="', Type, '",
#            data=Data_to_use_No_Missing)')
#   
#   # obtain stabilized weight
#   basic_stab_IP_weights=eval(parse(text=basic.stab.ipwtm_function))
#   
#   # export
#   Out=c()
#   Out$unstab_IP_weights=unstab_IP_weights
#   Out$adjusted_stab_IP_weights=adjusted_stab_IP_weights
#   Out$basic_stab_IP_weights=basic_stab_IP_weights
#   
#   return(Out)
# }



#********
# GEE_MSM
#********
# estimate the causal effect of exposure on the outcome using GEE
#****************************************************************
# Example
#********
# # (data simulation code source : https://rpubs.com/mbounthavong/IPTW_MSM_Tutorial)
# #set seed to replicate results
# set.seed(12345)
# 
# #define sample size
# n=2000
# #define confounder c1 (gender, male==1)
# male=rbinom(n,1,0.55)
# #define confounder c2 (age)
# age=exp(rnorm(n, 3, 0.5))
# #define treatment at time 1
# t_1=rbinom(n,1,0.20)
# #define treatment at time 2
# t_2=rbinom(n,1,0.20)
# #define treatment at time 3
# t_3=rbinom(n,1,0.20)
# #define depression at time 1 (prevalence=number per 100000 population)
# d_1=exp(rnorm(n, 0.001, 0.5))
# #define depression at time 2 (prevalence=number per 100000 population)
# d_2=exp(rnorm(n, 0.002, 0.5))
# #define depression at time 3 (prevalence=number per 100000 population)
# d_3=exp(rnorm(n, 0.004, 0.5))
# #define time-varying confounder v1 as a function of t1 and d1
# v_1=(0.4*t_1 + 0.80*d_1 + rnorm(n, 0, sqrt(0.99))) + 5
# #define time-varying confounder v2 as a function of t1 and d1
# v_2=(0.4*t_2 + 0.80*d_2 + rnorm(n, 0, sqrt(0.55))) + 5
# #define time-varying confounder v3 as a function of t1 and d1
# v_3=(0.4*t_3 + 0.80*d_3 + rnorm(n, 0, sqrt(0.33))) + 5
# #put all in a dataframe and write data to harddrive to use later in e.g. SPSS
# df1=data.frame(male, age, v_1, v_2, v_3, t_1, t_2, t_3, d_1, d_2, d_3)
# 
# #required packages
# lapply(c("geepack",
#          "survey",
#          "ipw",
#          "reshape",
#          "dplyr"), checkpackages)
# 
# # Data is readin long format
# data1=df1
# # Convert from wide to long format
# data_long<-reshape(data1, varying=c("v_1", "v_2", "v_3", "t_1","t_2","t_3", "d_1", "d_2", "d_3"), direction="long", idvar="id", sep="_")
# # Reshape from wide to long format
# data_long_sort=arrange(data_long, id, time)
# # Rearrange the dataset so that id is first
# data_long_sort=data_long_sort[c("id", "time", "age", "male", "t", "v", "d")]
# head(data_long_sort)
# 
# # male as factor
# data_long_sort$male=as.factor(data_long_sort$male)
# w=IPW(Exposure="t",
#       Time_Invariant_Covs=c("male", "age"),
#       Time_Varying_Covs=c("v"),
#       which.family="binomial",
#       Link="logit",
#       ID_Var="id",
#       Data="data_long_sort")
# head(w$unstab_IP_weights$ipw.weights, 10)
# head(w$basic_stab_IP_weights$ipw.weights, 10)
# head(w$adjusted_stab_IP_weights$ipw.weights, 10)
# data_long_sort$weights=w$adjusted_stab_IP_weights$ipw.weights
# GEE_MSM(Outcome="d",
#         Exposure="t",
#         which.family="gaussian",
#         ID_Var="id",
#         Weight_Var="weights",
#         Data=data_long_sort)
GEE_MSM=function(Outcome,
                 Exposure,
                 which.family="binomial",
                 ID_Var,
                 Weight_Var,
                 Data){
  # check out packages
  lapply(c("data.table",
           "dplyr",
           "doBy",
           
           "geepack"),
         checkpackages)
  
  # remove observations with missing data
  Data_to_use_No_Missing=as.data.table(na.omit(Data))
  if(nrow(Data)!=nrow(Data_to_use_No_Missing)){
    print("missing data removed.")
  }
  
  # Time_Point
  Data_to_use_No_Missing[, Time_Point:=0:(.N-1), by=ID_Var]
  
  # convert Exposure to numeric
  Data_to_use_No_Missing[, (Exposure):=as.numeric(as.character(eval(parse(text=Exposure))))]
  
  # generate lagged exposure with a lag of one period
  Data_to_use_No_Missing[,
                         (paste0("Lagged_", Exposure)):=lapply(.SD, function(x) lag(x, n=1)),
                         .SDcol=Exposure,
                         by=ID_Var]
  Data_to_use_No_Missing[is.na(eval(parse(text=paste0("Lagged_", Exposure)))), (paste0("Lagged_", Exposure)):=0]
  
  # cumsum lagged exposure (total number of previous measurement waves)
  Data_to_use_No_Missing[,
                         (paste0("Cumsum_Lagged_", Exposure)):=cumsum(eval(parse(text=paste0("Lagged_", Exposure)))),
                         by=ID_Var]
  
  # as data frame
  Data_to_use_No_Missing=as.data.frame(Data_to_use_No_Missing)
  
  model_formula=as.formula(paste(Outcome, "~", paste(c(Exposure, paste0("Cumsum_Lagged_", Exposure), "Time_Point"), collapse="+")))
  model_fit=geeglm(model_formula,
                   data=Data_to_use_No_Missing,
                   weights=Data_to_use_No_Missing[, Weight_Var],
                   id=Data_to_use_No_Missing[, ID_Var],
                   family=which.family,
                   waves=Data_to_use_No_Missing[, "Time_Point"],
                   corstr="ar1")
  
  # Individual Wald test and confidence interval for each parameter
  est=esticon(model_fit, diag(length(coef(model_fit))))[-1, ]
  
  # Output
  Output=c()
  Origin_N_Rows=nrow(Data)
  Used_N_Rows=nrow(Data_to_use_No_Missing)
  Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  Output$model_fit=model_fit
  Output$model_formula=model_formula
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



#*************************
# SMD difference Plot ----
#*************************
# visualization of standardized mean difference comparison
# Example
#********
# lapply(c("ggplot2"), checkpackages)
# SMD_Data=data.frame(
#   variable=c("AGE",
#              "GENDER_SELF",
#              "ETHNIC_WHITE",
#              "MOBILITY",
#              "SELFCARE",
#              "PAIN",
#              "ANXIETY"),
#   Unweighted=abs(rnorm(7))+0.2,
#   Weighted=abs(rnorm(7))
# )
# ## Create long-format data for ggplot2
# SMD_DataMelt=melt(data=SMD_Data,
#                   id.vars=c("variable"),
#                   variable.name="Method",
#                   value.name="SMD")
# colnames(SMD_DataMelt)=c("variable", "Method", "SMD")
# 
# ## Order variable names by magnitude of SMD
# varNames=as.character(SMD_Data$variable)[order(SMD_Data$Unweighted)]
# 
# ## Order factor levels in the same order
# SMD_DataMelt$variable=factor(SMD_DataMelt$variable,
#                              levels=varNames)
# 
# ## Plot using ggplot2
# ggplot(data=SMD_DataMelt, mapping=aes(x=variable, y=SMD,
#                                       group=Method, color=Method)) +
#   ggtitle("Title")+
#   # geom_line() +
#   scale_colour_manual("", values=c("chocolate3","cyan4")) +
#   # scale_fill_manual(values=c("chocolate3", "cyan4")) +
#   geom_point(shape=21, size=5, stroke=2,) +
#   geom_hline(yintercept=0.5, color=4, size=1.2, linetype=2) +
#   geom_vline(xintercept=c(1:19), color=8, linetype=5)+
#   coord_flip() +
#   theme_bw() +
#   theme(legend.key=element_blank(), text=element_text(size=30), legend.position=c(0.8, 0.2)) +
#   labs(y="Standardized mean difference", x="")



#*************************************************
#
# [ --- ITS ( Interrupted Time Series ) --- ] ----
#
#*************************************************
# Segmented_Regression_Model
#***************************
# Example
#********
# lapply(c("ggplot2",
#          "data.table"), checkpackages)
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
# # time
# df[, time:=as.Date(time, origin="2020-01-01")]
# ITS=Segmented_Regression_Model(Data=df,
#                                Res_Var="count",
#                                Time_Var="time",
#                                Int_Var="intv",
#                                ylim=c(min(df[["count"]], na.rm=T)-5,
#                                       max(df[["count"]], na.rm=T)+5),
#                                # main=Sub,
#                                ylab=paste0("Count"),
#                                xlab="Time",
#                                AR_Order=1, # p
#                                MA_Order=0)
# ITS$Fitted_Regression_Line_Plot() # !!! the post-intervention regression line is not accurate for seasonal-adjusted models !!!
Segmented_Regression_Model=function(Data,
                                    Res_Var,
                                    Time_Var,
                                    Int_Var,
                                    Period=12,
                                    AR_Order=1, # p
                                    MA_Order=1, # q
                                    ...){
  # check out packages
  lapply(c("data.table",
           "car", # for durbinWatsonTest
           "seastests", # for combined_test
           
           "nlme",
           "forecast",
           "zoo"), # to test autocorrelation
         checkpackages)
  
  # # as data frame
  # Data=as.data.frame(Data)
  Data=as.data.table(Data)
  Data[[Res_Var]]=as.numeric(as.character(Data[[Res_Var]]))
  
  #***************************************************************************************************
  # overall seasonality check (Webel-Ollech's test that combines the QS-test and the Kruskall-Wallis test)
  # 
  # Intervention=ts(Data[[Int_Var]],
  #                 start=c(year(min(Data[[Time_Var]])),
  #                         1), frequency=12)
  y=ts(Data[[Res_Var]],
       start=c(year(min(Data[[Time_Var]])),
               month(min(Data[[Time_Var]]))),
       frequency=Period)
  
  WO_Test=c()
  WO_Test$Result=combined_test(y)
  
  # message generated based on https://search.r-project.org/CRAN/refmans/seastests/html/combined_test.html
  # look at p-values of 'QS-R p-value' and 'KW-R p-value'
  if(WO_Test$Result$Pval[2]<0.01 | WO_Test$Result$Pval[3]<0.002){
    WO_Test$Message="The WO - test identifies seasonality"
    print(WO_Test$Message)
  }else{
    WO_Test$Message="The WO - test does not identify seasonality"
  }
  
  #*****************
  # define variables
  # Month
  Data$Month=month(Data[[Time_Var]])
  
  # Time
  Data[[Time_Var]]=as.factor(Data[[Time_Var]])
  Data$Time=as.numeric(Data[[Time_Var]])
  
  # Level
  Data$Level=as.numeric(Data[[Int_Var]])
  
  # Trend
  Data=as.data.table(Data)
  Data[, Trend:=0]
  Data[eval(parse(text=Int_Var))==1, Trend:=1:.N]
  
  #*********************************************
  # autocorrelation check (Breusch-Godfrey test)
  # examined if autocorrelation resides in the outcome by conducting the Breusch-Godfrey test up to 12-time points
  # The null hypothesis is that there is no serial correlation of any order up to 12
  # fit model (lm)
  if(WO_Test$Message=="The WO - test identifies seasonality"){
    # If there exists significant evidence of existence of seasonality from the Webel-Ollech test,
    # the seasonally adjusted model is fitted with Fourier terms (pairs of sine and cosine functions) with 12 months as the underlying period reflecting the full seasonal cycle
    # (reference : Interrupted time series regression for the evaluation of public health interventions - A tutorial)
    model_formula=as.formula(paste(Res_Var, "~ Time + Level + Trend + harmonic(Month, 2, ", Period, ")"))
  }else{
    model_formula=as.formula(paste(Res_Var, "~ Time + Level + Trend"))
  }
  
  # handle missing data
  Non_Missing_Outcome_Obs=which(!is.na(Data[[Res_Var]]))
  Data_to_fit=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data_to_fit)
  
  # fit lm
  lm_model_fit=lm(model_formula,
                  data=Data_to_fit)
  
  # Breusch-Godfrey test
  BG_Test=c()
  BG_Test$Result=checkresiduals(lm_model_fit,
                                lag=Period,
                                plot=FALSE)
  
  # Durbin-Watson test up to 12 time periods (for monthly data)
  # this test is additional
  DW_Test=car::durbinWatsonTest(lm_model_fit,
                                max.lag=Period,
                                alternative="two.sided")
  
  # acf
  Acf=acf(Data[, .SD, .SDcols=Res_Var],
          plot=FALSE,
          na.action=na.pass)
  
  # pacf
  Pacf=pacf(Data[, .SD, .SDcols=Res_Var],
            plot=FALSE,
            na.action=na.pass)
  
  if(BG_Test$Result$p.value<0.05){
    BG_Test$Message=list(paste0("Outcome : ", Res_Var),
                         "Breusch-Godfrey test identifies autocorrelation",
                         "To adjust for autocorrelation in the model, autoregressive and moving average processes need to be taken into account.",
                         "This can be done by fitting a model using generalized least squares (gls) with an autocorrelation-moving average correlation structure of order (p, q).",
                         "Look at the acf and pacf function plots to determine the autoregressive (p) and moving average orders (q) in accordance with the instruction in the table shown at 7:10 in the following link.",
                         "https://learning.edx.org/course/course-v1:UBCx+ITSx+1T2017/block-v1:UBCx+ITSx+1T2017+type@sequential+block@72dd230d284343fba05ea08e1c26ac01/block-v1:UBCx+ITSx+1T2017+type@vertical+block@afae5c71391440c0ad3f8221bd1f4238",
                         "(if the link is not available, watch 1.7 Autocorrelation - Autocorrelation.mp4)")
    print(BG_Test$Message)
    
    if(AR_Order==0 & MA_Order==0){
      Corr_Structure=NULL
    }else{
      Corr_Structure=corARMA(p=AR_Order, # default for p and q are 1
                             q=MA_Order, # determine p and q values in accordance with the instruction in the table shown at 7:10 at https://learning.edx.org/course/course-v1:UBCx+ITSx+1T2017/block-v1:UBCx+ITSx+1T2017+type@sequential+block@72dd230d284343fba05ea08e1c26ac01/block-v1:UBCx+ITSx+1T2017+type@vertical+block@afae5c71391440c0ad3f8221bd1f4238
                             form=~Time)
    }
  }else{
    BG_Test$Message="Breusch-Godfrey test does not identify autocorrelation"
    Corr_Structure=NULL
  }
  
  #**************************
  # fit the final model (gls)
  gls_model_fit=gls(model_formula,
                    data=Data_to_fit,
                    correlation=Corr_Structure)
  
  # number of observations from a model fit
  Used_N_Rows=nobs(gls_model_fit)
  
  # Output
  Output=c()
  Output$N_Time_Points=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)")
  Output$gls_model_fit=gls_model_fit
  Output$WO_Test=WO_Test
  Output$BG_Test=BG_Test
  Output$DW_Test=DW_Test
  Output$Acf=function(){plot(Acf, main=Res_Var)}
  Output$Pacf=function(){plot(Pacf, main=Res_Var)}
  Output$summ_table=as.data.frame(summary(gls_model_fit)$tTable)
  colnames(Output$summ_table)=c("Estimate", "Std.Error", "T-value", "P-value")
  
  # Coefficients in summ_table represent the followings.
  # Time : Pre-intervention slope by time
  # Level : Immediate level change after intervention
  # Trend : Trend (Slope) change after intervention
  Output$summ_table$Estimate=round(Output$summ_table$Estimate, 3)
  Output$summ_table$Std.Error=round(Output$summ_table$Std.Error, 3)
  Output$summ_table$`T-value`=round(Output$summ_table$`T-value`, 3)
  Output$summ_table$`P-value`=ifelse(Output$summ_table$`P-value`<0.001, "<0.001", round(Output$summ_table$`P-value`, 3))
  Output$Interpretation=paste0("The outcome changes by ", Output$summ_table["Time", "Estimate"], " on average by one unit increase of time in the pre-intervention period. ",
                               "This time effect changes to ", Output$summ_table["Time", "Estimate"]+Output$summ_table["Trend", "Estimate"],
                               "(=", Output$summ_table["Time", "Estimate"], "+", Output$summ_table["Trend", "Estimate"], ") in the post-intervention period. ",
                               "After the intervention, the outcome immediately changes by ",
                               Output$summ_table["Level", "Estimate"],
                               " on average.")
  
  # plot
  Output$Fitted_Regression_Line_Plot=function(){
    Data=as.data.frame(Data)
    plot(Data$Time,
         Data[[Res_Var]],
         # ylim=c(0, 100),
         ...,
         # main=Sub,
         # xlab="Time",
         # ylab=Res_Var,
         xaxt="n",
         pch=19,
         cex=1,
         cex.lab=1.5,
         cex.axis=1.5)
    axis(1,
         at=1:length(Data[[Time_Var]]),
         labels=format(as.Date(Data[[Time_Var]]), "%Y-%m"),
         cex.axis=1)
    
    # Pre_Period_End=length(Data[[Time_Var]][Data[[Int_Var]]==0])
    Post_Period_Start=which.min(Data[[Int_Var]]==0) # the first time value of the post-intervention period
    
    abline(
      v=Post_Period_Start,
      lty=2,
      col=1)
    
    # indices of times with non-missing outcome that are used to fit the gls model
    Non_missing_Time=match(Data_to_fit$Time,
                           Data$Time)
    
    # fitted value
    Fitted_Values=fitted(gls_model_fit)
    names(Fitted_Values)=Non_missing_Time
    
    # Pre_Period_End_No_Missing : the last time index of the pre-intervention period
    Pre_Period_End_No_Missing=max(Non_missing_Time[Non_missing_Time<Post_Period_Start])
    
    # pre-intervention regression line
    lines(Non_missing_Time[Non_missing_Time<=Pre_Period_End_No_Missing],
          Fitted_Values[Non_missing_Time<=Pre_Period_End_No_Missing],
          col="red", lwd=2)
    
    # post-intervention regression line
    lines(Non_missing_Time[Non_missing_Time>Pre_Period_End_No_Missing],
          Fitted_Values[Non_missing_Time>Pre_Period_End_No_Missing],
          col="blue", lwd=2)
    
    # extrapolated regression line extended from the pre-intervention regression line
    # !!! this one only works for non-seasonally-adjusted model !!!
    segments(1,
             gls_model_fit$coefficients[1]+gls_model_fit$coefficients[2],
             nrow(Data),
             gls_model_fit$coefficients[1]+gls_model_fit$coefficients[2]*nrow(Data),
             lty=2,
             lwd=2,
             col="red")
    
    # Post_Data_prediction=Data_to_fit[Time%in%c(Non_missing_Time[Non_missing_Time>Pre_Period_End_No_Missing]), ]
    # Post_Data_prediction[, Level:=0]
    # lines(Non_missing_Time[Non_missing_Time>Pre_Period_End_No_Missing],
    #       predict(gls_model_fit, newdata=Post_Data_prediction),
    #       col="blue", lwd=2)
    
    # legend("topleft",
    #        legend=c("Pre-change point estimated mean",
    #                 "Post-change point estimated mean",
    #                 "Projection based on pre-change point phase",
    #                 "Intervention time"),
    #        col=c("red",
    #              "blue",
    #              "red",
    #              "blaCk"),
    #        lty=c(1, 1, 2, 2),
    #        cex=1,
    #        text.font=1)
  }
  return(Output)
}


#************************************************
# Segmented_Regression_Model (old one / obsolete)
#************************************************
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
# Segmented_Regression_Model=function(Data, Res_Var, Time_Var, Int_Var){
#   # as data frame
#   Data=as.data.frame(Data)
#   Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
#   Data=Data[Non_Missing_Outcome_Obs, ]
#   Origin_N_Rows=nrow(Data)
# 
#   Data[, Res_Var]=as.numeric(as.character(Data[, Res_Var]))
# 
#   Data[, Time_Var]=as.factor(Data[, Time_Var])
#   Data$Time_Order=as.numeric(Data[, Time_Var])
# 
#   # fit model
#   model_fit=lm(as.formula(paste(Res_Var, "~", Int_Var, "*Time_Order")), data=Data)
# 
#   # number of observations from a model fit
#   Used_N_Rows=nobs(model_fit)
# 
#   # Output
#   Output=c()
#   Output$N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)")
#   Output$model_fit=model_fit
#   Output$summ_table=as.data.frame(summary(model_fit)$coefficients)
#   colnames(Output$summ_table)=c("Estimate", "Std.Error", "T-value", "P-value")
#   Output$summ_table$Estimate=round(Output$summ_table$Estimate, 3)
#   Output$summ_table$Std.Error=round(Output$summ_table$Std.Error, 3)
#   Output$summ_table$`T-value`=round(Output$summ_table$`T-value`, 3)
#   Output$summ_table$`P-value`=ifelse(Output$summ_table$`P-value`<0.001, "<0.001", round(Output$summ_table$`P-value`, 3))
# 
#   return(Output)
# }
# 
# #********************************
# # Segmented_Regression_Model_Plot
# #********************************
# Segmented_Regression_Model_Plot=function(Data,
#                                          X_Var,
#                                          Y_Var,
#                                          Group_Var,
#                                          X_Lab="Time",
#                                          Y_Lab="Frequency"){
#   
#   # plot
#   Trend_Plot=ggplot(Data, aes(x=eval(parse(text=X_Var)), y=eval(parse(text=Y_Var)))) +
#     geom_line()+
#     geom_point()+
#     geom_smooth(method="lm", se=T, aes(colour=eval(parse(text=Group_Var)))) +
#     theme_bw()+
#     xlab(X_Lab)+
#     ylab(Y_Lab)+
#     labs(colour="", size=16)+
#     theme(axis.title.x=element_text(size=rel(1.8)),
#           axis.title.y=element_text(size=rel(1.8)),
#           axis.text.x=element_text(size=rel(1.8)),
#           legend.text=element_text(size=rel(1.5)))
#   
#   Trend_Plot
# }

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
  
  # print a note
  print("The PH assumption is better to be considered at the multivariable analysis level.")
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
  Analyzed_Data<<-Data # for COX_Backward_by_AIC
  
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
  
  model_fit=coxph(fullmod, na.action=na.exclude, data=Analyzed_Data)
  
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
  Assumption_Table_Temp=Output$cox.zph$table
  Assumption_Table_Temp[, "p"]=ifelse(Assumption_Table_Temp[, "p"]<0.001, "<0.001", 
                                      Assumption_Table_Temp[, "p"])
  Output$cox.zph$table=rbind(cbind(rep(Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "chisq"],
                                       Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "df"]),
                                   rep(Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "df"],
                                       Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "df"]),
                                   rep(Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "p"],
                                       Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "df"]))
                             # GLOBAL=Assumption_Table_Temp[nrow(Assumption_Table_Temp), ]
  )
  colnames(Output$cox.zph$table)=c("chisq", "df", "p")
  Output$cox.zph$Vars_with_PH_assumption_violated=names(Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "p"][Assumption_Table_Temp[-nrow(Assumption_Table_Temp), "p"]<0.05])
  
  # Scaled schoenfeld residual plot
  # cutoff time points : look at the residual plot and visually check where the lines change angle
  # source(https://stats.stackexchange.com/questions/144923/extended-cox-model-and-cox-zph/238964#238964)
  Vars=Output$cox.zph$Vars_with_PH_assumption_violated
  Schoenfeld_Plot=function(Var){
    plot(Output$cox.zph[which(names(Assumption_Table_Temp[, "p"])==Var)],
         main="Scaled schoenfeld residual plot",
         lwd=2,
         cex.lab=1.5,
         cex.axis=2,
         cex.main=2,
         cex.sub=2)
    abline(0, 0, col=1, lty=3, lwd=2)
    abline(h=Output$model_fit$coef[which(names(Assumption_Table_Temp[, "p"])==Var)], col=3, lwd=2, lty=2)
    legend("bottomright",
           legend=c('Reference line for null effect',
                    "Average hazard over time",
                    "Time-varying hazard"),
           lty=c(3, 2, 1), col=c(1, 3, 1), lwd=2,
           cex=1.5)
  }
  Output$cox.zph$plots=function(Var){Schoenfeld_Plot(Var)}
  
  # Individual Wald test and confidence interval for each parameter
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
    # The proportional hazards assumption is no longer satisfied when using the extended Cox model. (That is, the assumption check is not necessary!)
    # If time-dependent variables are considered, the Cox model form may still be used, but such a model no longer satisfies the PH assumption, and is called the extended Cox model.
    # (p.109, Survival Analysis: A Self-Learning Text, Third Edition (Statistics for Biology and Health))
    print("Note : PH Assumption is not necessary for the extended Cox model that includes time-variant variables (so, the data is formatted in the counting process layout).")
  }
  return(Output)
}


#*************************
# COX_Confounder_Selection
#*************************
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
  # check out packages
  lapply(c("ggplot2", "survminer"), checkpackages)
  
  #**************
  # temp function
  # .set_font
  .set_font=function(font){
    font=ggpubr:::.parse_font(font)
    ggtext::element_markdown(size=font$size, face=font$face, colour=font$color)
  }
  
  # Survival_Table
  Survival_Table=function(x, y, z){
    temp_table=do.call(x,
                       list(fit=survfit_output,
                            data=Data,
                            xlab=y,
                            ylab=z,
                            risk.table.title="",
                            cumevents.title="",
                            cumcensor.title="",
                            fontsize=8,
                            xlim=c(0, 180),
                            break.time.by=28,
                            legend.labs=c("Methadone", "Buprenorphine"),
                            font.tickslab=20))
    temp_table=temp_table+theme(plot.subtitle=.set_font(25),
                                plot.caption=.set_font(25),
                                axis.title.x=.set_font(30),
                                axis.title.y=.set_font(20),
                                axis.text.x=.set_font(25),
                                axis.text.y=.set_font(15),
                                panel.border=element_rect(color="black",
                                                          fill=NA,
                                                          size=1))
    return(temp_table)
  }
  
  #
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
  ggsurv_plot=ggsurvplot(
    fit=survfit_output
    # risk.table=TRUE,
    # tables.height=0.2,
    , fontsize=10
    , break.time.by=28
    , xlim=c(0, 180)
    # , ggtheme=theme_bw(base_size=15)
    # , pval= Log_rank_test
    , ...
  )
  
  # ggsurv_plot
  ggsurv_plot$plot=ggsurv_plot$plot+
    annotate("text", x=0, y=0.2,
             label=Log_rank_test,
             cex=10, col="black",
             vjust=0, hjust=0,
             fontface=4)+
    guides(color=guide_legend(override.aes=list(size=1),
                              keywidth=3,
                              keyheight=3))+
    theme(panel.border=element_rect(color="black",
                                    fill=NA,
                                    size=1))
  # tables
  Funcs=c("ggrisktable",
          "ggcumevents",
          "ggcumcensor")
  Xlabs=c("",
          "",
          "Days")
  Ylabs=c("N at risk",
          "Cum. events",
          "Cum. censorings")
  Tables=mapply(Survival_Table,
                Funcs,
                Xlabs,
                Ylabs,
                SIMPLIFY=FALSE)
  
  names(Tables)=Funcs
  # Tables$nrow=length(Funcs)
  
  # output
  output=c()
  output$survfit_output=survfit_output
  output$ggsurv_plot=ggsurv_plot
  output$pairwise_test=pairwise_test
  output$summ_tables=Tables
  
  # return output
  return(output)
}


#********************
#
# COX_Backward_by_AIC
#
#*******************************
# AIC-based backward elimination
#*******************************
# # Example
# # Create a simple data set for a time-dependent model
# Data_to_use=list(id=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#                  start=c(1,2,5,2,1,7,3,4,8,8,3,3,2,1,5,2,1,6,2,3),
#                  stop=c(2,3,6,7,8,9,9,9,14,17,13,14,10,7,6,5,4,10,4,5),
#                  event=c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0),
#                  x1=c(1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1),
#                  x2=c(0,1,1,1,1,0,NA,0,1,0,0,1,1,0,1,1,1,1,0,1),
#                  x3=c(0,1,2,2,2,0,1,0,NA,0,2,2,0,1,0,0,1,1,0,2))
# Data_to_use$x3=as.factor(Data_to_use$x3)
# COX.fit=COX_Multivariable(Data=Data_to_use,
#                           # Pred_Vars=c("x1", "x2", "x3", "x2:x3"), #!!!! for now, algorithm works the best with no interaction term (PH_assumption_P.value needs to be further touched for merging in output)
#                           Pred_Vars=c("x1", "x2"),
#                           Res_Var="event",
#                           Group_Vars="id",
#                           Strat_Vars="x3",
#                           Start_Time="start",
#                           Stop_Time="stop")
# COX_Backward_by_AIC(Full_Model=COX.fit$model_fit,
#                     Pred_Vars=c("x1", "x2"))
COX_Backward_by_AIC=function(Full_Model,
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
    if(length(Include_Index)==0){
      print("All variables are removed.")
    }else{
      for(i in 1:length(Pred_Vars[Include_Index])){
        #i=1
        Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[Include_Index][i], collapse="-"))))
        Reduced_Model_AICs[i]=AIC(Current_Reduced_Model)
        print(paste0("Step : ", step, " - Vars : ", i, "/", length(Pred_Vars[Include_Index])))
      }
    }
    
    # AIC of multivariable model excluding each variable
    Temp_Table=data.table(
      Exclusion="-",
      Var=c("(none)", Pred_Vars[Include_Index]),
      AIC=c(Current_Full_Model_AIC, Reduced_Model_AICs))
    
    # order by AIC
    Temp_Table=Temp_Table[order(AIC), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(Temp_Table$Var[1]=="(none)"){
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
#               Pred_Vars=c(Pred_Vars[!Pred_Vars%in%c("sex", "age")],
#                           list(c("sex", "age", "sex:age"))),
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
    # Individual Wald test and confidence interval for each parameter
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
    # Individual Wald test and confidence interval for each parameter
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
    #   # Individual Wald test and confidence interval for each parameter
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
    # Individual Wald test and confidence interval for each parameter
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
                            `GVIF^(1/(2*Df))`=rep(Output_vif[, 3], Output_vif[, 2]),
                            `GVIF^(1/(2*Df))_Threshold`=sqrt(10), # threshold is sqrt(10) for now
                            # https://rdrr.io/cran/pedometrics/src/R/stepVIF.R
                            # https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif/96584#96584
                            N_data_used=N_data_used)
    
  }else{
    Output$summ_table=cbind(Output$summ_table[, c(1, 5, 4)],
                            VIF=Output_vif,
                            N_data_used=N_data_used)
  }
  
  # likelihood ratio test
  LRT_results=anova(model_fit, test="LRT") # synonymous with test="Chisq" (https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.glm.html)
  LRT_pvalues=LRT_results$`Pr(>Chi)`
  LRT_pvalues=ifelse(LRT_pvalues<0.001, "<0.001", 
                     format(round2(LRT_pvalues, 7), nsmall=3))
  Output$summ_table$`P.value(LRT)`=rep(LRT_pvalues[-1], LRT_results$Df[-1])
  Output$summ_table$`P.value(LRT)`[duplicated(rep(rownames(LRT_results)[-1], LRT_results$Df[-1]))]=""
  
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
  
  # Individual Wald test and confidence interval for each parameter
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
#**************
# Example
#************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_to_use=respiratory
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
# GEE_Bivariate(Data=Data_to_use,
#               Pred_Vars=c(Pred_Vars[!Pred_Vars%in%c("sex", "age")],
#                           list(c("sex", "age", "sex:age"))),
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
#*************************************************************************
# Note : 1. Argument must be declared with '<-' in a function for HH::vif!
#*************************************************************************
# lapply(c("geepack"), checkpackages)
# data("respiratory")
# Data_original=respiratory
# # generate missing data
# Data_original$outcome[1:5]=NA
# Data_original$treat[c(3, 7, 35, 74)]=NA
# Data_original$id[c(6, 25, 45, 98)]=NA
# Data_original$sex=as.character(Data_original$sex)
# Data_original$sex[c(2, 15, 35, 58, 93)]="N"
# Data_original$sex=factor(Data_original$sex)
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
# GEE_Multivariable(Data<-Data_original,
#                   Pred_Vars<-c("center", "sex", "age", "sex:age"),
#                   Res_Var<-Res_Var,
#                   Group_Var<-Group_Var,
#                   which.family<-"binomial (link='logit')")
# Data_original=as.data.table(Data_original)
GEE_Multivariable=function(Data,
                           Pred_Vars,
                           Res_Var,
                           Group_Var,
                           which.family){ # names of people should be numeric
  # check out packages
  lapply(c("geepack", "MESS", "doBy", "HH", "data.table"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  
  # as data frame
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # Non_Missing_Data for GEE_Backward_by_QIC
  if(sum(grepl(":", Pred_Vars))>0){
    Non_Missing_Data=Remove_missing(Data, # remove missing data
                                    c(Pred_Vars[!grepl(":", Pred_Vars)],
                                      Res_Var,
                                      Group_Var))
  }else{
    Non_Missing_Data=Remove_missing(Data, # remove missing data
                                    c(Pred_Vars,
                                      Res_Var,
                                      Group_Var))
  }
  
  # Convert code to numeric/factor (This is very important when running gee! Whether it is numeric or factor doesn't matter. They produce the same result!)
  # Non_Missing_Data[, Group_Var]=as.numeric(as.factor(Non_Missing_Data[, Group_Var]))
  Non_Missing_Data[, Group_Var]=as.factor(Non_Missing_Data[, Group_Var])
  Non_Missing_Data<<-Non_Missing_Data
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+")))
  model_fit=geeglm(as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"))), 
                   data=Non_Missing_Data, 
                   id=Non_Missing_Data[, Group_Var], 
                   family=eval(parse(text=which.family)), 
                   corstr="exchangeable")
  
  # number of observations from a model fit
  Used_N_Rows=nobs(model_fit)
  
  # Individual Wald test and confidence interval for each parameter
  est=esticon(model_fit, diag(length(coef(model_fit))))[-1, ]
  
  # Output
  Output=c()
  N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  
  model_fit$call=str2lang(paste0("geeglm(formula=", paste(Res_Var, "~", paste(Pred_Vars, collapse="+")),
                                 ", family=", which.family,
                                 ", data=Non_Missing_Data",
                                 ", id=", Group_Var,
                                 ", corstr='exchangeable')",
                                 collapse=""))
  
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
                            `GVIF^(1/(2*Df))`=rep(Output_vif[, 3], Output_vif[, 2]),
                            `GVIF^(1/(2*Df))_Threshold`=sqrt(10), # threshold is sqrt(10) for now
                            # https://rdrr.io/cran/pedometrics/src/R/stepVIF.R
                            # https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif/96584#96584
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
#****************************************************
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
# GEE.fit=GEE_Multivariable(Data<-Data_to_use,
#                           Pred_Vars<-Pred_Vars,
#                           Res_Var<-Res_Var,
#                           Group_Var<-Group_Var,
#                           which.family<-"binomial (link='logit')")
# Confounder_Steps=GEE_Confounder_Selection(Full_Model=GEE.fit$model_fit,
#                                           Main_Pred_Var="sex",
#                                           Potential_Con_Vars=Pred_Vars[Pred_Vars!="sex"], # for now, algorithm works with no interaction term
#                                           which.family="binomial (link='logit')", # distribution of the response variable
#                                           Min.Change.Percentage=5,
#                                           Estimate="raw_estimate") # raw_estimate, converted_estimate
# Confounder_Ind=which(Pred_Vars%in%Confounder_Steps$Confounders)
# GEE.confound.fit=GEE_Multivariable(Data<-Data_to_use,
#                                    Pred_Vars<-Pred_Vars[Confounder_Ind],
#                                    Res_Var<-Res_Var,
#                                    Group_Var<-Group_Var,
#                                    which.family<-"binomial (link='logit')")
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
  
  Output$Full_Multivariable_Model=GEE_Multivariable(Data<<-Data,
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
  Output$Confounder_Model=GEE_Multivariable(Data<<-Data,
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
# GEE.fit=GEE_Multivariable(Data=Data_to_use,
#                                    Pred_Vars=Pred_Vars,
#                                    Res_Var="outcome",
#                                    Group_Var="id",
#                                    which.family<-"binomial")
# QIC_Selection_Steps=GEE_Backward_by_QIC(Full_Model=GEE.fit$model_fit,
#                                         Pred_Vars=Pred_Vars)
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
    if(length(Include_Index)==0){
      print("All variables are removed.")
    }else{
      for(i in 1:length(Pred_Vars[Include_Index])){
        #i=1
        Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[Include_Index][i], collapse="-"))))
        Reduced_Model_QICs[i]=QIC(Current_Reduced_Model)["QIC"]
        print(paste0("Step : ", step, " - Vars : ", i, "/", length(Pred_Vars[Include_Index])))
      }
    }
    
    # QIC of multivariable model excluding each variable
    Temp_Table=data.table(
      Exclusion="-",
      Var=c("(none)", Pred_Vars[Include_Index]),
      QIC=c(Current_Full_Model_QIC, Reduced_Model_QICs))
    
    # order by QIC
    Temp_Table=Temp_Table[order(QIC), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(Temp_Table$Var[1]=="(none)"){
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
# GEE.fit=GEE_Multivariable(Data=Data_to_use,
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

#********************
# GEE_Backward_by_P_2
#********************
# GEE_Backward_by_P_2 is basically the same as GEE_Backward_by_P
GEE_Backward_by_P_2=function(Full_Model,
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
  Current_Full_Model=Full_Model
  
  #***************
  # main algorithm
  #***************
  Reduced_Model_QICs=c()
  
  Pred_Vars_Temp=Pred_Vars
  Summ_Table=list()
  Temp_Table=data.table(
    Step=1,
    Further_Excluded_Var="(none)",
    QIC=QIC(Current_Full_Model)["QIC"])
  
  # run GEE excluding one variable with the highest p-valuse in the current model
  for(Step in 1:length(Pred_Vars)){
    #Step=2
    Out$Model[[Step]]=Current_Full_Model
    Current_Coefficients=coef(Current_Full_Model)
    Current_Summ_Table=data.table(names=c("(Intercept)", Pred_Vars_Temp),
                                  esticon(Current_Full_Model, diag(length(Current_Coefficients)))[, c("estimate", "std.error", "p.value")])
    
    # save info of the current model
    Summ_Table[[Step]]=Current_Summ_Table[order(p.value, decreasing=TRUE)]
    
    # identify the variable to remove by p-value
    Var_To_Remove=Current_Summ_Table[, names][which(Current_Summ_Table$p.value==max(Current_Summ_Table[-1, p.value]))]
    Pred_Vars_Temp=Pred_Vars_Temp[Pred_Vars_Temp!=Var_To_Remove]
    
    # update the model
    Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Var_To_Remove, collapse="-"))))
    Current_Full_Model=Current_Reduced_Model
    
    # save info
    Temp_Table=rbind(Temp_Table,
                     data.table(
                       Step=Step+1,
                       Further_Excluded_Var=Var_To_Remove,
                       QIC=QIC(Current_Full_Model)["QIC"]))
    
    print(paste0("Step : ", Step, "/", length(Pred_Vars)))
  }
  Out$Model[[Step+1]]=Current_Full_Model
  
  # Out
  Out$summ_table=Summ_Table
  Out$QIC_Table=Temp_Table
  # the final model with the lowest QIC
  Out$Selected_Vars=Temp_Table$Further_Excluded_Var[(which.min(Temp_Table[, QIC])+1):nrow(Temp_Table)]
  
  return(Out)
}

#*******************************
# GEE_Backward_by_P_missing_Data
#*******************************
# If the variable contains missing values, these missing data are removed even when the reduced model is fitted.
# As there are one variable less in the reduced model, it is likely that a larger sample is used for the reduced model fit.
# In other words, the sample used for the reference model is likely to be smaller because of the variable that is included in the reference model, but not in the reduced model.
# On the contrary, GEE_Backward_by_P (or GEE_Backward_by_P_2) runs a reduced model based on the same sample that's used for the reference model through the 'update' function.
# I'm not sure for now which one to use between this one and GEE_Backward_by_P (or GEE_Backward_by_P_2).
# However, it might make more sense to use GEE_Backward_by_P (or GEE_Backward_by_P_2) because a reduced model is built and compared with the reference model based on the same sample.
GEE_Backward_by_P_missing_Data=function(Data,
                                        Pred_Vars,
                                        Res_Var,
                                        Group_Var,
                                        which.family){ # minimum percentage of change-in-estimate to terminate the algorithm
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
  Pred_Vars_Temp=Pred_Vars
  Full_Model=GEE_Multivariable(Data=Data,
                               Pred_Vars=Pred_Vars_Temp,
                               Res_Var=Res_Var,
                               Group_Var="CODE",
                               which.family="binomial")
  
  Current_Full_Model=Full_Model
  
  #***************
  # main algorithm
  #***************
  Reduced_Model_QICs=c()
  
  Summ_Table=list()
  Temp_Table=data.table(
    Step=1,
    Further_Excluded_Var="(none)",
    QIC=QIC(Current_Full_Model$model_fit)["QIC"])
  
  Out$Model[[1]]=Current_Full_Model$model_fit
  Summ_Table[[1]]=Current_Full_Model$summ_table[order(P.value, decreasing=TRUE)]
  # run GEE excluding one variable with the highest p-valuse in the current model
  for(Step in 1:length(Pred_Vars)){
    #Step=1
    est=esticon(Current_Full_Model$model_fit,
                diag(length(coef(Current_Full_Model$model_fit))))[-1, ]
    Current_Summ_Table=data.table(names(coef(Current_Full_Model$model_fit))[-1],
                                  est)
    Current_Summ_Table=Current_Summ_Table[order(p.value, decreasing=TRUE)]
    
    # identify the variable to remove by p-value
    Var_To_Remove=Current_Summ_Table[, 1][which.max(Current_Summ_Table[, p.value])]
    Var_To_Remove_Original_Name=Pred_Vars_Temp[unlist(lapply(Pred_Vars_Temp, function(x) grepl(x, Var_To_Remove)))]
    Pred_Vars_Temp=Pred_Vars_Temp[Pred_Vars_Temp!=Var_To_Remove_Original_Name]
    
    # update the model
    if(length(Pred_Vars_Temp)==0){
      break
    }else{
      Current_Reduced_Model=GEE_Multivariable(Data=Data,
                                              Pred_Vars=Pred_Vars_Temp,
                                              Res_Var=Res_Var,
                                              Group_Var="CODE",
                                              which.family="binomial")
      Current_Full_Model=Current_Reduced_Model
    }
    
    # save info
    Out$Model[[Step+1]]=Current_Full_Model$model_fit
    Summ_Table[[Step+1]]=Current_Full_Model$summ_table[order(P.value, decreasing=TRUE)]
    Temp_Table=rbind(Temp_Table,
                     data.table(
                       Step=Step+1,
                       Further_Excluded_Var=Var_To_Remove_Original_Name,
                       QIC=QIC(Current_Reduced_Model$model_fit)["QIC"]))
    
    print(paste0("Step : ", Step, "/", length(Pred_Vars)))
  }
  Out$Model[[Step+1]]=Current_Full_Model
  
  # Out
  Out$summ_table=Summ_Table
  Out$QIC_Table=Temp_Table
  # the final model with the lowest QIC
  Out$Selected_Vars=Temp_Table$Further_Excluded_Var[(which.min(Temp_Table[, QIC])+1):nrow(Temp_Table)]
  
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
#                Pred_Vars=c(Pred_Vars[!Pred_Vars%in%c("sex", "age")],
#                            list(c("sex", "age", "sex:age"))),
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
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use$sex[c(2, 15, 35, 58, 93)]="N"
# Data_to_use$sex=factor(Data_to_use$sex)
# Data_to_use$id=as.factor(Data_to_use$id)
# Data_to_use=Format_Columns(Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# #Two arguments (which.family and NAGQ) must be declared with '<-' in a function when estimating power!
# GLMM_Mult_model=GLMM_Multivariable(Data=rbind(Data_to_use, Data_to_use),
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
  lapply(c("lme4", "simr",
           "glmmTMB",
           "sjPlot",
           "MASS", "data.table", "optimx"), checkpackages)
  
  # as data frame
  Data=as.data.frame(Data)
  Non_Missing_Outcome_Obs=which(!is.na(Data[, Res_Var]))
  Data=Data[Non_Missing_Outcome_Obs, ]
  Origin_N_Rows=nrow(Data)
  
  # run model
  #fullmod=as.formula(paste(Res_Var, "~", paste(Pred_Vars, collapse="+"), paste("+(1|", Group_Var, ")", collapse=""), sep=""))
  if(grepl("gaussian", which.family)){
    lapply(c("lmerTest"), checkpackages) # this package is required to obtain p-values
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
    Output$summ_table$`P-value`=ifelse(Coef[, "Pr(>|t|)"][Coef.ind]<0.001, "<0.001", 
                                       format(round2(Coef[, ncol(Coef)][Coef.ind], 7), nsmall=3))
    Output$summ_table$Estimate.and.CI=paste0(format(round2(Coef[, "Estimate"][Coef.ind], 2), nsmall=2), 
                                             " (", format(round2(CI.raw[CI.raw.ind, 1], 2), nsmall=2), " - ", 
                                             format(round2(CI.raw[CI.ind, 2], 2), nsmall=2), ")")
  }else if(grepl("poisson", which.family) | grepl("negative_binomial", which.family)){
    Output$summ_table$`P-value`=ifelse(Coef[, "Pr(>|z|)"][Coef.ind]<0.001, "<0.001", 
                                       format(round2(Coef[, ncol(Coef)][Coef.ind], 7), nsmall=3))
    Output$summ_table$RR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }else if(grepl("binomial", which.family)){
    Output$summ_table$`P-value`=ifelse(Coef[, "Pr(>|z|)"][Coef.ind]<0.001, "<0.001", 
                                       format(round2(Coef[, ncol(Coef)][Coef.ind], 7), nsmall=3))
    Output$summ_table$OR.and.CI=paste0(format(round2(exp(Coef[, "Estimate"][Coef.ind]), 2), nsmall=2), 
                                       " (", format(round2(CI[CI.ind, 1], 2), nsmall=2), " - ", 
                                       format(round2(CI[CI.ind, 2], 2), nsmall=2), ")")
  }
  
  
  Output$summ_table=as.data.frame(Output$summ_table) %>% as.data.table(keep.rownames=TRUE)
  
  
  if(!is.null(dim(Output_vif))){
    Output$summ_table=cbind(Output$summ_table,
                            `GVIF^(1/(2*Df))`=rep(Output_vif[, 3], Output_vif[, 2]),
                            `GVIF^(1/(2*Df))_Threshold`=sqrt(10), # threshold is sqrt(10) for now
                            # https://rdrr.io/cran/pedometrics/src/R/stepVIF.R
                            # https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif/96584#96584
                            N_data_used=N_data_used)
  }else{
    Output$summ_table=cbind(Output$summ_table,
                            VIF=Output_vif,
                            N_data_used=N_data_used)
  }
  
  # likelihood ratio test
  # this works only for lmer (that is, which.family=="gaussian")
  if(grepl("gaussian", which.family)){
    LRT_results=anova(myfit)
    LRT_pvalues=LRT_results$`Pr(>F)`
    LRT_pvalues=ifelse(LRT_pvalues<0.001, "<0.001", 
                       format(round2(LRT_pvalues, 7), nsmall=3))
    Output$summ_table$`P.value(F-test)`=rep(LRT_pvalues, LRT_results$NumDF)
    Output$summ_table$`P.value(F-test)`[duplicated(rep(rownames(LRT_results), LRT_results$NumDF))]=""
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
# Pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# Res_var="outcome"
# rand_var="id"
# which.family="gaussian(link=identity)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_vars=="treat")]="P"
# levels.of.fact[which(Pred_vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data=Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars=Pred_vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 150), "sex"]="N"
# lambda=seq(0, 5, by=0.5)
# 
# # exclude missing obsevations
# Data_to_use=as.data.table(Data_to_use)
# Data_to_use=na.omit(Data_to_use[,
#                                 .SD,
#                                 .SDcols=c(Res_var, Pred_vars, rand_var)])
# 
# # grouping variable
# Data_to_use[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
# 
# # speicfy GLMM model
# form.fixed=as.formula(outcome ~ center + as.factor(treat) + as.factor(sex) + age + baseline + visit)
# # random effect
# form.rnd=list(id=~1)
# 
# # GLMM_LASSO_CV_Out
# GLMM_LASSO_CV_Out=GLMM_LASSO_CV(Data=Data_to_use,
#                                 form.fixed=form.fixed,
#                                 form.rnd=form.rnd,
#                                 which.family=which.family,
#                                 k=6,
#                                 lambda=lambda)
# 
# # train error
# GLMM_LASSO_CV_Out$Train_Error
# # cv error
# GLMM_LASSO_CV_Out$CV_Error
# # plot
# GLMM_LASSO_CV_Out$CV_Error_plot
# # optimal lambda
# GLMM_LASSO_CV_Out$Optimal_Lambda_by_CV_Error
# # IC_Matrix
# GLMM_LASSO_CV_Out$IC_matrix
# 
# # There's a function that performs a CV for GLMM, called cv.glmmLasso, in lmmen package.
# # However, there appears to be some issues when the function is excuted (fun a code below). I googled to find how to troubleshoot, but there's even not
# # a single example that shows the use of the function.
# # https://raw.githubusercontent.com/cran/glmmLasso/master/demo/glmmLasso-soccer.r
# # (Edit) The function seems to work fine now, so some parts from the original script of the function are integrated into GLMM_LASSO_CV to obtain AIC and BIC.
# # lapply(c("lmmen"), checkpackages)
# # Package 'lmmen' was removed from the CRAN repository. (https://CRAN.R-project.org/package=lmmen)
# Origianl_cv.glmmLasso=cv.glmmLasso(dat=Data_to_use,
#                                    form.fixed=form.fixed,
#                                    form.rnd=form.rnd,
#                                    family=which.family,
#                                    lambda=seq(0, 5, by=0.5))
# # compare BICs
# GLMM_LASSO_CV_Out$IC_matrix$BIC==Origianl_cv.glmmLasso$BIC_path
GLMM_LASSO_CV=function(Data,
                       form.fixed,
                       form.rnd,
                       which.family,
                       k=4,
                       lambda=seq(0, 10, by=1)){
  #Data=Data[sample(1:nrow(Data), 5000), ]
  
  # check out packages
  lapply(c("glmmLasso", "data.table", "dplyr", "ggplot2"), checkpackages)
  # convert data to data frame
  Data=as.data.table(Data)
  
  # res_var
  res_var=gsub("as.factor", "", as.character(form.fixed)[2])
  
  # pred_vars
  pred_vars=unlist(strsplit(
    gsub("[() ]",
         "",
         gsub("as.factor", "", as.character(form.fixed)[3])),
    "\\+"
  ))
  
  # grouping variable
  group_var=names(form.rnd)
  Data[, (group_var):=lapply(.SD, as.factor), .SDcols=group_var]
  
  # remove missing data
  Data=na.omit(Data[, .SD, .SDcols=c(res_var, pred_vars, group_var)])
  
  #***
  # CV
  #***
  # generate array containing fold-number for each sample (row)
  pass.ind=1
  while(sum(pass.ind)>0){
    folds=sample(rep_len(1:k, nrow(Data)), nrow(Data))
    for(k.ind in 1:k){
      #k.ind=1
      # actual split of the Data
      fold=which(folds == k.ind)
      
      # divide data into training and test sets
      Data_train=Data[-fold, ]
      Data_test=Data[fold, ]
      
      if(sum((Data_train[, .SD, .SDcols=pred_vars] %>% 
              lapply(function(x) length(unique(x))) %>% 
              unlist)==1)>0){
        print(which((Data_train[, .SD, .SDcols=pred_vars] %>% 
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
  
  # generate empty matrix to save Train_Error
  Train_Error=matrix(NA, length(lambda), k)
  rownames(Train_Error)=c(paste0("lambda=", lambda))
  colnames(Train_Error)=c(paste0(1:k, "nd sub"))
  # generate empty matrix to save CV_Error
  CV_Error=matrix(NA, length(lambda), k)
  rownames(CV_Error)=c(paste0("lambda=", lambda))
  colnames(CV_Error)=c(paste0(1:k, "nd sub"))
  # generate empty matrix to save IC_matrix
  IC_matrix=matrix(NA, length(lambda), ncol=2)
  rownames(IC_matrix)=c(paste0("lambda=", lambda))
  colnames(IC_matrix)=c("AIC", "BIC")
  
  # specify starting values for the very first fit; pay attention that Delta.start has suitable length! 
  d.size=(max(as.numeric(row.names(Data)))*(sum(grepl('^Z',names(Data)))+1))+(sum(grepl('^X',names(Data)))+1)
  Delta.start.base=Delta.start=as.matrix(t(rep(0,d.size)))
  Q.start.base=Q.start=0.1  
  
  # run algorithm
  for(lambda.ind in 1:length(lambda)){
    #lambda.ind=1
    
    # actual cross validation
    for(k.ind in 1:k) {
      #k.ind=1
      # actual split of the Data
      fold=which(folds == k.ind)
      
      # divide data into training and test sets
      Data_train=Data[-fold, ]
      Data_test=Data[fold, ]
      
      ## fit adjacent category model
      glmmLasso.cv.fit=glmmLasso(form.fixed, 
                                 rnd=form.rnd, 
                                 family=eval(parse(text=which.family)), 
                                 data=Data_train, 
                                 lambda=lambda[lambda.ind])
      
      # Make predictions and compute the R2, RMSE and MAE
      predictions_train=glmmLasso.cv.fit %>% predict(Data_train)
      predictions_test=glmmLasso.cv.fit %>% predict(Data_test)
      
      # Train_Error and CV_Error
      Train_Error[lambda.ind, k.ind]=mean(unlist(predictions_train - Data_train[, .SD, .SDcol=res_var])^2)
      CV_Error[lambda.ind, k.ind]=mean(unlist(predictions_test - Data_test[, .SD, .SDcol=res_var])^2)
      
      if(k.ind == k){
        # print process
        print(paste0("k : ", k.ind, ", lambda : ", lambda[lambda.ind]))
      }
    }
    
    # obtain AIC & BIC
    suppressMessages({
      suppressWarnings({
        glmmLasso.fit=try(glmmLasso::glmmLasso(fix=stats::as.formula(form.fixed),
                                               rnd=form.rnd,
                                               data=Data,lambda=lambda[lambda.ind],
                                               switch.NR=FALSE,final.re=TRUE,
                                               control=list(start=Delta.start[lambda.ind,],q.start=Q.start[lambda.ind]))
        )
      })      
    })
    
    if(class(glmmLasso.fit)!="try-error")
    {  
      IC_matrix[lambda.ind, "AIC"]=glmmLasso.fit$aic
      IC_matrix[lambda.ind, "BIC"]=glmmLasso.fit$bic
      Delta.start=rbind(Delta.start,glmmLasso.fit$Deltamatrix[glmmLasso.fit$conv.step,])
      Q.start=c(Q.start,glmmLasso.fit$Q_long[[glmmLasso.fit$conv.step+1]])
    }else{
      Delta.start=rbind(Delta.start,Delta.start.base)
      Q.start=c(Q.start,Q.start.base)
    }
    # 
    # glmmLasso.fit=glmmLasso(form.fixed, 
    #                         rnd=form.rnd, 
    #                         family=eval(parse(text=which.family)), 
    #                         data=Data, 
    #                         lambda=lambda[lambda.ind])
    # IC_matrix[lambda.ind, "AIC"]=glmmLasso.fit$aic
    # IC_matrix[lambda.ind, "BIC"]=glmmLasso.fit$bic
  }
  # combine Train_Error and CV_Error
  out=list()
  out$Train_Error=Train_Error
  out$CV_Error=CV_Error
  out$IC_matrix=as.data.table(IC_matrix, keep.rownames=TRUE)
  
  # optimal lambda by CV Error
  out$Optimal_Lambda_by_CV_Error=lambda[which.min(apply(CV_Error, 1, mean))]
  
  # plot
  Error_by_Lambda=data.frame(
    lambda=lambda,
    Error=c(apply(out$Train_Error, 1, mean), apply(out$CV_Error, 1, mean)),
    Label=c(rep("Train", length(lambda)), rep("CV", length(lambda)))
  )
  out$CV_Error_plot=Error_by_Lambda %>%
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
# Pred_vars=c("center", "treat", "sex", "age", "baseline", "visit")
# Res_var="outcome"
# rand_var="id"
# which.family="gaussian(link=identity)" # "gaussian(link=identity)", "binomial(link=logit)", "poisson(link=log)"
# vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_vars], class))=="integer", "num", "fact")
# levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
# levels.of.fact[which(Pred_vars=="treat")]="P"
# levels.of.fact[which(Pred_vars=="sex")]="F"
# 
# Data_to_use=Format_Columns(Data=Data_to_use,
#                            Res_Var="outcome",
#                            Pred_Vars=Pred_vars,
#                            vector.OF.classes.num.fact,
#                            levels.of.fact)
# 
# Data_to_use$sex=as.character(Data_to_use$sex)
# Data_to_use[sample(nrow(Data_to_use), 150), "sex"]="N"
# lambda=seq(0, 5, by=0.5)
# 
# # exclude missing obsevations
# Data_to_use=as.data.table(Data_to_use)
# Data_to_use=na.omit(Data_to_use[,
#                                 .SD,
#                                 .SDcols=c(Res_var, Pred_vars, rand_var)])
# 
# # grouping variable
# Data_to_use[, (rand_var):=lapply(.SD, as.factor), .SDcols=rand_var]
# 
# # speicfy GLMM model
# form.fixed=as.formula(outcome ~ center + as.factor(treat) + as.factor(sex) + age + baseline + visit)
# # random effect
# form.rnd=list(id=~1)
# 
# # GLMM_LASSO_CV_Out
# GLMM_LASSO_CV_Out=GLMM_LASSO_CV(Data=Data_to_use,
#                                 form.fixed=form.fixed,
#                                 form.rnd=form.rnd,
#                                 which.family=which.family,
#                                 k=6,
#                                 lambda=lambda)
# 
# # optimal lambda by CV error
# GLMM_LASSO_CV_Out$Optimal_Lambda_by_CV_Error
# # optimal labmda by AIC
# lambda[which.min(GLMM_LASSO_CV_Out$IC_matrix$AIC)]
# # optimal labmda by BIC
# lambda[which.min(GLMM_LASSO_CV_Out$IC_matrix$BIC)]
# 
# # fit model
# GLMM.LASSO.fit=GLMM_LASSO(Data=Data_to_use,
#                           Pred_vars,
#                           Res_var,
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

#*************
# cv.glmmLasso
#*************
# This function comes from the package 'lmmen' that was removed from the CRAN repository. (https://CRAN.R-project.org/package=lmmen)
#' @title Cross Validation for glmmLasso package
#' @description Cross Validation for glmmLasso package as shown in example xxx
#' @param dat data.frame, containing y,X,Z and subject variables
#' @param form.fixed formaula, fixed param formula, Default: NULL
#' @param form.rnd list, named list containing random effect formula, Default: NULL
#' @param lambda numeric, vector containing lasso penalty levels, Default: seq(500, 0, by = -5)
#' @param family family, family function that defines the distribution link of the glmm, Default: gaussian(link = "identity")
#' @return list of a fitted glmmLasso object and the cv BIC path
#' @examples
#' \dontrun{cv.glmmLasso(initialize_example(seed=1))}
#' @references A. Groll and G. Tutz. Variable selection for generalized linear mixed models by L1-penalized estimation. 
#'  Statistics and Computing, pages 1â€“18, 2014.
#'  
#'  \href{https://raw.githubusercontent.com/cran/glmmLasso/master/demo/glmmLasso-soccer.r}{cv function is the generalized form of last example glmmLasso package demo file}
#'  
#' @seealso 
#'  \code{\link[glmmLasso]{glmmLasso}}
#' @rdname cv.glmmLasso
#' @export 
#' @importFrom glmmLasso glmmLasso
#' @importFrom stats gaussian as.formula
cv.glmmLasso=function(dat,
                      form.fixed=NULL,
                      form.rnd=NULL,
                      lambda=seq(500,0,by=-5),
                      family=stats::gaussian(link = "identity")
){
  
  if(inherits(dat,'matrix')) dat <- as.data.frame(dat)
  
  d.size=(max(as.numeric(row.names(dat)))*(sum(grepl('^Z',names(dat)))+1))+(sum(grepl('^X',names(dat)))+1)
  
  dat<-data.frame(subject=as.factor(row.names(dat)),dat,check.names = FALSE,row.names = NULL)
  
  if(is.null(form.fixed)) form.fixed<-sprintf('y~%s',paste(grep('^X',names(dat),value = TRUE),collapse = '+'))
  if(is.null(form.rnd)) form.rnd<-eval(parse(text=sprintf('form.rnd<-list(subject=~1+%s)',paste(grep('^Z',names(dat),value = TRUE),collapse = '+'))))
  
  BIC_vec<-rep(Inf,length(lambda))
  
  # specify starting values for the very first fit; pay attention that Delta.start has suitable length! 
  
  Delta.start.base<-Delta.start<-as.matrix(t(rep(0,d.size)))
  Q.start.base<-Q.start<-0.1  
  
  for(j in 1:length(lambda))
  {
    suppressMessages({
      suppressWarnings({
        fn <- try(glmmLasso::glmmLasso(fix = stats::as.formula(form.fixed),
                                       rnd = form.rnd,
                                       data = dat,lambda = lambda[j],
                                       switch.NR = FALSE,final.re=TRUE,
                                       control = list(start=Delta.start[j,],q.start=Q.start[j]))      
        )
      })      
    })
    
    if(class(fn)!="try-error")
    {  
      BIC_vec[j]<-fn$bic
      Delta.start<-rbind(Delta.start,fn$Deltamatrix[fn$conv.step,])
      Q.start<-c(Q.start,fn$Q_long[[fn$conv.step+1]])
    }else{
      Delta.start<-rbind(Delta.start,Delta.start.base)
      Q.start<-c(Q.start,Q.start.base)
    }
  }
  
  opt<-which.min(BIC_vec)
  
  suppressWarnings({
    final <- glmmLasso::glmmLasso(fix = as.formula(form.fixed), rnd = form.rnd,
                                  data = dat, lambda=lambda[opt],switch.NR=FALSE,final.re=TRUE,
                                  control = list(start=Delta.start[opt,],q_start=Q.start.base))
    
    final
  })
  
  list(fit.opt=final,BIC_path=BIC_vec)
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



#*********************
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
# ********
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
#                                          Pred_Vars=Pred_Vars)
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
    if(length(Include_Index)==0){
      print("All variables are removed.")
    }else{
      for(i in 1:length(Pred_Vars[Include_Index])){
        #i=1
        Current_Reduced_Model=update(Current_Full_Model, formula(paste0(".~.-", paste(Pred_Vars[Include_Index][i], collapse="-"))))
        Reduced_Model_AICs[i]=AIC(Current_Reduced_Model)
        print(paste0("Step : ", step, " - Vars : ", i, "/", length(Pred_Vars[Include_Index])))
      }
    }
    
    # AIC of multivariable model excluding each variable
    Temp_Table=data.table(
      Exclusion="-",
      Var=c("(none)", Pred_Vars[Include_Index]),
      AIC=c(Current_Full_Model_AIC, Reduced_Model_AICs))
    
    # order by AIC
    Temp_Table=Temp_Table[order(AIC), ]
    
    # save summary table at the current step
    Out$summ_table[[step]]=Temp_Table
    
    if(Temp_Table$Var[1]=="(none)"){
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
    
    Output=rbind(Output, Temp$summ_table)
    
    # print out progress
    if(sum(grepl(":", Pred_Vars[i]))>0){
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", unlist(Pred_Vars[i])[grepl(":", unlist(Pred_Vars[i]))], " (", i ," out of ", length(Pred_Vars), ")"))
    }else{
      print(paste0("Res_Var : ", Res_Var, ", Pred_Var : ", Pred_Vars[i], " (", i ," out of ", length(Pred_Vars), ")"))
    }
  }
  
  return(as.data.table(Output, keep.rownames=TRUE))
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
  Output$model_fit=myfit
  
  Output$converged.set=data.table(count=count, k=k, maxit=maxit, tol=tol)
  
  N_data_used=paste0(Used_N_Rows, "/", Origin_N_Rows, " (", round(Used_N_Rows/Origin_N_Rows*100, 2), "%)") 
  
  Estimate=t(matrix(round2(Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  Std.Error=t(matrix(round2(SE.Coef[Coef.ind], 3), nrow=length(levels(Y))-1))
  `P-value`=t(matrix(ifelse(P_values[Coef.ind]<0.001, "<0.001", 
                            format(round2(P_values[Coef.ind], 3), nsmall=3)), nrow=length(levels(Y))-1))
  OR.and.CI=t(matrix(paste0(format(round(exp(Coef[Coef.ind]), 2), nsmall=2), 
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
  
  colnames(Estimate)=levels(Y)[-length(levels(Y))]
  colnames(Std.Error)=levels(Y)[-length(levels(Y))]
  colnames(`P-value`)=levels(Y)[-length(levels(Y))]
  colnames(OR.and.CI)=levels(Y)[-length(levels(Y))]
  rownames(Estimate)=row_names
  rownames(Std.Error)=row_names
  rownames(`P-value`)=row_names
  rownames(OR.and.CI)=row_names
  
  # summ_table
  summ_table=c()
  for(ind in 1:ncol(Estimate)){
    Temp_Summ=cbind(OR.and.CI[, ind],
                    `P-value`[, ind])
    colnames(Temp_Summ)=paste0(colnames(Estimate)[ind], c(" (OR.and.CI)", " (P-value)"))
    
    summ_table=cbind(summ_table, Temp_Summ)
  }
  rownames(summ_table)=rownames(OR.and.CI)
  summ_table=cbind(summ_table, N_data_used)
  
  Output$summ_table=as.data.table(summ_table,
                                  keep.rownames=TRUE)
  
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
#***********************
# CLMM_Ordinal_Bivariate
CLMM_Ordinal_Bivariate=function(Data,
                                Pred_Vars,
                                Type_Odds,
                                Res_Var,
                                Group_Var,
                                NAGQ=3,
                                ...){
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
    #i=5
    i<<-i
    if(Type_Odds[i]=="Prop"){
      model_fit=clmm2(as.formula(paste(Res_Var, "~", Pred_Vars[i])),
                      #nominal=as.formula(paste("~", Nom_Vars[i])),
                      random=eval(parse(text=Group_Var)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic",
                      ...)
      # adjust the value of grtol if the algorithm failed to converge
      # control=clmm2.control(grtol=1e-1)
    }else if(Type_Odds[i]=="Non_Prop"){
      model_fit=clmm2(as.formula(paste(Res_Var, "~1")),
                      nominal=as.formula(paste("~", Pred_Vars[i])),
                      random=eval(parse(text=Group_Var)),
                      data=Data,
                      Hess=TRUE,
                      nAGQ=NAGQ,
                      link="logistic",
                      ...)
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
                                    NAGQ=3,
                                    ...){
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
                    link="logistic",
                    ...)
  }else if(length(Nom_Vars)==0){ # if all are proportional odds
    model_fit=clmm2(as.formula(paste(Res_Var, "~", paste(Loc_Vars, collapse="+"))),
                    #nominal=as.formula(paste("~", paste(Nom_Vars, collapse="+"))),
                    random=eval(parse(text=Group_Var)),
                    data=Data,
                    Hess=TRUE,
                    nAGQ=NAGQ,
                    link="logistic",
                    ...)
  }else if(length(Loc_Vars)!=0 & length(Nom_Vars)!=0){ # mixed
    model_fit=clmm2(as.formula(paste(Res_Var, "~", paste(Loc_Vars, collapse="+"))),
                    nominal=as.formula(paste("~", paste(Nom_Vars, collapse="+"))),
                    random=eval(parse(text=Group_Var)),
                    data=Data,
                    Hess=TRUE,
                    nAGQ=NAGQ,
                    link="logistic",
                    ...)
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
#                             Ind_P_Value=T)
Contingency_Table_Generator=function(Data,
                                     Row_Var,
                                     Col_Var,
                                     Ref_of_Row_Var,
                                     Missing="Not_Include",
                                     Ind_P_Value=F){
  # library
  lapply(c("epitools"), checkpackages)
  
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
  colnames(Merged)[1:2]=c("Variable", "Value")
  colnames(Merged)[3:(3+ncol(Contingency_Table)-1)]=paste0(Col_Var, "=", colnames(Contingency_Table), " (n=", Sum_Col_Wise, ")")
  colnames(Merged)[3+ncol(Contingency_Table)]=paste0("Total (n=", sum(Sum_Col_Wise), ")")
  Out=Merged
  
  # calculate OR given a response variable of two levels
  if(sum(rownames(Contingency_Table)!="NA")>=2 &
     sum(colnames(Contingency_Table)!="NA")==2){
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
  if(sum(rownames(Contingency_Table)!="NA")>1 & sum(colnames(Contingency_Table)!="NA")>1){
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
# Data_to_use=as.data.table(respiratory)
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
Contingency_Table_Generator_Conti_X=function(Data,
                                             Row_Var,
                                             Col_Var,
                                             Ref_of_Row_Var,
                                             Missing="Not_Include"){
  # library
  lapply(c("doBy"), checkpackages)
  
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
  colnames(Out)[1:2]=c("Variable", "Value")
  
  #
  colnames(Out)[3:(3+length(Sum_Col_Wise)-1)]=paste0(Col_Var, "=", names(Sum_Col_Wise), " (n=", Sum_Col_Wise, ")")
  colnames(Out)[(3+length(Sum_Col_Wise))]=paste0("Total (n=", sum(Sum_Col_Wise), ")")
  colnames(Out)[(3+length(Sum_Col_Wise)+1):(3+length(Sum_Col_Wise)+5)]=c("OR (95% CI)", "P-value (GLM)", "P-value (Mann_Whitney)", "P-value (T_test)", "P-value (ANOVA)")
  
  # return
  return(as.data.table(Out))
}


#*************************************
#
# Weighted_Contingency_Table_Generator
#
#*************************************
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
# 
# # CT_1 using Contingency_Table_Generator()
# CT_1=Contingency_Table_Generator(Data=na.omit(Data_to_use),
#                                  Row_Var="sex",
#                                  Col_Var="outcome",
#                                  Ref_of_Row_Var="F",
#                                  Missing="Include",
#                                  Ind_P_Value=T)
# # A table made from CT_2 is identical to CT_1 above given Weight_Var=1 and no missing data.
# CT_2=Weighted_Contingency_Table_Generator(Data=na.omit(Data_to_use),
#                                           Row_Var="sex",
#                                           Col_Var="outcome",
#                                           Weight_Var=1)
# # for now, Treat should be numeric
# Data_to_use[treat=="A", Treat:=1]
# Data_to_use[treat=="P", Treat:=0]
# 
# # estimate inverse probability (IP) weights
# # distinguish time-invariant and -variant covariates
# Time_Invariant_Covs=c("baseline", "center")
# Time_Varying_Covs=c("sex", "age")
# Data_to_use=na.omit(Data_to_use)
# w=IPW(Exposure="Treat",
#       Time_Invariant_Covs=Time_Invariant_Covs,
#       Time_Varying_Covs=Time_Varying_Covs,
#       which.family="binomial",
#       Link="logit",
#       ID_Var="id",
#       Data="Data_to_use")
# head(w$unstab_IP_weights$ipw.weights, 10)
# head(w$basic_stab_IP_weights$ipw.weights, 10)
# head(w$adjusted_stab_IP_weights$ipw.weights, 10)
# Data_to_use[, Weights:=w$adjusted_stab_IP_weights$ipw.weights]
# # apply weights
# CT_3=Weighted_Contingency_Table_Generator(Data=na.omit(Data_to_use),
#                                           Row_Var="sex",
#                                           Col_Var="outcome",
#                                           Weight_Var="Weights")
Weighted_Contingency_Table_Generator=function(Data,
                                              Row_Var,
                                              Col_Var,
                                              # Missing,
                                              Weight_Var){
  # check packages
  lapply(c("data.table",
           "tableone",
           "jstable",
           
           "survey",
           "stringr", # for str_replace()
           
           "smd", # calculate smd
           "MBESS"), # for smd confidence interval
         checkpackages)
  
  # Data to data.frame
  Data=as.data.frame(Data)
  
  # apply weights to data
  weighteddata=svydesign(ids=~1,
                         data=Data,
                         weights=formula(paste0("~", Weight_Var)))
  
  #***********************
  # weighted table overall
  weightedtable_overall=svyCreateTableOne(vars=Row_Var,
                                          data=weighteddata,
                                          test=TRUE,
                                          smd=TRUE,
                                          factorVars=Row_Var)
  weightedtable_overall_result=print(weightedtable_overall,
                                     showAllLevels=TRUE,
                                     formatOptions=list(big.mark=","),
                                     noSpaces=TRUE,
                                     printToggle=FALSE)
  
  # # detailed information including missingness (this is for continuous)
  # summary(weightedtable_overall)
  # 
  # biomarkers <- c("bili","chol","copper","alk.phos","ast","trig","protime")
  # print(tab2, nonnormal = biomarkers, formatOptions = list(big.mark = ","))
  
  #*************************************
  # weighted table stratified by Col_Var
  weightedtable_stratified=svyCreateTableOne(vars=Row_Var,
                                             strata=Col_Var,
                                             data=weighteddata,
                                             test=TRUE,
                                             smd=TRUE,
                                             factorVars=Row_Var)
  weightedtable_stratified_result=print(weightedtable_stratified,
                                        showAllLevels=TRUE,
                                        formatOptions=list(big.mark=","),
                                        noSpaces=TRUE,
                                        smd=TRUE,
                                        printToggle=FALSE)
  weightedtable_stratified_result[weightedtable_stratified_result==""]=NA
  
  # #*******************************
  # # calculate confidence intervals
  # # smd (https://cran.r-project.org/web/packages/smd/vignettes/smd_usage.html)
  # smd.output=smd::smd(x=Data[, Row_Var],
  #                     g=Data[, Col_Var],
  #                     w=Data[, Weight_Var],
  #                     std.error=TRUE)
  # 
  # smd.conf=c()
  # for(i in 1:length(smd.output$estimate)){
  #   ci.smd.output=ci.smd(smd=smd.output[i, "estimate"],
  #                        n.1=table(Data[[Col_Var]])[1],
  #                        n.2=table(Data[[Col_Var]])[2])
  #   point.smd=ci.smd.output$smd
  #   lower.smd=ci.smd.output$Lower.Conf.Limit.smd
  #   upper.smd=ci.smd.output$Upper.Conf.Limit.smd
  #   smd.conf[i]=c(paste0(round(point.smd, 2),
  #                        " (",
  #                        round(lower.smd, 2),
  #                        " - ",
  #                        round(upper.smd, 2),
  #                        ")"))
  # }
  
  # check if Row_Var is factor or numeric
  Check_Factor=ifelse(is.null(levels(Data[, Row_Var])), 0, 1)
  if(Check_Factor==1){
    X3_Text=as.character(levels(Data[, Row_Var]))
    X3_Text=X3_Text[X3_Text%in%names(table(Data[, Row_Var])!=0)[(table(Data[, Row_Var])!=0)]]
  }else{
    stop("Row_Var is not a factor. Please double-check.")
  }
  
  # Col_Var levels
  Check_Factor=ifelse(is.null(levels(Data[, Col_Var])), 0, 1)
  if(Check_Factor==1){
    X4_Text=as.character(levels(Data[, Col_Var]))
  }else{
    X4_Text=sort(unique(Data[, Col_Var]))
  }
  
  # Out
  Out=matrix(NA, nrow=length(X3_Text), ncol=5+length(X4_Text))
  Out[, 1]=Row_Var
  Out[, 2]=weightedtable_stratified_result[-1, "level"]
  for(i in 1:length(X4_Text)){
    Out[, i+2]=str_replace(weightedtable_stratified_result[-1, which(X4_Text[i]==colnames(weightedtable_stratified_result))], "[)]", "%)")
  }
  Out[, i+3]=str_replace(weightedtable_overall_result[-1, "Overall"], "[)]", "%)")
  
  # The hypothesis test functions used by default are chisq.test() for categorical variables (with continuity correction) and oneway.test() for continous variables (with equal variance assumption, i.e., regular ANOVA).
  # Two-group ANOVA is equivalent of t-test.
  # You may be worried about the nonnormal variables and small cell counts in the stage variable. In such a situation, you can use the nonnormal argument like before as well as the exact (test) argument in the print() method.
  # Now kruskal.test() is used for the nonnormal continous variables and fisher.test() is used for categorical variables specified in the exact argument. kruskal.test() is equivalent to wilcox.test() in the two-group case.
  Out[, i+4]=weightedtable_stratified_result[-1, "p"] # P-value (Chi-square)
  # Out[, i+5]=c("", smd.conf)
  Out[, i+5]=weightedtable_stratified_result[-1, "SMD"]
  
  colnames(Out)=c("Variable",
                  "Value",
                  paste0(Col_Var,
                         "=",
                         X4_Text,
                         " (n=",
                         round(as.numeric(weightedtable_stratified_result["n", c(2:(1+length(X4_Text)))]), 1),
                         ")"),
                  paste0("Total (n=",
                         round(as.numeric(weightedtable_overall_result["n", 2]), 1),
                         ")"),
                  "P-value (Chi-square)", # default : chisq.test()
                  # fisher.test() is not available 
                  # "SMD (95% CI)",
                  "SMD") # t-test
  
  return(as.data.table(Out))
}


#*********************************************
#
# Weighted_Contingency_Table_Generator_Conti_X
#
#*********************************************
# Example
#********
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
# Data_to_use=as.data.table(respiratory)
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
#                                     Missing="Not_Include")
# Weighted_Contingency_Table_Generator_Conti_X(Data=BL_Data,
#                                              Row_Var="age",
#                                              Col_Var="outcome",
#                                              Weight_Var=1)
# 
# # for now, Treat should be numeric
# Data_to_use[treat=="A", Treat:=1]
# Data_to_use[treat=="P", Treat:=0]
# 
# # estimate inverse probability (IP) weights
# # distinguish time-invariant and -variant covariates
# Time_Invariant_Covs=c("baseline", "center")
# Time_Varying_Covs=c("sex", "age")
# Data_to_use=na.omit(Data_to_use)
# w=IPW(Exposure="Treat",
#       Time_Invariant_Covs=Time_Invariant_Covs,
#       Time_Varying_Covs=Time_Varying_Covs,
#       which.family="binomial",
#       Link="logit",
#       ID_Var="id",
#       Data="Data_to_use")
# head(w$unstab_IP_weights$ipw.weights, 10)
# head(w$basic_stab_IP_weights$ipw.weights, 10)
# head(w$adjusted_stab_IP_weights$ipw.weights, 10)
# Data_to_use[, Weights:=w$adjusted_stab_IP_weights$ipw.weights]
# # apply weights
# Weighted_Contingency_Table_Generator_Conti_X(Data=na.omit(Data_to_use),
#                                              Row_Var="age",
#                                              Col_Var="outcome",
#                                              Weight_Var="Weights")
Weighted_Contingency_Table_Generator_Conti_X=function(Data,
                                                      Row_Var,
                                                      Col_Var,
                                                      Weight_Var=1,
                                                      Form=1){
  # check packages
  lapply(c("data.table",
           "tableone",
           "jstable",
           
           "survey",
           
           "smd", # calculate smd
           "MBESS"), # for smd confidence interval
         checkpackages)
  
  # Data to data.frame
  Data=as.data.frame(Data)
  
  # apply weights to data
  weighteddata=svydesign(ids=~1,
                         data=Data,
                         weights=formula(paste0("~", Weight_Var)))
  
  #***********************
  # weighted table overall
  weightedtable_overall=svyCreateTableOne(vars=Row_Var,
                                          data=weighteddata,
                                          test=TRUE,
                                          smd=TRUE)
  weightedtable_overall_median_result=print(weightedtable_overall,
                                            showAllLevels=TRUE,
                                            nonnormal=Row_Var, # nonnormal
                                            formatOptions=list(big.mark=","),
                                            noSpaces=TRUE,
                                            printToggle=FALSE)
  weightedtable_overall_mean_result=print(weightedtable_overall,
                                          showAllLevels=TRUE,
                                          formatOptions=list(big.mark=","),
                                          noSpaces=TRUE,
                                          printToggle=FALSE)
  
  # # detailed information including missingness (this is for continuous)
  # summary(weightedtable_overall)
  # 
  # biomarkers <- c("bili","chol","copper","alk.phos","ast","trig","protime")
  # print(tab2, nonnormal = biomarkers, formatOptions = list(big.mark = ","))
  
  #*************************************
  # weighted table stratified by Col_Var
  weightedtable_stratified=svyCreateTableOne(vars=Row_Var,
                                             strata=Col_Var,
                                             data=weighteddata,
                                             test=TRUE,
                                             smd=TRUE)
  weightedtable_stratified_median_result=print(weightedtable_stratified,
                                               showAllLevels=TRUE,
                                               nonnormal=Row_Var, # nonnormal
                                               formatOptions=list(big.mark=","),
                                               noSpaces=TRUE,
                                               smd=TRUE,
                                               printToggle=FALSE)
  weightedtable_stratified_mean_result=print(weightedtable_stratified,
                                             showAllLevels=TRUE,
                                             formatOptions=list(big.mark=","),
                                             noSpaces=TRUE,
                                             smd=TRUE,
                                             printToggle=FALSE)
  
  # #*******************************
  # # calculate confidence intervals
  # # smd (https://cran.r-project.org/web/packages/smd/vignettes/smd_usage.html)
  # smd.output=smd::smd(x=Data[, Row_Var],
  #                     g=Data[, Col_Var],
  #                     w=Data[, Weight_Var],
  #                     std.error=TRUE)
  # 
  # for(i in 1:length(smd.output$estimate)){
  #   ci.smd.output=ci.smd(smd=smd.output[i, "estimate"],
  #                        n.1=table(Data[[Col_Var]])[1],
  #                        n.2=table(Data[[Col_Var]])[2])
  #   point.smd=ci.smd.output$smd
  #   lower.smd=ci.smd.output$Lower.Conf.Limit.smd
  #   upper.smd=ci.smd.output$Upper.Conf.Limit.smd
  #   smd.conf[i]=c(paste0(round(point.smd, 2),
  #                        " (",
  #                        round(lower.smd, 2),
  #                        " - ",
  #                        round(upper.smd, 2),
  #                        ")"))
  # }
  
  # #*******************************
  # # calculate confidence intervals
  # # smd (https://cran.r-project.org/web/packages/smd/vignettes/smd_usage.html)
  # point.smd=smd::smd(x=Data[, Col_Var],
  #                    g=Data[, Row_Var],
  #                    w=Data[, Weight_Var],
  #                    std.error=TRUE)
  # 
  # smd.conf=ci.smd(smd=point.smd$estimate,
  #                 n.1=table(Data[[Col_Var]])[1],
  #                 n.2=table(Data[[Col_Var]])[2])
  
  # check if Row_Var is numeric
  Num_Factor=is.numeric(Data[, Row_Var])
  if(Num_Factor==1 & Form==1){
    Out_Rows=7
  }else if(Num_Factor==1 & Form==2){
    Out_Rows=2
  }else{
    stop("Row_Var is not a numerical variable. Please double-check.")
  }
  
  # Col_Var levels
  Check_Factor=ifelse(is.null(levels(Data[, Col_Var])), 0, 1)
  if(Check_Factor==1){
    X4_Text=as.character(levels(Data[, Col_Var]))
  }else{
    X4_Text=as.character(sort(unique(Data[, Col_Var])))
  }
  # p-value test
  if(length(X4_Text)==2){
    p_value_name="P-value (T-test)"
  }else if(length(X4_Text)>2){
    p_value_name="P-value (ANOVE)"
  }
  # Out
  Out=matrix(NA, nrow=Out_Rows, ncol=6+length(X4_Text))
  
  if(Form==1){
    Out[, 1]=Row_Var
    Out[, 2]=c("Min", "Q1", "Median", "Mean", "SD", "Q3", "Max")
    for(i in 1:length(X4_Text)){
      Out[, i+2]=c(round(c(weightedtable_stratified$ContTable[[X4_Text[i]]][, "min"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "p25"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "median"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "mean"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "sd"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "p75"],
                           weightedtable_stratified$ContTable[[X4_Text[i]]][, "max"]), 2))
    }
    Out[, i+3]=c(round(c(weightedtable_overall$ContTable$Overall[, "min"],
                         weightedtable_overall$ContTable$Overall[, "p25"],
                         weightedtable_overall$ContTable$Overall[, "median"],
                         weightedtable_overall$ContTable$Overall[, "mean"],
                         weightedtable_overall$ContTable$Overall[, "sd"],
                         weightedtable_overall$ContTable$Overall[, "p75"],
                         weightedtable_overall$ContTable$Overall[, "max"]), 2))
    
    # The hypothesis test functions used by default are chisq.test() for categorical variables (with continuity correction) and oneway.test() for continous variables (with equal variance assumption, i.e., regular ANOVA).
    # Two-group ANOVA is equivalent of t-test.
    # You may be worried about the nonnormal variables and small cell counts in the stage variable. In such a situation, you can use the nonnormal argument like before as well as the exact (test) argument in the print() method.
    # Now kruskal.test() is used for the nonnormal continous variables and fisher.test() is used for categorical variables specified in the exact argument. kruskal.test() is equivalent to wilcox.test() in the two-group case.
    Out[3, i+4]=weightedtable_stratified_median_result[-1, "p"] # P-value (Mann_Whitney)
    Out[4, i+5]=weightedtable_stratified_mean_result[-1, "p"] # P-value (ANOVE or T-test)
    # Out[, i+6]=smd.conf
    Out[4, i+6]=weightedtable_stratified_mean_result[-1, "SMD"]
    colnames(Out)=c("Variable",
                    "Value",
                    paste0(Col_Var,
                           "=",
                           X4_Text,
                           " (n=",
                           round(as.numeric(weightedtable_stratified_mean_result["n", c(2:(1+length(X4_Text)))]), 1),
                           ")"),
                    paste0("Total (n=",
                           round(as.numeric(weightedtable_overall_mean_result["n", 2]), 1),
                           ")"),
                    "P-value (Mann_Whitney)",  # default : kruskal.test(), which is equivalent to wilcox.test()
                    p_value_name, # default : oneway.test()
                    # "SMD (95% CI)",
                    "SMD") # t-test
  }else if(Form==2){
    Out[1, 1]=paste0(Row_Var, " (median [IQR])")
    Out[2, 1]=paste0(Row_Var, " (mean (SD))")
    Out[, 2]=weightedtable_stratified_mean_result[-1, "level"]
    for(i in 1:length(X4_Text)){
      Out[1, i+2]=weightedtable_stratified_median_result[-1, which(X4_Text[i]==colnames(weightedtable_stratified_median_result))]
      Out[2, i+2]=weightedtable_stratified_mean_result[-1, which(X4_Text[i]==colnames(weightedtable_stratified_mean_result))]
    }
    Out[1, i+3]=weightedtable_overall_median_result[-1, "Overall"]
    Out[2, i+3]=weightedtable_overall_mean_result[-1, "Overall"]
    
    Out[1, i+4]=weightedtable_stratified_median_result[-1, "p"] # P-value (Mann_Whitney)
    Out[2, i+5]=weightedtable_stratified_mean_result[-1, "p"] # P-value (ANOVE or T-test)
    
    # Out[, i+5]=smd.conf
    Out[2, i+6]=weightedtable_stratified_mean_result[-1, "SMD"]
    
    colnames(Out)=c("Variable",
                    "Value",
                    paste0(Col_Var,
                           "=",
                           X4_Text,
                           " (n=",
                           round(as.numeric(weightedtable_stratified_mean_result["n", c(2:(1+length(X4_Text)))]), 1),
                           ")"),
                    paste0("Total (n=",
                           round(as.numeric(weightedtable_overall_mean_result["n", 2]), 1),
                           ")"),
                    "P-value (Mann_Whitney)",  # default : kruskal.test(), which is equivalent to wilcox.test()
                    p_value_name, # default : oneway.test()
                    # "SMD (95% CI)",
                    "SMD") # t-test
  }
  
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
  
  names(Table)[is.na(names(Table))]="NA"
  
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
#**************************************
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
              Value=c(names(Table)),
              data.table(unclass(Table)))
  }else if(Form==2){
    Out=cbind(Var, 
              Value="Median (IQR)", 
              paste0(Table[3], " (", Table[5]-Table[2], ")"))
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
#
# Raw_Contingency_Table_Generator
#
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
  colnames(Merged)[3+ncol(Contingency_Table)]="Overall"
  # colnames(Merged)[3+ncol(Contingency_Table)]=paste0(sum(Sum_Col_Wise))
  Out=Merged
  
  # return
  return(Out)
}

#*********************
#
# Line_Graph_Generator
#
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
  
  # Individual Wald test and confidence interval for each parameter
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







