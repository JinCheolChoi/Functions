# https://stats.stackexchange.com/questions/207807/95-confidence-interval-for-proportions-in-r
########### Overview of functions here:
# round2 - correctly round2s 10.5 to 11
#GET_PEOPLE_WHO_EVER_HAD_EVENT_1(Data,colsOfInterest.names)
#GETTING.STAT.BY.YEAR_PATIENTS(Data, ColumnsToUse.names, name.of.col.with.year)
#Calculate_NofPeople_withinTwoPeriods<-function(ID_vec,Periods_vec){}
#GEE_Bivariate(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact,levels.of.fact) ## id numeric
#GEE_Multivariable(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact)
#GEE_Backward_QIC(Data, ColumnsToUse, Outcome_name, ID_name, which.family,vector.OF.classes.num.fact)
#GEE_Bivariate_Adjusted_Personal<-function(Data, ColumnsToUse, dependent_var, ID, which.family, vector.OF.classes.num.fact, adj_name){}
#GEE_Confounding_interaction(Data, ColumnsToUse=ColumnsToUse_multi, dependent_var_vec, ID_vec, interaction_var_name1,interaction_var_name2, 
#                                      which.family="binomial",vector.OF.classes.num.fact,levels.of.fact, threshold)
#Incidence_Personal<-function(Data.last, stop_day_name, event_name){}
#Descriptive_stats_Personal(Data, id_name, NameOfMainVariable, ColumnsToUse)
#Descriptive_stats_all_types_of_data_Personal(Baseline_Data)
#Descriptive_stats_all_types_of_data_Personal_stratified(Data, Column_to_use_name, NameOfMainVariable)## NEW !!!
#Stats_StudyPeriod_and_Base<-function(Data, outcome_name, id_name)} #new; for one outcome and one variable at time
#Report_stratified_by_numberofvisits<-function(Data, id_name, outcome){} # Report RQ_0482
#StratifiedTwice<-function(Data, strat1, strat2, variable){}
#GET_Kaplan_Meier_Stats(Data, start_day_name,stop_day_name,event_name,id_name,strata_vector)
#Kaplan_Meier_Personal<-function(Data, id_name, Groups, start_day_name, stop_day_name, event_name){} ## no repeated
#Kaplan_Meier_time_updated<-function(Data, id_name, start_day_name, stop_day_name, event_name,strata_name, NAMES_0_1){} ## 
#Kaplan_Meier_end_event<-function(Data, id_name, stop_day_name, event_name){} ## will create data_last first
#Hazard_Personal<-function(Data, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse){}
#Cox_Confounding_Personal<-function(Data, indep_var_vec, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact){} # works with factors and with var of interest factor
#Cox_Adjusted_Personal<-function(Data, indep_var_vec, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse){}
#Cox_Multi_Personal(Data, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse,vector.OF.classes.num.fact)
#Cox_unadjusted_Personal(Data, id_name, start_day_vector,stop_day_vector,event_vector,ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact)
#Cox_Backward_AIC_Personal<-function(Data, id_name, start_day_vector, stop_day_vector, event_vector, ColumnsToUse, vector.OF.classes.num.fact){} ## Updated with fact ## works correctly
#Cox_bivariate_case.control_Personal<-function(Data, id_name, start_day_vector,stop_day_vector,event_vector,case.control_vector,ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact)
#GLM_Bivariate(Data.Frame,ColumnsToUse, Outcome_name, which.family) 
#GLM_LOGIT_Bivariate_Adjusted_Personal<-function(Data, ColumnsToUse.adj, dependent_var.adj, which.family.adj, adj_name){} # Request 11
#GLM_Multivariate(Data.Frame, ColumnsToUse, Outcome_name, which.family) ## works with age correctly
#GLM_Backward_AIC_Personal(Data, ColumnsToUse, Outcome_name, which.family,vector.OF.classes.num.fact){} ## explanatory; works with age
#GLM_Confounding_Personal(Data, ColumnsToUse, outcome_vec, main_explan_vec, which.family,vector.OF.classes.num.fact,threshold)
#GET_PEOPLE_WHO_EVER_HAD_EVENT_1<-function(Data,ID_name,colsOfInterest.names) # Request 26
#Check_multicollinearity_GLM(Data, dependent_var_name, ColumnsToUse, which.family)
#ORDINARY(Data, ColumnsToUse, Outcome, variable.fact.num, factor.levels) ## not working as function
#LINEAR_REGRESSION(Data,ColumnsToUse, Outcome, variable.fact.num, factor.levels)
#LM_Confounding_Personal<-function(Data, ColumnsToUse, dependent_var, independent_var, vector.OF.classes.num.fact)
#LM_Backward_AIC_Personal(Data, ColumnsToUse, Outcome_vector, vector.OF.classes.num.fact)
#GETTING.STAT.BY.YEAR_OBSERVATIONS(Data, ColumnsToUse.names, name.of.col.with.year)
#GET_STATS(Vector) #get min, max, mean, sd, median and IQR for one vector
#GET_STOP_DATE(Data) # get stop date by int_date
#McNemar<-function(Data,NameOfMainVariable,ColumnsToUse){}
#PROPORTIONS_before_after<-function(Data,NameOfMainVariable,ColumnsToUse) # NameOfMainVariable = event
#DATA_DIAGNOSTICS(Data,ColumnsToUse) ## check N of missing, 1 and 0 for each ColumnsToUse
#GLMM_Bivariate(Data_frame,ColumnsToUse,Outcome_name, ID_name,which.family, NAGQ)
#GLMM_Bivariate_linear(Data_frame,ColumnsToUse,Outcome_name, ID_name)
#GLMM_Bivariate_NB(Data_frame,ColumnsToUse,Outcome_name, ID_name, which.family)
#GLMM_Backward_AIC(Data_frame, ColumnsToUse, Outcome_name, ID_name, which.family,NAGQ)
#GLMM_Bivariate_Adjusted(Data_frame,ColumnsToUse,Columns.to.adj.for,dependent_var_name, ID_name,which.family)
#GLMM_Multivariable(Data_frame,ColumnsToUse,dependent_var_name, ID_name,which.family,NAGQ)
#GLMM_Multivariable_linear(Data_frame,ColumnsToUse,dependent_var_name, ID_name)
#GLMM_Multivariable_NB(Data_frame,ColumnsToUse,Outcome_name, ID_name)
#GLMM_Confounding(Data.F, ColumnsToUse, Outcome_name, ID_name, independent_var_name, which.family,threshold,NAGQ)
#Shift.To.First.1(Data,id.name,MainVar.name)
#Last.event.shifted.to.1(Data,id.name,MainVar.name)
#End.with.first.1(Data,id.name,MainVar.name)
# GLM_OFFSET_Bivariate(Data.Frame, ColumnsToUse, Outcome_name, Offset_name, FAMILY)
# GLM_OFFSET_Multi(Data.Frame, ColumnsToUse, Outcome_name, Offset_name, FAMILY)
# GLM_NB_Bivariate_Personal(Data.Frame, ColumnsToUse, Outcome_name, Offset_name)
# GLM_NB_Multi_Personal(Data.Frame, ColumnsToUse, Outcome_name, Offset_name)
# GLM_zeroinfl_NB_offset_Bivariate_Personal(Data, ColumnsToUse, Outcome_name, Offset_name, dist) # dist= poisson or negbin
# GLM_NB_Multi_Personal(Data.F, ColumnsToUse, Outcome_name, Offset_name)
# GLM_MULTINOMIAL_Bivariate_Personal(Data,ColumnsToUse, Outcome_name, variable.fact.num) # only worked with numeric; check it before using
# REASSIGN_BASELINE<-function(Data, int_date_vec, survey_vec)
# Find_breaking_point_piecewise_long(Data.F,Time_since_start_name,Code_name,Outcome_name,breaking_point_by_eye,intervals_of_search)

############## assign surveys to baseline: hasn't been checked as a function

Assign_survey<-function(Data, int_date_name, int_date_format){
Data[,int_date_name]<-format(as.Date(Data[,int_date_name], format=int_date_format),"%Y-%m-%d")
survey_new<-rep(NA,dim(Data)[1])
for(i in 1:dim(Data)[1]){
  
  date.t<-Data[i,int_date_name] 
  if(date.t>="2005-12-01" & date.t<"2006-06-01"){survey_new[i]<-"0"}
  if(date.t>="2006-06-01" & date.t<"2006-12-01"){survey_new[i]<-"1"}
  if(date.t>="2006-12-01" & date.t<"2007-06-01"){survey_new[i]<-"2"}
  if(date.t>="2007-06-01" & date.t<"2007-12-01"){survey_new[i]<-"3"}
  if(date.t>="2007-12-01" & date.t<"2008-06-01"){survey_new[i]<-"4"}
  if(date.t>="2008-06-01" & date.t<"2008-12-01"){survey_new[i]<-"5"}
  if(date.t>="2008-12-01" & date.t<"2009-06-01"){survey_new[i]<-"6"}
  if(date.t>="2009-06-01" & date.t<"2009-12-01"){survey_new[i]<-"7"}
  if(date.t>="2009-12-01" & date.t<"2010-06-01"){survey_new[i]<-"8"}
  if(date.t>="2010-06-01" & date.t<"2010-12-01"){survey_new[i]<-"9"}
  if(date.t>="2010-12-01" & date.t<"2011-06-01"){survey_new[i]<-"10"}
  if(date.t>="2011-06-01" & date.t<"2011-12-01"){survey_new[i]<-"11"}
  if(date.t>="2011-12-01" & date.t<"2012-06-01"){survey_new[i]<-"12"}
  if(date.t>="2012-06-01" & date.t<"2012-12-01"){survey_new[i]<-"13"}
  if(date.t>="2012-12-01" & date.t<"2013-06-01"){survey_new[i]<-"14"}
  if(date.t>="2013-06-01" & date.t<"2013-12-01"){survey_new[i]<-"15"}
  if(date.t>="2013-12-01" & date.t<"2014-06-01"){survey_new[i]<-"16"}
  if(date.t>="2014-06-01" & date.t<"2014-12-01"){survey_new[i]<-"17"}
  if(date.t>="2014-12-01" & date.t<"2015-06-01"){survey_new[i]<-"18"}
  if(date.t>="2015-06-01" & date.t<"2015-12-01"){survey_new[i]<-"19"}
  if(date.t>="2015-12-01" & date.t<"2016-06-01"){survey_new[i]<-"20"}
  if(date.t>="2016-06-01" & date.t<"2016-12-01"){survey_new[i]<-"21"}
  if(date.t>="2016-12-01" & date.t<"2017-06-01"){survey_new[i]<-"22"}
  if(date.t>="2017-06-01" & date.t<"2017-12-01"){survey_new[i]<-"23"}
  if(date.t>="2017-12-01" & date.t<"2018-06-01"){survey_new[i]<-"24"}
  if(date.t>="2018-06-01" & date.t<"2018-12-01"){survey_new[i]<-"25"}
}
return(survey_new)
}

#########################

GET_perc<-function(Data, col_name){
  TT<-table(Data[,col_name])
  res<-paste(TT," (",round2(TT/sum(TT)*100,1),"%); total known events=",sum(TT),sep="")
  names(res)<-names(TT)
return(res)
}
  
### binomial correlation phi coefficient

# library(psych)
# phi(t, digits = 2)

#### correctly round2s

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Confidence interval
#ci <- cbind(lwr=ctable[,"estimate"]-1.96*ctable[,"san.se"],upr=ctable[,"estimate"]+1.96*ctable[,"san.se"])

#Data[which(is.na(Data[,"CODE"])),"CODE"]<-Data[which(is.na(Data[,"CODE"])),"ARYS_ID"]
#Data<-Data[order(as.numeric(Data[,"SURVEY"])),]
#Data<-Data[order(as.numeric(Data[,"CODE"])),]
#which(is.na(Data[,"CODE"]))

############ below is used for Cox (to check that in case of "1" we remove all 0 following the last "1"; i.e. 00100100-->001001)

Last.event.shifted.to.1<-function(Data,id.name,MainVar.name){
  people<-unique(Data[,id.name])
  Last.event.shifted<-rep(NA,dim(Data)[2])
  for(i in 1:length(people)){
    
    temp<-Data[which(Data[,id.name]==people[i]),]
    if(length(temp)==dim(Data)[2]){Last.event.shifted<-rbind(Last.event.shifted,temp)}else{
      
      ind1<-which(as.numeric(temp[,MainVar.name])==1)
      if(length(ind1)==0){Last.event.shifted<-rbind(Last.event.shifted,temp)}else{
        
        Last.event.shifted<-rbind(Last.event.shifted,temp[1:ind1[length(ind1)],])
        
      }
      
    }
    
  }
  Last.event.shifted<-Last.event.shifted[-1,]
  return(Last.event.shifted)
}

### For those who report Any NAPOU during the study period, please use measures from their first 
#visit during the study period that involved a report of Any NAPOU

Shift.To.First.1<-function(Data,id.name,MainVar.name){
  people<-unique(Data[,id.name])
  Baseline.shifted<-matrix(NA,length(people),dim(Data)[2])
  colnames(Baseline.shifted)<-colnames(Data)
  rownames(Baseline.shifted)<-people
  
  for(i in 1:length(people)){
    
    temp<-Data[which(Data[,id.name]==people[i]),]
    if(length(temp)==dim(Data)[2]){Baseline.shifted[i,]<-temp}else{
      
      ind1<-which(as.numeric(temp[,MainVar.name])==1)
      if(length(ind1)==0){Baseline.shifted[i,]<-temp[1,]}else{
        
        Baseline.shifted[i,]<-temp[ind1[1],]
        
      }
      
    }
    
  }
  
  return(Baseline.shifted)
}


################ for the HCV incidence: collect last observations if all events = 0, or get the first observations = 1

End.with.first.1<-function(Data,id.name,MainVar.name){
  people<-unique(Data[,id.name])
  Last.shifted<-matrix(NA,length(people),dim(Data)[2])
  colnames(Last.shifted)<-colnames(Data)
  rownames(Last.shifted)<-people
  
  for(i in 1:length(people)){
    
    temp<-Data[which(Data[,id.name]==people[i]),]
    if(length(temp)==dim(Data)[2]){Last.shifted[i,]<-temp}else{
      
      ind1<-which(as.numeric(temp[,MainVar.name])==1)
      if(length(ind1)==0){Last.shifted[i,]<-temp[dim(temp)[1],]}else{
        
        Last.shifted[i,]<-temp[ind1[1],]
        
      }
      
    }
    
  }
  
  return(Last.shifted)
}


####### P-value by HR

#Est = log(Hazard[,"HR"])
#Lower = log(Hazard[,"L"])
#Upper = log(Hazard[,"U"])
#SE=(Upper-Lower)/(2*1.96)
#z=abs(Est/SE)
#P_value=exp(-0.717*z-0.416*z^2)

DATA_DIAGNOSTICS<-function(Data,ColumnsToUse){
  res.examine<-matrix(NA,length(ColumnsToUse),3)
  rownames(res.examine)<-ColumnsToUse
  colnames(res.examine)<-c("0","1","N NAS")
  for(i in 1:length(ColumnsToUse)){
    
    temp<-Data[,ColumnsToUse[i]]
    NAS<-length(which(is.na(temp)))
    names(NAS)<-"N NAS"
    cc<-c(table(temp),NAS)
    res.examine[i,]<-cc[match(colnames(res.examine),names(cc))]
  }
  
  Perc.NAs<-round2(res.examine[,"N NAS"]/dim(Data)[1]*100,0)
  res.examine<-cbind(res.examine,Perc.NAs)
  return(res.examine)
}

##############################################################
## calculating N of times in jail

N_of_times_percentages<-function(Data, Name_of_column){
  #Name_of_column - name of column which we're transforming into N of times and %
  people<-unique(as.numeric(Data[,1]))  
  N.in.Jail<-perc.in.Jail<-N.of.FU<-array()
  
  for(i in 1:length(people)){
    
    temp<-Data[as.numeric(Data[,1])==people[i],]
    
    if(length(temp)>dim(Data)[2]){
      N.in.Jail[i]<-sum(as.numeric(temp[,Name_of_column]),na.rm=T)
      perc.in.Jail[i]<-N.in.Jail[i]/dim(temp)[1]
      N.of.FU[i]<-dim(temp)[1]
    }else{
      N.in.Jail[i]<-as.numeric(temp[Name_of_column])
      perc.in.Jail[i]<-N.in.Jail[i]
      N.of.FU[i]<-1
    }
  }
  names(N.in.Jail)<-people
  perc.in.Jail<-round2(perc.in.Jail*100,0)
  Jail.add.info<-cbind(N.in.Jail,perc.in.Jail,N.of.FU)
  colnames(Jail.add.info)<-c(paste(Name_of_column,"_N_times",sep=""),paste(Name_of_column,"_perc_times",sep=""),"N_of_FUs")
  return(Jail.add.info)
}

############################################################

PROPORTIONS_before_after<-function(Data,NameOfMainVariable,ColumnsToUse){

  event.data<-Data[which(as.numeric(Data[,NameOfMainVariable])==1),]
  event.data.id<-paste(event.data[,1],"_",as.numeric(event.data[,"survey"]),sep="")
  event.data[,1]<-event.data.id
  before.data<-Data[(which(as.numeric(Data[,NameOfMainVariable])==1)-1),]
  before.data[,1]<-event.data.id ## to have matched events-id
  Data_before_event<-rbind(event.data,before.data)
  
  RES<-rep(NA, 5)
  
  for(i in 1:length(ColumnsToUse)){
    
    TT<-table(Data_before_event[,ColumnsToUse[i]])
    TT<-TT[match(c(1,0),as.numeric(names(TT)))]
    TT[which(is.na(TT))]<-0
    TT<-paste(TT, " (",round2(TT/dim(Data_before_event)[1]*100,2),")",sep="")
    TT0<-table(Data_before_event[which(as.numeric(Data_before_event[,NameOfMainVariable])==0),ColumnsToUse[i]])
    TT0<-TT0[match(c(1,0),as.numeric(names(TT0)))]
    TT0[which(is.na(TT0))]<-0
    TT0<-paste(TT0, " (",round2(TT0/length(which(as.numeric(Data_before_event[,NameOfMainVariable])==0))*100,2),")",sep="")
    TT1<-table(Data_before_event[which(as.numeric(Data_before_event[,NameOfMainVariable])==1),ColumnsToUse[i]])
    TT1<-TT1[match(c(1,0),as.numeric(names(TT1)))]
    TT1[which(is.na(TT1))]<-0
    TT1<-paste(TT1, " (",round2(TT1/length(which(as.numeric(Data_before_event[,NameOfMainVariable])==1))*100,2),")",sep="")
    RES_temp<-cbind(c(ColumnsToUse[i],""),c(1,0),TT,TT1,TT0)
    RES<-rbind(RES,RES_temp)
  }
  RES<-RES[-1,]
  colnames(RES)<-c("Variable","Value",paste("All, total n=",length(which(as.numeric(Data[,NameOfMainVariable])==1))*2,", N (%)",sep=""),
                   paste(NameOfMainVariable,"=1, total n=",length(which(as.numeric(Data[,NameOfMainVariable])==1)),", N (%)",sep=""),
                   paste(NameOfMainVariable,"=0, total n=",length(which(as.numeric(Data[,NameOfMainVariable])==1)),", N (%)",sep=""))
  return(RES)
}

########## McNemar's test.

McNemar<-function(Data,NameOfMainVariable,ColumnsToUse, ID_name){
  
  # NameOfMainVariable is usually cessation; it's before-after separator
  
  event.data<-Data[which(as.numeric(Data[,NameOfMainVariable])==1),]
  event.data.id<-paste(event.data[,1],"_",as.numeric(event.data[,"survey"]),sep="")
  before.data<-Data[(which(as.numeric(Data[,NameOfMainVariable])==1)-1),]
  if(all(event.data[,ID_name]==before.data[,ID_name])){
    event.data[,1]<-event.data.id
  before.data[,1]<-event.data.id ## to have matched events-id
  Data_before_event<-rbind(event.data,before.data)
  
  RES<-matrix(NA, length(ColumnsToUse),4)
  
  for(i in 1:length(ColumnsToUse)){
    
  after<-Data_before_event[which(as.numeric(Data_before_event[,NameOfMainVariable])==1),]
  before<-Data_before_event[which(as.numeric(Data_before_event[,NameOfMainVariable])==0),]

  Before.yes.after.yes<-which(as.numeric(before[,ColumnsToUse[i]])==1&as.numeric(after[,ColumnsToUse[i]])==1)
  Before.no.after.yes<-which(as.numeric(before[,ColumnsToUse[i]])==0&as.numeric(after[,ColumnsToUse[i]])==1)
  Before.yes.after.no<-which(as.numeric(before[,ColumnsToUse[i]])==1&as.numeric(after[,ColumnsToUse[i]])==0)
  Before.no.after.no<-which(as.numeric(before[,ColumnsToUse[i]])==0&as.numeric(after[,ColumnsToUse[i]])==0)
  
  m <- matrix(c(length(Before.yes.after.yes), length(Before.yes.after.no), 
                length(Before.no.after.yes), length(Before.no.after.no)), ncol=2, byrow=T)
  
  res.temp<-mcnemar.test(m)
  
  before.N<-length(which(as.numeric(before[,ColumnsToUse[i]])==1))
  after.N<-length(which(as.numeric(after[,ColumnsToUse[i]])==1))
  
  RES[i,1]<-paste(before.N," (",round2(before.N/dim(before)[1]*100,2),")",sep="")
  RES[i,2]<-paste(after.N," (",round2(after.N/dim(after)[1]*100,2),")",sep="")
  RES[i,3]<-round2(res.temp$statistic,2)
  RES[i,4]<-round2(res.temp$p.value ,4)
  }

  colnames(RES)<-c("before N (%)","after N (%)","McNemar's chi-squared","p value")
  rownames(RES)<-ColumnsToUse}else{RES<-"ids of controls don't match ids of cases. Order your data better"}
  return(RES)
}


####################################################################################
######## get min, max, mean, sd, median and IQR for one column


GET_STATS=function(Vector){
  Vector<-as.numeric(Vector)
  RES<-c(round2(median(Vector, na.rm=T),2), paste(
    #round2(diff(fivenum(Vector, na.rm=T)[c(2,4)]),2),
    "(",round2(fivenum(Vector, na.rm=T)[2],2),"-",round2(fivenum(Vector, na.rm=T)[4],2),")",sep=""), 
        round2(mean(Vector, na.rm=T),2), round2(sd(Vector, na.rm=T),2), min(Vector, na.rm=T), max(Vector, na.rm=T))
  names(RES)<-c("median", "(IQR range)", "mean", "sd", "min", "max")
  return(RES)
}

####################################################################################
############# GET STOP DATE ########################################################
####################################################################################

GET_STOP_DATE<-function(Data){
  people.unique<-unique(Data[,1])
  stop_date<-rep(NA, dim(Data)[1])  
  for(i in 1:length(people.unique)){
    
    ind<-which(Data[,1]==people.unique[i])
    if(length(ind)>1){
    temp<-Data[ind,]
    date.temp<-as.Date(temp[,"int_date"],"%m/%d/%Y")
    date.diff<-sapply(1:length(date.temp), function(x) date.temp[x]-date.temp[1])
    stop_date[ind]<-date.diff
    }else{
      stop_date[ind]<-0
    }
    }
  return(stop_date)
}

####################################################################################
############# difference between each pair of dates (months)
####################################################################################

#date<-as.Date(Data.temp[,"int_date"],"%m/%d/%Y")
#date.diff<-round2(c(0, sapply(2:length(date), function(x) date[x]-date[x-1]))/30.43,0)
#date.diff[which(duplicated(readData[,"code"])==F)]<-NA
# GET_STATS(date.diff)
#Data.base<-Data[which(duplicated(Data[,1])==F),]
#Data.last<-Data[which(duplicated(Data[,1], fromLast = T)==F),]
#Data.last.matched<-Data.last[match(Data.base[,1], Data.last[,1]),]
#N.of.days<-as.numeric(as.Date(Data.last.matched[,"int_date"],"%m/%d/%Y")-as.Date(Data.base[,"int_date"],"%m/%d/%Y"))
#Person.years<-(sum(N.of.days))/365.25

## time since the first FU
#people<-unique(Data$CODE)
#All.diff<-NA
#for(i in 1:length(people)){
  
#  temp<-subset(Data,CODE==people[i])
#  date.diff<-round2(sapply(1:length(temp$date), function(x) temp$date[x]-temp$date[1])/30.43,0)
 
#  All.diff<-c(All.diff,date.diff)
#}
#All.diff<-All.diff[-1]

####################################################################################
#### OLD Cochran-Armitage trend test
####################################################################################

#library("coin")

#Data.t<-GETTING.STAT.BY.YEAR_PATIENTS(Data, ColumnsToUse.names=ColumnsToUse, name.of.col.with.year="year")
#Data.t<-Data.t[which(rownames(Data.t)=="N"),]
#rownames(Data.t)<-Data.t[,1]
#Data.t<-Data.t[-1,]
#Data.t<-Data.t[,grep(" 1",colnames(Data.t))]
#Data.t[which(is.na(Data.t),arr.ind=T)]<-0
#class(Data.t)<-"numeric"
# Mann-Kendall test
#MannKendall.res<-matrix(NA,5,2)
#rownames(MannKendall.res)<-colnames(Data.t)
#colnames(MannKendall.res)<-c("tau","p-value")
#for(i in 1:5){
#  res <-  MannKendall(Data.t[,i])
#  MannKendall.res[i,1]<-res$tau[1]
#  MannKendall.res[i,2]<-res$sl[1]
#}
#Cochran test
#Matrix.where.cols.are.two.variables<-cbind(Data.t[,"linked 1"],rowSums(Data.t[,which(colnames(Data.t)%in%c("unlinked 1","linked 1")==F)])) # col1=one variable; col2=another variable
#cochran1 <- matrix(as.vector(Matrix.where.cols.are.two.variables),byrow = F, ncol = 2,
#                   dimnames = list("Year"=rownames(Matrix.where.cols.are.two.variables),"Test"=c("linked", "not linked")  ))
#coin::chisq_test(as.table(cochran1),scores = list("Year" = 1:11))

#
#test for trend: prop.trend.test {stats}

########### NEw Cochran - Armitage

#library("coin")

#Data.t<-GETTING.STAT.BY.YEAR_OBSERVATIONS(Data, ColumnsToUse.names, name.of.col.with.year="YEAR")
#Data.t<-Data.t[which(rownames(Data.t)=="N"),]
#rownames(Data.t)<-Data.t[,1]
#Data.t<-Data.t[-1,]

#Data.t1<-Data.t[,grep("_1",colnames(Data.t))]
#Data.t1[which(is.na(Data.t1),arr.ind=T)]<-0

#Data.t0<-Data.t[,grep("_0",colnames(Data.t))]
#Data.t0[which(is.na(Data.t0),arr.ind=T)]<-0

#class(Data.t1)<-class(Data.t0)<-"numeric"

## Cochran test
#i=1
#matrix.N.by.2<-cbind(Data.t1[,i],Data.t0[,i]) 
#cochran1 <- matrix(as.vector(matrix.N.by.2),byrow = F, ncol = 2,
#                   dimnames = list("Year"=rownames(matrix.N.by.2),"Test"=c("1", "0")  ))
#ct<-coin::chisq_test(as.table(cochran1),scores = list("Year" = 1:dim(Data.t1)[1]))
#statistic(ct)^2 # X^2


##### binary variables
# binary.ind<-which(sapply(1:dim(Data)[2], function(x) length(table(Data[,x])))==2)

###

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


############# check vif for collinearity

# Cox
#library(survival)
#library(rms)
#SURV<-Surv(as.numeric(Data[,"start_day"]),as.numeric(Data[,"stop_day"]), as.numeric(Data[,"cess"]))
#vars.all<-paste("as.numeric(",ColumnsToUse_lag,")",sep="")
#fullmod<-as.formula( paste( "SURV ~ cluster(arys_id)+", paste(vars.all, collapse = "+")))
#model.full <- coxph(fullmod, data=as.data.frame(Data))
#(vif(model.full))^2

Check_multicollinearity_GLM=function(Data, dependent_var_name, ColumnsToUse,which.family){
  
  non_missing_Data=na.omit(as.data.frame(Data[,c(dependent_var_name, ColumnsToUse)]))
  
  dependent_var_vec<-as.numeric(as.character(non_missing_Data[,dependent_var_name]))
  
  library(car)
  var<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  fullmod<-as.formula( paste( "dependent_var_vec ~", paste(var, collapse = "+")))
  mylogit<-glm(fullmod, data=non_missing_Data, family=which.family)
  OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))
  VIF<-round2(c(NA,vif(mylogit)),2)
  VIF.squared<-round2(VIF^2,2)
  RES<-cbind(summary(mylogit)$coefficients,OR.CI,VIF,VIF.squared)
  RES<-RES[-1,which(colnames(RES)%in%c("VIF","VIF.squared"))]
  rn<-gsub("as.numeric", "",rownames(RES))
  rn<-gsub("[()]", "",rn)
  rownames(RES)<-rn
  return(RES)
  
}

# Check_multicollinearity_GLM<-function(Data, dependent_var_name, ColumnsToUse,which.family){
#   
#   dependent_var_vec<-as.numeric(Data[,dependent_var_name])
#  
#   library(car)
#   var<-paste("as.numeric(", ColumnsToUse, ")",sep="")
#   fullmod<-as.formula( paste( "dependent_var_vec ~", paste(var, collapse = "+")))
#   mylogit<-glm(fullmod, data=as.data.frame(Data), family=which.family)
#   OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))
#   VIF<-round2(c(NA,vif(mylogit)),2)
#   VIF.squared<-round2(VIF^2,2)
#   RES<-cbind(summary(mylogit)$coefficients,OR.CI,VIF,VIF.squared)
#   RES<-RES[-1,which(colnames(RES)%in%c("VIF","VIF.squared"))]
#   rn<-gsub("as.numeric", "",rownames(RES))
#   rn<-gsub("[()]", "",rn)
#   rownames(RES)<-rn
#   return(RES)
#   
# }

##############################################################################
#### function which takes matrix of events as input and creates a matrix of people
# if a person had event 1 at least once, it will be returned as 1.
# if a person never had an event, it will be returned as 0
## you sohuld have at least 2 colsOfInterest.names;

GET_PEOPLE_WHO_EVER_HAD_EVENT_1<-function(Data,ID_name,colsOfInterest.names){ # first col is ID
  people<-unique(Data[,ID_name])
  Data.people<-matrix(NA,length(people),length(colsOfInterest.names))
  rownames(Data.people)<-people
  
  for(i in 1:length(people)){
    
    ind<-which(Data[,ID_name]%in%people[i])
    temp<-Data[ind,colsOfInterest.names]
    temp
    if(length(ind)==1){
      Data.people[i,]<-temp
      colnames(Data.people)<-names(temp)
    }else{
      class(temp)<-"numeric"
      ind1<-which(temp==1,arr.ind = T)[,2] #new
      Data.people[i,]<-colSums(temp) #new
      if(length(ind1)>0){Data.people[i,ind1]<-1}#new

      #Data.people[i,]<-colSums(temp, na.rm=T)
      #colnames(Data.people)<-colnames(temp)
    }
    
  }
  class(Data.people)<-"numeric"
  Data.people[which(Data.people>0, arr.ind=T)]<-1
  colnames(Data.people)<-colnames(Data[,colsOfInterest.names])
  return(Data.people)
}

####################################################################################
#### Getting stats for multiple columns withing each year; res for patients 
#### ColumnsToUse.names<-c("coercion", "all_treatment")
#### name.of.col.with.year<-"year"

GETTING.STAT.BY.YEAR_PATIENTS<-function(Data, ID_name,ColumnsToUse.names, name.of.col.with.year){
  
  years<-sort(unique(as.numeric(Data[,name.of.col.with.year])))
  
  # all years #
  temp<-Data

  temp.people<-GET_PEOPLE_WHO_EVER_HAD_EVENT_1(temp,ID_name,ColumnsToUse.names)
  RES<-t(Descriptive_stats_all_types_of_data_Personal(temp.people))[-2,]
  colnames(RES)<-paste(RES[1,],colnames(RES))
  RES<-RES[-1,]
  RES<-cbind("All years",RES)
  
  for(i in 1:length(years)){
    
    temp<-Data[which(as.numeric(Data[,name.of.col.with.year])==years[i]),]
    temp.people<-GET_PEOPLE_WHO_EVER_HAD_EVENT_1(temp,ID_name,ColumnsToUse.names)
    res<-t(Descriptive_stats_all_types_of_data_Personal(temp.people))[-2,]
    colnames(res)<-paste(res[1,],colnames(res))
    res<-res[-1,]
    res<-cbind(years[i],res)
    RES<-rbind(RES,res[,match(colnames(RES), colnames(res))])
    
  }
  return(RES)
}

#Data.t<-GETTING.STAT.BY.YEAR_PATIENTS(Data, ColumnsToUse.names=ColumnsToUse, name.of.col.with.year="year")
#Data.t<-Data.t[which(rownames(Data.t)=="%"),]
#rownames(Data.t)<-Data.t[,1]
#Data.t<-Data.t[-1,]
#Data.t<-Data.t[,grep(" 1",colnames(Data.t))]
#Data.t[which(is.na(Data.t),arr.ind=T)]<-0 ## all this - to extract % during each year

####################################################################################
#### Getting stats for multiple columns withing each year; res for observations 
#### ColumnsToUse.names<-c("coercion", "all_treatment")
#### name.of.col.with.year<-"year"

GETTING.STAT.BY.YEAR_OBSERVATIONS<-function(Data, ColumnsToUse.names, name.of.col.with.year){
  
  years<-sort(unique(as.numeric(Data[,name.of.col.with.year])))
  
  # all years #
  temp<-Data
  RES<-t(Descriptive_stats_all_types_of_data_Personal(temp[,ColumnsToUse.names]))[-2,]
  colnames(RES)<-paste(RES[1,],colnames(RES))
  RES<-RES[-1,]
  RES<-cbind("All years",RES)
  
  for(i in 1:length(years)){
    
    temp<-Data[which(as.numeric(Data[,name.of.col.with.year])==years[i]),]
    if(length(temp)>dim(Data)[2]){
    res<-t(Descriptive_stats_all_types_of_data_Personal(temp[,ColumnsToUse.names]))[-2,]
    colnames(res)<-paste(res[1,],colnames(res))
    res<-res[-1,]
    }else{
      temp.temp<-temp[ColumnsToUse.names]
      res.tt<-matrix(NA,2,dim(RES)[2])
      for(j in 1:length(temp.temp)){
      tt<-grep(names(temp.temp)[j],colnames(RES))
      if(as.numeric(temp.temp)[j]==1){
        res.tt[1,tt[3]]<-1
        res.tt[2,tt[3]]<-100}
      if(as.numeric(temp.temp)[j]==0){
        res.tt[1,tt[2]]<-1
        res.tt[2,tt[2]]<-100
        }
      }
      res.tt[1,2]<-res.tt[1,5]<-"not missing n=1"
      res<-res.tt
      colnames(res)<-colnames(RES)
    }
    res<-cbind(years[i],res)
    RES<-rbind(RES,res[,match(colnames(RES), colnames(res))])
    RES[which(is.na(RES),arr.ind=T)]<-0
  }
  return(RES)
}

####################################################################################
#### 1.1. n and % for the number of study visits that report the outcome (over the study period), 

Report_stratified_by_numberofvisits<-function(Data, id_name, outcome_name){
  Nofvisits<-table(Data[,id_name])
  N.of.visits<-Nofvisits[match(Data[,id_name], names(Nofvisits))]
  Data.new<-cbind(Data,N.of.visits)
  unique.N.of.visits<-sort(as.numeric(unique(N.of.visits)))
  Study.period<-matrix(NA,length(unique.N.of.visits), 2)
  rownames(Study.period)<-unique.N.of.visits
  colnames(Study.period)<-c("N of people never reported 1", "N of people reported 1 at least once")
  for(i in 1:length(unique.N.of.visits)){
    
    Data.temp<-Data.new[which(Data.new[,"N.of.visits"]==unique.N.of.visits[i]),]
    study.period<-as.numeric(Data.temp[,outcome_name])
    people1.id<-unique(Data.temp[which(study.period==1),id_name])
    people0.id<-unique(Data.temp[which(Data.temp[,id_name]%in%people1.id==F),id_name])
    TT<-c(length(people0.id),length(people1.id))
    Study.period[i,]<-paste(TT, " (",round2(TT/length(unique(Data.temp[,id_name]))*100,2),")",sep="")
    
  }
  
  Study.period<-cbind(rownames(Study.period),Study.period)
  colnames(Study.period)[1]<-"N of visits"
  return(Study.period)
}

####################################################################################
#### 1.2. the number of observations that report the outcome (at baseline and over study period),  

Stats_StudyPeriod_and_Base<-function(Data, outcome_name, id_name){
  study.period<-as.numeric(Data[,outcome_name])
  TT<-c(length(which(is.na(study.period))),length(which(study.period==0)),length(which(study.period==1)))
  study.period.res<-paste(TT, " (",round2(TT/dim(Data)[1]*100,2),")",sep="")
  Data.base<-Data[which(duplicated(Data[,1])==F),]
  
  #which(duplicated(as.numeric(Data.base[,1])))
  study.base<-as.numeric(Data.base[,outcome_name])
  TT<-c(length(which(is.na(study.base))),length(which(study.base==0)),length(which(study.base==1)))
  study.base.res<-paste(TT, " (",round2(TT/dim(Data)[1]*100,2),")",sep="")
  
  people1.id<-unique(Data[which(study.period==1),id_name])
  people0.id<-unique(Data[which(Data[,id_name]%in%people1.id==F),id_name])
  TT<-c(length(people0.id),length(people1.id))
  n.of.people.over.study.period<-paste(TT, " (",round2(TT/length(unique(Data[,id_name]))*100,2),")",sep="")
  
  RES<-c(study.period.res,study.base.res,n.of.people.over.study.period)
  names(RES)<-c("study period observations, NA","study period observations, 0","study period observations, 1", 
                "baseline, NA","baseline, 0", "baseline, 1",
                "study period people, 0", "study period people, 1")
  return(RES)
}

###########################################################################################
########## Function to calculate N of people who had interview within 2 periods ###########
###########################################################################################
## You will need: 
## ID_vector
## periods_vector
## periods<-cbind(readData[,"code"],readData[,"period"])
## Calculate_NofPeople_withinTwoPeriods(readData[,"code"],readData[,"period"])
## Request 15
##################################

Calculate_NofPeople_withinTwoPeriods<-function(ID_vec,Periods_vec){
  
  periods<-cbind(ID_vec,Periods_vec)  
  people<-unique(ID_vec)
  Period.matrix<-matrix(0,length(people),2)
  colnames(Period.matrix)<-names(table(periods[,2]))
  rownames(Period.matrix)<-people
  for(i in 1:length(people)){
    
    temp<-periods[which(periods[,1]==people[i]),]
    if(length(temp)>2){
      temp.Table<-table(temp[,2])
      Period.matrix[i,]<-temp.Table[match(colnames(Period.matrix),names(temp.Table))]
    }else{
      Period.matrix[i,]<-match(colnames(Period.matrix),temp[2])
    }
    
  }
  M<-c(NA,NA,NA)
  names(M)<-c(colnames(Period.matrix)[2],colnames(Period.matrix)[1],"Both periods")
  M[1]<-length(which(is.na(Period.matrix[,1])==T&is.na(Period.matrix[,2])==F))
  M[2]<-length(which(is.na(Period.matrix[,1])==F&is.na(Period.matrix[,2])==T))
  M[3]<-length(which(is.na(Period.matrix[,1])==F&is.na(Period.matrix[,2])==F))
  Perc<-round2(M/sum(M)*100,2)
  Periods.Res<-paste(M, " (",Perc,")",sep="")
  names(Periods.Res)<-names(M)
  return(Periods.Res)
}

###########################################################################################
########## Function to calculate bivariate GEE ############################################
###########################################################################################
## You will need: 
## dependent_var - dependent variables
## ID
## Data
## Names of columns to use as independent variables
## --> This function is pretty slow unfortunately <--
##################################
## Example: 
## DataHere<-"Z:/Request1 GEE RQ_0413/Data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar14 RQ_0413.csv"))
## Data[,"code"]<-gsub("A","999",Data[,"code"]) ## transforming char ID into numeric for GEE <--- important to make if faster
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(as.numeric(Data[,"code"])),]
## dependent_var<-as.numeric(Data[,"presc_opiads_l6m"])
## ID<-Data[,"code"]
## ColumnsToUse<-c("OLDER_AGE_FST", "white", "homeless_l6m", "heroin_l6m", "obtain10_l6m")
## Test<-GEE_Bivariate_Personal(Data, ColumnsToUse, dependent_var, ID, which.family="binomial")
###########################################################################################

GEE_Bivariate<-function(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact,levels.of.fact){

  Data[,ID_name]<-gsub("A",999,Data[,ID_name])
  dependent_var_vec<-as.numeric(Data[,Outcome_name])
  ID_vec<-as.numeric(Data[,ID_name])
  
  library(geepack)
  library(MESS)
  library(doBy)  
  
  Data.data<-Data[,ColumnsToUse]

  RES<-rep(NA, 7)
  rn<-NA
  for(i in 1:dim(Data.data)[2]){
    
    print(paste(i,ColumnsToUse[i],sep=" "))  
  
    temp<-Data.data[,i]
    if(length(table(temp))>1){
    if(vector.OF.classes.num.fact[i]=="num"){temp<-as.numeric(temp)}
    if(vector.OF.classes.num.fact[i]=="fact"){
      temp<-as.factor(temp)
      temp<-relevel(temp, ref=levels.of.fact[i])
      }
    keep<-which(is.na(temp)==F)
    dependent_var_vec01<-dependent_var_vec[keep]
    temp01<-temp[keep]
    ID_vec01<-ID_vec[keep]
  
      r1<-geeglm(dependent_var_vec01~temp01, id=ID_vec01,  family=which.family, corstr="exchangeable")
      # IndivID_vecual Wald test and confID_vecence interval for each parameter
      est <- esticon(r1, diag(dim(summary(r1)$coefficients)[1]))
      # Odds ratio and confID_vecence intervals
      OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
      nnn<-names(coef(r1))
      nnn<-gsub("temp",ColumnsToUse[i],nnn)
      rownames(OR.CI) <- nnn
      colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
  
      if(dim(est)[1]==2){
        
        RES.temp<-c(unlist(summary(r1)$coefficients[2,]),OR.CI[2,])
        RES<-rbind(RES,RES.temp)
        rn.temp<-colnames(Data.data)[i]
        rn<-c(rn,rn.temp)
        cn<-names(RES.temp)
        }else{
          
        RES.temp<-cbind(summary(r1)$coefficients[-1,],OR.CI[-1,])
        RES<-rbind(RES,RES.temp)
        nnn<-rownames(cbind(summary(r1)$coefficients[-1,],OR.CI[-1,]))
        nnn<-gsub("temp",ColumnsToUse[i],nnn)
        rn.temp<-nnn
        rn<-c(rn,rn.temp)
        cn<-colnames(RES.temp)}
  
 
    }}
    
  RES<-RES[-1,]
  rn<-rn[-1]
  
  rownames(RES)<-rn
  
  colnames(RES)<-cn
  RES.bivariate<-RES

  ## making res nicer

  RES<-RES[,-c(2,3)]
  RES[,c(1:2)]<-sapply(1:2, function(x) RES[,x]<-round2(RES[,x],3))
  RES[,c(3:5)]<-sapply(3:5, function(x) RES[,x]<-round2(RES[,x],2))
  RES[,2]<-format(RES[,2],nsmall=3)
  RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
  
  colnames(RES)<-c("Estimate", "P-value", "OR", "Lower OR", "Upper OR")
  rownames(RES)<-rownames(RES.bivariate)
  
  OR.and.CI<-paste(format(round2(as.numeric(RES[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(RES[,"Lower OR"]),2),nsmall=2)," -",
                   format(round2(as.numeric(RES[,"Upper OR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,OR.and.CI)
  RES<-RES[,c("Estimate","P-value","OR.and.CI")]
  
  
return(RES)
} ## names of people should be numeric

#######################################################################
###########################################################################################
########## Function to calculate confounding GEE with interaction #########################
###########################################################################################


GEE_Confounding_interaction<-function(Data, ColumnsToUse=ColumnsToUse_multi, dependent_var_vec, ID_vec, interaction_var_name1,interaction_var_name2, 
                                      which.family="binomial",vector.OF.classes.num.fact,levels.of.fact, threshold){  # threshold=5
  
  independent_var_vec1<-as.numeric(Data[,interaction_var_name1])
  independent_var_vec2<-as.numeric(Data[,interaction_var_name2])
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
    
    library(geepack)
    library(MESS)
    library(doBy) 
    
    if(length(dependent_var_vec)!=dim(Data)[1]|length(ID_vec)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
      
      names(vector.OF.classes.num.fact)<-ColumnsToUse
      del<-unique(c(which(is.na(Data[,which(colnames(Data)%in%ColumnsToUse)]), arr.ind=T)[,1],which(is.na(independent_var_vec1)|is.na(independent_var_vec2))))
      if(length(del)>0){
        dependent_var_vec01<-dependent_var_vec[-del]
        independent_var_vec1.01<-independent_var_vec1[-del]
        independent_var_vec2.01<-independent_var_vec2[-del]
        Data01<-Data[-del,]
        ID_vec01<-ID_vec[-del]
      }else{
        dependent_var_vec01<-dependent_var_vec
        independent_var_vec1.01<-independent_var_vec1
        independent_var_vec2.01<-independent_var_vec2
        Data01<-Data
        ID_vec01<-ID_vec
      }
      
      vars<-ColumnsToUse
      ColumnsToUse.fact<-vars
      
      vars.factor<-paste("as.factor(",vars,")",sep="")
      vars.numeric<-paste("as.numeric(",vars,")",sep="")
      vars[which(vector.OF.classes.num.fact=="fact")]<-vars.factor[which(vector.OF.classes.num.fact=="fact")]
      vars[which(vector.OF.classes.num.fact=="num")]<-vars.numeric[which(vector.OF.classes.num.fact=="num")]
      ColumnsToUse.fact<-vars
      
      Frame.Data<-as.data.frame(Data01) ## relevel factors
      if(length(which(vector.OF.classes.num.fact=="fact"))>0){
        factors.used.ind<-which(vector.OF.classes.num.fact=="fact")
        for(i in 1:length(factors.used.ind)){
          Frame.Data[,ColumnsToUse[factors.used.ind[i]]]<-relevel(Frame.Data[,ColumnsToUse[factors.used.ind[i]]],ref=levels.of.fact[factors.used.ind[i]])
        }
      }
      
      fullmod<-as.formula( paste( "dependent_var_vec01 ~ independent_var_vec1.01*independent_var_vec2.01+", paste(vars, collapse = "+")))
      
      model.full<-geeglm(fullmod, data=Frame.Data, id=ID_vec01, family=which.family, corstr="exchangeable")
      
      SS<-summary(model.full)$coeff
      coeff.indep.ind1<-grep("independent_var_vec1.01", rownames(SS))
      coeff.indep.ind2<-grep("independent_var_vec2.01", rownames(SS))
      coeff.indep.ind12<-grep("independent_var_vec1.01:independent_var_vec2.01", rownames(SS))
      coeff.indep.ind<-unique(c(coeff.indep.ind1,coeff.indep.ind2,coeff.indep.ind12))
      MAX.coeff<-which.max(abs(SS[coeff.indep.ind,1])) ## when indep_var is factor, we pick max coef of its levels
      names(MAX.coeff)<-rownames(SS)[coeff.indep.ind[MAX.coeff]]
      MAX.coeff.name<-names(MAX.coeff) ## leading factor
      COEF.full<-SS[MAX.coeff.name,1]
      
      COEF.to.save<-matrix(NA,1,3)
      COEF.to.save[1,]<-c(COEF.full,0,0)
      colnames(COEF.to.save)<-c("Estimate", "Delta", "abs Delta")
      removed.variables<-"full model"
      
      #### removing variables
      
      while(COEF.to.save[dim(COEF.to.save)[1],3]<=threshold){
        COEF.updated<-matrix(NA, length(vars), 3)
        rownames(COEF.updated)<-vars
        for (j in 1:length(vars)){
          
          vars.updated<-vars[-j]
          temp.model.formula<-as.formula( paste( "dependent_var_vec01 ~ independent_var_vec1.01*independent_var_vec2.01+", paste(vars.updated, collapse = "+")))
          model.updated <- geeglm(temp.model.formula, data=Frame.Data,id=ID_vec01, family=which.family, corstr="exchangeable")
          SS<-summary(model.updated)$coeff
          COEF.updated[j,1]<-SS[MAX.coeff.name,1]
          
          print(length(vars)-j)  
        }
        Delta<-(COEF.updated[,1]/COEF.full[1]-1)*100
        COEF.updated<-cbind(COEF.updated[,1],Delta, abs(Delta))
        remove.this<-which.min(abs(Delta))
        
        COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
        removed.variables<-c(removed.variables,names(remove.this))
        ## updating
        
        vars.save<-vars
        vars<-vars[-remove.this]
        
        
        fullmod<-as.formula( paste( "dependent_var_vec01 ~independent_var_vec1.01*independent_var_vec2.01+", paste(vars, collapse = "+")))
        model.full <- geeglm(fullmod, data=Frame.Data,id=ID_vec01, family=which.family, corstr="exchangeable")
        SS<-summary(model.full)$coeff
        COEF.full<-SS[MAX.coeff.name,1]
        
        COEF.to.save
      }
      
      rownames(COEF.to.save)<-removed.variables
      
      ind<-grep("age",vars.save)
      vars.save.names<-sapply(1:length(vars.save), function(x) strsplit(vars.save[x],"\\(")[[1]][2])
      vars.save.names<-sapply(1:length(vars.save.names), function(x) strsplit(vars.save.names[x],"\\)")[[1]][1])
      temp01.data<-Data01[,vars.save.names]
      if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
      if(length(ind)==1){
        age_vec<-as.numeric(temp01.data[,grep("age",colnames(temp01.data))])
        fullmod<-as.formula( paste( "dependent_var_vec01 ~ independent_var_vec1.01*independent_var_vec2.01+age_vec+", paste(vars.save[-ind],collapse = "+")))
        
      }else{
        fullmod<-as.formula( paste( "dependent_var_vec01 ~independent_var_vec1.01*independent_var_vec2.01+", paste(vars.save, collapse = "+")))
      }
      
      model.full <- geeglm(fullmod, data=Frame.Data,id=ID_vec01, family=which.family, corstr="exchangeable")  
      
      # IndivID_vecual Wald test and confID_vecence interval for each parameter
      est <- esticon(model.full, diag(dim(summary(model.full)$coefficients)[1]))
      # Odds ratio and confID_vecence intervals
      OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
      rownames(OR.CI) <- names(coef(model.full))
      colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
      
      
      final.result<-cbind(as.matrix(summary(model.full)$coeff)[-1,c(1,4)],OR.CI[-1,])
      Procedure<-c(colnames(COEF.to.save),"","")
      final.result[,1:2]<-sapply(1:2, function(x) round2(final.result[,x],4))
      final.result[,3:5]<-sapply(3:5, function(x) round2(final.result[,x],2))
      COEF.to.save[,1:3]<-sapply(1:3, function(x) round2(COEF.to.save[,x],3))
      COEF.to.save<-rbind(final.result,"",Procedure,cbind(COEF.to.save,"",""))
      
      
      rownames(COEF.to.save)<-gsub("as.numeric","",rownames(COEF.to.save))
      rownames(COEF.to.save)<-gsub("as.factor","",rownames(COEF.to.save))
      rownames(COEF.to.save)<-gsub("\\(","",rownames(COEF.to.save))
      rownames(COEF.to.save)<-gsub(")","",rownames(COEF.to.save))
      
      COEF.to.save[,2]<-gsub("as.numeric","",COEF.to.save[,2])
      COEF.to.save[,2]<-gsub("as.factor","",COEF.to.save[,2])
      COEF.to.save[,2]<-gsub("\\(","",COEF.to.save[,2])
      COEF.to.save[,2]<-gsub(")","",COEF.to.save[,2])
      
      rn<-rownames(COEF.to.save)
      rn<-gsub("independent_var_vec1.01",interaction_var_name1,rn)
      rn<-gsub("independent_var_vec2.01",interaction_var_name2,rn)
      
      rownames(COEF.to.save)<-rn
      return(COEF.to.save)}
    
  }
}

###########################################################################################
########## Function to calculate bivariate adjusted GEE ###################################
###########################################################################################

###########################################################################################

GEE_Bivariate_Adjusted_Personal<-function(Data, ColumnsToUse, dependent_var, ID, which.family, vector.OF.classes.num.fact, adj_name){
  
  library(geepack)
  library(MESS)
  library(doBy)  
  
  if(length(dependent_var)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  Data.data<-Data[,match(ColumnsToUse, colnames(Data))]
  adj_vector<-as.factor(Data[,adj_name]) ## <--- factor
  
  RES<-matrix(NA, dim(Data.data)[2],7) 
  for(i in 1:dim(Data.data)[2]){
    
    temp<-Data.data[,i]
    if(vector.OF.classes.num.fact[i]=="num"){temp<-as.numeric(temp)}
    if(vector.OF.classes.num.fact[i]=="fact"){temp<-as.factor(temp)}
    keep<-which(is.na(temp)==F)
    dependent_var01<-dependent_var[keep]
    temp01<-temp[keep]
    ID01<-ID[keep]
    adj_vector01<-as.numeric(adj_vector[keep])
    
    r1<-geeglm(dependent_var01~temp01+adj_vector01, id=ID01,  family=which.family, corstr="exchangeable")
    # Individual Wald test and confidence interval for each parameter
    est <- esticon(r1, diag(dim(summary(r1)$coefficients)[1]))
    # Odds ratio and confidence intervals
    OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
    rownames(OR.CI) <- names(coef(r1))
    colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
    
    res.temp<-as.matrix(cbind(summary(r1)$coefficients,OR.CI))
    RES[i,]<-res.temp[grep("temp",rownames(res.temp)),]
    rn<-colnames(res.temp)
    print(dim(Data.data)[2]-i)
    
  }
  
  rownames(RES)<-colnames(Data.data)
  colnames(RES)<-rn
  RES.bivariate<-RES
  
  ## making res nicer
  
  RES<-RES[,-c(2,3)]
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  RES[which(RES[,2]<0.0001),2]<-"<0.0001"
  colnames(RES)<-c("Estimate", "P-value", "OR", "Lower OR", "Upper OR")
  
  OR.for.paper<-paste(round2(as.numeric(RES[,"OR"]),2)," (",
                      round2(as.numeric(RES[,"Lower OR"]),2),"-",round2(as.numeric(RES[,"Upper OR"]),2),")",sep="")
  RES<-cbind(RES,OR.for.paper)
  
  return(RES)}
} ## names of people should be numeric

###########################################################################################
########## Function to calculate multivariate GEE #########################################
###########################################################################################
## You will need: 
## dependent_var - dependent variables
## ID
## Data
## Names of columns to use as independent variables
## --> This function is pretty slow unfortunately <--
##################################
## Example: 
## DataHere<-"Z:/Request1 GEE RQ_0413/Data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar14 RQ_0413.csv"))
## Data[,"code"]<-gsub("A","999",Data[,"code"]) ## transforming char ID into numeric for GEE <--- important to make if faster
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(as.numeric(Data[,"code"])),]
## dependent_var<-as.numeric(Data[,"presc_opiads_l6m"])
## ID<-Data[,"code"]
## ColumnsToUse<-c("OLDER_AGE_FST", "white", "homeless_l6m", "heroin_l6m", "obtain10_l6m")
## Test<-GEE_Multivariate_Personal(Data, ColumnsToUse, dependent_var, ID,which.family, vector.OF.classes.num.fact)
###########################################################################################

GEE_Multivariable<-function(Data, ColumnsToUse, Outcome_name, ID_name, which.family, vector.OF.classes.num.fact){ ## names of people should be numeric

  library(geepack)
  library(MESS)
  library(doBy)    
  
  Data[,ID_name]<-gsub("A",9999,Data[,ID_name])
  
  #vars<-ColumnsToUse
  #vars.factor<-paste("as.factor(",vars,")",sep="")
  #vars.numeric<-paste("as.numeric(",vars,")",sep="")
  #vars[which(vector.OF.classes.num.fact=="fact")]<-vars.factor[which(vector.OF.classes.num.fact=="fact")]
  #vars[which(vector.OF.classes.num.fact=="num")]<-vars.numeric[which(vector.OF.classes.num.fact=="num")]
  #ColumnsToUse.fact<-vars
  
  del<-unique(which(is.na(Data[,c(Outcome_name,ColumnsToUse)]), arr.ind=T)[,1])
  if(length(del)>0){
   temp01.data<-Data[-del,]  }else{  temp01.data<-Data  }

  ## new data frames, age problems solved
  
  Data.num<-temp01.data[,c(ID_name,Outcome_name,ColumnsToUse[which(vector.OF.classes.num.fact=="num")])]
  class(Data.num)<-"numeric"
  Data.Frame<-as.data.frame(Data.num)
  
  if(length(which(vector.OF.classes.num.fact=="fact"))>0){
    Data.fact<-temp01.data[,ColumnsToUse[which(vector.OF.classes.num.fact=="fact")]]
    Data.fact.Frame<-as.data.frame(Data.fact)
    colnames(Data.fact.Frame)<-ColumnsToUse[which(vector.OF.classes.num.fact=="fact")]
    Data.Frame<-cbind(Data.Frame,Data.fact.Frame)
  }
  
  ## new stopped 
  
  fullmod<-as.formula( paste( Outcome_name, " ~ ", paste(ColumnsToUse, collapse = "+")))
  r1<-geeglm(fullmod, data=Data.Frame, id=Data.Frame[,ID_name], family=which.family, corstr="exchangeable")

  # Individual Wald test and confidence interval for each parameter
  est <- esticon(r1, diag(dim(summary(r1)$coefficients)[1]))

  # Odds ratio and confidence intervals
  OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
  rownames(OR.CI) <- names(coef(r1))
  colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")

  RES<-cbind(as.matrix(summary(r1)$coefficients),OR.CI)
  rn<-rownames(RES)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(RES)<-rn

  ## making res nicer

  RES<-RES[-1,-c(2,3)]
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  RES[,2]<-format(RES[,2],nsmall=3)
  RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
  colnames(RES)<-c("Estimate", "P-value", "OR", "Lower OR", "Upper OR")
  
  OR.and.CI<-paste(format(round2(as.numeric(RES[,"OR"]),2),nsmall=2)," (",
                      format(round2(as.numeric(RES[,"Lower OR"]),2),nsmall=2)," - ",
                   format(round2(as.numeric(RES[,"Upper OR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,OR.and.CI)
  RES<-RES[,c(1,2,6)]
  
  return(RES)
} ## names of people should be numeric

###########################################################################################
########## Function to calculate backward GEE model selection based on QIC ################
###########################################################################################

GEE_Backward_QIC<-function(Data, ColumnsToUse, Outcome_name, ID_name, which.family,vector.OF.classes.num.fact){ ## names of people should be numeric
  
  Data[,ID_name]<-gsub("A",999,Data[,ID_name])
  
  ## check if some columns have a similar name
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
  
  
  library(geepack)
  library(MESS)
  library(doBy) 
  

  names(vector.OF.classes.num.fact)<-ColumnsToUse
  
  del<-unique(which(is.na(Data[,c(Outcome_name,ColumnsToUse)]), arr.ind=T)[,1])
  if(length(del)>0){
    temp01.data<-Data[-del,]  }else{  temp01.data<-Data  }
  
  ## new data frames, age problems solved
  
  Data.num<-temp01.data[,c(ID_name,Outcome_name,ColumnsToUse[which(vector.OF.classes.num.fact=="num")])]
  class(Data.num)<-"numeric"
  Data.Frame<-as.data.frame(Data.num)
  
  if(length(which(vector.OF.classes.num.fact=="fact"))>0){
    Data.fact<-temp01.data[,ColumnsToUse[which(vector.OF.classes.num.fact=="fact")]]
    Data.fact.Frame<-as.data.frame(Data.fact)
    colnames(Data.fact.Frame)<-ColumnsToUse[which(vector.OF.classes.num.fact=="fact")]
    Data.Frame<-cbind(Data.Frame,Data.fact.Frame)
  }

  vars<-ColumnsToUse
  vars.ALL<-ColumnsToUse
  
  QIC.RES<-REMOVE.RES<-COL.NAMES<-array()
  QIC.RES[1]<-NA
  QIC.RES_p<-NA

  ## while(QIC.RES[i]<QIC.RES[i-1]){
  for(i in 1:(length(vars)-1)){
    fullmod<-as.formula( paste( Outcome_name, " ~ ", paste(vars, collapse = "+")))
    r1<-geeglm(fullmod, data=Data.Frame,id=Data.Frame[,ID_name], family=which.family, corstr="exchangeable")
    QIC.RES[i+1]<-QIC(r1)[1]
    remove.this.id.name<-names(which(as.matrix(summary(r1)$coefficients)[-1,4]==max(as.matrix(summary(r1)$coefficients)[-1,4])))
    remove.this.id.name<-remove.this.id.name[1]
    QIC.RES_p<-c(QIC.RES_p,round2(max(as.matrix(summary(r1)$coefficients)[-1,4]),2))
    remove.this.id.name.clean<-ColumnsToUse[sapply(1:length(ColumnsToUse), function(x) grepl(ColumnsToUse[x],remove.this.id.name))]
    
    remove.this.id<-sapply(1:length(vars), function(x) grepl(remove.this.id.name.clean,vars[x]))
    
    REMOVE.RES[i]<-vars[remove.this.id]
    vars<- setdiff(vars, vars[remove.this.id])
    #i=i+1
    print(i)
    print(QIC.RES)
    print(REMOVE.RES)
  }
  QIC.RES<-QIC.RES[-1]
  QIC.RES_p<-QIC.RES_p[-1]
  REMOVE.RES<-REMOVE.RES

  RES<-cbind(QIC.RES, QIC.RES_p, REMOVE.RES)
  colnames(RES)<-c("QIC Before removal","p Before removal", "Removed variable")
  RES.QIC<-RES
  
  Top<-(which.min(as.numeric(RES[,"QIC Before removal"]))-1)
  if(Top==0){vars.keep<-vars.ALL}else{
  vars.keep<-vars.ALL[which(vars.ALL%in%(RES[1:Top,"Removed variable"])==F)]
  }
  
  RES.QIC[,"Removed variable"]<-gsub("as.factor","",RES.QIC[,"Removed variable"])
  RES.QIC[,"Removed variable"]<-gsub("as.numeric","",RES.QIC[,"Removed variable"])
  RES.QIC[,"Removed variable"]<-gsub(")1","",RES.QIC[,"Removed variable"])
  RES.QIC[,"Removed variable"]<-gsub(") 1","",RES.QIC[,"Removed variable"])
  RES.QIC[,"Removed variable"]<-gsub("[()]","",RES.QIC[,"Removed variable"])
  ############################### Final model #################################

  vector.OF.classes.num.fact.keep<-vector.OF.classes.num.fact[match(vars.keep,names(vector.OF.classes.num.fact))]
  fullmod<-as.formula( paste( Outcome_name, " ~", paste(vars.keep, collapse = "+")))
  
  r1<-geeglm(fullmod, data=Data.Frame, id=Data.Frame[,ID_name], family=which.family, corstr="exchangeable")
  # Individual Wald test and confidence interval for each parameter
  est <- esticon(r1, diag(dim(summary(r1)$coefficients)[1]))
  # Odds ratio and confidence intervals
  OR.CI <- round2(exp(cbind(est$Estimate, est$Lower, est$Upper)),2)
  rownames(OR.CI) <- names(coef(r1))
  colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")

  RES<-as.matrix(summary(r1)$coefficients)
  rownames(RES)<-gsub("as.factor","",rownames(RES))
  rownames(RES)<-gsub("as.numeric","",rownames(RES))
  rownames(RES)<-gsub(")1","",rownames(RES))
  rownames(RES)<-gsub("[()]","",rownames(RES))

  ## making res nicer

  RES<-RES[-1,-c(2,3)]
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  RES[,2]<-format(RES[,2],nsmall=3)
  RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
  #options(scipen=999)
  OR.CI<-OR.CI[-1,]
  OR.and.CI<-paste(format(round2(as.numeric(OR.CI[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(OR.CI[,"Lower OR"]),2),nsmall=2)," - ",
                   format(round2(as.numeric(OR.CI[,"Upper OR"]),2),nsmall=2),")",sep="")
  
  RES<-cbind(RES,OR.and.CI)
  RES<-rbind(RES, "",c("QIC = ", round2(QIC(r1)[1],0), ""))
  colnames(RES)<-c("Estimate", "P-value", "OR.and.CI")

  RES.QIC[,1]<-round2(as.numeric(RES.QIC[,1]),2)
  RES<-rbind(RES, "",c("Removed tests and corresponding QIC","", ""),
             c(colnames(RES.QIC)),cbind(RES.QIC))
  #RES<-RES[which(rownames(RES)!="Intercept"),]
  
  return(RES)  
  }
} ## names of people should be numeric

###########################################################################################
############### GLM CONFOUNDING AIC LOGIT #################################################
###########################################################################################
## Data=as.matrix(dataRead)
## dependent_var<-as.numeric(Data[,"BEFORE_IDRUGS"])
## ColumnsToUse<-c("heroin_l6m","obtain_from_doctor","EVER_SECC")
## vector.OF.classes.num.fact=rep("fact",length(ColumnsToUse))
## vector.OF.classes.num.fact[which(ColumnsToUse=="age")]<-"num"
## which.family="binomial"
## independent_var=as.factor(Data[,"OLDER_AGE"])
###########################################################################################

GEE_Confounding<-function(Data.F, ColumnsToUse, Outcome_name, ID_name, independent_var_name, which.family,threshold){  # threshold=5
  
  library(geepack)
  library(MESS)
  library(doBy) 
  
  ColumnsToUse.initial<-ColumnsToUse
  
  Data.F[,ID_name]<-gsub("A",9999,Data.F[,ID_name])
  vars<-ColumnsToUse

  not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse)]),arr.ind=T)[,1])==F) #getting not NA
  Data.F_full<-subset(Data.F,not.NA.id)
  fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars, collapse = "+")))
  model.full<-geeglm(fullmod, id=Data.F_full[,ID_name], data=Data.F_full, family=which.family, corstr="exchangeable")
  
  SS<-summary(model.full)$coeff
  coeff.indep.ind<-grep(independent_var_name, rownames(SS))
  MAX.coeff<-which.max(abs(SS[coeff.indep.ind,1])) ## when indep_var is factor, we pick max coef of its levels
  names(MAX.coeff)<-rownames(SS)[coeff.indep.ind[MAX.coeff]]
  MAX.coeff.name<-names(MAX.coeff) ## leading factor
  COEF.full<-SS[MAX.coeff.name,1]

  COEF.to.save<-matrix(NA,1,3)
  COEF.to.save[1,]<-c(COEF.full,0,0)
  colnames(COEF.to.save)<-c("Estimate", "Delta", "abs Delta")
  removed.variables<-"full model"
  
  #### removing variables
  
  while(COEF.to.save[dim(COEF.to.save)[1],3]<=threshold){
    COEF.updated<-matrix(NA, length(vars), 3)
    rownames(COEF.updated)<-vars
    for (j in 1:length(vars)){
      
      vars.updated<-vars[-j]
      
      ColumnsToUse_temp<-ColumnsToUse[-j]
      not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse_temp)]),arr.ind=T)[,1])==F) #getting not NA
      Data.F_temp<-subset(Data.F,not.NA.id)
      temp.model.formula<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars.updated, collapse = "+")))
      model.updated <- geeglm(temp.model.formula, data=Data.F_temp,id=Data.F_temp[,ID_name], family=which.family, corstr="exchangeable")
      SS<-summary(model.updated)$coeff
      COEF.updated[j,1]<-SS[MAX.coeff.name,1]
      
      print(length(vars)-j)  
    }
    Delta<-(COEF.updated[,1]/COEF.full[1]-1)*100
    COEF.updated<-cbind(COEF.updated[,1],Delta, abs(Delta))
    remove.this<-which.min(abs(Delta))
    
    COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
    removed.variables<-c(removed.variables,names(remove.this))
    
    ## updating
    
    vars.save<-vars
    ColumnsToUse.save<-ColumnsToUse
    
    vars<-vars[-remove.this]
    ColumnsToUse<-ColumnsToUse[-remove.this]
    
    not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse)]),arr.ind=T)[,1])==F) #getting not NA
    Data.F_next<-subset(Data.F,not.NA.id)
    fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars, collapse = "+")))
    model.full <- geeglm(fullmod, data=Data.F_next,id=Data.F_next[,ID_name], family=which.family, corstr="exchangeable")
    SS<-summary(model.full)$coeff
    COEF.full<-SS[MAX.coeff.name,1]
    
    COEF.to.save
  }
  
  rownames(COEF.to.save)<-removed.variables
  
  not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse.save)]),arr.ind=T)[,1])==F) #getting not NA
  Data.F_final<-subset(Data.F,not.NA.id)
  fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars.save, collapse = "+")))
  model.full <- geeglm(fullmod, data=Data.F_final,id=Data.F_final[,ID_name], family=which.family, corstr="exchangeable")
  # IndivID_vecual Wald test and confID_vecence interval for each parameter
  est <- esticon(model.full, diag(dim(summary(model.full)$coefficients)[1]))
  # Odds ratio and confID_vecence intervals
  OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
  rownames(OR.CI) <- names(coef(model.full))
  colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
  

  final.result<-cbind(as.matrix(summary(model.full)$coeff)[-1,c(1,4)],OR.CI[-1,])
  Procedure<-c(colnames(COEF.to.save))
  final.result[,1:2]<-sapply(1:2, function(x) round2(final.result[,x],3))
  final.result[,3:5]<-sapply(3:5, function(x) round2(final.result[,x],2))
  final.result[,2]<-format(final.result[,2],nsmall=3)
  final.result[which(as.numeric(final.result[,2])<0.001),2]<-"<0.001"
  
  OR.and.CI<-paste(format(round2(as.numeric(final.result[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(final.result[,"Lower OR"]),2),nsmall=2),"-",
                   format(round2(as.numeric(final.result[,"Upper OR"]),2),nsmall=2),")",sep="")
  final.result<-cbind(final.result[,c(1:2)],OR.and.CI)
  
  COEF.to.save[,1:3]<-sapply(1:3, function(x) round2(COEF.to.save[,x],3))
  COEF.to.save<-rbind(final.result,"",Procedure,COEF.to.save)
  
  colnames(COEF.to.save)<-c("Estimate","P-value","OR.and.CI")
  
  return(COEF.to.save)
  
  
}


###########################################################################################
########## Function to calculate Incidence ################################################
###########################################################################################
## You will need: 
## stop_day_name - name of column with stop_day (N of days passed from first FU)
## event_name - name of column with events (0 - control; 1 - case)
## Data.last <- dataset containing only last FUs
############# Example:
## DataHere<-"Z:/Request 2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar18 RQ_0438.csv"))
## Data.last<-Data[which(duplicated(Data[,1], fromLast=T)==F),]
## stop_day_name<-"stop_day"
## event_name<-"hcv_event"
## Temp<-Incidence_Personal(Data.last, stop_day_name, event_name)
###########################################################################################

Incidence_Personal<-function(Data.last, stop_day_name, event_name){

  library(epitools)
  ## Data.last<-Data[which(duplicated(Data[,1], fromLast=T)==F),]
  stop_day<-as.numeric(Data.last[,stop_day_name])
  event<-as.numeric(Data.last[,event_name])

  stop_day_cases<-as.numeric(stop_day[which(event==1)])
  stop_day_controls<-as.numeric(stop_day[which(event==0)])
  Person.years<-sum(as.numeric(stop_day),na.rm=T)/365.25 ## changed that
  N.people.converted<-length(stop_day_cases)
  Incidence<-pois.exact(N.people.converted, pt = Person.years, conf.level = 0.95)

  RES<-rbind(cbind(c("N of events", "Person-time risk (years)", "Incidence rate x 100 years", "CI 95% lower", "CI 95% upper"),
                 round2(t(Incidence[-length(Incidence)]),4)))
  RES<-rbind(c("Person-years",Person.years),RES)
  
  RES<-RES[-1,]
  RES[2,2]<-round2(as.numeric(RES[2,2]),0)
  RES[3:5,2]<-round2(as.numeric(RES[3:5,2])*100,1)

  return(RES)
}

###########################################################################################
########## Function to calculate Incidence for repeated events ############################
###########################################################################################
## You will need: 
## stop_day_name - name of column with stop_day (N of days passed from first FU)
## event_name - name of column with events (0 - control; 1 - case)
## Data <- dataset containing all FUs
############# Example:
## DataHere<-"Z:/Request 4 RQ_0431 Relapse/data/"
## Results<-"Z:/Request 4 RQ_0431 Relapse/result/"
## setwd(DataHere)
## dataRead<-read.csv("final_2016apr11 RQ_0431.csv")
## Incidence_Repeated_Personal(Data=dataRead, stop_day_name="stop_day", event_name="relapse")

###########################################################################################

Incidence_Repeated_Personal<-function(Data, stop_day_name, event_name){
  library(epitools)
  Data.last<-Data[which(duplicated(Data[,1], fromLast=T)==F),]
  stop_day<-as.numeric(Data.last[,stop_day_name]) #total N of years in the study
  event<-as.numeric(Data[,event_name]) ## N of all events
  
  Person.years<-sum(stop_day)/365.25
  N.people.converted<-length(which(event==1))
  Incidence<-pois.exact(N.people.converted, pt = Person.years, conf.level = 0.95)
  
  RES<-rbind(cbind(c("N of events converted", "Person-time risk (years)", "Incidence rate", "CI 95% lower", "CI 95% upper"),
                   round2(t(Incidence[-length(Incidence)]),2)))
  return(RES)
}

###########################################################################################
########## Function to calculate Cox unadjusted ###########################################
###########################################################################################
## You will need: 
# id_name - column with patients ids
# dependent_var_name - column with dependent var
# start_day_name - previous stop
# stop_day_name - N of days from first FU to recent FU
# event_name - column with event ("death")
############# Example:
## DataHere<-"Z:/Request 2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar18 RQ_0438.csv"))
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,1]),]
## id_vector<-as.factor(Data[,"id"])
## independent_var_vector<-as.factor(Data[,"NB_DTES_l6m"])
## start_day_vector<-as.numeric(Data[,"start_day"])
## stop_day_vector<-as.numeric(Data[,"stop_day"])
## event_vector<-as.numeric(Data[,"hcv_event"])
## levels.of.fact - should be NA for numeric and "some ref level" for factors
###########################################################################################

Cox_unadjusted_Personal<-function(Data, id_name, start_day_vector,stop_day_vector,event_vector,ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact){
  
  
  if(length(start_day_vector)!=dim(Data)[1]|length(event_vector)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  event_vector<-as.numeric(event_vector)
  id_vector=Data[,id_name]
  
  library("survival")
  
  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  var<-var.fact
  var[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]

  SURV<-Surv(start_day_vector,stop_day_vector, event_vector)
  
  FINAL<-matrix(NA, 1, 8)
  rn=0
  for(i in 1:length(ColumnsToUse)){
 
    Frame.Data<-as.data.frame(Data)
    if(vector.OF.classes.num.fact[i]=="fact"){
      fact.temp<-as.factor(Data[,ColumnsToUse[i]])
      fact.temp<-relevel(fact.temp, ref=levels.of.fact[i])
      model.unadj <- coxph(SURV~cluster(id_vector)+fact.temp)
    }
    if(vector.OF.classes.num.fact[i]=="num"){
      model.unadj <- coxph(SURV~cluster(id_vector)+as.numeric(Data[,ColumnsToUse[i]]))
    }
    
    COEF<-summary(model.unadj)$coeff
    CI<-confint(model.unadj, level = 0.95)
    Hazard<-summary(model.unadj)$conf.int
    RES<-cbind(as.matrix(COEF), as.matrix(CI), as.matrix(Hazard))
    colnames(RES)<-c(colnames(COEF), colnames(CI), colnames(Hazard))
  
    RES<-RES[,match(c("coef","2.5 %","97.5 %","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),colnames(RES))]
  
    FINAL<-rbind(FINAL,RES)
    if(is.matrix(RES)==F){
      rn<-c(rn,ColumnsToUse[i])
      cn=names(RES)}else{
      rn<-c(rn,rownames(RES))
      cn=colnames(RES)
    }

  }
  colnames(FINAL)<-cn
  rownames(FINAL)<-rn
  FINAL<-FINAL[-1,]
  
  colnames(FINAL)[which(colnames(FINAL)=="Pr(>|z|)")]<-"p-value"
  colnames(FINAL)[which(colnames(FINAL)=="exp(coef)")]<-"HR"
  colnames(FINAL)[which(colnames(FINAL)=="lower .95")]<-"HR lower 95% CI"
  colnames(FINAL)[which(colnames(FINAL)=="upper .95")]<-"HR upper 95% CI"
  FINAL<-FINAL[,-c(1:4)]
  FINAL[,1]<-round2(FINAL[,1],4)
  FINAL[,2]<-round2(FINAL[,2],2)
  FINAL[,3]<-round2(FINAL[,3],2)
  FINAL[,4]<-round2(FINAL[,4],2)

  return(FINAL)
  }
  
}

##  local.coxph.test(coxph.fit, 2:4)

###########################################################################################
####################### Cox bivariate comparing case.control i.e 2 groups #################
###########################################################################################

Cox_bivariate_case.control_Personal<-function(Data, id_name, start_day_vector,stop_day_vector,event_vector,case.control_vector,ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact){
  
  if(length(start_day_vector)!=dim(Data)[1]|length(event_vector)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  event_vector<-as.numeric(event_vector)
  id_vector=Data[,id_name]
  
  library("survival")
  
  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  var<-var.fact
  var[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]
  
  SURV<-Surv(start_day_vector,stop_day_vector, event_vector)
  
  FINAL<-matrix(NA, 1, 8)
  rn=0
  for(i in 1:length(ColumnsToUse)){
    
    Frame.Data<-as.data.frame(Data)
    if(vector.OF.classes.num.fact[i]=="fact"){
      fact.temp<-as.factor(Data[,ColumnsToUse[i]])
      fact.temp<-relevel(fact.temp, ref=levels.of.fact[i])
      model.unadj <- coxph(SURV~fact.temp*case.control+cluster(id_vector))
    }
    if(vector.OF.classes.num.fact[i]=="num"){
      model.unadj <- coxph(SURV~as.numeric(Data[,ColumnsToUse[i]])*case.control+cluster(id_vector))
    }
    
    COEF<-summary(model.unadj)$coeff
    CI<-confint(model.unadj, level = 0.95)
    Hazard<-summary(model.unadj)$conf.int
    RES<-cbind(as.matrix(COEF), as.matrix(CI), as.matrix(Hazard))[3,]
    names(RES)<-c(colnames(COEF), colnames(CI), colnames(Hazard))
    
    RES<-RES[match(c("coef","2.5 %","97.5 %","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),names(RES))]
    
    FINAL<-rbind(FINAL,RES)
    if(is.matrix(RES)==F){
      rn<-c(rn,ColumnsToUse[i])
      cn=names(RES)}else{
        rn<-c(rn,rownames(RES))
        cn=colnames(RES)
      }
    
  }
  colnames(FINAL)<-cn
  rownames(FINAL)<-rn
  FINAL<-FINAL[-1,]
  return(FINAL)
  }
}

##  local.coxph.test(coxph.fit, 2:4)

###########################################################################################
############### COX BACKWARD AIC LOGIT ####################################################
###########################################################################################

Cox_Backward_AIC_Personal<-function(Data, id_name, start_day_vector, stop_day_vector, event_vector, ColumnsToUse, vector.OF.classes.num.fact){
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
  
  library(zoo)
  library("survival")
  
  if(length(start_day_vector)!=dim(Data)[1]|length(event_vector)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  var<-var.fact
  var[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]
  var.save<-var
  
  AIC.RES<-REMOVE.RES<-COL.NAMES<-array()
  AIC.RES[1]<-NA
  AIC.RES_p<-NA
  
  SURV<-Surv(start_day_vector,stop_day_vector, event_vector)
  
  for(i in 1:(length(var)-2)){
    
    fullmod<-as.formula( paste( "SURV ~ cluster(",id_name,")+", paste(var, collapse = "+")))
    model.full <- coxph(fullmod, data=as.data.frame(Data))
    AIC.RES[i+1]<-AIC(model.full)[1]
    
    remove.this.id<-which(as.matrix(summary(model.full)$coefficients)[,"Pr(>|z|)"]==max(as.matrix(summary(model.full)$coefficients)[,"Pr(>|z|)"], na.rm=T))
    AIC.RES_p<-c(AIC.RES_p,round2(max(as.matrix(summary(model.full)$coefficients)[-1,"Pr(>|z|)"]),2))
    
    ## to extract var ##
    remove.this.name<-paste(strsplit(names(remove.this.id),")")[[1]][1],")",sep="")
    #REMOVE.RES[i]<-var[remove.this.id]
    REMOVE.RES[i]<-remove.this.name
    #var<- setdiff(var, var[remove.this.id])
    var<- setdiff(var, remove.this.name)
    #i=i+1
    print(i)
    print(AIC.RES)
    print(REMOVE.RES)
  }
  AIC.RES<-AIC.RES[-1]
  REMOVE.RES<-REMOVE.RES
  AIC.RES_p<-AIC.RES_p[-1]
  
  RES<-cbind(AIC.RES, AIC.RES_p,REMOVE.RES)
  colnames(RES)<-c("AIC Before removal","p Before removal", "Removed variable")
  RES.AIC<-RES
  
  ############################### Final model #################################
  
  Top<-(which.min(as.numeric(RES[,"AIC Before removal"]))-1)
  if(Top==0){vars.keep<-var.save}else{
  vars.keep<-var.save[which(var.save%in%(RES[1:Top,"Removed variable"])==F)]}
  
  #vars.keep<- setdiff(var.save, REMOVE.RES[-c((length(REMOVE.RES)-1):length(REMOVE.RES))])
  
  ind<-grep("age",vars.keep)
  if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
  if(length(ind)==1){
    
    rn<-gsub("as.factor", "",vars.keep)
    rn<-gsub("as.numeric", "",rn)
    rn<-gsub("[()]", "",rn)
    vars.save_names<-rn
    ind.c<-grep("age",colnames(Data[,vars.save_names]))
    age_vec<-as.numeric(Data[,vars.save_names[ind.c]])
    fullmod<-as.formula( paste( "SURV ~ age_vec+cluster(",id_name,")+", paste(vars.keep[-ind], collapse = "+")))
    
  }else{
    fullmod<-as.formula( paste( "SURV ~ cluster(",id_name,")+", paste(vars.keep, collapse = "+")))
  }
  
  
  #fullmod<-as.formula( paste( "SURV ~ cluster(",id_name,")+", paste(vars.keep, collapse = "+")))
  model.full<-coxph(fullmod, data=as.data.frame(Data))
  
  final.result<-cbind(summary(model.full)$coeff,"","",confint(model.full, level = 0.95),summary(model.full)$conf.int)
  final.result<-final.result[,match(c("coef","2.5 %","97.5 %","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),colnames(final.result))]
  
  RES<-rbind(final.result, "",c("AIC = ", AIC(model.full), "", "","","","",""))
  
  RES<-rbind(RES, "",c("Removed tests and corresponding AIC","", "", "","","","", ""),
             c(colnames(RES.AIC), "", "","","",""),cbind(RES.AIC, "", "","","",""))
  
  
  rownames(RES)<-gsub("as.numeric","",rownames(RES))
  rownames(RES)<-gsub("as.factor","",rownames(RES))
  rownames(RES)<-gsub("\\(","",rownames(RES))
  rownames(RES)<-gsub(")","",rownames(RES))
  
  RES[,2]<-gsub("as.numeric","",RES[,2])
  RES[,2]<-gsub("as.factor","",RES[,2])
  RES[,2]<-gsub("\\(","",RES[,2])
  RES[,2]<-gsub(")","",RES[,2])
  
  return(RES)
  }
  }
}

###########################################################################################
########## Function to calculate Descriptive stats for baseline ###########################
###########################################################################################
## You will need: 
# Data - data table (not necessary Baseline only; but should be ordered for FU and ID)
# id_name - name of column with individual IDs
# NameOfMainVariable - name of variable to use for division on multiple tables; should be 0 or 1
# ColumnsToUse - columns to use for chi square etc
############# Example:
## DataHere<-"Z:/Request2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar18 RQ_0438.csv"))
## id_name<-"id"
## NameOfMainVariable<-"NB_DTES_l6m"
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,id_name]),]
## ColumnsToUse<-c("age", "gender", "white")
## Temp<-Descriptive_stats_Personal(Data=as.matrix(dataRead), id_name, NameOfMainVariable, ColumnsToUse)
###########################################################################################

Descriptive_stats_Personal<-function(Data, id_name, NameOfMainVariable, ColumnsToUse){
  miss.ind<-which(ColumnsToUse%in%colnames(Data)==F)
  if(length(miss.ind)>0){print(paste(ColumnsToUse[miss.ind]," is/are missing in your dataset",sep=""))}else{
  
  library(epitools)
  
  Baseline<-Data[which(duplicated(Data[,id_name])==F),]
  Baseline0<-Baseline[which(as.numeric(Baseline[,NameOfMainVariable])==0),]
  Baseline1<-Baseline[which(as.numeric(Baseline[,NameOfMainVariable])==1),]
  
  ##############################################################
  
  People<-unique(Data[,id_name])
  
  #############################################################
  ############# Descriptive statistics for single measurements
  
  Baseline.everything<-Baseline
  Baseline.stats<-function(Data.table,Baseline.everything){
    
    Baseline<-Data.table[which(duplicated(Data.table[,id_name])==F),]
    Baseline.data<-Baseline[,which(colnames(Baseline)%in%ColumnsToUse)]
    Baseline.dependent_var<-as.numeric(Baseline[,NameOfMainVariable])
    class(Baseline.data)<-"numeric"
    Baseline.data.result1<-colSums(Baseline.data, na.rm=T)
    Baseline.data.result0<-sapply(1:dim(Baseline.data)[2], function(x) length(which(Baseline.data[,x]==0)))
    Baseline.data.result1.with.perc<-paste(Baseline.data.result1, " (",round2(Baseline.data.result1/dim(Baseline.data)[1]*100,1),")", sep="")
    Baseline.data.result0.with.perc<-paste(Baseline.data.result0, " (",round2(Baseline.data.result0/dim(Baseline.data)[1]*100,1),")", sep="")
    
    for(l in 1:dim(Baseline.data)[2]){
      temp<-as.numeric(Baseline.data[,l])
      if(all(temp%in%c(NA, 1, 0))==F){
        #Baseline.data.result1.with.perc[l]<-Baseline.data.result0.with.perc[l]<-
        #paste("mean = ", round2(mean(temp, na.rm=T),1), 
        #   ", sd = ", round2(sd(temp, na.rm=T),1),
        #  ", med = ", round2(median(temp, na.rm=T),1),
        # " range=(", round2(min(temp, na.rm=T),0),"-", round2(max(temp, na.rm=T),0),")",
        #",IQR (based on quantile) = ", round2(IQR(temp, na.rm=T),1),
        #",IQR (H-spread) and (Q1 - Q3) = ", round2(diff(fivenum(temp)[c(2,4)]),1)," (",round2(fivenum(temp)[2],1),"-",round2(fivenum(temp)[4],1),"),"
        #, sep="")
        
        Baseline.data.result1.with.perc[l]<-Baseline.data.result0.with.perc[l]<-
          paste("med (IQR) = ", round2(median(temp, na.rm=T),1)," (",round2(fivenum(temp)[2],1),"-",round2(fivenum(temp)[4],1),"),"
                , sep="")
          }}
    
    RESULT.base<-rbind(Baseline.data.result1.with.perc,Baseline.data.result0.with.perc)
    RESULT.base01<-rbind(rep(1, length(Baseline.data.result1.with.perc)),rep(0, length(Baseline.data.result0.with.perc)))
    
    colnames(RESULT.base)<-colnames(Baseline.data)
    
    
    RESULT<-cbind(c("", as.vector(RESULT.base01)), c("", as.vector(RESULT.base)))
    rn<-c("Baseline appointment", as.vector(rbind(colnames(RESULT.base), rep("",dim(RESULT.base)[2]))))
    RESULT<-cbind(rn, RESULT)
    colnames(RESULT)<-c("Demographic Characteristics", "Value", paste(deparse(substitute(Data.table)),
                                                        " Total n=",dim(Baseline.data)[1],
                                                        " (",round2(dim(Baseline.data)[1]/dim(Baseline.everything)[1]*100,2),"%)",
                                                        ", N (%)",sep=""))
    
    return(RESULT)
  } ## function for main stats
  
  #############################################################
  
  MainVariable0.res1<-Baseline.stats(Baseline0,Baseline.everything)
  MainVariable1.res1<-Baseline.stats(Baseline1,Baseline.everything)
  MainVariable.res1<-Baseline.stats(Baseline,Baseline.everything)
  
  #############################################################
  ############# Chi squared; Fisher and OR

  Baseline.dependent_var<-as.numeric(Baseline[,NameOfMainVariable])
  Baseline.data<-Baseline[,which(colnames(Baseline)%in%ColumnsToUse)]
  
  Pv.fisher<-XSq<-Pv<-rep(NA,dim(Baseline.data)[2])
  Wilcoxon<-OR.fisher<-OR<-matrix("", dim(Baseline.data)[2], 3)
  for(i in 1:dim(Baseline.data)[2]){
    
    temp<-as.numeric(Baseline.data[,i])
    temp2<-Baseline.dependent_var[which(is.na(temp)==F)]
    if(length(table(temp))>1&length(table(temp2))>1){
      if(all(temp%in%c(NA, 1, 0))){
        MainVariable1.table<-table(temp[which(Baseline.dependent_var==1)])
        MainVariable0.table<-table(temp[which(Baseline.dependent_var==0)])
        Matrix<-rbind(MainVariable0.table,MainVariable1.table)
        if(length(Matrix)<4){ #added recently
          XSq[i]<-Pv[i]<-OR[i,]<-Pv.fisher[i]<-OR.fisher[i,]<-NA
          Wilcoxon[i,1:3]<-rep("",3)
        }else{
        XSq[i]<-round2(chisq.test(Matrix, correct = FALSE)$statistic,2)
        Pv[i]<-chisq.test(Matrix, correct = FALSE)$p.value
        OR[i,]<-round2(epitab(Matrix, method = c("oddsratio"), conf.level = 0.95)$tab[2,5:7],2)
        Pv.fisher[i]<-fisher.test(Matrix)$p.value
        OR.fisher[i,]<-round2(c(fisher.test(Matrix)$estimate,fisher.test(Matrix)$conf.int[1:2]),2)
        Wilcoxon[i,1:3]<-rep("",3)}

      }else{
        
        mylogit<-glm(Baseline.dependent_var~temp, family="binomial")
        OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))[2,]
        names(OR.CI) <- c("OR", "Lower OR", "Upper OR")
        res.temp.glm<-c(summary(mylogit)$coefficients[2,],OR.CI)
        
        OR[i,]<-round2(res.temp.glm[c("OR","Lower OR","Upper OR")],3)
        Wilcoxon[i,1]<-wilcox.test(temp~Baseline.dependent_var)$p.value ## Wilcoxon
        Wilcoxon[i,2:3]<-t.test(temp~Baseline.dependent_var)$estimate
        Wilcoxon[i,]<-round2(as.numeric(Wilcoxon[i,]),4)
        
        }

    }
  }
  Pv<-round2(Pv,3)
  Pv.fisher<-round2(Pv.fisher,3)
  Pv[which(Pv<0.0001)]<-"<0.0001"
  Pv.fisher[which(Pv.fisher<0.0001)]<-"<0.0001"
  RES<-cbind(XSq, Pv, paste(OR[,1], " (",OR[,2], " - ",OR[,3],")",sep=""),
             Pv.fisher, paste(OR.fisher[,1], " (",OR.fisher[,2], " - ",OR.fisher[,3],")",sep=""),
             Wilcoxon[,1]) 
  rownames(RES)<-colnames(Baseline.data)
  colnames(RES)<-c("Chi-square", "p-value Chi-square", "OR (95% CI)", "p-value Fisher", "OR Fisher (95% CI)", 
                   "p-value Mann-Whitney")
  
  RESULT<-cbind(MainVariable.res1, MainVariable1.res1[,3], MainVariable0.res1[,3])
  colnames(RESULT)<-c(colnames(MainVariable.res1),colnames(MainVariable1.res1)[3], colnames(MainVariable0.res1)[3])
  RESULT<-cbind(RESULT, RES[match(RESULT[,1], rownames(RES)),])
  RESULT[which(is.na(RESULT),arr.ind=T)]<-""
  
  cn<-colnames(RESULT)
  cn<-gsub("Baseline Total","All, Total",cn)
  cn<-gsub("Baseline1",paste(NameOfMainVariable,"=1,",sep=""),cn)
  cn<-gsub("Baseline0",paste(NameOfMainVariable,"=0,",sep=""),cn)
  
  colnames(RESULT)<-cn

  RESULT<-RESULT[-1,]
  
  RESULT<-rbind(RESULT,c("* please use Fisher's test for counts<5", rep("",dim(RESULT)[2]-1)),
                c("** OR for continuous variables have been obtained using regression", rep("",dim(RESULT)[2]-1)))
  

  return(RESULT)
  }
}

###########################################################################################
#################### Function to calculate Kaplan Meier ###################################
###########################################################################################

### time updated ###

Kaplan_Meier_time_updated<-function(Data, id_name, start_day_name, stop_day_name, event_name, strata_name,
                                    NAMES_0_1){ ## 1 or name of covariate
  
  library("survival")
  
  Data<-Data[which(as.numeric(Data[,start_day_name])!=as.numeric(Data[,stop_day_name])),]
  
  Time1<-as.numeric(Data[,start_day_name])/30.43
  Time2<-as.numeric(Data[,stop_day_name])/30.43
  Event<-as.numeric(Data[,event_name])
  ID<-Data[,id_name]

  SURV<-Surv(Time1, Time2, Event)
  
  #NAMES<-c(paste(strata_name," = 0",sep=""),paste(strata_name," = 1",sep=""))
  NAMES<-NAMES_0_1
  
  fit <- survfit(SURV~as.numeric(Data[,strata_name]), id=ID)
  survtest <- coxph(SURV ~ as.numeric(Data[,strata_name]))
  logrank.p.value<-round2(summary(survtest)$sctest[3],3) #logrank p-value
  if(logrank.p.value>0){logrank.p.value<-paste(" = ",logrank.p.value,sep="")}
  if(logrank.p.value==0){logrank.p.value<-" < 0.001"}
  
  ss<-summary(fit)

  png(paste("Kaplan-Meier ",strata_name,".png",sep=""),width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 5)
  par(mai=c(0.5,0.5,0.5,0.5))
  plot(fit, fun = function(x) (1-x)*100,ylab="Cumulative Incidence (%)", xlab="Time (months)", lty=c(1:2), mark.time=F,
       xlim=c(0, max(ss$time)))
  legend("topleft", NAMES, lty = c(1:2),bty="n") 
  legend("bottomright", c(paste("Log-rank P",logrank.p.value,sep=""),""),bty="n") 
  dev.off()
  
}

### last event only (when death event) ###

Kaplan_Meier_end_event<-function(Data, id_name, stop_day_name, event_name){
  
  library("survival")
  
  Last_event<-Data[which(duplicated(Data[,id_name], fromLast=T)==F),]
  Time2<-as.numeric(Last_event[,stop_day_name])
  Event<-as.numeric(Last_event[,event_name])
  ID<-Data[,id_name]
  SURV<-Surv(Time2, Event)
  fit_last <- survfit(SURV~1)
  plot(fit_last)
  
}

#############################################################################################
## You will need: 
## Data
## id_name - name of column with patients id
## Groups - name of column with dependent_vars to compare
## start_day_name - name of column with start day
## stop_day_name - name of column with end day
## event_name - name of column with event
############# Example:
## DataHere<-"Z:/Request2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar18 RQ_0438.csv"))
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,1]),]
## id_name<-"id"
## Groups<-"NB_DTES_l6m"
## start_day_name<-"start_day"
## stop_day_name<-"stop_day"
## event_name<-"hcv_event"
## Temp<-Kaplan_Meier_Personal(Data, id_name, Groups, start_day_name, stop_day_name, event_name)
###########################################################################################

Kaplan_Meier_Personal<-function(Data, id_name, Groups, start_day_name, stop_day_name, event_name){
  library("survival")
  Data.kaplan.meier<-Data[, match(c(id_name, Groups, start_day_name, stop_day_name, event_name),colnames(Data))]
  Data.kaplan.meier.first<-Data.kaplan.meier[which(duplicated(Data.kaplan.meier[,id_name])==F),]
  Data.kaplan.meier.last<-Data.kaplan.meier[which(duplicated(Data.kaplan.meier[,id_name], fromLast=T)==F),]

  NB.first.app<-Data.kaplan.meier.first[match(Data.kaplan.meier.last[,id_name],Data.kaplan.meier.first[,id_name]),Groups]
  NB.last.app<-Data.kaplan.meier.last[,Groups]

  #############################################################

  SURV<-Surv(as.numeric(Data.kaplan.meier.last[,stop_day_name]), as.numeric(Data.kaplan.meier.last[,event_name]))

  LTY<-c(1:length(unique(Data[,Groups])))
  
  ### first app NB
  png("Kaplan-Meier from first FU.png")
  Kaplan.meier.first<-survfit(SURV~1+strata(NB.first.app))
  plot(Kaplan.meier.first,ylab="Survival Probability", xlab="N of days", lty=LTY, mark.time=F)
  legend("topright", paste("Group = ",names(table(NB.first.app)), sep=""), lty = LTY) 
  title("Kaplan-Meier Curves\nfor data from first FU")
  dev.off()
  
  RES.first<-survdiff(SURV~NB.first.app)
  p.val.first <- 1 - pchisq(RES.first$chisq, length(RES.first$n) - 1)

  ### last app NB
  png("Kaplan-Meier from last FU.png")
  Kaplan.meier.last<-survfit(SURV~1+strata(NB.last.app))
  plot(Kaplan.meier.last,ylab="Survival Probability", xlab="N of days", lty=LTY, mark.time=F)
  legend("topright", paste("Group = ",names(table(NB.last.app)), sep=""), lty = LTY) 
  title("Kaplan-Meier Curves\nfor data from last FU")
  dev.off()
  
  RES.last<-survdiff(SURV~NB.last.app)
  p.val.last <- 1 - pchisq(RES.last$chisq, length(RES.last$n) - 1)
RES<-c(p.val.first, p.val.last) 
names(RES)<-c("p-value First FU", "p-value Last FU")
return(RES)
}

###########################################################################################
#################### Function to calculate KM stats #######################################
###########################################################################################

#Data<-Data.first.event
#start_day_name<-"start_day"
#stop_day_name<-"stop_day"
#event_name<-"event"
#id_name<-"code"
#strata_vector<-as.numeric(Data[,"eviction"])

#GET_Kaplan_Meier_Stats(Data, start_day_name,stop_day_name,event_name,id_name,strata_vector)

GET_Kaplan_Meier_Stats<-function(Data, start_day_name,stop_day_name,event_name,id_name,strata_vector){
  RES<-matrix(NA,2,8)
  library("survival")
  
  Time1<-as.numeric(Data[,start_day_name])/30.43
  Time2<-as.numeric(Data[,stop_day_name])/30.43
  Event<-as.numeric(Data[,event_name])
  ID<-Data[,id_name]
  
  SURV<-Surv(Time1, Time2, Event)
  fit <- survfit(SURV~strata_vector, id=ID)
  ss<-summary(fit)
  
  STRATA<-ss$strata
  NAMES<-names(table(STRATA))
  TIME<-ss$time
  SURV<-ss$surv
  LOWER<-ss$lower
  UPPER<-ss$upper
  
  RES[,1]<-c(max(TIME[which(STRATA==NAMES[1])]),max(TIME[which(STRATA==NAMES[2])]))
  RES[,4]<-c(min(LOWER[which(STRATA==NAMES[1])]),min(LOWER[which(STRATA==NAMES[2])]))
  RES[,5]<-c(min(UPPER[which(STRATA==NAMES[1])]),min(UPPER[which(STRATA==NAMES[2])]))
  RES[,3]<-c(min(SURV[which(STRATA==NAMES[1])]),min(SURV[which(STRATA==NAMES[2])]))
  RES[,6]<-(1-RES[,3])*100
  RES[,7]<-(1-RES[,5])*100
  RES[,8]<-(1-RES[,4])*100
  RES[,2]<-xtabs(~strata_vector+Event)[,2]
  colnames(RES)<-c("time","n events=1","survival","Lower 95%","Upper 95%","cumulative incidence","Lower 95%","Upper 95%")
  rownames(RES)<-NAMES
  return(RES)
}

###########################################################################################
#################### Function to calculate Hazard #########################################
###########################################################################################
## You will need: 
## Data
## id_name - name of column with patients id
## start_day_name - name of column with start day
## stop_day_name - name of column with end day
## event_name - name of column with event
## ColumnsToUse - columns to use
############# Example:
## DataHere<-"Z:/Request2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-as.matrix(read.csv("final_2016mar18 RQ_0438.csv"))
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,1]),]
## ColumnsToUse<-c("age", "gender", "white", "unstable_housing", "mmt_l6m", "sextrade_l6m", "daily_noninj_crack_l6m")
## id_name<-"id"
## start_day_name<-"start_day"
## stop_day_name<-"stop_day"
## event_name<-"hcv_event"
## Temp<-Hazard_Personal(Data, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse)
###########################################################################################

Hazard_Personal<-function(Data, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse){

  library("survival")
  library(simPH)
  
  Data.cox<-Data[which(as.numeric(Data[,stop_day_name])>0),]
  SURV<-Surv(as.numeric(Data.cox[,start_day_name]),as.numeric(Data.cox[,stop_day_name]), as.numeric(Data.cox[,event_name]))
  Hazard<-matrix(NA, length(ColumnsToUse), 10)

  for(i in 1:length(ColumnsToUse)){
   fitCPH <- coxph(SURV ~ as.numeric(Data.cox[,which(colnames(Data.cox)==ColumnsToUse[i])])+cluster(Data.cox[,id_name]))
   ss<-summary(fitCPH)
   #names(ss)
   Hazard[i,]<-c(ss$conf.int, ss$coefficients)
  
  }
  colnames(Hazard)<-c(colnames(ss$conf.int), colnames(ss$coefficients))
  rownames(Hazard)<-ColumnsToUse
  return(Hazard)
}

###########################################################################################
#################### Function to calculate Cox confounding ################################
###########################################################################################
## You will need: 
## Data - should be data frame
## ColumnsToUse - columns to use
## start_day_name
## stop_day_name
## event_name - name of column with event ("death")
## indep_var - independent variable
## id_name
############# Example:
## DataHere<-"Z:/Request2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-read.csv("final_2016mar18 RQ_0438.csv")
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,1]),]
## ColumnsToUse<-c("age", "gender", "white", "unstable_housing", "mmt_l6m", "sextrade_l6m", "daily_noninj_crack_l6m")
## start_day_name<-"start_day"
## stop_day_name<-"stop_day"
## event_name<-"hcv_event"
## indep_var<-"NB_DTES_l6m"
## id_name<-"id"
##########################################################################################

Cox_Confounding_Personal<-function(Data, indep_var_vec, id_name, 
                                   start_day_name, stop_day_name, event_name, 
                                   ColumnsToUse,vector.OF.classes.num.fact,levels.of.fact){
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
  
  if(length(indep_var_vec)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  library(zoo)
  library("survival")
  
  
  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  vars<-var.fact
  vars[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]
  
  Frame.Data<-as.data.frame(Data) ## relevel factors
  if(length(which(vector.OF.classes.num.fact=="fact"))>0){
    factors.used.ind<-which(vector.OF.classes.num.fact=="fact")
    for(i in 1:length(factors.used.ind)){
      Frame.Data[,ColumnsToUse[factors.used.ind[i]]]<-relevel(Frame.Data[,ColumnsToUse[factors.used.ind[i]]],ref=levels.of.fact[factors.used.ind[i]])
    }
  }
  
  SURV<-Surv(as.numeric(Data[,start_day_name]),as.numeric(Data[,stop_day_name]), as.numeric(Data[,event_name]))
  
  #Data.data<-Data[,which(colnames(Data)%in%ColumnsToUse)]

  fullmod<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+", paste(vars, collapse = "+")))
  model.full <- coxph(fullmod, data=Frame.Data)
  
  SS<-summary(model.full)$coeff
  coeff.indep.ind<-grep("indep_var_vec", rownames(SS))
  MAX.coeff<-which.max(abs(SS[coeff.indep.ind,1])) ## when indep_var is factor, we pick max coef of its levels
  names(MAX.coeff)<-rownames(SS)[coeff.indep.ind[MAX.coeff]]
  MAX.coeff.name<-names(MAX.coeff) ## leading factor
  COEF.full<-SS[MAX.coeff.name,]
  CI<-confint(model.full, level = 0.95)[MAX.coeff.name,]

  COEF.to.save<-t(as.matrix(c(COEF.full,0,0,CI)))
  colnames(COEF.to.save)<-c(names(COEF.full), "Delta", "abs Delta", "5%CI", "95%CI")
  removed.variables<-"full model"
  #### removing variables

  while(COEF.to.save[dim(COEF.to.save)[1],8]<=5){
    COEF.updated<-matrix(NA, length(vars), 8)
    rownames(COEF.updated)<-vars
    for (j in 1:length(vars)){
    
      vars.updated<-vars[-j]
      temp.model.formula<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+", paste(vars.updated, collapse = "+")))
      model.updated <- coxph(temp.model.formula, data=Frame.Data)
      SS<-summary(model.updated)$coeff
      COEF.upd<-SS[MAX.coeff.name,]
      CI.upd<-confint(model.updated, level = 0.95)[MAX.coeff.name,]
      COEF.updated[j,]<-c(COEF.upd,CI.upd)
    
    }
    Delta<-(COEF.updated[,1]/COEF.full[1]-1)*100
    COEF.updated<-cbind(COEF.updated[,1:6],Delta, abs(Delta),COEF.updated[,7:8])
    remove.this<-which.min(abs(Delta))
  
    COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
    removed.variables<-c(removed.variables,names(remove.this))
    ## updating
  
    vars.save<-vars
    vars<-vars[-remove.this]
  
    fullmod<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+", paste(vars, collapse = "+")))
    model.full <- coxph(fullmod, data=Frame.Data)
    SS<-summary(model.full)$coeff
    COEF.full<-SS[MAX.coeff.name,]
  
    print(remove.this)
  }
  rownames(COEF.to.save)<-removed.variables

  ind<-grep("age",vars.save)
  if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
  if(length(ind)==1){
    
    rn<-gsub("as.factor", "",vars.save)
    rn<-gsub("as.numeric", "",rn)
    rn<-gsub("[()]", "",rn)
    vars.save_names<-rn
    ind.c<-grep("age",colnames(Data[,vars.save_names]))
    age_vec<-as.numeric(Data[,vars.save_names[ind.c]])
    fullmod<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+age_vec+", paste(vars.save[-ind], collapse = "+")))
    
  }else{
    fullmod<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+", paste(vars.save, collapse = "+")))
  }
  
  model.full <- coxph(fullmod, data=Frame.Data)
  
  final.result<-cbind(summary(model.full)$coeff,"","",confint(model.full, level = 0.95),summary(model.full)$conf.int)
  
  COEF.to.save<-COEF.to.save[,match(c("coef","5%CI","95%CI","z","Pr(>|z|)","Delta","abs Delta"),colnames(COEF.to.save))]
  final.result<-final.result[,match(c("coef","2.5 %","97.5 %","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),colnames(final.result))]
  
  COEF.to.save<-rbind(c("Multivariate result","","","","","","",""),final.result, "", 
                      c("Procedure","","","","","","",""), colnames(cbind(COEF.to.save,"")),cbind(COEF.to.save,""))
  
  rn<-rownames(COEF.to.save)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(COEF.to.save)<-rn
return(COEF.to.save)
  }
  }
}

###########################################################################################
#################### Function to calculate Cox adjusted ###################################
###########################################################################################
## You will need: 
## Data
## ColumnsToUse - columns to use
## start_day_name
## stop_day_name
## event_name - name of column with event ("death")
## indep_var - independent variable
## id_name
############# Example:
## DataHere<-"Z:/Request2 Cox RQ_0438/data/"
## setwd(DataHere)
## Data<-read.csv("final_2016mar18 RQ_0438.csv")
## Data<-Data[order(as.numeric(Data[,"survey"])),]
## Data<-Data[order(Data[,1]),]
## ColumnsToUse<-c("gender", "sextrade_l6m", "daily_heroin_inj_l6m", "daily_po_l6m", "help_l6m","borrow_l6m")
## indep_var<-"NB_DTES_l6m"
## id_name<-"id"
## start_day_name<-"start_day"
## stop_day_name<-"stop_day"
## event_name<-"hcv_event"
## Temp<-Cox_Adjusted_Personal(Data, indep_var_vec, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse)
##########################################################################################

Cox_Adjusted_Personal<-function(Data, indep_var_vec, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse){
  
  if(length(indep_var_vec)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  library(zoo)
  library("survival")
  
  Data.data<-Data[,which(colnames(Data)%in%ColumnsToUse)]
  SURV<-Surv(as.numeric(Data[,start_day_name]),as.numeric(Data[,stop_day_name]), as.numeric(Data[,event_name]))
  
  vars.all=colnames(Data.data)
  vars.all<-paste("as.numeric(",vars.all,")",sep="")
  vars<-vars.all
  fullmod<-as.formula( paste( "SURV ~ indep_var_vec+cluster(",id_name,")+", paste(vars, collapse = "+")))
  model.full <- coxph(fullmod, data=Data)
  COEF.full<-summary(model.full)$coefficients
  CI<-confint(model.full, level = 0.95)
  Hazard<-summary(model.full)$conf.int
  
  RES<-cbind(COEF.full, CI,Hazard)
  colnames(RES)<-c(colnames(COEF.full), "5%CI", "95%CI",colnames(Hazard))
  RES<-RES[,match(c("coef","5%CI","95%CI","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),colnames(RES))]
  return(RES)
  }
}

##########################################################################################
############### Cox multivariate #########################################################

Cox_Multi_Personal<-function(Data.F, id_name, start_day_name, stop_day_name, event_name, ColumnsToUse){
  library(zoo)
  library("survival")
  
  SURV<-Surv(as.numeric(Data.F[,start_day_name]),as.numeric(Data.F[,stop_day_name]), as.numeric(Data.F[,event_name]))
  fullmod<-as.formula( paste( "SURV ~ cluster(",id_name,")+", paste(ColumnsToUse, collapse = "+")))
  model.full <- coxph(fullmod, data=Data.F)
  COEF.full<-summary(model.full)$coefficients
  CI<-confint(model.full, level = 0.95)
  Hazard<-summary(model.full)$conf.int
  
  RES<-cbind(COEF.full, CI,Hazard)
  colnames(RES)<-c(colnames(COEF.full), "5%CI", "95%CI",colnames(Hazard))
  RES<-RES[,match(c("coef","5%CI","95%CI","z","Pr(>|z|)","exp(coef)","lower .95","upper .95"),colnames(RES))]
  RES<-RES[,-c(1:4)]
  colnames(RES)<-c("P-value","HR","HR lower 95% CI","HR upper 95% CI")
  return(RES)
}

#########################################################################################
############### Numbers for data stratified twice
#########################################################################################
## DataHere<-"Z:/Request 5 RQ_0446 Methadose/data/"
## setwd(DataHere)
## dataRead<-as.matrix(read.csv("final_2016apr12 RQ_0446.csv"))
## strat1<-"survey" ## any numbers here, group N1
## strat2<-"HIV" ## only 1 and 0, group N2 within group N1
## variable<-"received_methadose_l6m" ## only 1 and 0 # final group
## Data<-dataRead

StratifiedTwice<-function(Data, strat1, strat2, variable){
  FU<-sort(unique(Data[,strat1]))
  Mat.all<-c(0,0,0,0,0,0)
  names(Mat.all)<-c(strat1, "N",strat2, "N", variable, "N")
  
  for(i in 1:length(FU)){
    strat3.unique<-unique(sort(Data[,variable]))
    Mat.temp<-matrix("", length(strat3.unique)*2,6)
    collectFU.ind<-which(Data[,strat1]==FU[i])
    Mat.temp[,1:2]<-cbind(rep(FU[i],length(strat3.unique)*2),rep(length(collectFU.ind),length(strat3.unique)*2))
    
    dataFU.temp<-Data[collectFU.ind,]
    
    Hiv.out0<-length(which(as.numeric(dataFU.temp[,strat2])==0))
    Hiv.out1<-length(which(as.numeric(dataFU.temp[,strat2])==1))
    
    Mat.temp[,3:4]<-cbind(c(rep(0,length(strat3.unique)),rep(1,length(strat3.unique))),
                   c(rep(Hiv.out0,length(strat3.unique)),rep(Hiv.out1,length(strat3.unique))))
    
    #
    dataFU.temp.1<-dataFU.temp[which(as.numeric(dataFU.temp[,strat2])==0),variable]
    Table<-table(dataFU.temp.1)
    Table<-Table[match(strat3.unique,names(Table))]
    len0<-cbind(strat3.unique,Table)
    dataFU.temp.1<-dataFU.temp[which(as.numeric(dataFU.temp[,strat2])==1),variable]
    Table<-table(dataFU.temp.1)
    Table<-Table[match(strat3.unique,names(Table))]
    len1<-cbind(strat3.unique,Table)

    Mat.temp[,5:6]<-rbind(len0, len1)
    
    Mat.all<-rbind(Mat.all,Mat.temp)
  }
  
  Mat.all<-Mat.all[-1,]
  Mat.all[which(is.na(Mat.all),arr.ind=T)]<-0
  
  return(Mat.all)
}

#########################################################################################
############### Descriptive stats for all type of data, stratified by main variable #####
######### one variable of interest at time ##############################################
#########################################################################################

Descriptive_stats_all_types_of_data_Personal_stratified<-function(Data, Column_to_use_name, NameOfMainVariable){
  All<-Descriptive_stats_all_types_of_data_Personal(Data[,Column_to_use_name])
  All<-All[-grep("not missing",All[,3]),]
  All.id<-paste(All[,1],All[,2],sep="_")
  NameOfMainVariable1<-Descriptive_stats_all_types_of_data_Personal(Data[which(as.numeric(Data[,NameOfMainVariable])==1),Column_to_use_name])
  NameOfMainVariable1.id<-paste(NameOfMainVariable1[,1],NameOfMainVariable1[,2],sep="_")
  NameOfMainVariable1.add<-paste(NameOfMainVariable1[,3]," (", NameOfMainVariable1[,4], ")",sep="")
  NameOfMainVariable0<-Descriptive_stats_all_types_of_data_Personal(Data[which(as.numeric(Data[,NameOfMainVariable])==0),Column_to_use_name])
  NameOfMainVariable0.id<-paste(NameOfMainVariable0[,1],NameOfMainVariable0[,2],sep="_")
  NameOfMainVariable0.add<-paste(NameOfMainVariable0[,3]," (", NameOfMainVariable0[,4], ")",sep="")
  RES<-cbind(All[,2],paste(All[,3]," (", All[,4], ")",sep=""), 
             NameOfMainVariable1.add[match(All.id,NameOfMainVariable1.id)],
             NameOfMainVariable0.add[match(All.id,NameOfMainVariable0.id)])
  colnames(RES)<-c("Variable", paste("All ", NameOfMainVariable,sep=""), paste(NameOfMainVariable," = 1",sep=""), paste(NameOfMainVariable," = 0",sep=""))
  rownames(RES)<-All[,1]
  
  RES[which(is.na(RES),arr.ind=T)]<-0
  rn<-rownames(RES)
  ## chi square
add.chi.squared<-rep(NA, 6)
  for(t in 1:length(Column_to_use_name)){
    ind<-which(rownames(RES)==Column_to_use_name[t])
    if(length(ind)==1){
      
      add.chi.squared<-rbind(add.chi.squared,c(temp.all,"NA","NA"))
      }else{
    temp.all<-RES[ind,]
    temp<-RES[ind,-c(1:2)]
    temp[,1]<-sapply(1:dim(temp)[1], function(x) strsplit(temp[x,1],"\\(")[[1]][1])
    temp[,2]<-sapply(1:dim(temp)[1], function(x) strsplit(temp[x,2],"\\(")[[1]][1])
    class(temp)<-"numeric"
    temp[which(is.na(temp), arr.ind=T)]<-0
    
    if(length(ind)<4&sum(temp)>0){

    chi.squared<-round2(chisq.test(temp,correct = FALSE)$statistic,2)
    chi.squared<-c(chi.squared,rep("",dim(temp)[1]-1))
    chi.squared.p.value<-round2(chisq.test(temp)$p.value,2)
    chi.squared.p.value<-c(chi.squared.p.value,rep("",dim(temp)[1]-1))}else{
    
    chi.squared<-c(NA,rep("",dim(temp)[1]-1))
    chi.squared.p.value<-c(NA,rep("",dim(temp)[1]-1))}
    
    temp.all<-cbind(temp.all,chi.squared,chi.squared.p.value)
    add.chi.squared<-rbind(add.chi.squared,temp.all)
    
  }}
add.chi.squared<-add.chi.squared[-1,]
rownames(add.chi.squared)<-rn
  return(add.chi.squared)
}

#########################################################################################
############### Descriptive stats for all type of data (N and %; IQR med and min/max for num)
#########################################################################################
## DataHere<-"Z:/Request 7 RQ_0360A Injection Cessation/data/"
## setwd(DataHere)
## dataRead<-as.matrix(read.csv("final_2016apr12 RQ_0446.csv"))
## dataRead<-as.matrix(read.csv("final_2016apr20 RQ_0458.csv"))
## dataRead.base<-dataRead[which(as.numeric(dataRead[,"base"])==1),]
## ColumnsToUse<-c("stop_injecting_30days","NON_INJ_MON","JAIL","PREGNANCY","TREATMENT","HOSPITAL","FEEL_BREAK","OTH1","OTH1_TXT","OUT_JAIL",
##                "BOREDOM","DEPRESSION","PEER_PRESS","PARTNER_USE","HAD_MONEY","WHY_OTH","WHY_OTH_TXT")

## Data<-dataRead.base[,which(colnames(dataRead.base)%in%ColumnsToUse)]
## Descriptive_stats_all_types_of_data_Personal(Baseline_Data=Data)
## RES.all<-Descriptive_stats_all_types_of_data_Personal(Baseline_Data=Data)
## RES.all.id<-paste(RES.all[,1],RES.all[,2],sep="_")
## RES.0<-Descriptive_stats_all_types_of_data_Personal(Baseline_Data=Data[which(gr==0),])
## RES.0.id<-paste(RES.0[,1],RES.0[,2],sep="_")
## RES.1<-Descriptive_stats_all_types_of_data_Personal(Baseline_Data=Data[which(gr==1),])
## RES.1.id<-paste(RES.1[,1],RES.1[,2],sep="_")
## RES<-cbind(RES.all, RES.0[match(RES.all.id,RES.0.id),3:4],RES.1[match(RES.all.id,RES.1.id),3:4])
## Comment: "missing" or NA values are those which were blank in the database; N/A values are those which were inputed as N/A in the database; 

Descriptive_stats_all_types_of_data_Personal<-function(Baseline_Data){
  Data<-Baseline_Data
  MAT.all<-c(0,0,0,0)
  names(MAT.all)<-c("variable","value","N","%")
  for(i in 1:dim(Data)[2]){
    
    temp<-Data[,i]
    Unique<-unique(as.numeric(temp))
    
    if(all(is.na(unique(temp)))){
      id<-colnames(Data)[i]
      MAT.all<-rbind(MAT.all,c(id,"","All inputs are missing/blanc",""))
      
    }else{
    
    if(all(is.na(Unique))|length(which(is.na(Unique)==F))==1){ ## if data is character
      temp[which(temp=="")]<-NA
      ta<-table(temp)
      mat<-cbind(names(ta),ta)
    }else{
    
      if(all(Unique%in%c(NA,0,1))&length(table(Unique))<3){ ## if data is binary with NA
        temp<-as.numeric(temp)
        ta<-table(temp)
        mat<-cbind(names(ta),ta)
      }else{
      if(length(table(Unique))>=2){ ## if data is numeric ## it was "3" before
        temp<-as.numeric(temp)
        Med<-median(temp,na.rm=T)
        Iqr<-IQR(temp,na.rm=T)
        Min<-min(temp,na.rm=T)
        Max<-max(temp,na.rm=T)
        Mean<-mean(temp,na.rm=T)
        mat<-cbind(c("median","IQR","Min","Max","Mean"),c(Med,Iqr,Min,Max,Mean))
      }}
    
    }
    
    mat.pers<-round2(as.numeric(mat[,2])/dim(Data)[1]*100,2)
    mat<-cbind(mat,mat.pers)
    id<-colnames(Data)[i] ## to combine with other groups in the future using "match()"
    MAT.all<-rbind(MAT.all,c(id,"",paste("not missing n=",length(which(is.na(temp)==F)),sep=""),""),cbind(id,mat))}
  }
  MAT.all<-MAT.all[-1,]
  rownames(MAT.all)<-paste(MAT.all[,"variable"],MAT.all[,"value"],sep="_")
  return(MAT.all)
}

###########################################################################################
############### GLM BIVARIATE LOGIT #######################################################
###########################################################################################

GLM_Bivariate<-function(Data.Frame,ColumnsToUse, Outcome_name, which.family){
  
  RES.add<-rep(NA, 7)
  rn<-0
  for(i in 1:length(ColumnsToUse)){
    fullmod<-as.formula( paste(Outcome_name, " ~ ", paste(ColumnsToUse[i])))
      mylogit<-glm(fullmod, data=Data.Frame, family=which.family)
      OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))[-1,]
      if(length(OR.CI)==3){
        names(OR.CI) <- c("OR", "Lower OR", "Upper OR")
        temp<-c(summary(mylogit)$coefficients[-1,],OR.CI)
        rn<-c(rn, ColumnsToUse[i])
      }else{
      colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
      temp<-cbind(summary(mylogit)$coefficients[-1,],OR.CI)
      rn<-c(rn, rownames(temp))
      }
      
      RES.add<-rbind(RES.add,temp)

      
    }
    rownames(RES.add)<-rn
    RES<-RES.add

    RES<-RES[-1,-c(2,3)]
    
    options(scipen = 999)
    
    RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
    RES[,2]<-format(RES[,2],nsmall=3)
    RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
    colnames(RES)<-c("Estimate", "P-value", "OR", "Lower OR", "Upper OR")
    
    OR.and.CI<-paste(format(round2(as.numeric(RES[,"OR"]),2),nsmall=2)," (",
                     format(round2(as.numeric(RES[,"Lower OR"]),2),nsmall=2)," - ",
                     format(round2(as.numeric(RES[,"Upper OR"]),2),nsmall=2),")",sep="")
    RES<-cbind(RES,OR.and.CI)
    RES<-RES[,c(1,2,6)]

  
  return(RES)
}

###########################################################################################
############### GLM MULTIVARIATE LOGIT ####################################################
###########################################################################################
## Data=as.matrix(dataRead)
## dependent_var<-as.numeric(Data[,"BEFORE_IDRUGS"])
## ColumnsToUse<-cn[-which(cn%in%c("code","survey","int_date","BEFORE_IDRUGS"))]
###########################################################################################

GLM_Multivariate<-function(Data.Frame, ColumnsToUse, Outcome_name, which.family){
  
  fullmod<-as.formula( paste(Outcome_name, " ~ ", paste(ColumnsToUse,collapse = "+")))
  mylogit<-glm(fullmod, data=Data.Frame, family=which.family)
  OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))
  RES<-cbind(summary(mylogit)$coefficients,OR.CI)
  cn<-colnames(RES)
  ## making res nicer
  
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  RES<-RES[-1,-c(2,3)]
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  RES[,2]<-format(RES[,2],nsmall=3)
  RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
  colnames(RES)<-c("Estimate", "P-value", "OR", "Lower OR", "Upper OR")
  OR.and.CI<-paste(format(round2(as.numeric(RES[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(RES[,"Lower OR"]),2),nsmall=2)," - ",
                   format(round2(as.numeric(RES[,"Upper OR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,OR.and.CI)
  RES<-RES[,c(1,2,6)]
  return(RES)

} ## names of people should be numeric


###########################################################################################
############### GLM BIVARIATE ADJUSTED LOGIT ##############################################
###########################################################################################
## Data<-as.matrix(dataRead)
## dependent_var=as.numeric(Data[,"HOLISTIC_TX_ever"])
## ColumnsToUse<-cn[-which(cn%in%c("code","survey","int_date","BEFORE_IDRUGS", "coh"))]
## ColumnsToUse<-ColumnsToUse[which(ColumnsToUse=="Indigenous"):length(ColumnsToUse)]
## ColumnsToUse.adj<-ColumnsToUse[-1]
## vector.OF.classes.num.fact=c("num","fact") ## is defined by default, cannot be changed
## adj_name # name of variable of interest
## which.family.adj="binomial"
###########################################################################################

GLM_LOGIT_Bivariate_Adjusted_Personal<-function(Data, ColumnsToUse.adj, dependent_var.adj, which.family.adj, adj_name){
RES<-matrix(NA,length(ColumnsToUse.adj),7)
for(i in 1:length(ColumnsToUse.adj)){
  temp.res<-GLM_LOGIT_Multivariate_Personal(Data, ColumnsToUse=c(ColumnsToUse.adj[i],adj_name), 
                                            dependent_var_vec =dependent_var.adj, 
                                            which.family=which.family.adj,
                                            vector.OF.classes.num.fact=c("num","num"))
  RES[i,]<-temp.res[grep(ColumnsToUse.adj[i],rownames(temp.res)),]
}
colnames(RES)<-colnames(temp.res)
rownames(RES)<-ColumnsToUse.adj
return(RES)
}

###########################################################################################
############### GLM BACKWARD AIC LOGIT ####################################################
###########################################################################################
## Data=as.matrix(dataRead)
## dependent_var<-as.numeric(Data[,"BEFORE_IDRUGS"])
## ColumnsToUse<-cn[-which(cn%in%c("code","survey","int_date","BEFORE_IDRUGS"))]
###########################################################################################

GLM_Backward_AIC_Personal<-function(Data, ColumnsToUse, Outcome_name, which.family,vector.OF.classes.num.fact){
  
  dependent_var_vec<-as.numeric(Data[,Outcome_name])
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
    
  if(length(dependent_var_vec)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{
  
  del<-unique(which(is.na(Data[,which(colnames(Data)%in%ColumnsToUse)]), arr.ind=T)[,1])
  if(length(del)>0){
  temp01.data<-Data[-del,]
  dependent_var_vec<-dependent_var_vec[-del]
  }else{
    temp01.data<-Data
  }

  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  var<-var.fact
  var[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]
  var.save<-var
  
  AIC.RES<-REMOVE.RES<-COL.NAMES<-array()
  AIC.RES[1:2]<-c(20000000,10000000)
  i=2
  
  while((AIC.RES[i]<AIC.RES[i-1])&length(var)>0){
    
    ind<-which(var=="as.numeric(age)")
    if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
    if(length(ind)==1){
      age_vec<-as.numeric(temp01.data[,"age"])
      fullmod<-as.formula( paste( "dependent_var_vec ~ age_vec+", paste(var[-ind],collapse = "+")))
      
    }else{
      fullmod<-as.formula( paste( "dependent_var_vec ~ ", paste(var,collapse = "+")))
    }
    
    r1<-glm(fullmod, data=as.data.frame(temp01.data),family=which.family)
    AIC.RES[i+1]<-AIC(r1)[1]
    res.tt<-as.matrix(summary(r1)$coefficients)
    rr<-rownames(res.tt)[(which(res.tt[-1,4]==max(res.tt[-1,4]))+1)] ## because we removed intercept
    remove.this.name<-rr
    if(remove.this.name!="age_vec"){
    remove.this.name<-paste(strsplit(remove.this.name,"[)]")[[1]][1],")", sep="")}else{
      remove.this.name<-"as.numeric(age)"
    }
    REMOVE.RES[i]<-remove.this.name
    var<- setdiff(var, remove.this.name)
    i=i+1
    print(i)
    print(AIC.RES)
    print(REMOVE.RES)
  }
  AIC.RES<-AIC.RES[-c(1:2)]
  REMOVE.RES<-REMOVE.RES[-1]
  
  RES<-cbind(AIC.RES, REMOVE.RES)
  colnames(RES)<-c("Before removing", "To remove for this AIC")
  RES.AIC<-RES
  ############################### Final model #################################
  
  vars.keep<- setdiff(var.save, REMOVE.RES[-c((length(REMOVE.RES)-1):length(REMOVE.RES))])
  
  ind<-which(vars.keep=="as.numeric(age)")
  if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
  if(length(ind)==1){
    age_vec<-as.numeric(temp01.data[,"age"])
    fullmod<-as.formula( paste( "dependent_var_vec ~ age_vec+", paste(vars.keep[-ind],collapse = "+")))
    
  }else{
    fullmod<-as.formula( paste( "dependent_var_vec ~ ", paste(vars.keep,collapse = "+")))
  }
  
  mylogit<-glm(fullmod, data=as.data.frame(temp01.data), family=which.family)
  OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))
  RES<-cbind(summary(mylogit)$coefficients,OR.CI)
  cn<-colnames(RES)
  
  ## making res nicer
  
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  colnames(RES)<-cn
  
  RES<-rbind(RES, "",c("AIC = ", AIC(mylogit), "", "","","",""))
  
  rn<-rownames(RES)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(RES)<-rn
  
  RES<-RES[-1,]
  
  rn<-RES.AIC[,2]
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  RES.AIC[,2]<-rn
 
  RES<-rbind(RES, "",c("Removed tests and corresponding AIC","", "", "","","",""),
             c(colnames(RES.AIC), "", "","","",""),cbind(RES.AIC, "", "","","",""))
  return(RES)  
  }
} ## names of people should be numeric
}

###########################################################################################
############### GLM CONFOUNDING AIC LOGIT #################################################
###########################################################################################
## Data=as.matrix(dataRead)
## dependent_var<-as.numeric(Data[,"BEFORE_IDRUGS"])
## ColumnsToUse<-c("heroin_l6m","obtain_from_doctor","EVER_SECC")
## vector.OF.classes.num.fact=rep("fact",length(ColumnsToUse))
## vector.OF.classes.num.fact[which(ColumnsToUse=="age")]<-"num"
## which.family="binomial"
## independent_var=as.factor(Data[,"OLDER_AGE"])
###########################################################################################

GLM_Confounding_Personal<-function(Data.F, ColumnsToUse, outcome_name, main_explan_name, which.family,threshold){


names(vector.OF.classes.num.fact)<-ColumnsToUse
del<-unique(which(is.na(Data.F[,which(colnames(Data.F)%in%ColumnsToUse)]), arr.ind=T)[,1])
Data.F<-Data.F[-del,]

if(length(del)>0){
  Data.F<-Data.F[-del,]
}

vars<-ColumnsToUse
ColumnsToUse.fact<-vars

fullmod<-as.formula( paste( outcome_name, " ~ ", main_explan_name,"+", paste(vars, collapse = "+")))
model.full<-glm(fullmod, data=Data.F,family=which.family)
res.temp.t<-summary(model.full)$coefficients
ind_get<-grep(main_explan_name,rownames(res.temp.t))
COEF.full<-as.matrix(res.temp.t)[ind_get,1]
COEF.to.save<-matrix(NA,1,3)
COEF.to.save[1,]<-c(COEF.full,0,0)
colnames(COEF.to.save)<-c("Estimate", "Delta", "abs Delta")
removed.variables<-"full model"

while(COEF.to.save[dim(COEF.to.save)[1],3]<=threshold){
  COEF.updated<-matrix(NA, length(vars), 3)
  rownames(COEF.updated)<-vars
  for (j in 1:length(vars)){
    
    vars.updated<-vars[-j]
    temp.model.formula<-as.formula( paste( outcome_name," ~ ",main_explan_name,"+", paste(vars.updated, collapse = "+")))
    model.updated <- glm(temp.model.formula, data=Data.F,family=which.family)
    res.temp.t<-summary(model.updated)$coefficients
    ind_get<-grep(main_explan_name,rownames(res.temp.t))
    COEF.updated[j,1]<-as.matrix(res.temp.t)[ind_get,1]
    
    print(length(vars)-j)  
  }
  Delta<-((COEF.updated[,1]/COEF.full[1])-1)*100
  COEF.updated<-cbind(COEF.updated[,1],Delta, abs(Delta))
  remove.this<-which.min(abs(Delta))
  
  COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
  removed.variables<-c(removed.variables,names(remove.this))
  ## updating
  
  vars.save<-vars
  vars<-vars[-remove.this]
  remove.this
  
  fullmod<-as.formula( paste( outcome_name," ~ ",main_explan_name,"+",paste(vars, collapse = "+")))
  model.full <- glm(fullmod, data=Data.F,family=which.family)
  res.temp.t<-summary(model.full)$coefficients
  ind_get<-grep(main_explan_name,rownames(res.temp.t))
  COEF.full<-as.matrix(res.temp.t)[ind_get,1]
  COEF.updated[order(as.numeric(COEF.updated[,3])),]
  COEF.to.save
}

rownames(COEF.to.save)<-removed.variables

fullmod<-as.formula( paste( outcome_name," ~ ",main_explan_name,"+",paste(vars.save, collapse = "+")))

model.full <- glm(fullmod, data=Data.F,family=which.family)  

final.result<-as.matrix(summary(model.full)$coeff)

OR.CI<-exp(cbind(OR = coef(model.full), confint(model.full, level=0.95)))
RES<-cbind(final.result,OR.CI)

RES<-RES[-1,]

COEF.to.save<-rbind(c("Multivariate model","","","","","",""), colnames(RES),RES,"",c(colnames(COEF.to.save), rep("",4)),cbind(COEF.to.save,"","","",""))

return(COEF.to.save)

}
 
###################################################################

LINEAR_REGRESSION<-function(Data,ColumnsToUse, Outcome, variable.fact.num, factor.levels){
  ColumnsToUse_fact<-NULL
  ColumnsToUse_num<-ColumnsToUse[which(variable.fact.num=="num")]
  if(length(which(variable.fact.num=="fact"))>0){
    ColumnsToUse_fact<-ColumnsToUse[which(variable.fact.num=="fact")]
    factor.levels<-factor.levels[which(variable.fact.num=="fact")]
  }
  ## numeric part
  
  RES<-matrix(NA, length(ColumnsToUse_num),9) 
  for(i in 1:length(ColumnsToUse_num)){
    
    temp01<-as.numeric(Data[,ColumnsToUse_num[i]])
    if(length(table(temp01))>1){
      
      mylogit<-lm(Outcome~temp01)
      OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))[2,]
      Beta.CI<-cbind(coef(mylogit), confint(mylogit, level=0.95))[2,]
      names(OR.CI) <- c("OR", "Lower OR", "Upper OR")
      names(Beta.CI) <- c("Estimate", "CI 5%", "CI 95%")
      Rest.of.result<-summary(mylogit)$coefficients[2,-1]
      names(Rest.of.result)<-c("SE", "t value", "p-value")
      RES[i,]<-c(Beta.CI,Rest.of.result,OR.CI)
      }
    
  }
  
  rownames(RES)<-ColumnsToUse_num
  colnames(RES)<-names(c(Beta.CI,Rest.of.result,OR.CI))
  
  # factors
  
  if(length(ColumnsToUse_fact)>0){
    
    RES.add<-rep(NA, 9)
    for(i in 1:length(ColumnsToUse_fact)){
      
      temp01<-as.factor(Data[,ColumnsToUse_fact[i]])
      temp01[which(temp01=="")]<-NA
      temp01<-relevel(temp01, ref=factor.levels[i])
      mylogit<-lm(Outcome~temp01)
      OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))[-1,]
      colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
      Beta.CI<-cbind(coef(mylogit), confint(mylogit, level=0.95))[-1,]
      colnames(Beta.CI) <- c("Estimate", "CI 5%", "CI 95%")
      Rest.of.result<-summary(mylogit)$coefficients[-1,-1]
      colnames(Rest.of.result)<-c("SE", "t value", "p-value")
      temp<-cbind(Beta.CI,Rest.of.result,OR.CI)
      RES.add<-rbind(RES.add,temp)
      
    }
    
    RES<-rbind(RES,RES.add[-1,])
  }
  
  return(RES)
}

###################################################################

ORDINARY<-function(Data, ColumnsToUse, Outcome, variable.fact.num, factor.levels){
  library(MASS)
  
  ColumnsToUse_num<-ColumnsToUse[which(variable.fact.num=="num")]
  if(length(which(variable.fact.num=="fact"))>0){
    ColumnsToUse_fact<-ColumnsToUse[which(variable.fact.num=="fact")]
    factor.levels<-factor.levels[which(variable.fact.num=="fact")]
  }
  ## numeric part
  
  RES<-matrix(NA, length(ColumnsToUse_num),9) 
  for(i in 1:length(ColumnsToUse_num)){
    
    temp01<-as.numeric(Data[,ColumnsToUse_num[i]])
    if(length(table(temp01))>1){
      
      mylogit<-polr(factor(Outcome, ordered = T)~temp01,method="logistic")
      ctable <- coef(summary(mylogit))
      p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # http://r.789695.n4.nabble.com/No-P-values-in-polr-summary-td4678547.html
      ctable <- cbind(ctable, "p value" = p)[1,]
      ci <- confint(mylogit, level = 0.95)
      OR<-exp(c(OR = coef(mylogit, level = 0.95), ci))
      names(OR)<-c("OR","lower OR", "upper OR")
      t<-c(ctable, ci, OR)
      RES[i,]<-t
      #P.new.calc<-dropterm(mylogit, test = "Chisq") # https://groups.google.com/forum/#!topic/r-help-archive/_JImzihIpbQ
      #P.new[i]<-P.new.calc[[4]][2]

    }
    
  }
  rownames(RES)<-ColumnsToUse_num
  colnames(RES)<-names(t)
  
  # factors
  
  if(length(ColumnsToUse_fact)>0){
    
    RES.add<-rep(NA, 9)
    rn=NA
    for(i in 1:length(ColumnsToUse_fact)){
      
      temp01<-as.factor(Data[,ColumnsToUse_fact[i]])
      temp01<-relevel(temp01, ref=factor.levels[i])
      mylogit<-polr(factor(Outcome, ordered = T)~temp01,method="logistic")
      ctable <- coef(summary(mylogit))
      p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
      ctable <- cbind(ctable, "p value" = p)[1:2,]
      ci <- confint(mylogit, level = 0.95)
      OR<-exp(cbind(OR = coef(mylogit, level = 0.95), ci))
      colnames(OR)<-c("OR","lower OR", "upper OR")
      res_temp<-cbind(ctable, ci, OR)
      rn<-c(rn,rownames(res_temp))
      
      RES.add<-rbind(RES.add,res_temp)
      
    }
    
    RES<-rbind(RES,RES.add[-1,])
  }
  RES<-RES[,-c(2,5,6)]
  return(RES)
}

##########################################################################
################# LM Backward

LM_Backward_AIC_Personal<-function(Data, ColumnsToUse, Outcome_vector, vector.OF.classes.num.fact){
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
    
  if(length(Outcome_vector)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{ 
  
  var.fact<-paste("as.factor(", ColumnsToUse, ")",sep="")
  var.num<-paste("as.numeric(", ColumnsToUse, ")",sep="")
  var<-var.fact
  var[which(vector.OF.classes.num.fact=="num")]<-var.num[which(vector.OF.classes.num.fact=="num")]
  var.save<-var
  
  AIC.RES<-REMOVE.RES<-COL.NAMES<-array()
  AIC.RES[1:2]<-c(20000000,10000000)
  i=2
  
  while(AIC.RES[i]<AIC.RES[i-1]){
    fullmod<-as.formula( paste( "Outcome_vector ~ ", paste(var,collapse = "+")))
    r1<-lm(fullmod, data=as.data.frame(Data))
    AIC.RES[i+1]<-AIC(r1)[1]
    res.tt<-as.matrix(summary(r1)$coefficients)
    rr<-rownames(res.tt)[(which(res.tt[-1,4]==max(res.tt[-1,4]))+1)] ## because we removed intercept
    remove.this.name<-rr
    remove.this.name<-paste(strsplit(remove.this.name,"[)]")[[1]][1],")", sep="")
    REMOVE.RES[i]<-remove.this.name
    var<- setdiff(var, remove.this.name)
    i=i+1
    print(i)
    print(AIC.RES)
    print(REMOVE.RES)
  }
  AIC.RES<-AIC.RES[-c(1:2)]
  REMOVE.RES<-REMOVE.RES[-1]
  
  RES<-cbind(AIC.RES, REMOVE.RES)
  colnames(RES)<-c("Before removing", "To remove for this AIC")
  RES.AIC<-RES
  ############################### Final model #################################
  
  vars.keep<- setdiff(var.save, REMOVE.RES[-c((length(REMOVE.RES)-1):length(REMOVE.RES))])
  
  ind<-which(vars.keep=="as.numeric(age)")
  if(length(ind)>1){print("REMOVE AGE FROM THE DATA SET")}
  if(length(ind)==1){
    age_vec<-as.numeric(Data[,"age"])
    fullmod<-as.formula( paste( "Outcome_vector ~ age_vec+", paste(vars.keep[-ind],collapse = "+")))
    
  }else{
    fullmod<-as.formula( paste( "Outcome_vector ~ ", paste(vars.keep, collapse = "+")))
  }
  
  #fullmod<-as.formula( paste( "Outcome_vector ~", paste(vars.keep, collapse = "+")))
  mylogit<-lm(fullmod, data=as.data.frame(Data))
  OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))
  Beta.CI<-cbind(coef(mylogit), confint(mylogit, level=0.95))
  SS<-summary(mylogit)$coefficients[,-1]
  RES<-cbind(Beta.CI[match(rownames(SS), rownames(Beta.CI)),],SS,OR.CI[match(rownames(SS), rownames(OR.CI)),])
  cn<-c("Estimate","CI 5%", "CI 95%", "SE","t value","p-value","OR","CI 5%","CI 95%")
  
  ## making res nicer
  
  RES<-sapply(1:dim(RES)[2], function(x) RES[,x]<-round2(RES[,x],3))
  colnames(RES)<-cn
  
  RES<-rbind(RES, "",c("AIC = ", AIC(mylogit), "", "","","","","",""))
  
  rn<-rownames(RES)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(RES)<-rn
  
  RES<-RES[-1,]
  
  rn<-RES.AIC[,"To remove for this AIC"]
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  RES.AIC[,"To remove for this AIC"]<-rn
  
  RES<-rbind(RES, "",c("Removed tests and corresponding AIC","", "", "","","","","",""),
             c(colnames(RES.AIC), "", "","","","","",""),cbind(RES.AIC, "", "","","","","",""))
  return(RES) 
  }
} ## names of people should be numeric
}

###########################################################################################

LM_Confounding_Personal<-function(Data, ColumnsToUse, dependent_var, independent_var, vector.OF.classes.num.fact){
  
  N.of.similar.cols<-array()
  for(i in 1:length(ColumnsToUse)){
    N.of.similar.cols[i]<-length(grep(ColumnsToUse[i],ColumnsToUse))
  }
  if(max(N.of.similar.cols)>1){
    
    rename.these<-which(N.of.similar.cols>1)
    print(paste("Please, rename these columns to something unique: " ,ColumnsToUse[rename.these],sep=""))
  }else{
    
  if(length(dependent_var)!=dim(Data)[1]|length(independent_var)!=dim(Data)[1]){print("Data and vectors have different sizes")}else{ 
  
  names(vector.OF.classes.num.fact)<-ColumnsToUse
  del<-unique(which(is.na(Data[,which(colnames(Data)%in%ColumnsToUse)]), arr.ind=T)[,1])
  if(length(del)>0){
    dependent_var01<-dependent_var[-del]
    temp01.data<-Data[-del,]
    independent_var01<-independent_var[-del]
  }else{
    dependent_var01<-dependent_var
    temp01.data<-Data
    independent_var01<-independent_var
  }
  
  vars<-ColumnsToUse
  ColumnsToUse.fact<-vars
  
  vars.factor<-paste("as.factor(",vars,")",sep="")
  vars.numeric<-paste("as.numeric(",vars,")",sep="")
  vars[which(vector.OF.classes.num.fact=="fact")]<-vars.factor[which(vector.OF.classes.num.fact=="fact")]
  vars[which(vector.OF.classes.num.fact=="num")]<-vars.numeric[which(vector.OF.classes.num.fact=="num")]
  ColumnsToUse.fact<-vars
  
  fullmod<-as.formula( paste( "dependent_var01 ~ independent_var01+", paste(vars, collapse = "+")))
  model.full<-lm(fullmod, data=as.data.frame(temp01.data))
  res.temp.t<-summary(model.full)$coefficients
  ind_get<-c(grep("independent_var011",rownames(res.temp.t)),grep("independent_var01",rownames(res.temp.t)))[1]
  COEF.full<-as.matrix(res.temp.t)[ind_get,1]
  COEF.to.save<-matrix(NA,1,3)
  COEF.to.save[1,]<-c(COEF.full,0,0)
  colnames(COEF.to.save)<-c("Estimate", "Delta", "abs Delta")
  removed.variables<-"full model"
  
  while(COEF.to.save[dim(COEF.to.save)[1],3]<=5){
    COEF.updated<-matrix(NA, length(vars), 3)
    rownames(COEF.updated)<-vars
    for (j in 1:length(vars)){
      
      vars.updated<-vars[-j]
      temp.model.formula<-as.formula( paste( "dependent_var01 ~ independent_var01+", paste(vars.updated, collapse = "+")))
      model.updated <- lm(temp.model.formula, data=as.data.frame(temp01.data))
      res.temp.t<-summary(model.updated)$coefficients
      ind_get<-c(grep("independent_var011",rownames(res.temp.t)),grep("independent_var01",rownames(res.temp.t)))[1]
      COEF.updated[j,1]<-as.matrix(res.temp.t)[ind_get,1]
      
      print(length(vars)-j)  
    }
    Delta<-((COEF.updated[,1]/COEF.full[1])-1)*100
    COEF.updated<-cbind(COEF.updated[,1],Delta, abs(Delta))
    remove.this<-which.min(abs(Delta))
    
    COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
    removed.variables<-c(removed.variables,names(remove.this))
    ## updating
    
    vars.save<-vars
    vars<-vars[-remove.this]
    
    fullmod<-as.formula( paste( "dependent_var01 ~ independent_var01+",paste(vars, collapse = "+")))
    model.full <- lm(fullmod, data=as.data.frame(temp01.data))
    res.temp.t<-summary(model.full)$coefficients
    ind_get<-c(grep("independent_var011",rownames(res.temp.t)),grep("independent_var01",rownames(res.temp.t)))[1]
    COEF.full<-as.matrix(res.temp.t)[ind_get,1]
    
    COEF.to.save
  }
  
  rownames(COEF.to.save)<-removed.variables
  
  fullmod<-as.formula( paste( "dependent_var01 ~ independent_var01+", paste(vars.save, collapse = "+")))
  model.full <- lm(fullmod, data=as.data.frame(temp01.data))  
  
  final.result<-as.matrix(summary(model.full)$coeff)
  
  OR.CI<-exp(cbind(OR = coef(model.full), confint(model.full, level=0.95)))
  RES<-cbind(final.result,OR.CI)
  
  rn<-rownames(RES)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(RES)<-rn
  
  RES<-RES[-1,]
  
  rn<-rownames(COEF.to.save)
  rn<-gsub("as.factor", "",rn)
  rn<-gsub("as.numeric", "",rn)
  rn<-gsub("[()]", "",rn)
  rownames(COEF.to.save)<-rn
  
  COEF.to.save<-rbind(c("Multivariate model","","","","","",""), colnames(RES),RES,"",c(colnames(COEF.to.save), rep("",4)),cbind(COEF.to.save,"","","",""))
  
  return(COEF.to.save)
  }
}

}
#####################################################################################
###################### Bivariate GLMM ###############################################
#####################################################################################

GLMM_Bivariate<-function(Data_frame,ColumnsToUse,Outcome_name, ID_name,which.family, NAGQ){
  
  library("lme4")
  
  FinalRes<-rep(NA,9)
  rn<-NA
  for(i in 1:length(ColumnsToUse)){
    
    print(paste(i," ", ColumnsToUse[i],sep=""))
    
    fullmod<-as.formula( paste( Outcome_name, "~", ColumnsToUse[i],"+(1|",ID_name,")",sep=""))
    myfit<-glmer(fullmod, family=which.family,na.action = na.exclude, data=Data_frame,nAGQ=NAGQ)
    
    cc <- confint(myfit,parm="beta_",method="Wald") 
    ctab <- cbind(est=fixef(myfit),cc)
    rtab <- exp(ctab)
    
    Coef<-summary(myfit)$coefficients
    CI<-confint(myfit, level = 0.95,method = "Wald")
    RES<-cbind(Coef[,"Estimate"],CI[match(rownames(Coef), rownames(CI)),],Coef[,-1],rtab)
    colnames(RES)[1]<-"Estimate"
    colnames(RES)[7:9]<-c("OR","Lower 95%","Upper 95%")
    colnames(RES)[6]<-"p-value"
    colnames(RES)[1:3]<-c("Beta","Lower 95%","Upper 95%")
    FinalRes<-rbind(FinalRes,RES[-1,])
    rn<-c(rn, rownames(Coef)[-1])
    
  }
  rownames(FinalRes)<-rn
  FinalRes<-FinalRes[-1,]
  return(FinalRes)
}

#####################################################################################
###################### Bivariate GLMM NB ############################################
#####################################################################################

GLMM_Bivariate_NB<-function(Data_frame,ColumnsToUse,Outcome_name, ID_name,which.family){
  
  #install.packages("R2admb")
  #install.packages("glmmADMB", repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),type="source")
  
  Data_frame$CODE<-as.factor(Data_frame$CODE)
  
  library(glmmADMB)
  library(R2admb)
  
  FinalRes<-rep(NA,9)
  rn<-NA
  for(i in 1:length(ColumnsToUse)){
    
    Data_frame.no.na<-na.omit(Data_frame[,c(ID_name,Outcome_name,ColumnsToUse[i])])
    print(paste(i," ", ColumnsToUse[i],sep=""))
    
    fullmod<-as.formula( paste( Outcome_name, "~", ColumnsToUse[i],"+(1|",ID_name,")",sep=""))
    myfit<-glmmadmb(fullmod, data=Data_frame.no.na, which.family)
    
    cc <- confint(myfit,method="Wald") 
    ctab <- cbind(est=fixef(myfit),cc)
    rtab <- exp(ctab)
    
    Coef<-summary(myfit)$coefficients
    CI<-confint(myfit, level = 0.95,method = "Wald")
    RES<-cbind(Coef[,"Estimate"],CI[match(rownames(Coef), rownames(CI)),],Coef[,-1],rtab)
    colnames(RES)[1]<-"Estimate"
    colnames(RES)[7:9]<-c("OR","Lower 95%","Upper 95%")
    colnames(RES)[6]<-"p-value"
    colnames(RES)[1:3]<-c("Beta","Lower 95%","Upper 95%")
    FinalRes<-rbind(FinalRes,RES[-1,])
    rn<-c(rn, rownames(Coef)[-1])
    
  }
  rownames(FinalRes)<-rn
  FinalRes<-FinalRes[-1,]
  return(FinalRes)
}

#####################################################################################
###################### Bivariate linear GLMM ########################################
#####################################################################################

GLMM_Bivariate_linear<-function(Data_frame,ColumnsToUse,Outcome_name, ID_name){
  
  library(lmerTest)
  library(lme4)
  
  require(MASS)
  
  FinalRes<-rep(NA,5)
  rn<-NA
  for(i in 1:length(ColumnsToUse)){
    
    print(paste(i," ", ColumnsToUse[i],sep=""))
    
    fullmod<-as.formula( paste( Outcome_name, "~", ColumnsToUse[i],"+(1|",ID_name,")",sep=""))
    myfit<-lmer(fullmod, na.action = na.exclude, data=Data_frame)
    
    p<-summary(myfit)$coef[,c("t value","Pr(>|t|)")]
    
    cc <- confint(myfit,parm="beta_",method="Wald") 
    ctab <- cbind(est=fixef(myfit),cc,p)
    
    RES<-ctab
    rn<-c(rn,rownames(RES)[-1])
    FinalRes<-rbind(FinalRes,RES[-1,])
    
    
  }
  rownames(FinalRes)<-rn
  FinalRes<-FinalRes[-1,]
  
  OR<-exp(FinalRes[,1:3]) 
  colnames(OR)<-c("OR","lower OR","upper OR")
  OR.and.CI<-paste(format(round2(OR[,1],2),nsmall=2)," (",format(round2(OR[,2],2),nsmall=2)," - ",format(round2(OR[,3],2),nsmall=2),")",sep="")
  est<-round2(FinalRes[,"est"],3)
  P_value<-round2(FinalRes[,"Pr(>|t|)"],3)
  P_value[which(P_value<0.001)]<-"<0.001"
  
  FinalRes<-cbind(est,OR.and.CI,P_value)
  
  return(FinalRes)
}

#####################################################################################
###################### Confounding GLMM AIC #########################################
#####################################################################################

GLMM_Confounding<-function(Data.F, ColumnsToUse, Outcome_name, ID_name, independent_var_name, which.family,threshold, NAGQ){  # threshold=5
  
  library(lmerTest)
  library(lme4)
  
  ColumnsToUse.initial<-ColumnsToUse
  
  Data.F[,ID_name]<-gsub("A",9999,Data.F[,ID_name])
  vars<-ColumnsToUse
  
  not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse)]),arr.ind=T)[,1])==F) #getting not NA
  Data.F_full<-subset(Data.F,not.NA.id)
  
  fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars, collapse = "+"),"+(1|",ID_name,")"))
  model.full<-glmer(fullmod, data=Data.F_full, family=which.family,na.action = na.exclude,nAGQ=NAGQ)
  
  SS<-summary(model.full)$coeff
  coeff.indep.ind<-grep(independent_var_name, rownames(SS))
  MAX.coeff<-which.max(abs(SS[coeff.indep.ind,1])) ## when indep_var is factor, we pick max coef of its levels
  names(MAX.coeff)<-rownames(SS)[coeff.indep.ind[MAX.coeff]]
  MAX.coeff.name<-names(MAX.coeff) ## leading factor
  COEF.full<-SS[MAX.coeff.name,1]
  
  COEF.to.save<-matrix(NA,1,3)
  COEF.to.save[1,]<-c(COEF.full,0,0)
  colnames(COEF.to.save)<-c("Estimate", "Delta", "abs Delta")
  removed.variables<-"full model"
  
  #### removing variables
  
  while(COEF.to.save[dim(COEF.to.save)[1],3]<=threshold){
    COEF.updated<-matrix(NA, length(vars), 3)
    rownames(COEF.updated)<-vars
    for (j in 1:length(vars)){
      
      vars.updated<-vars[-j]
      
      ColumnsToUse_temp<-ColumnsToUse[-j]
      not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse_temp)]),arr.ind=T)[,1])==F) #getting not NA
      Data.F_temp<-subset(Data.F,not.NA.id)
      
      temp.model.formula<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars.updated, collapse = "+"),"+(1|",ID_name,")"))
      model.updated<-glmer(temp.model.formula, data=Data.F_temp, family=which.family,na.action = na.exclude,nAGQ=NAGQ)
      
      SS<-summary(model.updated)$coeff
      COEF.updated[j,1]<-SS[MAX.coeff.name,1]
      
      print(length(vars)-j)  
    }
    Delta<-(COEF.updated[,1]/COEF.full[1]-1)*100
    COEF.updated<-cbind(COEF.updated[,1],Delta, abs(Delta))
    remove.this<-which.min(abs(Delta))
    
    COEF.to.save<-rbind(COEF.to.save, COEF.updated[remove.this,])
    removed.variables<-c(removed.variables,names(remove.this))
    
    ## updating
    
    vars.save<-vars
    ColumnsToUse.save<-ColumnsToUse
    
    vars<-vars[-remove.this]
    ColumnsToUse<-ColumnsToUse[-remove.this]
    
    not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse)]),arr.ind=T)[,1])==F) #getting not NA
    Data.F_next<-subset(Data.F,not.NA.id)
    
    fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars, collapse = "+"),"+(1|",ID_name,")"))
    model.full<-glmer(fullmod, data=Data.F_next, family=which.family,na.action = na.exclude,nAGQ=NAGQ)

    SS<-summary(model.full)$coeff
    COEF.full<-SS[MAX.coeff.name,1]
    
    COEF.to.save
  }
  
  rownames(COEF.to.save)<-removed.variables
  
  not.NA.id<-(1:dim(Data.F)[1]%in%unique(which(is.na(Data.F[,c(Outcome_name,independent_var_name,ColumnsToUse.save)]),arr.ind=T)[,1])==F) #getting not NA

  fullmod<-as.formula( paste( Outcome_name, " ~ ",independent_var_name,"+", paste(vars.save, collapse = "+"),"+(1|",ID_name,")"))
  model.full<-glmer(fullmod, data=Data.F, family=which.family,na.action = na.exclude,nAGQ=NAGQ)
  
  cc <- confint(model.full,parm="beta_",method="Wald") 
  ctab <- cbind(est=fixef(model.full),cc)
  OR.CI <- exp(ctab)


  # Odds ratio and confID_vecence intervals

  colnames(OR.CI) <- c("OR", "Lower OR", "Upper OR")
  
  final.result<-cbind(as.matrix(summary(model.full)$coeff)[-1,c(1,4)],OR.CI[-1,])
  Procedure<-c(colnames(COEF.to.save))
  final.result[,1:2]<-sapply(1:2, function(x) round2(final.result[,x],3))
  final.result[,3:5]<-sapply(3:5, function(x) round2(final.result[,x],2))
  final.result[,2]<-format(final.result[,2],nsmall=3)
  final.result[which(as.numeric(final.result[,2])<0.001),2]<-"<0.001"
  
  OR.and.CI<-paste(format(round2(as.numeric(final.result[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(final.result[,"Lower OR"]),2),nsmall=2),"-",
                   format(round2(as.numeric(final.result[,"Upper OR"]),2),nsmall=2),")",sep="")
  final.result<-cbind(final.result[,c(1:2)],OR.and.CI)
  
  COEF.to.save[,1:3]<-sapply(1:3, function(x) round2(COEF.to.save[,x],3))
  COEF.to.save<-rbind(final.result,"",Procedure,COEF.to.save)
  
  colnames(COEF.to.save)<-c("Estimate","P-value","OR.and.CI")
  
  return(COEF.to.save)
  
  
}

#####################################################################################
###################### Explanatory GLMM AIC #########################################
#####################################################################################

GLMM_Backward_AIC<-function(Data_frame, ColumnsToUse, Outcome_name, ID_name, which.family,NAGQ){
  
library("lme4")
  
  vars<-ColumnsToUse
  vars.ALL<-vars
  
  AIC.RES<-REMOVE.RES<-COL.NAMES<-array()
  AIC.RES[1]<-NA
  AIC.RES_p<-NA

  #while(AIC.RES[i]<AIC.RES[i-1]){
  for(i in 1:(length(vars)-1)){
  
    fullmod<-as.formula( paste( Outcome_name, "~", paste(vars, collapse = "+"),"+(1|",ID_name,")"))
    myfit<-glmer(fullmod, family=which.family,na.action = na.exclude, data=Data_frame,nAGQ=NAGQ)
    ss<-as.matrix(summary(myfit)$coefficients)
    AIC.RES[i+1]<-AIC(myfit)[1]
    remove.this.id.name<-names(which(ss[-1,"Pr(>|z|)"]==max(ss[-1,"Pr(>|z|)"])))
    remove.this.id.name<-remove.this.id.name[1]
    AIC.RES_p<-c(AIC.RES_p,round2(max(ss[-1,"Pr(>|z|)"]),2))
    remove.this.id.name.clean<-ColumnsToUse[sapply(1:length(ColumnsToUse), function(x) grepl(ColumnsToUse[x],remove.this.id.name))]
    remove.this.id<-sapply(1:length(vars), function(x) grepl(remove.this.id.name.clean,vars[x]))
    REMOVE.RES[i]<-vars[remove.this.id]
    vars<- setdiff(vars, vars[remove.this.id])
    print(i)
    print(AIC.RES)
    print(REMOVE.RES)
  }
  AIC.RES<-AIC.RES[-1]
  AIC.RES_p<-AIC.RES_p[-1]
 
  RES<-cbind(AIC.RES, AIC.RES_p, REMOVE.RES)
  colnames(RES)<-c("AIC Before removal","p Before removal", "Removed variable")
  RES.AIC<-RES
  
  Top<-(which.min(as.numeric(RES[,"AIC Before removal"]))-1)
  if(Top==0){vars.keep<-vars.ALL}else{
  vars.keep<-vars.ALL[which(vars.ALL%in%(RES[1:Top,"Removed variable"])==F)]}
  
  ############################### Final model #################################
  
  fullmod<-as.formula( paste( Outcome_name, "~ ", paste(vars.keep, collapse = "+"),"+(1|",ID_name,")"))

  myfit<-glmer(fullmod, family=which.family,na.action = na.exclude, data=Data_frame,nAGQ=NAGQ)
  cc <- confint(myfit,parm="beta_",method="Wald") 
  ctab <- cbind(est=fixef(myfit),cc)
  rtab <- exp(ctab)
  
  Coef<-summary(myfit)$coefficients
  CI<-confint(myfit, level = 0.95,method = "Wald")
  RES<-cbind(Coef[,"Estimate"],CI[match(rownames(Coef), rownames(CI)),],Coef[,-1],rtab)
  colnames(RES)[1]<-"Estimate"
  colnames(RES)[7:9]<-c("OR","Lower 95%","Upper 95%")
  colnames(RES)[6]<-"p-value"
  colnames(RES)[1:3]<-c("Beta","Lower 95%","Upper 95%")
  RES<-RES[-1,]

  ## making res nicer
  
  RES<-RES[,-c(2:5)]
  RES[,c(1:2)]<-sapply(1:2, function(x) RES[,x]<-round2(RES[,x],3))
  RES[,c(3:5)]<-sapply(3:5, function(x) RES[,x]<-round2(RES[,x],2))
  RES[,2]<-format(RES[,2],nsmall=3)
  RES[which(as.numeric(RES[,2])<0.001),2]<-"<0.001"
  
  colnames(RES)<-c("Beta", "P-value", "OR", "Lower OR", "Upper OR")
  OR.and.CI<-paste(format(round2(as.numeric(RES[,"OR"]),2),nsmall=2)," (",
                   format(round2(as.numeric(RES[,"Lower OR"]),2),nsmall=2)," -",
                   format(round2(as.numeric(RES[,"Upper OR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,OR.and.CI)
  RES<-RES[,c("Beta","P-value","OR.and.CI")]
  
  RES<-rbind(RES,"",c("Procedure","",""),c("AIC","p-value","variable removed"),RES.AIC)
  return(RES)  
  
}

######################################################################################
######################################################################################

GLMM_Bivariate_Adjusted<-function(Data_frame,ColumnsToUse,Columns.to.adj.for,dependent_var_name, ID_name,which.family){
  
  library("lme4")
  
  FinalRes<-rep(NA,6)
  rn<-NA
  for(i in 1:length(ColumnsToUse)){
    
    fullmod<-as.formula( paste( dependent_var_name, "~", ColumnsToUse[i],"+",paste(Columns.to.adj.for, collapse = "+"),"+(1|",ID_name,")",sep=""))
    myfit<-glmer(fullmod, family=which.family,na.action = na.exclude, data=Data_frame,nAGQ=20)
    
    Coef<-summary(myfit)$coefficients
    CI<-confint(myfit, level = 0.95,method = "Wald")
    RES<-cbind(Coef[,"Estimate"],CI[match(rownames(Coef), rownames(CI)),],Coef[,-1])
    colnames(RES)[1]<-"Estimate"
    FinalRes<-rbind(FinalRes,RES[ColumnsToUse[i],])
    rn<-c(rn, ColumnsToUse[i])
    print(length(ColumnsToUse)-i)
    
  }
  rownames(FinalRes)<-rn
  FinalRes<-FinalRes[-1,]
  return(FinalRes)
}

######################################################################################
######################################################################################

GLMM_Multivariable<-function(Data_frame,ColumnsToUse,dependent_var_name, ID_name,which.family, NAGQ){
  
  library("lme4")
  
  fullmod<-as.formula( paste( dependent_var_name, "~", paste(ColumnsToUse, collapse = "+"),"+(1|",ID_name,")",sep=""))
  myfit<-glmer(fullmod, family=which.family,na.action = na.exclude, data=Data_frame,nAGQ=NAGQ)
  
  cc <- confint(myfit,parm="beta_",method="Wald") 
  ctab <- cbind(est=fixef(myfit),cc)
  rtab <- exp(ctab)
  
  Coef<-summary(myfit)$coefficients
  CI<-confint(myfit, level = 0.95,method = "Wald")
  RES<-cbind(Coef[,"Estimate"],CI[match(rownames(Coef), rownames(CI)),],Coef[,-1],rtab)
  colnames(RES)[1]<-"Estimate"
  colnames(RES)[7:9]<-c("OR","Lower 95%","Upper 95%")
  colnames(RES)[6]<-"p-value"
  colnames(RES)[1:3]<-c("Beta","Lower 95%","Upper 95%")
    
  RES<-RES[-1,]
  return(RES)
}

######################################################################################
######################################################################################

GLMM_Multivariable_linear<-function(Data_frame,ColumnsToUse,dependent_var_name, ID_name){
  
  library("lme4")
  library(lmerTest)
  
  fullmod<-as.formula( paste( dependent_var_name, "~", paste(ColumnsToUse, collapse = "+"),"+(1|",ID_name,")",sep=""))
  myfit<-lmer(fullmod, na.action = na.exclude, data=Data_frame)
  
  cc <- confint(myfit,parm="beta_",method="Wald") 
  ctab <- cbind(est=fixef(myfit),cc)
  rtab <- exp(ctab)
  
  #p<-summary(myfit)$coef[,c("t value","Pr(>|t|)")]
  
  Coef<-summary(myfit)$coefficients
  CI<-confint(myfit, level = 0.95,method = "Wald")
  RES<-cbind(Coef[,c("Estimate","Pr(>|t|)")],rtab)
  colnames(RES)[3:5]<-c("OR","Lower 95%","Upper 95%")
  colnames(RES)[2]<-"p-value"
  RES<-RES[-1,]
  
  OR.and.CI<-paste(format(round2(RES[,"OR"],2),nsmall=2)," (",format(round2(RES[,"Lower 95%"],2),nsmall=2)," - ",format(round2(RES[,"Upper 95%"],2),nsmall=2),")",sep="")
  Estimate<-round2(RES[,"Estimate"],3)
  P_value<-round2(RES[,"p-value"],3)
  P_value[which(P_value<0.001)]<-"<0.001"
    
  FinalRes<-cbind(Estimate,OR.and.CI,P_value)
    
    return(FinalRes)
}

#####################################################################################
###################### Bivariate GLMM NB ############################################
#####################################################################################

GLMM_Multivariable_NB<-function(Data_frame,ColumnsToUse,Outcome_name, ID_name,which.family){
  
  #install.packages("R2admb")
  #install.packages("glmmADMB", repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),type="source")
  
  Data_frame$CODE<-as.factor(Data_frame$CODE)
  
  library(glmmADMB)
  library(R2admb)
  
  Data_frame.no.na<-na.omit(Data_frame[,c(ID_name,Outcome_name,ColumnsToUse)])

  fullmod<-as.formula( paste( Outcome_name, "~", paste(ColumnsToUse, collapse = "+"),"+(1|",ID_name,")",sep=""))
  myfit<-glmmadmb(fullmod, data=Data_frame.no.na, which.family)
    
  cc <- confint(myfit,method="Wald") 
  ctab <- cbind(est=fixef(myfit),cc)
  rtab <- exp(ctab)
    
  Coef<-summary(myfit)$coefficients
  CI<-confint(myfit, level = 0.95,method = "Wald")
  RES<-cbind(Coef[,c("Estimate","Pr(>|z|)")],rtab)
  colnames(RES)[3:5]<-c("OR","Lower 95%","Upper 95%")
  colnames(RES)[2]<-"p-value"
  RES<-RES[-1,]
  
  OR.and.CI<-paste(format(round2(RES[,"OR"],2),nsmall=2)," (",format(round2(RES[,"Lower 95%"],2),nsmall=2)," - ",format(round2(RES[,"Upper 95%"],2),nsmall=2),")",sep="")
  Estimate<-round2(RES[,"Estimate"],3)
  P_value<-round2(RES[,"p-value"],3)
  P_value[which(P_value<0.001)]<-"<0.001"
  
  FinalRes<-cbind(Estimate,OR.and.CI,P_value)
  return(FinalRes)
}

#################### Ordinal logistic

#library("MASS")
#m <- polr(as.factor(Data.base[,"EQ5D4"]) ~ as.factor(Data.base[,"HIV_HCV"]), Hess=TRUE)
#ctable <- coef(summary(m))
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#ctable <- cbind(ctable, "p value" = p)
#ci <- confint(m)
#OR<-exp(cbind(OR = coef(m), ci))
#res<-cbind(ctable[1:dim(OR)[1],],OR)

###########################################################################################
###########################################################################################

GLM_OFFSET_Bivariate<-function(Data.Frame, ColumnsToUse, Outcome_name, Offset_name, FAMILY){
 
  RES<-rep(NA,7)
  for(i in 1:length(ColumnsToUse)){
    fullmod<-as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i],"+offset(log(",Offset_name,"))"))
    
    poiss.res<-glm(fullmod, family=FAMILY, data=Data.Frame)
    RR.CI<-exp(cbind(RR = coef(poiss.res), confint(poiss.res, level=0.95)))
    colnames(RR.CI) <- c("Rate Ratio", "Lower RR", "Upper RR")
    RES.temp<-cbind(summary(poiss.res)$coefficients,RR.CI)
    RES<-rbind(RES,RES.temp)
  }
  RES<-RES[-1,-2]
  RES<-RES[which(rownames(RES)%in%c("(Intercept)")==F),]
  
  RES<-RES[,-c(1:2)]
  colnames(RES)[which(colnames(RES)=="Pr(>|z|)")]<-"p-value"
  
  RES[,"p-value"]<-round2(RES[,"p-value"],3)
  Rate.Ratio.and.CI<-paste(
    format(round2(as.numeric(RES[,"Rate Ratio"]),2),nsmall=2)," (",
    format(round2(as.numeric(RES[,"Lower RR"]),2),nsmall=2)," - ",
    format(round2(as.numeric(RES[,"Upper RR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,Rate.Ratio.and.CI)
  RES<-RES[,c("p-value","Rate.Ratio.and.CI")]
  
  return(RES)
}

###########################################################################################
###########################################################################################

GLM_OFFSET_Multi<-function(Data.Frame, ColumnsToUse, Outcome_name, Offset_name, FAMILY){
  
  fullmod<-as.formula( paste( Outcome_name, " ~ ", paste(ColumnsToUse, collapse = "+"),"+offset(log(",Offset_name,"))"))
  poiss.res<-glm(fullmod, family=FAMILY, data=Data.Frame)
  RR.CI<-exp(cbind(RR = coef(poiss.res), confint(poiss.res, level=0.95)))
  colnames(RR.CI) <- c("Rate Ratio", "Lower RR", "Upper RR")
  RES<-cbind(summary(poiss.res)$coefficients,RR.CI)

  RES<-RES[which(rownames(RES)%in%c("(Intercept)")==F),]
  
  RES<-RES[,-c(1:2)]
  colnames(RES)[which(colnames(RES)=="Pr(>|z|)")]<-"p-value"
  
  RES[,"p-value"]<-round2(RES[,"p-value"],3)
  Rate.Ratio.and.CI<-paste(
    format(round2(as.numeric(RES[,"Rate Ratio"]),2),nsmall=2)," (",
    format(round2(as.numeric(RES[,"Lower RR"]),2),nsmall=2)," - ",
    format(round2(as.numeric(RES[,"Upper RR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,Rate.Ratio.and.CI)
  RES<-RES[,c("p-value","Rate.Ratio.and.CI")]
  
  return(RES)
}

###########################################################################################
###########################################################################################

GLM_NB_Bivariate_Personal<-function(Data.Frame, ColumnsToUse, Outcome_name, Offset_name){
  library(MASS)  
  
  RES<-rep(NA,7)
  for(i in 1:length(ColumnsToUse)){
  fullmod<-as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i],"+offset(log(",Offset_name,"))"))
  
  nb.res<-glm.nb(fullmod, data=Data.Frame)
  RR.CI<-exp(cbind(RR = coef(nb.res), confint(nb.res, level=0.95)))
  colnames(RR.CI) <- c("Rate Ratio", "Lower RR", "Upper RR")
  RES.temp<-cbind(summary(nb.res)$coefficients,RR.CI)
  RES<-rbind(RES,RES.temp)
  }
  RES<-RES[-1,-2]
  RES<-RES[which(rownames(RES)%in%c("(Intercept)")==F),]
  
  RES<-RES[,-c(1:2)]
  colnames(RES)[which(colnames(RES)=="Pr(>|z|)")]<-"p-value"
  
  RES[,"p-value"]<-round2(RES[,"p-value"],3)
  Rate.Ratio.and.CI<-paste(
    format(round2(as.numeric(RES[,"Rate Ratio"]),2),nsmall=2)," (",
    format(round2(as.numeric(RES[,"Lower RR"]),2),nsmall=2)," - ",
    format(round2(as.numeric(RES[,"Upper RR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,Rate.Ratio.and.CI)
  RES<-RES[,c("p-value","Rate.Ratio.and.CI")]
  
  return(RES)
}

###########################################################################################
###########################################################################################

GLM_NB_Multi_Personal<-function(Data.Frame, ColumnsToUse, Outcome_name, Offset_name){
  library(MASS)  
  
 
  fullmod<-as.formula( paste( Outcome_name, " ~ ", paste(ColumnsToUse, collapse = "+"),"+offset(log(",Offset_name,"))"))
  
  nb.res<-glm.nb(fullmod, data=Data.Frame)
  RR.CI<-exp(cbind(RR = coef(nb.res), confint(nb.res, level=0.95)))
  colnames(RR.CI) <- c("Rate Ratio", "Lower RR", "Upper RR")
  RES<-cbind(summary(nb.res)$coefficients,RR.CI)
  RES<-RES[-1,-2]
  
  RES<-RES[,-c(1:2)]
  colnames(RES)[which(colnames(RES)=="Pr(>|z|)")]<-"p-value"
  
  RES[,"p-value"]<-round2(RES[,"p-value"],3)
  Rate.Ratio.and.CI<-paste(
    format(round2(as.numeric(RES[,"Rate Ratio"]),2),nsmall=2)," (",
    format(round2(as.numeric(RES[,"Lower RR"]),2),nsmall=2)," - ",
    format(round2(as.numeric(RES[,"Upper RR"]),2),nsmall=2),")",sep="")
  RES<-cbind(RES,Rate.Ratio.and.CI)
  RES<-RES[,c("p-value","Rate.Ratio.and.CI")]
  
  return(RES)
}

###########################################################################################
###########################################################################################

GLM_zeroinfl_NB_offset_Bivariate_Personal<-function(Data, ColumnsToUse, Outcome_name, Offset_name, Dist){
  library(pscl)  
  
  Data.num<-Data[,c(Outcome_name, Offset_name, ColumnsToUse)]
  class(Data.num)<-"numeric"
  Data.Frame<-as.data.frame(Data.num)
  
  RES<-matrix(NA, length(ColumnsToUse),7) 
  for(i in 1:length(ColumnsToUse)){
    
    if(length(table(COVAR))>1){
      
      Formula<-as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i]))
      nb.res<-zeroinfl(Formula, offset=log(as.numeric(Data.Frame[,Offset_name])),data = Data.Frame, dist=Dist)
      
      #zeroinfl(N_JAIL~AGE_OUT, offset=log(NUMBER_FUS),data = Data.Frame, dist = "negbin")
      #zeroinfl(N_JAIL~AGE_OUT+offset(log(NUMBER_FUS))|AGE_OUT,data = Data.Frame, dist = "negbin")
      
      nb.res.summ<-summary(nb.res)$coeff$count
      RR.CI<-exp(cbind(RR = coef(nb.res), confint(nb.res, level=0.95)))[2,]
      names(RR.CI) <- c("Rate Ratio", "Lower RR", "Upper RR")
      RES[i,]<-c(nb.res.summ[2,],RR.CI)}
    
  }
  
  rownames(RES)<-ColumnsToUse
  colnames(RES)<-names(c(nb.res.summ[2,],RR.CI))
  
  
  RES<-RES[,-2]
  
  return(RES)
}

###########################################################################################
############### GLM BIVARIATE MULTINOMIAL #################################################
###########################################################################################

GLM_MULTINOMIAL_Bivariate_Personal<-function(Data,ColumnsToUse, Outcome_name, variable.fact.num){
  
  library("nnet")
  
  Data.num<-Data[,ColumnsToUse[which(variable.fact.num=="num")]]
  class(Data.num)<-"numeric"
  Data.Frame<-as.data.frame(Data.num)
  
  Data.fact<-Data[,c(Outcome_name,Outcome_name,ColumnsToUse[which(variable.fact.num=="fact")])] #cuplicated Outcome_name to keep matrix form
  Data.fact.Frame<-as.data.frame(Data.fact)
  colnames(Data.fact.Frame)<-c(Outcome_name,Outcome_name,ColumnsToUse[which(variable.fact.num=="fact")])
  Data.Frame<-cbind(Data.Frame,Data.fact.Frame)

  RES1<-matrix(NA, length(ColumnsToUse),5) 
  RES2<-matrix(NA, length(ColumnsToUse),5) 
  for(i in 1:length(ColumnsToUse)){
    
      Formula<-as.formula( paste( Outcome_name, " ~ ", ColumnsToUse[i]))
      myres<-multinom(Formula, data=Data.Frame)
      ss<-summary(myres)
      ci_all<-confint(myres, level=0.95)
      ci=rbind(ci_all[2,,1],ci_all[2,,2]) 
      RR.CI<-as.matrix(exp(cbind(RR = coef(myres)[,2], ci)))
      colnames(RR.CI) <- c("Relative RR", "Lower RR", "Upper RR")
      z <- ss$coefficients/ss$standard.errors
      # 2-tailed Wald z tests to test significance of coefficients
      p <- ((1 - pnorm(abs(z), 0, 1)) * 2)
      p<-p[,2]
      Coeff<-ss$coefficients[,2]
      RES1[i,]<-cbind(Coeff,p,RR.CI)[1,]
      RES2[i,]<-cbind(Coeff,p,RR.CI)[2,]
    
  }
  
    rownames(RES1)<-rownames(RES2)<-ColumnsToUse
  RES<-cbind(RES1,RES2)
  
  test<-cbind(Coeff,p,RR.CI)
  colnames(RES)<-c(paste(rownames(test)[1],"_",colnames(test),sep=""),paste(rownames(test)[2],"_",colnames(test),sep=""))
  
  return(RES)
}

###########################################################################################

GLM_MULTINOMIAL_Multivariate_Personal<-function(Data, ColumnsToUse, Outcome_name, variable.fact.num){

  library("nnet")
  
  Data.num<-Data[,ColumnsToUse[which(variable.fact.num=="num")]]
  class(Data.num)<-"numeric"
  Data.Frame<-as.data.frame(Data.num)
  
  Data.fact<-Data[,c(Outcome_name,Outcome_name,ColumnsToUse[which(variable.fact.num=="fact")])] #cuplicated Outcome_name to keep matrix form
  Data.fact.Frame<-as.data.frame(Data.fact)
  colnames(Data.fact.Frame)<-c(Outcome_name,Outcome_name,ColumnsToUse[which(variable.fact.num=="fact")])
  Data.Frame<-cbind(Data.Frame,Data.fact.Frame)
  
  Formula<-as.formula( paste( Outcome_name, " ~ ", paste(ColumnsToUse,collapse = "+")))
  myres<-multinom(Formula, data=Data.Frame)
  ss<-summary(myres)
  ci_all<-confint(myres, level=0.95)
  ci1=ci_all[,,1]
  ci2=ci_all[,,2]
  RR.CI1<-as.matrix(exp(cbind(RR = coef(myres)[1,], ci1)))
  RR.CI2<-as.matrix(exp(cbind(RR = coef(myres)[2,], ci2)))
  colnames(RR.CI1) <- colnames(RR.CI2) <- c("Relative RR", "Lower RR", "Upper RR")
  z <- ss$coefficients/ss$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- ((1 - pnorm(abs(z), 0, 1)) * 2)
  p1<-p[1,]
  p2<-p[2,]
  Coeff1<-ss$coefficients[1,]
  Coeff2<-ss$coefficients[2,]
  RES1<-cbind(Coeff1,p1,RR.CI1)
  RES2<-cbind(Coeff2,p2,RR.CI2)
    
  RES<-cbind(RES1,RES2)
  
  colnames(RES)<-c(paste(rownames(p)[1],"_",colnames(RES1),sep=""),paste(rownames(p)[2],"_",colnames(RES2),sep=""))
  
  return(RES)
 
} ## names of people should be numeric

####################### Mean

Descriptive_stats_Personal_mean<-function(Data, id_name, NameOfMainVariable, ColumnsToUse){
  
  miss.ind<-which(ColumnsToUse%in%colnames(Data)==F)
  if(length(miss.ind)>0){print(paste(ColumnsToUse[miss.ind]," is/are missing in your dataset",sep=""))}else{
    
    library(epitools)
    
    Baseline<-Data[which(duplicated(Data[,id_name])==F),]
    Baseline0<-Baseline[which(as.numeric(Baseline[,NameOfMainVariable])==0),]
    Baseline1<-Baseline[which(as.numeric(Baseline[,NameOfMainVariable])==1),]
    
    ##############################################################
    
    People<-unique(Data[,id_name])
    
    #############################################################
    ############# Descriptive statistics for single measurements
    
    Baseline.everything<-Baseline
    Baseline.stats<-function(Data.table,Baseline.everything){
      
      Baseline<-Data.table[which(duplicated(Data.table[,id_name])==F),]
      Baseline.data<-Baseline[,which(colnames(Baseline)%in%ColumnsToUse)]
      Baseline.dependent_var<-as.numeric(Baseline[,NameOfMainVariable])
      class(Baseline.data)<-"numeric"
      Baseline.data.result1<-colSums(Baseline.data, na.rm=T)
      Baseline.data.result0<-sapply(1:dim(Baseline.data)[2], function(x) length(which(Baseline.data[,x]==0)))
      Baseline.data.result1.with.perc<-paste(Baseline.data.result1, " (",round2(Baseline.data.result1/dim(Baseline.data)[1]*100,1),")", sep="")
      Baseline.data.result0.with.perc<-paste(Baseline.data.result0, " (",round2(Baseline.data.result0/dim(Baseline.data)[1]*100,1),")", sep="")
      
      for(l in 1:dim(Baseline.data)[2]){
        temp<-as.numeric(Baseline.data[,l])
        if(all(temp%in%c(NA, 1, 0))==F){
          #Baseline.data.result1.with.perc[l]<-Baseline.data.result0.with.perc[l]<-
          #paste("mean = ", round2(mean(temp, na.rm=T),1), 
          #   ", sd = ", round2(sd(temp, na.rm=T),1),
          #  ", med = ", round2(median(temp, na.rm=T),1),
          # " range=(", round2(min(temp, na.rm=T),0),"-", round2(max(temp, na.rm=T),0),")",
          #",IQR (based on quantile) = ", round2(IQR(temp, na.rm=T),1),
          #",IQR (H-spread) and (Q1 - Q3) = ", round2(diff(fivenum(temp)[c(2,4)]),1)," (",round2(fivenum(temp)[2],1),"-",round2(fivenum(temp)[4],1),"),"
          #, sep="")
          
          Baseline.data.result1.with.perc[l]<-Baseline.data.result0.with.perc[l]<-
            paste(round2(mean(temp, na.rm=T),2)," (", round2(sd(temp, na.rm=T),2),")", sep="")
        }}
      
      RESULT.base<-rbind(Baseline.data.result1.with.perc,Baseline.data.result0.with.perc)
      RESULT.base01<-rbind(rep(1, length(Baseline.data.result1.with.perc)),rep(0, length(Baseline.data.result0.with.perc)))
      
      colnames(RESULT.base)<-colnames(Baseline.data)
      
      
      RESULT<-cbind(c("", as.vector(RESULT.base01)), c("", as.vector(RESULT.base)))
      rn<-c("Baseline appointment", as.vector(rbind(colnames(RESULT.base), rep("",dim(RESULT.base)[2]))))
      RESULT<-cbind(rn, RESULT)
      colnames(RESULT)<-c("Demographic Characteristics", "Value", paste(deparse(substitute(Data.table)),
                                                                        " Total n=",dim(Baseline.data)[1],
                                                                        " (",round2(dim(Baseline.data)[1]/dim(Baseline.everything)[1]*100,2),"%)",
                                                                        ", N (%)",sep=""))
      
      return(RESULT)
    } ## function for main stats
    
    #############################################################
    
    MainVariable0.res1<-Baseline.stats(Baseline0,Baseline.everything)
    MainVariable1.res1<-Baseline.stats(Baseline1,Baseline.everything)
    MainVariable.res1<-Baseline.stats(Baseline,Baseline.everything)
    
    #############################################################
    ############# Chi squared; Fisher and OR
    
    Baseline.dependent_var<-as.numeric(Baseline[,NameOfMainVariable])
    Baseline.data<-Baseline[,which(colnames(Baseline)%in%ColumnsToUse)]
    
    Pv.fisher<-XSq<-Pv<-rep(NA,dim(Baseline.data)[2])
    Wilcoxon<-OR.fisher<-OR<-matrix("", dim(Baseline.data)[2], 3)
    for(i in 1:dim(Baseline.data)[2]){
      
      temp<-as.numeric(Baseline.data[,i])
      temp2<-Baseline.dependent_var[which(is.na(temp)==F)]
      if(length(table(temp))>1&length(table(temp2))>1){
        if(all(temp%in%c(NA, 1, 0))){
          MainVariable1.table<-table(temp[which(Baseline.dependent_var==1)])
          MainVariable0.table<-table(temp[which(Baseline.dependent_var==0)])
          Matrix<-rbind(MainVariable0.table,MainVariable1.table)
          if(length(Matrix)<4){ #added recently
            XSq[i]<-Pv[i]<-OR[i,]<-Pv.fisher[i]<-OR.fisher[i,]<-NA
            Wilcoxon[i,1:3]<-rep("",3)
          }else{
            XSq[i]<-round2(chisq.test(Matrix, correct = FALSE)$statistic,2)
            Pv[i]<-chisq.test(Matrix, correct = FALSE)$p.value
            OR[i,]<-round2(epitab(Matrix, method = c("oddsratio"), conf.level = 0.95)$tab[2,5:7],2)
            Pv.fisher[i]<-fisher.test(Matrix)$p.value
            OR.fisher[i,]<-round2(c(fisher.test(Matrix)$estimate,fisher.test(Matrix)$conf.int[1:2]),2)
            Wilcoxon[i,1:3]<-rep("",3)}
          
        }else{
          
          mylogit<-glm(Baseline.dependent_var~temp, family="binomial")
          OR.CI<-exp(cbind(OR = coef(mylogit), confint(mylogit, level=0.95)))[2,]
          names(OR.CI) <- c("OR", "Lower OR", "Upper OR")
          res.temp.glm<-c(summary(mylogit)$coefficients[2,],OR.CI)
          
          OR[i,]<-round2(res.temp.glm[c("OR","Lower OR","Upper OR")],3)
          Wilcoxon[i,1]<-wilcox.test(temp~Baseline.dependent_var)$p.value ## Wilcoxon
          Wilcoxon[i,2:3]<-t.test(temp~Baseline.dependent_var)$estimate
          Wilcoxon[i,]<-round2(as.numeric(Wilcoxon[i,]),4)
          
        }
        
      }
    }
    Pv<-round2(Pv,3)
    Pv.fisher<-round2(Pv.fisher,3)
    Pv[which(Pv<0.0001)]<-"<0.0001"
    Pv.fisher[which(Pv.fisher<0.0001)]<-"<0.0001"
    RES<-cbind(XSq, Pv, paste(OR[,1], " (",OR[,2], " - ",OR[,3],")",sep=""),
               Pv.fisher, paste(OR.fisher[,1], " (",OR.fisher[,2], " - ",OR.fisher[,3],")",sep=""),
               Wilcoxon[,1]) 
    rownames(RES)<-colnames(Baseline.data)
    colnames(RES)<-c("Chi-square", "p-value Chi-square", "OR (95% CI)", "p-value Fisher", "OR Fisher (95% CI)", 
                     "p-value Mann-Whitney")
    
    RESULT<-cbind(MainVariable.res1, MainVariable1.res1[,3], MainVariable0.res1[,3])
    colnames(RESULT)<-c(colnames(MainVariable.res1),colnames(MainVariable1.res1)[3], colnames(MainVariable0.res1)[3])
    RESULT<-cbind(RESULT, RES[match(RESULT[,1], rownames(RES)),])
    RESULT[which(is.na(RESULT),arr.ind=T)]<-""
    
    cn<-colnames(RESULT)
    cn<-gsub("Baseline Total","All, Total",cn)
    cn<-gsub("Baseline1",paste(NameOfMainVariable,"=1,",sep=""),cn)
    cn<-gsub("Baseline0",paste(NameOfMainVariable,"=0,",sep=""),cn)
    
    colnames(RESULT)<-cn
    
    RESULT<-RESULT[-1,]
    
    RESULT<-rbind(RESULT,c("* please use Fisher's test for counts<5", rep("",dim(RESULT)[2]-1)),
                  c("** OR for continuous variables have been obtained using regression", rep("",dim(RESULT)[2]-1)))
    
    
    return(RESULT)
  }
}

#########################################################
############## FU Period ################################
## Provides actual FU period instead of "0" (baseline) ##
## int_date should be "as.Date" format

REASSIGN_BASELINE<-function(Data, int_date_vec, survey_vec){
int_date<-int_date_vec
obs_period<-as.numeric(survey_vec)

for(i in 1:dim(Data)[1]){
  
  if ("2005-12-01"<=int_date[i] & int_date[i]<"2006-06-01") {obs_period[i]=0}
  if ("2006-06-01"<=int_date[i] & int_date[i]<"2006-12-01") {obs_period[i]=1}
  if ("2006-12-01"<=int_date[i] & int_date[i]<"2007-06-01") {obs_period[i]=2}
  if ("2007-06-01"<=int_date[i] & int_date[i]<"2007-12-01") {obs_period[i]=3}
  if ("2007-12-01"<=int_date[i] & int_date[i]<"2008-06-01") {obs_period[i]=4}
  if ("2008-06-01"<=int_date[i] & int_date[i]<"2008-12-01") {obs_period[i]=5}
  if ("2008-12-01"<=int_date[i] & int_date[i]<"2009-06-01") {obs_period[i]=6}
  if ("2009-06-01"<=int_date[i] & int_date[i]<"2009-12-01") {obs_period[i]=7}
  if ("2009-12-01"<=int_date[i] & int_date[i]<"2010-06-01") {obs_period[i]=8}
  if ("2010-06-01"<=int_date[i] & int_date[i]<"2010-12-01") {obs_period[i]=9}
  if ("2010-12-01"<=int_date[i] & int_date[i]<"2011-06-01") {obs_period[i]=10}
  if ("2011-06-01"<=int_date[i] & int_date[i]<"2011-12-01") {obs_period[i]=11}
  if ("2011-12-01"<=int_date[i] & int_date[i]<"2012-06-01") {obs_period[i]=12}
  if ("2012-06-01"<=int_date[i] & int_date[i]<"2012-12-01") {obs_period[i]=13}
  if ("2012-12-01"<=int_date[i] & int_date[i]<"2013-06-01") {obs_period[i]=14}
  if ("2013-06-01"<=int_date[i] & int_date[i]<"2013-12-01") {obs_period[i]=15}
  if ("2013-12-01"<=int_date[i] & int_date[i]<"2014-06-01") {obs_period[i]=16}
  if ("2014-06-01"<=int_date[i] & int_date[i]<"2014-12-01") {obs_period[i]=17}
  if ("2014-12-01"<=int_date[i] & int_date[i]<"2015-06-01") {obs_period[i]=18}
  if ("2015-06-01"<=int_date[i] & int_date[i]<"2015-12-01") {obs_period[i]=19}
  if ("2015-12-01"<=int_date[i] & int_date[i]<"2016-06-01") {obs_period[i]=20}
  if ("2016-06-01"<=int_date[i] & int_date[i]<"2016-12-01") {obs_period[i]=21}
  if ("2016-12-01"<=int_date[i] & int_date[i]<"2017-06-01") {obs_period[i]=22}
  if ("2017-06-01"<=int_date[i] & int_date[i]<"2017-12-01") {obs_period[i]=23}

print(dim(Data)[1]-i)
  }

return(obs_period)
}

#########################################################################################################################
### Wrapper for Mixed effects model with variable break point
### The function below finds the breaking point for the line of your piecewise model
#########################################################################################################################

# Here we wrap the call to lmer in a function that is passed the breakpoint as a parameter, 
# then minimize the deviance of the fitted model conditional upon the breakpoint using optimize. 
# This maximizes the profile log likelihood for the breakpoint, and, in general (i.e., not just for this problem) 
# if the function interior to the wrapper (lmer in this case) finds maximum likelihood estimates conditional upon 
# the parameter passed to it, the whole procedure finds the joint maximum likelihood estimates for all the parameters.
################################################################
## using:
## https://www.researchgate.net/post/Can_I_still_do_a_piecewise_broken-line_regression_in_R_if_I_used_the_same_animals_many_times
## and 
## https://stats.stackexchange.com/questions/19772/estimating-the-break-point-in-a-broken-stick-piecewise-linear-model-with-rando/19777#19777

#Data=sleepstudy
#Time_since_start_name<-"Days"
#Code_name<-"Subject"
#Outcome_name<-"Reaction"
#breaking_point_by_eye = 3 ## by "eye" breaking point
# intervals_of_search=0.5

#bp.all<-Find_breaking_point_piecewise(Data,Time_since_start_name,Code_name,Outcome_name,breaking_point_by_eye)

Find_breaking_point_piecewise_long<-function(Data.F,Time_since_start_name,Code_name,Outcome_name,breaking_point_by_eye,intervals_of_search){
  
  library(lme4)
  Data<-Data.F
  bp<-breaking_point_by_eye
  foo <- function(bp)
  {
    b1 <- function(x, bp) ifelse(x < bp, bp - x, 0)
    b2 <- function(x, bp) ifelse(x < bp, 0, x - bp)
    
    fullmod<-as.formula( paste( Outcome_name, "~ b1(",Time_since_start_name,", bp) + b2(", Time_since_start_name,", bp) + 
                                (b1(",Time_since_start_name,", bp) + b2(",Time_since_start_name,", bp) | ",Code_name,")",sep=""))
    
    mod <- lmer(fullmod, data = Data)
    
    deviance(mod)
  }
  
  search.range <- c(min(Data[,Time_since_start_name])+intervals_of_search,max(Data[,Time_since_start_name])-intervals_of_search) # search through every 0.5 points
  foo.opt <- optimize(foo, interval = search.range)
  bp <- foo.opt$minimum
  Break.point<-bp
  
  #To get a confidence interval for the breakpoint, you could use the profile likelihood. Add, e.g., qchisq(0.95,1) 
  # to the minimum deviance (for a 95% confidence interval) then search for points where foo(x) is equal to the calculated value:
  
  foo.root <- function(bp, tgt)
  {
    foo(bp) - tgt
  }
  tgt <- foo.opt$objective + qchisq(0.95,1)
  lb95 <- uniroot(foo.root, lower=search.range[1], upper=bp, tgt=tgt)
  ub95 <- uniroot(foo.root, lower=bp, upper=search.range[2], tgt=tgt)
  CI.lower<-lb95$root
  CI.upper<-ub95$root
  
  RES<-c(Break.point,CI.lower,CI.upper)
  names(RES)<-c("Break.point","CI.lower","CI.upper")
  
  return(RES)
}

#########################################################################################################################
### Wrapper for Mixed effects model with variable break point: CROSS-SECTIONAL (!)
### The function below finds the breaking point for the line of your piecewise model
#########################################################################################################################

# Here we wrap the call to lmer in a function that is passed the breakpoint as a parameter, 
# then minimize the deviance of the fitted model conditional upon the breakpoint using optimize. 
# This maximizes the profile log likelihood for the breakpoint, and, in general (i.e., not just for this problem) 
# if the function interior to the wrapper (lmer in this case) finds maximum likelihood estimates conditional upon 
# the parameter passed to it, the whole procedure finds the joint maximum likelihood estimates for all the parameters.
################################################################
## using:
## https://www.researchgate.net/post/Can_I_still_do_a_piecewise_broken-line_regression_in_R_if_I_used_the_same_animals_many_times
## and 
## https://stats.stackexchange.com/questions/19772/estimating-the-break-point-in-a-broken-stick-piecewise-linear-model-with-rando/19777#19777

#Data=sleepstudy
#Time_since_start_name<-"Days"
#Code_name<-"Subject"
#Outcome_name<-"Reaction"
#breaking_point_by_eye = 3 ## by "eye" breaking point
# intervals_of_search=0.5

#bp.all<-Find_breaking_point_piecewise(Data,Time_since_start_name,Outcome_name,breaking_point_by_eye)

Find_breaking_point_piecewise_cross<-function(Data.F,Time_since_start_name,Outcome_name,breaking_point_by_eye,intervals_of_search=2){
  
  Data<-Data.F
  bp<-breaking_point_by_eye
  foo <- function(bp)
  {
    b1 <- function(x, bp) ifelse(x < bp, bp - x, 0)
    b2 <- function(x, bp) ifelse(x < bp, 0, x - bp)
    
    fullmod<-as.formula( paste( Outcome_name, "~ b1(",Time_since_start_name,", bp) + b2(", Time_since_start_name,", bp) + 
                                (b1(",Time_since_start_name,", bp) + b2(",Time_since_start_name,", bp))",sep=""))
    
    mod <- lm(fullmod, data = Data)
    
    deviance(mod)
  }
  
  search.range <- c(min(Data[,Time_since_start_name])+intervals_of_search,max(Data[,Time_since_start_name])-intervals_of_search) # search through every 0.5 points
  foo.opt <- optimize(foo, interval = search.range)
  bp <- foo.opt$minimum
  Break.point<-bp
  
  #To get a confidence interval for the breakpoint, you could use the profile likelihood. Add, e.g., qchisq(0.95,1) 
  # to the minimum deviance (for a 95% confidence interval) then search for points where foo(x) is equal to the calculated value:
  
  foo.root <- function(bp, tgt)
  {
    foo(bp) - tgt
  }
  tgt <- foo.opt$objective + qchisq(0.95,1)
  lb95 <- uniroot(foo.root, lower=search.range[1], upper=bp, tgt=tgt)
  ub95 <- uniroot(foo.root, lower=bp, upper=search.range[2], tgt=tgt)
  CI.lower<-lb95$root
  CI.upper<-ub95$root
  
  RES<-c(Break.point,CI.lower,CI.upper)
  names(RES)<-c("Break.point","CI.lower","CI.upper")
  
  return(RES)
}


