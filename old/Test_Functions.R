####################################################
# define a function that generates a summary table #
# for (2), (3), (4), (5) in Question 1             #
####################################################
n_by_homlss=function(factor){
  interview.data[, .(n = .N), by=c("homeless", factor)] %>% 
    mutate(freq = n / sum(n)) %>% 
    as.data.frame()
}

##########################################################
# define a function that estimates odds ratio, CI,       #
# and p-value based on logistic regression for (1) - (7) #
# in Question 1                                          #
##########################################################
odds.ratio.comp = function(dat, res.var, pre.var){
  dat = as.data.frame(dat)
  fit=glm(dat[,res.var] ~ as.matrix(dat[,pre.var]), binomial(logit))
  
  output=list()
  output$CI=matrix(NA,nrow=2,ncol=length(pre.var))
  
  # extract odds ratio, CI, and p.value for predictor variables
  for(i in pre.var){
    ind=which(pre.var==i)
    output$odds.ratio[ind]=exp(fit$coefficients[ind+1])
    output$CI[,ind]=c(exp(confint.default(fit)[ind+1,1]), exp(confint.default(fit)[ind+1,2]))
    output$p.value[ind]=coef(summary(fit))[ind+1,4]
  }
  
  # name rows and columns of the output
  names(output$odds.ratio)=pre.var
  rownames(output$CI)=c("min", "max")
  colnames(output$CI)=pre.var
  names(output$p.value)=pre.var
  
  return(output)
}

##########################################################
# define a function that finds variables                 #
# with minimum information criteria for Question 3       #
##########################################################
min.aic.model = function(dat, res.var, pre.var){
  dat = as.data.frame(dat)
  
  # get list of all combinations
  all.combs=do.call(CJ, replicate(length(pre.var)-1, 0:1, FALSE))
  names(all.combs)=pre.var[-which(pre.var=="base.log10.p")]
  
  # run interative runs for all variable combinations
  aic.rec=c()
  for(i in 1:nrow(all.combs)){
    if(i == 1){
      fit=glm(dat[,res.var] ~ as.matrix(dat[,"base.log10.p"]), binomial(logit))
      aic.rec[i]=fit$aic
    }else{
      var.incld=pre.var[as.vector(all.combs[i,]==1) %>% which()]
      fit=glm(dat[,res.var] ~ as.matrix(dat[,c("base.log10.p", var.incld)]), binomial(logit))
      aic.rec[i]=fit$aic
    }
  }
  # minimum aic
  min.aic=aic.rec[which.min(aic.rec)]
  
  # variable combinations with minimum aic
  min.aic.vars=pre.var[as.vector(all.combs[which.min(aic.rec),]==1) %>% which()]
  
  # double check
  # glm(dat[,res.var] ~ as.matrix(dat[,c("base.log10.p", min.aic.vars)]), binomial(logit))$aic
  
  output=c()
  output$min.aic.vars=min.aic.vars
  output$min.aic=min.aic
  return(output)
}
