##Utilities


####Get bias and RMSE
bias_rmse<-function(mean_all, true_mean)
{
  
  estimate<-mean(unlist(mean_all))
  
  bias<-mean(unlist(lapply(seq_along(mean_all), function(j) {
    return(((mean_all[[j]] - true_mean)/true_mean)*100)
  })))
  
  rmse<-sqrt(var(unlist(mean_all))  + bias^2)/estimate
  return(c(estimate, bias, rmse))
}



ad_to_output<-function(result, scenario, tp){
  name<-deparse(substitute(result))
  
  name<-substr(name,8,nchar(name))
  dimnames(result) <- list(c("Estimate","Bias","RMSE"), tp)
  scenario[[paste0(name)]]<-result
  

  
  return(scenario)
  
  
}
