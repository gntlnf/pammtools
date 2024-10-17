rm(list = ls())



library(survival)
library(etm)
library(mvna)
library(data.table)
library(dplyr)
library(MASS)
library(xtable)


## add path
# dirHome <- "/home/alexandra/Documents/Recurrent_Events/Programs/"
dirHome <- "code/fct/"

### load simulation  algorithms
# source(paste0(dirHome, "simulation_recurrent_events.R"))
### load utilities
# source(paste0(dirHome, "Utils.R"))

#load scenarios
# source(paste0(dirHome, "ScenarioMasterfile.R"))

### felipe: load ohne absolute pfade
source("aus_sim_repo/fct/simulation_recurrent_events.R")
source("aus_sim_repo/fct/Utils.R")
source("aus_sim_repo/ScenarioMasterfile.R")

#function true mean 
trueMean <- function(scenario, tp, output_name){
  iter <- 100000
  data_recurr_true <- replicate(10, simu_recurr(iter, scenario$alpha, cens = FALSE, gamma = scenario$gamma, 
                                                type2 = FALSE, m = scenario$m, stat.depend = FALSE, 
                                                cens.haz = c(0, 0, 0, 0, 0), death.haz = scenario$death.haz,
                                                staggered = scenario$staggered, nondeg = scenario$nondeg, 
                                                stopcrit = scenario$stopcrit, 
                                                EOS = scenario$EOS), simplify = FALSE)
  
  
  ## temp
  # plot(sort(unique(data_recurr_true[[1]]$entry_act)))
  nevents <- lapply(seq_along(data_recurr_true), function(j) {
    data_recurr_true[[j]] <- data_recurr_true[[j]][!(grepl("death", data_recurr_true[[j]]$to)), ]
    mean_event <- sapply(tp, function(t) {
      max_event <- aggregate(to ~ id, data = data_recurr_true[[j]][data_recurr_true[[j]]$exit <= t, ], max)
      mean_event <- sum(as.numeric(max_event$to)) / iter
      return(mean_event)
    })
    return(mean_event)
  })
  
  true_mean <- rowMeans(matrix(unlist(nevents), nrow = length(tp)))
  save(true_mean, 
       # file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/", output_name, ".rda"))
       # ohne absolute pfade
       file = paste0("saves/", output_name, ".rda"))
  return(true_mean)
}



##function

CalculateMeans <- function(repN, true_mean, scenario, output_name, boxplot = FALSE, term_event) {
  tp <- c(40, 60, 80, 100)
  if (term_event) {
    source(paste0(dirHome, "fun_estimators_terminating.R"))
  } else {
    source(paste0(dirHome, "fun_estimators.R"))
  }
  
  
  data_recurr <- replicate(repN, simu_recurr(scenario$N, scenario$alpha,
                                             cens = scenario$cens, gamma = scenario$gamma,
                                             type2 = scenario$type2, m = scenario$m,
                                             stat.depend = scenario$stat.depend,
                                             cens.haz = scenario$cens.haz, death.haz = scenario$death.haz,
                                             staggered = scenario$staggered, nondeg = scenario$nondeg,
                                             stopcrit = scenario$stopcrit, EOS = scenario$EOS, censRate = scenario$censRate
  ),
  simplify = FALSE
  )
  
  ### AJ
  StudymeanAJ <- get_mean_AJ(data_recurr, tp)
  
  # meanAJ<-rowMeans(matrix(unlist(StudymeanAJ),nrow=length(tp)))
  result_AJ <- sapply(seq_along(tp),
    function(t) {
      bias_rmse(matrix(unlist(StudymeanAJ), nrow = length(tp))[t, ], true_mean[t])
    }
  )
  
  
  scenario <- ad_to_output(result_AJ, scenario, tp)
  print("AJ finished")
  
  #### NA
  StudymeanNA <- get_mean_NA(data_recurr, tp)
  result_NA <- sapply(
    seq_along(tp),
    function(t) {
      bias_rmse(matrix(unlist(StudymeanNA), nrow = length(tp))[t, ], true_mean[t])
    }
  )
  
  scenario <- ad_to_output(result_NA, scenario, tp)
  print("NA finished")
  
  #### EB1= P(X_t=i) with KM
  StudymeanEB1 <- get_mean_EB1(data_recurr, tp)
  result_EB1 <- sapply(
    seq_along(tp),
    function(t) {
      bias_rmse(matrix(unlist(StudymeanEB1), nrow = length(tp))[t, ], true_mean[t])
    }
  )
  
  scenario <- ad_to_output(result_EB1, scenario, tp)
  print("EB1 finished")
  
  #### EB2= P(X_t=i) with AJ
  StudymeanEB2 <- get_mean_EB2(data_recurr, tp)
  result_EB2 <- sapply(
    seq_along(tp),
    function(t) {
      bias_rmse(matrix(unlist(StudymeanEB2), nrow = length(tp))[t, ], true_mean[t])
    }
  )
  
  
  scenario <- ad_to_output(result_EB2, scenario, tp)
  print("EB2 finished")
  if (boxplot == TRUE) {
    boxplotData <- data.frame(
      scenario = rep(scenario$Scenario, repN),
      AJ = matrix(unlist(StudymeanAJ), nrow = 4, ncol = repN)[3, ],
      NAE = matrix(unlist(StudymeanNA), nrow = 4, ncol = repN)[3, ],
      EB1 = matrix(unlist(StudymeanEB1), nrow = 4, ncol = repN)[3, ],
      EB2 = matrix(unlist(StudymeanEB2), nrow = 4, ncol = repN)[3, ]
    )
    
    # save(boxplotData, 
    #      file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/", "Boxplot_", output_name, ".rda"))
    save(boxplotData, 
         file = paste0("results/", "Boxplot_", output_name, ".rda"))
  }
  
  res_table <- data.frame(
    estimator = c("AJ", "NA", "EB1", "EB2"),
    BiasT40 = c(scenario[["AJ"]][2, 1], scenario[["NA"]][2, 1], scenario[["EB1"]][2, 1], scenario[["EB2"]][2, 1]),
    BiasT60 = c(scenario[["AJ"]][2, 2], scenario[["NA"]][2, 2], scenario[["EB1"]][2, 2], scenario[["EB2"]][2, 2]),
    BiasT80 = c(scenario[["AJ"]][2, 3], scenario[["NA"]][2, 3], scenario[["EB1"]][2, 3], scenario[["EB2"]][2, 3]),
    BiasT100 = c(scenario[["AJ"]][2, 4], scenario[["NA"]][2, 4], scenario[["EB1"]][2, 4], scenario[["EB2"]][2, 4]),
    RMSET40 = c(scenario[["AJ"]][3, 1], scenario[["NA"]][3, 1], scenario[["EB1"]][3, 1], scenario[["EB2"]][3, 1]),
    RMSET60 = c(scenario[["AJ"]][3, 2], scenario[["NA"]][3, 2], scenario[["EB1"]][3, 2], scenario[["EB2"]][3, 2]),
    RMSET80 = c(scenario[["AJ"]][3, 3], scenario[["NA"]][3, 3], scenario[["EB1"]][3, 3], scenario[["EB2"]][3, 3]),
    RMSET100 = c(scenario[["AJ"]][3, 4], scenario[["NA"]][3, 4], scenario[["EB1"]][3, 4], scenario[["EB2"]][3, 4])
  )
  
  print(xtable(res_table, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2)),
        include.rownames = FALSE,
        booktabs = TRUE,
        # file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/", "Table_", output_name, ".tex")
        file = paste0("results/", "Table_", output_name, ".tex")
  )
  # save(scenario,
  #      file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/", "ResultData_", output_name, ".rda"))
  save(scenario,
       file = paste0("results/", "ResultData_", output_name, ".rda"))
}





#true mean master
True1 <- trueMean(scenario=scenarioMaster[[1]], c(40,60,80,100), "True1")
True2 <- trueMean(scenario=scenarioMaster[[5]], c(40,60,80,100), "True2")
True5 <- trueMean(scenario=scenarioMaster[[17]], c(40,60,80,100), "True5")
True6 <- trueMean(scenario=scenarioMaster[[21]], c(40,60,80,100), "True6")
True9 <- trueMean(scenario=scenarioMaster[[33]], c(40,60,80,100), "True9")
True10 <- trueMean(scenario=scenarioMaster[[37]], c(40,60,80,100), "True10")

trueMeanMaster <- list(True1, True2, True1, True2, True5, True6, True5, True6, True9, True10, True9, True10)
# save(trueMeanMaster, 
#      file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/trueMeanMaster.rda"))
save(trueMeanMaster, 
     file = paste0("results/trueMeanMaster.rda"))

# load(file = paste0("/home/alexandra/Documents/Recurrent_Events/Results_Rev/trueMeanMaster.rda"))
load(file = paste0("results/trueMeanMaster.rda"))
trueMeanMaster <- list(True1, True2)

#function calls
#Scenario 1 Poisson, random censoring, no terminating eve
set.seed(1234)
Scenario1_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[1]], output_name="Scenario1_N100", boxplot=TRUE, term_event=FALSE)
Scenario1_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[2]], output_name="Scenario1_N25", boxplot=FALSE, term_event=FALSE)
Scenario1_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[3]], output_name="Scenario1_N50", boxplot=FALSE, term_event=FALSE)
Scenario1_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[4]], output_name="Scenario1_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 2 Poisson, random censoring, terminating event
set.seed(1235)
Scenario2_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[2]], scenario=scenarioMaster[[5]], output_name="Scenario2_N100", boxplot=TRUE, term_event=TRUE)
Scenario2_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[2]], scenario=scenarioMaster[[6]], output_name="Scenario2_N25", boxplot=FALSE, term_event=TRUE)
Scenario2_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[2]], scenario=scenarioMaster[[7]], output_name="Scenario2_N50", boxplot=FALSE, term_event=TRUE)
Scenario2_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[2]], scenario=scenarioMaster[[8]], output_name="Scenario2_N200", boxplot=FALSE, term_event=TRUE)

#Scenario 3 Poisson, state censoring, no terminating eve
set.seed(1236)
Scenario3_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[3]], scenario=scenarioMaster[[9]], output_name="Scenario3_N100", boxplot=TRUE, term_event=FALSE)
Scenario3_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[3]], scenario=scenarioMaster[[10]], output_name="Scenario3_N25", boxplot=FALSE, term_event=FALSE)
Scenario3_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[3]], scenario=scenarioMaster[[11]], output_name="Scenario3_N50", boxplot=FALSE, term_event=FALSE)
Scenario3_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[3]], scenario=scenarioMaster[[12]], output_name="Scenario3_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 4 Poisson, state censoring, terminating event
set.seed(1237)
Scenario4_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[4]], scenario=scenarioMaster[[13]], output_name="Scenario4_N100", boxplot=TRUE, term_event=TRUE)
Scenario4_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[4]], scenario=scenarioMaster[[14]], output_name="Scenario4_N25", boxplot=FALSE, term_event=TRUE)
Scenario4_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[4]], scenario=scenarioMaster[[15]], output_name="Scenario4_N50", boxplot=FALSE, term_event=TRUE)
Scenario4_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[4]], scenario=scenarioMaster[[16]], output_name="Scenario4_N200", boxplot=FALSE, term_event=TRUE)

#Scenario 5 Markov, random censoring, no terminating event
set.seed(1238)
Scenario5_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[5]], scenario=scenarioMaster[[17]], output_name="Scenario5_N100", boxplot=TRUE, term_event=FALSE)
Scenario5_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[5]], scenario=scenarioMaster[[18]], output_name="Scenario5_N25", boxplot=FALSE, term_event=FALSE)
Scenario5_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[5]], scenario=scenarioMaster[[19]], output_name="Scenario5_N50", boxplot=FALSE, term_event=FALSE)
Scenario5_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[5]], scenario=scenarioMaster[[20]], output_name="Scenario5_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 6 Markov, random censoring, terminating event
set.seed(1239)
Scenario6_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[6]], scenario=scenarioMaster[[21]], output_name="Scenario6_N100", boxplot=TRUE, term_event=TRUE)
Scenario6_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[6]], scenario=scenarioMaster[[22]], output_name="Scenario6_N25", boxplot=FALSE, term_event=TRUE)
Scenario6_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[6]], scenario=scenarioMaster[[23]], output_name="Scenario6_N50", boxplot=FALSE, term_event=TRUE)
Scenario6_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[6]], scenario=scenarioMaster[[24]], output_name="Scenario6_N200", boxplot=FALSE, term_event=TRUE)



#Scenario 7 Markov, stat censoring, no terminating event
set.seed(1240)
Scenario7_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[7]], scenario=scenarioMaster[[25]], output_name="Scenario7_N100", boxplot=TRUE, term_event=FALSE)
Scenario7_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[7]], scenario=scenarioMaster[[26]], output_name="Scenario7_N25", boxplot=FALSE, term_event=FALSE)
Scenario7_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[7]], scenario=scenarioMaster[[27]], output_name="Scenario7_N50", boxplot=FALSE, term_event=FALSE)
Scenario7_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[7]], scenario=scenarioMaster[[28]], output_name="Scenario7_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 8 Markov, stat censoring, terminating event
set.seed(1241)
Scenario8_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[8]], scenario=scenarioMaster[[29]], output_name="Scenario8_N100", boxplot=TRUE, term_event=TRUE)
Scenario8_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[8]], scenario=scenarioMaster[[30]], output_name="Scenario8_N25", boxplot=FALSE, term_event=TRUE)
Scenario8_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[8]], scenario=scenarioMaster[[31]], output_name="Scenario8_N50", boxplot=FALSE, term_event=TRUE)
Scenario8_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[8]], scenario=scenarioMaster[[32]], output_name="Scenario8_N200", boxplot=FALSE, term_event=TRUE)




#Scenario 9 non Markov, random censoring, no terminating event
set.seed(1243)
Scenario9_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[33]], output_name="Scenario9_N100", boxplot=TRUE, term_event=FALSE)
Scenario9_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[34]], output_name="Scenario9_N25", boxplot=FALSE, term_event=FALSE)
Scenario9_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[35]], output_name="Scenario9_N50", boxplot=FALSE, term_event=FALSE)
Scenario9_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[36]], output_name="Scenario9_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 10 non Markov, random censoring, terminating event
set.seed(1244)
Scenario10_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[37]], output_name="Scenario10_N100", boxplot=TRUE, term_event=TRUE)
Scenario10_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[38]], output_name="Scenario10_N25", boxplot=FALSE, term_event=TRUE)
Scenario10_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[39]], output_name="Scenario10_N50", boxplot=FALSE, term_event=TRUE)
Scenario10_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[40]], output_name="Scenario10_N200", boxplot=FALSE, term_event=TRUE)



#Scenario 11 non Markov, stat censoring, no terminating event
set.seed(1245)
Scenario11_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[41]], output_name="Scenario11_N100", boxplot=TRUE, term_event=FALSE)
Scenario11_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[42]], output_name="Scenario11_N25", boxplot=FALSE, term_event=FALSE)
Scenario11_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[43]], output_name="Scenario11_N50", boxplot=FALSE, term_event=FALSE)
Scenario11_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[44]], output_name="Scenario11_N200", boxplot=FALSE, term_event=FALSE)

#Scenario 12 non Markov, stat censoring, terminating event
set.seed(1246)
Scenario12_N1<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[45]], output_name="Scenario12_N100", boxplot=TRUE, term_event=TRUE)
Scenario12_N2<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[46]], output_name="Scenario12_N25", boxplot=FALSE, term_event=TRUE)
Scenario12_N3<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[47]], output_name="Scenario12_N50", boxplot=FALSE, term_event=TRUE)
Scenario12_N4<-CalculateMeans(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[48]], output_name="Scenario12_N200", boxplot=FALSE, term_event=TRUE)

