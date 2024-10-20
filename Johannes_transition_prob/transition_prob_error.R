load("Johannes_transition_prob\\tempdata.rda")
setwd("C:\\Users\\ra82mih\\Documents\\pammtools\\Johannes_transition_prob")
devtools::load_all()

tempdata <- data[[1]] 
tempdata <- rbind(data[[1]], data[[2]], data[[3]], data[[4]], data[[5]],
                  data[[6]], data[[7]], data[[8]], data[[9]], data[[10]])
tempdata <- do.call(rbind, data)

temp <- tempdata %>%
  mutate(enum = from,
         event = ifelse(to != "cens", 1, 0),
         transition = as.factor(paste0(from, "->", to)),
         time = exit) %>%
  as_ped(Surv(entry, exit, event) ~ time # 1 #  exit
         ,
         id = "id",
         transition = #"enum"#
           "transition"
         ,
         cut = quantile(tempdata$exit, probs = seq(0, 1, length.out = 21)),
         timescale = "calendar"
  )



mod <- pamm(ped_status ~  s(tend) + transition #+ s(id, bs = "re") 
            ,
            data=temp, 
            engine   = "bam", 
            method   = "fREML",
            offset = offset,
            discrete = TRUE,
            family = poisson())


test <- temp %>% 
  make_newdata(tend = unique(tend),
               transition = unique(transition)) %>%
  group_by(transition) 

 
max_enum <- max(as.numeric(tempdata$to), na.rm=T)

trans_mat <- get_trans_prob(newdata = test %>% add_cumu_hazard(mod), object = mod, ci=F)[[2]] 
#trans_mat <- ifelse(trans_mat<0, 0, trans_mat)
event_typesn <- c(1:max_enum)


sapply(c(40, 60, 80, 100), function(t) {
  last_time <- temp %>% 
    filter(time <= t, ped_status == 1) %>%
    nrow()
  
  sum(sapply(event_typesn, \(i) {trans_mat[1, i + 1, last_time]}) * (event_typesn))
  
  
})
















# test %>% 
#   groupy_by(transition) %>% 
#   summarise(trans_prob = sum(trans_prob))
# 
# 
# 
# trans <- unname(vapply(test$transition, function(x) as.numeric(unname(unlist(strsplit(x, "->"))))[2], numeric(1)))



## jetzt mehr als ein datensatz


set.seed(111)

data <- sim(repN=10, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[1]])


#xxx <- 0
# library(dplyr)

# Modify your function to ensure transition is treated as a column name

# hier hab ich jetzt viel quatsch wegen meinem fix und sehr viele negative zahlen un NAs
results <- vapply(data, function(tempdata) {
  #xxx <- xxx + 1 
  # Prepare the data frame
  temp <- tempdata %>%
    mutate(enum = from,
           event = ifelse(to != "cens", 1, 0),
           transition = as.factor(paste0(from, "->", to)),
           time = exit) %>%
    as_ped(Surv(entry, exit, event) ~ time,
           id = "id",
           transition = "transition",
           timescale = "calendar")
  
  # Fit the model
  mod <- pamm(ped_status ~ s(tend) + transition,
              data = temp, 
              engine = "bam", 
              method = "fREML",
              offset = offset,
              discrete = TRUE,
              family = poisson())
  
  test <- make_newdata(temp,
                 tend = unique(tend),
                 transition = unique(transition)) %>%
    group_by(transition) 
  
  # Get transition probabilities
  trans_mat <- get_trans_prob(newdata = test %>% add_cumu_hazard(mod), object = mod, ci = FALSE)[[2]] 
  # trans_mat <- ifelse(trans_mat<0, 0, trans_mat)
  max_enum <- max(as.numeric(tempdata$to), na.rm=T)
  
  # Define event types
  event_typesn <- 1:max_enum
  
  # Compute expected mean using vapply
  expected_mean_marcov_t <- vapply(c(40, 60, 80, 100), function(t) {
    last_time <- temp %>% 
      filter(time <= t, ped_status == 1) %>%
      nrow()
    
    sum(sapply(event_typesn, function(i) trans_mat[1, i + 1, last_time]) * event_typesn)
    
  }, numeric(1)) # Specify the return type and length
  # print(xxx)
  # Return the results for this data frame
  return(expected_mean_marcov_t)
}
, numeric(4)) # Specify the output size for each iteration




results <- data.frame()

for(i in seq_along(data)) {

tempdata <- data[[i]]  

temp <- tempdata %>%
  mutate(enum = from,
         event = ifelse(to != "cens", 1, 0),
         transition = as.factor(paste0(from, "->", to)),
         time = exit) %>%
  as_ped(Surv(entry, exit, event) ~ time # 1 #  exit
         ,
         id = "id",
         transition = #"enum"#
           "transition"
         ,
         #cut = quantile(tempdata$exit, probs = seq(0, 1, length.out = 21)),
         timescale = "calendar"
  )



mod <- pamm(ped_status ~  s(tend, by = transition) + transition #+ s(id, bs = "re") 
            ,
            data=temp, 
            engine   = "bam", 
            method   = "fREML",
            offset = offset,
            discrete = TRUE,
            family = poisson())


test <- temp %>% 
  make_newdata(tend = unique(tend),
               transition = unique(transition)) %>%
  group_by(transition) 

# ggplot(test, aes(x = tend))+
#   geom_line(aes(y = trans_prob))+
#   facet_wrap(~transition, scales = "free_y")

max_enum <- max(as.numeric(tempdata$to), na.rm=T)

trans_mat <- get_trans_prob(newdata = test %>% add_cumu_hazard(mod), object = mod, ci=F)[[2]] 

event_typesn <- c(1:max_enum)


results <- rbind(results, sapply(c(40, 60, 80, 100), function(t) {
  last_time <- temp %>% 
    filter(time <= t, ped_status == 1) %>%
    nrow()
  
  sum(sapply(event_typesn, \(i) {trans_mat[1, i + 1, last_time]}) * (event_typesn))
  

}))
}


