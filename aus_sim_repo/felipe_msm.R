# load("data/True1.rda")
# True1 <- true_mean
# load("data/True2.rda")
# True2 <- true_mean

True1 <- c(1.997601, 2.996432, 3.997578, 4.998081)
True2 <- c(1.815338, 2.593804, 3.291802, 3.905253)

set.seed(111)

tempdata <- sim(repN=1, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[1]])[[1]]

temp <- tempdata %>%
  mutate(enum = from,
         event = ifelse(to != "cens", 1, 0),
         transition = as.factor(paste0(from, "->", to))) %>%
  as_ped(Surv(entry, exit, event) ~  1 # exit
                    ,
                    id = "id",
                    transition = "transition" ,
                    #cut = quantile(tempdata$exit, probs = seq(0, 1, length.out = 21)),
                    timescale = "calendar"
  )



mod <- pamm(ped_status ~  s(tend, by = transition)  #+ s(id, bs = "re") 
            ,
            data=temp, 
            engine   = "bam", 
            method   = "fREML",
            offset = offset,
            discrete = TRUE,
            family = poisson())

summary(mod)
plot(mod)

temp %>% 
  make_newdata(tend = unique(tend),
               enum = unique(enum)) %>%  
  mutate(transition = enum) %>% 
  add_trans_prob(mod)


cbind(temp, fit = mod$fitted.values) %>%
  mutate(time = ifelse(exp(offset) == tend, tend, exp(offset) + tstart)) %>%
  group_by(id) %>%
  summarise(est_enum40 = sum(fit[time<=40]),
            est_enum60 = sum(fit[time<=60]),
            est_enum80 = sum(fit[time<=80]),
            est_enum100 = sum(fit[time<=100])) %>%
  ungroup() %>%
  # select(-id) %>%
  colMeans()




set.seed(111)
tempdata <- sim(repN=100, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[1]])
data <- data.frame()

for(i in seq_along(tempdata)) {
  
  temp <- tempdata[[i]] %>%
    mutate(enum = from,
           event = ifelse(to != "cens", 1, 0),
           transition = as.factor(paste0(from, "->", to))) %>%
    as_ped_multistate(Surv(entry, exit, event) ~ 1,
                      id = "id",
                      transition = "transition",
                      # ,cut = quantile(tempdata[[i]]$exit, probs = seq(0, 1, length.out = 21))
                      timescale = "calendar"
    )
  
  mod <- pamm(ped_status ~ s(tend, by = transition) # + s(id, bs = "re")
              ,
              data=temp, 
              engine   = "bam", 
              method   = "fREML",
              offset = offset,
              discrete = TRUE,
              family = poisson())

  data <- rbind(data,
                cbind(temp, fit = mod$fitted.values) %>%
                  mutate(time = ifelse(exp(offset) == tend, tend, exp(offset) + tstart)) %>%
                  group_by(id) %>%
                  summarise(est_enum40 = sum(fit[time<=40]),
                            est_enum60 = sum(fit[time<=60]),
                            est_enum80 = sum(fit[time<=80]),
                            est_enum100 = sum(fit[time<=100])) %>%
                  ungroup() %>%
                  #select(-id) %>%
                  colMeans())
}



colMeans(data)











# hier irrelevant erstmal

get_trans_prob(temp%>%mutate(intlen=tend-tstart, transition = enum)%>%get_cumu_hazard(mod))



test <- temp %>% 
  mutate(transition = paste0(enum, "->", enum+1)) %>%
  make_newdata(tend = unique(tend),
               #enum = unique(enum),
               transition = unique(transition)) %>%
  group_by(transition) %>% 
  add_cumu_hazard(mod) %>% 
  add_trans_prob(mod)

seqq <- list()
for (i in 1:max(temp$enum)) {
  seqq[[i]] <- i:max(temp$enum)
}
seqq <- unlist(seqq)
transitions <- paste0(rep(0:(max(temp$enum)-1), 11:1), "->", 
       seqq)





