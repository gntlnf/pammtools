load("Johannes_transition_prob/tempdata.rda")

devtools::load_all()

temp <- tempdata %>%
  mutate(enum = from,
         event = ifelse(to != "cens", 1, 0),
         transition = as.factor(paste0(from, "->", to))) %>%
  as_ped(Surv(entry, exit, event) ~  1 # exit
         ,
         id = "id",
         transition = "enum"#"transition"
         ,
         #cut = quantile(tempdata$exit, probs = seq(0, 1, length.out = 21)),
         timescale = "calendar"
  )



mod <- pamm(ped_status ~  s(tend, by = enum) + enum #+ s(id, bs = "re") 
            ,
            data=temp, 
            engine   = "bam", 
            method   = "fREML",
            offset = offset,
            discrete = TRUE,
            family = poisson())
