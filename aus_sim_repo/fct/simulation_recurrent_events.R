

## simulation algorithm
# N
# alpha
# cens
# gamma
# type2
# m
# state.depend
# cens.haz
# death.haz
# staggered
# non-deg
# stopcrit
# EOS


simu_recurr <- function(N, alpha, cens = FALSE, gamma = TRUE, type2 = FALSE, m = 200, stat.depend = FALSE,
                        cens.haz = c(0, 0, 0, 0), death.haz = c(0, 0, 0, 0), staggered = FALSE, nondeg = FALSE,
                        stopcrit = 1, EOS = 50, censRate = 1 / 30) {
  # consider common gamma frailty RV per individual

  if (gamma == TRUE) {
    Z <- rgamma(N, 1, 1)
  }
  if (gamma == FALSE) {
    Z <- 1
  }
  cens.time <- rexp(N, censRate)
  entry <- rep(0, N)
  entry_act <- rep(0, N)
  if (staggered == TRUE) {
    # generate recruiting time assuming uniform distribution
    entry_act <- runif(N, 0, 50)
  }
  recurr <- NULL
  cens.ind <- FALSE
  death.ind <- FALSE
  ## events until all participants are death or censored
  # or till end of study
  i <- 1
  expr <- TRUE
  while (expr) {
    U <- runif(N)
    if (i == 2 && nondeg == TRUE) {
      alpha2 <- ifelse(entry <= 2, alpha[2], 0.5)
      wait_time <- ((-log(1 - U)) / ((alpha2 + cens.haz[i] + death.haz[i]) * Z))
    }
    if (i != 2 || nondeg == FALSE) {
      wait_time <- ((-log(1 - U)) / ((alpha[i] + cens.haz[i] + death.haz[i]) * Z))
    }
    wait_time <- wait_time + entry
    exit <- wait_time
    to_prob <- rbinom(N, 1, (alpha[i]) / ((alpha[i] + death.haz[i])))

    to <- ifelse(to_prob == 0, paste0("death_", i - 1), i)


    if (stat.depend == TRUE) {
      to_prob1 <- (alpha[i]) / ((alpha[i] + cens.haz[i] + death.haz[i]))
      to_prob2 <- (cens.haz[i]) / ((alpha[i] + cens.haz[i] + death.haz[i]))
      to_prob <- rmultinom(N, size = 1, prob = c(to_prob1, to_prob2, 1 - (to_prob1 + to_prob2)))

      to <- ifelse(to_prob[1, ] == 1, i, ifelse(to_prob[2, ] == 1, "cens", paste0("death_", i - 1)))
    }


    if (cens == TRUE) {
      to <- ifelse(cens.time < wait_time, "cens", to)
      exit <- pmin(cens.time, wait_time)
    }
    # store row in a data.frame

    newrows <- data.frame(
      id = 1:N, from = rep(i - 1, N), to = to, entry = entry, exit = exit,
      entry_act = entry_act, stringsAsFactors = FALSE
    )
    ## delete individuals already censored or dead
    newrows <- newrows[cens.ind == FALSE & death.ind == FALSE, ]
    entry <- wait_time
    cens.ind <- (to == "cens" | cens.ind == TRUE)
    death.ind <- (grepl("death", to) | death.ind == TRUE)
    recurr <- rbind(recurr, newrows)


    ## check if still patients alive and uncensored

    if (stopcrit == 1) {
      expr <- length(recurr$id[recurr$to != "cens" & !(grepl("death", recurr$to))]) >= 1
    }
    ### stop criterion 2 until EOS
    if (stopcrit == 2) {
      highest <- by(recurr, recurr$id, tail, n = 1)
      highest_id <- do.call("rbind", as.list(highest))

      expr <- length(highest_id$id[highest_id$exit < EOS & highest_id$to != "cens" & !(grepl("death", highest_id$to))]) >= 1
    }

    if (is.na(alpha[i + 1])) {
      alpha[i + 1] <- alpha[i]
      cens.haz[i + 1] <- cens.haz[i]
      death.haz[i + 1] <- death.haz[i]
    }
    i <- i + 1
    ## if alpha not specified alpha[i+1]<-alpha[i]
  }


  recurr <- recurr[order(recurr$id), ]
  # apply type 2 censoring
  if (type2) {
    actual_exit <- recurr$exit + recurr$entry_act
    actual_entry <- recurr$entry + recurr$entry_act
    recurr <- cbind(recurr, actual_exit, actual_entry)
    ## add censoring as soon as m first events have been occurred.
    time_events <- sort(recurr$exit)
    cens.time <- time_events[m]
    print(cens.time)

    # delete all rows with entry time <= cens.time
    recurr <- recurr[!(recurr$actual_entry >= cens.time), ]
    # censor all patients with (actual) event time > cens.time

    recurr$to <- ifelse(recurr$actual_exit > cens.time, "cens", recurr$to)
    recurr$exit <- ifelse(recurr$actual_exit > cens.time, cens.time - recurr$entry_act, recurr$exit)
  } # end loop type 2 censoring=TRUE


  return(recurr)
}


