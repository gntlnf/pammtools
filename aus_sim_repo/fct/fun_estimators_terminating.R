### non-parametric estimators - terminating event



# Aalen-Johansen - k*state-occupation probabilities
get_mean_AJ <- function(data_recurr = data_recurr, tp) {
  expected_mean_marcov <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[!(grepl("death", data$to)) & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = 2 * (I + 1), nrow = 2 * (I + 1))

    for (i in 1:I) {
      tra[i, i + 1] <- TRUE
      tra[i, I + 1 + i] <- TRUE
      if (i == I) {
        tra[i + 1, I + i + 2] <- TRUE
      }
    }
    cens_code <- "cens"
    ### we has to consider death events
    all_deaths <- paste("death_", 0:I, sep = "")
    event_types <- c(0:I, all_deaths)
    event_typesn <- c(1:I)

    ## mean by time-point
    expected_mean_marcov_t <- sapply(tp, function(t) {
      AJ <- etm(data, event_types, tra, cens_code, 0, tp[length(tp)], covariance = FALSE)
      time_AJ <- AJ[["time"]][AJ[["time"]] <= t]
      last_time <- length(time_AJ)

      expected_mean_marcov <- sum(sapply(event_typesn, function(i) {
        ## i*P(X(t) in {i, death_i})
        summand <- i * (AJ[["est"]][1, i + 1, last_time] + AJ[["est"]][1, I + i + 2, last_time])
        return(summand)
      }))
    })
    return(expected_mean_marcov_t)
  })
}



# EB




get_mean_EB1 <- function(data_recurr = data_recurr, tp = tp) {
  expected_mean <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    cens_code <- "cens"
    ## we do not have to know after which event the death event occurred
    data$to <- ifelse(grepl("death", data$to), "death", data$to)
    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[data$to != "death" & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = I + 2, nrow = I + 2)

    for (i in 1:I) {
      tra[i, i + 1] <- TRUE
    }
    tra[0:(I + 1), I + 2] <- TRUE

    start <- 0

    event_types <- c(0:I, "death")
    event_typesn <- c(1:I)

    AJ_all <- etm(data, event_types, tra, cens_code, start, tp[length(tp)], covariance = FALSE)


    expected_mean_t <- sapply(tp, function(t) {
      ## mean by time-point:
      event_times <- AJ_all[["time"]][AJ_all[["time"]] <= t]
      last_time <- event_times[length(event_times)]

      expected_mean <- sum(sapply(event_typesn, function(i) {
        summand1 <- sum(sapply(seq_along(event_times), function(j) {
          if (j != length(event_times)) {
            AJ <- etm(data, event_types, tra, cens_code, event_times[j], last_time, covariance = FALSE)

            if (is.null(AJ[["time"]])) {
              return(0)
            } else {
              P_2 <- prod(sapply(seq_along(event_times[1:j]), function(k) {
                factor1 <- ifelse(AJ_all[["n.event"]][
                  i, i + 1,
                  as.character(event_times[k])
                ] +
                  sum(AJ_all[["n.event"]][1:i, I + 2, as.character(event_times[k])]) == 0,
                0,
                (AJ_all[["n.event"]][i, i + 1, as.character(event_times[k])] +
                  sum(AJ_all[["n.event"]][1:i, I + 2, as.character(event_times[k])])) / sum(AJ_all[["n.risk"]][k, 1:i])
                )

                return(1 - factor1)
              }))

              P_1 <- prod(sapply(seq_along(event_times[1:j]), function(k) {
                if (i == 1) {
                  factor1 <- 1
                } else {
                  factor1 <- ifelse(AJ_all[["n.event"]][i - 1, i, as.character(event_times[k])] + sum(AJ_all[["n.event"]][1:i - 1, I + 2, as.character(event_times[k])]) == 0, 0,
                    (AJ_all[["n.event"]][i - 1, i, as.character(event_times[k])] + sum(AJ_all[["n.event"]][1:i - 1, I + 2, as.character(event_times[k])])) / sum(AJ_all[["n.risk"]][k, 1:i - 1])
                  )
                }
                return(1 - factor1)
              }))
              prod_KM <- P_2 - P_1

              summand <- AJ[["est"]][i, i + 1, as.character(event_times[j + 1])] * prod_KM

              # print(((AJ_all[["est"]] [1,i,as.character(event_times[j])]+AJ_all[["est"]] [1,I+i+1,as.character(event_times[j])])))
              return(summand)
              # print(AJ_all[["est"]][1,i,as.character(event_times[j])])
              # if(abs(AJ_all[["est"]][1,i,as.character(event_times[j])]-prod_KM)>0.1){
              # print(i)
              # print(j)
              # }
              return(summand)
            }
          } else {
            return(0)
          }
        }))
      }))

      return(expected_mean)
    })
    return(expected_mean_t)
  })
  return(expected_mean)
}




#

get_mean_EB2 <- function(data_recurr = data_recurr, tp = tp) {
  expected_mean <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[!(grepl("death", data$to)) & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = 2 * (I + 1), nrow = 2 * (I + 1))

    for (i in 1:I) {
      tra[i, i + 1] <- TRUE
      tra[i, I + 1 + i] <- TRUE
      if (i == I) {
        tra[i + 1, I + i + 2] <- TRUE
      }
    }
    cens_code <- "cens"
    ### we has to consider death events
    all_deaths <- paste("death_", 0:I, sep = "")
    event_types <- c(0:I, all_deaths)
    event_typesn <- c(1:I)


    start <- 0


    AJ_all <- etm(data, event_types, tra, cens_code, start, tp[length(tp)], covariance = FALSE)


    expected_mean_t <- sapply(tp, function(t) {
      ## mean by time-point:
      event_times <- AJ_all[["time"]][AJ_all[["time"]] <= t]
      last_time <- event_times[length(event_times)]

      expected_mean <- sum(sapply(seq_along(event_times), function(j) {
        if (j != 1) {
          AJ <- etm(data, event_types, tra, cens_code, event_times[j - 1], last_time, covariance = FALSE)
        }
        summand1 <- sum(sapply(event_typesn, function(i) {
          if (j != 1) {
            if (is.null(AJ[["time"]])) {
              return(0)
            } else {
              summand <- AJ[["est"]][i, i + 1, as.character(event_times[j])] *
                AJ_all[["est"]][1, i, as.character(event_times[j - 1])]
              return(summand)
            }
          } else {
            return(0)
          }
        }))
      }))

      return(expected_mean)
    })
    return(expected_mean_t)
  })
  return(expected_mean)
}




get_mean_NA <- function(data_recurr = data_recurr, tp = tp) {
  expected_mean_NA_death <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    ## for NA we do not have to know after which event the death event occurred
    data$to <- ifelse(grepl("death", data$to), "death", data$to)

    event_times <- sort(data$exit)
    ## KM estimator
    highest <- by(data, data$id, tail, n = 1)
    data_KM <- do.call("rbind", as.list(highest))
    data_KM$exit <- ifelse(!(data_KM$to %in% c("death", "cens")),
      event_times[length(event_times)], data_KM$exit
    )
    KM_all <- survfit(Surv(exit, to == "death") ~ 1, data = data_KM)
    ## mean by time-point
    expected_mean <- sapply(tp, function(t) {
      last_t <- length(event_times[event_times <= t])

      result <- sum(sapply(seq_along(event_times[1:last_t]), function(j) {
        predictTime <- findInterval(event_times[j], KM_all[["time"]])
        if (predictTime == 0) {
          KM <- 1
        } else {
          KM <- KM_all[["surv"]][predictTime]
        }


        data$to <- ifelse(data$to == "death", "cens", data$to)
        cens_code <- "cens"
        data$to <- ifelse(data$to != "cens", 1, "cens")
        data$from <- 0
        tra1 <- matrix(FALSE, ncol = 2, nrow = 2)
        tra1[1, 2] <- TRUE

        NA1 <- mvna(data, c("0", "1"), tra1, cens_code)
        NA_u <- NA1[["n.event"]][1, 2, as.character(event_times[j])] / NA1[["n.risk"]][j]

        return(NA_u * KM)
      }))
      return(result)
    })
    return(expected_mean)
  })
  return(expected_mean_NA_death)
}
