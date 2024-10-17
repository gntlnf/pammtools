### non-parametric estimators - without terminating event



# help function for transition matrix

# Aalen-Johansen - k*state-occupation probabilities
get_mean_AJ <- function(data_recurr = data_recurr, tp) {
  expected_mean_marcov <- lapply(seq_along(data_recurr), function(x) {
    data <- data_recurr[[x]]
    EOS <- tp[length(tp)]
    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[!(grepl("death", data$to)) & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = I + 2, nrow = I + 2)

    for (i in 1:(I + 1)) {
      tra[i, i + 1] <- TRUE
    }
    cens_code <- "cens"

    event_types <- c(0:(I + 1))
    event_typesn <- c(1:(I + 1))

    ## mean by time-point
    expected_mean_marcov_t <- sapply(tp, function(t) {
      AJ <- etm(data, event_types, tra, cens_code, 0, tp[length(tp)], covariance = FALSE)
      time_AJ <- AJ[["time"]][AJ[["time"]] <= t]
      last_time <- length(time_AJ)

      expected_mean_marcov <- sum(sapply(event_typesn, function(i) {
        ## i*P(X(t)=i)
        summand <- i * AJ[["est"]][1, i + 1, last_time]
        return(summand)
      }))
    })
    return(expected_mean_marcov_t)
  })
}




get_mean_NA <- function(data_recurr = data_recurr, tp = tp) {
  mean_NA <- lapply(seq_along(data_recurr), function(x) {
    data <- data_recurr[[x]]
    cens_code <- "cens"
    data$to <- ifelse(data$to != "cens", 1, "cens")
    data$from <- 0
    tra1 <- matrix(FALSE, ncol = 2, nrow = 2)
    tra1[1, 2] <- TRUE

    NA1 <- mvna(data, c("0", "1"), tra1, cens_code)

    return(predict(NA1, tp, tr.choice = c("0 1"))[["0 1"]][["na"]])
  })
  return(mean_NA)
}



get_mean_EB1 <- function(data_recurr = data_recurr, tp = tp) {
  expected_mean <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    EOS <- tp[length(tp)]

    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[!(grepl("death", data$to)) & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = I + 2, nrow = I + 2)

    for (i in 1:(I + 1)) {
      tra[i, i + 1] <- TRUE
    }
    cens_code <- "cens"

    event_types <- c(0:(I + 1))
    event_typesn <- c(1:(I + 1))

    cens_code <- "cens"
    start <- 0

    AJ_all <- etm(data, event_types, tra, cens_code, start, EOS, covariance = FALSE)


    expected_mean_t <- sapply(tp, function(t) {
      ## mean by time-point:
      event_times <- AJ_all[["time"]][AJ_all[["time"]] <= t]
      last_time <- event_times[length(event_times)]
      expected_mean <- sum(sapply(seq_along(event_times), function(j) {
        summand1 <- sum(sapply(event_typesn, function(i) {
          if (j != length(event_times)) {
            AJ <- etm(data, event_types, tra, cens_code, event_times[j], last_time, covariance = FALSE)
            if (is.null(AJ[["time"]])) {
              return(0)
            } else {
              P_2 <- prod(sapply(seq_along(event_times[1:j]), function(k) {
                factor1 <- ifelse(AJ_all[["n.event"]][i, i + 1, as.character(event_times[k])] == 0, 0,
                  AJ_all[["n.event"]][i, i + 1, as.character(event_times[k])] / sum(AJ_all[["n.risk"]][k, 1:i])
                )
                return(1 - factor1)
              }))

              P_1 <- prod(sapply(seq_along(event_times[1:j]), function(k) {
                if (i == 1) {
                  factor1 <- 1
                } else {
                  factor1 <- ifelse(AJ_all[["n.event"]][i - 1, i, as.character(event_times[k])] == 0, 0,
                    AJ_all[["n.event"]][i - 1, i, as.character(event_times[k])] / sum(AJ_all[["n.risk"]][k, 1:i - 1])
                  )
                }
                return(1 - factor1)
              }))
              prod_KM <- P_2 - P_1

              summand <- AJ[["est"]][i, i + 1, as.character(event_times[j + 1])] * prod_KM

              return(summand)
            }
          } else {
            return(0)
          }
        }))
        return(summand1)
      }))
      return(expected_mean)
    })
    return(expected_mean_t)
  })

  return(expected_mean)
}


get_mean_EB2 <- function(data_recurr = data_recurr, tp = tp) {
  expected_mean <- lapply(seq_along(data_recurr), function(x) {
    print(x)
    data <- data_recurr[[x]]
    EOS <- tp[length(tp)]

    ## transition matrix depend on I maximal number of events
    data_recurr2 <- data[!(grepl("death", data$to)) & data$to != "cens", ]
    I <- max(as.numeric(data_recurr2$to))

    tra <- matrix(FALSE, ncol = I + 2, nrow = I + 2)

    for (i in 1:(I + 1)) {
      tra[i, i + 1] <- TRUE
    }
    cens_code <- "cens"

    event_types <- c(0:(I + 1))
    event_typesn <- c(1:(I + 1))

    cens_code <- "cens"
    start <- 0

    AJ_all <- etm(data, event_types, tra, cens_code, start, EOS, covariance = FALSE)


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