outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
View(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcome) {
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  heart_attack <- as.numeric(dat[, 11])
  heart_failure <- as.numeric(dat[, 17])
  pneumonia <- as.numeric(dat[,23])
  #a <- as.character(outcome)
  resultados <- c("heart attack","heart failure","pneumonia")
  if (any(state==dat$State)){}else{stop("invalid state")}
  if (any(outcome==resultados)){}else{stop("invalid outcome")}
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  }
