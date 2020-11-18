rm(list=ls())

#Part1
best <- function(state, outcome) {
  library(dplyr)
  dat <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  resultados <- c("heart attack","heart failure","pneumonia")
 
   if (any(state==dat$State)){}else{stop("invalid state")}
   if (any(outcome==resultados)){}else{stop("invalid outcome")}
 
  x <- na.fail (filter(dat,State==state))
  if (outcome=="heart attack"){
    b<-which.min(x[, 11])
    x[b,2]
    }
  else if(outcome=="heart failure"){
    b<-which.min(x[, 17])
    x[b,2]
    }
  else if(outcome=="pneumonia"){
    b<-which.min(x[, 23])
    x[b,2]
   }
}

#Part2
rankhospital <- function(state, outcome, num = "best") {
  library(dplyr)
  dat <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  resultados <- c("heart attack","heart failure","pneumonia")
  
  if (any(state==dat$State)){}else{stop("invalid state")}
  if (any(outcome==resultados)){}else{stop("invalid outcome")}
  
  x <- (filter(dat,State==state))
  
  if (outcome=="heart attack"){
    y <- x[order(as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),x$Hospital.Name),]
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available") 
      if(num=="best"){num<-1}else if(num=="worst"){num<-nrow(y)}else{}
      y[num,2]
  }
  else if(outcome=="heart failure"){
    y <- (x[order(as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),x$Hospital.Name),])
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available") 
     if(num=="best"){num<-1}else if(num=="worst"){num<-nrow(y)}else{}
     y[num,2]
  }
  else if(outcome=="pneumonia"){
    y <- (x[order(as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),x$Hospital.Name),])
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available")
     if(num=="best"){num<-1}else if(num=="worst"){num<-nrow(y)}else{}
     y[num,2]
  }
}

#Part3
rankall <- function(outcome, num = "best") {
  library(dplyr)
  dat <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  resultados <- c("heart attack","heart failure","pneumonia")
  if (any(outcome==resultados)){}else{stop("invalid outcome")}
  hospital <- numeric()
  
if (outcome=="heart attack"){
    y <- dat[order(as.numeric(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),dat$Hospital.Name),]
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available") 
    
      if(num=="best"){
        a <- rep(1,54)
        names(a) <- unique(y$State)
        num <- a}
      else if(num=="worst"){num<-table(y$State)
      }else{
        a <- rep(num,54)
        names(a) <- unique(y$State)
        num<-a}
    for (i in unique(y$State)) 
      {
       hospital[i] <- filter(y,y$State==i)[num[i],2]
      }
    resposta <- data.frame(hospital,state=names(hospital))
    resposta[order(resposta$state),]
    
}else if(outcome=="heart failure"){
    y <- dat[order(as.numeric(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),dat$Hospital.Name),]
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available") 
    
     if(num=="best"){
       a <- rep(1,54)
       names(a) <- unique(y$State)
       num <- a}
     else if(num=="worst"){num<-table(y$State)
     }else{
       a <- rep(num,54)
       names(a) <- unique(y$State)
       num<-a}
    for (i in unique(y$State)) 
    {
      hospital[i] <- filter(y,y$State==i)[num[i],2]
    }
    resposta <- data.frame(hospital,state=names(hospital))
    resposta[order(resposta$state),]
    
  
}else if(outcome=="pneumonia"){
    
    y <- dat[order(as.numeric(dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),dat$Hospital.Name),]
    y <- filter(y,!y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available") 
    
     if(num=="best"){
       a <- rep(1,54)
       names(a) <- unique(y$State)
       num <- a}
    else if(num=="worst"){num<-table(y$State)
    }else{
       a <- rep(num,54)
       names(a) <- unique(y$State)
       num<-a}
    
   for (i in unique(y$State)) 
     {
      hospital[i] <- filter(y,y$State==i)[num[i],2]
     }
    resposta <- data.frame(hospital,state=names(hospital))
    resposta[order(resposta$state),]
}else{}
}









