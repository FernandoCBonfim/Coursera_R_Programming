rm(list=ls())

#Part1

pol_mean <- function(dir,pol,id=1:332){
   arquivos <- list.files(dir,full.names=TRUE)
  dados <- data.frame()
  for (i in id) {
    dados <- rbind(dados, read.csv(arquivos[i]))  }
  if(pol=="sulfate"){p<-2}else{
    if(pol=="nitrate"){p<-3}else{}
  }
  mean(dados[,p],na.rm = TRUE)
   }
#Part2

complete <- function(dir,id=1:332)
  {
  arquivos <- list.files(dir,full.names=TRUE)
  nobs <- numeric()
  tabela <- data.frame()
  for (i in id) 
    {
    a <- read.csv(arquivos[i])
    nobs[i] <- sum(!is.na(a$sulfate&a$nitrate)) 
    }
  tabela <- cbind(id,nobs)
  
  View(nobs)
  }


    