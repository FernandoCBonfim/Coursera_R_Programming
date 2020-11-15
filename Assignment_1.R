rm(list=ls())
library(dplyr)

#Part1

pollutantmean <- function(dir,pol,id=1:332){
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
  linhas <- data.frame()
  nobs <- numeric()
  x <- seq(length(id))
  for (i in x) 
    {
     a <- id[i]
     linhas <- read.csv(arquivos[a])
     nobs[i] <- nrow(na.omit(linhas))
    }
    tabela <- data.frame(id,nobs)
  tabela
}

#Part3

corr <- function(dir,threshold=0)
{
  arquivos <- list.files(dir,full.names=TRUE)
  linhas <- data.frame()
  dados <- numeric()
  x <- complete(dir)
  y <- which(x$nobs>threshold,x$id)
  z <- sum(x$nobs>threshold)
  if (z>0){
  for (i in 1:z) 
   { 
    a <- y[i]
    linhas <- na.omit(read.csv(arquivos[a]))
    dados[i] <- cor(linhas$sulfate,linhas$nitrate)
   }
  }else{dados<-numeric(0)}
  dados
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

