rm(list=ls())
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
  



