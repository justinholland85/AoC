
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_09")



Input    <-  fread("Example_Day_09a.txt", header = FALSE, sep = "^")$V1



Input    <-  read.csv("Input_Day_09.txt", header = FALSE, sep = "^")$V1


# All sequences are the same length

X        <-  do.call(rbind, lapply(strsplit(Input, " "), as.numeric))





F.Difference     <-  function(X){
  
  N.Col  <-  ncol(X)
  
  dX     <- cbind(X[,-1]) - cbind(X[,-N.Col])
  
  return(dX)
  
  
}


F.Predict  <-  function(X, dX){
  
  N.Col    <-  ncol(X)
  
  Y        <-  cbind(X, X[,N.Col] + dX[,N.Col])
  
  return(Y)
  
  
}



F.Process  <-  function(X){

  N.X      <-  ncol(X)
  
Diffs       <-  list()

Diffs[[1]]   <-  X


for(i in 1:(N.X - 2)){
  
  Diffs[[i+1]]  <-  F.Difference(Diffs[[i]]  )
  
  
  
}


Diffs[[N.X - 1]]  <-  cbind(Diffs[[N.X - 1]] , 0)


for(i in (N.X - 2):1){
  
  X       <-  Diffs[[i]]  
  dX      <-  Diffs[[i+1]]
  
  Diffs[[i]]  <- F.Predict(X,dX)
  
  
  
}

return(Diffs)
}


Y  <-  F.Process(X)

Y.1  <- Y[[1]]

sum(Y.1 [,N.X+1])


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


N.X      <-  ncol(X)


# For part 2, just reverse X:

X   <-  X[,seq(N.X, 1)]


Y  <-  F.Process(X)

Y.1  <- Y[[1]]

sum(Y.1 [,N.X+1])
