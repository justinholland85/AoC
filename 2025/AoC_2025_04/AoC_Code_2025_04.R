######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_04")


Data.0  <-  read.csv("AoC_Input_2025_04_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_04_1.txt", header = FALSE, sep = "")$V1



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.0

#====================================================================================================#
# F.Part.1
#====================================================================================================#



F.Part.1  <-  function(Data){
  
  
  #--- Data setup

  Data  <- do.call(rbind, strsplit(Data, split = ""))
  Data  <-  1 * (Data == "@")
  
  Nx    <-  ncol(Data)
  Ny    <-  nrow(Data)
  
  #--- Gridify
  
  Gridify  <-  Lib.Gridify(X = seq(to = Nx), 
                           Y = seq(to = Ny))
  
  
  Grid.DF  <-  data.frame(X = c(Gridify$Mat.X), 
                          Y = c(Gridify$Mat.Y), 
                          V = c(Data))
  
  
  #--- Duplify 
  
  Delta       <-  c(-1, 0 , 1)
  Delta.Grid  <-  expand.grid(Y = Delta, X = Delta)[ ,c(2:1)]  

  N.Delta     <-  nrow(Delta.Grid)
  
  Duplify     <-  list()
  
  for(i in 1:N.Delta){
    
    Temp          <-  Grid.DF
    Temp$X        <-  Temp$X + Delta.Grid$X[i]
    Temp$Y        <-  Temp$Y + Delta.Grid$Y[i]
    
    Duplify[[i]]  <-  Temp
    
  }
  
  
  Drop.Which   <-  which(Delta.Grid$X == 0 & Delta.Grid$Y == 0)
  
  Duplify      <-  Duplify[-Drop.Which]
  
  #--- Count 
  
  Tall         <-  Lib.ListBind.ByNames(Duplify)
  
  Tuple        <-  Lib.Tuple.Sum(Tall$X, Tall$Y, Var = Tall$V, Names = c("X", "Y"))
  
  Match        <-  Lib.Tuple.Match(Grid.DF[,c("X", "Y")], 
                                   Tuple$Index)
  
  Grid.DF$N    <-  Tuple$Index.Sum[Match]
  

  
  return(sum(Grid.DF$V * (Grid.DF$N < 4 )))
  
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.0)
F.Part.1(Data.1)




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Data   <-  Data.0


#====================================================================================================#
# F.Part.2
#====================================================================================================#



F.Part.2  <-  function(Data){
  
  
  #--- Data setup
  
  Data  <- do.call(rbind, strsplit(Data, split = ""))
  Data  <-  1 * (Data == "@")
  
  Nx    <-  ncol(Data)
  Ny    <-  nrow(Data)
  
  #--- Gridify
  
  Gridify  <-  Lib.Gridify(X = seq(to = Nx), 
                           Y = seq(to = Ny))
  
  
  Grid.DF  <-  data.frame(X = c(Gridify$Mat.X), 
                          Y = c(Gridify$Mat.Y))
  
  
  #--- Duplify 
  
  Delta       <-  c(-1, 0 , 1)
  Delta.Grid  <-  expand.grid(Y = Delta, X = Delta)[ ,c(2:1)]  
  
  N.Delta     <-  nrow(Delta.Grid)
  
  Duplify     <-  list()
  
  for(i in 1:N.Delta){
    
    Temp          <-  Grid.DF
    Temp$X        <-  Grid.DF$X + Delta.Grid$X[i]
    Temp$Y        <-  Grid.DF$Y + Delta.Grid$Y[i]
    
    Duplify[[i]]  <-  Temp
    
  }
  
  
  Drop.Which   <-  which(Delta.Grid$X == 0 & Delta.Grid$Y == 0)
  
  Duplify      <-  Duplify[-Drop.Which]
  
  Tall         <-  Lib.ListBind.ByNames(Duplify)
  
  Carry        <-  Data
  N.Carry      <-  sum(Data)
  
  Removed       <-  numeric(0)
  
  i  <-  1
  
  repeat{
   
    Tall$V        <-  c(Carry)
    
    Tuple        <-  Lib.Tuple.Sum(Tall$X, Tall$Y, Var = Tall$V, Names = c("X", "Y"))
    
    Match         <-  Lib.Tuple.Match(Grid.DF[, c("X", "Y")], 
                                     Tuple$Index)
    
    Grid.DF$N     <-  Tuple$Index.Sum[Match]
    
    Remove        <-  c(Carry) * (Grid.DF$N < 4)
    
    Removed[[i]]  <-  sum(Remove) 
    
    Carry         <-  Carry - Remove
    
    if(sum(Carry) == N.Carry){break} else {N.Carry <- sum(Carry)}
    
    print(i)
    i <- i + 1
    
  }
  
  
  
  #--- Count 
  

  

  return(sum(Removed))
  
  
}




#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)


























