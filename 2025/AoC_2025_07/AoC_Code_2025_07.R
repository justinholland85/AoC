######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_07")


Data.0  <-  read.csv("AoC_Input_2025_07_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_07_1.txt", header = FALSE, sep = "")$V1

options(scipen = 99)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.0
Data   <-  Data.1




#====================================================================================================#
# F.Part.1
#====================================================================================================#

F.Part.1  <-  function(Data){
  
#--- Data setup
  
  Mat    <-  do.call(rbind,strsplit(Data, split = ""))
  
  Mat[1,which(Mat[1,] == "S")]  <-  "|"
   
  N.Row   <-  nrow(Mat)
  
  Count    <- numeric(0)
  
  for(i in 1:(N.Row - 1)){Which
    
    R.0    <-  Mat[i,]
    R.1    <-  Mat[i + 1, ]
    
    #--- Idenfity Continuation and Splits
    
    Cont   <-  which((R.0 == "|") & (R.1 == "."))
    Split  <-  which((R.0 == "|") & (R.1 == "^"))
    
    #-- Continuations
    Mat[i+1,Cont]  <-  "|"
  
    #-- Splits 
    Mat[i+1, unique(c(Split - 1, Split + 1))]   <- "|"
      
    Count[[i]]    <-  length(Split)
    
  }
  
  Output  <-  list("Sum"   = sum(Count), 
                   "Count" = Count,
                   "Mat"   = Mat)
  
  return(Output)
  
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.0)$Sum
F.Part.1(Data.1)$Sum



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2.Fail
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
# This works on the example but is too slow

Data   <-  Data.0
Data   <-  Data.1




#====================================================================================================#
# F.Part.2.Fail
#====================================================================================================#

F.Part.2.Fail  <-  function(Data){
  
  #--- Data setup
  
  Mat      <-  do.call(rbind,strsplit(Data, split = ""))
  Start    <-  which(Mat[1,] == "S")
  
  Mat[1,Start]  <-  "|"
  
  N.Row   <-  nrow(Mat)
  
  Count    <- numeric(0)
  
  Paths      <-  list(c(Start))  
  Path.Last  <-  unlist(lapply(lapply(Paths, rev), Lib.Take.One, 1))
  
  
  for(i in 1:(N.Row - 1)){
    
    R.0    <-  Mat[i,]
    R.1    <-  Mat[i + 1, ]
    
    #--- Idenfity Continuation and Splits
    
    Cont   <-  which((R.0 == "|") & (R.1 == "."))
    Split  <-  which((R.0 == "|") & (R.1 == "^"))
    
    #--- Records Paths
    N.Splits    <-  length(Split)
    
    if(N.Splits > 0){
    for(j in 1:N.Splits){
      
      Which    <-  which(Path.Last == Split[j])
      Current  <-  Paths[Which]
      
      Paths     <-  Paths[-Which]
      
      New      <-  c(lapply(Current, c, Split[j] - 1), lapply(Current, c, Split[j] + 1))
      
      Paths     <-  c(Paths, New)
      Path.Last  <-  unlist(lapply(lapply(Paths, rev), Lib.Take.One, 1))
      
    }}
    
    
    #-- Continuations
    Mat[i+1,Cont]  <-  "|"
    
    #-- Splits 
    Mat[i+1, unique(c(Split - 1, Split + 1))]   <- "|"
    
    Count[[i]]    <-  length(Split)
    
  }
  
  Output  <-  list("Sum"     = sum(Count), 
                   "Count"   = Count,
                   "Paths"   = Paths, 
                   "N.Paths" = length(Paths),
                   "Mat"     = Mat)
  
  return(Output)
  
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2.Fail(Data.0)$N.Paths


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Data   <-  Data.0
Data   <-  Data.1

#====================================================================================================#
# F.Part.2
#====================================================================================================#

F.Part.2  <-  function(Data){
  
  #--- Data setup
  
  Mat            <-  do.call(rbind,strsplit(Data, split = ""))
  Start          <-  which(Mat[1,] == "S")
  
  Mat[1,Start]   <-  "|"
  
  N.Row          <-  nrow(Mat)
  
  Count          <- numeric(0)
  
  Paths          <-  array(dim = dim(Mat), 0)
  Paths[1,Start] <-  1
  
  
  for(i in 1:(N.Row - 1)){Which
    
    R.0    <-  Mat[i,]
    R.1    <-  Mat[i + 1, ]
    
    #--- Idenfity Continuation and Splits
    
    Cont   <-  which((R.0 == "|") & (R.1 == "."))
    Split  <-  which((R.0 == "|") & (R.1 == "^"))
    
    #-- Continuations
    Mat[i+1,   Cont]  <-  "|"
    Paths[i+1, Cont]  <-  Paths[i, Cont] 
    
    #-- Splits 
    Mat[i+1, unique(c(Split - 1, Split + 1))]   <- "|"
    
    # Adjust Paths Left & Right
    Paths[i+1, Split-1]  <- Paths[i+1, Split-1] +  Paths[i, Split]
    Paths[i+1, Split+1]  <- Paths[i+1, Split+1] +  Paths[i, Split]
    
    Count[[i]]    <-  length(Split)
    
  }
  
  N.Paths    <-  sum(Paths[N.Row,])
  
  Output  <-  list("Sum"     = sum(Count), 
                   "N.Paths" = N.Paths,
                   "Count"   = Count,
                   "Mat"     = Mat)
  
  return(Output)
  
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#


F.Part.2(Data.0)$N.Paths
F.Part.2(Data.1)$N.Paths





