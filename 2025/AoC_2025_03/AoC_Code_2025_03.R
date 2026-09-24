######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_03")


Data.0  <-  read.csv("AoC_Input_2025_03_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_03_1.txt", header = FALSE, sep = "", colClasses = "character")$V1

options(scipen=99)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


#====================================================================================================#
# F.Part.1
#====================================================================================================#

Data        <-  Data.0
Data        <-  Data.1


F.Part.1    <-  function(Data){
  
  #-- Find firt and lat occurrence of each digit
  MaxPos   <-  nchar(as.character(Data[1]))
  
  Digits  <-  seq(9, 1)
  
  Pos.1   <-  list()
  Pos.N   <-  list()
  
  for(i in 1:9){
      
    Gregexpr  <-  lapply(gregexpr(pattern = as.character(Digits[i]), text =  Data), as.numeric)
    Length    <-  lapply(Gregexpr, length)
    
    
    Pos.1[[i]]   <-  unlist(lapply(Gregexpr, Lib.Take.Which, 1))
    Pos.N[[i]]   <-  unlist(mapply(Lib.Take.Which, Gregexpr, Length, SIMPLIFY = FALSE))
    
  }
  
  Pos.1  <-  do.call(rbind, Pos.1)
  Pos.N  <-  do.call(rbind, Pos.N)
  
  #-- the last position can't be first choice
  Pos.1[which(Pos.1 == MaxPos)]  <-  -1
  
  
  #---  Find highest value first choice
  
  Dummy.Dig   <-  array(dim = dim(Pos.1), Digits)

  
  Dig.First       <-  apply((Pos.1 > 0) * Digits, 2, max )
  Dig.First.Mat   <-  t(array(dim = rev(dim(Dummy.Dig)), Dig.First))
  
  Pos.First       <-  (Dummy.Dig == Dig.First.Mat) * Pos.1
  Pos.First       <-  apply(Pos.First, 2, max)
  Pos.First.Mat   <-  t(array(dim = rev(dim(Dummy.Dig)), Pos.First))
  
  Dig.Second      <-  apply((Pos.N > Pos.First.Mat) * Dummy.Dig, 2, max )
  
  Pairs           <-  as.numeric(paste0(Dig.First, Dig.Second))
  
  return(sum(Pairs))
  
  
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

X   <-  as.character(Data.0[[2]])
d   <-  12


#====================================================================================================#
# F.Sub.2.0
#====================================================================================================#


F.Sub.2.0   <- function(X, d){
  
  Digits   <-  seq(9, 1)
  MaxPos   <-  nchar(X)
  
  Pos     <-  numeric(0)
  
    for(i in 1:9){
    
      Pos[[i]]    <-  regexpr(pattern = as.character(Digits[i]), text =  X)[1]
      
    }
  
  Pos[which(Pos > MaxPos -  d + 1)]  <-  -1
  
  Dig        <-  max(Digits * (Pos >  0))
  
  MoveTo     <- Pos[match(Dig, Digits)]

  Y  <-  substr(X, MoveTo + 1, nchar(X))

  Output  <-  list(Dig= Dig,
                   Y  = Y
                   )
  
  return(Output)
  
}

#====================================================================================================#
# F.Sub.2.1
#====================================================================================================#

X   <-  as.character(Data.0[[2]])
d   <-  12


F.Sub.2.1   <- function(X, d){
  
 Dig  <-  character(0)
  
 for(i in 1:d){
   
 Temp  <-  F.Sub.2.0(X, d + 1 - i)
 
 Dig[[i]]  <-  Temp$Dig
 X         <-  Temp$Y 
 
 }
  
 return(as.numeric(paste(Dig, collapse = "")))
  
  
}



#====================================================================================================#
# F.Part.2
#====================================================================================================#

Data        <-  Data.0
Data        <-  Data.1

F.Part.2    <-  function(Data, d){
  
  N         <-  length(Data)
  
  Results   <-   numeric(0)
    
    
  for(i in 1:N){
    
    Results[[i]]  <-  F.Sub.2.1(as.character(Data[[i]]), d)
    
  }
  
  return(sum(Results))
  
  
  }
  
  
  


#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0, 12)
F.Part.2(Data.1, 12)



