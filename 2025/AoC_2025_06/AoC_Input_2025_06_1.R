######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_06")

options(scipen = 99)

Data.0  <-  as.matrix(read.csv("AoC_Input_2025_06_0.txt", header = FALSE, sep = ""))
Data.1  <-  as.matrix(read.csv("AoC_Input_2025_06_1.txt", header = FALSE, sep = ""))

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
  

  N.Row   <-  nrow(Data)

  Operator  <-  as.character(Data[N.Row, ])
  Numbers    <-  Data[-N.Row, ]  
  Numbers    <-  array(dim = dim(Numbers), as.numeric(Numbers))
  
 
  Operate    <- mapply(Reduce, as.list(Operator),  Lib.ColsToList(Numbers))
  
  return(sum(Operate))
  
  
  
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


Data.0  <-  read.csv("AoC_Input_2025_06_0.txt", header = FALSE, sep = "^")$V1
Data.1  <-  read.csv("AoC_Input_2025_06_1.txt", header = FALSE, sep = "^")$V1

# When reading in the actual data has the same character length on each row, the example had one missing 
# space on the last row so I added it manually in the text file to accurately reflect the actual data. 


Data   <-  Data.0
Data   <-  Data.1



#====================================================================================================#
# F.Part.2
#====================================================================================================#

F.Part.2  <-  function(Data){
  
  Split         <-  strsplit(Data, split = "")
  Mat           <-  do.call(rbind, Split)  
  
  N.Row         <-  nrow(Mat)

  OpRow         <-  Mat[N.Row, ]
  
  # I think adding N + 2 value will make a cleaner loop
  OpPos         <-  c(which(OpRow != " "), ncol(Mat) + 2)
  
  N.Ops         <-  length(OpPos) - 1
  
  Mat           <-  Mat[-N.Row, ]
  
  NumberMats    <-  list()
  
  for(i in 1:N.Ops){
    
    NumberMats[[i]]  <-  Mat[, OpPos[i]:(OpPos[i+1] - 2)]
    
  }

  Numbers    <-  lapply(NumberMats, apply, MARGIN = 2, paste, collapse = "")
  Numbers    <-  lapply(Numbers, as.numeric)
  
  Operator   <-  OpRow[OpPos[-length(OpPos)]]
  
  
  Operate    <-  mapply(Reduce, as.list(Operator),  Numbers)
  
  return(sum(Operate))
  
  
  
}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)
