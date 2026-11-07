######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2019
Day           <- 3

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)


Name.Data.0      <-  paste0(Ref, "_Input_0.txt")
Name.Data.1      <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)
Data.0      <-  readLines(Name.Data.0)
Data.1      <-  readLines(Name.Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
# Data   <-  Data.0
Data   <-  Data.1



F.Part.1          <-  function(Data){
  
  Split       <-  strsplit(Data, ",")
  
  VectorMap   <-  list("R" = c(1, 0),
                       "U" = c(0, 1), 
                       "L" = c(-1, 0), 
                       "D" = c(0, -1))
    
  F.Path    <-  function(List){
    
    Dir       <-  substr(List, 1, 1)
    Len       <-  as.numeric(substr(List, 2, nchar(List)))    
    
    Moves     <-  do.call(rbind, rep(VectorMap[Dir], Len))
    
    Position  <-  apply(Moves, 2, cumsum)

    return(Position)
    
  }
  
  Positions   <-  lapply(Split, F.Path)
  
  Match       <-  Lib.Tuple.Match(data.frame(Positions[[1]]), 
                                  data.frame(Positions[[2]]))
  
  Intersects  <-  Positions[[1]][!is.na(Match), ]
  
  Dist        <-  rowSums(abs(Intersects))
  
  MinDist     <-  min(Dist)
  
  return(MinDist)
  
  
}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.0)
F.Part.1(Data.1)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
# Data   <-  Data.0
Data   <-  Data.1



F.Part.2          <-  function(Data){
  
  Split       <-  strsplit(Data, ",")
  
  VectorMap   <-  list("R" = c(1, 0),
                       "U" = c(0, 1), 
                       "L" = c(-1, 0), 
                       "D" = c(0, -1))
  
  F.Path    <-  function(List){
    
    Dir     <-  substr(List, 1, 1)
    Len     <-  as.numeric(substr(List, 2, nchar(List)))    
    
    Moves   <-  do.call(rbind, rep(VectorMap[Dir], Len))
    
    Position <-  apply(Moves, 2, cumsum)
    Position  <-  data.frame(Position)
    names(Position)  <-  c("X", "Y")
    
    return(Position)
    
  }
  
  Positions   <-  lapply(Split, F.Path)
  
  Match       <-  Lib.Tuple.Match(Positions[[1]], 
                                  Positions[[2]])
  
  Intersects       <-  Positions[[1]]
  Intersects$ID.A  <-  seq(1, nrow(Intersects))
  Intersects$ID.B  <-  Match
  Intersects       <-  Intersects[!is.na(Match),]
  
  MinDist          <-  min(Intersects$ID.A + Intersects$ID.B)
  
  return(MinDist)
  
  
}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)

