######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2015
Day           <- 3

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)


#Name.Data.0      <-  paste0(Ref, "_Input_0.txt")
Name.Data.1      <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)
#Data.0      <-  readLines(Name.Data.0)
Data.1      <-  readLines(Name.Data.1)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.1

F.Part.1          <-  function(Data){
 
  Split               <-  strsplit(Data, "")[[1]]
  Moves               <-  match(Split, c("^", ">", "v", "<"))
  
  VectorMap           <-  list(c(0, 1), 
                               c(1, 0), 
                               c(0, -1), 
                               c(-1, 0))

  
  Vectors             <-  data.frame(do.call(rbind, VectorMap[Moves]))
  names(Vectors)      <-  c("X", "Y")
  
  
  Position            <-  data.frame(rbind(c(0,0), apply(Vectors, 2, cumsum)))
  
  Position$ID         <-  Lib.GroupOn(Position$X, Position$Y)
  
  Position$VisitNum   <-  Lib.SeqOn(Position$ID)
  
  HouseCount          <-  sum(Position$VisitNum == 1)
  
  return(HouseCount)
  
   
}

#====================================================================================================#
# Execution  
#====================================================================================================#
#F.Part.1(Data.0)
F.Part.1(Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.1

F.Part.2          <-  function(Data){
  
  Split               <-  strsplit(Data, "")[[1]]
  Moves               <-  match(Split, c("^", ">", "v", "<"))
  
  VectorMap           <-  list(c(0, 1), 
                               c(1, 0), 
                               c(0, -1), 
                               c(-1, 0))
  
  
  Vectors             <-  data.frame(do.call(rbind, VectorMap[Moves]))
  names(Vectors)      <-  c("X", "Y")
  
  Vectors.ID          <-  seq(1, nrow(Vectors))
  
  Vectors.Santa       <-  Vectors[Vectors.ID%%2 == 1, ]
  Vectors.Robot       <-  Vectors[Vectors.ID%%2 == 0, ]
  
  
  Position.Santa      <-  data.frame(rbind(c(0,0), apply(Vectors.Santa, 2, cumsum)))
  Position.Robot      <-  data.frame(rbind(c(0,0), apply(Vectors.Robot, 2, cumsum)))
  
  Position            <-  rbind(Position.Santa, Position.Robot)
  
  Position$ID         <-  Lib.GroupOn(Position$X, Position$Y)
  
  Position$VisitNum   <-  Lib.SeqOn(Position$ID)
  
  HouseCount          <-  sum(Position$VisitNum == 1)
  
  return(HouseCount)
  
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#
#F.Part.1(Data.0)
F.Part.2(Data.1)








