######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2017
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


F.Part.1      <-  function(Data){
  
  Target      <-  as.numeric(Data)
  
  OddRoot     <-  floor(Target ^ .5)
  OddRoot     <-  ifelse(OddRoot %% 2 == 0, OddRoot - 1, OddRoot)
  
  Remainder   <-  Target - OddRoot^2
  
  BasePos     <-  ((OddRoot - 1) / 2) * c(1, -1)

  SideLen     <-  OddRoot + 1
  
  if(Remainder == 0){Add  <-  c(0,0)}
  
  if(0 < Remainder & Remainder <= OddRoot + 1){Add  <-  c(1, Remainder - 1)} 
  
  if(SideLen  <  Remainder & Remainder <= 2 * SideLen){
    LeftShift <- Remainder - (OddRoot + 1)
    Add       <-  c(1 - LeftShift,  OddRoot)} 
  
  if(2 * SideLen  < Remainder & Remainder <= 3 * SideLen){
    DownShift <- Remainder - (2 * SideLen)
    Add       <-  c( -OddRoot,  OddRoot - DownShift)} 
  
  if(3 * SideLen  < Remainder){
    RightShift <- Remainder - (3 * SideLen)
    Add        <-  c( RightShift - OddRoot,  -1)} 
  
  
  Pos <- BasePos + Add
  
  Manahtten  <-  sum(abs(Pos))
  
  return(Manahtten)
  
}




#====================================================================================================#
# Execution  
#====================================================================================================#
#F.Part.1(Data.0)
#F.Part.1(1)
#F.Part.1(12)
#F.Part.1(23)
#F.Part.1(1024)

F.Part.1(Data.1)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.1


F.Part.2      <-  function(Data){
  
  Target      <-  as.numeric(Data)
  
  #====================================================================================================#
  # F.FindPos
  #====================================================================================================#
  
  F.FindPos  <-  function(Target){
  
  OddRoot     <-  floor(Target ^ .5)
  OddRoot     <-  ifelse(OddRoot %% 2 == 0, OddRoot - 1, OddRoot)
  
  Remainder   <-  Target - OddRoot^2
  
  BasePos     <-  ((OddRoot - 1) / 2) * c(1, -1)
  
  SideLen     <-  OddRoot + 1
  
  if(Remainder == 0){Add  <-  c(0,0)}
  
  if(0 < Remainder & Remainder <= OddRoot + 1){Add  <-  c(1, Remainder - 1)} 
  
  if(SideLen  <  Remainder & Remainder <= 2 * SideLen){
    LeftShift <- Remainder - (OddRoot + 1)
    Add       <-  c(1 - LeftShift,  OddRoot)} 
  
  if(2 * SideLen  < Remainder & Remainder <= 3 * SideLen){
    DownShift <- Remainder - (2 * SideLen)
    Add       <-  c( -OddRoot,  OddRoot - DownShift)} 
  
  if(3 * SideLen  < Remainder){
    RightShift <- Remainder - (3 * SideLen)
    Add        <-  c( RightShift - OddRoot,  -1)} 
  
  
  Pos <- BasePos + Add
  
  return(Pos)
  }
     


  #====================================================================================================#
  # F.MainLoop
  #====================================================================================================#



  a.ID    <-  c(1)
  a.Value <-  c(1)
  a.X     <-  c(0)
  a.Y     <-  c(0)

  i  <-  1
  
  repeat{
    
    i  <-  i + 1
    
    Pos  <-  F.FindPos(i)
    
    Which       <-  which(abs(a.X -  Pos[1]) <= 1 & abs(a.Y - Pos[2]) <= 1)
    
    Value        <-  sum(a.Value[Which])
    
    a.ID[[i]]    <-  i
    a.X[[i]]     <-  Pos[1]
    a.Y[[i]]     <-  Pos[2]
    a.Value[[i]] <-  Value
    
    if(Value > Target){break}
    
  }
  

  return(Value)
  
}




#====================================================================================================#
# Execution  
#====================================================================================================#
#F.Part.1(Data.0)
#F.Part.1(1)
#F.Part.1(12)
#F.Part.1(23)
#F.Part.1(1024)

F.Part.2(Data.1)



