######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2018
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
Data   <-  Data.0

Data   <-  Data.1


F.Part.1          <-  function(Data){
  
  
  Split    <-  strsplit(Data, " ")
  
  ID       <-  as.numeric(gsub("#", "" ,unlist(lapply(Split, Lib.Take.One, 1))))
  
  Pos      <-  strsplit(unlist(lapply(Split, Lib.Take.One, 3)), ",")
  Pos.X    <-  as.numeric(unlist(lapply(Pos, Lib.Take.One, 1)))
  Pos.Y    <-  as.numeric(gsub(":", "", unlist(lapply(Pos, Lib.Take.One, 2))))
  
  Dim      <-  lapply(strsplit(unlist(lapply(Split, Lib.Take.One, 4)), "x"), as.numeric)
  Dim.X    <-  unlist(lapply(Dim, Lib.Take.One, 1))
  Dim.Y    <-  unlist(lapply(Dim, Lib.Take.One, 2))
  
  Claims   <-  data.frame(ID  = ID, 
                          rX  = Pos.X,
                          rY  = Pos.Y, 
                          dX  = Dim.X, 
                          dY  = Dim.Y)
  
  
  Claims$X0  <-  Claims$rX + 1
  Claims$Y0  <-  Claims$rY + 1
  Claims$X1  <-  Claims$X0 + Claims$dX - 1
  Claims$Y1  <-  Claims$Y0 + Claims$dY - 1
  
  Count      <-  array(dim = c(1000, 1000), 0)
  
  for(i in 1:max(ID)){
    
    Range.X   <-  seq(Claims$X0[i], Claims$X1[i])
    Range.Y   <-  seq(Claims$Y0[i], Claims$Y1[i])
    
    Count[Range.X, Range.Y]  <-  Count[Range.X, Range.Y] +  1

  }
  

  Dupes    <-  sum(Count > 1)
  
  return(Dupes)
  
  
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
Data   <-  Data.1


F.Part.2          <-  function(Data){
  
  
  Split    <-  strsplit(Data, " ")
  
  ID       <-  as.numeric(gsub("#", "" ,unlist(lapply(Split, Lib.Take.One, 1))))
  
  Pos      <-  strsplit(unlist(lapply(Split, Lib.Take.One, 3)), ",")
  Pos.X    <-  as.numeric(unlist(lapply(Pos, Lib.Take.One, 1)))
  Pos.Y    <-  as.numeric(gsub(":", "", unlist(lapply(Pos, Lib.Take.One, 2))))
  
  Dim      <-  lapply(strsplit(unlist(lapply(Split, Lib.Take.One, 4)), "x"), as.numeric)
  Dim.X    <-  unlist(lapply(Dim, Lib.Take.One, 1))
  Dim.Y    <-  unlist(lapply(Dim, Lib.Take.One, 2))
  
  Claims   <-  data.frame(ID  = ID, 
                          rX  = Pos.X,
                          rY  = Pos.Y, 
                          dX  = Dim.X, 
                          dY  = Dim.Y)
  
  
  Claims$X0         <-  Claims$rX + 1
  Claims$Y0         <-  Claims$rY + 1
  Claims$X1         <-  Claims$X0 + Claims$dX - 1
  Claims$Y1         <-  Claims$Y0 + Claims$dY - 1
  
  Claims$Overlaps   <-  Inf
  
  Count      <-  array(dim = c(1000, 1000), 0)
  
  for(i in 1:max(ID)){
    
    Range.X   <-  seq(Claims$X0[i], Claims$X1[i])
    Range.Y   <-  seq(Claims$Y0[i], Claims$Y1[i])
    
    Count[Range.X, Range.Y]  <-  Count[Range.X, Range.Y] +  1
    
  }
  
  for(i in 1:max(ID)){
    
    Range.X   <-  seq(Claims$X0[i], Claims$X1[i])
    Range.Y   <-  seq(Claims$Y0[i], Claims$Y1[i])
    
    Claims$Overlaps[i] <- sum(Count[Range.X, Range.Y] > 1)
    
  }
  
  IndpendenyID    <-  which(Claims$Overlap == 0)

  return(IndpendenyID)
  
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#
F.Part.2(Data.0)
F.Part.2(Data.1)

