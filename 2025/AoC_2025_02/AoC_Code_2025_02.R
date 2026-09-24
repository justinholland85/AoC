######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_02")


Data.0  <-  read.csv("AoC_Input_2025_02_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_02_1.txt", header = FALSE, sep = "")$V1

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
  
#---- Data Setup

  
  
  Split         <-  unlist(strsplit(Data, split = ","))
  Split         <-  lapply(strsplit(Split, split = "-"), as.numeric)
   
  Pairs         <-  data.frame(do.call(rbind, Split))
  
  names(Pairs)  <-  c("From", "To")
  
  Pairs$Delta   <-  Pairs$To - Pairs$From 
  
  Seq           <-  mapply(seq, as.list(Pairs$From), as.list(Pairs$To), SIMPLIFY = FALSE)
  Numbers.0     <-  unlist(Seq)

#----- Quick Filters

# -- number of digits must be even
DigSplit    <-  lapply(strsplit(as.character(Numbers.0), split = ""), as.numeric)
Length      <-  unlist(lapply(DigSplit, length))

Numbers.1   <-   Numbers.0[which(Length  %% 2 == 0 )]

#--- split in two parts
AsChar     <-  as.character(Numbers.1)
Nchar      <-  nchar(AsChar)

PartA      <- substr(Numbers.1, 1, Nchar / 2)
PartB      <- substr(Numbers.1, Nchar / 2 +  1, Nchar)

#-- Check and Return
Check  <-  PartA == PartB


return(sum(Check * Numbers.1))

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

#====================================================================================================#
# F.Sub.2
#====================================================================================================#

k  <-  55
X  <-  Numbers.1[k]

F.Sub.1  <-  function(X){
  
  X       <-  as.character(X)
  Nchar   <-  nchar(X)
  
  Tests    <-  numeric(0)
  
  for(i in 1:ceiling(Nchar/2)){
    
    i.Str  <-  substr(X, 1, i)
    Gsub   <-  gsub(pattern = i.Str, replacement = "", x = X)
    
    Tests[[i]]  <-  Gsub == ""
  }
  
  
  return(sum(Tests) > 0)
  
  
}






#====================================================================================================#
# F.Part.2
#====================================================================================================#

Data        <-  Data.0
Data        <-  Data.1

F.Part.2    <-  function(Data){
  
  #---- Data Setup
  
  Split         <-  unlist(strsplit(Data, split = ","))
  Split         <-  lapply(strsplit(Split, split = "-"), as.numeric)
  
  Pairs         <-  data.frame(do.call(rbind, Split))
  
  names(Pairs)  <-  c("From", "To")
  
  Pairs$Delta   <-  Pairs$To - Pairs$From 
  
  Seq           <-  mapply(seq, as.list(Pairs$From), as.list(Pairs$To), SIMPLIFY = FALSE)
  Numbers.0     <-  unlist(Seq)
  
  #----- Quick Filters
  
  # --  each digit must appear more than once
  DigSplit    <-  lapply(strsplit(as.character(Numbers.0), split = ""), as.numeric)
  
  Table       <-  lapply(DigSplit, table)
  Table       <-  lapply(Table, as.numeric)
  Table       <-  lapply(Table, table)
  Values      <-  lapply(Table, Lib.NumericNames)
  
  Length      <-  unlist(lapply(Table, length))
  Gt.1        <-  unlist(lapply(lapply(Values, ">", 1), prod))
  
  Numbers.1   <-  Numbers.0[which(Gt.1 == 1)]

  
  
  #-- Check and Return
  
  N           <-  length(Numbers.1)
  Check       <-  numeric(0)
  
  for(i in 1:N){
    
    Check[[i]]  <-   F.Sub.1(Numbers.1[i])
    
   # print(i)
    
  }
  
  
  return(sum(Check * Numbers.1))
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)
