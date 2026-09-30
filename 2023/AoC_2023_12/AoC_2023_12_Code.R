######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/OneDrive/Documents/GitHub/AoC/2023/AoC_2023_12")
options(scipen = 99)

Data.0    <-  read.csv("AoC_2023_12_Input_0.txt", header = FALSE, sep = "^")$V1
Data.1    <-  read.csv("AoC_2023_12_Input_1.txt", header = FALSE, sep = "^")$V1




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Data   <-  Data.0
Data   <-  Data.1


#====================================================================================================#
# F.Sub.1
#====================================================================================================#


F.Sub.1    <-  function(X, X.Pattern){
  
  
  X.Split    <-  strsplit(X, "")[[1]]
  Length     <-  length(X.Split)
  
  N.q        <-  sum(X.Split == "?")
  
  Grid       <-  expand.grid(rep(list(c("#", ".")), N.q), stringsAsFactors = FALSE)
  
  N.Grid     <-  nrow(Grid)
  
  Combos     <-  list()
  
  q.cum      <-  cumsum(X.Split == "?")    
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create all possible combinations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
  
  for(i in 1:Length){
    
    if(X.Split[[i]] == "?"){ Temp <- Grid[[q.cum[[i]] ]]} else { Temp <-  rep(X.Split[[i]], N.Grid)}
    
    Combos[[i]]  <-  Temp
    
  }
  
  Combos   <-  do.call(cbind, Combos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
# Sequence the combos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
  
  Binaries    <- 1 * (Combos == "#")
  Binaries
  
  Sequence      <-  0 * Binaries
  Sequence[,1]  <-  Binaries[, 1]
  
  for(i in 2:Length){
    
    Sequence[,i]  <-  (Sequence[,i-1] + Binaries[,i]) * Binaries[,i]
    
    
  }
  
  
  EndSequence  <-  Sequence
  
  
  for(i in 1:(Length - 1)){
    
    EndSequence[,i]  <-   Sequence[,i] * ( Sequence[,i+1] == 0 )
    
  }
  
  
  EndSequence  <-  Lib.RowsToList(EndSequence)
  
  Keep  <-  lapply(lapply(EndSequence, ">", 0), which)
  
  Sequenced  <-  mapply( Lib.Take.Which, EndSequence, Keep, SIMPLIFY = FALSE)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
# Check Pattern
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
  
  Which     <-  which(unlist(lapply(Sequenced, identical, X.Pattern)))
  
  Count    <-  length(Which)
  
  Output   <-  list("Count"       = Count, 
                    "Combos"      = Combos, 
                    "Binaries"    = Binaries, 
                    "Sequence"    = Sequence, 
                    "EndSequence" = EndSequence, 
                    "Sequenced"   = Sequenced,
                    "Which"       = Which
  )
  
  
  return(Output)
  
  
}




#====================================================================================================#
# F.Part.1
#====================================================================================================#


F.Part.1   <-  function(Data){
  
  
  
  Split        <- strsplit(Data, " ")
  
  String       <-  lapply(Split ,  Lib.Take.One, 1)
  Pattern      <-  lapply(Split ,  Lib.Take.One, 2)
  Pattern      <-  lapply(lapply(lapply(Pattern, strsplit, ","), Lib.Take.One, 1), as.numeric)
  
  Chars        <-  lapply(lapply(String, strsplit, ""), Lib.Take.One, 1)
  
  
  
  Results   <-  numeric(0)
  
  for(k in 1:length(String)){
    
    
    X          <-  String[[k]]
    X.Pattern  <-  Pattern[[k]]
    
    
    Results[[k]]  <-  F.Sub.1(X, X.Pattern)$Count
    print(k)
  }
  
  
  return(sum(Results))
  
  
  
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

Data         <-  Data.1


Split        <- strsplit(Data, " ")

String       <-  lapply(Split ,  Lib.Take.One, 1)
Pattern      <-  lapply(Split ,  Lib.Take.One, 2)


String.Ext   <-  lapply(lapply(String, rep, 5), paste, collapse = "?")      
Pattern.Ext  <-  lapply(lapply(Pattern, rep, 5), paste, collapse = ",")  





String.Ext[[997]]
Pattern.Ext[[997]]








