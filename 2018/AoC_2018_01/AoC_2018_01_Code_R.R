######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2018
Day           <- 1

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)

Name.Data.1   <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)

# Data.0 is not useful here - it is just several messy examples

Data.1  <-  readLines(Name.Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.1

F.Part.1   <-  function(Data){
  
  Delta     <-  as.numeric(Data)
  
  Freq      <-  sum(Delta)  
  
  return(Freq)
  

}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.1)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.1

F.Part.2   <-  function(Data){
  
  Delta     <-  as.numeric(Data)
  

  Stop      <-  0
  
  List      <-  numeric(0) 
  Counter   <-  0
  
  while(Stop == 0){
    
    Counter  <- Counter +  1
    if(Counter %% 10 == 0){print(Counter)}
    
    List     <- c(List, Delta)
    CumSum   <-  cumsum(List)
    Seq      <-  Lib.SeqOn(CumSum)
    
    if(max(Seq) > 1){Stop <- 1}

  }
  
  
  Which  <-  which(Seq == 2)[1]
  Freq   <-  CumSum[Which]

  return(Freq)
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#
Time   <- proc.time()
F.Part.2(Data.1)
Time - proc.time()



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Score:
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Answer.p1      <-  1047
Answer.p2      <-  982
BadEntries.p1  <-  0
BadEntries.p2  <-  0
Help.p1        <-  "none"
Help.p2        <-  "none"
Diff.p1        <-  0
Diff.p2        <-  0
Start.p1       <-  20261009
Comp.p1        <-  20261009
Start.p2       <-  20261010
Comp.p2        <-  20261010





######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2.v1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.1

F.Part.2.v1 <-  function(Data){
  
  Delta      <-  as.numeric(Data)
  N          <-  length(Delta)
  
  CumSum     <-  Delta[1]
  i          <-  1
  
  repeat{
    
    i        <-  i + 1
    Cycle    <-  (i %% N)
    if(Cycle == 0){Cycle <- N}
      
    Freq  <-  CumSum[i - 1] + Delta[Cycle]
    
    if(Freq %in% CumSum){break} else {CumSum[[i]] <- Freq}
    
    if(Cycle == N &  (i / N) %% 10 == 0) print(i / N)

  }
  
  return(Freq)
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#
Time   <- proc.time()
F.Part.2.v1(Data.1)
Time - proc.time()

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2.v2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.1

F.Part.2.v2 <-  function(Data){
  
  Delta      <-  as.numeric(Data)
  Delta.Cum  <-  cumsum(Delta)
  N          <-  length(Delta)
  
  Base       <-  0
  
  CumSum     <-  numeric(0)
  
  Counter          <-  1
  
  repeat{
    
    New      <-  Base + Delta.Cum
    
    Exists   <-  New %in% CumSum

    if(sum(Exists) > 0){break} else {
      CumSum <- c(CumSum , New)
      Base    <-  New[N] }
    
    Counter  <-  Counter + 1
    if(Counter  %% 10 == 0){print(Counter )}
    
  }
  
  Freq  <-  New[which(Exists)][1]
  
  return(Freq)
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#
Time   <- proc.time()
F.Part.2.v2(Data.1)
Time - proc.time()




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2.v3
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.1

F.Part.2.v3 <-  function(Data){
  
  Delta      <-  as.numeric(Data)
  Delta.Cum  <-  cumsum(Delta)
  N          <-  length(Delta)
  
  Base       <-  0
  
  CumSum     <- list()
  
  i          <-  1
  
  repeat{
    
    New      <-  Base + Delta.Cum
    
    Exists   <-  New %in% unlist(CumSum)
    
    if(sum(Exists) > 0){break} else {
      CumSum[[i]] <- New
      Base    <-  New[N] }
    
    i  <-  i + 1
    if(i  %% 10 == 0){print(i )}
    
  }
  
  Freq  <-  New[which(Exists)][1]
  
  return(Freq)
  
}


#====================================================================================================#
# Execution  
#====================================================================================================#
Time   <- proc.time()
F.Part.2.v3(Data.1)
Time - proc.time()

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# ChatGPT Learnings
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# LEARNINGS: Algorithm Choice, Implementation and Data Structures
#
# 1. The cost of an algorithm depends on the implementation.
#    The original R solution repeatedly recalculates cumulative sums and identifies duplicates
#    using data.table. It is conceptually wasteful, but much of the work happens in optimised
#    code. The direct Python translation uses a much less efficient implementation of grouping.
#
# 2. Changing the algorithm can matter more than changing the syntax.
#    V2 moves the cumulative-sum calculation outside the loop and processes whole cycles at once.
#    This is a meaningful algorithmic improvement, independent of whether it is written in R or
#    Python.
#
# 3. Data structures matter.
#    R vectors, Python lists, NumPy arrays and Python sets have different costs for appending,
#    searching and numerical operations. Learning when each is appropriate is useful for future
#    problems.
#
# 4. Optimisation is not always worthwhile.
#    The original solution was a good answer to the problem as actually posed. The later versions
#    are experiments designed to teach something, not necessarily to produce a better solution.
#
# OVERALL:
# Keep the different versions as a record of how algorithm choice, language implementation and
# data structures affect performance. The aim is not to produce increasingly clever solutions,
# but to develop pattern recognition for future problems.

# Version  Description                         R       Python
# -------  ----------------------------------  ------  -------
# v0       Recalculate everything             5 sec   392 sec
# v1       Incremental, check history          28 sec  124 sec
# v2       Precompute cycle, batch processing  0.2sec  0.55 sec






