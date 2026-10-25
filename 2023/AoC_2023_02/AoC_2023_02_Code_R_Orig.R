source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_02")

Input   <-  read.csv("Example.txt", header = FALSE, sep = "^")$V1

Input   <-  read.csv("Input_Day_02.txt", header = FALSE, sep = "^")$V1


#--- Functions

F.ColCounts   <-  function(y){
  
  Colours     <-  c("red", "green", "blue")
  N.Colours   <-  length(Colours)
  
  Split       <-  gsub(pattern = " ", "",  strsplit(y, ",")[[1]])
  
  Counts      <-  numeric()
  
  for(i in 1:N.Colours){
    
    Grep     <-  grep(pattern = Colours[i], Split, value = TRUE )
    Gsub     <-  gsub(pattern = Colours[i], replacement = "", Grep) 
    Count    <-  as.numeric(Gsub)   
    
    Counts[[i]]  <-  ifelse(length(Count) == 0, 0, Count)
    
    
  }
  
  names(Counts)  <-  Colours
  
  return(Counts)
  
  
}


F.ColCounts.Wrapper    <-  function(Y){
  
  Counts     <-  list()
  
  for(i in 1:length(Y)){
    
    Counts[[i]]  <-  F.ColCounts(Y[i])
    
  }
  
  Counts  <-  do.call(rbind, Counts)
  
  return(Counts)
  
}


F.Valid    <-  function(Max, Pars.Max){
  
  return(t(t(Max) <= Pars.Max))
  
  
}
  
#--- Execution

X        <-  strsplit(Input, ":")

x.Game   <-  unlist(lapply(X , Lib.Take.One, 1))
x.0      <-  unlist(lapply(X , Lib.Take.One, 2))
x.1      <-  strsplit(x.0, ";") 


ColCounts   <-  lapply(x.1, F.ColCounts.Wrapper)
Max         <-  lapply(ColCounts, apply, 2, max)
Max         <-  do.call(rbind, Max)


Pars.Max     <-  c(12, 13, 14)



Good  <-  rowSums(F.Valid(Max, Pars.Max)) == 3
ID    <-  seq(1, length(Input))


sum(ID[Good])





######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

sum(apply(Max, 1, prod))











