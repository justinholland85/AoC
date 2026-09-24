
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_06")


Input   <-  fread("Example_Day_06.txt", header = FALSE, sep = "^")$V1


Input   <-  read.csv("Input_Day_06.txt", header = FALSE, sep = "^")$V1

F.KeepNotNA   <-  function(X){return(X[which(!is.na(X))])}

Text.Time      <-  "Time:"
Text.Dist      <-  "Distance:"


Time           <-  gsub(pattern = Text.Time,  replacement = "", grep(pattern = Text.Time,     Input, value = TRUE))
Dist           <-  gsub(pattern = Text.Dist,  replacement = "", grep(pattern = Text.Dist, Input, value = TRUE))

Time           <-  as.numeric(strsplit(Time, " ")[[1]])
Dist          <-  as.numeric(strsplit(Dist, " ")[[1]])

Time           <-  F.KeepNotNA(Time)
Dist           <-  F.KeepNotNA(Dist)

# distance (y), hold time (x), total time (t)
# y = x(t - x) => 0 = -x^2 + xt - y

A  <- -1
B  <- Time
C  <-  -Dist


F.Quadratic  <-  function(a,b,c){
  
  
  Det     <-  (b^2 - 4 * a * c )
  
  x.0    <-  (-b + Det ^ .5) / (2 * a)
  x.1    <-  (-b - Det ^ .5) / (2 * a)
  
  Min     <-  pmin(x.0, x.1)
  Max     <-  pmax(x.0, x.1)
  
  Min     <- ceiling(Min) + (Min %% 1 == 0)  
  Max     <- floor(Max)   - (Max %% 1 == 0)  
  
  N       <-  Max - Min + 1
  
  return(N)
  
  
}

N <- F.Quadratic(A,B,C)
prod(N)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Time.Collapsed    <-  as.numeric(paste(as.character(Time), collapse = ""))
Dist.Collapsed    <-  as.numeric(paste(as.character(Dist), collapse = ""))


F.Quadratic(-1, Time.Collapsed, -Dist.Collapsed)










