
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_04")

Input   <-  read.csv("Example_Day_04.txt", header = FALSE, sep = "^")$V1


Input   <-  read.csv("Input_Day_04.txt", header = FALSE, sep = "^")$V1



F.KeepNotNA   <-  function(X){return(X[which(!is.na(X))])}

Cards         <-  unlist(lapply(strsplit(Input, ":"), Lib.Take.Which, 2))

Split         <- strsplit(x = Cards, split =  "\\|")

Set.1         <-  unlist(lapply(Split,  Lib.Take.Which, 1))
Set.2         <-  unlist(lapply(Split,  Lib.Take.Which, 2))

Set.1         <-  strsplit(Set.1, " ")
Set.2         <-  strsplit(Set.2, " ")

Set.1         <-  lapply(Set.1, as.numeric)
Set.2         <-  lapply(Set.2, as.numeric)

Set.1         <-  lapply(Set.1, F.KeepNotNA)
Set.2         <-  lapply(Set.2, F.KeepNotNA)

Intersect     <-  mapply('%in%', Set.2, Set.1, SIMPLIFY = FALSE)

Winners       <-  unlist(lapply(Intersect, sum))

sum(floor(2 ^ (Winners -1) ))

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


N.Cards          <-  length(Input)


Copies           <-  rep(1, N.Cards)

for(i in 1:N.Cards){
  
  i.Winners      <-  Winners[i]
  i.Copies       <-  Copies[i]
  
if(i.Winners == 0){Range <- numeric(0)} else {Range <- seq(pmin(i + 1        , N.Cards), 
                                                           pmin(i + i.Winners, N.Cards)) }
  
  Copies[Range]  <-  Copies[Range] + i.Copies
  
}

sum(Copies)



















