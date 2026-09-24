
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_08")



Input    <-  fread("Example_Day_08a.txt", header = FALSE, sep = "^")$V1
Input    <-  fread("Example_Day_08b.txt", header = FALSE, sep = "^")$V1
Input    <-  fread("Example_Day_08c.txt", header = FALSE, sep = "^")$V1


Input    <-  read.csv("Input_Day_08.txt", header = FALSE, sep = "^")$V1


Input     <-  Input[which(Input != "")]

Instructions    <-  match(strsplit(Input[[1]], "")[[1]], c("L", "R"))
N.Instructions  <-  length(Instructions)


Map             <-  Input[2:length(Input)]


Map.Split       <-  strsplit(Map, split = "=")


Map.From         <-  gsub(pattern = " ", replacement = "", unlist(lapply(Map.Split, Lib.Take.One, 1)))

Map.To           <-  unlist(lapply(Map.Split, Lib.Take.One, 2))

Map.To.Split     <-  strsplit(Map.To, ",")

Map.To.Left      <-  unlist(lapply(Map.To.Split, Lib.Take.One, 1))
Map.To.Left      <-  gsub(pattern = " ", replacement = "", Map.To.Left)
Map.To.Left      <-  gsub(pattern = "\\(", replacement = "", Map.To.Left)

Map.To.Right      <-  unlist(lapply(Map.To.Split, Lib.Take.One, 2))
Map.To.Right      <-  gsub(pattern = " ", replacement = "", Map.To.Right)
Map.To.Right      <-  gsub(pattern = "\\)", replacement = "", Map.To.Right)




Map               <-  data.frame(From  = Map.From, 
                                 Left  = Map.To.Left, 
                                 Right = Map.To.Right)


F.Map             <-  function(x, d, Map){
  
  Side     <-  Map[[1 + d]]
  
  return(Side[match(x, Map$From)])
  
}



x  <-  "AAA"
i  <- 1

repeat{
  
  n  <-  (i - 1) %% N.Instructions + 1
  d  <-  Instructions[[n]]
  x  <-  F.Map(x, d, Map)
  
  if(x == "ZZZ"){break}
  
  i <- i + 1
  
}

i



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


#====================================================================================================#
# This seems to take a long time, will try a mathematical approach
#====================================================================================================#



F.End.Char    <-  function(x){return(substr(x , 3, 3))}

x   <-  Map$From[which(F.End.Char(Map$From) == "A")]


i  <- 1

repeat{
  
  n  <-  (i - 1) %% N.Instructions + 1
  d  <-  Instructions[[n]]
  x  <-  F.Map(x, d, Map)
  z  <-  F.End.Char(x)  == "Z"
  
  if(prod(z) == 1){break}
  
  i <- i + 1
  
}

i




sum( Map$From %in%  Map$From)


sum( Map$Left %in%  Map$From)

which(! (Map$Left %in% Map$From))
which(! (Map$Right %in% Map$From))


Map[92, ]
Map[64, ]


"GSC" %in% Map$From




#====================================================================================================#
# Cycle Analysis
#====================================================================================================#

# The approach below is not properly gneralised but works on my data given some apparent simiplifcations
# in my day. 

# the approach is to work out the cycle start and length of the cycle for each individual input, then
# use prime factorisation to find the lowest common multiple. 

# For each of the 6 inputs:
#   - There is only one Z in their cycle
#   - That Z occurs on the first LR instruction in the cycle
#   - the period of each as only 2 prime factors of which one of those factors is common to all 

# If any of the above were not true you would need to rejig a little to handle it


F.End.Char    <-  function(x){return(substr(x , 3, 3))}

X   <-  Map$From[which(F.End.Char(Map$From) == "A")]


X.Path.x    <-  list()

for(j in 1:length(X)){

  x  <-  X[j]


Path.x  <-  character(0)
Path.N  <-  numeric(0)

i  <-  1

repeat{
  
  n  <-  (i - 1) %% N.Instructions + 1
  d  <-  Instructions[[n]]
  
  Prev   <-  Path.N[which(Path.x == x )]
  Stop   <-  n %in% Prev
  
  Path.x[[i]]  <-  x
  Path.N[[i]]  <-  n
  
  if(Stop){break}
  
  x  <-  F.Map(x, d, Map)
  i <- i + 1
  
}

X.Path.x[[j]]  <-  Path.x

print(j)


}



# All are only Z on one value in their path. 
# for each need to work out the first time and the cycle length



F.Cycle  <-  function(Path.x){
  
  N.p      <-  length(Path.x)
  p.i      <-  seq(1, length(Path.x))
  p.n      <-  (p.i - 1) %% N.Instructions + 1
  
  End.x    <-  Path.x[[N.p]]
  End.n    <-  p.n[[N.p]]
  
  Z        <-  F.End.Char(Path.x) == "Z"
  
  Which    <-  which(Path.x == End.x & p.n == End.n)[1]
  
  Delta    <-  N.p - Which
  
  Which.Z  <-  which(  Z )

  Z.n      <-  p.n[Which.Z]
  
  Output   <-  data.frame(Z.n   = Z.n, 
                          Delta = Delta,
                          First = Which.Z)
  
  return(Output)
  
  
}


X.Cycle   <-  list()

for(i in 1:length(X)){
  
  X.Cycle[[i]]  <-  F.Cycle(X.Path.x[[i]])
  
}




F.PrimeFactorisation   <-  function(N){
  
  N.Factors   <-  numeric(0)
  
  M   <-  N
  
  i  <-  1
  p  <-  1
  
  Factors  <-  numeric(0)
  
  while(M > 1){
    
    P    <-  Primes[p]
    
    IsFactor  <-  (M %% P ) == 0
    
    if(!IsFactor){p  <- p + 1}
    if(IsFactor){ 
      M             <-  M / P
      Factors[[i]]  <-  P
      i             <-  i + 1
     }
    
    
  }
  
  return(Factors)
  
}


Delta    <-  lapply(X.Cycle, Lib.Take.One, "Delta")

Primes    <-  Lib.PrimesCalc(20000)

Decomp   <-  lapply(Delta, F.PrimeFactorisation)
options(scipen = 99)
prod(unique(unlist(Decomp)))





