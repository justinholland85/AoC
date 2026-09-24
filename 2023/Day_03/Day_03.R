source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_03")





Input   <-  read.csv("Example_Day_03.txt", header = FALSE, sep = "^")$V1


Input   <-  read.csv("Input_Day_03.txt", header = FALSE, sep = "^")$V1


X     <-  do.call(rbind, strsplit(Input, ""))

N.Col   <-  ncol(X)
N.Row   <-  nrow(X)


options(warn = -1)
X.n    <-  array(dim = dim(X), as.numeric(X)) 
options(warn = 1)


X.num   <-  !is.na(X.n)
X.dot   <-  X  == "."
X.sym   <-  !X.num & !X.dot


#--- Find adjacent points

Y.adj   <-  array(dim = dim(X), 0)

for(i in 1:N.Row){
for(j in 1:N.Col){

  X.a  <- pmax(j - 1, 0)
  X.b  <- pmin(j + 1, N.Col)
  Y.a  <- pmax(i - 1, 0)
  Y.b  <- pmin(i + 1, N.Col)
  
  if(X.sym[i,j] == 1){ Y.adj[Y.a:Y.b, X.a:X.b]  <-  1 }
  
}}

#--- Adjacent Numbers 
Z.Prod   <-  Y.adj *  X.num



#--- Roll  adjacency left and right 
for(i in 2:N.Col){
  
  Z.Prod[,i]  <-  pmin(Z.Prod[,i] + Z.Prod[,i-1] * X.num[,i], 1)


}

for(i in (N.Col - 1):1){
  
  Z.Prod[,i]  <-  pmin(Z.Prod[,i]  + Z.Prod[,i+1] * X.num[,i] , 1)
  
  
}


#--- Stringify good numbers 
Z.n                      <-  X.n
Z.n[which(Z.Prod == 0)]  <-  NA

Z.n[which(is.na(Z.n))]   <-  "_"


Z <-  apply(Z.n, 1, paste, collapse = "")

Z.Split   <-  strsplit(Z, "_")
Z.Split   <-  unlist(Z.Split)

sum(as.numeric(Z.Split), na.rm = TRUE)






######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Input   <-  read.csv("Example_Day_03b.txt", header = FALSE, sep = "^")$V1

Input   <-  read.csv("Input_Day_03.txt", header = FALSE, sep = "^")$V1



X     <-  do.call(rbind, strsplit(Input, ""))

N.Col   <-  ncol(X)
N.Row   <-  nrow(X)


options(warn = -1)
X.n    <-  array(dim = dim(X), as.numeric(X)) 
options(warn = 1)
X.num   <-  !is.na(X.n)

X.star   <-  X  == "*"


Y.count   <-  array(dim = dim(X), 0)

for(i in 1:N.Row){
for(j in 1:N.Col){

      Left          <-  ifelse(j == 1                   , 0 , X.num[i, j - 1])
      Right         <-  ifelse(j == N.Col               , 0 , X.num[i, j + 1]) 
       
      North.W       <-  ifelse(j == 1     | i == 1      , 0 , X.num[i - 1, j - 1]) 
      North.E       <-  ifelse(j == N.Col | i == 1      , 0 , X.num[i - 1, j + 1])   
      North.C       <-  ifelse(             i == 1      , 0 , X.num[i - 1, j])     
        
      South.W       <-  ifelse(j == 1     | i == N.Row  , 0 , X.num[i + 1, j - 1]) 
      South.E       <-  ifelse(j == N.Col | i == N.Row  , 0 , X.num[i + 1, j + 1])   
      South.C       <-  ifelse(             i == N.Row  , 0 , X.num[i + 1, j])   
      
      North         <-  (North.W + North.E + North.C ) - (North.W * North.C) - (North.E * North.C)
      South         <-  (South.W + South.E + South.C ) - (South.W * South.C) - (South.E * South.C)
      
      Y.count[i,j]  <-  Left + Right + North + South
      
    
  }}


Y.Gear    <-  X.star * (Y.count == 2)


#--- Gear Adjacent Numbers

v.i   <-  numeric(0)
v.j   <-  numeric(0)
v.n1  <-  numeric(0)
v.n2  <-  numeric(0)

Counter  <-  1

for(i in 1:N.Row){
for(j in 1:N.Col){
    
  
  if(Y.Gear[i,j] == 1){
    
    Y.adj   <-  array(dim = dim(X), 0)
  
    X.a  <- pmax(j - 1, 0)
    X.b  <- pmin(j + 1, N.Col)
    Y.a  <- pmax(i - 1, 0)
    Y.b  <- pmin(i + 1, N.Col)
    
    Y.adj[Y.a:Y.b, X.a:X.b]  <-  1
    
    Z.Prod   <-  Y.adj *  X.num
    
  #--- Roll  adjacency left and right 
  for(k in 2:N.Col){
    
    Z.Prod[,k]    <-  pmin(Z.Prod[,k] + Z.Prod[,k - 1] * X.num[,k], 1)
    
  }
  
  for(k in (N.Col - 1):1){
    
    Z.Prod[,k]  <-  pmin(Z.Prod[,k] + Z.Prod[,k + 1] * X.num[,k] , 1)
    
  }
    
    #--- Stringify good numbers 
    Z.n                      <-  X.n
    Z.n[which(Z.Prod == 0)]  <-  NA
    
    Z.n[which(is.na(Z.n))]   <-  "_"
    
    
    Z <-  apply(Z.n, 1, paste, collapse = "")
    
    Z.Split   <-  strsplit(Z, "_")
    Z.Split   <-  unlist(Z.Split)
    Z.Split   <-  as.numeric(Z.Split[which(Z.Split != "")])
    
    v.i[[Counter]]   <-  i
    v.j[[Counter]]   <-  j
    v.n1[[Counter]]  <-  Z.Split[[1]]
    v.n2[[Counter]]  <-  Z.Split[[2]]
    
    Counter  <-  Counter + 1

  }
  }}
DF   <-  data.frame(v.i, v.j, v.n1 , v.n2)


sum(DF$v.n1 * DF$v.n2)


i  <-  74
j  <-  108


X[(i-5):(i+5), (j-5):(j+5)]



Y.adj[(i-5):(i+5), (j-5):(j+5)]
Z.Prod[(i-5):(i+5), (j-5):(j+5)]

