
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_11")



Input    <-  read.csv("Example_Day_11.txt", header = FALSE, sep = "^")$V1

Input    <-  read.csv("Input_Day_11.txt", header = FALSE, sep = "^")$V1


X        <-  do.call(rbind, strsplit(Input, ""))
X        <-  1 * (X == "#") 



F.Expand    <-  function(X){
  
  z  <-  1
  
  repeat{
    
    N.Col   <-  ncol(X)
    
    if(z > N.Col){break}
    
    if(sum(X[,z]) == 0){
      

      Left    <-  X[,1:z]
      
      Dummy   <-  0 * X[,1]
      
      if(z < N.Col){Right  <-  X[,(z+1):N.Col]} else {Right  <-  NULL}
      
      X       <-    cbind(Left, Dummy, Right)    
      
      z       <-  z + 2
      
    } else {z   <- z +  1}
    
    
  }
  
  colnames(X)  <-  NULL
  
  
  z  <-  1
  
  repeat{
    
    N.Row   <-  nrow(X)
    
    if(z > N.Row){break}
    
    if(sum(X[z,]) == 0){
      
      
      Top    <-  X[1:z,]
      
      Dummy   <-  0 * X[1,]
      
      if(z < N.Row){Bottom  <-  X[(z+1):N.Row,]} else {Bottom  <-  NULL}
      
      X       <-    rbind(Top, Dummy, Bottom)    
      
      z       <-  z + 2
      
    } else {z   <- z +  1}
    
    
  }
  
  rownames(X)  <-  NULL
  
  return(X)
  
  
}


Y   <-  F.Expand(X)


Y.Grid.x    <-  t(array(dim=rev(dim(Y)), seq(1, ncol(Y))))
Y.Grid.y    <-  array(dim = dim(Y), seq(1, nrow(Y)))


Which    <-  which(Y == 1)

Points   <-  data.frame("X" = Y.Grid.x[Which],
                        "Y" = Y.Grid.y[Which])


N.Points  <-  length(Which)


Grid       <-  expand.grid("A" = seq(1, N.Points), "B" = seq(1, N.Points))
Grid       <-  Grid[which(Grid$A < Grid$B), ]

N.Grid     <-  nrow(Grid)


A          <-  Points[Grid$A, ]
B          <-  Points[Grid$B, ]


ManHat    <-  sum(abs(A - B))
ManHat




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2 (is really just an alternative approach to part 1)
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Input    <-  read.csv("Example_Day_11.txt", header = FALSE, sep = "^")$V1

Input    <-  read.csv("Input_Day_11.txt", header = FALSE, sep = "^")$V1

V         <-  1000000

X         <-  do.call(rbind, strsplit(Input, ""))
X         <-  1 * (X == "#") 



Y.Grid.x    <-  t(array(dim=rev(dim(X)), seq(1, ncol(X))))
Y.Grid.y    <-  array(dim = dim(X), seq(1, nrow(X)))


Which    <-  which(X == 1)

Points   <-  data.frame("X" = Y.Grid.x[Which],
                        "Y" = Y.Grid.y[Which])


N.Points  <-  length(Which)


Grid       <-  expand.grid("A" = seq(1, N.Points), "B" = seq(1, N.Points))
Grid       <-  Grid[which(Grid$A < Grid$B), ]

N.Grid     <-  nrow(Grid)




N.Col     <-  ncol(X)
N.Row     <-  nrow(X)

M.Col     <-  1 + (colSums(X) == 0) * (V - 1)  
M.Row     <-  1 + (rowSums(X) == 0) * (V - 1) 


Sig.Col   <-  cumsum(M.Col)
Sig.Row   <-  cumsum(M.Row)


Points$Sig.X   <-  Sig.Col[Points$X]
Points$Sig.Y   <-  Sig.Row[Points$Y]


  

A          <-  Points[Grid$A,c("Sig.X", "Sig.Y") ]
B          <-  Points[Grid$B,c("Sig.X", "Sig.Y") ]

  
  


ManHat    <-  sum(abs(A - B))
ManHat
















