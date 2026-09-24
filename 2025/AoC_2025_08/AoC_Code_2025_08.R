######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_08")


Data.0  <-  read.csv("AoC_Input_2025_08_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_08_1.txt", header = FALSE, sep = "")$V1

options(scipen = 99)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.0
Data   <-  Data.1

N  <-   10
M  <-  3


#====================================================================================================#
# F.Part.1
#====================================================================================================#

F.Part.1  <-  function(Data, N, M){
  
  #--- Data setup
  
  Points    <-  do.call(rbind,lapply(strsplit(Data, split = ","), as.numeric))
  colnames(Points)   <-  c("X", "Y", "Z")
  
 
  N.Points      <- nrow(Points) 
  
  Product       <-  expand.grid(A = seq(1,N.Points), B = seq(1, N.Points))
  
  Product       <-  Product[which(Product$B > Product$A), ]
  
  
  Points.A      <-  Points[Product$A,]
  Points.B      <-  Points[Product$B,]
  
  
  Product$Dist  <-  rowSums(abs(Points.A - Points.B)^2)^.5 
  
  Product       <-  Product[order(Product$Dist), ]
  

  DF            <-  data.frame(ID      = seq(1, N.Points), 
                               Cluster = seq(1, N.Points) )
  
  
  for(i in 1:N){
    
    i.A    <-  Product$A[[i]]
    i.B    <-  Product$B[[i]]
    
    Cluster.A  <-  DF$Cluster[i.A]
    Cluster.B  <-  DF$Cluster[i.B]
    
    DF$Cluster[which(DF$Cluster == Cluster.B )]  <-  Cluster.A
    
    print(i)
    
  }
  
  Table    <-  sort(table(DF$Cluster), decreasing = TRUE)
  
  
  return(prod(as.numeric(Table[1:M])))
  
}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.0, 10, 3)
F.Part.1(Data.1, 1000, 3)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Data   <-  Data.0
Data   <-  Data.1




#====================================================================================================#
# F.Part.2
#====================================================================================================#

F.Part.2  <-  function(Data){
  
  #--- Data setup
  
  Points    <-  do.call(rbind,lapply(strsplit(Data, split = ","), as.numeric))
  colnames(Points)   <-  c("X", "Y", "Z")
  
  
  N.Points      <- nrow(Points) 
  
  Product       <-  expand.grid(A = seq(1,N.Points), B = seq(1, N.Points))
  
  Product       <-  Product[which(Product$B > Product$A), ]
  
  
  Points.A      <-  Points[Product$A,]
  Points.B      <-  Points[Product$B,]
  
  
  Product$Dist  <-  rowSums(abs(Points.A - Points.B)^2)^.5 
  
  Product       <-  Product[order(Product$Dist), ]
  
  
  DF            <-  data.frame(ID      = seq(1, N.Points), 
                               Cluster = seq(1, N.Points) )
  
    i  <-  0
  
    repeat{
  
    i  <-  i + 1  
    print(i)
    
    i.A    <-  Product$A[[i]]
    i.B    <-  Product$B[[i]]
    
    Cluster.A  <-  DF$Cluster[i.A]
    Cluster.B  <-  DF$Cluster[i.B]
    
    DF$Cluster[which(DF$Cluster == Cluster.B )]  <-  Cluster.A
    
 
    
    if(length(unique(DF$Cluster)) == 1){ break }
    
  }
  
  Points   <-  data.frame(Points)
    
  X.A      <-  Points$X[i.A]  
  X.B      <-  Points$X[i.B]  
    

  
  
  return(X.A * X.B)
  
}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)




