######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

setwd("C:/Users/justi/OneDrive/Model/Advent of Code/2025/AoC_2025_09")


Data.0  <-  read.csv("AoC_Input_2025_09_0.txt", header = FALSE, sep = "")$V1
Data.1  <-  read.csv("AoC_Input_2025_09_1.txt", header = FALSE, sep = "")$V1

options(scipen = 99)

#====================================================================================================#
# Failures
#====================================================================================================#

# 2 Failures submitted on part 2. 
#   - first error (inpsired by chatGPT) was the assumption that testing corners of the rectangle would 
#      be sufficient - stupid not picking up on the likely error - plotting makes it clear. 
#   - second error was due to my boundary handling in the point in polygon test - 
#     originally I just put the boundary points themselves as inside which works for the first part 
#     but not testing all points between. Function now tests whether a point lies on any line
#     between the polygon vertices. 

# Fixing up the PlotInPoly to take boudary and handle hoziontal goes took a couple of goes. 

# This puzzle was great learning. 

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

F.Part.1  <- function(Data){
  
  Points    <-  do.call(rbind,lapply(strsplit(Data, split = ","), as.numeric))
  colnames(Points)   <-  c("X", "Y")
  
  N.Points      <-  nrow(Points) 
  
  Product       <-  expand.grid(A = seq(1,N.Points), B = seq(1, N.Points))
  
#   Product       <-  Product[which(Product$B > Product$A), ]
  
  
  Points.A      <-  Points[Product$A,]
  Points.B      <-  Points[Product$B,]
  
  # Note that we need to add one to each of the differences as we are counting whole squares - 
  #  zero width rectangles still have area
  Product$Area  <-  apply(abs(Points.A - Points.B) + 1,1, prod)
  
  return(max(  Product$Area))
  
  
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
Data   <-  Data.0
Data   <-  Data.1

N  <-   10
M  <-  3

# Start of package approach from ChatGPT
# install.packages('sf')
# library(sf)
# Poly <-  st_polygon(list(rbind(Points, Points[1,])))
# st_polygon(Points)

# To solve this I wrote the Lib.PointInPoly function. I knew I did not have an efficient method to 
#  solve the point in polygon problem in my intellectual tool boc. ChatGPT pointed me to some pacakge
#  functions but this was a good chance to understand the solution to this problem. ChatGPT gave me
#  the ray casting approach. 



#====================================================================================================#
# F.Sub.2   
#====================================================================================================#
# Run through sequence of points to find if any are included in a rectangle

F.Sub.2   <-  function(Product, Bad){
  
  N.Bad     <-  nrow(Bad)
  N.Prod    <-  nrow(Product)
  
  Safe    <-  rep(1, N.Prod)  
  
  for(i in 1:N.Bad){
    
    i.Bad.X    <-  Bad$X[i]
    i.Bad.Y    <-  Bad$Y[i]
    
    Test.X     <-  (Product$Points.A$X > i.Bad.X) != (Product$Points.B$X  > i.Bad.X) | 
                   (Product$Points.A$X == i.Bad.X) | (Product$Points.B$X == i.Bad.X)
    
    Test.Y     <-  (Product$Points.A$Y > i.Bad.Y) != (Product$Points.B$Y  > i.Bad.Y) | 
                   (Product$Points.A$Y == i.Bad.Y) | (Product$Points.B$Y == i.Bad.Y)
    
    Test       <-  Test.X & Test.Y
    
    Safe       <-  Safe * (!Test)
    
  }
  
  return(Safe)
  
}


#====================================================================================================#
# F.Part.2
#====================================================================================================#

F.Part.2  <- function(Data){
  
  Points    <-  do.call(rbind,lapply(strsplit(Data, split = ","), as.numeric))
  colnames(Points)   <-  c("X", "Y")
  Points             <-  data.frame(Points)
  
  N.Points      <-  nrow(Points) 
  
  Product       <-  expand.grid(A = seq(1,N.Points), B = seq(1, N.Points))
  Product       <-  Product[Product$A < Product$B, ]
  
  #   Product       <-  Product[which(Product$B > Product$A), ]
  
  
  Points.A         <-  data.frame(Points[Product$A,])
  Points.B         <-  data.frame(Points[Product$B,])
  
  Points.C         <-  data.frame(X = Points.A$X, 
                                  Y = Points.B$Y)
  
  Points.D         <-  data.frame(X = Points.B$X, 
                                  Y = Points.A$Y)


  Points.C.Inside  <-  Lib.PointInPoly(Points.C$X, Points.C$Y, Points$X, Points$Y)$InBound
  Points.D.Inside  <-  Lib.PointInPoly(Points.D$X, Points.D$Y, Points$X, Points$Y)$InBound
  
  
  
  # Note that we need to add one to each of the differences as we are counting whole squares - 
  #  zero width rectangles still have area
  Product$Area     <-  (abs(Points.A$X - Points.B$X) + 1) * (abs(Points.A$Y - Points.B$Y) + 1)
  Product$Inside   <-   Points.C.Inside & Points.D.Inside
  
  
  
  # Stopped here but got an error. Looking at data it seems that the points are correctly calssified 
  # in-out but the polygon is bascially a circle with a horizontal hole - checking the 4 corners is not 
  # sufficient. 
  
  # will try to throw away any points that fail the 4 corners, order by area, then loop and algorithm
  # top to bottom until we find a safe rectangle, will check all points on the border of the rectangle,
  # identify bad points, then throw away any pairs that include bad points.
  
  Product$Points.A    <-  Points.A
  Product$Points.B    <-  Points.B
  Product$Points.C    <-  Points.C
  Product$Points.D    <-  Points.D

  
  Product             <-  Product[which(Product$Inside == TRUE), ]
  Product             <-  Product[order(Product$Area,decreasing = TRUE), ]
  

  repeat{
  
  
  AC.y                <-  seq(Product$Points.A$Y[1], Product$Points.C$Y[1])
  AD.x                <-  seq(Product$Points.A$X[1], Product$Points.D$X[1])
  
  BC.x                <-  seq(Product$Points.B$X[1], Product$Points.C$X[1])
  BD.y                <-  seq(Product$Points.B$Y[1], Product$Points.D$Y[1])
  
  A.x                 <-  Product$Points.A$X[1]
  A.y                 <-  Product$Points.A$Y[1]
  B.x                 <-  Product$Points.B$X[1]
  B.y                 <-  Product$Points.B$Y[1]
  
  AC.x                <-  rep(A.x,length(AC.y))
  AD.y                <-  rep(A.y,length(AD.x))
                              
  BC.y                <-  rep(B.y,length(BC.x))
  BD.x                <-  rep(B.x,length(BD.y))
  
  
  Test.AC             <-  Lib.PointInPoly(AC.x, AC.y, Points$X, Points$Y)$InBound
  Test.AD             <-  Lib.PointInPoly(AD.x, AD.y, Points$X, Points$Y)$InBound
  
  Test.BC             <-  Lib.PointInPoly(BC.x, BC.y, Points$X, Points$Y)$InBound
  Test.BD             <-  Lib.PointInPoly(BD.x, BD.y, Points$X, Points$Y)$InBound
  
  if(prod(c(Test.AC, Test.AD, Test.BC, Test.BD)) == 1){
    
    break
    
  } else {
    
    Bad.AC          <-  data.frame(X = AC.x, Y = AC.y)[which(!Test.AC), ]
    Bad.AD          <-  data.frame(X = AD.x, Y = AD.y)[which(!Test.AD), ]
    Bad.BC          <-  data.frame(X = BC.x, Y = BC.y)[which(!Test.BC), ]
    Bad.BD          <-  data.frame(X = BD.x, Y = BD.y)[which(!Test.BD), ]
    
    Bad             <-  Lib.ListBind.ByNames(list(Bad.AC, Bad.AD, Bad.BC, Bad.BD))
     
    Product.Safe    <-  F.Sub.2(Product, Bad) 
    
    Product         <-  Product[which(Product.Safe== 1), ]
    
 }
}
  
  
  
  return(max(Product$Area))
  
  
}  

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)



4588384997
1297583960

Col.A     <-  rep('red' ,nrow(Points.A))
Col.A[which(Points.A.Inside)]  <-  'blue'



Col.C     <-  rep('red' ,nrow(Points.C))
Col.C[which(Points.C.Inside)]  <-  'blue'



Lib.Plot.Blank(c(0,100000), c(0,100000))
polygon(Points$X, Points$Y)
points(Points.C$X, Points.C$Y, cex =.2 , pch = 16, col = Col.C)


points(Points.A$X, Points.A$Y, cex = 1.5, pch = 16, col = Col.A)







# plot the results of main rectangle test - Full 


Lib.Plot.Blank(c(0,100000), c(0,100000))
polygon(Points$X, Points$Y)
points(Points$X,  Points$Y, pch = 16, col = "black", cex = 1)


points(A.x, A.y, cex = 2, pch = 16, col = "green")
points(B.x, B.y, cex = 2, pch = 16, col = "green")


Col.AC   <-   rep('red' , length(Test.AC))
Col.AC[which(Test.AC)]  <-  'blue'
points(AC.x, AC.y, cex = .55, pch = 16, col = Col.AC)

Col.AD   <-   rep('red' , length(Test.AD))
Col.AD[which(Test.AD)]  <-  'blue'
points(AD.x, AD.y, cex = .55, pch = 16, col = Col.AD)

Col.BC   <-   rep('red' , length(Test.BC))
Col.BC[which(Test.BC)]  <-  'blue'
points(BC.x, BC.y, cex = .55, pch = 16, col = Col.BC)

Col.BD   <-   rep('red' , length(Test.BD))
Col.BD[which(Test.BD)]  <-  'blue'
points(BD.x, BD.y, cex = .55, pch = 16, col = Col.BD)







Lib.Plot.Blank(c(A.x - 100 , A.x + 100), c(B.y -100 , B.y + 100 ))
polygon(Points$X, Points$Y)

points(A.x, A.y, cex = 2, pch = 16, col = "green")
points(B.x, B.y, cex = 2, pch = 16, col = "green")


points(AC.x, AC.y, cex = .5, pch = 16, col = "orange")
points(AD.x, AD.y, cex = .5, pch = 16, col = "orange")
points(BC.x, BC.y, cex = .5, pch = 16, col = "orange")
points(BD.x, BD.y, cex = .5, pch = 16, col = "orange")

points(AC.x, AC.y, cex = .5, pch = 16, col = Col.AC)
points(AD.x, AD.y, cex = .5, pch = 16, col = Col.AD)
points(BC.x, BC.y, cex = .5, pch = 16, col = Col.BC)
points(BD.x, BD.y, cex = .5, pch = 16, col = Col.BD)



Lib.Plot.Blank(c(B.x - 100 , B.x + 100), c(B.y -100 , B.y + 100 ))
polygon(Points$X, Points$Y)

points(A.x, A.y, cex = 2, pch = 16, col = "green")
points(B.x, B.y, cex = 2, pch = 16, col = "green")


points(AC.x, AC.y, cex = .5, pch = 16, col = "orange")
points(AD.x, AD.y, cex = .5, pch = 16, col = "orange")
points(BC.x, BC.y, cex = .5, pch = 16, col = "orange")
points(BD.x, BD.y, cex = .5, pch = 16, col = "orange")

points(AC.x, AC.y, cex = .5, pch = 16, col = Col.AC)
points(AD.x, AD.y, cex = .5, pch = 16, col = Col.AD)
points(BC.x, BC.y, cex = .5, pch = 16, col = Col.BC)
points(BD.x, BD.y, cex = .5, pch = 16, col = Col.BD)




# plot the results of main rectangle test


Lib.Plot.Blank(c(A.x - 100 , A.x + 100), c(A.y -100 , A.y + 100 ))

polygon(Points$X, Points$Y)
points(Points$X,  Points$Y, pch = 16, col = "black", cex = 1)

Col.AC   <-   rep('red' , length(Test.AC))
Col.AC[which(Test.AC)]  <-  'blue'
points(AC.x, AC.y, cex = .55, pch = 16, col = Col.AC)

Col.AD   <-   rep('red' , length(Test.AD))
Col.AD[which(Test.AD)]  <-  'blue'
points(AD.x, AD.y, cex = .55, pch = 16, col = Col.AD)

Col.BC   <-   rep('red' , length(Test.BC))
Col.BC[which(Test.AC)]  <-  'blue'
points(BC.x, BC.y, cex = .55, pch = 16, col = Col.BC)

Col.BD   <-   rep('red' , length(Test.BD))
Col.BD[which(Test.BD)]  <-  'blue'
points(BD.x, BD.y, cex = .55, pch = 16, col = Col.BD)

points(A.x, A.y, cex = 2, pch = 16, col = "green")
points(B.x, B.y, cex = 2, pch = 16, col = "green")

Lib.Plot.Blank(c(0,100000), c(0,100000))
polygon(Points$X, Points$Y)
points(Bad$X, Bad$Y, cex = .55, pch = 16, col = "red")


Test.Bad    <-  Lib.Tuple.Match(Bad, Points)

A.x
A.y


Bad

Bad.BC.x   <-  BC.x[which(!Test.BC)]
Bad.BC.y   <-  BC.y[which(!Test.BC)]



Lib.PointInPoly(Bad.BC.x,Bad.BC.y  ,Points$X, Points$Y)


P.x     <-  head(Bad.BC.x)
P.y     <-  head(Bad.BC.y)
Poly.x  <-  Points$X
Poly.y  <-  Points$Y

Points[c(438, 439, 440), ]







