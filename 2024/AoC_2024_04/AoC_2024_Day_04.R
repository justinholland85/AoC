
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/2024/Day_04")
options(scipen = 99)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Name.Example      <-  "Example_Day_04.txt"
Name.Input        <-  "Input_Day_04.txt"


Example           <-  read.csv(Name.Example, header = FALSE, sep = "^")$V1
Input             <-  read.csv(Name.Input,   header = FALSE, sep = "^")$V1


Data.Example      <-  do.call(rbind,strsplit(Example, ""))
Data.Input        <-  do.call(rbind,strsplit(Input, ""))

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 1 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

#====================================================================================================#
# F.Grid
#====================================================================================================#

Data    <- Data.Input

F.Grid            <-  function(Data){
  
  
  N.X             <-  ncol(Data)
  N.Y             <-  nrow(Data)
  
  Grid            <-  Lib.Gridify(X = seq(1,N.X), Y = seq(1, N.Y))
  
  
  DF              <-  data.frame("Letter" = c(Data), 
                                 "X"      = c(Grid$Mat.X), 
                                 "Y"      = c(Grid$Mat.Y)  )
  
  return(DF)
  
  
}



#====================================================================================================#
# F.Word.Dir
#====================================================================================================#

Grid.DF     <-  F.Grid(Data)


X   <-  50
Y   <-  50


dX  <-  1
dY  <-  -1

N  <-  4

F.Word.Dir  <-  function(X, Y, dX, dY, N, Grid.DF){
  
  
  vX          <-  X + seq(0,N-1) * dX
  vY          <-  Y + seq(0,N-1) * dY
  
  Match       <-  Lib.Tuple.Match(data.frame(vX, vY), 
                                  Grid.DF[,c("X", "Y")])
  
  
  Word        <-  Grid.DF$Letter[Match] 
  
  Output      <-  list("Word" = Word)
  
  return(Output)
  
}

#====================================================================================================#
# F.Word.AllDirs
#====================================================================================================#

F.Word.AllDirs    <-  function(X, Y, N, Grid.DF){
  
  
  
  Directions    <-  expand.grid(dX = c(-1, 0,  1), 
                                dY = c(-1, 0 , 1))
  
  
  Words         <-  list()

  for(i in 1:nrow(Directions)){
    
    Words[[i]]   <-  F.Word.Dir(X, Y, Directions$dX[[i]], Directions$dY[[i]], N, Grid.DF)$Word
    
  }  
  
  return(Words)
  
}


#====================================================================================================#
# F.Part.1
#====================================================================================================#


F.Part.1  <-  function(Data){
  
  Grid.DF     <-  F.Grid(Data)
  
  Base        <-  Grid.DF[which(Grid.DF$Letter == "X"), ]
  
  List        <-  list()  
  
  for(i in 1:nrow(Base)){

    List[[i]]   <-  F.Word.AllDirs(Base$X[[i]], 
                                   Base$Y[[i]], 
                                   N,
                                   Grid.DF)
    
    print(i)
    
  }
  
  List  <-  do.call(c, List)
  
  
  Words   <-  unlist(lapply(List, paste, collapse =""))
  
  return(sum(Words == "XMAS"))
  
  
}




#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.1(Data.Example)
F.Part.1(Data.Input)







######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

#====================================================================================================#
# F.X.Letters
#====================================================================================================#


Data   <- Data.Input
  
Grid.DF     <-  F.Grid(Data)


X  <-  50
Y  <-  50


F.Letters.X  <-  function(X, Y, Grid.DF){
  
  
  X.a          <-  X - 1
  X.b          <-  X + 1
  Y.a          <-  Y - 1 
  Y.b          <-  Y + 1 
  
  vX           <-  c(X.a, X.a, X.b, X.b)
  vY           <-  c(Y.a, Y.b, Y.b, Y.a)
  
  Match        <-  Lib.Tuple.Match(data.frame(vX, vY), 
                                  Grid.DF[,c("X", "Y")])
  
  
  Letters      <-  Grid.DF$Letter[Match] 
  
  Output      <-  list("Letters" = Letters)
  
  return(Output)
  
}




#====================================================================================================#
# F.Part.2
#====================================================================================================#



F.Part.2  <-  function(Data){
  
  
  N.X            <-  ncol(Data)
  N.Y            <-  nrow(Data)
  
  Grid.DF        <-  F.Grid(Data)
  
  Base           <-  Grid.DF[which(Grid.DF$Letter == "A"), ]
   
  Point.Tests    <-  numeric(0)  
  
  Point.Letters  <-  list()
  
  Point.Square   <-  list()
  
  
  for(i in 1:nrow(Base)){
    
    X   <-  Base$X[[i]]
    Y   <-  Base$Y[[i]]
    
    
    Letters.X          <-  F.Letters.X(X, 
                                      Y,
                                      Grid.DF)[[1]]
    
    
    Point.Letters[[i]]  <-  Letters.X
    
    
    Point.Square[[i]]   <-  Data[  pmax(pmin(seq(Y - 1 ,Y + 1), N.Y), 1),
                                   pmax(pmin(seq(X - 1, X + 1), N.X), 1) ]
    
    Count.M             <-  sum(Lib.NA.To.Zero(Letters.X == "M"))
    Count.S             <-  sum(Lib.NA.To.Zero(Letters.X == "S"))
    Diag.Diff           <-  Lib.NA.To.Zero(Letters.X[1] != Letters.X[3])
    
      
    Test                <-  Count.M == 2 & Count.S == 2 & Diag.Diff == 1
    
    Point.Tests[[i]]    <-  Test
    
    print(i)
    
  }
  
  
## Insepection  
 # Which    <-  which(Point.Tests == 1)
  
 # k  <-  3
  
 # Point.Letters[[Which[[k]]]]
  
  #Point.Square[[Which[[k]]]]


  return(sum(Point.Tests))
  
  
}


#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.2(Data.Example)
F.Part.2(Data.Input)










