
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_10")



Input    <-  read.csv("Example_Day_10.txt", header = FALSE, sep = "^")$V1

Input    <-  read.csv("Input_Day_10.txt", header = FALSE, sep = "^")$V1


X        <-  do.call(rbind, strsplit(Input, ""))
X        <-  cbind(".",  rbind(".", X, "."), ".")



F.Square   <-  function(Row, Col, X){
  
  A.0   <-  pmax(Row - 1, 1)
  A.1   <-  pmin(Row + 1, N.Row)
  
  B.0   <-  pmax(Col - 1, 1)
  B.1   <-  pmin(Col + 1, N.Col)
  
  
  return(X[A.0:A.1, B.0:B.1])
  
  
}

Chars    <-  c("|",
               "-", 
               "L", 
               "J",
               "7",
               "F"
               )


Links    <-  list(c(1,5),
                  c(3,7),
                  c(1,3),
                  c(1,7),
                  c(5,7),
                  c(3,5))









N.Col      <-  ncol(X)
N.Row      <-  nrow(X)

X.Col      <-  t(array(dim = c(N.Row, N.Col), seq(1, N.Col)))
X.Row      <-  array(dim = c(N.Col, N.Row), seq(1, N.Row))

Start.Row  <-  X.Row[which(X == "S")] 
Start.Col  <-  X.Col[which(X == "S")] 



Square.Ref   <-  do.call(rbind, list(c(8,1,2),
                                     c(7,0,3),
                                     c(6,5,4)))


Square.Ops  <-   do.call(rbind, list(c(4,5,6),
                                     c(3,0,7),
                                     c(2,1,8)))
         


F.CompatSets    <-  function(Links, Square.Ref, Square.Ops){
  
  N.Links   <-   length(Links)
  
  Map.Inversion   <-  data.frame(Square.Ref = c(Square.Ref), 
                                 Square.Ops = c(Square.Ops))
  
  
  
  Links           <-  do.call(rbind, Links)
  Links.Inv       <-  array(dim = dim(Links), Map.Inversion$Square.Ops[match(c(Links),  Map.Inversion$Square.Ref)])
  
  
  Grid            <-  expand.grid(Base = seq(1,N.Links),
                                  Inv  = seq(1,N.Links))
  
  
  Grid.Orig       <-  Lib.RowsToList(Links)[Grid$Base]
  Grid.Inv        <-  Lib.RowsToList(Links.Inv)[Grid$Inv]
  
  Intersect       <-  mapply(intersect, Grid.Orig, Grid.Inv, SIMPLIFY = FALSE)
  
  Length          <-  unlist(lapply(Intersect, length))
  
  x.From          <-  rep(Grid$Base, Length)
  x.To            <-  rep(Grid$Inv, Length)
  x.Loc           <-  unlist(Intersect)
  
  Compat          <-  data.frame("From" = x.From, 
                                 "To"   = x.To, 
                                 "Loc"  = x.Loc)
  
  
  return(Compat)
  
}



CompatSets   <-  F.CompatSets(Links, Square.Ref, Square.Ops)





                

X.Num        <-  array(dim = dim(X), match(c(X), Chars))

Square       <- F.Square(Start.Row,  Start.Col, X.Num)
Square.Orig  <- F.Square(Start.Row,  Start.Col, X)

Square[2,2]  <- 1


F.Compat   <-  function(Square, Links, Square.Ref){
  
  
  Base                                 <-  Square[which(Square.Ref == 0)] 

  Compat                               <-  0 * Square.Ref
  
  Test.Sets                            <-  data.frame("From" = Base, 
                                                      "To"   = c(Square),
                                                      "Loc"  = c(Square.Ref))
  
  Match                                <-  Lib.Tuple.Match(Test.Sets, 
                                                           CompatSets)
  
  Which                                <-  which(!is.na(Match))
  
  Compat[match(Test.Sets$Loc[Which], c(Square.Ref) )]  <-  1
  
  Pos                                  <-  Square.Ref[which(Compat == 1)]
  
  N                                    <-  sum(Compat)
  
  Output                               <-  list("Compat" = Compat, 
                                                "Pos"    = Pos, 
                                                "N"      = N)
  
  
  return(Output)
  
  }



F.Start                                <-  function(Square, Links, Square.Ref){
  
  N.Links   <-  length(Links)
  
  for(i in 1:N.Links){
    
    Square[2,2]  <-  i
    
    Compat       <-  F.Compat(Square, Links, Square.Ref) 
    
    if(Compat$N == 2){
      
      Value      <-  i
      NextPos    <-  min(Compat$Pos)
      
      break
      
    }
    
   
      }
  
  Output    <-  list("Value"   = Value, 
                     "NextPos" = NextPos)
  
  
  return(Output)
  
  
}


F.Start(Square, Links, Square.Ref)


#====================================================================================================#
# F.AdjPos: Modify row and column to next position
#====================================================================================================#

F.AdjPos    <-  function(Row, Col, NextPos, Square.Ref){
  

  Adj.Col   <-  do.call(rbind, list(c(-1,0,1),
                                    c(-1,0,1),
                                    c(-1,0,1)))
  

  
  Adj.Row   <-  do.call(rbind, list(c(-1,-1,-1),
                                    c(0,0,0),
                                    c(1,1,1)))
  
  Match     <-  match(NextPos, c(Square.Ref))
  
  Row.Next  <-  Row + Adj.Row[[Match]]
  Col.Next  <-  Col + Adj.Col[[Match]]
  
  
  Output    <-  list(Row.Next = Row.Next, 
                     Col.Next = Col.Next)
  
  return(Output)
  
  
}
  


#====================================================================================================#
#  Main Loop to follow the path
#====================================================================================================#


  N.Col        <-  ncol(X)
  N.Row        <-  nrow(X)
  
  X.Col        <-  t(array(dim = c(N.Col, N.Row), seq(1, N.Col)))
  X.Row        <-  array(dim = c(N.Row, N.Col), seq(1, N.Row))
  
  Start.Row    <-  X.Row[which(X == "S")] 
  Start.Col    <-  X.Col[which(X == "S")] 
  
  X[which(X == "S")]
  
  X.Num        <-  array(dim = dim(X), match(c(X), Chars))
  
  Square       <-  F.Square(Start.Row,  Start.Col, X.Num)
  Square.Orig  <-  F.Square(Start.Row,  Start.Col, X)
  
  
  Start        <-  F.Start(Square, Links, Square.Ref)
  NextPos      <-  Start$NextPos
  X.Num[Start.Row, Start.Col]  <-  Start$Value
  
  
  Path.Row     <-  Start.Row
  Path.Col     <-  Start.Col
  
  X.Path       <-  0 * Lib.NA.To.Zero(X.Num)   
  X.Path[Start.Row, Start.Col]   <-  Start$NextPos
  
  Row          <-  Start.Row
  Col          <-  Start.Col
  
  Pos.Prev     <-  Square.Ops[match(NextPos, Square.Ref)]
  
  
  
  
  repeat{
    
    AdjPos       <-  F.AdjPos(Row, Col, NextPos, Square.Ref) 

    Row          <-  AdjPos$Row.Next
    Col          <-  AdjPos$Col.Next
    
    Path.Row     <-  c(Path.Row, Row)
    Path.Col     <-  c(Path.Col, Col)
    

    
    if(Row == Start.Row & Col == Start.Col){break}
    
    Square               <-  F.Square(Row,  Col, X.Num)
    Square.Orig          <-  F.Square(Row,  Col, X)
    
    
    Compat               <-  F.Compat(Square, Links, Square.Ref)
    
    NextPos              <-  setdiff(Compat$Pos, Pos.Prev)
    
    X.Path[Row, Col]     <-  NextPos
    
    Pos.Prev     <-  Square.Ops[match(NextPos, Square.Ref)]
    
    if(length(Path.Row) %% 1000 == 0){print(length(Path.Row))}
    
  }
  
  
  
  


(length(Path.Row) - 1) / 2


head(Path.Row)
head(Path.Col)

tail(Path.Row)
tail(Path.Col)




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


N.Col.X    <-  ncol(X)
N.Row.X    <-  nrow(X)

X.Grid.x   <-  t(array(dim = rev(dim(X)),seq(1, N.Col.X) ))
X.Grid.y   <-  array(dim = dim(X),seq(1, N.Row.X) )


Y.Grid.x   <-  t(array(dim = rev(2 * dim(X) -1), seq(1, N.Col.X , .5) ))
Y.Grid.y   <-  array(dim = 2 * dim(X) -1 ,seq(1, N.Row.X, .5) )


Y         <-  array(dim = dim(Y.Grid.x), 0)


N.Col      <-  ncol(Y)
N.Row      <-  nrow(Y)



N.Path    <-  length(Path.Row) 
Length    <-  numeric(0)

for(i in 1:(N.Path - 1)){
  
  A.Row  <-  Path.Row[[i]]
  A.Col  <-  Path.Col[[i]]
  
  B.Row  <-  Path.Row[[i + 1]]
  B.Col  <-  Path.Col[[i + 1]]
  
  Row.Min  <-  min(A.Row, B.Row)
  Row.Max  <-  max(A.Row, B.Row)
  
  Col.Min  <-  min(A.Col, B.Col)
  Col.Max  <-  max(A.Col, B.Col)
  
  Which  <-  which(Row.Min <= Y.Grid.y & Y.Grid.y <= Row.Max & 
                   Col.Min <= Y.Grid.x & Y.Grid.x <= Col.Max  )
  
  
  Length[[i]]  <-  length(Which)
  
  Y[Which]  <- 1
  
  if(i %% 1000 == 0){print(i)}
  
}


Y[1,]      <-  -1
Y[,1]      <-  -1
Y[N.Row,]  <-  -1
Y[,N.Col]  <-  -1



#====================================================================================================#
# Coils
#====================================================================================================#



Coil          <-  0 * Y
Coil[1,]      <-  -1
Coil[,1]      <-  -1
Coil[N.Row,]  <-  -1
Coil[,N.Col]  <-  -1


A.Min    <-  2
A.Max    <-  nrow(Y) - 1

B.Min    <-  2
B.Max    <-  ncol(Y) - 1

Dir      <- 1

x    <-  B.Min
y    <-  A.Min 

z    <-  0

#for( i in 1:32000){
repeat{
  z  <-  z + 1
  
  Coil[y,x]  <-  z
  
  if(Dir == 1){
    
    if(x < B.Max){ x   <- x + 1} else { Dir <- 2}

    }

  
  if(Dir == 2){
    
    if(y < A.Max){ y     <-  y + 1} else {Dir  <-  3}

  }
  
  if(Dir == 3){
   
     if(x > B.Min){ x   <- x - 1} else {Dir <- 4}

  }
  
  if(Dir == 4){
    
    if(y >  A.Min + 1){ y     <-  y - 1} else { Dir <- 5}

  }
  
  if(Dir == 5){
    
    Delta.A   <-  A.Max - A.Min
    Delta.B   <-  B.Max - B.Min
    
    if(Delta.A > 2  & Delta.B > 2){ 
      
      A.Min   <-  A.Min + 1
      A.Max   <-  A.Max - 1
      
      B.Min   <-  B.Min + 1
      B.Max   <-  B.Max - 1
      
      x       <-  B.Min
      y       <-  A.Min
      
      Dir     <-  1
      
    } else {Dir <-  6}
    
  }
    
    if(Dir == 6){break}
    
  if(z %% 1000 == 0){print(z)}
    
}

# The above doesn't finish the last spot, solution and fix below is not general. 
N.Col.Y   <-  ncol(Y)

N.Row.Y   <-  nrow(Y)

Remainder    <-  sum(Coil == 0)

Coil[which(Coil == 0)]  <-  max(Coil) + seq(1, Remainder)

Coil.0   <-  Coil


#====================================================================================================#
# Coil.1
#====================================================================================================#
Coil.1  <-  Y * 0
Coil.2  <-  Y * 0

Coil.1[which(Coil.0 == -1)]  <-  -1
Coil.2[which(Coil.0 == -1)]  <-  -1

z  <-  0

for(i in 2:(nrow(Y) - 1)){
for(j in 2:(ncol(Y) - 1)){
  
  z            <-  z + 1
  
  Coil.1[i,j]  <-  z
  
}}

z  <-  0
for(j in 2:(ncol(Y) - 1)){
for(i in 2:(nrow(Y) - 1)){

    z            <-  z + 1
    
    Coil.2[i,j]  <-  z
    
  }}

Coil.3  <-  Coil.1[,seq(ncol(Y), 1)]   
Coil.4  <-  Coil.2[seq(nrow(Y), 1),]   


Coils    <-  list(Coil.0,
                  Coil.1,
                  Coil.2,
                  Coil.3,
                  Coil.4)

#====================================================================================================#
# F.Infiltrate
#====================================================================================================#


F.Infiltrate   <-   function(Y, Coil){


Max.Coil   <-  max(Coil)


N.Col      <-  ncol(Y)
N.Row      <-  nrow(Y)


for(i in 1:Max.Coil){
  
  Row   <-  which(rowSums(Coil == i) == 1)
  
  Col   <-  which(colSums(Coil == i) == 1)
  
  
  Square   <-  F.Square(Row, Col, Y)
  
  Touching    <-  Square[match(c(1,3,5,7), Square.Ref)]
  
  if(Square[2,2] == 0 & sum(Touching == -1) > 0 ){Y[Row, Col] <- -1}
  
  if(i %% 1000 == 0){print(i)}
  
}


return(Y)

}







#====================================================================================================#
# Infiltration Loop
#====================================================================================================#

z        <-  0

repeat{

z        <-  z + 1
  
z.Coil   <-  Coils[[z %% 5 + 1]]

Y.In     <-  Y
Y.Out    <-  F.Infiltrate(Y.In, z.Coil)

Coil     <-  Coil

Delta     <-  sum(Y.In == 0) - sum(Y.Out == 0)

Y         <-  Y.Out

print(paste0("z = ", z, " ; Delta = ", Delta))

if(Delta == 0){break}

}




colSums(Y == -1)

colSums(Y == 1)



colSums(Y.Grid.x %% 1 == 0 & Y.Grid.y %% 1 == 0)

colSums(Y == 0 & Y.Grid.x %% 1 == 0 & Y.Grid.y %% 1 == 0)

sum(Y == 0 & Y.Grid.x %% 1 == 0 & Y.Grid.y %% 1 == 0)

colSums(Y == 0)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Debug Graphics
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Y.Grid.x


Y.Plot.y    <- max(Y.Grid.y) - Y.Grid.y
Y.Plot.x    <-  Y.Grid.x

X.Plot.y    <- max(X.Grid.y) - X.Grid.y
X.Plot.x    <-  X.Grid.x



X.Lim       <-  c(0, max(Y.Plot.x))
Y.Lim       <-  c(0, max(Y.Plot.y))

X.Axis      <-  seq(0,max(X.Lim), max(X.Lim) /11)
Y.Axis      <-  seq(0,max(Y.Lim), max(Y.Lim) /11)

Lib.Plot.Blank(X.Lim, Y.Lim)
Lib.PlotLines.X(X.Axis, Y.Lim )
Lib.PlotLines.Y(Y.Axis, X.Lim )

for(i in 1:(N.Path - 1)){

  Col.A   <-  Path.Col[[i]]
  Col.B   <-  Path.Col[[i+1]]
  
  Row.A   <-  Path.Row[[i]]
  Row.B   <-  Path.Row[[i+1]]
  
  
  x.A      <-  X.Plot.x[Row.A, Col.A]
  y.A      <-  X.Plot.y[Row.A, Col.A]
  
  x.B      <-  X.Plot.x[Row.B, Col.B]
  y.B      <-  X.Plot.y[Row.B, Col.B]
  
  
  points(c(x.A, x.B), c(y.A, y.B), type  = 'b', pch = 16, col = 'blue')
  
  
  
}


Which.0    <-  which(Y == 0 & Y.Grid.x %% 1 == 0 & Y.Grid.y %% 1 == 0)

Which.1    <-  which(Y == 1)

Which.2    <-  which(Y == -1)

points(Y.Plot.x[Which.0], Y.Plot.y[Which.0], pch = 16, col = 'red')
points(Y.Plot.x[Which.2], Y.Plot.y[Which.2], pch = 16, col = 'green')

points(Y.Plot.x[Which.1], Y.Plot.y[Which.1], pch = 16, cex = 1.1)















