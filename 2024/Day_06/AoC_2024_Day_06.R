
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/2024/Day_06")
options(scipen = 99)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Name.Example      <-  "Example_Day_06.txt"
Name.Input        <-  "Input_Day_06.txt"


Example           <-  read.csv(Name.Example, header = FALSE, sep = "")$V1
Input             <-  read.csv(Name.Input,   header = FALSE, sep = "")$V1


#====================================================================================================#
# F.Data.Sep  
#====================================================================================================#

Raw      <-  Example

F.Data.Sep    <-  function(Raw){
  
  Split       <- lapply(lapply(Raw, strsplit, ""), Lib.Take.One, 1)
  

  
  return(do.call(rbind, Split))
  
  
}

#====================================================================================================#
# Data Execute  
#====================================================================================================#


Data.Example      <-  F.Data.Sep(Example)
Data.Input        <-  F.Data.Sep(Input)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

#====================================================================================================#
# F.Numerify
#====================================================================================================#

Data   <-  Data.Example

F.Numerify  <-  function(Data){
  
  Map       <-  array(dim = dim(Data))
  
  Map[which(Data == ".")]   <-  0
  Map[which(Data == "#")]   <-  1
  Map[which(Data == "^")]   <-  2
  
  return(Map)
  
  
}


#====================================================================================================#
# F.Rotate
#====================================================================================================#



F.Rotate   <-  function(Dir.N){
  
  
  Seq.X    <-  c(0, 1, 0, -1)
  Seq.Y    <-  c(-1,0, 1, 0)
  
  Dir.N    <-  ifelse(Dir.N == 4, 1, Dir.N + 1)
  Dir.X    <-  Seq.X[Dir.N]
  Dir.Y    <-  Seq.Y[Dir.N]
  
  Output  <-  list(Dir.N   = Dir.N,
                   Dir.X   = Dir.X, 
                   Dir.Y   = Dir.Y)
  
  return(Output)
  
  
  
  
}






#====================================================================================================#
# F.Part.1 
#====================================================================================================#



F.Part.1    <-  function(Data){
  
Map        <-  F.Numerify(Data)
  
Nx         <-  ncol(Map)
Ny         <-  nrow(Map)


Coords     <-  Lib.Grid.Coords(Nx - 1, Ny - 1, 1, Nx, 1, Ny)
Coords$Y   <-  rev(Coords$Y)


Grid       <-  Lib.Gridify(Coords)

Grid.X     <-  Grid$Mat.X
Grid.Y     <-  Grid$Mat.Y


Visit      <-  0 * Map


Pos.X      <-  Grid.X[which(Map==2)]
Pos.Y      <-  Grid.Y[which(Map==2)]

Visit[Pos.Y, Pos.X]  <-  1


Dir.N      <-  1
Dir.X      <-  0
Dir.Y      <-  -1

i          <-  1

repeat{


Check.X    <-  Pos.X + Dir.X
Check.Y    <-  Pos.Y + Dir.Y

Exit       <-  Check.X < 1 | Check.X > Nx | Check.Y < 1 | Check.Y > Ny

if(Exit){break}

Blocked   <-  Map[Check.Y, Check.X] == 1

if(Blocked){
  
  Rotate    <-  F.Rotate(Dir.N)  
  
  Dir.N     <-   Rotate$Dir.N
  Dir.X     <-   Rotate$Dir.X
  Dir.Y     <-   Rotate$Dir.Y
  
  
  
} else {
  
  i         <-  i + 1
  
  
  Pos.X     <-  Check.X
  Pos.Y     <-  Check.Y
  
  Visit[Check.Y, Check.X]  <-  i
  
  
}

}

Output    <-  list("Count" = sum(Visit > 0), 
                   "Visit" = Visit)


return(Output)

}


#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.1(Data.Example)
F.Part.1(Data.Input)$Count


Data    <-  Data.Input




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################







Map        <-  F.Numerify(Data)

Nx         <-  ncol(Map)
Ny         <-  nrow(Map)


Coords     <-  Lib.Grid.Coords(Nx - 1, Ny - 1, 1, Nx, 1, Ny)
Coords$Y   <-  rev(Coords$Y)


Grid       <-  Lib.Gridify(Coords)

Grid.X     <-  Grid$Mat.X
Grid.Y     <-  Grid$Mat.Y


DF         <-  data.frame(X   = c(Grid.X), 
                          Y   = c(Grid.Y), 
                          Map = c(Map))



DF         <-  DF[which(DF$Map == 1), ]



