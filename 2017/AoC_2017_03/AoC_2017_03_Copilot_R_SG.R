######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC 2017 Day 03
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC_Copilot/"
Year          <- 2017
Day           <- 3

Ref           <- paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <- paste0(Root, Year, "/", Ref)

Name.Data.0   <- paste0(Ref, "_Input_0.txt")
Name.Data.1   <- paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data
#====================================================================================================#

setwd(Dir)

Data.0        <- readLines(Name.Data.0, warn = FALSE)
Data.1        <- readLines(Name.Data.1, warn = FALSE)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data       <-  Data.0
# Data       <-  Data.1

Data   <-  15

F.Part.1     <-  function(Data){

  Square      <-  as.numeric(Data)
  Ring        <-  ceiling((sqrt(Square) - 1) / 2)

  Distance    <-  numeric(length(Square))
  Outside     <-  Ring > 0

  # Each ring ends at an odd square; side midpoints are one radius from the origin.
  Side.Length <-  2 * Ring[Outside]
  Ring.Max    <-  (Side.Length + 1)^2
  Offset      <-  (Ring.Max - Square[Outside]) %% Side.Length

  Distance[Outside] <- Ring[Outside] + abs(Offset - Ring[Outside])

  return(Distance)
}

#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.1(Data.0)
F.Part.1(Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data       <-  Data.0
# Data       <-  Data.1

F.Part.2     <-  function(Data){

  sapply(as.numeric(Data), function(Target){

    VectorMap   <-  rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))
    Position    <-  c(0, 0)
    Squares     <-  data.frame(X = 0, Y = 0, Value = 1)
    Side.Length <-  1

    repeat{

      for(Direction in 1:4){

        # Spiral runs grow in pairs: 1, 1, 2, 2, 3, 3, ...
        Steps       <-  Side.Length + (Direction - 1) %/% 2

        for(i in seq_len(Steps)){

          Position    <-  Position + VectorMap[Direction, ]

          Adjacent    <-  abs(Squares$X - Position[1]) <= 1 &
                          abs(Squares$Y - Position[2]) <= 1

          Value       <-  sum(Squares$Value[Adjacent])

          if(Value > Target){return(Value)}

          Squares     <-  rbind(Squares, data.frame(X = Position[1],
                                                   Y = Position[2],
                                                   Value = Value))
        }
      }

      Side.Length <-  Side.Length + 2
    }
  })
}

#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)
