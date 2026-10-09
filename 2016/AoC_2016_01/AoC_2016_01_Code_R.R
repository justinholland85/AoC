######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2016
Day           <- 1

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)

Name.Data.1   <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)

# Data.0 is not useful here - it is just several messy examples

Data.1  <-  readLines(Name.Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data       <-  Data.1


F.Part.1   <-  function(Data){

  
  Split      <-  trimws(strsplit(Data,",")[[1]])
  
  Turn       <-  substr(Split, 1, 1)
  Steps      <-  as.numeric(substr(Split, 2, nchar(Split)))
  
  Turn       <-  ifelse(Turn == "L", -1, 1)  
  Direction  <-  cumsum(Turn) %% 4
  
  # resisted urge to get vectors by math
  VectorMap  <-  do.call(rbind,list("0" = c("x" = 0, "y" = 1),
                                    "1" = c("x" = 1, "y" = 0),
                                    "2" = c("x" = 0, "y" = -1),
                                    "3" = c("x" = -1, "y" = 0)))
  
  
  
  Vectors    <- VectorMap[Direction + 1, ] 
  
  Move       <-  Vectors * Steps

  Position   <-  apply(Move, 2, cumsum)   
  
  BunnyAt    <-  Position[length(Split), ]
  
  Manhatten  <-  sum(abs(BunnyAt))
  
  return(Manhatten)
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################



# Data       <-  Data.1


F.Part.2   <-  function(Data){
  
  
  Split      <-  trimws(strsplit(Data,",")[[1]])
  
  Turn       <-  substr(Split, 1, 1)
  Steps      <-  as.numeric(substr(Split, 2, nchar(Split)))
  
  Turn       <-  ifelse(Turn == "L", -1, 1)  
  Direction  <-  cumsum(Turn) %% 4
  
  # resisted urge to get vectors by math
  VectorMap  <-  do.call(rbind,list("0" = c("x" = 0, "y" = 1),
                                    "1" = c("x" = 1, "y" = 0),
                                    "2" = c("x" = 0, "y" = -1),
                                    "3" = c("x" = -1, "y" = 0)))
  
  
  
  Vectors    <-  Lib.RowsToList(VectorMap[Direction + 1, ]) 
  
  Move       <-  do.call(rbind, c(list(c(0,0)), rep(Vectors , Steps)))
  
  Position   <-  data.frame(apply(Move, 2, cumsum))   
  
  Group      <-  Lib.GroupOn(Position$x, Position$y)
  Seq        <-  Lib.SeqOn(Group)
  
  BunnyRef   <-  which(Seq==2)[1]
  BunnyAt    <-  Position[BunnyRef, ]
  
  
  Manhatten  <-  sum(abs(BunnyAt))
  
  return(Manhatten)
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.1)
