######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2021
Day           <- 2

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)


Name.Data.0      <-  paste0(Ref, "_Input_0.txt")
Name.Data.1      <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)
Data.0      <-  readLines(Name.Data.0)
Data.1      <-  readLines(Name.Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data        <-  Data.0
# Data        <-  Data.1

F.Part.1          <-  function(Data){
  
  Split              <-  strsplit(Data, " ")
  
  Direction          <-  unlist(lapply(Split, Lib.Take.One, 1))
  Move               <-  as.numeric(unlist(lapply(Split, Lib.Take.One, 2)))
  
  Vector.Map         <-  list("up"      = c(0, 1), 
                              "forward" = c(1, 0), 
                              "down"    = c(0, -1))
  
  Vectors            <-  data.frame(do.call(rbind, Vector.Map[Direction]))
  names(Vectors)     <-  c("X", "Y")
  rownames(Vectors)  <-  NULL
  
  Delta              <-  Vectors * Move
  
  EndPosition        <-  colSums(Delta)
  
  EndProduct         <-  prod(EndPosition) 
  
  return(abs(EndProduct))
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#
F.Part.1(Data.0)

F.Part.1(Data.1)


D   <- Direction[1]
X <- Move[1]

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data        <-  Data.0
# Data        <-  Data.1

F.Part.2          <-  function(Data){
  
  Split              <-  strsplit(Data, " ")
  
  Direction          <-  unlist(lapply(Split, Lib.Take.One, 1))
  Move               <-  as.numeric(unlist(lapply(Split, Lib.Take.One, 2)))
  
  P                  <-  list("aim"  = 0, 
                              "horz" = 0,
                              "depth" = 0
                              )

  
  F.Move            <- function(P, D, X){
    
    if(D == "up"){P$aim        <- P$aim + X}
    if(D == "down"){P$aim      <- P$aim - X}
    if(D == "forward"){P$horz <- P$horz + X; P$depth <- P$depth - abs(P$aim) * X}
    
    return(P)
    
  }
  
  
  for(i in 1:length(Direction)){
    
    P   <-  F.Mov(P, Direction[i], Move[i])
    
  }
  
  
  EndProduct         <-  prod(abs(P$depth) * P$horz) 
  
  
  return(abs(EndProduct))
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#
F.Part.2(Data.0)

F.Part.2(Data.1)







