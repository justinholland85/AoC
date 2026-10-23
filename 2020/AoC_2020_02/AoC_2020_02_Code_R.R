######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2020
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

F.Part.1    <-  function(Data){
  
  Split         <-  strsplit(Data, " ")
  
  Range         <-  unlist(lapply(Split, Lib.Take.One, 1))
  Range         <-  do.call(rbind,lapply(strsplit(Range, "-"), as.numeric))
  Range         <-  data.frame(Range)
  names(Range)  <-  c("A", "B")
  
  Letter        <-  gsub(":", "", unlist(lapply(Split, Lib.Take.One, 2)))
  
  Password      <-  strsplit(unlist(lapply(Split, Lib.Take.One, 3)), "")
  
  Count         <-  unlist(lapply(mapply('==', Password, as.list(Letter), SIMPLIFY = FALSE), sum))
  
  Valid         <-  Range$A <= Count & Count <= Range$B
  
  Valid.Count   <- sum(Valid)
  
  return(Valid.Count)
  
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

# Data        <-  Data.0
# Data        <-  Data.1

F.Part.2    <-  function(Data){
  
  Split             <-  strsplit(Data, " ")
  
  Positions         <-  unlist(lapply(Split, Lib.Take.One, 1))
  Positions         <-  do.call(rbind,lapply(strsplit(Positions, "-"), as.numeric))
  Positions         <-  data.frame(Positions)
  names(Positions)  <-  c("A", "B")
  
  Letter            <-  gsub(":", "", unlist(lapply(Split, Lib.Take.One, 2)))
   
  Password          <-  strsplit(unlist(lapply(Split, Lib.Take.One, 3)), "")
  
  Pos.A             <-  mapply(Lib.Take.A.From.B, as.list(Positions$A),  Password)
  Pos.B             <-  mapply(Lib.Take.A.From.B, as.list(Positions$B),  Password)

  Valid             <-  ((Pos.A == Letter) + (Pos.B == Letter)) == 1
  
  Valid.Count       <- sum(Valid)
  
  return(Valid.Count)
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#
F.Part.2(Data.0)

F.Part.2(Data.1)












