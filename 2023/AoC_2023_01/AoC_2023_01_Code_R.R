######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2023
Day           <- 1

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)

Name.Data.0a  <-  paste0(Ref, "_Input_0_p1.txt")
Name.Data.0b  <-  paste0(Ref, "_Input_0_p2.txt")
Name.Data.1   <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)

# Data.0 is not useful here - it is just several messy examples
Data.0a  <-  readLines(Name.Data.0a)
Data.0b  <-  readLines(Name.Data.0b)
Data.1   <-  readLines(Name.Data.1)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.0a
# Data   <-  Data.1


# NOTE_TouchUp_20261012: 
#   Modified for modern pattern and consistency, mostly maintained original conceptualisation with 
#   a few coding tweaks

F.Part.1   <-  function(Data){
 

Numbers       <-  Data

for(i in 1:26){
Numbers       <-  gsub(letters[i], replacement = "", x = Numbers)
}

Numbers       <-  strsplit(Numbers, "")

Count         <-  lapply(Numbers, length)

First         <-  unlist(lapply(Numbers, Lib.Take.Which, 1))
Last          <-  mapply(Lib.Take.Which, Numbers, Count)

CalibVal      <-  as.numeric(paste0(First, Last))  
CalibVal.Sum  <-  sum(CalibVal)

return(CalibVal.Sum)

}



#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.1(Data.0a)
F.Part.1(Data.1)




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data   <-  Data.0b
# Data   <-  Data.1

F.Part.2   <-  function(Data){


# NOTE_TouchUp_20261012: 
#   Modified for modern pattern and consistency, have changed conceptualisation a little
#   with significant tidy


  X           <-  Data

  Numbers     <-  c("one",
                    "two",
                    "three",
                    "four",
                    "five",
                    "six",
                    "seven",
                    "eight",
                    "nine")
  

  Nchar        <-  nchar(Numbers)
  
  Sub          <-  paste0(substr(Numbers, 1, 1),  seq(1,9), substr(Numbers, Nchar, Nchar)) 

for(i in 1:9){
  
   X           <-  gsub( pattern = Numbers[i], replacement = Sub[i], x =  X)
  
}

  CalibVal.Sum  <- F.Part.1(X)
  

return(CalibVal.Sum)

}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0b)
F.Part.2(Data.1)
