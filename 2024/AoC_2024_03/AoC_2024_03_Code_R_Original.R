
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/2024/Day_03")
options(scipen = 99)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Name.Example      <-  "Example_Day_03.txt"
Name.Example.pt2  <-  "Example_Day_03_pt2.txt"
Name.Input        <-  "Input_Day_03.txt"


Example           <-  readChar(Name.Example,     file.info(Name.Example)$size)
Example.pt2       <-  readChar(Name.Example.pt2, file.info(Name.Example.pt2)$size)
Input             <-  readChar(Name.Input,       file.info(Name.Input)$size)



Data.Example      <-  Example
Data.Example.pt2  <-  Example.pt2
Data.Input        <-  Input
   


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 1 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

#====================================================================================================#
# F.Part.1
#====================================================================================================#

F.Part.1                               <-  function(Data){
  
Pos                                    <-  as.numeric(gregexpr(pattern ='mul\\(', text = Data)[[1]])


N.Pos                                  <-  length(Pos)
N.Char                                 <-  nchar(Data)

Split.1                                <-   character(0)


for(i in 1:N.Pos){
  
  A                                    <- Pos[i]
  B                                    <- ifelse(i==N.Pos, N.Char, Pos[i+1]) 
  
  Split.1[[i]]                         <-  substr(Data, A, B)
  
  
}








Split.1                                <- gsub(pattern = "mul\\(", replacement = "", Split.1)

Pos.1.Comma                            <-  unlist(lapply(lapply(gregexpr(pattern =',',Split.1), 
                                                                Lib.Take.One, 1), as.numeric))
Pos.1.Comma[which(Pos.1.Comma == -1)]  <-  NA

options(warn = -1)
Num.1                                  <-  as.numeric(substr(Split.1, 1, Pos.1.Comma - 1 ))
options(warn = 0)

 
Split.1.Nchar                          <-  nchar(Split.1)
Split.2                                <-  substr(Split.1, Pos.1.Comma + 1, Split.1.Nchar)  

Pos.2.Bracket                          <-  unlist(lapply(lapply(gregexpr(pattern ='\\)',Split.2),
                                                                Lib.Take.One, 1), as.numeric))

options(warn = -1)
Num.2                                  <-  as.numeric(substr(Split.2, 1, Pos.2.Bracket - 1 ))
options(warn = 0)


return(sum(Lib.NA.To.Zero(Num.1) * (Lib.NA.To.Zero(Num.2))))



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
# F.Part.2
#====================================================================================================#

F.Part.2                               <-  function(Data){
  
  Pos                                    <-  as.numeric(gregexpr(pattern ='mul\\(', text = Data)[[1]])
  
  N.Pos                                  <-  length(Pos)
  N.Char                                 <-  nchar(Data)
  

  #-- part 2 on off switch
  
  Pos.Dont                               <-  as.numeric(gregexpr(pattern ="don't\\(\\)", text = Data)[[1]])
  Pos.Do                                 <-  as.numeric(gregexpr(pattern ="do\\(\\)", text = Data)[[1]])
  
  Switch.Pos                             <-  c(Pos.Dont, Pos.Do)
  Switch.Dir                             <-  c(rep(0, length(Pos.Dont)), 
                                               rep(1, length(Pos.Do)))
  
  Order                                  <-  order(Switch.Pos)
   
  Switch.Pos                             <-  Switch.Pos[Order]
  Switch.Dir                             <-  Switch.Dir[Order]
  
  Pos.OnOff                              <-  rep(1, N.Char)
  
  for(i in 1:length(Switch.Pos)){
    
    Pos.OnOff[Switch.Pos[[i]]:N.Char]    <-  Switch.Dir[[i]]
    
    
  }
  
  OnOff                                  <-  Pos.OnOff[Pos]

  ###
  
  Split.1                                <-   character(0)
  
  
  for(i in 1:N.Pos){
    
    A                                    <- Pos[i]
    B                                    <- ifelse(i==N.Pos, N.Char, Pos[i+1]) 
    
    Split.1[[i]]                         <-  substr(Data, A, B)
    
    
  }
  

  
  
  Split.1                                <- gsub(pattern = "mul\\(", replacement = "", Split.1)
  
  Pos.1.Comma                            <-  unlist(lapply(lapply(gregexpr(pattern =',',Split.1), 
                                                                  Lib.Take.One, 1), as.numeric))
  Pos.1.Comma[which(Pos.1.Comma == -1)]  <-  NA
  
  options(warn = -1)
  Num.1                                  <-  as.numeric(substr(Split.1, 1, Pos.1.Comma - 1 ))
  options(warn = 0)
  
  
  Split.1.Nchar                          <-  nchar(Split.1)
  Split.2                                <-  substr(Split.1, Pos.1.Comma + 1, Split.1.Nchar)  
  
  Pos.2.Bracket                          <-  unlist(lapply(lapply(gregexpr(pattern ='\\)',Split.2),
                                                                  Lib.Take.One, 1), as.numeric))
  
  options(warn = -1)
  Num.2                                  <-  as.numeric(substr(Split.2, 1, Pos.2.Bracket - 1 ))
  options(warn = 0)
  
  
  return(sum(Lib.NA.To.Zero(Num.1) * (Lib.NA.To.Zero(Num.2)) * OnOff))
  
  
  
}



#====================================================================================================#
# Execute
#====================================================================================================#

Data   <-  Data.Example.pt2

F.Part.2(Data.Example.pt2)
F.Part.2(Data.Input)












