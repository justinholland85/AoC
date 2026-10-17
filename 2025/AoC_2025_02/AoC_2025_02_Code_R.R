######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2025
Day           <- 2

Ref           <-  paste0("AoC_", Year, "_", Lib.LeadingZeros(Day, 2))
Dir           <-  paste0(Root,Year,"/", Ref)

Name.Data.0   <-  paste0(Ref, "_Input_0.txt")
Name.Data.1   <-  paste0(Ref, "_Input_1.txt")

#====================================================================================================#
# Data  
#====================================================================================================#

setwd(Dir)

# Data.0 is not useful here - it is just several messy examples
Data.0  <-  readLines(Name.Data.0)
Data.1  <-  readLines(Name.Data.1)


#====================================================================================================#
# Modification notes  
#====================================================================================================#

# When I originally did this 'on the day' in 2025 I did it up brute force by computing every numbeer 
#  in the range and testing the validity. It worked within allowable time and so I accepted the result
#  as it met my acceptable standard at that time. 

# In 2026, while experimenting with AI assisted, got ChatGPT to compare the main conceptual different 
#   between my approach and the copilot approach because I remembered I was not really happy with this
#   result. 

# The key idea was that Copilot used the reversed brute force - calculate the possible invalids first and 
#  then test against the ranges. I had not considered the possilbilty that this would be more efficient
#  at the time. 

# That idea was sufficient, the tweaks rest on that idea, I did not consider or explore any other 
#  tactics. 


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Data        <-  Data.0
# Data        <-  Data.1

F.Part.1    <-  function(Data){
  
  #--------------------------------------------------------------------------------------------------#
  # Data setup
  #--------------------------------------------------------------------------------------------------#
  
  Split         <-  unlist(strsplit(Data, split = ","))
  Split         <-  lapply(strsplit(Split, split = "-"), as.numeric)
  
  Pairs         <-  data.frame(do.call(rbind, Split))
  names(Pairs)  <-  c("From", "To")
  
  Pairs         <-  Pairs[order(Pairs$From), ]
  N.Pairs       <-  nrow(Pairs)
  

  
  #--------------------------------------------------------------------------------------------------#
  # Obtain list of all possible invalids
  #--------------------------------------------------------------------------------------------------#
  
  MaxDigs       <-  nchar(max(Pairs))
  
  Digits        <-  seq(0,9)
  
  Invalids      <-  list()
  
  Counter       <-  0
  
  for(Digs   in 2:MaxDigs){
  for(Factor in 1:floor(Digs / 2)){

  
  if((Digs / Factor)  == 2 ){
  
  Counter             <-  Counter + 1
    
  Complement          <-  Digs / Factor
  
  Grid                <-  expand.grid(rep(list(Digits), Factor))
  ConCat              <-  apply(Grid, 1, paste, collapse = "")
  Repeat              <-  do.call(paste0, rep(list(ConCat), Complement))
  Numbers             <-  sort(unique(as.numeric(Repeat)))
  Invalids[[Counter]] <-  Numbers[which(nchar(Numbers) == Digs)]
  
  }}}
  
  Invalids            <-  sort(unique(unlist(Invalids)))
  
  #--------------------------------------------------------------------------------------------------#
  # Test Invalids against Ranges
  #--------------------------------------------------------------------------------------------------#
  
  RangeMarker         <-  rep(FALSE, length(Invalids))
  
  for(i in 1:N.Pairs){
    
    RangeMarker  <-  RangeMarker | (Pairs$From[[i]] <= Invalids & Invalids <= Pairs$To[[i]])
    
  }
  
  
  Sum.RangeInvalids    <-  sum(Invalids * RangeMarker)
  
  return(Sum.RangeInvalids)
  
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


Data        <-  Data.0
Data        <-  Data.1

F.Part.2    <-  function(Data){
  
  #--------------------------------------------------------------------------------------------------#
  # Data setup
  #--------------------------------------------------------------------------------------------------#
  
  Split         <-  unlist(strsplit(Data, split = ","))
  Split         <-  lapply(strsplit(Split, split = "-"), as.numeric)
  
  Pairs         <-  data.frame(do.call(rbind, Split))
  names(Pairs)  <-  c("From", "To")
  
  Pairs         <-  Pairs[order(Pairs$From), ]
  N.Pairs       <-  nrow(Pairs)
  
  
  
  #--------------------------------------------------------------------------------------------------#
  # Obtain list of all possible invalids
  #--------------------------------------------------------------------------------------------------#
  
  MaxDigs       <-  nchar(max(Pairs))
  
  Digits        <-  seq(0,9)
  
  Invalids      <-  list()
  
  Counter       <-  0
  
  for(Digs   in 2:MaxDigs){
    for(Factor in 1:floor(Digs / 2)){
      
      
      if((Digs / Factor)  %% 1 == 0 ){
        
        Counter             <-  Counter + 1
        
        Complement          <-  Digs / Factor
        
        Grid                <-  expand.grid(rep(list(Digits), Factor))
        ConCat              <-  apply(Grid, 1, paste, collapse = "")
        Repeat              <-  do.call(paste0, rep(list(ConCat), Complement))
        Numbers             <-  sort(unique(as.numeric(Repeat)))
        Invalids[[Counter]] <-  Numbers[which(nchar(Numbers) == Digs)]
        
      }}}
  
  Invalids            <-  sort(unique(unlist(Invalids)))
  
  #--------------------------------------------------------------------------------------------------#
  # Test Invalids against Ranges
  #--------------------------------------------------------------------------------------------------#
  
  RangeMarker         <-  rep(FALSE, length(Invalids))
  
  for(i in 1:N.Pairs){
    
    RangeMarker  <-  RangeMarker | (Pairs$From[[i]] <= Invalids & Invalids <= Pairs$To[[i]])
    
  }
  
  
  Sum.RangeInvalids    <-  sum(Invalids * RangeMarker)
  
  return(Sum.RangeInvalids)
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)








