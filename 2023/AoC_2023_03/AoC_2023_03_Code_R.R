######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# AoC Header 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"
Year          <- 2023
Day           <- 3

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

#====================================================================================================#
# Revision Note 20261112
#====================================================================================================#
# Original solution was one of my early ones, when I looked at it it just llooked aweful so just 
# completely rewrote. I could have done without regex of course but got help for that.


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
# Data   <-  Data.0
# Data   <-  Data.1

F.Part.1                    <-  function(Data){
  
  #----------------------------------------------------------------------------------------------------#
  # Info Org
  #----------------------------------------------------------------------------------------------------#
  
  
  Split     <-  do.call(rbind, strsplit(Data, ""))
  N.X       <-  ncol(Split)
  N.Y       <-  nrow(Split)
  

  Numbers   <-  1 * (array(dim = dim(Split), grepl("^[0-9]$", Split)))
  Symbols   <-  1 * (array(dim = dim(Split), grepl("^[-#$%&*/@+=]$", Split)))
   
  
  Adjacent  <-  0 * Symbols
  
  for(i in 1:N.Y){
  for(j in 1:N.X){
   

    Y0  <- max(i - 1, 1)
    Y1  <- min(i + 1, N.Y)
    X0  <- max(j - 1, 1)
    X1  <- min(j + 1, N.X)
    
    Adjacent[i, j]  <-  sum(Symbols[Y0:Y1, X0:X1]) > 0
    
  }}
  
  #----------------------------------------------------------------------------------------------------#
  # F.RowHandle
  #----------------------------------------------------------------------------------------------------#
  
  F.RowHandle     <-  function(i){
    
   
    X          <-  Numbers[i, ]  
    Start      <-  1 * (X == 1 & c(0, X[-N.X]) == 0) 
    Seq        <-  cumsum(Start) * X
    
    
    Value      <-  as.numeric(unlist(lapply(tapply(Split[i,X==1],    Seq[X==1], c), paste, collapse = "")))
    Keep       <-  as.numeric(unlist(lapply(tapply(Adjacent[i,X==1], Seq[X==1], c), paste, collapse = "")))
    
    return(Value[Keep > 0])
    
  }
  

  #----------------------------------------------------------------------------------------------------#
  # Loop
  #----------------------------------------------------------------------------------------------------#
  
  Values  <-  sapply(seq(1, N.Y), F.RowHandle)
  
  Sum     <- sum(unlist(Values))
  
  return(Sum)
  
  
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
# Data   <-  Data.0
# Data   <-  Data.1

F.Part.2                    <-  function(Data){
  
  #----------------------------------------------------------------------------------------------------#
  # Info Org
  #----------------------------------------------------------------------------------------------------#
  
  
  Split     <-  do.call(rbind, strsplit(Data, ""))
  N.X       <-  ncol(Split)
  N.Y       <-  nrow(Split)
  
  Numbers   <-  Split %in% as.character(seq(0, 10))
  
  Numbers   <-  1 * (array(dim = dim(Split), grepl("^[0-9]$", Split)))
  Symbols   <-  1 * (array(dim = dim(Split), grepl("^[-#$%&*/@+=]$", Split)))
  Stars     <-  1 * (Split == "*")
  Stars_ID  <-  Lib.Flatwise(Stars, cumsum) * Stars



  
  #----------------------------------------------------------------------------------------------------#
  # F.RowHandle
  #----------------------------------------------------------------------------------------------------#
  
  F.RowHandle    <-  function(i){
    
    
    X          <-  Numbers[i, ]  
    Start      <-  1 * (X == 1 & c(0, X[-N.X]) == 0) 
    Seq        <-  (cumsum(Start) + i * 1000) * X
    
    Value      <-  as.numeric(unlist(lapply(tapply(Split[i,X==1],    Seq[X==1], c), paste, collapse = "")))
    ID         <-  Seq[Start == 1]
    
    ID.Map     <-  data.frame("ID"    = ID, 
                              "Value" = Value)                  
    
    Output     <-  list(ID.Map = ID.Map,    
                        ID     = Seq)
    
    return(Output)
    
  }
  
  
  
  #----------------------------------------------------------------------------------------------------#
  # Loop
  #----------------------------------------------------------------------------------------------------#
  
  PartNumbers     <-  lapply(as.list(seq(1, N.Y)), F.RowHandle)
  
  Part_ID         <-  do.call(rbind, lapply(PartNumbers, Lib.Take.One, "ID"))
  ID.Map          <-  do.call(rbind, lapply(PartNumbers, Lib.Take.One, "ID.Map"))
  
  Adjacent        <-  list()
  
  for(i in 1:N.Y){
  for(j in 1:N.X){
      
      if(Stars[i,j] == 1){
      Y0  <- max(i - 1, 1)
      Y1  <- min(i + 1, N.Y)
      X0  <- max(j - 1, 1)
      X1  <- min(j + 1, N.X)
      
      Star_ID             <-  Stars_ID[i,j]
      
      Adjacent[[Star_ID]] <-  setdiff(c(Part_ID[Y0:Y1, X0:X1]), 0)
      
      }
     
    }}
  
  Adjacent.Count   <-  unlist(lapply(Adjacent, length))
  
  Gears            <-  data.frame(do.call(rbind, Adjacent[which(Adjacent.Count == 2)]))
  names(Gears)     <-  c("ID.A", "ID.B")
  
  Gears$Val.A      <-  ID.Map$Value[match(Gears$ID.A, ID.Map$ID)]
  Gears$Val.B      <-  ID.Map$Value[match(Gears$ID.B, ID.Map$ID)]
  
  Gears$Prod       <-  Gears$Val.A * Gears$Val.B
  
  Sum              <- sum(Gears$Prod)
  
  return(Sum)
  
  
}

#====================================================================================================#
# Execution  
#====================================================================================================#

F.Part.2(Data.0)
F.Part.2(Data.1)
