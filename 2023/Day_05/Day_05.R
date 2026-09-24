
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_05")





Input   <-  read.csv("Example_Day_05.txt", header = FALSE, sep = "^")$V1


Input   <-  read.csv("Input_Day_05.txt", header = FALSE, sep = "^")$V1


Text.Seeds       <-  "seeds: "

Seeds    <-  gsub(pattern = Text.Seeds, replacement = "", grep(Text.Seeds,  Input, value= TRUE)) 
Seeds    <-  as.numeric(strsplit(Seeds, " ")[[1]])



Order   <-  c("seed",
              "soil",
              "fertilizer",
              "water",
              "light",
              "temperature",
              "humidity",
              "location")

N.Order  <-  length(Order)

Order.Text  <- character(0)
Order.Line  <- numeric(0)  


for(i in 1:(N.Order - 1)){
  
  Order.Text[[i]]  <-  paste0(Order[[i]], "-to-", Order[[i+1]])
  Order.Line[[i]]  <-  grep(Order.Text[[i]], Input)
  
}

Order.Line         <-  c(Order.Line, length(Input) + 1)

Data               <-  list()

for(i in 1:(N.Order - 1)){
  
  i.A   <-  Order.Line[[i]]   + 1
  i.B   <-  Order.Line[[i+1]] - 1
  
  Data[[i]]  <-  Input[i.A:i.B]
  
}

names(Data)  <-  Order.Text


F.Do.Call   <-  function(args, what){
  
  return(do.call(what, args))
  
}


Data   <-  lapply(Data, strsplit, " ")
Data   <-  lapply(Data, lapply, as.numeric)
Data   <-  lapply(Data, F.Do.Call, rbind)



Data   <-  lapply(Data,  Lib.GiveNames.Cols, Names = c("Destination", "Origin", "Range"))
Data   <-  lapply(Data, data.frame)



Origin  <-  lapply(Data, Lib.Take.One, "Origin")
Order   <-  lapply(Origin, order)

Data    <-  mapply(Lib.Take.Which.Rows, Data, Order, SIMPLIFY = FALSE)

#--- reordering of variables unnecessary but works better for my brain
Data   <-  lapply(Data, Lib.Take.Which.Cols, c(2,1,3))


Base    <-  Seeds
Map     <-  Data$`seed-to-soil`


F.Mapify  <-  function(Base, Map){
   
  RangeMap                        <-  Lib.RangeMap(Base, c(Map$Origin, Inf))
  RangeMap[which(RangeMap == 0)]  <-  NA
  
  b.Origin      <-  Map$Origin[RangeMap]
  b.Range       <-  Map$Range[RangeMap]
  b.Destination <-  Map$Destination[RangeMap]
  
  b.Delta   <-  Base - b.Origin
  
  Which.Out  <-  which(b.Delta >= b.Range)
  
  b.Delta[Which.Out]  <-  NA 
  
  b.Result            <- b.Destination + b.Delta 
  
  Which.IsNa          <-  which(is.na(b.Result))
  
  
  b.Result[Which.IsNa]  <-  Base[Which.IsNa]
  
  return(b.Result)
  
}



Base    <-  Seeds

for(i in 1:(N.Order - 1)){
  
  Base  <-  F.Mapify(Base,  Data[[i]])
  
  
}

min(Base)


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

#====================================================================================================#
# Fails due to vector size computation
#====================================================================================================#

Seeds    <-  gsub(pattern = Text.Seeds, replacement = "", grep(Text.Seeds,  Input, value= TRUE)) 
Seeds    <-  as.numeric(strsplit(Seeds, " ")[[1]])


Seed.Pairs   <-  data.frame(t(array(dim= c(2, length(Seeds) / 2), Seeds)))
names(Seed.Pairs)  <-  c("start", "length")


Seed.Pairs$End   <-  Seed.Pairs$start + Seed.Pairs$length - 1

Seq    <-  mapply(seq, as.list(Seed.Pairs$start), as.list(Seed.Pairs$End), SIMPLIFY = FALSE)



Seeds  <-  sort(unlist(Seq))



for(i in 1:(N.Order - 1)){
  
  Base  <-  F.Mapify(Base,  Data[[i]])
  
  
}

min(Base)


#====================================================================================================#
# Rejig for lots of seeds
#====================================================================================================#


for(i in 1:(N.Order-1)){
  
    Data[[i]]$Origin.Max        <-  Data[[i]]$Origin  + Data[[i]]$Range - 1 
    
    Data[[i]]$Destination.Max   <-  Data[[i]]$Destination + Data[[i]]$Range - 1 
  
  
  
}


Seeds              <-  gsub(pattern = Text.Seeds, replacement = "", grep(Text.Seeds,  Input, value= TRUE)) 
Seeds              <-  as.numeric(strsplit(Seeds, " ")[[1]])


Seed.Pairs         <-  data.frame(t(array(dim= c(2, length(Seeds) / 2), Seeds)))
names(Seed.Pairs)  <-  c("Start", "Length")

Seed.Pairs$End     <-  Seed.Pairs$Start + Seed.Pairs$Length - 1  


F.Mapify.Ranges  <-  function(Base, Map){
  

  
  Result.Start   <-  numeric(0)
  Result.End     <-  numeric(0)
  
  Base.Start     <-  Base$Start
  Base.End       <-  Base$End
  
  n  <-  1
  
  N.Map          <-  nrow(Map)
  
  while(n <= N){
    
    N                               <-  length(Base.Start)
    
    n.Base.Start    <-  Base.Start[n]
    n.Base.End      <-  Base.End[n]
    
    RangeMap                        <-  Lib.RangeMap(n.Base.Start, c(Map$Origin, Inf))
    if(RangeMap == N.Map){b.Next  <-  Inf} else {b.Next <- Map$Origin.Max[RangeMap + 1]}
    
    IntoNext           <-   n.Base.End >= b.Next 
    
    if(IntoNext){Base.Start[[N  + 1]]  <-  b.Next
                 Base.End[[N  + 1]]    <-  n.Base.End
                 n.Base.End            <-  b.Next - 1}
    
    
    if(RangeMap == 0){
      
      Result.Start[[n]]  <-  n.Base.Start
      Result.End[[n]]    <-  n.Base.End
      
      
    }
    
    if(RangeMap > 0){
    
    b.Origin                        <-  Map$Origin[RangeMap]
    b.Range                         <-  Map$Range[RangeMap]
    b.Destination                   <-  Map$Destination[RangeMap]
    b.Origin.Max                    <-  Map$Origin.Max[RangeMap]
 
    N                               <-  length(Base.Start)
    
    if(n.Base.Start > b.Origin.Max){      
      
    Result.Start[[n]]               <-  n.Base.Start
    Result.End[[n]]                 <-  n.Base.End
      
      
    }
    
    if(n.Base.Start <= b.Origin.Max & n.Base.End > b.Origin.Max){  
    
      Base.Start[[N  + 1]]  <-  b.Origin.Max + 1
      Base.End[[N  + 1]]    <-  n.Base.End
      n.Base.End            <-  b.Origin.Max
    
    }
    
  if(n.Base.Start <= b.Origin.Max){
    
    Start.Delta        <-     n.Base.Start - b.Origin
    End.Delta          <-     n.Base.End   - b.Origin
    
    Result.Start[[n]]  <-     b.Destination +  Start.Delta
    Result.End[[n]]    <-     b.Destination +  End.Delta
    
  }
    
  }
    
    n  <-  n + 1 
    N                               <-  length(Base.Start)
    
  }
  
  Results    <-  data.frame(Start = Result.Start,
                            End = Result.End)
  
  
  return(Results)
  
  
  }



Base  <-  Seed.Pairs

  
for(i in 1:(N.Order - 1)){
    
    Base  <-  F.Mapify.Ranges(Base,  Data[[i]])
  
}

min(Base$Start)








