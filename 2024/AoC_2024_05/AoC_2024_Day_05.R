
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/2024/Day_05")
options(scipen = 99)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Data 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
Name.Example      <-  "Example_Day_05.txt"
Name.Input        <-  "Input_Day_05.txt"


Example           <-  read.csv(Name.Example, header = FALSE, sep = "^")$V1
Input             <-  read.csv(Name.Input,   header = FALSE, sep = "^")$V1



#====================================================================================================#
# F.Data.Sep  
#====================================================================================================#

Raw      <-  Example

F.Data.Sep    <-  function(Raw){
  
  
 Rules   <-   Raw[grep(pattern = "\\|" , x = Raw)]
 Order   <-   Raw[grep(pattern = "," , x = Raw)]
  
 
 Rules   <-  do.call(rbind, lapply(strsplit(Rules, split = "\\|"), as.numeric))
 Order   <-  lapply(strsplit(Order, split = ","), as.numeric)
 
 
  
 Data    <- list("Rules" = Rules, 
                 "Order" = Order)
 
 return(Data)
  
  
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
# F.Part.1
#====================================================================================================#

F.Part.1            <-  function(Data){

Order               <-  Data$Order
Rules               <-  Data$Rules

N.Order             <-  length(Order)

Order.N             <-  unlist(lapply(Order, length))

Order.Page          <-  unlist(Order)
Order.ID            <-  rep(seq(1,length(Order)), Order.N)

Order.DT            <-  data.table(Page = Order.Page, 
                                   ID   = Order.ID)

Order.DT$Seq        <-  Lib.SeqOn(Order.DT$ID)
  
Product             <-  merge(Order.DT, Order.DT, by = "ID", allow.cartesian = TRUE)

Product             <-  Product[which(Product$Seq.x < Seq.y), ]   

Match.Rev           <-  Lib.Tuple.Match(data.frame(Product$Page.x, Product$Page.y), 
                                        data.frame(Rules[,2], Rules[,1]))


Product$BadMatch    <-  !is.na(Match.Rev)
  
Tapply              <-  tapply(Product$BadMatch , Product$ID, sum)   

ID.Bad              <-  Lib.NumericNames(Tapply[which(Tapply > 0)])


Order.Bad           <-  rep(0, N.Order)
Order.Bad[ID.Bad]   <-  1


Order.Middle        <-  mapply(Lib.Take.One, Order, ceiling(Order.N / 2))

return(sum(Order.Middle  * (1 - Order.Bad)))

}



#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.1(Data.Example)
F.Part.1(Data.Input)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

X      <-  Data.Input$Order[[1]]
Rules  <-  Data.Input$Rules

#====================================================================================================#
# F.ReOrder   
#====================================================================================================#


F.ReOrder   <-  function(X, Rules){
  
  
  x.Rules   <-  Rules[which(Rules[,1] %in% X & Rules[,2] %in% X), ]
  
  
  r.ID     <-  X
  
  Order    <-  list()
  
  i  <-  0
  
  repeat{
    
    i            <-  i + 1  
    
    i.ID         <-  r.ID[which(!(r.ID %in% x.Rules[,2]))]
    
    Order[[i]]   <-  i.ID
    
    x.Rules      <-  rbind(x.Rules[-which(x.Rules[,1] %in% i.ID), ])
    
    r.ID         <-  setdiff(r.ID, i.ID)
    
    if(length(r.ID) == 0){break}
    
    
  }
  
  
  Order  <-  unlist(Order)
  
  return(Order)
  
  
}



#====================================================================================================#
# F.Part.2
#====================================================================================================#

F.Part.2            <-  function(Data){
  
  Order               <-  Data$Order
  Rules               <-  Data$Rules
  
  N.Order             <-  length(Order)
  
  Order.N             <-  unlist(lapply(Order, length))
  
  Order.Page          <-  unlist(Order)
  Order.ID            <-  rep(seq(1,length(Order)), Order.N)
  
  Order.DT            <-  data.table(Page = Order.Page, 
                                     ID   = Order.ID)
  
  Order.DT$Seq        <-  Lib.SeqOn(Order.DT$ID)
  
  Product             <-  merge(Order.DT, Order.DT, by = "ID", allow.cartesian = TRUE)
  
  Product             <-  Product[which(Product$Seq.x < Seq.y), ]   
  
  Match.Rev           <-  Lib.Tuple.Match(data.frame(Product$Page.x, Product$Page.y), 
                                          data.frame(Rules[,2], Rules[,1]))
  
  
  Product$BadMatch    <-  !is.na(Match.Rev)
  
  Tapply              <-  tapply(Product$BadMatch , Product$ID, sum)   
  
  ID.Bad              <-  Lib.NumericNames(Tapply[which(Tapply > 0)])
  
  
  Orders.Bad          <-  Order[ID.Bad]
  

  ReOrdered           <-  lapply(Orders.Bad, F.ReOrder, Rules)
  
  
  Order.Middle        <-  mapply(Lib.Take.One, ReOrdered, ceiling(Order.N[ID.Bad] / 2))
  
  return(sum(Order.Middle))
  
}


#====================================================================================================#
# Execute
#====================================================================================================#

F.Part.2(Data.Example)
F.Part.2(Data.Input)

