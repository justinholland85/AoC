
source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day_07")



Input    <-  fread("Example_Day_07.txt", header = FALSE, sep = "^")$V1



Input    <-  read.csv("Input_Day_07.txt", header = FALSE, sep = "^")$V1

CardOrder    <-  rev(c("A", "K", "Q", "J", "T", seq(9,2)))

Split    <-  strsplit(Input, " ")

Bid      <-  as.numeric(unlist(lapply(Split, Lib.Take.One, 2)))
Hand     <-  unlist(lapply(Split, Lib.Take.One, 1))

N.Hands   <-  length(Input)

F.TableTable   <- function(X){return(table(table(X)))}


  
  
Split   <-  strsplit(Hand, "")
  
Table   <-  Lib.MapListToSet(lapply(Split, F.TableTable), seq(1,5))
Table   <-  Lib.NA.To.Zero(do.call(rbind, Table))
  
  
Type    <-  (Table[,1] == 5)                   * 1  + 
            (Table[,2] == 1 & Table[,3] == 0)  * 2  +
            (Table[,2] == 2)                   * 3  + 
            (Table[,3] == 1 & Table[,2] == 0)  * 4  +
            (Table[,3] == 1 & Table[,2] == 1)  * 5  +
            (Table[,4] == 1)                   * 6  +  
            (Table[,5] == 1)                   * 7 
  


Split   <-  do.call(rbind, Split)
Split   <-  array(dim = dim(Split), match(c(Split), CardOrder))


Order.Cards  <-  do.call(order, Lib.ColsToList(Split) ) 
Rank.Cards   <-  match(seq(1, N.Hands), Order.Cards)

Hand.Power   <-  Type +  Rank.Cards / (N.Hands + 1) 

Rank.Hand    <-  Lib.Rank(Hand.Power, FALSE)        

sum(Rank.Hand * Bid)



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################
CardOrder    <-  rev(c("A", "K", "Q", "T", seq(9,2), "J"))



Split   <-  strsplit(Hand, "")

Table   <-  Lib.MapListToSet(lapply(Split, F.TableTable), seq(1,5))
Table   <-  Lib.NA.To.Zero(do.call(rbind, Table))

Split   <-  do.call(rbind, Split)
Split   <-  array(dim = dim(Split), match(c(Split), CardOrder))

Jokers  <-  rowSums(Split == match("J", CardOrder))

Dummy   <-  t(array(dim = c(5, N.Hands), seq(1,5)))

Jokers.Mat   <-  Dummy == Jokers

Table        <-  Table - Jokers.Mat

Max          <-  apply(Dummy * (Table > 0), 1, max)

Max.Mat      <-  (Dummy == Max)

Table        <-  Table - Max.Mat

Add          <-  Max + Jokers

Add.Mat      <-   (Dummy == Add)

Table        <-  Table + Add.Mat

Type         <-  (Table[,1] == 5)                   * 1  + 
                 (Table[,2] == 1 & Table[,3] == 0)  * 2  +
                 (Table[,2] == 2)                   * 3  + 
                 (Table[,3] == 1 & Table[,2] == 0)  * 4  +
                 (Table[,3] == 1 & Table[,2] == 1)  * 5  +
                 (Table[,4] == 1)                   * 6  +  
                 (Table[,5] == 1)                   * 7 



Order.Cards  <-  do.call(order, Lib.ColsToList(Split) ) 
Rank.Cards   <-  match(seq(1, N.Hands), Order.Cards)

Hand.Power   <-  Type +  Rank.Cards / (N.Hands + 1) 

Rank.Hand    <-  Lib.Rank(Hand.Power, FALSE)        

sum(Rank.Hand * Bid)








