source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


setwd("C:/Users/justi/Documents/Model/AdventOfCode/Day 1")



Input    <- data.frame(V1=c("1abc2",
               "pqr3stu8vwx",
               "a1b2c3d4e5f",
               "treb7uchet"), stringsAsFactors = FALSE)


Input   <-  read.csv("Input_1a.txt", header = FALSE)

Split     <-  strsplit(Input$V1, "")
AsNumeric <-  lapply(Split, as.numeric) 

Which     <-  lapply(lapply(lapply(AsNumeric, is.na ), '!'), which)
Clean     <-  mapply(Lib.Take.Which, AsNumeric, Which, SIMPLIFY = FALSE)

Length    <-  lapply(Clean, length)

First     <-  unlist(lapply(Clean, Lib.Take.Which, 1))
Last      <-  unlist(mapply(Lib.Take.Which, Clean, Length, SIMPLIFY = FALSE))

Value     <-  as.numeric(paste0(First, Last))
sum(as.numeric(paste0(First, Last)))


head(Input)
head(First)
head(Last)

head(Clean)



tail(Input)
tail(Value)

match("827", Input$V1)

Input[407, ]
Value[407]

sum(Value)




table(unlist(Split))



X  <-  Input$V1


for(i in 1:26){
 
   X  <-  gsub( pattern = letters[i], replacement = "", x =  X)
  
}

Nchar   <-  nchar(X)

First.b   <-  as.numeric(substr(x = X, 1, 1))
Last.b    <-  as.numeric(substr(x = X, Nchar, Nchar))

sum(as.numeric(paste0(First.b, Last.b)))


identidal()

identical(unlist(Length), Nchar)


which(First != First.b)
which(Last != Last.b)

which(Length != Nchar)

Which    <-  head(which(Length != Nchar))
Input[Which,]


Lib.NA.To.Val(AsNumeric[[1]], NULL)




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Part 2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################






Input    <- data.frame(V1=c("two1nine",
                            "eightwothree",
                            "abcone2threexyz",
                            "xtwone3four",
                            "4nineeightseven2",
                            "zoneight234",
                            "7pqrstsixteen"), stringsAsFactors = FALSE)

Input   <-  read.csv("Input_1a.txt", header = FALSE)

Numbers     <-  c("one",
                  "two",
                  "three",
                  "four",
                  "five",
                  "six",
                  "seven",
                  "eight",
                  "nine")


Sub        <-  paste0(Numbers, seq(1,9), Numbers)



X  <-  Input$V1


for(i in 1:9){
  
  X  <-  gsub( pattern = Numbers[i], replacement = Sub[i], x =  X)
  
}

for(i in 1:26){
  
  X  <-  gsub( pattern = letters[i], replacement = "", x =  X)
  
}

Nchar   <-  nchar(X)

First.b   <-  as.numeric(substr(x = X, 1, 1))
Last.b    <-  as.numeric(substr(x = X, Nchar, Nchar))

sum(as.numeric(paste0(First.b, Last.b)))




