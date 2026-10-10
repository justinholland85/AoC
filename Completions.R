source("C:/Users/justi/OneDrive/Documents/GitHub/RLibrary/Start.R")

Root          <- "C:/Users/justi/OneDrive/Documents/GitHub/AoC/"

Year.Days     <-  c("2015" = 25, 
                    "2016" = 25,
                    "2017" = 25,
                    "2018" = 25,
                    "2019" = 25,
                    "2020" = 25,
                    "2021" = 25,
                    "2022" = 25,
                    "2023" = 25,
                    "2024" = 25,
                    "2025" = 12)

Grid  <-  list()

for(i in 1:length(Year.Days)){
  
  Grid[[i]]  <-  expand.grid(Year = Lib.NumericNames(Year.Days)[[i]], 
                             Day  = seq(1, Year.Days[[i]]), 
                             Part = c(1, 2))
  
  
  
}


Grid  <-  do.call(rbind, Grid)

Grid   <-  Grid[do.call(order, Grid), ]

setwd(Root)

fwrite(Grid, "Completions.csv")