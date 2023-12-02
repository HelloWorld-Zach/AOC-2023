

library(tidyverse)

games <- readLines("2023_d2.txt")


count <- 0 

for(game in games){
  
  red <- max(as.double(unlist(str_extract_all(game, '\\d+(?= red)'))))
  green <- max(as.double(unlist(str_extract_all(game, '\\d+(?= green)'))))
  blue <- max(as.double(unlist(str_extract_all(game, '\\d+(?= blue)'))))
  
  if(red <= 12 & green <= 13 & blue <= 14) count <- count + which(games == game)
  
}


count  

#----------------------------------------------Part 2--------------------------------------------

part2 <- 0 

for(game in games){
  
  red <- max(as.double(unlist(str_extract_all(game, '\\d+(?= red)'))))
  green <- max(as.double(unlist(str_extract_all(game, '\\d+(?= green)'))))
  blue <- max(as.double(unlist(str_extract_all(game, '\\d+(?= blue)'))))
  
  part2 <- part2 + red * green * blue
  
}


part2



