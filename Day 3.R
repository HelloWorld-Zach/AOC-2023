library(tidyverse)

input <- readLines("2023_d3.txt")

len <- length(input)
width <- nchar(input[1])

input1 <- array('.', dim = c(len + 2, width + 2))

for(i in seq_along(input)) input1[(1+i), 2:141] <- str_split(input[i], '')[[1]]

input2 <- c()

for(i in 1:142) input2[i] <- str_flatten(input1[i, ])
  
get_neighbors <- function(row, start, end){
  
   neighbors <- str_split(str_flatten(c(substr(input2[row], start - 1, start - 1), 
                                        substr(input2[row], end + 1, end + 1), 
                                        substr(input2[row + 1], start - 1, end + 1),
                                        substr(input2[row - 1], start - 1, end + 1))), '')[[1]]
   
   ifelse(sum(setdiff(unique(as.vector(input1)), c(0:9, '.')) %in% neighbors) != 0, as.integer(substr(input2[row], start, end)), 0)
  
}
  
part1 <- 0

for(i in 2:141){
  starts <- as.double(str_locate_all(input2[i], '\\d++')[[1]][,1])
  ends <- as.double(str_locate_all(input2[i], '\\d++')[[1]][,2]) 
  
  for(j in seq_along(starts)) part1 <- part1 + get_neighbors(i, starts[j], ends[j])
}

part1

#----------------------------------------------------------------------- Part 2 --------------------------------------------------------------------------------


get_gears <- function(row, start, end){
  neighbors <- str_split(str_flatten(c(substr(input2[row], start - 1, start - 1), 
                                       substr(input2[row], end + 1, end + 1), 
                                       substr(input2[row + 1], start - 1, end + 1),
                                       substr(input2[row - 1], start - 1, end + 1))), '')[[1]]
  
  positions <- c(paste0('(', row, ',', start - 1, ')'), 
                 paste0('(', row, ',', end + 1, ')'),
                 paste0('(', row + 1, ',', (start - 1):(end + 1), ')'), 
                 paste0('(', row - 1, ',', (start - 1):(end + 1), ')'))

  ifelse(sum('*' %in% neighbors) != 0 , positions[which(neighbors == '*')], 0)
}


positions <- c()
gear_spots <- c()

for(i in 2:141){
  starts <- as.double(str_locate_all(input2[i], '\\d++')[[1]][,1])
  ends <- as.double(str_locate_all(input2[i], '\\d++')[[1]][,2])
  
  for(j in seq_along(starts)){
    
    gear_spots <- c(gear_spots, get_gears(i, starts[j], ends[j]))
    positions <- c(positions, paste0(i, ',', starts[j], ',', ends[j]))
    
  }
  
}

part2 <- 0 
seen <- c()
for(i in seq_along(gear_spots)){
  
  if(gear_spots[i] == "0" | length(which(gear_spots == gear_spots[i])) != 2 | sum(which(gear_spots == gear_spots[i]) %in% seen) != 0) next 
  else {
    locations <- which(gear_spots == gear_spots[i])
    parsed1 <- as.double(str_extract_all(positions[locations[1]], '\\d++')[[1]])
    parsed2 <- as.double(str_extract_all(positions[locations[2]], '\\d++')[[1]])
    part2 <- part2 + as.double(substr(input2[parsed1[1]], parsed1[2], parsed1[3])) * as.double(substr(input2[parsed2[1]], parsed2[2], parsed2[3]))
    seen <- c(seen, locations)
  }
    
  
}

part2
