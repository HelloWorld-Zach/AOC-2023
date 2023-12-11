#-------------------------------------------- 2023 Day 11 ------------------------------------------------

library(tidyverse)
library(tictoc)
tic()
input <- readLines('2023_d11.txt')

universe <- array(0, dim = c(length(input), nchar(input[1])))

for(i in 1:nrow(universe)) universe[i,] <- str_split(input[i], '')[[1]]

empty_rows <- c()
empty_cols <- c()
for(i in 1:nrow(universe)) if(all(universe[i,] == '.')) empty_rows <- c(empty_rows, i)
for(i in 1:ncol(universe)) if(all(universe[,i] == '.')) empty_cols <- c(empty_cols , i)

path_length <- function(x1, x2, y1, y2) abs(x1 - x2) + abs(y1 - y2)

xs <- which(universe == '#', arr.ind = T)[,1]
ys <- which(universe == '#', arr.ind = T)[,2]

xs_p1 <- map_dbl(seq_along(xs), ~ xs[.] + sum(xs[.] > empty_rows))
ys_p1 <-  map_dbl(seq_along(ys), ~ ys[.] + sum(ys[.] > empty_cols))

crossing(left = paste0(xs_p1, ",", ys_p1), right = paste0(xs_p1, ",", ys_p1)) |>
  rowwise() |>
  mutate(distance = path_length(
    as.integer(str_extract_all(left, "\\d+")[[1]][1]),
    as.integer(str_extract_all(right, "\\d+")[[1]][1]),
    as.integer(str_extract_all(left, "\\d+")[[1]][2]),
    as.integer(str_extract_all(right, "\\d+")[[1]][2])
  ) / 2) |>
  pull(distance) |>
  sum()

#--------------------------------------------- Part 2 ----------------------------------------------------

xs_p2 <- map_dbl(seq_along(xs), ~ xs[.] + (10 ^ 6 - 1) * sum(xs[.] > empty_rows))
ys_p2 <-  map_dbl(seq_along(ys), ~ ys[.] + (10 ^ 6 - 1) * sum(ys[.] > empty_cols))

crossing(left = paste0(xs_p2, ",", ys_p2), right = paste0(xs_p2, ",", ys_p2)) |>
  rowwise() |>
  mutate(distance = path_length(
    as.integer(str_extract_all(left, "\\d+")[[1]][1]),
    as.integer(str_extract_all(right, "\\d+")[[1]][1]),
    as.integer(str_extract_all(left, "\\d+")[[1]][2]),
    as.integer(str_extract_all(right, "\\d+")[[1]][2])
  ) / 2) |>
  pull(distance) |>
  sum() 

toc() # 62 seconds 
