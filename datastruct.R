library("xlsx")
library("tidyverse")

Mz <- read.xlsx("../../dados/Dados/Demographics.xlsx", 1, header = T)
Dz <- read.xlsx("../../dados/Dados/Demographics.xlsx", 2, header = T)
pairing <- read.xlsx("../../dados/Dados/List_twin_pairs.xlsx", 1, header = T)

