# Load packages
library(readr)
library(ggplot2)
library(dplyr)

# Source data is a txt file, so use read_delim with some added options.
# Note that data is tab-delimited.
lang <- read_delim(file = "UPSID_MATRIX.txt",
                   delim = "\t",
                   na = c("", "NA"))
