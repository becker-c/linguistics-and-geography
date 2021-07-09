"
 Download the files using the links below, extract them from their zip archives,
 and place them into a directory of your choosing.
 
 http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip
 http://web.phonetik.uni-frankfurt.de/upsid_matrix.txt.zip
 
"
library(readr)
library(ggplot2)
library(dplyr)

"
Set working directory to wherever you have the data stored.
Prepare the individual language files for extracting their data; we can take
advantage of the fact that these files follow LGXXXX.INF (where X = some number)

"
data_dir <- getwd()

regex_for_files <- "LG[[:digit:]]+.INF"
language_files <- list.files(path = data_dir,
                             recursive = TRUE,
                             pattern = regex_for_files,
                             full.names = TRUE)

"
The following (commented) code is for preparing one file, and is included
to show our thought process, and make later code easier to understand.
"
# "
# Every file is formatted similarly. We are only interested in name
# and language classification, so prepare R to extract these fields into a
# df. Ideally, Name will be a string, and classification will be a factor.
# It would be more user-friendly to rename 'classification' to 'family'
# "
# 
# "
# Start by getting one file to work. Data is delimited by COLON (:), get
# it into a DF. It will look wonky, but we will clean it afterwards.
# I used LG4101.INF as my test file, so you can add that into your root
# directory if you wish.
# "
# 
#df <- read_delim("LG4101.INF", sep = ":",
#                 header = FALSE, strip.white = TRUE) %>%
#  t() %>%
#  as.data.frame()
# 
# "
# Name columns now that data is transposed
# "
#df <- rename(df, Name = V1) %>%
#  rename(Family = V4) %>%
#  select(Name, Family)
# 
#df <- subset(df, df$Name != "Language name")
# 
# "
# Apply this to multiple files now.
# "

prepareFiles <- function(file){
  aFile <- read.delim(file,
                      sep = ":",
                      header = FALSE,
                      strip.white = TRUE) %>%
    t() %>%
    as.data.frame()
}

"
  Read in all files; they are a list of files, but we can work with this.
"

df <- sapply(language_files, prepareFiles)

"
 We can't rename a list. We will have to index into the list for every file
 accessed, i.e. use a loop.

 For every dataframe in df, select the V1 and V4 COLS, rename the
 V1, V4 COLS to Name and Family, then remove the first ROW that
 doesn't have any useful info.
 
 After these operations, append that row to the existing data frame.
 
"
compiled <- as.data.frame(NULL)

for (x in 1:length(df)){
  
  df[[x]] <- select(df[[x]], V1, V4) %>%
    rename(Name = V1) %>%
    rename(Family = V4)

  df[[x]] <- subset(df[[x]], df[[x]]$Name != 'Language name')
  
  compiled <- rbind(compiled, df[[x]])
  
}

compiled <- arrange(compiled, Name)
compiled <- subset(compiled, compiled$Name != '\'Language name')

"
Set df to NULL since we do not need it anymore and it is a large list
"
df <- NULL

"
Add syllables to each language
http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip

First, we want to intersect the langs that have syllable data.
Do this before adding the syllable data!

Read 'UPSID_MATRIX.txt' into object 'skylab' to create a new data frame.

"

skylab <- read_delim(file = "UPSID_MATRIX.txt",
                     delim = "\t",
                     quote = "",
                     col_names = FALSE,
                     trim_ws = TRUE,
                     na = c("NA", " "))

"
Some syllables are denoted as numbers; cast all cols as chars so that NAs
remain as NULL, rather than NA. This is necessary for converting syllables to
factors later.

"

mutate(skylab, across(everything(), as.character))

skylab <- mutate_all(skylab, as.character)

"
Rename first column to 'Name' to make data more readable

"
names(skylab)[1] <- c("Name")

"
Need tidyr to use unite function in line code below, which is part of
install.packages('tidyverse'); install and and load tidyr

"
# install.packages("tidyverse")
library(tidyr)

"
Collapse columns 2 to max into one column named Syllables and save new
dataframe to skylab1. We know the language name is stored in column 1 so start
on column 2.

"
max_cols <- ncol(skylab)
skylab1 <- unite(skylab, Syllables, 2:max_cols, sep = ",")

"
Use inner_join to intersect 'skylab1' with 'compiled' and save new dataframe
to 'lang_sylabls'

"
lang <- inner_join(compiled, skylab1,
                   by = NULL, copy = FALSE,
                   suffix = c(".compiled",".skylab1"))

"
Drop 'Family' column to keep only 'Names' and 'Syllables' columns, then check
the dataframe for any missing values

"
lang$Family <- NULL
sum(is.na(lang))


"
Austin
TODO: Map family/classification to continent
https://en.wikipedia.org/wiki/List_of_language_families#Language_families_(non-sign)
"
