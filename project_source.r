# Load packages
library(readr)
library(ggplot2)
library(dplyr)

"
 Download the files here, extract them from their zip archives,
 and place them into a directory of your choosing.
 
 http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip
 http://web.phonetik.uni-frankfurt.de/upsid_matrix.txt.zip
 
"

# Set working directory to wherever you have the data stored.
data_dir <- getwd()

# Prepare the individual language files for extracting their data
regex_for_files <- "LG[[:digit:]]+.INF"
language_files <- list.files(path = data_dir,
                             recursive = TRUE,
                             pattern = regex_for_files,
                             full.names = TRUE)
"
Every file is formatted similarly. We are only interested in name
and language classification, so prepare R to extract these fields into a
df. Ideally, Name will be a string, and classification will be a factor.
It would be more user-friendly to rename 'classification' to 'family'
"

"
Start by getting one file to work. Data is delimited by COLON (:), get
it into a DF. It will look wonky, but we will clean it afterwards.
"
df <- read.delim("LG4101.INF", sep = ":",
                 header = FALSE, strip.white = TRUE) %>%
  t() %>%
  as.data.frame()

"
Name columns now that data is transposed
"
df <- rename(df, Name = V1) %>%
  rename(Family = V4) %>%
  select(Name, Family)

df <- subset(df, df$Name != "Language name")

"
Apply this to multiple files now.
"

prepareFiles <- function(file){
  aFile <- read.delim(file, sep = ":",
             header = FALSE, strip.white = TRUE) %>%
    t() %>%
    as.data.frame()
}

"
  This function will populate data in form of a list, and we can't
  rename a list. We will have to index into the list for every file
  accessed, i.e. use a loop.
"

# Read in all files; they are a list, but we can work with this
df <- sapply(language_files, prepareFiles)

"
 For every dataframe in df, select the V1 and V4 COLS, rename the
 V1, V4 COLS to Name and Family, then remove the first ROW that
 doesn't have any useful info (which is there as an artifact of how
 I read the files in, there is likely a better way.)
 
 After these operations, append that row to a new frame; pre
"
compiled <- as.data.frame(NULL)

for (x in 1:length(df)){
  
  df[[x]] <- select(df[[x]], V1, V4) %>%
    rename(Name = V1) %>%
    rename(Family = V4)

  df[[x]] <- subset(df[[x]], df[[x]]$Name != 'Language name')
  
  compiled <- rbind(compiled, df[[x]])
  
}

"
TODO: Add syllables to each language
http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip

"

# # Source data is a txt file, so use read_delim with some added options.
# # Note that data is tab-delimited.
# lang <- read_delim(file = "UPSID_MATRIX.txt",
#                    delim = "\t",
#                    na = c("", "NA"))

"
TODO: Map family/classification to continent
https://en.wikipedia.org/wiki/List_of_language_families#Language_families_(non-sign)
"