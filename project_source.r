"
 Download the files using the links below, extract them from their zip archives,
 and place them into a directory of your choosing.
 
 http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip
 http://web.phonetik.uni-frankfurt.de/upsid_matrix.txt.zip
 
"
library(readr)
library(ggplot2)
library(dplyr)
library(ISOcodes)
library(tidyverse)


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
Add continents to each language. Start with continents that are in the Family
Name.

"
compiled$Continent[grepl("Khoisan|Niger|Chadic|Saharan|Afro",
                         compiled$Family)] <- "Africa"

compiled$Continent[grepl("Sino|Dravidian|Burushaski|Chukchi|Siberian|Ket",
                         compiled$Family)] <- "Asia"

compiled$Continent[grepl("Caucasian|Ural|Indo-European|Basque",
                         compiled$Family)] <- "Europe"

compiled$Continent[grepl("(N\\.|Northern|North) Americ(an|a)|Caucasian|Eskimo|Na-Dene",
                         compiled$Family)] <- "North America"

compiled$Continent[grepl("Australian|Austro|Papuan|Polynesian",
                         compiled$Family)] <- "Oceania"

compiled$Continent[grepl("(S\\.|Southern|South) Americ(an|a)",
                         compiled$Family)] <- "South America"


"
Add syllables to each language
http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip

Read 'UPSID_MATRIX.txt' into object 'skylab'. Disable quoting since some
syllables use double-quotes to denote symbols. Delimit with tabs.

Some syllables are denoted as numbers; cast all cols as chars to minimize number
of NAs. This is necessary for converting syllables to factor levels later, when
syllables are just one long string.

"

skylab <- read_delim(file = "UPSID_MATRIX.txt",
                     delim = "\t",
                     quote = "",
                     col_types = cols(.default = col_character()),
                     col_names = FALSE,
                     trim_ws = TRUE,
                     na = c("NA", " "))


"
Rename first column to 'Name' to make data more readable

"
names(skylab)[1] <- c("Name")

# "
# Create factor that contains every syllable as a level. Do this by iterating thru
# all syllable lists (i.e. for each language), but by column; we search by column
# to take advantage of the syllables being sorted in their own way (e.g. notice
# how syllables denoted by 'p' and 'b' always come first, second respectively).
# Once we hit the syllable we need, we append to our list, then move on without
# having to search the other lists.
# "
# 
# all_syls <- c()
# for (aCol in 2:ncol(skylab)){                                             # Iterate thru every column / syllable (start at column 2 to exclude name)
#   done <- (aCol * 100)/ ncol(skylab)                                      # Create a "progress" message based on number of columns traversed since this loop can take a few seconds...
#   if (done %% 5 == 0) message(
#     sprintf("%s%% of the way through a long loop.", round(done)))
#   for (aRow in 1:nrow(skylab)){                                           # Iterate thru each row / language
#     if (skylab[[aRow, aCol]] == "" || is.na(skylab[[aRow,aCol]])) next    # If this language does not have this syllable, move on to the next language
#     all_syls <- c(all_syls, skylab[[aRow, aCol]])                         # If the code is at this point, we know the current index has a syllable. Append that syllable to the vector
#     names(skylab)[aCol] <- skylab[[aRow, aCol]]
#     break                                                                 # Move on to the next syllable
#   }
# }

"
Rename each column to whatever syllable it contains. Then, change the column to
type logical, denoting if that language contains that syllable or not.

"

for (aCol in 2:ncol(skylab)){
  for (aRow in 1:nrow(skylab)){
    if (skylab[[aRow, aCol]] == "" || is.na(skylab[[aRow,aCol]])) next
    names(skylab)[aCol] <- paste("Syl:", skylab[[aRow, aCol]])
    break
  }
  skylab[aCol] <- grepl("\\S", skylab[[aCol]])
  
  done <- (aCol * 100)/ ncol(skylab)
  if (done %% 5 == 0) message(
    sprintf("%s%% of the way through a long loop.", round(done)))
}

# skylab_alt <- skylab[,1]
# 
# for (aRow in 2:nrow(skylab)){
#   skylab_alt[aRow,]$Syllables <- skylab[aRow,2:ncol(skylab)]
# }

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
#skylab1 <- unite(skylab, Syllables, 2:max_cols, sep = ",")

"
Use inner_join to intersect 'skylab1' with 'compiled' and save new dataframe
to 'lang_sylabls'

"
lang <- inner_join(compiled, skylab,
                   by = NULL, copy = FALSE,
                   suffix = c(".compiled",".skylab1"))

"
Drop 'Family' column to keep only 'Names' and 'Syllables' columns, then check
the dataframe for any missing values

"

sum(is.na(lang))

"
Map family/classification to continent using country and language ISO Codes
https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
https://en.wikipedia.org/wiki/ISO_639-2

"
country <- ISO_3166_1  # Country code
lang_code <- ISO_639_2 # Language code

url <- "https://www.ethnologue.com/codes/LanguageIndex.tab"
download.file(url, "LanguageIndex.txt")
lang_index <- read_delim(file = "LanguageIndex.txt",
                         delim = "\t")

"
LanugageIndex.txt contains characters from ISO_8859-2; this encoding
contains letters that are not in the English alphabet. Convert these chars
to ASCII to get English alphabet equivalents.

"

#lang_index$Name <- iconv(lang_index$Name, to = 'ASCII//TRANSLIT') 
lang_index1 <- iconv(lang_index$Name, to = 'ASCII//TRANSLIT') 
Name_update <- matrix(lang_index1)
lang_index <- mutate(lang_index, Name_update) # adding the new column
lang_index = as.data.frame(lang_index)
compiledmap <- data.frame()
compiled1 <- data.frame()

for(i in 1:nrow(compiled)) {
  for(j in 1:nrow(lang_index)){
    if(compiled[i,1] == as.character(toupper(lang_index[j,4])))
    {
      compiledmap <- rbind(compiledmap, lang_index [j, ]) # save the mapped data frame
      compiled1 <- rbind(compiled1, compiled [i, ]) # to find which languages were mapped
    }
  }
  done <- i * 100 / nrow(compiled)
  message(sprintf("%.3f%% of the way through a very long loop.", done))
}

