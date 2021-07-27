"
 Download the files using the links below, extract them from their zip archives,
 and place them into a directory of your choosing.
 
 http://www.linguistics.ucla.edu/faciliti/sales/upsid.zip
 http://web.phonetik.uni-frankfurt.de/upsid_matrix.txt.zip

"
rm(list = ls())

library(readr)
library(ggplot2)
library(dplyr)
library(ISOcodes)
library(tidyverse)
library(countrycode)
library(ISOcodes)
library(shiny)
library(reshape2)
library(scales)
library(stringr)
library(choroplethrAdmin1)
library(choroplethr)
library(choroplethrMaps)


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
Remove df since we do not need it anymore and it is a large list
"
rm(df)

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
to 'lang'

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
Future Enhancement

"
country <- ISO_3166_1  # Country code
#lang_code <- ISO_639_2 # Language code

url <- "https://www.ethnologue.com/codes/LanguageIndex.tab"
download.file(url, "LanguageIndex.txt")
lang_index <- read_delim(file = "LanguageIndex.txt",
                         delim = "\t")

rm(url)

"
LanugageIndex.txt contains characters from ISO_8859-2; this encoding
contains letters that are not in the English alphabet. Convert these chars
to ASCII to get English alphabet equivalents.

"
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

countrymap <- data.frame()
maps <- compiledmap
maps$CountryID[is.na(maps$CountryID)] <- "NAM"  # NA value is actually for country Namibia
country$Alpha_2[country$Alpha_2=="NA"] <- "NAM" # NA value is actually for country Namibia
for (a in 1:nrow(maps)){
  for (b in 1:nrow(country)){
    if(maps[a,2] == toupper(country[b,1])){
      countrymap <- rbind(countrymap, country[b,])
    }
  }
  
}
compiled1 <- cbind(compiled1, countrymap$Alpha_2, countrymap$Name)
compiled1 <- mutate(compiled1, Continent = countrycode(countrymap$Name, 'country.name', 'continent'))

compiled2 <- data.frame()
for (c in 1: nrow(compiled1)){
  for(d in 1:nrow(lang)){
    if(compiled1[c,1] == lang[d,1]){
      compiled2 <- rbind(compiled2, lang[d, ])
    }
  }
}

compiled3 <- compiled2
compiled3$Family <-  paste0(compiled1$Continent)
names(compiled3)[2] <- paste("Continent")
names(compiled3)[3] <- paste("Country")

compiled3$Country <- paste0(compiled1$`countrymap$Name`)

# Save compiled3 off for use in shiny
write_csv(compiled3, "compiled3.csv")

# Summary Tables and Plots

# Make a copy of compiled3 into lang1
lang1 <- compiled3

# Summary table by Name
compiled3 <- group_by(compiled3, Name)
summ <- summarize(compiled3, value = n())

# Summary table by Family
compiled3 <- group_by(compiled3, Country)
summ1 <- summarize(compiled3, value = n())

# Summary table by Continent
compiled3 <- group_by(compiled3, Continent)
summ2 <- summarize(compiled3, value = n())

# Summary table by CountryID
compiled3 <- group_by(compiled3, Country)
summ3 <- summarize(compiled3, value = n())

"
Create data frame to plot top 10 Countries by number of language

"

# Create new data frame "Top_10" of top 10 countries by number of language
Top_10 <- top_n(summ3, 10) %>%
  arrange(-value)

# Plot graph of top 10 countries by number of language

g1 <- qplot(Country, value, data = Top_10, geom = "point", 
             fill = I("blue"), color = I("blue"), alpha = I(0.7))
g1 <- g1 + ggtitle("Top 10 Countries by Number of Languages")
g1 <- g1 + xlab("Country")
g1 <- g1 + ylab("Count")
g1
ggsave(filename = "By_Top_10_Country.png", plot = g1, 
       width = 13, height = 6, dpi = 600)

# Bar plot of count of language by continent
g2 <- qplot(Continent, data = compiled3, geom = "bar", 
            fill = I("lightblue"), 
            color = I("blue"), alpha = I(2))
g2 <- g2 + ggtitle("Count of Languages by Continent")
g2 <- g2 + xlab("Continent")
g2 <- g2 + ylab("Count")
g2
ggsave(filename = "By_Num_Lang_Continent.png", plot = g2, 
       width = 6, height = 4, dpi = 600)


# Bar plot of syllable `Syl: p` in continents: bilabial plosive
g3 <- qplot(`Syl: p`, data = compiled3, geom = "bar", facets = . ~ Continent,
            fill = Continent) +
  ggtitle("Bilabial Plosive (p) Occurance in Languages, by Continent")
g3
ggsave(filename = "By_Syllable_Continent.png", plot = g3, 
       width = 6, height = 4, dpi = 600)

# Rename Name with value before summarying in table
compiled1 <- compiled1 %>%
  rename(value = Name)

# Summary table by the country whole name, save new data frame in summ4
# rename `country$Name` and "value" to region and value respectively
# and change region to lower case
compiled3 <- group_by(compiled3, Country)
compiled3$Country <- tolower(compiled3$Country)

"
Using error of regions that could not be mapped, manually change the names of
the countries in summ4 to make them compatible with country choropleth
"
compiled3$Country[compiled3$Country == "moldova, republic of"] <- "moldova"
compiled3$Country[compiled3$Country == "north macedonia"] <- "macedonia"
compiled3$Country[compiled3$Country == "united states"] <- "united states of america"
compiled3$Country[compiled3$Country == "russian federation"] <- "russia"
compiled3$Country[compiled3$Country == "syrian arab republic"] <- "syria"
compiled3$Country[compiled3$Country == "timor-leste"] <- "east timor"
compiled3$Country[compiled3$Country == "taiwan, province of china"] <- "taiwan"
compiled3$Country[compiled3$Country == "tanzania, united republic of"] <- "united republic of tanzania"
compiled3$Country[compiled3$Country == "venezuela, bolivarian republic of"] <- "venezuela"
compiled3$Country[compiled3$Country == "viet nam"] <- "vietnam"
compiled3$Country[compiled3$Country == "bolivia, plurinational state of"] <- "bolivia"
compiled3$Country[compiled3$Country == "brunei darussalam"] <- "brunei"
compiled3$Country[compiled3$Country == "congo"] <- "republic of congo"
compiled3$Country[compiled3$Country == "congo, the democratic republic of the"] <- "democratic republic of the congo"
compiled3$Country[compiled3$Country == "czechia"] <- "czech republic"
compiled3$Country[compiled3$Country == "guinea-bissau"] <- "guinea bissau"
compiled3$Country[compiled3$Country == "iran, islamic republic of"] <- "iran"
compiled3$Country[compiled3$Country == "lao people's democratic republic"] <- "laos"
compiled3$Country[compiled3$Country == "northern cyprus"] <- "cyprus"

# create a summary table to plot 
# a choropleth map of language by country
summ4 <- summarize(compiled3, value = n())
summ4 <- summ4 %>%
  rename(region = Country, value = value) %>%
  select(region, value)

# Plot country map based on language by country grouped in summ4
# saved graph in object g4
g4 <- country_choropleth(summ4, title = "Language/Country", 
                         num_colors = 4, zoom = NULL)
g4 <- g4 + scale_fill_discrete(na.value = "grey")
g4
ggsave(filename = "By_Lang_Country.png", plot = g4, 
       width = 6, height = 4, dpi = 600)

# Bar plot of syllable `Syl: n|` in continents
g5 <- qplot(`Syl: n|`, data = compiled3, geom = "bar", facets = . ~ Continent,
            fill = Continent) +
  ggtitle("Nasalized Voiced Dental Affricated Click ('n|') Occurance in Languages, by Continent")
g5
ggsave(filename = "By_Syllable_Click_Continent.png", plot = g5,
       width = 6, height = 4, dpi = 600)

"
Create a df for use in our interactive c-map. Base this off of compiled3.
Intent is to visualize which languages are in which countries, so remove
syllables.
"
compiled4 <- compiled3[, 1:3]

"
Since we want to display which regions have which languages, we can filter on
a language and the regions it appears in will be dispalyed. Since a region
either does or does not include a language, we can create a new column named 
'value' that has a value of 1. All other countries will have a value of 0/NA.
Then, we can dynamically select which language we want to view via shiny.
(Filtering will take place in our shiny code, since that's what we want to be
dynamic)
"
compiled4 <- rename(compiled4, region = Country)
compiled4$value <- 1
write_csv(compiled4, "compiled4.csv")

"Cleanup, so our workspace is not as cluttered."
rm(a, b, c, d, i, j, x, aCol, aRow, done, max_cols, compiled1, compiled2,
   Name_update, compiledmap, maps, lang_index, lang_index1)

print("Sourcing complete.")
