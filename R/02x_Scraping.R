#### Preamble ####
# Purpose: The purpose of this script is to download all of the coffees by Sey Coffees that have been archived on Sey.com
# Author: Thomas Rosenthal
# Email: t.rose.github@protonmail.com
# Last updated: 25 Feb 2021
# Prerequisites: see description file, several packages
# Misc: runs on github actions

#build directories
dir.create('R/inputs/data/Sey', recursive=TRUE)
dir.create('R/outputs/')

library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(data.table)
library(rvest)
library(stringr)
library(here)
library(xml2)
library(plyr)

#get rundate for file names
rundate = toString(sapply(date(now()), gsub, pattern = "-", replacement = "", fixed = TRUE))

#get and save data
raw_data <- read_html("https://www.seycoffee.com/collections/archived-coffees")
xml2::write_html(raw_data,paste0("R/inputs/data/Sey/SeyArchive_" ,rundate, ".html"))

#get node
slightly_cleaned_data <-
  raw_data %>%
  html_nodes("div [class='coffees_products_product_inner']") %>%
  html_nodes('a') %>%
  html_attr("href") #%>%

#URL from href
URLs_generation <- tibble(raw_text = slightly_cleaned_data)

#add prefix: "https://www.seycoffee.com"
URLs_generation <- URLs_generation %>% mutate(URL = paste0("https://www.seycoffee.com",raw_text))

#generate URLs
URLs <- URLs_generation$URL

#remove any URLs that resolve to 404/similar
checkURLs <- lapply(URLs, function(u) {
  tryCatch({
    html_obj <- read_html(u)
    draft_table <- html_nodes(html_obj,'table')
    cik <- substr(u,start = 41,stop = 47)
    draft1 <- html_table(draft_table,fill = TRUE)
    final <- u
  }, error = function(x) NULL)
})

#removes the URLs that failed
URLs <-  unlist(checkURLs)

#create table for each coffee from URLs, constructed in three parts and merged
coffee_table <- data.frame()
coffee_base <- data.frame()
tnotes_table <- data.frame()
cnt <- 0

for(i in URLs){
  coffee_row <- read_html(i)
  cnt <- cnt+1

  #grab four sections to build dataframe
  #name, producer, country
  coffee_basics <-
    coffee_row %>%
    html_nodes("div [class='coffee_title coffee_titleMobile']") %>%
    html_text()

  #column names for the details
  coffee_columns <-
    coffee_row %>%
    html_nodes("div [class='coffee_technicalDetails_detail_title']") %>%
    html_text()

  #variety, altitiude, etc
  coffee_details <-
    coffee_row %>%
    html_nodes("div [class='coffee_technicalDetails_detail_description']") %>%
    html_text()

  #tasting note line
  coffee_blurb <-
    coffee_row %>%
    html_nodes("div [class='coffee_keyInfo_shortBlurb'] p") %>%
    html_text()

  #save the htmls
  #we wont save sey's html due to the google api key, but using rcron we'll keep them locally
  #write_html(coffee_row,paste0("inputs/data/Sey/Coffees/Sey_", cnt, "_" ,rundate, ".html"))

  #create matrix and columns x details dataframe
  mat <- matrix(coffee_details,, length(coffee_details), byrow = TRUE)
  coffee <- as.data.frame(mat, stringsAsFactors = FALSE)
  colnames(coffee) = coffee_columns

  #create j-index to join on
  coffee <- coffee %>%
    rename_with(str_to_upper) %>%  #fixes title case vs lower case issue for colnames between filenames
    rename_at(vars(contains('S')), ~ str_remove(.,  "S")) %>% #handles VARIETY vs VARIETIES
    mutate(j = cnt) %>%
    mutate(URL = i) #add URL from where it was drawn

  #rbind
  coffee_table <- rbind(coffee_table, data.frame(coffee))

  #tibble coffee_basics, seperate row into relevent columns
  coffee_basics <- tibble(coffee_basics) %>%
    separate(coffee_basics, into = c("1","CoffeeName","Producer","Var - Proc","Country"), sep = "\\n",  remove = TRUE) %>%
    select(-1) %>%
    separate('Var - Proc', into = c("Var","Process"), sep = " - ",  remove = TRUE) %>%
    select(-Var) %>%
    mutate(j = cnt)

  #rbind
  coffee_base <- rbind(coffee_base, coffee_basics)

  #merge on j's
  washed_coffee <- merge(coffee_base, coffee_table, j=j)

  #Add "Sey Coffee" to all rows
  washed_coffee$Roaster <- "Sey Coffee"

  # coffee blurb, tasting note decompositon
  coffee_blurb <- coffee_blurb[1]

  # Selects the last full sentence, usually "In this cup..."
  coffee_notes <- sub(".*(\\.|\\?|\\!) ", "", trimws(coffee_blurb))
  # regex looks to select words before commas and before and, does not always succeed
  a <- str_split(coffee_notes, '!(?<=and).*|(,| and)')
  #adjust for first word before first comma, usually is a tasting note
  a[[1]][1] <- word(a[[1]][1],-1)

  #data frame our list, remove NAs
  aa <- plyr::ldply (a, data.frame)
  aa[aa==""] <- NA
  aa <- na.omit(aa)

  colnames(aa) <- "TastingNotes"
  #remove punctuation from end
  aa <- aa %>%
    mutate_all(funs(str_replace(., "[.]", "")))

  #convert rows to columns, merge columsn into "TastingNotes" with a comma delim
  aa <- aa %>% spread("TastingNotes","TastingNotes")
  aa <- aa %>% unite("TastingNotes", colnames(aa), sep= ', ')

  #create j-index to join on
  aa <- aa %>%
    mutate(j = cnt)

  #generate tnotes_table
  for (i in nrow(aa)) {
    tnotes_table <- rbind(tnotes_table, data.frame(aa))
  }

  #merge
  final <- merge(washed_coffee, tnotes_table, j=j)
}

#reorder Roaster to start, URL to end
finalx <- final %>%  select(-j) %>%
  relocate(Roaster, .before = CoffeeName) %>%
  relocate(URL, .after = TastingNotes)

#write to csv
path_out = here::here('R/outputs/')
file_name = paste0(path_out, 'SeyArchive_',rundate,".csv")
write_csv(final,file_name)
