
#### Preamble ####
# Purpose: The purpose of this script is to download all of the coffees by Mongraph on EightOunceCoffee.ca
# Author: Thomas Rosenthal
# Email: mrpotatocode@ttrroossee.anonaddy.com
# Last updated: 31 Jan 2021
# Prerequisites: ?
# Misc:#### Set up workspace ####

#build directories
dir.create('R/inputs/data/EightOunce/Coffees', recursive=TRUE)
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

#get rundate for file names
rundate = toString(sapply(date(now()), gsub, pattern = "-", replacement = "", fixed = TRUE))

#get and save data
raw_data <- read_html("https://eightouncecoffee.ca/collections/monogram-coffee")
xml2::write_html(raw_data,paste0("R/inputs/data/EightOunce/Monogram_" ,rundate, ".html"))

#get roaster, its easier to have it seperate
Roaster <- tibble("https://eightouncecoffee.ca/collections/monogram-coffee") %>% 
  separate(1, into = c("1","Roaster"), sep = "collections/",  remove = TRUE) %>% 
  select(-1) %>% 
  sapply(gsub, pattern = "-", replacement = " ", fixed = TRUE) %>% 
  tools::toTitleCase()

#get node
slightly_cleaned_data <- 
  raw_data %>% 
  html_nodes("div [class='grid-product__title grid-product__title--heading']") %>% 
  html_text()

#tibble the character vector
our_data <- 
  tibble(raw_text = slightly_cleaned_data)


#get the coffee name
coffee_names <- our_data %>% 
  rename(Name = raw_text) %>% 
  select(Name) %>%
  mutate(lower_name =  str_to_lower(Name)) %>% 
  mutate(is_blend = if_else(lower_name %like% "blend",1,0),
         is_espresso = if_else(lower_name %like% "espresso",1,0),
         is_decaf = if_else(lower_name %like% "decaf",1,0),
         is_on_sale = if_else(lower_name %like% "sale",1,0)) %>% 
  filter(is_blend != 1, is_espresso != 1, is_decaf != 1, is_on_sale != 1) %>% 
  select(-is_blend, -is_espresso, -is_decaf, -is_blend, -is_on_sale)


#covert to hypenated words (e.g. 'the-library-specialty-coffee-ethiopia-chelelektu-washed')
#add prefix: 'https://eightouncecoffee.ca/collections/monogram-coffee/products/'
hypenated_data <- coffee_names %>% select(lower_name) %>% 
  apply( MARGIN = 2, FUN = trimws) %>% 
  sapply(gsub, pattern = " ", replacement = "-", fixed = TRUE) %>% 
  sapply(gsub, pattern = ",", replacement = "", fixed = TRUE) %>%
  sapply(gsub, pattern = "---", replacement = "-", fixed = TRUE) %>%
  as.data.frame() %>% 
  rename(Hypen_Name = 1) 

#paste these to be URLs
hypenated_data$URL <- paste0("https://eightouncecoffee.ca/collections/monogram-coffee/products/",hypenated_data[,1])

#build table
hypenated_data <- cbind(lower_name = rownames(hypenated_data), hypenated_data) 
#reset rowname as index
rownames(hypenated_data) <- 1:nrow(hypenated_data)

#generate URLs
URLs <- hypenated_data$URL

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

#create table for each coffee from URLs
coffee_table <- data.frame()
cnt <- 0

for(i in URLs){
  coffee_row <- read_html(i)
  cnt <- cnt+1
  
  #dig into body table
  slightly_cleaned_coffee <- 
    coffee_row %>% 
    html_nodes("div [id='content']") %>%
    html_nodes("ul") %>%
    html_nodes("li") %>%
    html_nodes("p") %>%
    html_text()
  
  #save the htmls
  xml2::write_html(coffee_row,paste0("R/inputs/data/EightOunce/Coffees/Monogram_", cnt, "_" ,rundate, ".html"))
  
  
  #tibble the character vector
  coffee <- 
    tibble(raw_text = slightly_cleaned_coffee)
  
  #seperate details row into relevent columns
  coffee <- coffee %>%
    pivot_wider(names_from = 1, values_from = raw_text) %>% 
    rename(table = 1) %>%   #, detail1 = 2, detail2 = 3, detail3 = 4, detail4 = 5 -- some investigation into how to make this dynamic?
    separate(table, into = c("1","Region","Variety","Processing","TastingNotes"), sep = "\\w+:",  remove = TRUE) %>% 
    select("Region","Variety","Processing","TastingNotes")  
  
  #removes any that we didn't run
  coffee$URL <- i
  
  #build table
  for (i in nrow(coffee)) {
    coffee_table <- rbind(coffee_table, data.frame(coffee))
  }
  #pause for 2.5 seconds to scrape friendly
  Sys.sleep(2.5)
}

#get country
coffee_table$Country <- sapply(strsplit(coffee_table$Region, ","), function(x) trimws(x[length(x)]))

#get names of coffees
coffee_names <- coffee_names %>% 
  separate(Name, into = c("Roaster","CoffeeName"), sep = case_when("Name" %like% "Monogram " ~ "Monogram ",
                                                                   TRUE ~ "Monogram "), remove = TRUE)
#merge which coffees had data so we can join on index
filtered_URLs <- merge(hypenated_data,URLs, by.x='URL', by.y ='y', all=FALSE)

#update roaster 
coffee_names$Roaster <-Roaster

#merge (documentation incomplete)
newCoffeeName <- coffee_names %>% select(CoffeeName) %>% 
  sapply(gsub, pattern = "-", replacement = "", fixed = TRUE) %>% 
  apply( MARGIN = 2, FUN = trimws) %>% 
  as.data.frame()

coffee_names$CoffeeName = newCoffeeName[,1]

filtered <- merge(filtered_URLs,coffee_names, by='lower_name')
coffee_names <- filtered %>% select(Roaster, CoffeeName, URL)

#final merge
final <- merge(coffee_names, coffee_table, by='URL', all=TRUE) %>% select(-URL)

#write to csv
path_out = here::here('R/outputs/')
file_name = paste0(path_out, 'EightOunce_Monogram_',rundate,".csv")
write_csv(final,file_name)
