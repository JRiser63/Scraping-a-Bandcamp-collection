#Load relevant libraries####
library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RSelenium)
library(tools)
library(stringi)
library(tidytext)
library(wordcloud2)
library(stringr)


#Finding the url, and establishing a remote driver to get into the URL####
#Creating an object further functions can direct to for cleaner code

url <- "https://bandcamp.com/<xxx>" #Replace <xxx> with collection of interest

#Starting a driver so we can automatically click the "load" button in our bandcamp collection
#Chrome driver found at C:\Users\<xxx>\Documents\chromedriver
#Chrome driver has been added to the system path 

rd <- RSelenium::rsDriver(port = 4444L, browser = "chrome", chromever = "108.0.5359.71")
ffd <-  rd$client

#Navigating to the url above####

ffd$navigate(url)

#Find the load button & click it####
#After the load button is clicked, the page needs to scroll to pull in the rest of the data

load_button <- ffd$findElement(using = "class name", "show-more")
load_button$clickElement()

#Looping the scroll feature####
#Not too sure how many times we need to scroll (the parameters in our for loop), but I can stop once it reaches the end
for(i in 1:100){
  ffd$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(2)
}

#Scraping the html data and turning the info in the tags to text####

html_data <- ffd$getPageSource()[[1]]

artist_data <- html_data %>%
  read_html() %>%
  html_nodes('div.collection-title-details a.item-link div.collection-item-artist')  %>%
  html_text() 

#The gift I gave in MY collection will always appear last
#This ensures it gets deleted every time 

artist_data <- artist_data[-length(artist_data)]



#As there's no purchase data on the gift, the purchase data simply reflects my actual collection
purchase_data <- html_data %>%
  read_html() %>%
  html_nodes('div.collected-by div.collected-by-header a.item-link.also-link') %>%
  html_text()

album_data <- html_data %>%
  read_html() %>%
  html_nodes('div.collection-title-details a.item-link div.collection-item-title') %>%
  html_text()

#The gift I gave in MY collection will always appear last
#This ensures it gets deleted every time 
album_data <- album_data[-length(album_data)]




# Album art ####
#All albums are 700x700 so conversion to tensor would be easily applied to the whole dataset 

cover_image <- html_data %>%
  read_html() %>%
  html_nodes('img.collection-item-art') %>%
  html_attr('src')

cover_image <- cover_image[-length(cover_image)]


#The below loop will go through all links (where the images are stored) and download them to a constantly updating destination file

for(i in seq_along(cover_image)){
  destination <- paste0("C:\\Users\\<name>\\<PathOfInterest>\\Album_", i, ".jpg")
  download.file(cover_image[i], destfile = destination, mode = "wb") #Need to download the image as a "wb" as the pictures seem to be binary files
}

#Creating a dataframe to neatly store my scraped data####

bc_collection <- data.frame(album_data, artist_data, purchase_data)

#Cleaning the data to strip out unnecessary characters, spaces, and standardizing the text####

bc_collection_copy <- bc_collection %>%
  
  dplyr::mutate(album_clean = gsub("\n", "", album_data),
         album_clean = stringi::stri_trans_totitle(gsub("\\(gift given\\)", "", album_data)),
         purchase_clean = as.integer(gsub(" other collections", "", purchase_data)),
         artist_clean = stringi::stri_trans_totitle(gsub("by ", "", artist_data))) %>%
  
  dplyr::mutate(album_clean = gsub("\n", "", album_clean),
                album_clean = str_trim(album_clean, side = c("right"))) %>%
  
  dplyr::select(artist_clean, album_clean, purchase_clean) 

write.csv(bc_collection_copy,"C:\\Users\\<PathOfInterest>\\Clean Bandcamp collection.csv", row.names = F)

#To create a local copy of dirty data####
write.csv(bc_collection, "C:\\Users\\<PathOfInterest>\\Raw Bandcamp collection.csv")


#Import clean data####
#Now that the data has been scraped, we don't need to establish the driver anymore

bc_collection <- read.csv("C:\\Users\\<xxx>\\R\\Clean Bandcamp collection.csv") #Commenting out since X acts as my proxy for time, row.names = 'X')


# Simple plots to assess the Bandcamp collection of interest
#Did music popularity change over time?####
ggplot(data = bc_collection, aes(x = X, y = purchase_clean)) +
  geom_line() + 
  labs(x = 'Time (Descending order)', y = 'Number of Purchases') + #Since we don't have purchase date, we have to use the index (row number) as a proxy for when an album was bought
  ggtitle("Purchases made Over Time")

#Do some genres have more popularity than others?####
ggplot2::ggplot(data = bc_collection, aes(x = X, y = purchase_clean, colour=genre, fill = genre)) +
  geom_bar(stat='identity') +
  facet_wrap(~genre, scales = "free") +
  labs(x = 'Time (Reverse Order)', y = 'Number of Purchases')

#Do I prefer one genre?
ggplot2::ggplot(data = bc_collection, aes(format(genre))) +
  geom_bar(stat = 'count', aes(colour = genre, fill = genre)) +
  labs(x = "Genre", y = "Count of Genre in Collection") +
  ggtitle("Count of Albums by Genre")

#Any standout artists?
grouped_band_count <- bc_collection %>%
  group_by(artist_clean) %>%
  mutate(count = n(), 
         total_purchases = sum(purchase_clean)) %>%
  select(artist_clean, count, total_purchases) %>%
  distinct()

ggplot(data = grouped_band_count, aes(x = count, y = total_purchases)) +
  geom_point()


#Something I did for my collection was label all of the genres (what could be done is instead loop through all pages of every album and pull the tags from those pages to assign genre
  # , but multiple tags are usually assigned to promote fans from other genres to listen)


#Unnesting words####
unnested_albums <- bc_collection %>%
  unnest_tokens(word, album_clean) %>%
  filter(nchar(word) > 3)

unnested_bands <- bc_collection %>%
  unnest_tokens(word, artist_clean) %>%
  filter(nchar(word) > 3)

album_wc <- unnested_albums %>%
  count(word, sort = T) 

#Generating a wordcloud of the popular words from the album names in my collection
album_wc_image <- wordcloud2(album_wc[1:20, ], size = 0.75)
