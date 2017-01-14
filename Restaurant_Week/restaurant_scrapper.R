library(leaflet)
library(rvest)
library(stringr)
library(dplyr)
library(xml2)

# This code does the initial scraping of restaurant week and Michelin star restaurants 

# Had to run the following block four times because I couldn't figure out how to paginate in R. If anyone knows, please let me know!
# Scrapes the page for restaurant names and puts into a dataframe
restaurant_week_p1 = read_html("https://www.opentable.com/promo.aspx?covers=2&currentview=list&datetime=2017-01-09+19%3A00&metroid=8&promoid=69&ref=1351&size=100&sort=Rating&from=0")
restaurant_week_p1 = html_nodes(restaurant_week_p1,".rest-row-name-text")
restaurant_week_p1 = html_text(restaurant_week_p1)
restaurant_week_p1 = as.data.frame(restaurant_week_p1, stringsAsFactors= F)
names(restaurant_week_p1) = c("restaurant")

restaurant_week_p2 = read_html("https://www.opentable.com/promo.aspx?covers=2&currentview=list&datetime=2017-01-09+19%3A00&metroid=8&promoid=69&ref=1351&size=100&sort=Rating&from=100")
restaurant_week_p2 = html_nodes(restaurant_week_p2,".rest-row-name-text")
restaurant_week_p2 = html_text(restaurant_week_p2)
restaurant_week_p2 = as.data.frame(restaurant_week_p2, stringsAsFactors= F)
names(restaurant_week_p2) = c("restaurant")

restaurant_week_p3 = read_html("https://www.opentable.com/promo.aspx?covers=2&currentview=list&datetime=2017-01-09+19%3A00&metroid=8&promoid=69&ref=1351&size=100&sort=Rating&from=200")
restaurant_week_p3 = html_nodes(restaurant_week_p3,".rest-row-name-text")
restaurant_week_p3 = html_text(restaurant_week_p3)
restaurant_week_p3 = as.data.frame(restaurant_week_p3, stringsAsFactors= F)
restaurant_week_p3 = unname(restaurant_week_p3)
names(restaurant_week_p3) = c("restaurant")

restaurant_week_p4 = read_html("https://www.opentable.com/promo.aspx?covers=2&currentview=list&datetime=2017-01-09+19%3A00&metroid=8&promoid=69&ref=1351&size=100&sort=Rating&from=300")
restaurant_week_p4 = html_nodes(restaurant_week_p4,".rest-row-name-text")
restaurant_week_p4 = html_text(restaurant_week_p4)
restaurant_week_p4 = as.data.frame(restaurant_week_p4, stringsAsFactors= F)
restaurant_week_p4 = unname(restaurant_week_p4)
names(restaurant_week_p4) = c("restaurant")

restaurant_week = rbind(restaurant_week_p1,restaurant_week_p2)
restaurant_week = rbind(restaurant_week, restaurant_week_p3)
restaurant_week = rbind(restaurant_week, restaurant_week_p4)

# Scrape the Wikipedia page for Michelin Star restaurants in 2017. Special code needed 
# because star ratings are in image form and needed to be converted to text
url <- "http://en.wikipedia.org/wiki/List_of_Michelin_starred_restaurants_in_New_York_City"
doc <- read_html(url)
col_names <- doc %>% html_nodes("#mw-content-text > table > tr:nth-child(1) > th") %>% html_text()
tbody <- doc %>% html_nodes("#mw-content-text > table > tr:not(:first-child)")

extract_tr <- function(tr){
  scope <- tr %>% html_children()
  c(scope[1:2] %>% html_text(),
    scope[3:length(scope)] %>% html_node("img") %>% html_attr("alt"))
}

res <- tbody %>% sapply(extract_tr)
res <- as.data.frame(t(res), stringsAsFactors = FALSE)
colnames(res) <- col_names

michelin_stars = res[,c(1:2,14)]
michelin_stars=michelin_stars[!(is.na(michelin_stars$`2017 (77)[15]`)),]


# Merge list of Michelin restaurants with 'restaurants' dataframe
restaurants=merge(restaurant_week,michelin_stars,by.x="restaurant",by.y="Name",all.x=T)
restaurants = restaurants[,c(1,3)]
restaurants = as.data.frame(restaurants,stringsAsFactors=F)

# Rename some elements to allow it to run easier through APIs
restaurants[4,1] = '2West'
restaurants[225,1] = 'Members Dining Room at the Metropolitan Museum of Art'
restaurants[196,1] = 'Lafayette Restaurant'
View(restaurants)

setwd("C://../")
write.csv(restaurants,file="restaurants.csv")
