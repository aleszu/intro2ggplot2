
# Intermediate R tutorial 


# Exploring Facebook political ads 
# Load data from CSV and ggplot2 with scales 

library()
library(dplyr)

df <- read.csv("fbads_bcir.csv", header=TRUE, stringsAsFactors = FALSE) 
glimpse(df)

df$date <- as.Date(df$date) # make R recognize date column 
glimpse(df)

summary(df) # check date min, max and other variables

hist(df$date, breaks=50)
hist(df$impressions, breaks=20)

library(ggplot2)
ggplot(df, aes(date)) + 
  geom_histogram(stat="count")

library(scales)

ggplot(df, aes(date)) + 
  geom_histogram(stat="count") +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b %Y")) +
  ggtitle("Facebook ads collected by ProPublica")

# Apply dplyr's filter with stringr

library(stringr)

df_beto <- df %>%
  filter(str_detect(message, "Beto"))

glimpse(df)
glimpse(df_beto)

ggplot(df_beto, aes(date)) + 
  geom_histogram(stat="count") +
  ggtitle("Facebook ads collected by ProPublica on Beto O'Rourke")




# ****Scatter plots!!!****

# First, understanding stringr

shortsentence <- "Hello world"
shortsentence

mylongsentence <- "A pistol shot at five o'clock"
mylongsentence

str_length(shortsentence) # includes spaces
str_length(mylongsentence)

# Inspired by https://buzzfeednews.github.io/2018-01-trump-state-of-the-union/
# Data from https://github.com/BuzzFeedNews/2018-01-trump-state-of-the-union

sou <- read.csv("sou.csv", header=TRUE, stringsAsFactors = FALSE)
glimpse(sou)

presidents <- read.csv("presidents.csv", header=TRUE, stringsAsFactors = FALSE)
glimpse(presidents)

sou <- sou %>%
  left_join(presidents) # This will merge or "join" in the president's party

glimpse(sou)

# https://github.com/aleszu/intro2ggplot2/blob/master/dplyr-joins.png?raw=true
# More resources on joins and binding rows or columns: 
# https://rpubs.com/williamsurles/293454


# ****MUTATE!****
# Using mutate to create a new column based on a new function
# First we'll fix the date

sou <- sou %>% 
  mutate(year = as.Date(date)) 
glimpse(sou)

# Next we'll measure the length of each state of the union, 
# loop through the whole "text" column and create the column with result

sou <- sou %>%
  mutate(length = str_length(text))
glimpse(sou)

summary(sou$length)

# Plot histogram
ggplot(sou, aes(length)) + geom_histogram()

# Plot box plot which has median, 1st and 3rd quartiles, min and max. 
ggplot(sou, aes(party, length)) + geom_boxplot()

# Plot scatterplot
ggplot(sou, aes(year, length)) + geom_point() 

# Improve scatterplot
ggplot(sou, aes(year, length)) + 
  geom_point(aes(color = party)) +
  scale_x_date(breaks = date_breaks("30 years"), labels = date_format("%Y")) +
  geom_smooth(method=loess) # this adds a "loess" local weighted regression line, a smooth curve for scatterplots
  #+geom_smooth(method=lm)  # One can also do a linear regression to see that fit







# Scatterplot of Roll Call
# https://www.rollcall.com/news/campaigns/lead-midterms-twitter-republicans-went-high-democrats-went-low

rollcalldf <- read.csv("final-df-roll-call.csv", header=TRUE, stringsAsFactors = FALSE)

glimpse(rollcalldf)

ggplot(rollcalldf, aes(y=percent_of_vote, x=avgscore, color=party)) + 
  geom_point(aes(size=followers_count)) + 
  geom_smooth(method=lm, se=F)+
  scale_size(name="", range = c(2, 8)) +
  scale_color_manual(values=c("#404f7c", "#34a35c", "#34a35c", "#c63b3b")) +
  theme_minimal() +
  xlab("Average sentiment of tweets") +
  ylab("Percent of vote")









# Map with ggplot 

library(dplyr)

schoolshootings <- read.csv("school-shootings-since-2015.csv", stringsAsFactors=F)
glimpse(schoolshootings)

library(maps)
states <- map_data("state")
states %>% glimpse()

ggplot() + 
  geom_polygon(data=states, aes(x=long, y=lat, group=group),colour="white", fill="grey92" ) + 
  geom_point(data=schoolshootings, aes(x=long, y=lat), color="black") +  #size = killed
  theme_void() +
  coord_fixed(ratio = 1.3)

# massachusetts <- states %>%
#   filter(region == "massachusetts")
# glimpse(massachusetts)

ggsave("map2.png")



# Interactive map

library(leaflet)
library(geojsonio)

boston <- read.csv("boston-breweries.csv", stringsAsFactors=F)

glimpse(boston)

bostoneight <- geojsonio::geojson_read("mass-8ft.geojson", what = "sp")
glimpse(bostoneight)

bostonmap <- leaflet(boston) %>% #addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
  #  attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13)  %>% 
  # addPolygons(data=bostoneight,
  #             col = 'dodgerblue',
  #             stroke = FALSE, 
  #             fillOpacity = 0.3, 
  #             smoothFactor = 0.5) %>%
  addCircleMarkers(data=boston,
                   lng=~lon, 
                   lat=~lat, 
                   label=~string,
                   labelOptions = labelOptions(noHide = TRUE, offset=c(0,-12), textOnly = TRUE),
                   weight = 3, 
                   radius=8, 
                   color="#ffa500", 
                   stroke = TRUE, 
                   fillOpacity = 1)  

bostonmap






