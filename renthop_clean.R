library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggmap)


setwd("C:/Kaggle_2sigma_rent")

train <- fromJSON("./data/train.json") %>% bind_rows 
test <- fromJSON("./data/test.json") %>% bind_rows 

train$train <- TRUE
test$train <- FALSE
test$interest_level <- 'none'
data <- rbind(train, test)

# Keep list variables for later analysis
features <- data[,c("listing_id","interest_level", "train",'features')]
photos <- data[,c("listing_id",'photos')]

# Remove list variables from data
data$features <- NULL
data$photos <- NULL 


# Convert to data.frame
data <- sapply(data, unlist) %>%
    data.frame(., stringsAsFactors = FALSE)


# Identify numeric variables
numerical_variables <- c("bathrooms", "bedrooms",
                         "longitude", "latitude", "price")

# transform the numerical variables to numeric type
res <-  apply(data[,numerical_variables],2,as.numeric)
data[, numerical_variables] <- res

# Transform the outcome variable to factor
data$interest_level <- as.factor(data$interest_level)

# Identify records with na coordinates
coord_na <- data[data$latitude == 0.0 | data$longitude == 0.0,]
head(coord_na)


# Find the coordinates related to the addresses
coords <- sapply(coord_na$street_address,
                 function(x) geocode(x, source = "google")) 
coords <- data.frame(t(coords))

# Update the coordinates
data[data$latitude == 0.0 | data$longitude == 0.0,]$longitude <- unlist(coords$lon)
data[data$latitude == 0.0 | data$longitude == 0.0,]$latitude <- unlist(coords$lat)

save(data, file = "./files/data_cleaned.Rda")
save(features, file = "./files/data_features.Rda")
save(photos, file = "./files/data_photos.Rda")



