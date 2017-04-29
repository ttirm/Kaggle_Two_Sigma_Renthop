library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(lazyeval)
library(reshape2)
library(stringr)
library(gsubfn)

library(tm)
library(SnowballC)
library(Matrix)
library(RWeka)
library(pointdensityP)
library(RecordLinkage)

library(syuzhet)

source("./utils.R")

setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_2sigma_rent")
load("./files/data_cleaned.Rda")

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}



top_words <- function(data, tag){
    all_text <- Corpus(VectorSource(data))

    dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                                  removeNumbers=FALSE,stopwords=TRUE,
                                                  stemming=FALSE,weighting=function(x) weightTfIdf(x,normalize=FALSE)))

    dtm <- removeSparseTerms(dtm,0.999)
    df_q<-Matrix(as.matrix(dtm),sparse=T)
    df_q<-as.data.frame(as.matrix(dtm))
    colnames(df_q)=paste(tag,colnames(df_q),sep="")
    df_q
}



similarity <- levenshteinSim(tolower(data$street_address),tolower(data$display_address))
data$similarity <-similarity > 0.5
data$created_num <- as.numeric(difftime(data$created, min(data$created), units = "secs"))
data$listing_num <- as.numeric(data$listing_id) - min(as.numeric(data$listing_id))

#plot(data[data$interest_level == "high","created_num"], data[data$interest_level == "high","listing_num"])


# Define a regression line just for high interest houses
fit_lm <- lm(data = data[data$interest_level == "high",], listing_num~created_num)

res <- vector()
for(i in 1:nrow(data)){
    # Calculate the distance between each point and the line previously defined
    res[i] <- distancePointLine(data[i, "created_num"], data[i, "listing_num"], 0.05456, 836.87602)
}

# Normalize
res1 <- (res - mean(res))/sd(res)

data$listing_diver <- res1


load("./files/data_features.Rda")
load("./files/data_photos.Rda")

# Count the number of features
res <- unlist(lapply(features$features, length))
data$features_num <- res


# Count the number of photos
res <- unlist(lapply(photos$photos, length))
data$photos_num <- res



data1 <-data[order(data$created),]

# Verify which records are duplicated
res <- duplicated(data[, c("building_id", "street_address", "display_address", "description", "bathrooms", 
                           "bedrooms", "price", "latitude", "longitude", "features_num", "photos_num")])

data1$duplicated <- res
data <- left_join(data, data1[, c("listing_id", "duplicated")])

rm(photos)
rm(features)
rm(data1)
gc()


data$street_address <- tolower(data$street_address)

numbers <- gsub( " .*$", "", data$street_address)
numbers <- gsub( "-.*$", "", numbers)
numbers <- gsub( "\\..*$", "", numbers)
numbers <- gsub( "[a-z]*", "", numbers)
numbers <- gsub("[^[:alnum:] ]", "", numbers)
numbers[numbers == ""] <- 0

# Empty street records
data$numbers <- numbers

# Normalize street address 
data$street_address <- gsub("first", "1",data$street_address)
data$street_address <- gsub("second", "2",data$street_address)
data$street_address <- gsub("third", "3",data$street_address)
data$street_address <- gsub("fourth", "4",data$street_address)
data$street_address <- gsub("fifth", "5",data$street_address)
data$street_address <- gsub("sixth", "6",data$street_address)
data$street_address <- gsub("seventh", "7",data$street_address)
data$street_address <- gsub("eighth", "8",data$street_address)
data$street_address <- gsub("ninth", "9",data$street_address)
data$street_address <- gsub("tenth", "10",data$street_address)
data$street_address <- gsub("eleventh", "11",data$street_address)
data$street_address <- gsub(" w ", " west ",data$street_address)
data$street_address <- gsub(" e ", " east ",data$street_address)
data$street_address <- gsub(" n ", " north ",data$street_address)
data$street_address <- gsub(" s ", " south ",data$street_address)
data$street_address <- gsub("st.", " street ",data$street_address)
data$street_address <- gsub("pl.", " place ",data$street_address)
data$street_address <- gsub("st ", " street ",data$street_address)
data$street_address <- gsub(" ave ", " avenue ",data$street_address)
data$street_address <- trimws(gsub(" +", " ",data$street_address))

# Normalize display address
data$display_address <- tolower(data$display_address)
data$display_address <- gsub("first", "1",data$display_address)
data$display_address <- gsub("second", "2",data$display_address)
data$display_address <- gsub("third", "3",data$display_address)
data$display_address <- gsub("fourth", "4",data$display_address)
data$display_address <- gsub("fifth", "5",data$display_address)
data$display_address <- gsub("sixth", "6",data$display_address)
data$display_address <- gsub("seventh", "7",data$display_address)
data$display_address <- gsub("eighth", "8",data$display_address)
data$display_address <- gsub("ninth", "9",data$display_address)
data$display_address <- gsub("tenth", "10",data$display_address)
data$display_address <- gsub("eleventh", "11",data$display_address)
data$display_address <- gsub(" w ", " west ",data$display_address)
data$display_address <- gsub(" e ", " east ",data$display_address)
data$display_address <- gsub(" n ", " north ",data$display_address)
data$display_address <- gsub(" s ", " south ",data$display_address)
data$display_address <- gsub("st.", " street ",data$display_address)
data$display_address <- gsub("pl.", " place ",data$display_address)
data$display_address <- gsub("st ", " street ",data$display_address)
data$display_address <- gsub(" ave ", " avenue ",data$display_address)
data$display_address <- trimws(gsub(" +", " ",data$display_address))



#number of buildings per street
data$street_address <- trimws(tolower(data$street_address))
hold <- group_by(data, street_address)
hold <- summarise(hold, street_num = n(), Unique_buildings = n_distinct(building_id))
hold$street_price_mean <- sapply(hold$street_address, function(x){mean(data[data$street_address == x,"price"], na.rm = TRUE)})
hold$street_price_sd <- sapply(hold$street_address, function(x){sd(data[data$street_address == x,"price"], na.rm = TRUE)})
data <- left_join(data, hold)
data$street_address[data$street_num == 1] <- -1

# Group records that belong to repeated houses
hold <- group_by_(data[,], .dots=c("building_id", "bathrooms", 
                                "bedrooms", "latitude", "longitude", "features_num", "photos_num")) %>%
    count(building_id, bathrooms, 
          bedrooms, latitude, longitude, features_num, photos_num)

hold$house <-  paste0("house_" ,as.character(row.names(hold)))
data <- left_join(data, hold)

# Verify which houses had a price drop after the first record
# house <- unique(data$house)
# for(i in 1:length(house)){
#     set <- data[data$house == house[i], c("created", "price")]
#     set <- set[order(set$created, decreasing = FALSE),]
#     first_price <- head(set$price,1)
#     data$droped[data$house == house[i] & data$price < first_price] <- TRUE
#     data$droped[data$house == house[i] & data$price >= first_price] <- FALSE
# 
# }
# drp <- data$droped
# save(drp, file = "./files/price_droped.Rda")

# Load a previously created indicator (to save time)
load("./files/price_droped.Rda")
data$droped <- drp

# Assign -1 to all display addresses that apear just one time
data$display_address <- trimws(tolower(data$display_address))
hold <- group_by(data, display_address)
hold <- summarise(hold, display_num = n())
data <- left_join(data, hold)
data$display_address[data$display_num == 1] <- -1
#data$street_price_dev <- (data$price - data$street_price_mean) / data$street_price_sd
#data$street_price_dev[data$display_num == 1] <- -1



hold <- group_by(data, manager_id)
hold <- summarise(hold, manager_num = n(), sum_manager_buildings = n_distinct(building_id))
data <- left_join(data, hold)
# Define as ranked all the managers with more than 20 records
data$manager_ranked <- data$manager_num > 20
# Normalize manager listing numbers
data$manager_num_scale <- (data$manager_num - mean(data$manager_num) /  sd(data$manager_num))
# Assign -1 the managers with just one listing
data$manager_id[data$manager_num == 1] <- -1




# Transform the date of creation to date type
data$created <- as.POSIXct(data$created)
data$month_c <- month(data$created)
data$week <- week(data$created)
data$wday_c <- wday(data$created)
data$hour_c <- hour(data$created)
data$mday_c <- mday(data$created)






data1 <- data
data1$street_address <- unlist(sapply(data1$street_address, tolower))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("\\.", ' ',x)))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("-", '',x)))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("^[a-z]+ ", '123 ',x)))

# Define avenue number
res1 <- strapplyc(gsub("^[0-9]+ ", "",data1$street_address), "(.*) ave", simplify = TRUE)

#res[lapply(res,length)==0] <- -1
res1[lapply(res1,length)==0] <- -1
data1$avenue_num <- trimws(unlist(res1))

data1$avenue_num <- gsub("first", "1",data1$avenue_num)
data1$avenue_num <- gsub("second", "2",data1$avenue_num)
data1$avenue_num <- gsub("third", "3",data1$avenue_num)
data1$avenue_num <- gsub("fourth", "4",data1$avenue_num)
data1$avenue_num <- gsub("fifth", "5",data1$avenue_num)
data1$avenue_num <- gsub("sixth", "6",data1$avenue_num)
data1$avenue_num <- gsub("seventh", "7",data1$avenue_num)
data1$avenue_num <- gsub("eighth", "8",data1$avenue_num)
data1$avenue_num <- gsub("ninth", "9",data1$avenue_num)
data1$avenue_num <- gsub("tenth", "10",data1$avenue_num)
data1$avenue_num <- gsub("eleventh", "11",data1$avenue_num)
data1$avenue_num <- gsub("west end", "11",data1$avenue_num)
data1$avenue_num <- gsub("lexington", "4",data1$avenue_num)
data1$avenue_num <- gsub("central park west", "8",data1$avenue_num)
data1$avenue_num <- gsub("lexington", "4",data1$avenue_num)
data1$avenue_num <- gsub("columbus", "9",data1$avenue_num)
data1$avenue_num <- gsub("amsterdam", "10",data1$avenue_num)
data1$avenue_num <- gsub("park", "4",data1$avenue_num)
data1$avenue_num <- gsub("madison", "4",data1$avenue_num)
data1$avenue_num <- gsub("americas", "6",data1$avenue_num)
data1$avenue_num <- gsub("fashion", "7",data1$avenue_num)
data1$avenue_num <- gsub("st|nd|rd|th", "",data1$avenue_num)

res <- unlist(sapply(data1$avenue_num, function (x) {ifelse(length(tail(unlist(strsplit(x, " ")), 1)) >0,tail(unlist(strsplit(x, " ")), 1), -1)}))
unique(res[nchar(res) == 1 & !grepl("[1-9]+", res)])
res[nchar(res) == 1 & !grepl("[1-9]+", res)] <- -1
res[nchar(res) == 2 & grepl("[1-9]+", res)] <- -1
res[nchar(res) == 3 & grepl("[1-9]+", res)] <- -1
unique(res)

data1$avenue_num <- res
street <- data1[, c("street_address","avenue_num")]
hold <- group_by(street, avenue_num)
hold <- summarise(hold, ave_num = n())
street <- left_join(street, hold)

# Assign -1 to all the avenues with less than 10 appearances
street$avenue_num[street$ave_num < 10] <- -1
data$avenue_num <- street$avenue_num
rm(street)
gc()

# Creates a feature of records within Manhattan
data$manhattan <- grepl(" east | west | w | e | south | north ([0-9]+|[0-9]+([a-z]){2}|[0-9]+ ([a-z]){2})", data1$street_address)

data$manhattan[!data$manhattan] <- grepl("(first|second|third|lexington|park|park|madison|fifth|sixth|seventh|eighth|ninth|tenth|elenventh|twelfth|amsterdam|york|columbus|convent) (ave|av|avenue)",
                                               data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("avenue of the americas", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("broadway", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("west end (avenue|ave)", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("(ft|fort) washington (avenue|ave)", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("avenue|av|ave (c|d)", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("( [0-9]+| [0-9]+([a-z]){2}| [0-9]+ ([a-z]){2}) (ave|av|avenue)", data1$street_address[!data$manhattan])
data$manhattan[!data$manhattan] <- grepl("^[0-9]+ ([0-9]+|[0-9]+([a-z]){2}|[0-9]+ ([a-z]){2} (st|street))", data1$street_address[!data$manhattan])

data$avenue <- grepl(" avenue | avenue*| ave | ave*", data1$street_address)

rm(data1)
gc()

#head(data$building_id)

# Round the coordinates to aggregate close places
data$lat <- round(data$latitude,4)
data$lon <- round(data$longitude,4)

# Coordinates deviation from median
data$lat_median <- data$latitude - median(data$latitude)
data$lng_median <- data$longitude - median(data$longitude)



# Bathrooms and Bedrooms features
data$bathrooms[data$bathrooms >= 10] <- as.numeric(unlist(lapply(strsplit(as.character(round(data$bathrooms[data$bathrooms >= 10],0)),""),max)))
hold <- group_by(data, building_id)
hold <- summarise(hold, build_num = n())
hold$beds_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bedrooms"], na.rm = TRUE)})
hold$baths_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bathrooms"], na.rm = TRUE)})
hold$price_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"price"], na.rm = TRUE)})
hold$beds_sd <- sapply(hold$building_id, function(x){sd(data[data$building_id == x,"bedrooms"], na.rm = TRUE)})
hold$baths_sd <- sapply(hold$building_id, function(x){sd(data[data$building_id == x,"bathrooms"], na.rm = TRUE)})
hold$price_sd <- sapply(hold$building_id, function(x){sd(data[data$building_id == x,"price"], na.rm = TRUE)})
hold$beds_median <- sapply(hold$building_id, function(x){median(data[data$building_id == x,"bedrooms"], na.rm = TRUE)})
hold$baths_median <- sapply(hold$building_id, function(x){median(data[data$building_id == x,"bathrooms"], na.rm = TRUE)})
data <- left_join(data, hold)
data$building_id[data$build_num == 1] <- -1

data$bathrooms[data$bathrooms > 3] <- 3
data$bedrooms[data$bedrooms > 5] <- 5

# Log of price
data$price_log <- log(data$price+1)


data$rooms_dev <- (data$bedrooms - data$beds_mean) / data$beds_sd
data$baths_dev <- (data$bathrooms - data$baths_mean) / data$baths_sd
data$price_dev <- (data$price - data$price_mean) / data$price_sd

#data$rooms_median <- data$bedrooms - data$beds_median
#data$baths_median <- data$bathrooms - data$baths_median

data$bed_bath_ratio <- round(data$bedrooms/data$bathrooms,1)
data$bed_bath_ratio[is.na(data$bed_bath_ratio)] <- -1
data$bed_bath_ratio[data$bed_bath_ratio == Inf] <- -1
#data$bed_bath_ratio[data$bed_bath_ratio > 3] <- 3

data$price_bed <- data$price / data$bedrooms
data$price_bath <- data$price / data$bathrooms
data$price_bb <- data$price / (data$bedrooms + data$bathrooms)
data$price_bath[!is.finite(data$price_bath)] <- -1
data$price_bed[!is.finite(data$price_bed)] <- -1
data$price_bb[!is.finite(data$price_bb)] <- -1

data$bed_rat <- data$bedrooms / (data$bedrooms + data$bathrooms)
data$bath_rat <- data$bathrooms / (data$bedrooms + data$bathrooms)
data$bath_rat[!is.finite(data$bath_rat)] <- -1
data$bed_rat[!is.finite(data$bed_rat)] <- -1

data$bed_minus_bath <- data$bedrooms - data$bathrooms
data$bed_plus_bath <- data$bedrooms + data$bathrooms

data$half_bathrooms <- (round(data$bathrooms)-data$bathrooms) > 0

# Coordinates deviation from center (scaled)
data$lat_center <- (data$lat - 40.7831)/sd(data$lat)
data$lon_center <- (data$lon - (-73.9712))/sd(data$lon)

# Scaled coordinates
data$lat_norm <- (data$latitude - mean(data$latitude))/sd(data$latitude)
data$lon_norm <- (data$longitude - mean(data$longitude))/sd(data$longitude)

# Coordinates 40 clusters
res <- kmeans(data[,c("lat_norm", "lon_norm")], 40, nstart = 30)
data$neighbours <- res$cluster

# Coordinates 100 clusters
res <- kmeans(data[,c("lat_norm", "lon_norm")], 100, nstart = 30)
data$neighbours_100 <- res$cluster

# Coordinates 100 clusters with manager number of buildings
res <- kmeans(data[,c("lat_norm", "lon_norm", "manager_num_scale")], 100, nstart = 30)
data$neighbours_100_managers <- res$cluster


hold <- group_by(data, neighbours_100)
hold <- summarise(hold, clust_num = n())
hold$clust_mean <- sapply(hold$neighbours_100, function(x){mean(data[data$neighbours_100 == x,"price"], na.rm = TRUE)})
hold$clust_sd <- sapply(hold$neighbours_100, function(x){sd(data[data$neighbours_100 == x,"price"], na.rm = TRUE)})
data <- left_join(data, hold)
data$price_clust <- (data$price - data$clust_mean)/data$clust_sd

# Price per bedroom within cluster
data$mean_neighbourhood_ppb <- data$clust_mean/data$bedrooms

data$low <- as.integer(data$interest_level == 'low')
data$medium <- as.integer(data$interest_level == 'medium')
data$high <- as.integer(data$interest_level == 'high')

data$low_medium <- as.integer(data$interest_level == 'low' || data$interest_level == 'medium')
data$medium_high <- as.integer(data$interest_level == 'medium' || data$interest_level == 'high')
data$high_low <- as.integer(data$interest_level == 'high' || data$interest_level == 'low')



data$low_prob <- mean(data$low[data$train == TRUE], na.rm = TRUE)
data$medium_prob <- mean(data$medium[data$train == TRUE], na.rm = TRUE)
data$high_prob <- mean(data$high[data$train == TRUE], na.rm = TRUE)

data$low_medium_prob <- mean(data$low_medium[data$train == TRUE], na.rm = TRUE)
data$medium_high_prob <- mean(data$medium_high[data$train == TRUE], na.rm = TRUE)
data$high_low_prob <- mean(data$high_low[data$train == TRUE], na.rm = TRUE)

#head(data$building_id)

train <- data[data$train == TRUE,]
test <- data[data$train == FALSE,]



weight_prior_mean <- function(data, col_group, col_out, k, f, g = 1, lambda= NULL, cv = cv1){
    
    tr <- data[data$train == TRUE,]
    ts <- data[data$train == FALSE,]
    ind <- unlist(cv, use.names=FALSE)
    vec_avg <- NULL
    #print(head(tr$building_id))
    for (i in 1:length(cv)){
        #print(head(tr))
        trs <- tr[cv[[i]],]
        trr <- tr[-cv[[i]],]
        hold <- group_by_(trr, col_group)
        hold <- summarise_(hold,mean = interp(~mean(var, na.rm = TRUE), var = as.name(col_out)), 
                           sum = interp(~sum(var), var = as.name(col_out)),
                           count = interp(~length(var), var = as.name(col_out)))
        print(head(hold, 20))
        #trs1 <- merge(trs, hold, by = col_group, all.x=TRUE, sort=FALSE)
        trs1 <- left_join(trs, hold)
        #print(head(trs1, 20))
        trs1$mean[is.na(trs1$mean)] <- mean(trs1[,paste0(col_out, '_prob')])
        trs1$sum[is.na(trs1$sum)] <- 0
        trs1$count[is.na(trs1$count)] <- 0
        if(!is.null(lambda)){
            trs1$beta <- lambda

        } else{
            trs1$beta <- (1 / (g + exp((trs1$count - k) / f)))
        }
        trs1$wp_avg <- (1 - trs1$beta) * trs1$mean + trs1$beta * trs1[,paste0(col_out, '_prob')]
        trs1$wp_avg[is.na(trs1$wp_avg)] <- -1
        #trs1$wp_avg <- trs1$wp_avg*(1+(runif(nrow(trs1))-0.5)*0.01)
        vec_avg <- c(vec_avg, trs1$wp_avg)
    }
    vec_train <- data.frame(ind, vec_avg)
    vec_train <- vec_train[order(vec_train$ind),]
    #print(head(vec_train))
    hold <- group_by_(tr, col_group)
    hold <- summarise_(hold,mean = interp(~mean(var, na.rm = TRUE), var = as.name(col_out)),
                       sum = interp(~sum(var, na.rm = TRUE), var = as.name(col_out)),
                       count = interp(~length(var), var = as.name(col_out)))
    #ts <- merge(ts, hold, by = col_group, all.x=TRUE, sort=FALSE)
    ts <- left_join(ts, hold)
    ts$mean[is.na(ts$mean)] <- mean(ts[,paste0(col_out, '_prob')])
    ts$sum[is.na(ts$sum)] <- 0
    ts$count[is.na(ts$count)] <- 0
    if(!is.null(lambda)){
        ts$beta <- lambda
    } else{
        ts$beta <- 1 / (g + exp((ts$count - k) / f))
    }

    ts$wp_avg <- (1 - ts$beta) * ts$mean + ts$beta * ts[,paste0(col_out, '_prob')]
    ts$wp_avg[is.na(ts$wp_avg)] <- ts[is.na(ts$wp_avg), paste0(col_out, '_prob')]

    return(c(vec_train$vec_avg, ts$wp_avg))
 
}



set.seed(1103)
cv1 <- createFolds(train$interest_level, k = 5, list = TRUE, returnTrain = FALSE)

# Load manager percentilles calculated in cv_mean.R
load("./cv_mean_1.csv")
data <- cbind(data, vec_train)
rm(vec_train)
gc()


# Weighted average of the prior probability (adaptation from Branden Murray's work)
#############################################################################################
data$wei_build_low <-weight_prior_mean(data[, c('building_id', 'low', 'train', 'low_prob')], 'building_id', 'low', k = 5, f= 1, cv = cv1)
data$wei_build_medium <-weight_prior_mean(data[, c('building_id', 'medium', 'train', 'medium_prob')], 'building_id', 'medium', k = 5, f= 1, cv = cv1)
data$wei_build_high <-weight_prior_mean(data[, c('building_id', 'high', 'train', 'high_prob')], 'building_id', 'high', k = 5, f= 1, cv = cv1)

data$wei_build_low_1 <-weight_prior_mean(data[, c('building_id', 'low', 'train', 'low_prob')], 'building_id', 'low', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_build_medium_1 <-weight_prior_mean(data[, c('building_id', 'medium', 'train', 'medium_prob')], 'building_id', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_build_high_1 <-weight_prior_mean(data[, c('building_id', 'high', 'train', 'high_prob')], 'building_id', 'high', k = 5, f= 1,lambda = 0, cv = cv1)

data$wei_build_low_scale <- data$wei_build_low_1 - min(data$wei_build_low_1[data$wei_build_low_1 >0]) / (max(data$wei_build_low_1) - min(data$wei_build_low_1[data$wei_build_low_1 >0]))
data$wei_build_medium_scale <- data$wei_build_medium_1 - min(data$wei_build_medium_1[data$wei_build_medium_1 >0]) / (max(data$wei_build_medium_1) - min(data$wei_build_medium_1[data$wei_build_medium_1 >0]))
data$wei_build_high_scale <- data$wei_build_high_1 - min(data$wei_build_high_1[data$wei_build_high_1 >0]) / (max(data$wei_build_high_1) - min(data$wei_build_high_1[data$wei_build_high_1 >0]))


data$wei_manager_low <-weight_prior_mean(data[, c('manager_id', 'low', 'train', 'low_prob')], 'manager_id', 'low', k = 5, f= 1, cv = cv1)
data$wei_manager_medium <-weight_prior_mean(data[, c('manager_id', 'medium', 'train', 'medium_prob')], 'manager_id', 'medium', k = 5, f= 1, cv = cv1)
data$wei_manager_high <-weight_prior_mean(data[, c('manager_id', 'high', 'train', 'high_prob')], 'manager_id', 'high', k = 5, f= 1, cv = cv1)

data$wei_manager_low_1 <-weight_prior_mean(data[, c('manager_id', 'low', 'train', 'low_prob')], 'manager_id', 'low', k = 5, f= 1, lambda = 0, cv = cv1)
data$wei_manager_medium_1 <-weight_prior_mean(data[, c('manager_id', 'medium', 'train', 'medium_prob')], 'manager_id', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_manager_high_1 <-weight_prior_mean(data[, c('manager_id', 'high', 'train', 'high_prob')], 'manager_id', 'high', k = 5, f= 1,lambda = 0, cv = cv1)

data$wei_manager_low_scale <- (data$wei_manager_low_1 - min(data$wei_manager_low_1[data$wei_manager_low_1 >0])) / (max(data$wei_manager_low_1) - min(data$wei_manager_low_1[data$wei_manager_low_1 >0]))
data$wei_manager_medium_scale <- (data$wei_manager_medium_1 - min(data$wei_manager_medium_1[data$wei_manager_medium_1 >0])) / (max(data$wei_manager_medium_1) - min(data$wei_manager_medium_1[data$wei_manager_medium_1 >0]))
data$wei_manager_high_scale <- (data$wei_manager_high_1 - min(data$wei_manager_high_1[data$wei_manager_high_1 >0])) / (max(data$wei_manager_high_1) - min(data$wei_manager_high_1[data$wei_manager_high_1 >0]))


data$wei_bedrooms_low_1 <-weight_prior_mean(data[, c('bedrooms', 'low', 'train', 'low_prob')], 'bedrooms', 'low', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_bedrooms_medium_1 <-weight_prior_mean(data[, c('bedrooms', 'medium', 'train', 'medium_prob')], 'bedrooms', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_bedrooms_high_1 <-weight_prior_mean(data[, c('bedrooms', 'high', 'train', 'high_prob')], 'bedrooms', 'high', k = 5, f= 1,lambda = 0, cv = cv1)


data$wei_house_low_1 <-weight_prior_mean(data[, c('house', 'low', 'train', 'low_prob')], 'house', 'low', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_house_medium_1 <-weight_prior_mean(data[, c('house', 'medium', 'train', 'medium_prob')], 'house', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_house_high_1 <-weight_prior_mean(data[, c('house', 'high', 'train', 'high_prob')], 'house', 'high', k = 5, f= 1,lambda = 0, cv = cv1)


res <- kmeans(data[,c("wei_manager_low_scale", "wei_manager_low_scale", "wei_manager_low_scale", 
                      "wei_build_low_scale", "wei_build_low_scale", "wei_build_low_scale")], 9, nstart = 50)
data$cluster_managers <- res$cluster

data$wei_avenue_medium <-weight_prior_mean(data[, c('avenue_num', 'medium', 'train', 'medium_prob')], 'avenue_num', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_avenue_high <-weight_prior_mean(data[, c('avenue_num', 'high', 'train', 'high_prob')], 'avenue_num', 'high', k = 5, f= 1,lambda = 0, cv = cv1)

data$wei_display_medium <-weight_prior_mean(data[, c('display_address', 'medium', 'train', 'medium_prob')], 'display_address', 'medium', k = 5, f= 1, cv = cv1)
data$wei_display_high <-weight_prior_mean(data[, c('display_address', 'high', 'train', 'high_prob')], 'display_address', 'high', k = 5, f= 1, cv = cv1)


data$wei_street_medium <-weight_prior_mean(data[, c('street_address', 'medium', 'train', 'medium_prob')], 'street_address', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_street_high <-weight_prior_mean(data[, c('street_address', 'high', 'train', 'high_prob')], 'street_address', 'high', k = 5, f= 1,lambda = 0, cv = cv1)

data$wei_clust_low <-weight_prior_mean(data[, c('neighbours_100', 'low', 'train', 'low_prob')], 'neighbours_100', 'low', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_clust_medium <-weight_prior_mean(data[, c('neighbours_100', 'medium', 'train', 'medium_prob')], 'neighbours_100', 'medium', k = 5, f= 1,lambda = 0, cv = cv1)
data$wei_clust_high <-weight_prior_mean(data[, c('neighbours_100', 'high', 'train', 'high_prob')], 'neighbours_100', 'high', k = 5, f= 1,lambda = 0, cv = cv1)


data$manager_skill_hi <- weight_points_power(data[, c('manager_id', 'high', 'train', 'high_prob')], 'manager_id', 'high', cv = cv1) / sum(data$high)
data$manager_skill_medium <- weight_points_power(data[, c('manager_id', 'medium', 'train', 'medium_prob')], 'manager_id', 'medium', cv = cv1) / sum(data$medium)
data$manager_skill_low <- weight_points_power(data[, c('manager_id', 'low', 'train', 'low_prob')], 'manager_id', 'low', cv = cv1) / sum(data$low)
############################################################################################################################


num <- weight_points_power(data[, c('manager_id', 'high', 'train', 'high_prob')], 'manager_id', 'high', cv = cv1)
# Percentilles
res <- quantile(num, c(0.99))
data$top1 <- (num >= res)
res <- quantile(num, c(0.01))
#res <- quantile(num, c(0.95))
data$top99 <- (num >= res)

# Percentagem from total highs/mediums/lows
data$build_skill_hi <- weight_points_power(data[, c('building_id', 'high', 'train', 'high_prob')], 'building_id', 'high', cv = cv1) / sum(data$high)
data$build_skill_medium <- weight_points_power(data[, c('building_id', 'medium', 'train', 'medium_prob')], 'building_id', 'medium', cv = cv1) / sum(data$medium)
data$build_skill_low <- weight_points_power(data[, c('building_id', 'low', 'train', 'low_prob')], 'building_id', 'low', cv = cv1) / sum(data$low)


# data$manager_skill_composed <- data$manager_skill_hi + data$manager_skill_medium +data$manager_skill_low
data$manager_skill_composed <- data$manager_skill_hi + data$manager_skill_medium

#Creation of quantiles (managers)
res <- quantile(data$manager_skill_composed, probs = seq(0, 1, 0.1), na.rm = TRUE)
data$manager_quantile[data$manager_skill_composed == res[1]] <- 0
data$manager_quantile[data$manager_skill_composed > res[1] & data$manager_skill_composed <= res[2]] <- 1
data$manager_quantile[data$manager_skill_composed > res[2] & data$manager_skill_composed <= res[3]] <- 2
data$manager_quantile[data$manager_skill_composed > res[3] & data$manager_skill_composed <= res[4]] <- 3
data$manager_quantile[data$manager_skill_composed > res[4] & data$manager_skill_composed <= res[5]] <- 4
data$manager_quantile[data$manager_skill_composed > res[5] & data$manager_skill_composed <= res[6]] <- 5
data$manager_quantile[data$manager_skill_composed > res[6] & data$manager_skill_composed <= res[7]] <- 6
data$manager_quantile[data$manager_skill_composed > res[7] & data$manager_skill_composed <= res[8]] <- 7
data$manager_quantile[data$manager_skill_composed > res[8] & data$manager_skill_composed <= res[9]] <- 8
data$manager_quantile[data$manager_skill_composed > res[9] & data$manager_skill_composed <= res[10]] <- 9
data$manager_quantile[data$manager_skill_composed > res[10] & data$manager_skill_composed <= res[11]] <- 10

# data$building_skill_composed <- data$build_skill_hi + data$build_skill_medium +data$build_skill_low
data$building_skill_composed <- data$build_skill_hi + data$build_skill_medium

#Creation of quantiles (buildings)
res <- quantile(data$building_skill_composed, probs = seq(0, 1, 0.1), na.rm = TRUE)
data$building_quantile[data$building_skill_composed == res[1]] <- 0
data$building_quantile[data$building_skill_composed > res[1] & data$wei_build_high <= res[2]] <- 1
data$building_quantile[data$building_skill_composed > res[2] & data$wei_build_high <= res[3]] <- 2
data$building_quantile[data$building_skill_composed > res[3] & data$wei_build_high <= res[4]] <- 3
data$building_quantile[data$building_skill_composed > res[4] & data$wei_build_high <= res[5]] <- 4
data$building_quantile[data$building_skill_composed > res[5] & data$wei_build_high <= res[6]] <- 5
data$building_quantile[data$building_skill_composed > res[6] & data$wei_build_high <= res[7]] <- 6
data$building_quantile[data$building_skill_composed > res[7] & data$wei_build_high <= res[8]] <- 7
data$building_quantile[data$building_skill_composed > res[8] & data$wei_build_high <= res[9]] <- 8
data$building_quantile[data$building_skill_composed > res[9] & data$wei_build_high <= res[10]] <- 9
data$building_quantile[data$building_skill_composed > res[10] & data$wei_build_high <= res[11]] <- 10

data$rating <- data$building_quantile + data$manager_quantile

save(data, file = "./files/data_all_1.Rda")
load("./files/data_all_1.Rda")

library(geosphere)
rad_dist <- function(x, y, z){
    distm (c(data$longitude[x], data$latitude[x]), c(y, z), fun = distHaversine)
}

# Calculate the distance from important neighbourhoods
data$manhattan_dist <-  sapply(seq_len(nrow(data)), rad_dist, -73.9943, 40.7527)
data$brooklyn_dist <-  sapply(seq_len(nrow(data)), rad_dist,  -73.952222, 40.624722)
data$bronx_dist <-  sapply(seq_len(nrow(data)), rad_dist, -73.8648, 40.8448)
data$queens_dist <-  sapply(seq_len(nrow(data)), rad_dist, -73.7949, 40.7282)
data$staten_dist <-  sapply(seq_len(nrow(data)), rad_dist, -74.1502, 40.5795)



data$manager_id <- as.integer(as.factor(data$manager_id))
data$house <- as.integer(as.factor(data$house))
data$numbers <- as.integer(data$numbers)
data$building_id <- as.integer(as.factor(data$building_id))
data$street_address <- as.integer(as.factor(data$street_address))
data$display_address <- as.integer(as.factor(data$display_address))
data$avenue_num <- as.integer(as.factor(data$avenue_num))

# Count words
data$description_num <- str_count(data$description)

# Count upper case letters
data$description_cap <- sapply(regmatches(data$description, gregexpr("[A-Z]", data$description, perl=TRUE)), length)

# Count exclamation marks
data$description_exclamation <- sapply(regmatches(data$description, gregexpr("!", data$description, perl=TRUE)), length)

# Position of the first exclamation mark
data$exclamation_first <- sapply(gregexpr("!", data$description, perl=TRUE), function(x)x[1])

# Ratio of exclamation marks per total letters
data$exlamation_ratio <- data$description_exclamation/data$description_num

# Search for strings
data$description_br <- sapply(regmatches(data$description, gregexpr('<br /><br />', data$description, perl=TRUE)), length)
data$description_redacted <- sapply(regmatches(data$description, gregexpr('website_redacted', data$description, perl=TRUE)), length)
data$description_at <- sapply(regmatches(data$description, gregexpr('@', data$description, perl=TRUE)), length)
data$description_star <- sapply(regmatches(data$description, gregexpr('\\*', data$description, perl=TRUE)), length)
data$description_phone <- sapply(regmatches(data$description, gregexpr('.*?(\\(?\\d{3}\\D{0,3}\\d{3}\\D{0,3}\\d{4}).*?', data$description, perl=TRUE)), length)



train[train$interest_level == "high", "description"]
sum(sapply(regmatches(train[train$interest_level == "high", "description"], gregexpr('gorgeous', train[train$interest_level == "high", "description"], perl=TRUE)), length))/length(train[train$interest_level == "high", "description"])

sum(sapply(regmatches(data$description, gregexpr('gorgeous', data$description, perl=TRUE)), length))/length(data$description)


load("./files/data_features.Rda")
load("./files/data_photos.Rda")


data$upper_num <- sapply(regmatches(features$features, gregexpr("[A-Z]", features$features, perl=TRUE)), length)

#**********************************************
# feat <- features
# feat$features <- gsub(" ", "_", feat$features)
# feat$features <- gsub("&", "_", feat$features)
# feat$features <- gsub("-", "_", feat$features)
# feat$features <- gsub("@", "", feat$features)
# feat$features <- gsub("'", "", feat$features)
# feat$features <- gsub("!", "", feat$features)
# feat$features <- gsub("\\*+", "", feat$features)
# feat$features <- gsub("\\(|\\)", "", feat$features)
# feat <- top_words(feat$features, "feat_")
# data <- cbind(data, feat)



#**********************************************

feat <- data.frame(listing_id = unlist(rep(features$listing_id, sapply(features$features, length))),
                   features = unlist(features$features))
feat$features <- str_split(feat$features, " \\* ")
feat <- data.frame(listing_id = unlist(rep(feat$listing_id, sapply(feat$features, length))),
                   features = unlist(feat$features))


mean_nchars <- function(x){
    round(mean(sapply(x, nchar), na.rm = TRUE),0)
}

res <- sapply(features$features, mean_nchars)
res[is.na(res)] <- 0
data$ft_mean_length <- res

feat$features <- trimws(tolower(feat$features))

feat$features <- paste0("feature_",trimws(tolower(feat$features)))
feat$features <- gsub("_+", "_", feat$features)
feat$features <- gsub("\\*+", "_", feat$features)
feat$features <- gsub("/+", "_", feat$features)
feat$features <- gsub("_a_c", "_ac", feat$features)
feat$features <- gsub("_$", "", feat$features)
feat$features <- gsub("s$", "", feat$features)
feat$features <- gsub(" ", "_", feat$features)
feat$features <- gsub("&", "_", feat$features)
feat$features <- gsub("-", "_", feat$features)
feat$features <- gsub("@", "", feat$features)
feat$features <- gsub("'", "", feat$features)
feat$features <- gsub("!", "", feat$features)
feat$features <- gsub("\\(|\\)", "", feat$features)

feat_backup <- feat

feat$features[is.na(feat$features)] <- "no_feat"

# Load features created in feature_ngrams.R 
load("./files/feat_hm.Rda")

find_feat <- function(x, vec){
    ifelse(tolower(unlist(features$features[x])) %in% vec,1,0) 
}
# Define groups of features
data$f_dogs <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("dogs", "dog"))))
data$f_cats <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("cats","cat"))))
data$f_nofee <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("no fee", "no-fee", "no fee", "nofee", "no_fee"))))
data$f_lowfee <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c ("reduced_fee", "low_fee", "reduced fee", "low fee"))))
data$f_excl <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("exclusive"))))
data$f_wood <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("parquet", "hardwood"))))
data$f_door <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("concierge", "doorman", "housekeep", "in_super"))))
data$f_prewar <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("prewar", "pre_war", "pre war", "pre-war"))))
data$f_laundry <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("laundry", "lndry" ,"laundry in Unit"))))
data$f_fit <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("health", "gym", "fitness", "training"))))
data$f_park <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("parking"))))
data$f_transp <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("train", "subway", "transport"))))
data$f_util <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("utilities", "heat water", "water included" ,"dishwasher"))))
data$f_elev <- max(unlist(sapply(seq_len(nrow(data)), find_feat, c("elevator"))))



 

hold <- group_by(feat, features)
hold <- summarise(hold, features_num = n())
#feat <- merge(feat, hold, by= "features", all.x=TRUE, sort=FALSE)
feat <- left_join(feat, hold)

# In a previous version were selected all the features with more than 9 appearences, however in the final version were selected the features present in feat_hm
#feat <- feat[feat$features_num >= 10 & !grepl("^feature_$",feat$features),]
feat <- feat[(feat$features %in% feat_hm) & !grepl("^feature_$",feat$features),]
unique(feat$features)[order(unique(feat$features))]

feat1 <- dcast(feat, listing_id ~ features, fun.aggregate = function(x) as.integer(length(x) > 0), value.var = "features")
data$listing_id <- as.numeric(data$listing_id)
data <- merge(data, feat1, by= "listing_id", all.x=TRUE, sort=FALSE)



# Log of listing_id
data$listing_id_log <- log(data$listing_id)

# Sentiment analysis
sentiment <- get_nrc_sentiment(unlist(data$description))
colnames(sentiment) <- paste0("sen_", colnames(sentiment))
data <- cbind(data,sentiment)

# Coordinates density
density <- pointdensity(df =data, lat_col = "latitude", lon_col = "longitude", grid_size = 0.5, radius = 1)
data$density <- density$count  

# Magic feature (leakage from photos files)
magic_feat <- read.csv("./files/listing_image_time.csv")
names(magic_feat) <- c("listing_id", "time_stamp")
data <- left_join(data, magic_feat)            

for(i in 1:ncol(data)){
    if(sum(is.na(data[,i]))>0){
        data[is.na(data[,i]), i] <- -1
    }

}


train <- data[data$train == TRUE,]
test <- data[data$train == FALSE,]

save(data, file = "./files/data_all.Rda")
save(train, file = "./files/train_all.Rda")
save(test, file = "./files/test.Rda")
