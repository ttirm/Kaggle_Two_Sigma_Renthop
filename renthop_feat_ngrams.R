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

library(syuzhet)

setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_2sigma_rent")
load("./files/data_cleaned.Rda")

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# res <- lapply(data$description, function(x)gsub('!<br /><br />', '',x))
# res <- lapply(res, function(x)gsub('/><br', '',x))
# res <- lapply(res, function(x)gsub('<br', '',x))
# res <- lapply(res, function(x)gsub('<p><a  website_redacted ', '',x))
# res <- lapply(res, function(x)gsub(' 24', 'twenty four',x))
# data$description <- res

# all_text <- Corpus(VectorSource(res))
# all_text <- tm_map(all_text, tolower)
# all_text <- tm_map(all_text, removeWords, stopwords("english"))
# all_text <- tm_map(all_text, stripWhitespace)
# all_text <- tm_map(all_text, removePunctuation)
# all_text <- tm_map(all_text, stemDocument)
# 
# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# dtm <- DocumentTermMatrix(all_text, control = list(tokenize = TrigramTokenizer,
#                                               wordLengths=c(3, Inf), 
#                                               bounds=list(global=c(2, Inf))))


# top_words <- function(data, tag){
#     all_text <- Corpus(VectorSource(data))
#     
#     dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
#                                                   removeNumbers=TRUE,stopwords=TRUE,
#                                                   stemming=FALSE,weighting=function(x) weightTfIdf(x,normalize=FALSE)))
#     
#     dtm <- removeSparseTerms(dtm,0.99)
#     df_q<-Matrix(as.matrix(dtm),sparse=T)
#     df_q<-as.data.frame(as.matrix(dtm))
#     colnames(df_q)=paste(tag,colnames(df_q),sep="")
#     df_q
# }
# 
# top_words <- function(data, tag){
#     all_text <- Corpus(VectorSource(data))
#     BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#     dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
#                                                   removeNumbers=TRUE,stopwords=TRUE,
#                                                   stemming=FALSE,tokenize = BigramTokenizer,weighting=function(x) weightTfIdf(x,normalize=T)))
#     
#     #dtm <- removeSparseTerms(dtm,0.99)
#     df_q<-Matrix(as.matrix(dtm),sparse=T)
#     df_q<-as.data.frame(as.matrix(dtm))
#     colnames(df_q)=paste(tag,colnames(df_q),sep="")
#     df_q
# }
# 
# 
# 
# df_q1 <- top_words(unlist(data$description[data$interest_level == "high"]), "high_")
# df_q1 <- df_q1[,names(sort(colSums(df_q1), decreasing = TRUE)[1:200])]
# words <- names(sort(colSums(df_q1), decreasing = TRUE)[1:100])
# df_q1 <- top_words(data$description, "high_")
# 
# data1 <- cbind(data$interest_level, df_q1)
# data1 <- data1[data$train == TRUE & data$interest_level == 'high',-1]
# sort(colSums(data1), decreasing = TRUE)[1:50]
# 
# data1 <- cbind(data$interest_level, df_q1)
# data1 <- data1[data$train == TRUE & data$interest_level == 'medium',-1]
# sort(colSums(data1), decreasing = TRUE)[1:50]
# 
# df_q1 <- df_q1[,words]
# for (i in 1:length(names(df_q1))){
#     df_q1[df_q1[,i] > 0,i] <- 1
# }



# hold <- group_by(data, building_id)
# hold <- summarise(hold, build_num = n())
# hold$street_address_m <- sapply(hold$building_id, function(x){getmode(trimws(tolower(data[data$building_id == x,"street_address"])))})
# hold$display_address_m <- sapply(hold$building_id, function(x){getmode(trimws(tolower(data[data$building_id == x,"display_address"])))})
# #hold$beds_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bedrooms"], na.rm = TRUE)})
# #hold$baths_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bathrooms"], na.rm = TRUE)})
# data <- left_join(data, hold)
# #data$building_id[data$build_num == 1] <- -1

# data$street_address <- data$street_address_m
# data$display_address <- data$display_address_m
# data <- data[,!(names(data) %in% c("street_address_m", "display_address_m"))]

hold <- group_by(data, manager_id)
hold <- summarise(hold, manager_num = n())
data <- left_join(data, hold)
data$manager_id[data$manager_num == 1] <- -1


data$street_address <- trimws(tolower(data$street_address))
hold <- group_by(data, street_address)
hold <- summarise(hold, street_num = n())
data <- left_join(data, hold)
data$street_address[data$street_num == 1] <- -1

data$display_address <- trimws(tolower(data$display_address))
hold <- group_by(data, display_address)
hold <- summarise(hold, display_num = n())
data <- left_join(data, hold)
data$display_address[data$display_num == 1] <- -1


# Transform the date of creation to date type
data$created <- as.POSIXct(data$created)
data$month_c <- month(data$created)
data$week <- week(data$created)
data$wday_c <- wday(data$created)
data$hour_c <- hour(data$created)
data$mday_c <- mday(data$created)




#str(data[, -which(colnames(data) == 'description')])

data1 <- data
data1$street_address <- unlist(sapply(data1$street_address, tolower))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("\\.", ' ',x)))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("-", '',x)))
data1$street_address <- unlist(sapply(data1$street_address, function(x) gsub("^[a-z]+ ", '123 ',x)))



#res <- strapplyc(gsub("^[0-9]+ ", "",data1$street_address), "(.*) avenue", simplify = TRUE)
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
data1$avenue_num <- gsub("st|nd|rd|th", "",data1$avenue_num)

res <- unlist(sapply(data1$avenue_num, function (x) {ifelse(length(tail(unlist(strsplit(x, " ")), 1)) >0,tail(unlist(strsplit(x, " ")), 1), -1)}))
data1$avenue_num <- res
street <- data1[, c("street_address","avenue_num")]
hold <- group_by(street, avenue_num)
hold <- summarise(hold, ave_num = n())
street <- left_join(street, hold)
# street$avenue_num[street$ave_num <= 10] <- -1
street$avenue_num[street$ave_num == 1] <- -1
data$avenue_num <- street$avenue_num
rm(street)
gc()

#unique(data$avenue_num[order(data$avenue_num)])
#length(head(strsplit(data1$avenue_num, " "), 20))
#head(data1$avenue_num, 20)

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

data$bathrooms[data$bathrooms >= 10] <- as.numeric(unlist(lapply(strsplit(as.character(round(data$bathrooms[data$bathrooms >= 10],0)),""),max)))

hold <- group_by(data, building_id)
hold <- summarise(hold, build_num = n())
hold$beds_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bedrooms"], na.rm = TRUE)})
hold$baths_mean <- sapply(hold$building_id, function(x){mean(data[data$building_id == x,"bathrooms"], na.rm = TRUE)})
data <- left_join(data, hold)
data$building_id[data$build_num == 1] <- -1

data$bathrooms[data$bathrooms > 3] <- 3
data$bedrooms[data$bedrooms > 5] <- 5
data$price_log <- log(data$price+1)

data$rooms_dev <- data$bedrooms - data$beds_mean
data$baths_dev <- data$bathrooms - data$baths_mean

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

data$lat_center <- (data$lat - 40.7831)/sd(data$lat)
data$lon_center <- (data$lon - (-73.9712))/sd(data$lon)



data$low <- as.integer(data$interest_level == 'low')
data$medium <- as.integer(data$interest_level == 'medium')
data$high <- as.integer(data$interest_level == 'high')



data$low_prob <- mean(data$low[data$train == TRUE], na.rm = TRUE)
data$medium_prob <- mean(data$medium[data$train == TRUE], na.rm = TRUE)
data$high_prob <- mean(data$high[data$train == TRUE], na.rm = TRUE)

#head(data$building_id)

train <- data[data$train == TRUE,]
test <- data[data$train == FALSE,]



#head(data$building_id)

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
        hold <- summarise_(hold,mean = interp(~mean(var), var = as.name(col_out)), 
                           sum = interp(~sum(var), var = as.name(col_out)),
                           count = interp(~length(var), var = as.name(col_out)))
        #print(head(hold, 20))
        #trs1 <- merge(trs, hold, by = col_group, all.x=TRUE, sort=FALSE)
        trs1 <- left_join(trs, hold)
        #print(head(trs1, 20))
        trs1$mean[is.na(trs1$mean)] <- trs1[is.na(trs1$mean),paste0(col_out, '_prob')]
        trs1$sum[is.na(trs1$sum)] <- 0
        trs1$count[is.na(trs1$count)] <- 0
        if(!is.null(lambda)){
            trs1$beta <- lambda

        } else{
            trs1$beta <- (1 / (g + exp((trs1$count - k) / f)))
        }
        trs1$wp_avg <- (1 - trs1$beta) * trs1$mean + trs1$beta * trs1[,paste0(col_out, '_prob')]
        print(head(trs1, 20))
        trs1$wp_avg[is.na(trs1$wp_avg)] <- trs1[is.na(trs1$wp_avg), paste0(col_out, '_prob')]
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
    ts$mean[is.na(ts$mean)] <- ts[is.na(ts$mean),paste0(col_out, '_prob')]
    ts$sum[is.na(ts$sum)] <- 0
    ts$count[is.na(ts$count)] <- 0
    if(!is.null(lambda)){
        ts$beta <- lambda
    } else{
        ts$beta <- 1 / (g + exp((ts$count - k) / f))
    }

    ts$wp_avg <- (1 - ts$beta) * ts$mean + ts$beta * ts[,paste0(col_out, '_prob')]
    ts$wp_avg[is.na(ts$wp_avg)] <- ts[is.na(ts$wp_avg), paste0(col_out, '_prob')]
    print(length(c(vec_train, ts$wp_avg)))

    return(c(vec_train$vec_avg, ts$wp_avg))
 
}



#data <- data[order(data$train, decreasing = TRUE),]

set.seed(1103)
#set.seed(321)
cv1 <- createFolds(train$interest_level, k = 5, list = TRUE, returnTrain = FALSE)

head(data)

#data$wei_build_low <-weight_prior_mean(data[, c('building_id', 'low', 'train', 'low_prob')], 'building_id', 'low', k = 5, f= 1, cv = cv1)
data$wei_build_medium <-weight_prior_mean(data[, c('building_id', 'medium', 'train', 'medium_prob')], 'building_id', 'medium', k = 5, f= 1, cv = cv1)
data$wei_build_high <-weight_prior_mean(data[, c('building_id', 'high', 'train', 'high_prob')], 'building_id', 'high', k = 5, f= 1, cv = cv1)

#data$wei_manager_low <-weight_prior_mean(data[, c('manager_id', 'low', 'train', 'low_prob')], 'manager_id', 'low', k = 5, f= 1, cv = cv1)
data$wei_manager_medium <-weight_prior_mean(data[, c('manager_id', 'medium', 'train', 'medium_prob')], 'manager_id', 'medium', k = 5, f= 1, cv = cv1)
data$wei_manager_high <-weight_prior_mean(data[, c('manager_id', 'high', 'train', 'high_prob')], 'manager_id', 'high', k = 5, f= 1, cv = cv1)


data$wei_avenue_medium <-weight_prior_mean(data[, c('avenue_num', 'medium', 'train', 'medium_prob')], 'avenue_num', 'medium', k = 10, f= 1, cv = cv1)
data$wei_avenue_high <-weight_prior_mean(data[, c('avenue_num', 'high', 'train', 'high_prob')], 'avenue_num', 'high', k = 10, f= 1, cv = cv1)

data$wei_display_medium <-weight_prior_mean(data[, c('display_address', 'medium', 'train', 'medium_prob')], 'display_address', 'medium', k = 5, f= 1, cv = cv1)
data$wei_display_high <-weight_prior_mean(data[, c('display_address', 'high', 'train', 'high_prob')], 'display_address', 'high', k = 5, f= 1, cv = cv1)


data$manager_id <- as.integer(as.factor(data$manager_id))
data$building_id <- as.integer(as.factor(data$building_id))
data$street_address <- as.integer(as.factor(data$street_address))
data$display_address <- as.integer(as.factor(data$display_address))
data$avenue_num <- as.integer(as.factor(data$avenue_num))
data$description_num <- str_count(data$description)


load("./files/data_features.Rda")
load("./files/data_photos.Rda")

head(features)
res <- unlist(lapply(features$features, length))
data$features_num <- res


res <- unlist(lapply(photos$photos, length))
data$photos_num <- res

data$upper_num <- sapply(regmatches(features$features, gregexpr("[A-Z]", features$features, perl=TRUE)), length)

feat <- data.frame(listing_id = unlist(rep(features$listing_id, sapply(features$features, length))), features = unlist(features$features))
#feat$features[nchar(as.character(feat$features)) == 1] <- '1'
feat$features <- str_split(feat$features, "\\*")
feat <- data.frame(listing_id = unlist(rep(feat$listing_id, sapply(feat$features, length))), features = unlist(feat$features))
feat$features <- str_split(feat$features, "\\.")
feat <- data.frame(listing_id = unlist(rep(feat$listing_id, sapply(feat$features, length))), features = unlist(feat$features))

feat$features <- trimws(tolower(feat$features))
feat$features = gsub("porter", "doorman", feat$features)
feat$features = gsub("full time super", "full time doorman", feat$features)
feat$features = gsub("full reno", "full renovated", feat$features)

feat$features = gsub("twenty four hour", "24", feat$features)
feat$features = gsub("24/7", "24", feat$features)
feat$features = gsub("24hr", "24", feat$features)
feat$features = gsub("24-hour", "24", feat$features)
feat$features = gsub("24hour", "24", feat$features)
feat$features = gsub("24 hour", "24", feat$features)
feat$features = gsub("ft ", "24 ", feat$features)
feat$features = gsub("full time", "24", feat$features)

feat$features <- gsub(" |-|/|&", "_", feat$features)

feat$features <- gsub("'", "", feat$features)
feat$features <- gsub("and", "\\+", feat$features)
feat$features <- gsub("!+", "", feat$features)
feat$features <- gsub("\\(s\\)", "", feat$features)
feat$features <- paste0("feature_",trimws(tolower(feat$features)))
feat$features <- gsub("_+", "_", feat$features)
feat$features <- gsub("_a_c", "_ac", feat$features)
hold <- group_by(feat, features)
hold <- summarise(hold, features_num = n())
#feat <- merge(feat, hold, by= "features", all.x=TRUE, sort=FALSE)
feat <- left_join(feat, hold)
feat <- feat[feat$features_num >= 10,]
unique(feat$features)[order(unique(feat$features))]

feat1 <- dcast(feat, listing_id ~ features, fun.aggregate = function(x) as.integer(length(x) > 0), value.var = "features")

data <- merge(data, feat1, by= "listing_id", all.x=TRUE, sort=FALSE)
data$listing_id <- as.numeric(data$listing_id)

#data <- cbind(data, df_q1)
# sentiment <- get_nrc_sentiment(unlist(data$description))
# dim(sentiment)
# colnames(sentiment) <- paste0("sen_", colnames(sentiment))

data <- cbind(data,sentiment)

for(i in 1:ncol(data)){
    if(sum(is.na(data[,i]))>0){
        data[is.na(data[,i]), i] <- -1
    }

}

train <- data[data$train == TRUE,]
test <- data[data$train == FALSE,]

save(train, file = "./files/train_all.Rda")

# trainIndex <- createDataPartition(train$interest_level, p=0.85, list=FALSE)
# train_1 <- train[trainIndex,]
# eval <- train[-trainIndex,]

#save(train_1, file = "./data science/files/train.Rda")
#save(eval, file = "./data science/files/eval.Rda")
save(test, file = "./files/test.Rda")
