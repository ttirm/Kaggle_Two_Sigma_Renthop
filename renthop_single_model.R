library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggmap)
library(RJSONIO)
library(lubridate)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)

source("./utils.R")

setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_2sigma_rent")
load("./files/train_all.Rda")
load("./files/test.Rda")

model_columns <- c("listing_id", 
                   "listing_diver",
                   "bathrooms","bed_bath_ratio", "bedrooms","price",
                   "week","wday_c", "mday_c", "month_c",
                   "hour_c",
                   "similarity", "droped",
                   "house", 
                   #"time_stamp",
                   #"created_num",
                   #"manhattan",
                   #"avenue_num",
                   #avenue,
                   "lat_center","lon_center",
                   "lat_median","lng_median",
                   "neighbours_100", 
                   "density",
                   "neighbours",
                   #"cluster_managers",
                   "lat", "lon",
                   #"perc_5", "perc_7", "perc_9",
                   #"wei_build_low","wei_build_medium", "wei_build_high",
                   "wei_build_low_1","wei_build_medium_1", "wei_build_high_1",
                   #"wei_manager_medium", "wei_manager_high",
                   #"wei_manager_low", "wei_manager_medium", "wei_manager_high",
                   "wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
                   "wei_house_low_1", "wei_house_medium_1", "wei_house_high_1",
                   #"wei_manager_low_scale", "wei_manager_medium_scale", "wei_manager_high_scale",
                   #"wei_build_low_scale", "wei_build_medium_scale", "wei_build_high_scale",
                   #"wei_manager_low_medium", "wei_manager_medium_high", "wei_manager_high_low",
                   #"wei_street_high","wei_street_medium",
                   #"manager_quantile", "building_quantile", "rating",
                   "manager_skill_hi", "manager_skill_medium", "manager_skill_low",
                   "wei_avenue_medium", "wei_avenue_high",
                   #"wei_display_medium", "wei_display_high",
                   "wei_clust_low","wei_clust_medium", "wei_clust_high",
                   "manager_num", "sum_manager_buildings",
                   "bed_plus_bath", "bed_minus_bath",
                   "half_bathrooms", "price_clust",
                   #"rooms_dev", "baths_dev", "price_dev",
                   #"rooms_median", "baths_median",
                   "price_log", "upper_num", 
                   "top1", "top99",
                   #"numbers",
                   #"street_price_dev", 
                   #"price_clust",
                   "duplicated",
                   #"ft_mean_length", 
                   #"manager_ranked",
                   "bed_rat", "bath_rat", "price_bed", "price_bath", "price_bb",
                   #"mean_neighbourhood_ppb",
                   #"beds_median", "baths_median",
                   "manager_id", "building_id", "street_address", "display_address", 
                   "description_exclamation", "exclamation_first", "exlamation_ratio",
                   "description_br", "description_redacted", "description_at", "description_phone",
                   "manhattan_dist", "brooklyn_dist", "bronx_dist", "queens_dist", "staten_dist",
                   "f_dogs", "f_cats", "f_nofee", "f_lowfee", "f_excl", "f_wood", "f_door", "f_prewar", "f_laundry", "f_fit", "f_wood", "f_park", "f_transp", "f_util", "f_elev",
                   "features_num", "photos_num", "description_num")



train_x <- train[, c(model_columns,colnames(train)[grepl("feature_", colnames(train))])]
test_x <- test[, c(model_columns,colnames(test)[grepl("feature_",colnames(test))])]

# train_x <- train[, c(model_columns,colnames(train)[grepl("feature_", colnames(train))], colnames(train)[grepl("sen_",colnames(train))])]
# test_x <- test[, c(model_columns,colnames(test)[grepl("feature_",colnames(test))], colnames(test)[grepl("sen_",colnames(test))])]



train$interest_level <- factor(train$interest_level, levels = c("low", "medium", "high"))
levels(train$interest_level) <- c(1, 2, 3)

y_train <-as.numeric(train$interest_level)


labels <- y_train-1
xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=labels, missing=NaN)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_x), missing=NaN)

n_cl <- length(levels(y_train))-1

length(levels(y_train))

set.seed(123)
history <- xgb.cv(data = xgb_train, nround=400, nthread = 13, nfold = 10, subsample = .7,
                  max.depth =6, eta = 0.1, gamma = 1, colsample_bytree = 0.7, num_class = 3,min_child_weight = 1,
                  objective = "multi:softprob", eval_metric = "mlogloss",booster = "gbtree")

 # history <- xgb.cv(data = xgb_train, nround=3100, nthread = 13, nfold = 5, subsample = .7,
 #                   max.depth =7, eta = 0.01, gamma = 1, colsample_bytree = 0.7, num_class = 3,min_child_weight = 1,
 #                   objective = "multi:softprob", eval_metric = "mlogloss",booster = "gbtree")

 set.seed(123)
 param <- list(booster="gbtree",
               objective="multi:softprob",
               eval_metric="mlogloss",
               nthread=13,
               num_class=3,
               eta = .02,
               gamma = 1,
               max_depth = 6,
               min_child_weight = 1,
               subsample = .7,
               colsample_bytree = .7
 )
 
 xgb2 <- xgb.train(data = xgb_train,
                   params = param,
                   # watchlist=watch,
                   # nrounds = xgb2cv$best_ntreelimit
                   nrounds = 2000
 )
 
 sPreds <- as.data.frame(t(matrix(predict(xgb2, xgb_test), nrow=3, ncol=nrow(xgb_test))))
 colnames(sPreds) <- c("low", "medium", "high")
 submission <- data.frame(listing_id = test$listing_id, sPreds)
 write.csv(submission, "./sub.csv", row.names=FALSE)
 
