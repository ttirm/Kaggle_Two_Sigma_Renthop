library(MLmetrics)
library(dplyr)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)

source("./utils.R")

# setwd("C:/Kaggle_2sigma_rent")
load("./files/train_all.Rda")
load("./files/test.Rda")

train_folds <- function(fold, dataset, model, cols, outcome, ids, test_tot){
    
    dataset$interest_level <- factor(dataset$interest_level, levels = c("low", "medium", "high"))
    if (model == "xgb"){
        levels(dataset$interest_level) <- c(1, 2, 3)
    }
    
    
    testset <- dataset[fold, (names(dataset) %in% cols) ]
    y <- dataset[fold, outcome]
    id <- dataset[fold, ids]

    trainset <- dataset[-fold, (names(dataset) %in% cols)]
    trainout <- dataset[-fold, outcome]
    testid <- test_tot[,ids]
    rm(dataset)
    gc()
    print(1)
    if (model == "xgb"){
        
        y <- as.numeric(y)
        trainout <- as.numeric(trainout)
        
        labels <- trainout-1
        
        train_x <- trainset

        y_train <-  trainout

        xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                                 label=labels, missing=NA)
        

        # Base level predictions
        print(2)
        param<-list(
            booster="gbtree",
            objective="multi:softprob",
            eval_metric="mlogloss",
            nthread=13,
            num_class=3,
            eta = .01,
            gamma = 1,
            max_depth = 4,
            min_child_weight = 1,
            subsample = .7,
            colsample_bytree = .7
        )

        fit <-
            xgb.train(params = param,
                      data = xgb_train,
                      nrounds = 1000,
                      watchlist = list(train = xgb_train),
                      verbose = FALSE,
                      print_every_n = 50)
        
        print(3)
        xgb_test_cv <- xgb.DMatrix(model.matrix(~., data = testset),
                                   missing=NA)
        
        yhat <- as.data.frame(t(matrix(predict(fit, newdata=xgb_test_cv), nrow=3, ncol=nrow(xgb_test_cv))))
        #colnames(yhat)
        #score <- MultiLogLoss(y,yhat)
        score <- 0
        print(4)
        xgb_test <- xgb.DMatrix(model.matrix(~., data = test_tot[,(names(test_tot) %in% cols)]),
                                missing=NA)
        
        test_yhat <- as.data.frame(t(matrix(predict(fit, newdata=xgb_test), nrow=3, ncol=nrow(xgb_test))))
        colnames(test_yhat) <- c("low", "medium", "high")
    }else if (model == "caret"){
        
        trainset$interest_level <- trainout
        
        print(2)
        set.seed(825)
        fit <- train(interest_level ~ .,
                     data = trainset,
                     method=met,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=CARET.TUNE.GRID,
                     preProcess= pre,
                     metric="logLoss",
                     #                      tuneLength=tun,
                     #                      linout=lin,
                     #                      trace=tra,
                     maximize=FALSE)
        
        print(3)
        yhat <- predict(fit,newdata = testset,type = "prob")
        print(4)
        test_yhat <- predict(fit,newdata = test_tot[, !(names(test_tot) %in% c("interest_level"))],type = "prob")
        
        
        #score <- MultiLogLoss(y,yhat)
        score <- 0
        #print(score)
        
    }else if(model == 'rf'){
        
        train_x <- trainset[, c(model_columns,colnames(train)[grepl("feature_", colnames(train))], "interest_level")]
        train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))
        
        fit <- randomForest(interest_level ~ .,
                            data = train_x)
        
        print(3)
        yhat <- predict(fit,newdata = testset,type = 'prob')
        print(4)
        test_yhat <- predict(fit,newdata = test_tot[, -1],type = 'prob')

        
        x = data.frame(matrix(nrow = nrow(testset), ncol = 0))
        x$obs = factor(validation$interest_level)
        y = predict(fit, validation,type = 'prob')
        x = cbind(x,y)
        mnLogLoss(x, lev = x$obs)

        
    }
    print(5)
    ans <- list(model=fit,
                score=score,
                predictions=data.frame(ID=id,yhat=yhat,y=y),
                test_predictions=data.frame(ID=testid,yhat=test_yhat))
    
    return(ans)
    
}

set.seed(29)
folds <- createFolds(train$interest_level, k=5)
folds


#xgboost Model - 0
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
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

cols <- c(cols,colnames(train)[grepl("feature_", colnames(train))])

xgboost_set_0 <- lapply(folds, train_folds, train, "xgb", cols, "interest_level", "listing_id", test)

save(xgboost_set_0, file = "./files/gboost_set_0.Rda")

#xgboost Model - 1
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
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
          #"wei_malnager_low_medium", "wei_manager_medium_high", "wei_manager_high_low",
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
          "features_num", "photos_num", "description_num")

cols <- c(cols,colnames(train)[grepl("feature_", colnames(train))])

xgboost_set <- lapply(folds, train_folds, train, "xgb", cols, "interest_level", "listing_id", test)

save(xgboost_set, file = "./files/gboost_set_1.Rda")

# submission5 <- data.frame(listing_id=xgboost_set$Fold1$test_predictions$ID, low=(xgboost_set$Fold1$test_predictions$yhat.low +
#     xgboost_set$Fold2$test_predictions$yhat.low + xgboost_set$Fold3$test_predictions$yhat.low + xgboost_set$Fold4$test_predictions$yhat.low +
#         xgboost_set$Fold5$test_predictions$yhat.low)/5, medium=(xgboost_set$Fold1$test_predictions$yhat.medium +
#         xgboost_set$Fold2$test_predictions$yhat.medium + xgboost_set$Fold3$test_predictions$yhat.medium +
#             xgboost_set$Fold4$test_predictions$yhat.medium +
#             xgboost_set$Fold5$test_predictions$yhat.medium)/5,
#     high=(xgboost_set$Fold1$test_predictions$yhat.high +xgboost_set$Fold2$test_predictions$yhat.high +
#               xgboost_set$Fold3$test_predictions$yhat.high +
#                 xgboost_set$Fold4$test_predictions$yhat.high + xgboost_set$Fold5$test_predictions$yhat.high)/5)
# write.csv(submission5, "./submission5.csv", row.names=FALSE)

#xgboost Model - 2
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          #"similarity", "droped",
          #"house", 
          "time_stamp",
          #"created_num",
          #"manhattan",
          #"avenue_num",
          #avenue,
          #"lat_center","lon_center",
          "lat_median","lng_median",
          "neighbours_100", 
          "density",
          "neighbours",
          #"cluster_managers",
          "lat", "lon",
          #"perc_5", "perc_7", "perc_9",
          "wei_build_low","wei_build_medium", "wei_build_high",
          #"wei_build_low_1","wei_build_medium_1", "wei_build_high_1",
          #"wei_manager_medium", "wei_manager_high",
          "wei_manager_low", "wei_manager_medium", "wei_manager_high",
          #"wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
          "wei_house_low_1", "wei_house_medium_1", "wei_house_high_1",
          #"wei_manager_low_scale", "wei_manager_medium_scale", "wei_manager_high_scale",
          "wei_build_low_scale", "wei_build_medium_scale", "wei_build_high_scale",
          #"wei_manager_low_medium", "wei_manager_medium_high", "wei_manager_high_low",
          #"wei_street_high","wei_street_medium",
          #"manager_quantile", "building_quantile", "rating",
          #"manager_skill_hi", "manager_skill_medium", "manager_skill_low",
          #"wei_avenue_medium", "wei_avenue_high",
          #"wei_display_medium", "wei_display_high",
          "wei_clust_low","wei_clust_medium", "wei_clust_high",
          "manager_num", "sum_manager_buildings",
          "bed_plus_bath", "bed_minus_bath",
          "half_bathrooms", "price_clust",
          #"rooms_dev", "baths_dev", "price_dev",
          #"rooms_median", "baths_median",
          "price_log", "upper_num", 
          #"top1", "top99",
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
          "features_num", "photos_num", "description_num")



cols <- c(cols,colnames(train)[grepl("feature_", colnames(train))], colnames(train)[grepl("sen_",colnames(train))])

xgboost_set_2 <- lapply(folds, train_folds, train, "xgb", cols, "interest_level", "listing_id", test)

save(xgboost_set_2, file = "./files/gboost_set_2.Rda")

# submission6 <- data.frame(listing_id=xgboost_set$Fold1$test_predictions$ID, low=(xgboost_set_2$Fold1$test_predictions$yhat.low +
#                           xgboost_set_2$Fold2$test_predictions$yhat.low + xgboost_set_2$Fold3$test_predictions$yhat.low + xgboost_set_2$Fold4$test_predictions$yhat.low +
#                           xgboost_set_2$Fold5$test_predictions$yhat.low)/5, medium=(xgboost_set_2$Fold1$test_predictions$yhat.medium +
#                           xgboost_set_2$Fold2$test_predictions$yhat.medium + xgboost_set_2$Fold3$test_predictions$yhat.medium +
#                           xgboost_set_2$Fold4$test_predictions$yhat.medium +
#                           xgboost_set_2$Fold5$test_predictions$yhat.medium)/5,
#                           high=(xgboost_set_2$Fold1$test_predictions$yhat.high +xgboost_set_2$Fold2$test_predictions$yhat.high +
#                                     xgboost_set_2$Fold3$test_predictions$yhat.high +
#                                     xgboost_set_2$Fold4$test_predictions$yhat.high + xgboost_set_2$Fold5$test_predictions$yhat.high)/5)

#xgboost Model - 3
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          #"listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
          #"created_num",
          #"manhattan",
          #"avenue_num",
          #avenue,
          "lat_center","lon_center",
          "lat_median","lng_median",
          "neighbours_100", 
          #"density",
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
          "manager_quantile", "building_quantile", "rating",
          #"manager_skill_hi", "manager_skill_medium", "manager_skill_low",
          "wei_avenue_medium", "wei_avenue_high",
          #"wei_display_medium", "wei_display_high",
          "wei_clust_low","wei_clust_medium", "wei_clust_high",
          #"manager_num", "sum_manager_buildings",
          "bed_plus_bath", "bed_minus_bath",
          "half_bathrooms", "price_clust",
          #"rooms_dev", "baths_dev", "price_dev",
          #"rooms_median", "baths_median",
          "price_log", "upper_num", 
          #"top1", "top99",
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
          #"description_br", "description_redacted", "description_at", "description_phone",
          "features_num", "photos_num", "description_num")



cols <- c(cols,colnames(train)[grepl("feature_", colnames(train))])

xgboost_set_3 <- lapply(folds, train_folds, train, "xgb", cols, "interest_level", "listing_id", test)

save(xgboost_set_3, file = "./files/gboost_set_3.Rda")

# submission7 <- data.frame(listing_id=xgboost_set$Fold1$test_predictions$ID, low=(xgboost_set_3$Fold1$test_predictions$yhat.low +
#                           xgboost_set_3$Fold2$test_predictions$yhat.low + xgboost_set_3$Fold3$test_predictions$yhat.low + xgboost_set_3$Fold4$test_predictions$yhat.low +
#                           xgboost_set_3$Fold5$test_predictions$yhat.low)/5, medium=(xgboost_set_3$Fold1$test_predictions$yhat.medium +
#                           xgboost_set_3$Fold2$test_predictions$yhat.medium + xgboost_set_3$Fold3$test_predictions$yhat.medium +
#                           xgboost_set_3$Fold4$test_predictions$yhat.medium +
#                           xgboost_set_3$Fold5$test_predictions$yhat.medium)/5,
#                           high=(xgboost_set_3$Fold1$test_predictions$yhat.high +xgboost_set_3$Fold2$test_predictions$yhat.high +
#                                     xgboost_set_3$Fold3$test_predictions$yhat.high +
#                                     xgboost_set_3$Fold4$test_predictions$yhat.high + xgboost_set_3$Fold5$test_predictions$yhat.high)/5)
# 


#knn Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("bathrooms","bed_bath_ratio", "bedrooms","price",
                   "hour_c","mday_c",
                    #"time_stamp",
                   #"manhattan",
                   "neighbours_100",
                   "latitude", "longitude",
                   #"manager_quantile", "building_quantile",
                    "wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
                   "manager_num", "sum_manager_buildings",
                   "bed_plus_bath", "bed_minus_bath",
                   "price_log",
                   "bed_rat", "bath_rat", "price_bed", "price_bath", "price_bb",
                   "features_num")

train_x <- train[, c(cols,"listing_id", "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))


CARET.TUNE.GRID <- data.frame(k = 100)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)



pre <- c("center","scale")
tun <- NULL
lin <- NULL
tra <- NULL
met <- "knn"


knn_set <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(knn_set, file = "./files/knn_set.Rda")

#knn1 Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("wei_build_low","wei_build_medium", "wei_build_high",
          "wei_build_low_1","wei_build_medium_1", "wei_build_high_1",
          "wei_manager_low", "wei_manager_medium", "wei_manager_high",
          "wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
          "price","density","rating")

train_x <- train[, c(cols,"listing_id", "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))


CARET.TUNE.GRID <- data.frame(k = 100)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)



pre <- c("center","scale")
tun <- NULL
lin <- NULL
tra <- NULL
met <- "knn"


knn_set1 <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(knn_set1, file = "./files/knn_set1.Rda")


#knn2 Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("wei_build_low","wei_build_medium", "wei_build_high",
          "wei_manager_low", "wei_manager_medium", "wei_manager_high",
          "wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
          "price","density")

train_x <- train[, c(cols,"listing_id", "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))


CARET.TUNE.GRID <- data.frame(k = 70)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)



pre <- c("center","scale")
tun <- NULL
lin <- NULL
tra <- NULL
met <- "knn"


knn_set2 <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(knn_set2, file = "./files/knn_set2.Rda")


#rf 1 Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id",
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
          #"created_num",
          #"manhattan",
          "avenue_num",
          #avenue,
          #"lat_center","lon_center",
          "neighbours_100", 
          "density",
          "neighbours",
          #"cluster_managers",
          "lat", "lon",
          #"wei_build_low","wei_build_medium", "wei_build_high",
          "wei_build_low_1","wei_build_medium_1", "wei_build_high_1",
          #"wei_manager_medium", "wei_manager_high",
          #"wei_manager_low", "wei_manager_medium", "wei_manager_high",
          "wei_manager_low_1", "wei_manager_medium_1", "wei_manager_high_1",
          "wei_house_low_1", "wei_house_medium_1", "wei_house_high_1",
          #"wei_manager_low_scale", "wei_manager_medium_scale", "wei_manager_high_scale",
          #"wei_build_low_scale", "wei_build_medium_scale", "wei_build_high_scale",
          #"wei_manager_low_medium", "wei_manager_medium_high", "wei_manager_high_low",
          "wei_street_high","wei_street_medium",
          #"manager_quantile", "building_quantile", "rating",
          "manager_skill_hi", "manager_skill_medium", "manager_skill_low",
          #"wei_avenue_medium", "wei_avenue_high",
          #"wei_display_medium", "wei_display_high",
          "wei_clust_low","wei_clust_medium", "wei_clust_high",
          "manager_num", "sum_manager_buildings",
          "bed_plus_bath", "bed_minus_bath",
          "half_bathrooms", "price_clust",
          #"rooms_dev", "baths_dev", "price_dev",
          #"rooms_median", "baths_median",
          "price_log", "upper_num", 
          #"numbers",
          #"street_price_dev", 
          #"price_clust",
          "duplicated",
          #"ft_mean_length", 
          "manager_ranked",
          "bed_rat", "bath_rat", "price_bed", "price_bath", "price_bb",
          #"mean_neighbourhood_ppb",
          #"beds_median", "baths_median",
          "manager_id", "building_id", "street_address", "display_address",
          "features_num", "photos_num", "description_num")

train_x <- train[, c(cols, "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))


CARET.TUNE.GRID <-  expand.grid(.mtry=sqrt(ncol(train_x)))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)



pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "rf"


rf_set <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(rf_set, file = "./files/rf_set.Rda")

#rf 2 Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
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

train_x <- train[, c(cols, "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))


CARET.TUNE.GRID <-  expand.grid(.mtry=sqrt(ncol(train_x)))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)



pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "rf"


rf_set_1 <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(rf_set_1, file = "./files/rf_set_1.Rda")




#glmnet Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
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
          "features_num", "photos_num", "description_num")

train_x <- train[, c(cols, "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))

CARET.TUNE.GRID <-  expand.grid(interaction.depth=3, # Depth of variable interactions
                                n.trees=150,	        # Num trees to fit
                                shrinkage=0.1,		# Try 2 values for learning rate 
                                n.minobsinnode = 10)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)

pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "gbm"


gbm_set <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(gbm_set, file = "./files/glmnet_set.Rda")

#glmnet Model
##############################################################################################################

set.seed(29)
folds <- createFolds(train$interest_level, k=5)

cols <- c("listing_id", 
          "listing_diver",
          "bathrooms","bed_bath_ratio", "bedrooms","price",
          "week","wday_c", "mday_c", "month_c",
          "hour_c",
          "similarity", "droped",
          "house", 
          "time_stamp",
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

train_x <- train[, c(cols, "interest_level")]
train_x$interest_level <- factor(train_x$interest_level, levels = c("low", "medium", "high"))

CARET.TUNE.GRID <-  expand.grid(interaction.depth=3, # Depth of variable interactions
                                n.trees=150,	        # Num trees to fit
                                shrinkage=0.1,		# Try 2 values for learning rate 
                                n.minobsinnode = 10)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE,
                                 classProbs = TRUE,
                                 summaryFunction = mnLogLoss)

pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "gbm"


gbm_set_1 <- lapply(folds, train_folds, train_x, "caret", cols, "interest_level", "listing_id", test)

save(gbm_set_1, file = "./files/glmnet_set_1.Rda")
