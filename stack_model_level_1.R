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
        
        # Level 1 predictions
        
        print(2)
        param<-list(
            booster="gbtree",
            objective="multi:softprob",
            eval_metric="mlogloss",
            nthread=13,
            num_class=3,
            eta = .02,
            gamma = 1,
            max_depth = 6,
            min_child_weight = 1,
            subsample = .7,
            colsample_bytree = .5
        )

        fit <-
            xgb.train(params = param,
                      data = xgb_train,
                      nrounds = 2000,
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



#Level 1 Model - xgboost 
##############################################################################################################

load("./files/gboost_set_0.Rda")
load("./files/gboost_set_1.Rda")
load("./files/gboost_set_2.Rda")
load("./files/gboost_set_3.Rda")
load("./files/rf_set.Rda")
load("./files/svm_set.Rda")
load("./files/svm_set_1.Rda")

xgboost_yhat_ID <- do.call(c,lapply(xgboost_set,function(x){x$predictions$ID}))
xgboost_yhat_v1 <- do.call(c,lapply(xgboost_set,function(x){x$predictions$yhat.V1}))
xgboost_yhat_v2 <- do.call(c,lapply(xgboost_set,function(x){x$predictions$yhat.V2}))
xgboost_yhat_v3 <- do.call(c,lapply(xgboost_set,function(x){x$predictions$yhat.V3}))
xgboost_yhat_2_v1 <- do.call(c,lapply(xgboost_set_2,function(x){x$predictions$yhat.V1}))
xgboost_yhat_2_v2 <- do.call(c,lapply(xgboost_set_2,function(x){x$predictions$yhat.V2}))
xgboost_yhat_2_v3 <- do.call(c,lapply(xgboost_set_2,function(x){x$predictions$yhat.V3}))
xgboost_yhat_3_v1 <- do.call(c,lapply(xgboost_set_3,function(x){x$predictions$yhat.V1}))
xgboost_yhat_3_v2 <- do.call(c,lapply(xgboost_set_3,function(x){x$predictions$yhat.V2}))
xgboost_yhat_3_v3 <- do.call(c,lapply(xgboost_set_3,function(x){x$predictions$yhat.V3}))

xgboost_yhat_0_v1 <- do.call(c,lapply(xgboost_set_0,function(x){x$predictions$yhat.V1}))
xgboost_yhat_0_v2 <- do.call(c,lapply(xgboost_set_0,function(x){x$predictions$yhat.V2}))
xgboost_yhat_0_v3 <- do.call(c,lapply(xgboost_set_0,function(x){x$predictions$yhat.V3}))

rf_yhat_v1 <- do.call(c,lapply(rf_set,function(x){x$predictions$yhat.low}))
rf_yhat_v2 <- do.call(c,lapply(rf_set,function(x){x$predictions$yhat.medium}))
rf_yhat_v3 <- do.call(c,lapply(rf_set,function(x){x$predictions$yhat.high}))

gbm_yhat_v1 <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat.low}))
gbm_yhat_v2 <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat.medium}))
gbm_yhat_v3 <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat.high}))

gbm_yhat_1_v1 <- do.call(c,lapply(gbm_set_1,function(x){x$predictions$yhat.low}))
gbm_yhat_1_v2 <- do.call(c,lapply(gbm_set_1,function(x){x$predictions$yhat.medium}))
gbm_yhat_1_v3 <- do.call(c,lapply(gbm_set_1,function(x){x$predictions$yhat.high}))

xgboost_yhat_id <- apply(do.call(cbind,lapply(xgboost_set,function(x){x$test_predictions$ID})),1,mean)

xgboost_yhat_low <- apply(do.call(cbind,lapply(xgboost_set,function(x){x$test_predictions$yhat.low})),1,mean)
xgboost_yhat_medium <- apply(do.call(cbind,lapply(xgboost_set,function(x){x$test_predictions$yhat.medium})),1,mean)
xgboost_yhat_high <- apply(do.call(cbind,lapply(xgboost_set,function(x){x$test_predictions$yhat.high})),1,mean)

xgboost_yhat_v2_low <- apply(do.call(cbind,lapply(xgboost_set_2,function(x){x$test_predictions$yhat.low})),1,mean)
xgboost_yhat_v2_medium <- apply(do.call(cbind,lapply(xgboost_set_2,function(x){x$test_predictions$yhat.medium})),1,mean)
xgboost_yhat_v2_high <- apply(do.call(cbind,lapply(xgboost_set_2,function(x){x$test_predictions$yhat.high})),1,mean)

xgboost_yhat_v3_low <- apply(do.call(cbind,lapply(xgboost_set_3,function(x){x$test_predictions$yhat.low})),1,mean)
xgboost_yhat_v3_medium <- apply(do.call(cbind,lapply(xgboost_set_3,function(x){x$test_predictions$yhat.medium})),1,mean)
xgboost_yhat_v3_high <- apply(do.call(cbind,lapply(xgboost_set_3,function(x){x$test_predictions$yhat.high})),1,mean)

xgboost_yhat_v0_low <- apply(do.call(cbind,lapply(xgboost_set_0,function(x){x$test_predictions$yhat.low})),1,mean)
xgboost_yhat_v0_medium <- apply(do.call(cbind,lapply(xgboost_set_0,function(x){x$test_predictions$yhat.medium})),1,mean)
xgboost_yhat_v0_high <- apply(do.call(cbind,lapply(xgboost_set_0,function(x){x$test_predictions$yhat.high})),1,mean)

xgboost_yhat_v4_low <- apply(do.call(cbind,lapply(xgboost_set_4,function(x){x$test_predictions$yhat.low})),1,mean)
xgboost_yhat_v4_medium <- apply(do.call(cbind,lapply(xgboost_set_4,function(x){x$test_predictions$yhat.medium})),1,mean)
xgboost_yhat_v4_high <- apply(do.call(cbind,lapply(xgboost_set_4,function(x){x$test_predictions$yhat.high})),1,mean)

rf_yhat_v3_low <- apply(do.call(cbind,lapply(rf_set,function(x){x$test_predictions$yhat.low})),1,mean)
rf_yhat_v3_medium <- apply(do.call(cbind,lapply(rf_set,function(x){x$test_predictions$yhat.medium})),1,mean)
rf_yhat_v3_high <- apply(do.call(cbind,lapply(rf_set,function(x){x$test_predictions$yhat.high})),1,mean)

gbm_yhat_v3_low <- apply(do.call(cbind,lapply(gbm_set,function(x){x$test_predictions$yhat.low})),1,mean)
gbm_yhat_v3_medium <- apply(do.call(cbind,lapply(gbm_set,function(x){x$test_predictions$yhat.medium})),1,mean)
gbm_yhat_v3_high <- apply(do.call(cbind,lapply(gbm_set,function(x){x$test_predictions$yhat.high})),1,mean)

gbm_yhat_v1_low <- apply(do.call(cbind,lapply(gbm_set_1,function(x){x$test_predictions$yhat.low})),1,mean)
gbm_yhat_v1_medium <- apply(do.call(cbind,lapply(gbm_set_1,function(x){x$test_predictions$yhat.medium})),1,mean)
gbm_yhat_v1_high <- apply(do.call(cbind,lapply(gbm_set_1,function(x){x$test_predictions$yhat.high})),1,mean)


rm(xgboost_set, xgboost_set_2, xgboost_set_3,xgboost_set_0, rf_set, gbm_set, gbm_set_1)
gc()

load("./files/knn_set.Rda")
load("./files/knn_set1.Rda")
load("./files/knn_set2.Rda")


knn_yhat_v1 <- do.call(c,lapply(knn_set,function(x){x$predictions$yhat.low}))
knn_yhat_v2 <- do.call(c,lapply(knn_set,function(x){x$predictions$yhat.medium}))
knn_yhat_v3 <- do.call(c,lapply(knn_set,function(x){x$predictions$yhat.high}))

knn_yhat_2_v1 <- do.call(c,lapply(knn_set1,function(x){x$predictions$yhat.low}))
knn_yhat_2_v2 <- do.call(c,lapply(knn_set1,function(x){x$predictions$yhat.medium}))
knn_yhat_2_v3 <- do.call(c,lapply(knn_set1,function(x){x$predictions$yhat.high}))

knn_yhat_3_v1 <- do.call(c,lapply(knn_set2,function(x){x$predictions$yhat.low}))
knn_yhat_3_v2 <- do.call(c,lapply(knn_set2,function(x){x$predictions$yhat.medium}))
knn_yhat_3_v3 <- do.call(c,lapply(knn_set2,function(x){x$predictions$yhat.high}))

knn_yhat_low <- apply(do.call(cbind,lapply(knn_set,function(x){x$test_predictions$yhat.low})),1,mean)
knn_yhat_medium <- apply(do.call(cbind,lapply(knn_set,function(x){x$test_predictions$yhat.medium})),1,mean)
knn_yhat_high <- apply(do.call(cbind,lapply(knn_set,function(x){x$test_predictions$yhat.high})),1,mean)

knn_yhat_2_low <- apply(do.call(cbind,lapply(knn_set1,function(x){x$test_predictions$yhat.low})),1,mean)
knn_yhat_2_medium <- apply(do.call(cbind,lapply(knn_set1,function(x){x$test_predictions$yhat.medium})),1,mean)
knn_yhat_2_high <- apply(do.call(cbind,lapply(knn_set1,function(x){x$test_predictions$yhat.high})),1,mean)

knn_yhat_3_low <- apply(do.call(cbind,lapply(knn_set2,function(x){x$test_predictions$yhat.low})),1,mean)
knn_yhat_3_medium <- apply(do.call(cbind,lapply(knn_set2,function(x){x$test_predictions$yhat.medium})),1,mean)
knn_yhat_3_high <- apply(do.call(cbind,lapply(knn_set2,function(x){x$test_predictions$yhat.high})),1,mean)

rm(knn_set, knn_set1, knn_set2)
gc()




data_l1 <- data.frame(listing_id = xgboost_yhat_ID,
                       xgboost1_1 = xgboost_yhat_v1,xgboost1_2 = xgboost_yhat_v2,xgboost1_3 = xgboost_yhat_v3
                      ,xgboost2_1 = xgboost_yhat_2_v1, xgboost2_2 = xgboost_yhat_2_v2, xgboost2_3 = xgboost_yhat_2_v3
                      ,xgboost3_1 = xgboost_yhat_3_v1, xgboost3_2 = xgboost_yhat_3_v2, xgboost3_3 = xgboost_yhat_3_v3
                      ,xgboost0_1 = xgboost_yhat_0_v1, xgboost0_2 = xgboost_yhat_0_v2, xgboost0_3 = xgboost_yhat_0_v3
                      ,knn_1 = knn_yhat_v1, knn_2 = knn_yhat_v2, knn_3 = knn_yhat_v3
                      ,knn_2_1 = knn_yhat_2_v1, knn_2_2 = knn_yhat_2_v2, knn_2_3 = knn_yhat_2_v3
                      ,knn_3_1 = knn_yhat_3_v1, knn_3_2 = knn_yhat_3_v2, knn_3_3 = knn_yhat_3_v3
                      ,rf_1 = rf_yhat_v1, rf_2 = rf_yhat_v2, rf_3 = rf_yhat_v3
                      ,gbm_low = gbm_yhat_v1, gbm_medium = gbm_yhat_v2, gbm_high = gbm_yhat_v3
                      ,gbm_low_1 = gbm_yhat_1_v1, gbm_medium_1 = gbm_yhat_1_v2, gbm_high = gbm_yhat_1_v3
                      )

data_l1 <- left_join(train[, c("listing_id", "interest_level")],data_l1)


test_l1 <- data.frame(listing_id = xgboost_yhat_id,
                       xgboost1_1 = xgboost_yhat_low,xgboost1_2 = xgboost_yhat_medium,xgboost1_3 = xgboost_yhat_high
                      ,xgboost2_1 = xgboost_yhat_v2_low, xgboost2_2 = xgboost_yhat_v2_medium, xgboost2_3 = xgboost_yhat_v2_high
                      ,xgboost3_1 = xgboost_yhat_v3_low, xgboost3_2 = xgboost_yhat_v3_medium, xgboost3_3 = xgboost_yhat_v3_high
                      ,xgboost0_1 = xgboost_yhat_v0_low, xgboost0_2 = xgboost_yhat_v0_medium, xgboost0_3 = xgboost_yhat_v0_high
                      ,xgboost4_1 = xgboost_yhat_v4_low, xgboost4_2 = xgboost_yhat_v4_medium, xgboost4_3 = xgboost_yhat_v4_high
                      ,knn_1 = knn_yhat_low, knn_2 = knn_yhat_medium, knn_3 = knn_yhat_high
                      ,knn_2_1 = knn_yhat_2_low, knn_2_2 = knn_yhat_2_medium, knn_2_3 = knn_yhat_2_high
                      ,knn_3_1 = knn_yhat_3_low, knn_3_2 = knn_yhat_3_medium, knn_3_3 = knn_yhat_3_high
                      ,rf_1 = rf_yhat_v3_low, rf_2 = rf_yhat_v3_medium, rf_3 = rf_yhat_v3_high
                      ,gbm_low = gbm_yhat_v3_low, gbm_medium = gbm_yhat_v3_medium, gbm_high = gbm_yhat_v3_high
                      ,gbm_low_1 = gbm_yhat_v1_low, gbm_medium_1 = gbm_yhat_v1_medium, gbm_high_1 = gbm_yhat_v1_high
                      )



train_x <- data_l1[, !(names(data_l1) %in% c("listing_id", "interest_level"))]
test_x <- test_l1[, !(names(test_l1) %in% c("listing_id"))]


data_l1$interest_level <- factor(data_l1$interest_level, levels = c("low", "medium", "high"))
levels(data_l1$interest_level) <- c(1, 2, 3)

y_train <-as.numeric(data_l1$interest_level)


labels <- y_train-1
xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=labels, missing=NaN)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_x[]), missing=NaN)

n_cl <- length(levels(y_train))-1

length(levels(y_train))

set.seed(123)
history <- xgb.cv(data = xgb_train, nround=1000, nthread = 13, nfold = 10, subsample = .7,
                  max.depth =4, eta = 0.01, gamma = 1, colsample_bytree = 0.7, num_class = 3,min_child_weight = 1,
                  objective = "multi:softprob", eval_metric = "mlogloss",booster = "gbtree")

set.seed(123)
param <- list(booster="gbtree",
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

xgb2 <- xgb.train(data = xgb_train,
                  params = param,
                  # watchlist=watch,
                  # nrounds = xgb2cv$best_ntreelimit
                  nrounds = 1000
)

sPreds <- as.data.frame(t(matrix(predict(xgb2, xgb_test), nrow=3, ncol=nrow(xgb_test))))
colnames(sPreds) <- c("low", "medium", "high")
submission3 <- data.frame(listing_id = test$listing_id, sPreds)
write.csv(submission3, "./sub3.csv", row.names=FALSE)

