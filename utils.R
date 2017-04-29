LogLosSummary <- function (data, lev = NULL, model = NULL) {
    LogLos <- function(actual, pred, eps = 1e-15) {
        stopifnot(all(dim(actual) == dim(pred)))
        pred[pred < eps] <- eps
        pred[pred > 1 - eps] <- 1 - eps
        -sum(actual * log(pred)) / nrow(pred) 
    }
    if (is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
    pred <- data[, "pred"]
    obs <- data[, "obs"]
    isNA <- is.na(pred)
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    data <- data[!isNA, ]
    cls <- levels(obs)
    
    if (length(obs) + length(pred) == 0) {
        out <- rep(NA, 2)
    } else {
        pred <- factor(pred, levels = levels(obs))
        require("e1071")
        out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag",                                                                                                                                                             "kappa")]
        
        probs <- data[, cls]
        actual <- model.matrix(~ obs - 1)
        out2 <- LogLos(actual = actual, pred = probs)
    }
    out <- c(out, out2)
    names(out) <- c("Accuracy", "Kappa", "LogLoss")
    
    if (any(is.nan(out))) out[is.nan(out)] <- NA 
    
    out
}


## Function that is used to compute the multi-class logloss of validation set given a model
mnLogLoss_validation = function(model){
    x = data.frame(matrix(nrow = nrow(validation), ncol = 0))
    x$obs = factor(validation$interest_level)
    y = predict(model, validation,type = 'prob')
    x = cbind(x,y)
    mnLogLoss(x, lev = c('high','low','medium'))
}

weight_points_power <- function(data, col_group, col_out, cv = cv1){
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
        
        trs1$wp_power <- trs1$sum 
        print(head(trs1, 20))
        trs1$wp_power[is.na(trs1$wp_power)] <- 0
        #trs1$wp_avg <- trs1$wp_avg*(1+(runif(nrow(trs1))-0.5)*0.01)
        vec_avg <- c(vec_avg, trs1$wp_power)
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
    
    ts$wp_power <- ts$sum
    ts$wp_power[is.na(ts$wp_power)] <- 0
    print(length(c(vec_train, ts$wp_power)))
    
    return(c(vec_train$vec_avg, ts$wp_power))
    
}

#Calculate the distance between a point and a line (1)
dist2d <- function(point,line_start, line_end) {
    v1 <- line_start - line_end
    v2 <- point - line_start
    m <- cbind(v1,v2)
    d <- abs(det(m))/sqrt(sum(v1*v1))
}


#Calculate the distance between a point and a line (based on slope and intercept)
distancePointLine <- function(x, y, slope, intercept) {
    ## x, y is the point to test.
    ## slope, intercept is the line to check distance.
    ##
    ## Returns distance from the line.
    ##
    ## Returns 9999 on 0 denominator conditions.
    x1 <- x-10
    x2 <- x+10
    y1 <- x1*slope+intercept
    y2 <- x2*slope+intercept
    distancePointSegment(x,y, x1,y1, x2,y2)
}

distancePointSegment <- function(px, py, x1, y1, x2, y2) {
    ## px,py is the point to test.
    ## x1,y1,x2,y2 is the line to check distance.
    ##
    ## Returns distance from the line, or if the intersecting point on the line nearest
    ## the point tested is outside the endpoints of the line, the distance to the
    ## nearest endpoint.
    ##
    ## Returns 9999 on 0 denominator conditions.
    lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)
    ans <- NULL
    ix <- iy <- 0   # intersecting point
    lineMag <- lineMagnitude(x1, y1, x2, y2)
    if( lineMag < 0.00000001) {
        warning("short segment")
        return(9999)
    }
    u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
    u <- u / (lineMag * lineMag)
    if((u < 0.00001) || (u > 1)) {
        ## closest point does not fall within the line segment, take the shorter distance
        ## to an endpoint
        ix <- lineMagnitude(px, py, x1, y1)
        iy <- lineMagnitude(px, py, x2, y2)
        if(ix > iy)  ans <- iy
        else ans <- ix
    } else {
        ## Intersecting point is on the line, use the formula
        ix <- x1 + u * (x2 - x1)
        iy <- y1 + u * (y2 - y1)
        ans <- lineMagnitude(px, py, ix, iy)
    }
    ans
}

distancePointLineTest <- function() {
    if(abs(distancePointSegment(  5,   5,  10, 10, 20, 20) - 7.07106781186548)>.0001)
        stop("error 1")
    if(abs(distancePointSegment( 15,  15,  10, 10, 20, 20) - 0)>.0001)
        stop("error 2")
    if(abs(distancePointSegment( 15,  15,  20, 10, 20, 20) - 5)>.0001)
        stop("error 3")
    if(abs(distancePointSegment(  0,  15,  20, 10, 20, 20) - 20)>.0001)
        stop("error 4")
    if(abs(distancePointSegment(  0,  25,  20, 10, 20, 20) - 20.6155281280883)>.0001)
        stop("error 5")
    if(abs(distancePointSegment(-13, -25, -50, 10, 20, 20) - 39.8808224589213)>.0001)
        stop("error 6")
    if(abs(distancePointSegment(  0,   3,   0, -4,  5,  0) - 5.466082)>.0001)
        stop("error 7")
    if(abs(distancePointSegment(  0,   9,   0, -4,  0, 15) - 0)>.0001)
        stop("error 8")
    if(abs(distancePointSegment(  0,   0,   0, -2,  2,  0)^2 - 2)>.0001)
        stop("error 9")
    return(TRUE)
}