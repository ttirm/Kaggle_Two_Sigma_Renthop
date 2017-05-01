load("./files/data_features.Rda")

features <- features[features$interest_level == "high",]

feat <- data.frame(listing_id = unlist(rep(features$listing_id, sapply(features$features, length))),
                   features = unlist(features$features))
feat$features <- str_split(feat$features, " \\* ")
feat <- data.frame(listing_id = unlist(rep(feat$listing_id, sapply(feat$features, length))),
                   features = unlist(feat$features))


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

hold <- group_by(feat, features)
hold <- summarise(hold, features_num = n())
#feat <- merge(feat, hold, by= "features", all.x=TRUE, sort=FALSE)
feat <- left_join(feat, hold)

feat <- feat[feat$features_num >= 3 & !grepl("^feature_$",feat$features),]
high <- unique(feat$features)

load("./files/data_features.Rda")

features <- features[features$interest_level == "medium",]

feat <- data.frame(listing_id = unlist(rep(features$listing_id, sapply(features$features, length))),
                   features = unlist(features$features))
feat$features <- str_split(feat$features, " \\* ")
feat <- data.frame(listing_id = unlist(rep(feat$listing_id, sapply(feat$features, length))),
                   features = unlist(feat$features))


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

hold <- group_by(feat, features)
hold <- summarise(hold, features_num = n())
#feat <- merge(feat, hold, by= "features", all.x=TRUE, sort=FALSE)
feat <- left_join(feat, hold)

feat <- feat[feat$features_num >= 5 & !grepl("^feature_$",feat$features),]
medium <- unique(feat$features)

feat_hm <- unique(c(high, medium))
save(feat_hm, file = "./files/feat_hm.Rda")
