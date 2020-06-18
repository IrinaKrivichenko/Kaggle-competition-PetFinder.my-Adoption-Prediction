library(tidyverse) # general utility functions
library(xgboost) # for xgboost
#library(microbenchmark )


clean <- function(ds){  
  ds <- ds %>% select(c("Description","AdoptionSpeed"))
  ds$desc_ <- sapply(ds$Description, FUN = function(x){as.numeric(nchar(x) ) } )
  ds$Description <- tolower(ds$Description)
  ds$desc_cute <- sapply(ds$Description, FUN = function(x){ifelse(grepl("cute", x), x <- 1, x <- 0)})
  ds$desc_energetic <- sapply(ds$Description, FUN = function(x){ifelse(grepl("energ", x), x <- 1, x <- 0)})
  ds$desc_play <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" play", x), x <- 1, x <- 0)})
  ds$desc_rescu <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" rescu", x), x <- 1, x <- 0)})
  ds$desc_responsible <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" responsibl", x), x <- 1, x <- 0)})
  ds$desc_care <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" care", x), x <- 1, x <- 0)})
  ds$desc_alone <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" alone", x), x <- 1, x <- 0)})
  ds$desc_temporaly <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" tempora", x), x <- 1, x <- 0)})
  ds$desc_dump <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" dump", x), x <- 1, x <- 0)})
  ds$desc_health <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" health", x), x <- 1, x <- 0)})
  ds$desc_adorable <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" adorab", x), x <- 1, x <- 0)})
  ds$desc_home <- sapply(ds$Description, FUN = function(x){ifelse(grepl("home", x), x <- 1, x <- 0)})
  ds$desc_active <- sapply(ds$Description, FUN = function(x){ifelse(grepl("active", x), x <- 1, x <- 0)})
  ds$desc_master <- sapply(ds$Description, FUN = function(x){ifelse(grepl("master", x), x <- 1, x <- 0)})
  ds$desc_adopt <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" adopt", x), x <- 1, x <- 0)})
  ds$desc_love <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" lov", x), x <- 1, x <- 0)})
  ds$desc_cannot <- sapply(ds$Description, FUN = function(x){ifelse((grepl(" cannot ", x)||grepl(" can't ", x)||grepl(" cant ", x)), x <- 1, x <- 0)})
  ds$desc_owner <- sapply(ds$Description, FUN = function(x){ifelse(grepl("owner", x), x <- 1, x <- 0)})
  ds$desc_attentive <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" attent", x), x <- 1, x <- 0)})
  ds$desc_stray <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" stray", x), x <- 1, x <- 0)})
  ds$desc_serious <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" seriou", x), x <- 1, x <- 0)})
  ds$desc_help <- sapply(ds$Description, FUN = function(x){ifelse(grepl("help", x), x <- 1, x <- 0)})
  ds$desc_friend <- sapply(ds$Description, FUN = function(x){ifelse(grepl("friend", x), x <- 1, x <- 0)})
  ds$desc_good <- sapply(ds$Description, FUN = function(x){ifelse(grepl("good", x), x <- 1, x <- 0)})
  ds$desc_lost <- sapply(ds$Description, FUN = function(x){ifelse(grepl("lost", x), x <- 1, x <- 0)})
  ds$desc_found <- sapply(ds$Description, FUN = function(x){ifelse(grepl("found", x), x <- 1, x <- 0)})
  ds$desc_lookingfor <- sapply(ds$Description, FUN = function(x){ifelse(grepl("looking for", x), x <- 1, x <- 0)})
  ds$desc_kind <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" kind", x), x <- 1, x <- 0)})
  ds$desc_quite <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" qui", x), x <- 1, x <- 0)})
  ds$desc_curious <- sapply(ds$Description, FUN = function(x){ifelse(grepl("curious", x), x <- 1, x <- 0)})
  ds$desc_voice <- sapply(ds$Description, FUN = function(x){ifelse(grepl("voice", x), x <- 1, x <- 0)})
  ds$desc_most <- sapply(ds$Description, FUN = function(x){ifelse(grepl("most", x), x <- 1, x <- 0)})
  ds$desc_superlative <- sapply(ds$Description, FUN = function(x){ifelse(grepl("est ", x), x <- 1, x <- 0)})
  ds$desc_better <- sapply(ds$Description, FUN = function(x){ifelse(grepl("better", x), x <- 1, x <- 0)})
  ds$desc_notafraid <- sapply(ds$Description, FUN = function(x){ifelse(grepl("t afraid", x), x <- 1, x <- 0)})
  ds$desc_afraid <- sapply(ds$Description, FUN = function(x){ifelse(grepl("afraid", x), x <- 1, x <- 0)})
  ds$desc_beautiful <- sapply(ds$Description, FUN = function(x){ifelse(grepl("beaut", x), x <- 1, x <- 0)})
  ds$desc_companion <- sapply(ds$Description, FUN = function(x){ifelse(grepl("companion", x), x <- 1, x <- 0)})
  ds$desc_please <- sapply(ds$Description, FUN = function(x){ifelse(grepl("please", x), x <- 1, x <- 0)})
  ds$desc_family <- sapply(ds$Description, FUN = function(x){ifelse(grepl("famil", x), x <- 1, x <- 0)})
  ds$desc_stranger <- sapply(ds$Description, FUN = function(x){ifelse(grepl("stranger", x), x <- 1, x <- 0)})
  ds$desc_happy <- sapply(ds$Description, FUN = function(x){ifelse(grepl("happ", x), x <- 1, x <- 0)})
  ds$desc_need <- sapply(ds$Description, FUN = function(x){ifelse(grepl("need", x), x <- 1, x <- 0)})
  ds$desc_calm <- sapply(ds$Description, FUN = function(x){ifelse(grepl("calm", x), x <- 1, x <- 0)})
  ds$desc_call <- sapply(ds$Description, FUN = function(x){ifelse(grepl("call", x), x <- 1, x <- 0)})
  ds$desc_sms <- sapply(ds$Description, FUN = function(x){ifelse(grepl("sms", x), x <- 1, x <- 0)})
  ds$desc_mail <- sapply(ds$Description, FUN = function(x){ifelse(grepl("mail", x), x <- 1, x <- 0)})
  ds$desc_thank <- sapply(ds$Description, FUN = function(x){ifelse(grepl("thank", x), x <- 1, x <- 0)})
  ds$desc_find <- sapply(ds$Description, FUN = function(x){ifelse(grepl("find", x), x <- 1, x <- 0)})
  ds$desc_toilet <- sapply(ds$Description, FUN = function(x){ifelse(grepl("toilet", x), x <- 1, x <- 0)})
  ds$desc_save <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" sav", x), x <- 1, x <- 0)})
  ds$desc_allow <- sapply(ds$Description, FUN = function(x){ifelse(grepl("allow", x), x <- 1, x <- 0)})
  ds$desc_road <- sapply(ds$Description, FUN = function(x){ifelse(grepl("road", x), x <- 1, x <- 0)})
  ds$desc_indoor <- sapply(ds$Description, FUN = function(x){ifelse(grepl("indoor", x), x <- 1, x <- 0)})
  ds$desc_children <- sapply(ds$Description, FUN = function(x){ifelse(grepl("children", x), x <- 1, x <- 0)})
  ds$desc_gentle <- sapply(ds$Description, FUN = function(x){ifelse(grepl("gentl", x), x <- 1, x <- 0)})
  ds$desc_sick <- sapply(ds$Description, FUN = function(x){ifelse(grepl("sick", x), x <- 1, x <- 0)})
  ds$desc_abandon <- sapply(ds$Description, FUN = function(x){ifelse(grepl("abandon", x), x <- 1, x <- 0)})
  ds$desc_well <- sapply(ds$Description, FUN = function(x){ifelse(grepl("well", x), x <- 1, x <- 0)})
  ds$desc_gorgeous <- sapply(ds$Description, FUN = function(x){ifelse(grepl("gorgeous", x), x <- 1, x <- 0)})
  ds$desc_must <- sapply(ds$Description, FUN = function(x){ifelse(grepl("must", x), x <- 1, x <- 0)})
  ds$desc_fat <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" fat", x), x <- 1, x <- 0)})
  ds$desc_street <- sapply(ds$Description, FUN = function(x){ifelse(grepl("street", x), x <- 1, x <- 0)})
  ds$desc_bark <- sapply(ds$Description, FUN = function(x){ifelse(grepl("bark", x), x <- 1, x <- 0)})
  ds$desc_contact <- sapply(ds$Description, FUN = function(x){ifelse(grepl("contact", x), x <- 1, x <- 0)})
  ds$desc_guard <- sapply(ds$Description, FUN = function(x){ifelse(grepl("guard", x), x <- 1, x <- 0)})
  ds$desc_potential <- sapply(ds$Description, FUN = function(x){ifelse(grepl("potential", x), x <- 1, x <- 0)})
  ds$desc_emotion <- sapply(ds$Description, FUN = function(x){ifelse(grepl("emotion", x), x <- 1, x <- 0)})
  ds$desc_fail <- sapply(ds$Description, FUN = function(x){ifelse(grepl("fail", x), x <- 1, x <- 0)})
  ds$desc_... <- sapply(ds$Description, FUN = function(x){ifelse(grepl("...", x), x <- 1, x <- 0)})
  ds$desc_only <- sapply(ds$Description, FUN = function(x){ifelse(grepl("only", x), x <- 1, x <- 0)})
  ds$desc_vet <- sapply(ds$Description, FUN = function(x){ifelse(grepl("vet", x), x <- 1, x <- 0)})
  ds$desc_scare <- sapply(ds$Description, FUN = function(x){ifelse(grepl("scare", x), x <- 1, x <- 0)})
  ds$desc_job <- sapply(ds$Description, FUN = function(x){ifelse(grepl("job", x), x <- 1, x <- 0)})
  ds$desc_obey <- sapply(ds$Description, FUN = function(x){ifelse(grepl(" obe", x), x <- 1, x <- 0)})
  ds$desc_train <- sapply(ds$Description, FUN = function(x){ifelse(grepl("train", x), x <- 1, x <- 0)})
  ds$desc_behave <- sapply(ds$Description, FUN = function(x){ifelse(grepl("behav", x), x <- 1, x <- 0)})
 
  
  ds <- ds %>% select_if(is.numeric)
  return( ds)
}


setwd("I:/Documents/Kaggle/PetFinder.my Adoption Prediction/input")
train = read_csv("../input/train/train.csv")
desc_train <- clean(train)

numberOfTrainingSamples <- round(nrow(desc_train) * .9)
train_data <- desc_train[1:numberOfTrainingSamples,]
test_data <- desc_train[-(1:numberOfTrainingSamples),]
"
numberOfTrainingSamples <- round(nrow(desc_train) * .1)
test_data <- desc_train[1:numberOfTrainingSamples,]
train_data <- desc_train[-(1:numberOfTrainingSamples),]"

# convert the dataframe into a matrix
makeDMatrix <- function(tdata){
  #Remove information about the target variable from the training data
  tdata_TargetRemoved <- tdata %>% select(-AdoptionSpeed)
  
  Labels <- tdata %>% select(AdoptionSpeed) %>% as.matrix()
  Info_matrix <- data.matrix(tdata_TargetRemoved)
  
  # put our testing & training data into two seperates Dmatrixs objects
  dtrain <- xgb.DMatrix(data = Info_matrix, label = Labels)
}

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- makeDMatrix(train_data)#xgb.DMatrix(data = train_data, label = train_labels)
dtest <- makeDMatrix(test_data)#xgb.DMatrix(data = test_data, label = test_labels)

# train a model using our training data
param <- list("objective" = "multi:softprob"
              ,"eval_metric" = "mlogloss"
              ,"num_class" = 5
              ,"eta" = 0.1, "max_depth" = 6, "gamma" = 0.3,min_child_weight=1
)
watchlist <- list("train"=dtrain, "test"=dtest)

#model <- xgboost(param=param, data=dtrain, nrounds=20)
model <- xgb.train(param=param, data=dtrain, nrounds=65, watchlist=watchlist,method = 'ada')


#training & test error plot
e <- data.frame(model$evaluation_log)
plot(e$iter,e$train_mlogloss, col='blue')
lines(e$iter,e$test_mlogloss, col='red')

e[e$test_mlogloss == min(e$test_mlogloss),]

#feature importance
imp <- xgb.importance(colnames(dtrain), model = model)

imp <- as.data.frame(imp)
#.9 65   65       1.178947      1.333166
#.1 64   64       1.186026      1.295689
write_csv(imp, "feature_importance.csv")

p <- predict(model, newdata = dtest)
head(p)
p <- matrix(p, nrow = 4, ncol = length(p)/4)%>% t() %>% 
  data.frame() %>% 
  mutate(realTarget = test_data$Target+1, predTarget = max.col(.,"last") )

#write_csv(p, "Kaggle/Costa Rican Household Poverty Level Prediction/input/sample.csv")