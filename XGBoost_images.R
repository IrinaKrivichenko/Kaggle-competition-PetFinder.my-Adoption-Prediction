library(tidyverse) # general utility functions
library(xgboost) # for xgboost
library(OpenImageR)
library(data.table)
library(imager)
library(keras)

load_image<-function(path){ 
  img <- image_load(path = path, target_size = c(256,256))
  img_arr <- image_to_array(img)   
  img_arr <- array_reshape(img_arr, c( 256, 256, 3))
  new_image<-img_arr    
  new_image<-densenet_preprocess_input(new_image,data_format="channels_last")   
  return(new_image)
}
resize_to_square<-function(im){
  old_size <- dim(im)[1:2] 
  img_size <- 256
  ratio <- img_size/max(old_size)
  new_size<-c(round(old_size[1]*ratio),round(old_size[2]*ratio))
  im <- resizeImage(im, new_size[1], new_size[2])
  #plot(as.raster(im))
  delta_w <- img_size - new_size[2]
  delta_h <- img_size - new_size[1]
  top <- delta_h%/%2
  bottom <- delta_h-(delta_h%/%2)
  left<- delta_w%/%2
  right <- delta_w-(delta_w%/%2)
  color <- c(0, 0, 0)
  new_im = FillBorder(im, top, bottom, left, right, value=color)
  #plot(as.raster(new_im))
  return(new_im)
}
FillBorder<-function(im, top, bottom, left, right,value=color){
  if (top!=0){
    new_image1<-c()
    for (j in 1:3){
      for (i in 1:256){
        new_image1<-c(new_image1,rep(0,top),im[,i,j],rep(0,bottom))
      }
    }
  } else {
    new_image1<-c(rep(0,left*256),im[,,1],rep(0,right*256),rep(0,left*256),im[,,2],rep(0,right*256),rep(0,left*256),im[,,3],rep(0,right*256))
  }
  
  dim(new_image1)<-c(256,256,3)
  return(new_image1)
  
}
prepare_img_model <- function(){
  conv_base <- application_densenet121(
    weights = "imagenet",
    include_top = FALSE,
    input_shape = c(256, 256, 3)
  ) 
  expand_dims<-function(args){
    k_expand_dims(args,axis = -1)
  }
  lambda2<-function(args){
    args[,,1]
  }
  input_tensor <- layer_input(shape = c(256,256,3))
  output_tensor <- input_tensor %>%
    conv_base %>%
    layer_global_average_pooling_2d %>%
    layer_lambda(expand_dims) %>%
    layer_average_pooling_1d(4)  %>%
    layer_lambda(lambda2) 
  model <- keras_model(input_tensor, output_tensor)
}

clean <- function(ds){  
  ds$Name <- tolower(ds$Name)
  ds$named <- sapply(ds$Name, FUN = function(x){ifelse((is.na(x)  || grepl("name", x)), x <- 0, x <- 1)})
  ds$numname <- sapply(ds$Name, FUN = function(x){ifelse( grepl("[[:digit:]]", x), x <- 1, x <- 0)})
  ds$State <- sapply(ds$State, FUN = function(x){ifelse( x==41415, x <- 0, x <- x)})
  ##############################
  #image_columns <- paste0("X.",c(0:255),".")
  
  #  ds$State <- sapply(ds$State, FUN = function(x){ifelse( x==41415, x <- 0, x <- x)})
  " 
  ds$Breed1 <- sapply(ds$Breed1, FUN = function(x){ifelse( x!=307&&x!=218&&x!=213&&x!=205&&x!=195
  &&x!=189&&x!=179&&x!=152&&x!=141&&x!=128
  &&x!=109&&x!=103&&x!=78&&x!=69&&x!=20
  &&x!=306&&x!=303&&x!=299&&x!=292&&x!=285
  &&x!=283&&x!=266&&x!=265&&x!=264&&x!=254&&x!=247&&x!=243
  , 0 , x)})
  ds$Breed2 <- sapply(ds$Breed2, FUN = function(x){ifelse( x>0, 1 , 0)})
  "
  #list all the columns that has one of two possible values (0 or 1)
  bool = c("Type", #"Breed1",  
           "Gender",   "Color1", "Color2", "Color3",
           #"MaturitySize", 
           "FurLength", "Vaccinated",
           "Dewormed", "Sterilized", "Health"#, "State"
  )
  ds[bool] <- lapply(ds[bool], as.factor) # make sure bool columns is a factor & not a number
  #breeds <- read_csv("../input/breed_labels.csv")  #%>% select(-c("Name","RescuerID","Description","PetID", "State"))
  #breeds <- rbind(data.frame(BreedID=c(0), Type=c(0), BreedName=c("No breed")), breeds)
  #df$Breed1 = factor(df$Breed1,levels = breeds$BreedID,labels = breeds$BreedName)
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
  ds <- ds %>% select(-c("Name","RescuerID","Description","PetID"))
  ds = data.frame(model.matrix(~.-1,ds))
  ds <- ds %>% select_if(is.numeric)
  return( ds)
}

setwd("I:/Documents/Kaggle/PetFinder.my Adoption Prediction/input")
train <- read_csv("../input/train/train_img_features.csv")
train_densenet121 <- clean(train)












numberOfTrainingSamples <- round(nrow(train_densenet121) * .9)
train_data <- train_densenet121[1:numberOfTrainingSamples,]
test_data <- train_densenet121[-(1:numberOfTrainingSamples),]
"
numberOfTrainingSamples <- round(nrow(train) * .1)
test_data <- train[1:numberOfTrainingSamples,]
train_data <- train[-(1:numberOfTrainingSamples),]"

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
#write_csv(imp, "../input/feature_importance.csv")

p <- predict(model, newdata = dtest)
head(p)
p <- matrix(p, nrow = 4, ncol = length(p)/4)%>% t() %>% 
  data.frame() %>% 
  mutate(realTarget = test_data$Target+1, predTarget = max.col(.,"last") )

#write_csv(p, "Kaggle/Costa Rican Household Poverty Level Prediction/input/sample.csv")