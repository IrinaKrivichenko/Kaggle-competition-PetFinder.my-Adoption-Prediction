library(tidyverse) # general utility functions
#library(microbenchmark )

clean <- function(ds){  
  ds$Name <- tolower(ds$Name)
  ds$named <- sapply(ds$Name, FUN = function(x){ifelse((is.na(x)  || grepl("name", x)), x <- 0, x <- 1)})
  ds$numname <- sapply(ds$Name, FUN = function(x){ifelse( grepl("[[:digit:]]", x), x <- 1, x <- 0)})
  
  #list all the columns that has one of two possible values (0 or 1)
  bool = c("Type",  "Gender", "Color1", "Color2", "Color3",
           "MaturitySize", "FurLength", "Vaccinated",
           "Dewormed", "Sterilized", "Health", "State")
  ds[bool] <- lapply(ds[bool], as.factor) # make sure bool columns is a factor & not a number
  # breeds <- read_csv("../input/breed_labels.csv")  %>% select(-c("Name","RescuerID","Description","PetID", "State"))
  #breeds <- rbind(data.frame(BreedID=c(0), Type=c(0), BreedName=c("No breed")), breeds)
  #df$Breed1 = factor(df$Breed1,levels = breeds$BreedID,labels = breeds$BreedName)
  # ds <- ds %>% select(-c("Name","RescuerID","Description","PetID", "State"))
  #   ds = data.frame(model.matrix(~.-1,ds))
  return( ds)
}
balancing_classes <- function(ds){
  dsn <- ds %>% group_by(AdoptionSpeed) %>% tally()
  n0 <- dsn$n[1]
  n1 <- dsn$n[2]
  n2 <- dsn$n[3]
  n3 <- dsn$n[4]
  n4 <- dsn$n[5]
  if(n1 > n3){n_class <- ceiling(n1/n0)}else{n_class <- ceiling(n3/n0)}
  ds0 <- ds %>% filter(AdoptionSpeed==0) 
  ds1 <- ds %>% filter(AdoptionSpeed==1) 
  ds2 <- ds %>% filter(AdoptionSpeed==2) 
  ds3 <- ds %>% filter(AdoptionSpeed==3) 
  ds4 <- ds %>% filter(AdoptionSpeed==4) 
  df <- data.frame()
  for(i in 1:n_class){
    df <- rbind(df, ds0)
  }
  n_class <- n_class*n0
  size <- n_class-n1
  ds_ind <- sample(seq_len(nrow(ds1)), size = size)
  df <- rbind(df,ds1[ds_ind, ],ds1)
  size <- n_class-n3
  ds_ind <- sample(seq_len(nrow(ds3)), size = size)
  df <- rbind(df,ds3[ds_ind, ],ds3)
  size <- n_class
  ds_ind <- sample(seq_len(nrow(ds2)), size = size)
  df <- rbind(df,ds2[ds_ind, ])
  size <- n_class
  ds_ind <- sample(seq_len(nrow(ds4)), size = size)
  df <- rbind(df,ds4[ds_ind, ])
  df_ind <- sample(seq_len(nrow(df)), size = nrow(df))
  df <- df[df_ind, ]
  return(df)
}
setwd("I:/Documents/Kaggle/PetFinder.my Adoption Prediction/input")
train = read_csv("../input/train/train.csv")
train <- clean(train)
summary(train)

train <- balancing_classes(train)
#clean(train[0:10,])[,c("Name","named","numname")]
#list.files(path = "../input/train_images")
#ds = train[c(0,1),]

# make a bar plot
ggplot((train %>% filter(Breed1<=266, Breed1>=264)), aes(x = AdoptionSpeed,
                                                         fill = Breed1 # map the fill color to caramel           
)) + # set up the plot
  geom_bar(position = "dodge") + # add the barpot
  facet_wrap(c("Breed1")) # put each level of "Breed3" in a different facet


# make a bar plot
ggplot((train %>% filter(Breed1==247)), aes(x = AdoptionSpeed,
                                            fill = Breed1 # map the fill color to caramel           
)) + # set up the plot
  geom_bar(position = "dodge") + # add the barpot
  facet_wrap(c("Breed1")) # put each level of "Breed3" in a different facet

# make a bar plot
ggplot(train, aes(x = AdoptionSpeed)) + # set up the plot
  geom_bar(position = "dodge")  # add the barpot

train %>% filter(Breed1!=307, Breed1!=266, Breed1!=265, Breed1!=299, Breed1!=264, Breed1!=292, Breed1!=285, Breed1!=141, Breed1!=205, Breed1!=179) %>% 
  group_by(Breed1) %>% 
  tally()  %>%  
  arrange(desc(n))%>% 
  filter(n>40)

#percent contains % in each AdoptionSpeed group
train %>% #filter(named==0) %>% 
  group_by(AdoptionSpeed) %>% 
  tally()
#percent contains % in each AdoptionSpeed group
train %>% #filter(named==0) %>% 
  group_by(named,AdoptionSpeed) %>% 
  tally()  %>%  
  group_by(AdoptionSpeed) %>% 
  mutate(percent = n/sum(n))

#percent contains % in each AdoptionSpeed group
train %>% #filter(named==0) %>% 
  group_by(numname,AdoptionSpeed) %>% 
  tally()  %>%  
  group_by(AdoptionSpeed) %>% 
  mutate(percent = n/sum(n))

# make a bar plot
ggplot(train, aes(x = AdoptionSpeed,
                  fill = named # map the fill color to caramel           
)) + # set up the plot
  geom_bar(position = "dodge") + # add the barpot
  facet_wrap(c("named")) # put each level of "caramel" in a different facet