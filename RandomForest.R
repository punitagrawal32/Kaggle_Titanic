## Will solve this problem using the RandomForest classifier. Will need to impute the missing values, engineer a few features
## and then we can build a model based on the training data, and test it on the Kaggle test data set

train <- read.csv("../train.csv", stringsAsFactors = F)
test  <- read.csv("../test.csv", stringsAsFactors = F)
test$Survived<- NA  #adding new column to enable binding 
both  <- rbind (train, test)
str(both) #checking the combined data

## feature engineering 
both$title<- sapply(test$Name, FUN=function(x){strsplit(x,split= '[,.]')[[1]][2]}) #extracting title from name 
both$surname<- sapply(test$Name, FUN=function(x){strsplit(x,split= '[,.]')[[1]][1]}) #extracting surname from name 
table( both$Sex, both$title) #sex wise titles
## we observe that most titles are mr,mrs, master, or miss. set the other titles to lady or sir
both$title[ both$title %in% c("Dona","Mlle","Mme","Ms","the Countess") ] <- 'Lady'
both$title[ both$title %in% c("Capt","Col","Don","Rev","Jonkheer","Sir","Major") ] <- 'Sir'
table( both$Sex, both$title) #view updated titles
both$FamilySize<- both$Parch+ both$SibSp +1 #calculating family size 
ggplot(both[1:891,], aes(x = FamilySize, fill = factor(Survived))) +geom_bar(stat='count',position='dodge') +scale_x_continuous
(breaks=c(0:11)) +labs(x = 'Family Size', y='Frequency') #data vizualization relation b/w family size and survival rate
## we see that individuals and families above 4 people have less chances of surviving so we create required groups
both$FamilySizeName[both$FamilySize == 1] <- 'individual'
both$FamilySizeName[both$FamilySize < 5 & both$FamilySize > 1] <- 'small'
both$FsamilySizeName[both$FamilySize > 4] <- 'large'

## missing value imputation as we are working with randomForest which cannot work around missing values
summary(both) #gives us a summary of all variables in the data frame 'both'. 'Age', 'Fare' and 'embarked' have missing values/NAs
## we must predict/ fill missing values. I will predict the NA age values(263) and set the missing values of oter variables
fit<- rpart( Age~ Pclass+Age+Sex+SibSp+Parch+title+FamilySizeName+Fare+Embarked,data= both[!is.na(both$Age),],method='anova')
both$Age[is.na(both$Age)] <- predict(fit, both[is.na(both$Age),])
summary(both$Age) #no more NA values
summary(both$Embarked) #2 missing values
table(both$Embarked) 
## we will impute that the missing values are 'S' because the majority of the passengers belong there
both$Embarked[ which(both$Embarked == '')] <- 'S'
summary(both$Fare)
both$Fare[ which(is.na(both$Fare))] <- median(both$Fare,na.rm=T)
summary(both)

##now we are ready to build our predictive model. new features have been engineered and missing values have been imputed
train1<-both[1:891,]
test1<-both[892:1309,]
set.seed(100) 
#forest of conditional inference trees will be used 
rffit<- cForest(as.factor(Survived)~Pclass + Sex + Age + Fare +Embarked + Title + FamilySizeName ,data=train1,
controls=cforest_unbiased(ntree=2000, mtry=3))
FinalPrediction <- predict(rffit, test, OOB=TRUE, type = "response")
