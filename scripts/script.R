train <- read.csv("C:/Users/Antonio/Dropbox/developer/R/titanic/kaggle_titanic_R/data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("C:/Users/Antonio/Dropbox/developer/R/titanic/kaggle_titanic_R/data/test.csv", stringsAsFactors=FALSE)
str(train)

table(train$Survived)
prop.table(table(train$Survived))

test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "tds_mrts.csv", row.names = FALSE)

#Score 0.62679

#2nd model =====

summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)
#Añadimos 1 para que compute en la dirección 1 (filas). Un 2 computaría en columnas

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender_model.csv", row.names = FALSE)

#Score -- 0.76555

#3rd model =====

summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1

summary(train$Child)

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})


train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20]<-0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender_sex_fare_model.csv", row.names = FALSE)

# Modelo 4 ===== arboles de decision

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

plot(fit)
text(fit)

install.packages('rattle')
library(rattle)

install.packages('rpart.plot')
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             #data=train,
             #method="class",
             #control=rpart.control(cp=0,minsplit=2))
#new.fit <- prp(fit,snip=TRUE)$obj
#fancyRpartPlot(new.fit)

# 5 ==== Tryin' feature eng

test$Survived <- NA
combi <- rbind(train, test)

#Vamos a jugar con los characters, los convertimos a strings!
combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

fancyRpartPlot(fit)

which(combi$Embarked == '')

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featured_eng.csv", row.names = FALSE)

# 6 Random Forest

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

# Cambiamos los NA en 'Embarked' que tenemos por S
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Vemos los NA que tenemos en "FARE"
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# R sólo acepta RF's con col con 32 factores
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

install.packages('randomForest')
library(randomForest)

combi$Sex=as.factor(combi$Sex)
combi$Embarked=as.factor(combi$Embarked)
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(123)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)
train=as.factor(train)
summary(train$FamilyID2)
which(is.nan(train))
train$Sex=as.factor(train$Sex)
train$Embarked=as.factor(train$Embarked)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "muhforest.csv", row.names = FALSE)

# Conditional inference trees

install.packages('party')
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                Embarked + Title + FamilySize + FamilyID,
                data = train, 
                controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, newdata=test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditional_forest.csv", row.names = FALSE)

summary(Prediction)
