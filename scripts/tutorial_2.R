# Packages and importing stuff
library('dplyr') #Manipulating data

install.packages('Amelia')
library(Amelia)

install.packages('ggplot2')
library(ggplot2)

# Loading data

train <- read.csv("C:/Users/Antonio/Dropbox/developer/R/titanic/kaggle_titanic_R/data/train.csv", 
                  stringsAsFactors=FALSE)
test <- read.csv("C:/Users/Antonio/Dropbox/developer/R/titanic/kaggle_titanic_R/data/test.csv", 
                 stringsAsFactors=FALSE)
combi<-bind_rows(train, test)
str(combi)

# Checking missing values (missing values or empty values)
colSums(is.na(combi)|combi=='')

missmap(combi, 
        main="Titanic Data - Missings Map",
        col=c("yellow", "black"), 
        legend=FALSE)


filter(combi, is.na(Fare)==TRUE|Fare=='')

ggplot(filter(combi, Pclass==3 & Embarked=="S"), aes(Fare)) +                       
  geom_density(fill="blue", alpha=0.5) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='darkblue', linetype='dashed', size=2) +
  geom_vline(aes(xintercept=mean(Fare, na.rm=T)), colour='red', linetype='dashed', size=2) +
  ggtitle("Fare distribution of third class passengers \n embarked from Southampton port") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#En la gráfica observamos que la media está lejos de la mediana
#y los datos se concentran más cerca de la mediana. Por lo tanto
#imputamos los datos de la mediana al NA faltante en Fare

combi$Fare[is.na(combi$Fare)==TRUE] = median(filter(combi, Pclass==3 & Embarked=="S")$Fare, na.rm=TRUE)
colSums(is.na(combi)|combi=='')

# Vemos ahora qué falta en "Embarked"
filter(combi, is.na(Embarked)==TRUE|Embarked=='')

table(filter(combi, Pclass==1)$Embarked)

ggplot(filter(combi, is.na(Embarked)==FALSE & Embarked!='' & Pclass==1), 
       aes(Embarked, Fare)) +     
  geom_boxplot(aes(colour = Embarked)) +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', size=2) +
  ggtitle("Fare distribution of first class passengers") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Los imputamos a cherbourg

combi$Embarked[combi$Embarked==""] = "C"
colSums(is.na(combi)|combi=='')

imputed.age <- impute_age(combi$Age,combi$Pclass)
combi$Age <- imputed.age

colSums(is.na(combi)|combi=='')

# FEATURE ENGINEERING

# Extracting titles

head(combi$Name)
# Grab passenger title from passenger name
combi$Title <- gsub("^.*, (.*?)\\..*$", "\\1", combi$Name)
table(combi$Sex, combi$Title)

# First, I reassign few categories 
combi$Title[combi$Title == 'Mlle' | combi$Title == 'Ms'] <- 'Miss' 
combi$Title[combi$Title == 'Mme']  <- 'Mrs' 

# Then, I create a new category with low frequency of titles
Other <- c('Dona', 'Dr', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Rev', 'Sir')
combi$Title[combi$Title %in% Other]  <- 'Other'

# Let's see if it worked
table(combi$Sex, combi$Title)

# Family size 
FamilySize <- combi$SibSp + combi$Parch + 1
table(FamilySize)

combi$FamilySize <- sapply(1:nrow(combi), function(x) 
          ifelse(FamilySize[x]==1, "Single", 
          ifelse(FamilySize[x]>4, "Large", "Small")))

table(combi$FamilySize)

colSums(is.na(combi)|combi=='')

## 4 Exploratory Data Analysis ##

combi$Survived = factor(combi$Survived)
combi$Pclass = factor(combi$Pclass)
combi$Sex = factor(combi$Sex)
combi$Embarked = factor(combi$Embarked)
combi$Title = factor(combi$Title)
combi$FamilySize = factor(combi$FamilySize, levels=c("Single","Small","Large"))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Pclass, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9, position="dodge") +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.6,0.05)) +
  ylab("Porcentaje") + 
  ggtitle("Tasa de supervivencia basada en Pclass") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Sex, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.4,0.05)) +
  ylab("Porcentaje") + 
  ggtitle("Tasa de supervivencia basada en Pclass y Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Pclass, Age)) + 
  geom_violin(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  ggtitle("Survival Rate based on Pclass and Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~FamilySize) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on FamilySize and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(combi, is.na(Survived)==FALSE), aes(Embarked, Fare)) + 
  geom_boxplot(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_manual(values=c("#56B4E9", "#CC79A7")) +
  ggtitle("Survival Rate based on Embarked and Fare") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# LANZAMOS SIMULACIONES

new_train <- combi[1:891, c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]
new_test <- combi[892:1309, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(new_train$Survived, SplitRatio = 0.8)
train_train = subset(new_train, split == TRUE)
train_test = subset(new_train, split == FALSE)


cor(train_train[,unlist(lapply(train_train,is.numeric))])
# Fitting Logistic Regression to the Training set
classifier = glm(Survived ~ ., family = binomial(link='logit'), data = train_train)
classifier <- step(classifier)
summary(classifier)

install.packages('car')
library(car)
vif(classifier)


classifier = glm(Survived ~ . -Sex, family = binomial(link='logit'), data = train_train)
classifier <- step(classifier)
summary(classifier)
vif(classifier)
durbinWatsonTest(classifier)

prob_pred = predict(classifier, type = 'response', newdata = train_test)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

table(train_test$Survived, y_pred > 0.5)
error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy of the model',round(1-error,4))

Prediction <- predict(classifier, type = "response", newdata=new_test)
y_pred = ifelse(Prediction > 0.5, 1, 0)
submit <- data.frame(PassengerId = test$PassengerId, Survived = y_pred)
write.csv(submit, file = "model_1.csv", row.names = FALSE)

### MODELO 2 ####

paste('Age variance: ',var(train_train$Age),', SibSp variance: ',var(train_train$SibSp),', Parch variance: ',var(train_train$Parch),', Fare variance: ',var(train_train$Fare))

standardized.train = cbind(select(train_train, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(train_train$Age), Fare = scale(train_train$Fare))
paste('Age variance: ',var(standardized.train$Age),', Fare variance: ',var(standardized.train$Fare))

standardized.test = cbind(select(train_test, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(train_test$Age), Fare = scale(train_test$Fare))
paste('Age variance: ',var(standardized.test$Age),', Fare variance: ',var(standardized.test$Fare))

install.packages('e1071')
library(e1071)
# Fitting Linear SVM to the Training set
classifier = svm(Survived ~ .,
                 data = standardized.train,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Validation set results
y_pred = predict(classifier, newdata = standardized.test[,-which(names(standardized.test)=="Survived")])

# Checking the prediction accuracy
table(standardized.test$Survived, y_pred) # Confusion matrix

error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

tune.results <- tune(svm,
                     Survived ~ .,
                     data = standardized.train,
                     kernel='radial',
                     ranges=list(cost=2^(-2:2), gamma=2^(-6:-2)))
summary(tune.results)

# The best non-linear SVM performance occurs with cost=4 and gamma=0.125

# Fitting Non-linear SVM to the Training set
classifier = svm(Survived ~ .,
                 data = standardized.train,
                 type = 'C-classification',
                 kernel = 'radial',
                 cost = 2,
                 gamma = 0.125)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = standardized.test[,-which(names(standardized.test)=="Survived")])

# Checking the prediction accuracy
table(train_test$Survived, y_pred) # Confusion matrix

error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

standardized.submit= cbind(select(new_test, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(new_test$Age), Fare = scale(new_test$Fare))
paste('Age variance: ',var(standardized.submit$Age),', Fare variance: ',var(standardized.submit$Fare))

Prediction <- predict(classifier, type = "response", newdata=new_test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "model_2.csv", row.names = FALSE)

table(Prediction)

#### MODEL 3 - DECISION TREE ####

classifier = rpart(Survived ~ ., data = train_train, method = 'class')

# Tree Visualization
rpart.plot(classifier, extra=4)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = train_test[,-which(names(train_test)=="Survived")], type='class')

# Checking the prediction accuracy
table(train_test$Survived, y_pred) # Confusion matrix

error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

install.packages('caret')
library(caret)

set.seed(789)
folds = createMultiFolds(train_train$Survived, k = 10, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train_train, method = "rpart", trControl = control)

rpart.plot(classifier_cv$finalModel, extra=4)

# Predicting the Validation set results
y_pred = predict(classifier_cv, newdata = train_test[,-which(names(train_test)=="Survived")])

# Checking the prediction accuracy
table(train_test$Survived, y_pred) # Confusion matrix
error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Random Forest =====
# Fitting Random Forest Classification to the Training set
set.seed(432)
classifier = randomForest(Survived ~ ., data = train_train)

# Choosing the number of trees
plot(classifier)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = train_test[,-which(names(train_test)=="Survived")])

# Checking the prediction accuracy
table(train_test$Survived, y_pred) # Confusion matrix

error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

Prediction <- predict(classifier, type = "response", newdata=new_test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "model_muh_forst.csv", row.names = FALSE)

table(Prediction)


set.seed(651)
folds = createMultiFolds(train_train$Survived, k = 10)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train_train, method = "rf", trControl = control)

# Predicting the Validation set results
Prediction <- predict(classifier_cv, newdata=new_test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "model_muh_forst.csv", row.names = FALSE)

table(Prediction)
# Checking the prediction accuracy
table(test$Survived, y_pred) # Confusion matrix

gini = as.data.frame(importance(classifier))
gini = data.frame(Feature = row.names(gini), 
                  MeanGini = round(gini[ ,'MeanDecreaseGini'], 2))
gini = gini[order(-gini[,"MeanGini"]),]

ggplot(gini,aes(reorder(gini$Feature,gini$MeanGini), gini$MeanGini, group=1)) + 
  geom_point(color='red',shape=17,size=2) + 
  geom_line(color='blue',size=1) +
  scale_y_continuous(breaks=seq(0,60,10)) +
  xlab("Feature") + 
  ggtitle("Mean Gini Index of Features") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Naive Bayes ####

# Fitting Naive Bayes to the Training set
classifier = naiveBayes(Survived ~ ., data = train_train)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = train_test[,-which(names(train_test)=="Survived")])

# Checking the prediction accuracy
table(train_test$Survived, y_pred) # Confusion matrix

error <- mean(train_test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))