data <- read.csv("~/titanic/data/titanic_full.csv",header = T)
#test <- read.csv("~/titanic/data/titanic_full.csv",header=T)

train <- data[c(1:891),]
test <- data[c(892:1309),]
#Submission 1#############################
summary(train$sex)
prop.table(table(train$sex,train$survived),1)
test$survived <- 0
test$survived[test$sex=="female"] <- 1
#submit <- data.frame(PassengerId = test$PassengerId, survived = test$survived)
#write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


# Submission 2 ##########
summary(train$age)
train$Child <- 0
train$Child[train$age<18] <- 1
aggregate(survived~sex+Child,data = train,FUN = function(x){sum(x)/length(x)})
train$fare2 <- '30+'
train$fare2[train$fare < 10] <- '<10'
train$fare2[train$fare < 20 & train$fare >= 10] <- '10-20'
train$fare2[train$fare < 30 & train$fare >= 20] <- '20-30'

aggregate(survived~fare2+pclass+sex,data = train,FUN = function(x){sum(x)/length(x)})
test$survived <- 0
test$survived[test$sex=="female"] <- 1
test$survived[test$sex=="female"& test$pclass==3 & test$fare>20] <- 0
dim(test)
#submit <- data.frame(PassengerId = test$PassengerId, survived = test$survived)
#write.csv(submit, file = "theyallperish.csv", row.names = FALSE)



#Submission 3 ######
library(rpart)
fit <- rpart(survived~pclass+sex+age+fare+sibsp+parch+embarked,data=train,method='class')
plot(fit)
text(fit)
#install.packages("rattle")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

prediction <- predict(fit,test,type='class')
#submit <- data.frame(PassengerId = test$PassengerId, survived = prediction)
#write.csv(submit,file = "myfirstdtree.csv",row.names = FALSE)

?rpart.control
fit <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
             data=train,
             method="class",
             control=rpart.control( cp=0.01,minsplit = 2 ))
fancyRpartPlot(fit)
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


#Sybmission 4  With Feature Engineering ########
test$survived <- NA
combi <- read.csv("~/titanic/data/titanic_full.csv",header = T)
View(combi)
combi$name <- as.character(combi$name)
combi$Title <- sapply(combi$name,FUN=function(x) { strsplit(x,split = '[,.]') [[1]][2] } )
combi$Title <- sub(" ","",combi$Title)


table(combi$Title)
combi$Title[combi$Title%in% c("Capt","Major","Don")] <- "Sir"
combi$Title[combi$Title%in% c("Dona","Jonkheer","the Countess")] <- "Lady"
combi$Title[combi$Title%in% c("Mlle","Mme","Ms")] <- "Mlle"
table(combi$Title)

combi$Familyname <- sapply(combi$name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
combi$FamilySize <- combi$parch+combi$sibsp+1
combi$FamilyId <- paste(as.character(combi$FamilySize),combi$Familyname,sep='-')
combi$FamilyId[combi$FamilySize<=2] <- "Small"
combi$FamilyId <- factor(combi$FamilyId)
table(combi$FamilyId)


famIds <- data.frame(table(combi$FamilyId))
famIds <- famIds[famIds$Freq<=2,]
combi$FamilyId[combi$FamilyId%in%famIds$Var1] <- "Small"
combi$FamilyId <- factor(combi$FamilyId)
table(combi$FamilyId)

train <- combi[1:891,]
test <- combi[892:1309,]
library(rpart)
fit <- rpart(survived~sex+pclass+FamilyId+FamilySize+fare+sibsp+parch+embarked+Title
             ,data = train,method='class')
library(rpart)
fancyRpartPlot(fit)
prediction <- predict(fit,test,type='class')
#submit <- data.frame(PassengerId = test$PassengerId, survived = prediction)
#write.csv(submit,file = "mysecondtree.csv",row.names = FALSE)

# Submission 5 Random Forest Prediction ########
combi <- rbind(train,test)
summary(combi$age)
agefit <- rpart(age~pclass+sex+Title+parch+sibsp+embarked+FamilySize+fare,
                data=combi[!is.na(combi$age),],
                method = "anova")

combi$age[is.na(combi$age)] <- predict(agefit, combi[is.na(combi$age),])
summary(combi$age)
summary(combi[1:length(combi)]) 

combi$embarked[combi$embarked==""] <- "S"
combi$fare[is.na(combi$fare)] <- 14
summary(combi[1:length(combi)]) 
combi$embarked <- factor(combi$embarked)
str(combi)

combi$FamilyId2 <- combi$FamilyId
combi$FamilyId2 <- as.character(combi$FamilyId2)
combi$FamilyId2[combi$FamilySize<=3] <- "Small"
combi$FamilyId2 <- factor(combi$FamilyId2)
combi$Title <- factor(combi$Title)
combi$Familyname <- factor(combi$Familyname)

str(combi)

train <- combi[1:891,]
test <- combi[892:1309,]


#여기서부터는 랜덤포레스트 인듯

set.seed(415)
install.packages("party")
library(party)

#fit <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare +
#                      embarked + Title + FamilySize + FamilyId2,
#                    data=train, 
#                    importance=TRUE, 
#                    ntree=1000)

#varImpPlot(fit)
prediction <- predict(fit,test)
#이 부분은 내가 수정
summary(prediction)
#library(RColorBrewer)
#fancyRpartPlot(prediction)
#library(tree)
#tree(prediction)

#submit <- data.frame(PassengerId = test$PassengerId, survived = prediction)
#write.csv(submit,file = "randomforest.csv",row.names = FALSE)


#str(train)
#set.seed(444)
#fit <- cforest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare +
#                 embarked + Title + FamilySize +FamilyId,
#               data = train, 
#               controls=cforest_unbiased(ntree=2000, mtry=3))

#Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#submit <- data.frame(PassengerId = test$PassengerId, survived = Prediction)
#write.csv(submit,file = "cforest.csv",row.names = FALSE)

