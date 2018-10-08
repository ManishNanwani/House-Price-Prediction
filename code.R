## kaggle  house price prediction

df<-read.csv("F:\\kaggle\\train.csv")
test<-read.csv("F:\\kaggle\\test.csv")
View(df)
View(test)
summary(df)
summary(test)
str(df)

model<-lm(SalePrice ~ ., data=df)

## For all the Columns, wherever NA means a factor of Not Present, converting that into a category from NA
df$Alley<-factor(ifelse(is.na(df$Alley),"No Alley",df$Alley))
test$Alley<-factor(ifelse(is.na(test$Alley),"No Alley",test$Alley))

#View(df[is.na(df$BsmtExposure),])
# View(df[is.na(df$BsmtFinType2),])

df$BsmtQual<-factor(ifelse(is.na(df$BsmtQual),"No Bsmt",df$BsmtQual))
df$BsmtCond<-factor(ifelse(is.na(df$BsmtCond),"No Bsmt",df$BsmtCond))
df$BsmtFinType1<-factor(ifelse(is.na(df$BsmtFinType1),"No Bsmt",df$BsmtFinType1))

df$BsmtExposure<-factor(ifelse(df$BsmtQual=="No Bsmt","No Bsmt",df$BsmtExposure))
df$BsmtFinType2<-factor(ifelse(df$BsmtQual=="No Bsmt","No Bsmt",df$BsmtFinType2)) ## One real NA in each of these 2 variables

## For train
test$BsmtQual<-factor(ifelse(is.na(test$BsmtQual),"No Bsmt",test$BsmtQual))
test$BsmtCond<-factor(ifelse(is.na(test$BsmtCond),"No Bsmt",test$BsmtCond))
test$BsmtFinType1<-factor(ifelse(is.na(test$BsmtFinType1),"No Bsmt",test$BsmtFinType1))

test$BsmtExposure<-factor(ifelse(is.na(test$BsmtExposure),"No Bsmt",test$BsmtExposure))
test$BsmtFinType2<-factor(ifelse(is.na(test$BsmtFinType2),"No Bsmt",test$BsmtFinType2))


# View(df[df$Fireplaces==0,])
df$FireplaceQu<-factor(ifelse(is.na(df$FireplaceQu),"No",df$FireplaceQu))
test$FireplaceQu<-factor(ifelse(is.na(test$FireplaceQu),"No",test$FireplaceQu))

# View(df[is.na(df$GarageType),])
df$GarageType<-factor(ifelse(is.na(df$GarageType),"No",df$GarageType))
test$GarageType<-factor(ifelse(is.na(test$GarageType),"No",test$GarageType))

df$GarageYrBlt<-factor(ifelse(is.na(df$GarageYrBlt),"No",df$GarageYrBlt))
test$GarageYrBlt<-factor(ifelse(is.na(test$GarageYrBlt),"No",test$GarageYrBlt))

df$GarageFinish<-factor(ifelse(is.na(df$GarageFinish),"No",df$GarageFinish))
test$GarageFinish<-factor(ifelse(is.na(test$GarageFinish),"No",test$GarageFinish))

df$GarageQual<-factor(ifelse(is.na(df$GarageQual),"No",df$GarageQual))
test$GarageQual<-factor(ifelse(is.na(test$GarageQual),"No",test$GarageQual))

df$GarageCond<-factor(ifelse(is.na(df$GarageCond),"No",df$GarageCond))
test$GarageCond<-factor(ifelse(is.na(test$GarageCond),"No",test$GarageCond))

# View(df[df$PoolArea==0,])
df$PoolQC<-factor(ifelse(is.na(df$PoolQC),"No",df$PoolQC))
test$PoolQC<-factor(ifelse(is.na(test$PoolQC),"No",test$PoolQC))

df$Fence<-factor(ifelse(is.na(df$Fence),"No",df$Fence))
test$Fence<-factor(ifelse(is.na(test$Fence),"No",test$Fence))

# View(df[df$MiscVal==0 & !is.na(df$MiscFeature),])
df$MiscFeature<-factor(ifelse(is.na(df$MiscFeature),"None",df$MiscFeature))
test$MiscFeature<-factor(ifelse(is.na(test$MiscFeature),"None",test$MiscFeature))

## Removing NA values by imputation techniques
library(randomForest)
library(mice)
tempdata=mice(df,method="rf")
train_imp =complete(tempdata)
View(train_imp)
summary(train_imp)

## Running model for all the columns

model<-lm(SalePrice ~ ., data=train_imp)
print(model)
options(max.print = 10000)
summary(model)


## Removing NA values by imputation techniques from test data
tempdata=mice(test,method="rf")
test_imp =complete(tempdata)
test_imp
summary(test_imp)
boxplot(df$SalePrice)
colnames(train_imp)

## Outliers
train_final<-train_imp[,c(3,5,6,11,12,13,14,15,16,18,19,20,21,22,23,27,28,31,32,33,35,37,38,44,45,52,53,54,56,57,59,62,63,64,65,67,71,72,73,74,81)]
quantile(train_final$SalePrice,c(0.01,0.9,0.95,0.995,1))
train_final$SalePrice<-ifelse(train_final$SalePrice>450000,450000,train_final$SalePrice)
max(train_final$SalePrice)
## Again developing moddel, using the significant column variables 


model<-lm(SalePrice ~ ., data=train_final)
print(model)
options(max.print = 10000)
summary(model)


## Predicting values
test_final<-test_imp[,c(3,5,6,11,12,13,14,15,16,18,19,20,21,22,23,27,28,31,32,33,35,37,38,44,45,52,53,54,56,57,59,62,63,64,65,67,71,72,73,74)]
View(test_final)
result=predict(model,test_final)
result[result<0]=2379.66

x<-cbind(test$Id,result)
colnames(x)<-c("ID","SalePrice")
write.csv(x,file="submission.csv", row.names = F)


### The End


