library(stringr)
library(stringi)
library(qdapRegex)
library(plyr)
library(Amelia)
library(missForest)
library(caret)
cleanbusiness <- read.csv("/Users/ncakpan/Desktop/239Project/business_clean.csv",header = TRUE, na.strings = c(""))

cleanbusinessResturants <- NULL
businessCategories <- c("categories.0","categories.1","categories.2",
                        "categories.3","categories.4","categories.5",
                        "categories.6","categories.7","categories.8","categories.9")

cleanbusinessResturants <- cleanbusiness[which(cleanbusiness$categories.0 == "Restaurants" | 
                                                 cleanbusiness$categories.1 == "Restaurants" |
                                                 cleanbusiness$categories.2 == "Restaurants" |
                                                 cleanbusiness$categories.3 == "Restaurants" |
                                                 cleanbusiness$categories.4 == "Restaurants" |
                                                 cleanbusiness$categories.5 == "Restaurants" |
                                                 cleanbusiness$categories.6 == "Restaurants" |
                                                 cleanbusiness$categories.7 == "Restaurants" |
                                                 cleanbusiness$categories.8 == "Restaurants" |
                                                 cleanbusiness$categories.9 == "Restaurants" &
                                                 cleanbusiness$stars>=0),]


usstatelist <- read.csv("states.csv", header = TRUE)
usstates <- usstatelist$abbreviation
cleanBusinessResturantsInUSA <- cleanbusinessResturants[which(cleanbusinessResturants$state %in% usstates),]
names(cleanBusinessResturantsInUSA)
unnecessaryColumns <-c("Hair","Dietary", "Payment" ,"Insurance" ,"Appointment")


for (i in 1:length(unnecessaryColumns)) {
  if (i == 1) {
    businessResturantsInUSACleanedColumns <- cleanBusinessResturantsInUSA[,-which(str_detect(names(cleanBusinessResturantsInUSA) , unnecessaryColumns[i]))]        
  }
  else{
    businessResturantsInUSACleanedColumns <- businessResturantsInUSACleanedColumns[,-which(str_detect(names(businessResturantsInUSACleanedColumns) , unnecessaryColumns[i]))]        
  }
  
}

businessResturantsInUSACleanedColumns <- businessResturantsInUSACleanedColumns[which(businessResturantsInUSACleanedColumns$state == "AZ"),]
businessResturantsInUSACleanedColumns$zipcode <- as.numeric(lapply(rm_zip(businessResturantsInUSACleanedColumns$full_address, extract=TRUE), tail, 1))
businessResturantsInUSACleanedColumns <-ddply(businessResturantsInUSACleanedColumns, "zipcode", function(value) {if(nrow(value)>20) value else NULL})

businessResturantsInUSACleanedColumns$Category = "Restaurants"
misses <- businessResturantsInUSACleanedColumns[,which(str_detect(names(businessResturantsInUSACleanedColumns),c("categories"))) ]
misses$stars <- businessResturantsInUSACleanedColumns$stars
missesSuccess <- subset(misses,misses$stars>3)
missesSuccess$category = "Restaurants"
testcat<-ddply(missesSuccess, businessCategories, function(value) {if(nrow(value)>50) value else NULL})
test<-missesSuccess[!duplicated(missesSuccess[businessCategories,businessCategories]),]
uni.cat.0<- unique(missesSuccess$categories.0)
as.factor(businessResturantsInUSACleanedColumns$stars)
str(businessResturantsInUSACleanedColumns)

apply(businessResturantsInUSACleanedColumns, 2, function(x) sum(is.na(x)))
cleanBusinessResturantsInUSA[,]
missmap(businessResturantsInUSACleanedColumns[,1:30], main = "Missing values vs Observed")
missmap(businessResturantsInUSACleanedColumns[,31:60], main = "Missing values vs Observed")
missmap(businessResturantsInUSACleanedColumns[,61:96], main = "Missing values vs Observed")
businessResturantsInUSACleanedColumns$success <- ifelse(businessResturantsInUSACleanedColumns$stars>=3.5 ,1,0)
totalNAValues<-as.data.frame(apply(businessResturantsInUSACleanedColumns, 2, function(x) sum(is.na(x))))
names(totalNAValues)<-"TotalNAValues"

naValuesVector<- c(as.character(rownames(subset(totalNAValues,totalNAValues$TotalNAValues <= 1000))))
naValuesVector
businessResturantsInUSACleanedColumns<-businessResturantsInUSACleanedColumns[,which(colnames(businessResturantsInUSACleanedColumns) %in% naValuesVector)]
missmapDataTest<-businessResturantsInUSACleanedColumns[,-which(businessResturantsInUSACleanedColumns == which(totalNAValues$TotalNAValues>2600))]

totalNAValues<-as.data.frame(apply(businessResturantsInUSACleanedColumns, 2, function(x) sum(is.na(x))))
names(totalNAValues)<-"TotalNAValues"
businessResturantsInUSACleanedColumns$full_address <- NULL
businessResturantsInUSACleanedColumns$longitude <- NULL
businessResturantsInUSACleanedColumns$latitude <- NULL
businessResturantsInUSACleanedColumns$name <- NULL
businessResturantsInUSACleanedColumns$zipcode <- as.factor(businessResturantsInUSACleanedColumns$zipcode)
businessResturantsInUSACleanedColumns$Category <- as.factor(businessResturantsInUSACleanedColumns$Category)
glmtest<-glm(success ~ (stars) , family = binomial, data = businessResturantsInUSACleanedColumns )
summary(glmtest)
str(businessResturantsInUSACleanedColumns)
testdf <- businessResturantsInUSACleanedColumns
str(as.factor(testdf$stars))
tapply( impu1$imp10.stars,impu1$imp10.attributes.Accepts.Credit.Cards, mean)
tapply( testdf$stars,testdf$attributes.Accepts.Credit.Cards, mean)
tapply( testdf$stars,testdf$attributes.Price.Range, mean)
tapply( testdf$stars,testdf$attributes.Music.karaoke, mean)
tapply( testdf$stars,testdf$categories.7, mean)
tapply( testdf$stars,testdf$attributes.Alcohol, mean)
tapply( testdf$stars,testdf$open, mean)
str(businessResturantsInUSACleanedColumns$attributes.Music.karaoke)
str(businessResturantsInUSACleanedColumns$attributes.Price.Range)


testDataMissForestNA <- businessResturantsInUSACleanedColumns[,]
str(testDataMissForestNA)

testDataMissForestNA$full_address <- NULL
testDataMissForestNA$name <- NULL
testDataMissForestNA$stars <- as.integer(testDataMissForestNA$stars)
testDataMissForestNA
str(testDataMissForestNA)

nominalsTest = c()
numericalsTest = c()
idValuesTest = c()
nlevels(testDataMissForestNA)

out<-c(lapply(testDataMissForestNA, nlevels))
for (i in 1:length(out)) {
  if (out[i] > 10) {
    idValuesTest = c(idValuesTest,names(out)[i])
  }
}
for(i in 1:ncol(basicTestDataMissForestNA)){
  
  if(is.factor(basicTestDataMissForestNA[,i])){
    nominalsTest = c(nominalsTest, names(basicTestDataMissForestNA)[i])
  }
  
  else if (nlevels(basicTestDataMissForestNA[,i])>10){
    idValuesTest = c(idValuesTest,names(basicTestDataMissForestNA[i]) )
  }
  else{
    numericalsTest = c(numericalsTest, names(basicTestDataMissForestNA)[i])
  }
}
x<-(10)
for (i in 1:ncol(basicTestDataMissForestNA)) {
  xtrs<-nlevels(names(basicTestDataMissForestNA)[i])
}
nlevels(testDataMissForestNA)

idValues <- c("categories.0","categories.1","city","zipcode",
              "Category","business_id","state","type","stars","review_count")
nom <- c("attributes.Accepts.Credit.Cards","attributes.Good.For.Groups","attributes.Outdoor.Seating","attributes.Good.for.Kids",
         "attributes.Alcohol","attributes.Attire","attributes.Delivery","attributes.Take.out",
         "attributes.Takes.Reservations","attributes.Waiter.Service","attributes.Good.For.dessert",
         "attributes.Good.For.latenight","attributes.Good.For.lunch","attributes.Good.For.dinner",
         "attributes.Good.For.breakfast","attributes.Good.For.brunch","attributes.Parking.garage","attributes.Parking.street",
         "attributes.Parking.validated","attributes.Parking.lot","attributes.Parking.valet","open"
)
ord <- c("attributes.Price.Range")
str(testDataMissForestNA)
a.out <- amelia(testDataMissForestNA, 
                m = 10,
                p2s = 0,
                parallel = "multicore",
                ncpus = 4,
                noms = nom, 
                ords = ord,
                idvars = idValues
)
summary(a.out)
impu1<- as.data.frame(a.out$imputations[10])

hist(a.out$imputations[[3]]$stars, col="grey", border="white")
plot(a.out)

write.amelia(obj=a.out, file.stem = "ImputedCleanData")
str(impu1)
naForImpu1 <-as.data.frame(apply(impu1, 2, function(x) sum(is.na(x))))
naForImpu1
write.csv(businessResturantsInUSACleanedColumns, file = "LatestCleaned.csv")
str(businessResturantsInUSACleanedColumns)



businesses_us_imputed_clean <- read.csv(file.choose())

businesses.only.categorical.columns <- businesses_us_imputed_clean[ , !names(businesses_us_imputed_clean) %in% c("business_id","full_address","name","longitude","latitude","type","state","categories.0","categories.1","open","city","Category","zipcode","success","X")]

businesses.only.categorical.columns$attributes.Price.Range.f <- factor(businesses.only.categorical.columns$attributes.Price.Range)

train = sample(1:nrow(businesses.only.categorical.columns),nrow(businesses.only.categorical.columns)/2)

test = -train

training_data = businesses.only.categorical.columns[train,]

testing_data = businesses.only.categorical.columns[test,]

linear_model1 = lm(businesses.only.categorical.columns$stars ~ .,data = businesses.only.categorical.columns)

liner.model = lm(training_data$stars ~ ., data = training_data)
summary(linear_model1)
abline(linear_model1)



a = summary(linear_model1)$r.squared
b = summary(linear_model1)$r.squared
c = summary(linear_model1)$r.squared
d = summary(linear_model1)$r.squared
e = summary(linear_model1)$r.squared

Rsquare = (a+b+c+d+e)/5

tapply(a,b,c,d,e, mean)

predict_linear_model = predict.lm(liner.model,testing_data)
summary(predict_linear_model)

install.packages("DAAG")
install.packages("lattice")
library(lattice)
library(DAAG)
cv.lm(businesses.only.categorical.columns, linear_model1)$delta[1] # 10 fold cross-validation

require(caret)
flds <- createFolds(businesses.only.categorical.columns, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"
summary(train)
summary(flds)
str(flds[1])
summary(businesses.only.categorical.columns[flds$train])
linear_model2 = lm(businesses.only.categorical.columns[flds$train]$stars ~ .,data = businesses.only.categorical.columns[flds$train])

library(caret)
data(GermanCredit)
Train <- createDataPartition(businesses.only.categorical.columns$Class, p=0.6, list=FALSE)



library(mlbench)
folds <- createFolds(businesses.only.categorical.columns)
str(folds)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = businesses.only.categorical.columns)
dim(businesses.only.categorical.columns)
unlist(lapply(split_up, nrow))


mydata <- data.frame(ymat, xmat)
fit <- lm(ymat ~ ., data=mydata)
library(DAAG)
cv.lm(businesses.only.categorical.columns, linear_model1, m=10)

actual <- testing_data$stars
rsq <- 1 - (sum(actual - predict_linear_model)^2) / (sum(actual - mean(actual))^2)
rsq <- 1-sum((actual-predict_linear_model)^2)/sum((actual-mean(actual))^2)
summary(rsq)

summary(businesses.only.categorical.columns$stars)
summary(linear_model1)
prediction_model = predict.lm(linear_model1,training_data)
summary(prediction_model)
summary(prediction_model)
plot(prediction_model)
#1- in this the predicted value of stars is on the x axis and fitted value, the error rate are on the y axis
#the red line or the error rate is almost flat here which shows linearity assumption is met.


businesses_us_cleaned <- read.csv(file.choose())

library(mice)
attach(businesses_us_cleaned)
head(businesses_us_cleaned)
set.seed(2)

#business_csv3 = businesses_us_cleaned[ , names(businesses_us_cleaned) %in% c("city","business_id","categories.0","categories.1", "stars", "review_count")]
business_csv3 = businesses_us_cleaned[ , names(businesses_us_cleaned) %in% c("city","business_id","attributes.Price.Range", "stars", "review_count")]
high_rating = ifelse(business_csv3$stars >= 3.5, "YES", "NO")

#with na
businesses_us_cleaned[businesses_us_cleaned==""] <- NA

#remove cols that have NAs more than or equal to 3600
businesses.with.less.na <- businesses_us_cleaned[, colSums(is.na(businesses_us_cleaned)) < 3600]

businesses_us_imputed_clean <- read.csv(file.choose())

businesses.only.categorical.columns <- businesses_us_imputed_clean[ , !names(businesses_us_imputed_clean) %in% c("business_id","full_address","name","longitude","latitude","type","state","categories.0","categories.1","open","city","Category","zipcode","success","X")]
businesses.only.categorical.columns$attributes.Price.Range.f <- factor(businesses.only.categorical.columns$attributes.Price.Range)

which(colnames(businesses.only.categorical.columns)=="attributes.Price.Range")

#remove price range continuous
businesses.only.categorical.columns <- businesses.only.categorical.columns[,-5]
head(businesses.only.categorical.columns)

#library(mice)
#impute <- mice(businesses.only.categorical.columns, m=1, maxit = 1, method = 'pmm', seed = 500)
#businesses.categorical.imputed <- complete(impute,1)

#train = sample(1:nrow(completedData),nrow(completedData)/2)
#test = -train
#training_data = businesses.categorical.imputed[train,]
#testing_data = businesses.categorical.imputed[test,]

#linear reg
liner.model = lm(businesses.only.categorical.columns$stars ~ ., data = businesses.only.categorical.columns)
summary(liner.model)

train = sample(1:nrow(businesses.only.categorical.columns),nrow(businesses.only.categorical.columns)/2)
test = -train
training_data = businesses.only.categorical.columns[train,]
testing_data = businesses.only.categorical.columns[test,]

liner.model = lm(training_data$stars ~ ., data = training_data)
summary(liner.model)

predict_linear_model = predict.lm(liner.model,testing_data)
summary(predict_linear_model)
points(predict_linear_model, testing_data)

#logistic reg
success = ifelse(businesses.only.categorical.columns$stars >= 3.5, 1, 0)
businesses.only.categorical.columns[,"success"] <- success

businesses.only.categorical.no.stars <- businesses.only.categorical.columns[ , !names(businesses.only.categorical.columns) %in% c("stars")]
businesses.only.categorical.no.stars$success <- factor(businesses.only.categorical.no.stars$success)

business_model_sample = glm(businesses.only.categorical.no.stars$success ~ ., data = businesses.only.categorical.no.stars, family = binomial)
summary(business_model_sample)

#impute
#impute <- mice(businesses.with.less.na, m=1, maxit = 1, method = 'pmm', seed = 500)
#businesses.imputed <- complete(impute,1)

#attach variable to dataset 
rest.success <- data.frame(business_csv3, high_rating)

rest.success[rest.success==""] <- NA

one.tenth = sample(1:nrow(rest.success),nrow(rest.success)/200)

one.tenth.data = rest.success[one.tenth,]
temp.one.tenth <- mice(rest.success, m=1, maxit = 1, method = 'pmm', seed = 2)
imputedData <- complete(temp.one.tenth,1)
View(imputedData)

#rest.success.imputed <- rfImpute(high_rating ~rest.success$categories.1, rest.success)
library(mice)
tempdata <- mice(rest.success, m=5, maxit = 2, method = 'pmm', seed = 500)
completedData <- complete(tempdata,1)
head(completedData)
??mice

attach(completedData)

train = sample(1:nrow(completedData),nrow(completedData)/2)
test = -train
training_data = completedData[train,]
testing_data = completedData[test,]

high_test = high_rating[test]

business_model_sample = glm(high_rating ~ stars, 
                            data = training_data, family = binomial)
model_pred_probs = predict(business_model_sample, testing_data, type = "response")


