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


