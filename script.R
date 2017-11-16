##Saratoga Houses
##Regression model

##Libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)

##Read data
saratoga<-read.csv("data/SaratogaHouses.csv", header=TRUE)
saratoga<-saratoga[,-1]


##Structure of the data
##The housing data set has 1728 rows and 16 variables with the target (DV) sale price
##There are no missing values in the data
dim(saratoga)
str(saratoga)
sum(is.na(saratoga))


##Data issues
##The variable bathrooms has numeric value with half bathrooms indicating bathrooms without bathtubs.
is.numeric(saratoga$bathrooms)

##Descriptive statistics
summary(saratoga)

##Correlations among internal house charcteristics and price
##DV price has many strong positive correlations with many variables
internal_chars <- c('age', 'lotSize', 'landValue', 'livingArea', 'bedrooms', 'fireplaces', 'bathrooms', 'rooms', 'pctCollege', 'price')
corrplot(cor(saratoga[, internal_chars]))


##Price has the strongest correlation with livingArea
##Linear positive correlation with many outliers?!
ggplot(aes(x=livingArea, y=price), data=saratoga) + geom_point(color='blue')


##Visualise without outliers (exclude houses >3000 sq feet)
##There is a small house with very high price
saratoga %>% 
        filter(livingArea < 3000) %>%
        ggplot(aes(x=livingArea, y=price)) + 
        geom_point(color='blue', alpha=0.5) +
        labs(x='Area', y='Price', title='Price by area in sq meters')


##Analyse rooms
##Strong positive correlations between rooms, bathrooms, bedrooms(expected)
rooms <- c('bedrooms', 'bathrooms', 'rooms')
corrplot(cor(saratoga[, rooms]))

##Rooms distribution
##The majority of the houses have 7 rooms or less
table(saratoga$rooms)
ggplot(aes(x=rooms), data=saratoga) + 
        geom_histogram(fill='blue', bins=15, binwidth = 1) + 
        ggtitle('Distribution of room count')


##Age distribution
##83 observations with age 0
##Many outliers with only 1 house (removed)
##The distribution has positive skewness and the extreme majority of houses are below 50 years old
table(saratoga$age)
saratoga %>% 
        filter(age <= 130) %>% 
        ggplot(aes(x=age)) + 
        geom_histogram(fill='blue') + 
        ggtitle('Distribution of house age')


##Checking relation between price and age
##The relationship appears somewhat steady over time, especially below 50 years. There is strong volatility in the later years.
##This is not a real effect but simply due to the sparseness of observations until after 50 year old houses as the age distribution revealed.
saratoga %>% 
        filter(age <= 130) %>%
        group_by(age) %>% 
        summarise(mean_price=mean(price)) %>%
        ggplot(aes(x=age, y=mean_price)) +
        geom_line(stat='identity', color='blue') + 
        geom_smooth(color='darkgrey') +
        ggtitle('Mean price by age')



##Price and New Construction
saratoga %>% 
        filter(!is.na(newConstruction)) %>%
        ggplot(aes(x=as.factor(newConstruction), y=log10(price))) + 
        geom_jitter(color='grey', alpha=0.2) + 
        geom_violin(fill='blue', alpha=0.7) +
        ggtitle('Log10 of median price by new or not new construction')


#Anlayse the price distribution (left skewness)
saratoga %>% 
        ggplot(aes(x=price)) + 
        geom_histogram(fill='blue') + 
        ggtitle('Distribution of DV price')

#price log10
saratoga %>%
        ggplot(aes(x=log(price))) + 
        geom_histogram(fill='blue') + 
        ggtitle('Distribution of DV price')

#multi histograms
saratoga %>%
        select(price, lotSize, age, livingArea, pctCollege,bedrooms,
               fireplaces, bathrooms, rooms) %>%
        multi.hist(density=TRUE, dcol="red", main = "Histogram")

#normalized histograms     
saratoga %>%
        select(price, lotSize, age, livingArea, pctCollege,bedrooms,
               fireplaces, bathrooms, rooms) %>%
        sapply(log) %>%
        multi.hist(density=TRUE, dcol="red", main = "Histogram")

#ANOVA
anova1 <- aov(saratoga$price ~ saratoga$sewer)
anova1
TukeyHSD(anova1)

anova2 <- aov(saratoga$price ~ saratoga$fuel)
anova2
TukeyHSD(anova2)

anova3 <- aov(saratoga$price ~ saratoga$heating)
anova3
TukeyHSD(anova3)

#T-test
#waterfront
waterNo <- saratoga %>%
        select(price, waterfront) %>%
        filter(waterfront == "No")
waterYes <- saratoga %>%
        select(price, waterfront) %>%
        filter(waterfront == "Yes")
t.test(waterNo$price, waterYes$price, alternative = "two.sided")

#newConstruction
constNo <- saratoga %>%
        select(price, newConstruction) %>%
        filter(newConstruction == "No")
constYes <- saratoga %>%
        select(price, newConstruction) %>%
        filter(newConstruction == "Yes")
t.test(constNo$price, constYes$price, alternative = "two.sided")

#centralAir
centNo <- saratoga %>%
        select(price, centralAir) %>%
        filter(centralAir == "No")
centYes <- saratoga %>%
        select(price, centralAir) %>%
        filter(centralAir == "Yes")
t.test(centNo$price, centYes$price, alternative = "two.sided")

#Model fitting
#Split the set into train and test
set.seed(2017)
train.size <- 0.8
train.index <- sample.int(length(saratoga$price), round(length(saratoga$price) * train.size))
train <- saratoga[train.index,]
test <- saratoga[-train.index,]



#log transform
#train$price <- log(train$price + 1)
#train$lotSize <- log(train$lotSize + 1)
#train$livingArea <- log(train$livingArea + 1)
#train$rooms <- log(train$rooms + 1)
#train$age <- log(train$age + 1)

#Linear regression model (result -> R^2 = 0.6607 before log transformation)
model <- lm(price~lotSize+age+landValue+livingArea+pctCollege+
                        bedrooms+fireplaces+bathrooms+rooms+heating+fuel+
                        sewer+waterfront+newConstruction+centralAir, data = train)


summary(model)


#Residual plots
plot(model)


#prediction
train$pred.price <- predict(model, newdata = train, select =
                                    c(price,lotSize,age,landValue,livingArea,
                                        bedrooms,fireplaces+bathrooms,rooms,heating+fuel+
                                        sewer+waterfront,newConstruction,centralAir))
test$pred.price <- predict(model, newdata = test, select =
                                    c(price,lotSize,age,landValue,livingArea,
                                      bedrooms,fireplaces+bathrooms,rooms,heating+fuel+
                                      sewer+waterfront,newConstruction,centralAir))

#train set 
train.corr <- round(cor(train$pred.price, train$price), 2)
train.RMSE <- round(sqrt(mean((train$pred.price - train$price)^2)))
train.MAE <- round(mean(abs(train$pred.price - train$price)))
c(train.corr^2, train.RMSE, train.MAE) #[1]     0.6561 56675.0000 40911.0000

#test set
test.corr <- round(cor(test$pred.price, test$price), 2)
test.RMSE <- round(sqrt(mean((test$pred.price - test$price)^2)))
test.MAE <- round(mean(abs(test$pred.price - test$price)))
c(test.corr^2, test.RMSE, test.MAE) #[1]     0.6241 63329.0000 41732.0000



#Cutoff of extreme values (Cooks distance) 1st
cutoff <- 4/((nrow(train)-length(model$coefficients)-2)) #D plot 4/(n-k-1)
plot(model, which = 4, cook.levels = cutoff) #D values > cutoff
plot(model, which = 5, cook.levels = cutoff)

train <- train[-which(rownames(train)
                      %in% c("602", "1202", "1279")),]

##Refit the model ##R^2 = 0.6677
model <- lm(price~lotSize+age+landValue+livingArea+pctCollege+
                        bedrooms+fireplaces+bathrooms+rooms+heating+fuel+
                        sewer+waterfront+newConstruction+centralAir, data = train)   
summary(model)

#Cooks distance 2nd run
plot(model, which = 4, cook.levels = cutoff) #D values > cutoff
plot(model, which = 5, cook.levels = cutoff)

train <- train[-which(rownames(train)
                      %in% c("141", "437", "571")),]

##Refit the model ##R^2 = 0.6683
model <- lm(price~lotSize+age+landValue+livingArea+pctCollege+
                    bedrooms+fireplaces+bathrooms+rooms+heating+fuel+
                    sewer+waterfront+newConstruction+centralAir, data = train)   

summary(model)

#Cooks distance 3rd run
plot(model, which = 4, cook.levels = cutoff) #D values > cutoff
plot(model, which = 5, cook.levels = cutoff)

train <- train[-which(rownames(train)
                      %in% c("49", "1139", "1202")),]

##Refit the model
model <- lm(price~lotSize+age+landValue+livingArea+pctCollege+
                    bedrooms+fireplaces+bathrooms+rooms+heating+fuel+
                    sewer+waterfront+newConstruction+centralAir, data = train)   ##R^2 = 0.6683

summary(model)


#Multicolinearity removed heating/fuel (high vif)
vif(model)

##Refit the model ##R^2 = 0.6648
model <- lm(price~lotSize+age+landValue+livingArea+pctCollege+
                    bedrooms+fireplaces+bathrooms+rooms+
                    sewer+waterfront+newConstruction+centralAir, data = train)   

summary(model)
