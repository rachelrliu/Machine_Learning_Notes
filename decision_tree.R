library(ggplot2)
library(rpart)
library(rpart.plot)

divide <- function(data,attr,value){
        #Divides a set on a spefific column(attr)
        #This function can only handel numeirc values
        set1 <- subset(data,data[,attr] >= value)
        set2 <- subset(data,data[,attr] < value)
        return(list(set1,set2))
}

entropy <- function(data,target_attr){
        #Calculates the entropy of the given data set for the target attribute.
        freq <- table(data[,target_attr])/nrow(data)
        data_entropy <- - sum(freq * log2(freq))
        return(data_entropy)
}

entropy_gain <- function(data,set1,set2,target_attr){
        #Calculates the Information Gain of the given data after splitting 
        sub_entropy <- nrow(set1)/nrow(data) * entropy(set1,target_attr) + 
                nrow(set2)/nrow(data) * entropy(set2,target_attr)
        return (entropy(data,target_attr) - sub_entropy)
 
}

best_cut_off <- function(data,attr,target_attr){
        #Find the best cut off value for a selected attribute by brutal force
        best_IG = 0.0
        start = min(data[,attr])
        end = max(data[,attr])
        by = 0.01
        for (i in seq(start,end,by)){
                set1 = divide(data,attr,i)[[1]]
                set2 = divide(data,attr,i)[[2]]
                IG = entropy_gain(data,set1,set2,target_attr)
                if(IG > best_IG){
                        best_IG = IG
                }
        }
        return(best_IG)
}

#-----------------------Feature Selecton--------------------------#
iris <- read.csv('HW1.csv')
pw <- best_cut_off(iris,'Petal_width','Species_No')
pl <- best_cut_off(iris,'Petal_length','Species_No')
sw <- best_cut_off(iris,'Sepal_length','Species_No')
sl <- best_cut_off(iris,'Sepal_width','Species_No')
plt_df <- data.frame(Features = colnames(iris)[2:5], IG = c(pw,pl,sw,sl))
plt_df

ggplot(plt_df) + geom_bar(aes(x = Features, y = IG),stat = 'identity',width = .5,alpha = .8) +
        ggtitle('Iris Features by Information Gain \n') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))


#----------------------------Model-------------------------------#
set.seed(111)
#split
train_index <- sample(1:nrow(iris),nrow(iris) * .7)
test_index <- -train_index
train <- iris[train_index,]
test <- iris[test_index,]

#fit the tree model using training dataset
#start with petal_width
dct1 <- rpart(Species_name ~ Petal_width + Petal_length + Sepal_width +Sepal_length, 
              data = train, method = 'class',
              control = rpart.control(parms = list(split = "information"))) 
p1 <- predict(dct1,test,type = 'class')
cat('Tree1 *accuracy: ',sum(p1 == test[,6])/nrow(test))


#start with petal_length
dct2  <- rpart(Species_name ~ Petal_length + Petal_width + Sepal_width +Sepal_length, 
               data = train, method = 'class',
               control = rpart.control(parms = list(split = "information"))) 
p2 <- predict(dct2,test,type = 'class')
cat('Tree2 *accuracy: ',sum(p2 == test[,6])/nrow(test))


#start with petal_length, reduce minimal split to 10
dct3  <- rpart(Species_name ~ Petal_length + Petal_width + Sepal_width +Sepal_length, 
               data = train, method = 'class',
               control = rpart.control(parms = list(split = "information"),
                                       minsplit = 10,
                                       cp = 0.001)) 
p3 <- predict(dct3,test,type = 'class')
cat('Tree3 *accuracy: ',sum(p3 == test[,6])/nrow(test))


#start with petal_length, reduce minimal split to 1
dct4  <- rpart(Species_name ~ Petal_length + Petal_width + Sepal_width +Sepal_length, 
               data = train, method = 'class',
               control = rpart.control(parms = list(split = "information"),
                                       minsplit = 1,
                                       cp = 0.001)) 
p4 <- predict(dct4,test,type = 'class')
cat('Tree4 *accuracy: ',sum(p4 == test[,6])/nrow(test))


#Normalization?
scaled_train <- cbind(data.frame(scale(train[,2:5])),Species_name = train[,6])
scaled_test <- cbind(data.frame(scale(test[,2:5])),Species_name = test[,6])

dct5  <- rpart(Species_name ~ Petal_length + Petal_width + Sepal_width +Sepal_length, 
               data = scaled_train, method = 'class',
               control = rpart.control(parms = list(split = "information"),
                                       minsplit = 10,
                                       cp = 0.001)) 
p5 <- predict(dct5,scaled_test,type = 'class')
cat('Tree5 *accuracy: ',sum(p5 == scaled_test[,5])/nrow(test)) #same


#best
rpart.plot(dct3,type = 4,extra = 101)
table(test[,6],p3)

