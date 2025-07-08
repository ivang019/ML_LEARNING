#Chapter 2

#There are two major types of ML algorithms: Supervised and unsupervised. Supervised learning
#models are those in wich a ML model is scored and tuned against some sort of known quantity.
#unsupervised are those in which ML models  derives patterns and information from data determining
#the known quantity tuning parameter itself

#regression example
plot(y = mtcars$mpg, x = mtcars$disp, xlab = "Engine Size (cubic inches)",
     ylab = "Fuel Efficiency (Miles per Gallon)")
model <- lm(formula = mpg ~ disp, data = mtcars) #MCO
summary(model)
coef(model)[2] * 200 + coef(model)[1] #Calculate mpg for a car with disp = 200

#Training and testing data
summary(model)


#We make the segmentation between training and testing data with 80% an 20%

split_size = 0.8 #Define the share 
sample_size = floor(split_size * nrow(mtcars)) #total size of training data
set.seed(123) #Set the seed for get the same output
train_indices <- sample(seq_len(nrow(mtcars)), size = sample_size) #Define the dataframe of traing
train <- mtcars[train_indices, ]
test <- mtcars[-train_indices, ] #The complement is the other set


#We apply the model only to the training data

model2 <- lm(mpg ~ disp, data = train)
new.data <- data.frame(disp = test$disp) #Extract the vector from test data
test$output <- predict(model2, new.data) #Apply the model output to he tes data
sqrt(sum(test$mpg - test$output)^2/nrow(test)) #Calculate the Root Mean Square Error (RMSE)


#El error cuadrático medio (RMSE) se puede interpretar como la diferencia promedio +/- esperada entre un valor predicho y el valor real. 
#Es la desviación estándar de los residuos (la diferencia entre el valor observado y el valor predicho para una característica). 
#RMSE se mide en la misma unidad que el valor objetivo.

#Como ejemplo, supongamos que nuestro objetivo es predecir el valor del contrato y obtenemos RMSE = 1250. 
#Esto significa que, en promedio, el valor predicho difiere +/- 1,250 $ del valor real

#Classification models

#Logistic regression

  plot(x = mtcars$mpg, y = mtcars$am, xlab = "Fuel Efficiency (Miles per Gallon)",
       ylab = "Vehicle Transmission Type (0 = Automatic, 1 = Manual)")
  
  #We can found if a vehicle has authomatic or manual transmission type in function of the
  #fuel efficiency (mpg)
  
  #we separete the training and testing data
  
  Label.train = train[, 9]
  Data.train = train[, -9]
  
  #We build the model with the training data and predict with the testing
  
  model = LogitBoost(Data.train, Label.train) 
  Data.test = test
  Lab = predict(model, Data.test, type = "raw")
  data.frame(row.names(test), test$mpg, test$am, Lab)
  ##
#The mpg is not the best way to predict the type of tranmission
  
#Supervised clustering methods

#Clustering is when you have a set of data and want to define classes based on how
# closely they are grouped. Sometimes, groupings of data might not be inmediatly
#obvious, and clustering algorithms can help to find patters. One of the most
#popular forms of classification is kmeans
  
#We visualize iris width as function of petal length
  
plot(x = iris$Petal.Length, y = iris$Petal.Width, xlab = "Petal Length",
       ylab = "Petal Width")

#We want to try to find three distinct groups in wich to classify this dataset
#We use kmeans

data = data.frame(iris$Petal.Length, iris$Petal.Width) #Create dataset
iris.kmeans <- kmeans(data, 2) #Apply kmeans to find two groups
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans$cluster,
     xlab = "Petal Length", ylab = "Petal Width") #graph width vs length with the cluster shape
points(iris.kmeans$centers, pch = 8, cex = 2) #graph with centers points

#We can disinct the two obviously groups, but we find an outlier.
#So, We apply the kmean function with three groups 

iris.kmeans3 <- kmeans(data, 3) #Apply again the code
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster,
     xlab = "Petal Length", ylab = "Petal Width") #Graph agian
points(iris.kmeans3$centers, pch = 8, cex = 2)

#You have to find the apropiate number of groups, to few underfit, to much made simply


#Actually, We have a data variable called "species" that sparate the data in
# groups by characteristics

par(mfrow = c(1, 2))
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster,
     xlab = "Petal Length", ylab = "Petal Width", main = "Model Output") #Clustering plot
plot(x = iris$Petal.Length, y = iris$Petal.Width,
     pch = as.integer(iris$Species),
     xlab = "Petal Length", ylab = "Petal Width", main = "Actual Data") #Actual data

table(iris.kmeans3$cluster, iris$Species) #We join the datasets in a table

#We only have six predictions from 150 that no fix with the model, this is a 
#effectiveness of 96%


#Mixed methods. Now, we will talk about mixed methods, that combines regression
#and classification methods.

#Example of tree

library(party)
tree <- ctree(mpg ~ ., data = mtcars)
plot(tree)

#We use the train data to create another tree

tree.train <- ctree(mpg ~ ., data = train)
plot(tree.train)


test$mpg.tree <- predict(tree.train, test)
test$class <- predict(tree.train, test, type = "node")
data.frame(row.names(test), test$mpg, test$mpg.tree, test$class)

#Random Forests

#This excersice is just a example of the intuition about the result of
#use random forest. The graph shows that when you have more trees you error
# is lower

mtcars.rf <- randomForest(mpg ~ ., data = mtcars, ntree = 1000,
                          keep.forest = FALSE, importance = FALSE) #A random forest with 1000 trees
plot(mtcars.rf, log = "y", title = "")

#Neural networks

#This is another easy example of what is a neural network. Neural networks
#needs inputs layer, that reflects the features, the hidden layer that is the number 
#of nodes to computes some function of each feature; and the output layer that is a final 
#processing node (matbe a function)

set.seed(123)

iris.nn <- nnet(Species ~ ., data = iris, size = 2) # A neural network with 2 hidden layers
                                            
table(iris$Species, predict(iris.nn, iris, type = "class")) #Apply the network output to assign the class


#Support Vector Machines

#This is a similar but different model to the logistical regression. 
#It is recommend use logit models when the number of features y larger than
#the number of observations, else you should use SVM model

#In this example, the output is very similar to the output of the neuronal network model

library(e1071)
iris.svm <- svm(Species ~ ., data = iris)
table(iris$Species, predict(iris.svm, iris, type = "class"))


#Unsupervised Clustering Methods

#Unsupervised Clustering Methods

#In the next example the data doesn´t have a explicit categorical label.
#You can use the kmeans clustering algorithm

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2), matrix(rnorm(100,
                                                                mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y") #Here We built a random dataset with two noraml distributions
plot(x)

cl <- kmeans(x, 2)
plot(x, pch = cl$cluster) #The clusteritazion shows the perfect segmentation between both distributions


cl[2] #gives the centers of each cluster



