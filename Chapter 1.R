#Chapter 1. We import the mtcars database.
####

mtcars %>% explore_tbl()
head(mtcars)

# As example of a static report, We make a graph if the distribution of vehicle
#fuel efficiency of our dataset. Morever, the static graph shows that the Top 3 
#vehicles fuel efficiency is Toyota corolla, Fiat 128 and Honda civic

#op <- par(mar = c(10, 4, 4, 2) + 0.1) #margin formatting

barplot(mtcars$mpg, names.arg = row.names(mtcars), las = 2, ylab = "Fuel
Efficiency in Miles per Gallon")

#Search for a relationship between the features

pairs(mtcars, lower.panel = NULL)

#We found a relationship between mpg and wt (efficiency and weight)
#We make zoom with a plot. The result is intuitive because to more weight more
#efforts is require to create move

plot(y = mtcars$mpg, x = mtcars$wt, xlab = "Vehicle Weight",
   ylab = "Vehicle Fuel Efficiency")

#Now we know that exist a relationship betwan mpf an wt, we can apply a
#mco (write mpg as function of mpg)

mt.model <- lm(formula = mpg ~ wt, data = mtcars)
summary(mt.model)

#Algorithms versus Models: What´s the difference?

#An algorithm is a set of steps performed in order

#A machine learning model like regression or clustering or neural networks relies on
#the workings of algorithms to help them run in the first place. Algorithms are the
#engine that underlie the simple R code that we run. They do all the heavy lifting of
#multiplying matrices, optimizing results, and outputting a number for us to use

#There are three major types of models: regression models,
#classification models, and mixed models that are a combination

#Methods of splitting up data for training and testing purposes are known as sampling
#techniques. These can come in many flavors like taking the top 40 rows of data as the
#training set, taking random rows from our data, or more advanced techniques.

#Some examples of cross-validation techniques in R include the following:
#• Bootstrap cross-validation
#• Bootstrap 632 cross-validation
#• k-fold cross-validation
#• Repeated cross-validation
#• Leave-one-out cross-validation

#The typical gold standard of cross-validation techniques is k-fold
#cross-validation, wherein you pick k = 10 folds against which to validate. This is the
#best balance between efficient data usage and avoiding splits in the data that might be
#poor choices.
