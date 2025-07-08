#Chapter 3.  Sampling Statistics and Model Training in R.

#Usually you need to make a sampling to separete your population in two groups:
# Training and testing data.

#Population (the whole collection or universe)

#sample (a portion og the population that we select for analysis)

# Bias. Sampling bias happens when the smapling distribution not matches with ppulation
#distrbution.

#First 1. Simple randome. 

iris.df <- data.frame(iris) #Call database 
sample.index <- sample(1:nrow(iris.df), nrow(iris) * 0.75, replace = FALSE) #Chosing 75% of ppulation with single observation
head(iris[sample.index, ])

summary(iris)
summary(iris[sample.index,]) #We comparate the stats between sample and pop

#Second 2. Stratified sample

library(fifer)
## Loading required package: MASS
summary(stratified(iris, "Sepal.Length", 0.7)) # We choose a random sample for this feature

summary(stratified(iris, c("Sepal.Width", "Petal.Width"), 0.7)) #Adding another feature

#Third 3. Systematic sample. 
sys.sample = function(N, n) {
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k * (n - 1), k)
}
systematic.index <- sys.sample(nrow(iris), nrow(iris) * 0.75)
summary(iris[systematic.index, ]) #


###Training and Testing


#Training and Test Sets: Regression Modeling 

#Full data

set.seed(123)
x <- rnorm(100, 2, 1) # Depended variable

y = exp(x) + rnorm(5, 0, 2) # Function
plot(x, y)
linear <- lm(y ~ x) # Linear regression
abline(a = coef(linear[1], b = coef(linear[2], lty = 2))) #Plotting coeficient

summary(linear)

#Apply to 70% of data
data <- data.frame(x, y) # Build a daaframe
data.samples <- sample(1:nrow(data), nrow(data) * 0.7, replace = FALSE) #Sample
training.data <- data[data.samples, ] #Define sample
test.data <- data[-data.samples, ]#Define test

train.linear <- lm(y ~ x, training.data) #Apply to sample
train.output <- predict(train.linear, test.data) #Apply to test

#Testing with RMSE
RMSE.df = data.frame(predicted = train.output, actual = test.data$y,
                     SE = ((train.output - test.data$y)^2/length(train.output))) #Calculating for each observation in the test

head(RMSE.df)

sqrt(sum(RMSE.df$SE)) # Calculate of RMSE

#We comparate against a quadratic regression model
train.quadratic <- lm(y ~ x^2 + x, training.data)
quadratic.output <- predict(train.quadratic, test.data)
RMSE.quad.df = data.frame(predicted = quadratic.output, actual = test.data$y,
                          SE = ((quadratic.output - test.data$y)^2/length(train.output)))
head(RMSE.quad.df)
sqrt(sum(RMSE.quad.df$SE))

#And with higher level

train.polyn <- lm(y ~ poly(x, 4), training.data)
polyn.output <- predict(train.polyn, test.data)
RMSE.polyn.df = data.frame(predicted = polyn.output, actual = test.data$y,
                           SE = ((polyn.output - test.data$y)^2/length(train.output)))
head(RMSE.polyn.df)
sqrt(sum(RMSE.polyn.df$SE))

#










