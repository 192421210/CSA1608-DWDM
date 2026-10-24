# #Question1: Imagine that you have selected data from an All Electronics data warehouse for analysis. The dataset contains the prices of commonly sold items, rounded to the nearest dollar. The numbers have been sorted as follows:
# 
# 1, 1, 5, 5, 5, 5, 5, 8, 8, 10, 10, 10, 10, 10, 12, 14, 14, 15, 15, 15, 15, 15, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30
# 
# Perform the following:
#   
#   Divide the dataset into equal-frequency bins of size 3.
# 
# Apply data smoothing using bin means and bin boundaries.
# 
# Plot a histogram for the above frequency distribution.

# Dataset
data <- c(1, 1, 5, 5, 5, 5, 5, 8, 8,
          10, 10, 10, 10, 10, 12, 14, 14,
          15, 15, 15, 15, 15, 18, 18, 18,
          18, 18, 20, 20, 20, 20, 20, 20,
          21, 21, 21, 21, 25, 25, 25, 25,
          25, 28, 28, 30)
bins <- split(data, ceiling(seq_along(data) / 3))
print(bins)
mean_smoothing <- lapply(bins, function(x) {
  rep(mean(x), length(x))
})
print(mean_smoothing)
boundary_smoothing <- lapply(bins, function(x) {
  low <- min(x)
  high <- max(x)
  sapply(x, function(value) {
    if (abs(value - low) <= abs(value - high)) {
      low
    } else {
      high
    }
  })
})
print(boundary_smoothing)
hist(data,
     main = "Histogram of Item Prices",
     xlab = "Price",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")


# #Question2: Imagine that you have selected data from an All Electronics data warehouse for analysis. The dataset contains the prices of commonly sold items, rounded to the nearest dollar. The numbers have been sorted as follows:
# 
# 1, 1, 5, 5, 5, 5, 5, 8, 8, 10, 10, 10, 10, 10, 12, 14, 14, 15, 15, 15, 15, 15, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30
# 
# Perform the following:
#   
#   Divide the dataset into equal-frequency bins of size 3.
# 
# Apply data smoothing using bin means and bin boundaries.
# 
# Plot a histogram for the above frequency distribution.

# Dataset
data <- c(1, 1, 5, 5, 5, 5, 5, 8, 8,
          10, 10, 10, 10, 10, 12, 14, 14,
          15, 15, 15, 15, 15, 18, 18, 18,
          18, 18, 20, 20, 20, 20, 20, 20,
          21, 21, 21, 21, 25, 25, 25, 25,
          25, 28, 28, 30)
bins <- split(data, ceiling(seq_along(data) / 3))
print(bins)
mean_smoothing <- lapply(bins, function(x) {
  rep(mean(x), length(x))
})
print(mean_smoothing)
boundary_smoothing <- lapply(bins, function(x) {
  low <- min(x)
  high <- max(x)
  sapply(x, function(value) {
    if (abs(value - low) <= abs(value - high)) {
      low
    } else {
      high
    }
  })
})
print(boundary_smoothing)
hist(data,
     main = "Histogram of Item Prices",
     xlab = "Price",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

# #Question 3: Comparison of Class Marks
# 
# Question: Two Maths teachers are comparing how their Year 9 classes performed in the end-of-year exams. Their results are as follows:
#   
#   Class A: 76, 35, 47, 64, 95, 66, 89, 36, 84
# 
# Class B: 51, 56, 84, 60, 59, 70, 63, 66, 50
# 
# Find which class had the higher mean, median and range.

Plot the above in a boxplot and give the inferences.
A <- c(76, 35, 47, 64, 95, 66, 89, 36, 84)
B <- c(51, 56, 84, 60, 59, 70, 63, 66, 50)
mean(A)
mean(B)
median(A)
median(B)
range(A)
range(B)
boxplot(A, B,
        names = c("Class A", "Class B"),
        main = "Comparison of Class Marks",
        ylab = "Marks",
        col = c("lightblue", "lightgreen"))

#Question 4: Data Normalization

# Question: Consider an example to make the calculation method clear. Assume that the minimum and maximum values for feature F are $50,000 and $100,000, respectively. It needs to range from 0 to 1. In accordance with min-max normalization, v=$80,000.
# 
# Normalize the value using min-max normalization.
# 
# Use the following data to perform:
#   
#   (a) Min-max normalization by setting minimum = 0 and maximum = 1.
# 
# (b) Z-score normalization.

#Data: 200, 300, 400, 600, 1000.
# Example: Min-max normalization
v <- 80000
min_value <- 50000
max_value <- 100000

normalized_v <- (v - min_value) /
  (max_value - min_value)

print(normalized_v)

# Dataset
data <- c(200, 300, 400, 600, 1000)
min_max <- (data - min(data)) /
  (max(data) - min(data))
print(min_max)
z_score <- (data - mean(data)) / sd(data)
print(z_score)

# Question5: Make a histogram for the AirPassengers dataset, starting at 100 on the x-axis and using values from 200 to 700, with bins 150 wide.

data <- AirPassengers
hist(data,
     breaks = seq(100, 700, by = 150),
     xlim = c(100, 700),
     main = "Histogram of AirPassengers",
     xlab = "Number of Passengers",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

#Question 6: Obtain multiple lines in the same plot using the mtcars dataset and the attributes mpg and qsec.

data(mtcars)
plot(mtcars$mpg,
     type = "l",
     col = "blue",
     ylim = range(c(mtcars$mpg, mtcars$qsec)),
     xlab = "Car Index",
     ylab = "Values",
     main = "MPG and QSEC")

lines(mtcars$qsec, col = "red", lty = 2)
legend("topright",
       legend = c("MPG", "QSEC"),
       col = c("blue", "red"),
       lty = c(1, 2))

# Question 7 : Download the dataset water from the datasets package in R. Find out whether there is a linear relationship between the attributes mortality and hardness by plotting a scatterplot. Fit the data into a linear regression model and predict the mortality for hardness = 88.
# getwd()
file.exists("water.csv")
data <- read.csv("water.csv")
names(data)
head(data)
plot(data$hardness, data$mortality,
     xlab = "Hardness",
     ylab = "Mortality",
     main = "Hardness vs Mortality",
     pch = 19,
     col = "blue")
model <- lm(mortality ~ hardness, data = data)
abline(model, col = "red", lwd = 2)
summary(model)
predict(model,
        newdata = data.frame(hardness = 88))

# Question 8: Boxplot of MPG vs Cylinders
# Question: Create a boxplot graph for the relationship between mpg (miles per gallon) and cyl (number of cylinders) for the dataset mtcars available in the R environment.

data(mtcars)

boxplot(mpg ~ cyl,
        data = mtcars,
        main = "MPG vs Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon",
        col = "lightblue")

# Question 9: Boxplot for Tennis Players' Scores
# Question: Assume that the tennis coach wants to determine if any of his team players are scoring outliers. To visualize the distribution of points scored by his players, explain how he can develop the boxplot. Give a suitable example using boxplot visualization technique.

scores <- c(45, 50, 55, 60, 62, 65, 68,
            70, 72, 75, 80, 85, 95, 150)

boxplot(scores,
        main = "Tennis Players' Scores",
        ylab = "Points Scored",
        col = "lightgreen")

# Question 10: Diabetes Dataset – Scatterplot and Bar Chart
# Question: Implement using R language an analysis showing how age group affects blood pressure based on the diabetes dataset. Show it using a scatterplot and bar chart (BloodPressure vs Age), using the dataset diabetes.csv.

diabetes <- read.csv("diabetes.csv")
plot(diabetes$Age,
     diabetes$BloodPressure,
     main = "Blood Pressure vs Age",
     xlab = "Age",
     ylab = "Blood Pressure",
     pch = 19,
     col = "blue")
barplot(diabetes$BloodPressure,
        names.arg = diabetes$Age,
        main = "Blood Pressure by Age",
        xlab = "Age",
        ylab = "Blood Pressure",
        col = "lightblue",
        las = 2)