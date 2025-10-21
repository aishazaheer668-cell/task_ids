#question no 2

# Salary Vector
salary <- c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000)

# Calculate Average Salary
average_salary <- mean(salary)
average_salary

# Age Vector
age <- c(34, 29, 40, 30, 35, 27, 41, 30)

# Find Minimum and Maximum Age
min_age <- min(age)
max_age <- max(age)

min_age
max_age

# Employee List
employee1 <- list(
  Name = "X",
  Department = "HR",
  Age = 34,
  Salary = 50000
)

# Display each element
employee1
employee1$Name
employee1$Department
employee1$Age
employee1$Salary

# Variables
salary <- c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000)
experience <- c(5, 3, 10, 4, 2, 7, 9, 6)

# Mean
mean_salary <- mean(salary)
mean_experience <- mean(experience)

# Standard Deviation
sd_salary <- sd(salary)
sd_experience <- sd(experience)

# Correlation
correlation <- cor(salary, experience)

# Display results
mean_salary
sd_salary
correlation

#question no 3

# Load dataset
data(mtcars)

# Scatter plot with trend line
plot(mtcars$hp, mtcars$mpg,
     main = "Relationship between Horsepower and Miles per Gallon",
     xlab = "Horsepower (hp)",
     ylab = "Miles per Gallon (mpg)",
     pch = 19, col = "blue")

# Add a trend line (regression line)
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)

# Box plot of mpg grouped by cylinder count
boxplot(mpg ~ cyl, data = mtcars,
        main = "MPG Distribution by Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles per Gallon (mpg)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Histogram of car weights
hist(mtcars$wt,
     breaks = 10,                       # Custom number of bins
     col = "orange",
     main = "Distribution of Car Weights",
     xlab = "Weight (1000 lbs)",
     ylab = "Frequency",
     border = "brown")