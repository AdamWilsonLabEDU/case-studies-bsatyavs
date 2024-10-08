data(iris)
#print dataset
print(iris)

#calculate the mean of Petal.length
petal_length_mean <- mean(iris$Petal.Length)
print(petal_length_mean)

#plot the histogram of petal.length
hist(iris$Petal.Length)

#install ggplot2 library
#Plot the Geom histogram of petal.length
ggplot(data = iris, aes(x = Petal.Length)) +
  geom_histogram(color = "white", fill = "red")

summary(iris)

ggplot(data = iris, aes(x = Petal.Length)) +
  geom_histogram(bins=50)


