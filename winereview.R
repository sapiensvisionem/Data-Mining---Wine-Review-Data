full_data <- read.csv("../input/winemag-data-130k-v2.csv")
str(full_data)
print(colSums(is.na(full_data)))
full_data <- full_data[!is.na(full_data$price), ]
unique_vals <- lapply(full_data, unique)
sapply(unique_vals, length)

ggplot(data = new_data, aes(x= wordcount))+
  geom_histogram(binwidth = 3)+
  labs(x = "Word Count", y= "Frequency", title = "Distribution of word count of description") #slightly right skewed

ggplot(data = new_data, aes(x= wordcount, y= wine_type, fill = wine_type))+
  geom_density_ridges ()+
  labs(x = "Word Count", title = "Distribution of word count of description")+
  scale_fill_cyclical(values = c("#CC3300", "#FFCC00"))

ggplot(data = new_data, aes(x=wordcount, y=points))+
  geom_point()

cor(new_data$points, new_data$wordcount)


ggplot(data = full_data, aes(x= points, colour = I('black'), fill = I('#099DD9')))+
  geom_histogram(binwidth = 1)+
  labs(x = "Points", y= "Frequency", title = "Distribution of points")

ggplot(data = full_data, aes(x= price, colour = I('black'), fill = I('#099DD9')))+
  geom_histogram()+
  labs(x = "Price", y= "Frequency", title = "Distribution of prices") #Strongly right skewed

ggplot(data = full_data, aes(x= log(price), colour = I('black'), fill = I('#099DD9')))+
  geom_histogram()+
  labs(x = "log(Price)", y= "Frequency", title = "Distribution of log(prices)") #slightly right skewed

word_reg <- lm(points ~ wordcount, data = new_data)
summary(word_reg)
par(mfrow = c(2,2))
plot(word_reg)

Price_points_reg <- lm(log(price) ~ points + wine_type, data = new_data)
summary(Price_points_reg)
par(mfrow = c(2,2))
plot(Price_points_reg)
