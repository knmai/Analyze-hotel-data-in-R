library(tidyverse)
data("diamonds")
View(diamonds)
head(diamonds)
str(diamonds)
colnames(diamonds)
#Add column to data frame
mutate(diamonds, carat_2=carat* 100)

names <- c("Jake", "Bela", "Win", "Jane")
ages <- c(5,9,8,10)
people <- data.frame(names, ages)
view(people)
head(people)
glimpse(people)
str(people)
colnames(people)
mutate(people, age_in_20=ages+20)

fruits <- c("apple", "oranges", "mangoes", "kiwi", "grapes")
rating <- c(4,5,5,3,4)
fruit_ranks <- data.frame(fruits, rating)
view(fruit_ranks)
ggplot(data = fruit_ranks, aes(x=fruits, y=rating)) 


#working with tibbles
as_tibble(diamonds)
data(mtcars)


bookings_df <- read_csv("hotel_bookings.csv")
view(bookings_df)
str(bookings_df)
glimpse(bookings_df)
colnames(bookings_df)
as_tibble(bookings_df)

new_df <- select(bookings_df, `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dr`, adults)
new_df
view(new_df)
mutate(new_df, total = `adr` / adults)



























