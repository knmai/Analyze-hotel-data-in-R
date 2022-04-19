library(tidyverse)
library(skimr)
library(janitor)

#importing data
hotel_bookings <- read_csv("hotel_bookings.csv")

head(hotel_bookings)

glimpse(hotel_bookings)
#There are 32 columns, the arrival date month is a double type data so false

#arranging data from ascending or descending order
arrange(hotel_bookings,desc(lead_time))

#helps view the table
ldtm_tbl <- arrange(hotel_bookings,desc(lead_time))
view(ldtm_tbl) #highest lead time is 737

#sorting data without using arrange
max(hotel_bookings$lead_time)
min(hotel_bookings$lead_time)
mean(hotel_bookings$lead_time)
median(hotel_bookings$lead_time)
min(lead_time)


hotel_summary <- 
  hotel_bookings %>%
  group_by(hotel) %>%
  summarise(average_lead_time=mean(lead_time),
            min_lead_time=min(lead_time),
            max_lead_time=max(lead_time))



#               Palmer penguins
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("tidyverse")
library(tidyverse)
data("penguins")
view(penguins)

ggplot(data = penguins)+geom_point(mapping = aes(x=bill_length_mm, y=bill_depth_mm, color = island, size=body_mass_g))

zlboston <- read.csv("zillow-boston.csv")
glimpse(zlboston)
ggplot(data="zillow-boston.csv") + geom_point(mapping=aes(x=postcode, y=price, color=bedroom_number))

hotel_bookings<- read.csv("hotel_bookings.csv")
glimpse(hotel_bookings)
ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))

ggplot(data = penguins)+
  geom_smooth(mapping = aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(mapping = aes(x=flipper_length_mm, y=body_mass_g))

ggplot(data = penguins)+
  geom_smooth(mapping = aes(x=flipper_length_mm, y=body_mass_g, linetype=species))

ggplot(data = penguins)+
  geom_smooth(mapping = aes(x=flipper_length_mm, y=body_mass_g))+
  geom_jitter(mapping = aes(x=flipper_length_mm, y=body_mass_g))
  
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = clarity))
  

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel, fill = market_segment))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type )

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_grid(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

data %>%
  filter(variable1 == "DS") %>%  
  ggplot(aes(x = weight, y = variable2, colour = variable1)) +  
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")

ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel, fill = market_segment))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel)) +
  facet_wrap(~ market_segment)

onlineta_city_hotels <- filter(hotel_bookings, 
                               (hotel=="City Hotel" & 
                                  hotel_bookings$market_segment=="Online TA"))
View(onlineta_city_hotels)

onlineta_city_hotels_v2 <- hotel_bookings %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")

ggplot(data = onlineta_city_hotels_v2) +
  geom_point(mapping = aes(x = lead_time, y = children))

ggsave("Example.png")

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="City hotel vs Resort hotel")

min(hotel_bookings$arrival_date_year)
mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", mindate, " to ", maxdate))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")

ggsave('hotel_booking_chart.png')
































































