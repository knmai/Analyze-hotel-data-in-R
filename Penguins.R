bookings_df <- read_csv("hotel_bookings.csv")
trimmed_df <- bookings_df %>% 
  select(hotel ,is_canceled ,lead_time ) %>% 
  rename(hotel_type=hotel)
example_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")
head(bookings_df)

example_df <- bookings_df %>%
  mutate(guests = adults + children + babies)
head(example_df)
example_df <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled) , avg_lead_time = mean(lead_time))

help("arrange")
install.packages("Tmisc")
library(Tmisc)
data(quartet)
view(quartet)
