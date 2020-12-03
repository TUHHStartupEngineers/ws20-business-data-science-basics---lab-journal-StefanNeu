# Load libraries ----

library(tidyverse)
library(readxl)

# Importing Files ----

bikes_tbl <- read_excel(path = "data-science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
 
# Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%

  # Separate location into city and state
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%

   select(quantity, price, order.date, city, state) 

# Sales by Location And Year----

library(lubridate)

sales_by_year_and_loc_tbl <- bike_orderlines_wrangled_tbl %>%

  # Add year and total price column
  mutate(total_price = quantity * price) %>%
  mutate(year = year(order.date)) %>%

  # Group by state AND year to make 12 individual plots for every state
  group_by(state, year) %>%

  # Remove some columns
  select(-quantity, -price, -order.date, -city) %>%

  # Since we grouped by state and year, we get the sales for every year and state
  summarize(sales = sum(total_price)) %>%

  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# Visualization

sales_by_year_and_loc_tbl %>%

  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales, fill = state)) +

  # Don't use manual colors, because facet_wrap will take care of it
  geom_col() +
  
  # Plots 12 individual figures for every state
  facet_wrap(~ state) + 

  # Note that the individual plots already have the state names as title, so legend is unneccessary
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year for the respective states",
    x = "Year", # Override defaults for x and y
    y = "Revenue"
  )
