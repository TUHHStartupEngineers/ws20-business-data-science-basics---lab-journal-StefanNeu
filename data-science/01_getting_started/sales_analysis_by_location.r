# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
library(readxl)

# 2.0 Importing Files ----

bikes_tbl <- read_excel(path = "data-science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


# 3.0 Examining Data ----

orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
# 
# 
#   
# # 5.0 Wrangling Data ----
# 
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%

  # 5.1 Separate category name
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%

   select(quantity, price, order.date, city, state) 


# 6.0 Business Insights ----
# 6.1 Sales by Location ----

library(lubridate)

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%

  # Add year column
  mutate(total_price = quantity * price) %>%
  
  mutate(year = year(order.date)) %>%

  group_by(state) %>%
  
  select(-quantity, -price, -order.date, -city) %>%

  summarize(sales = sum(total_price)) %>%
# 
#   # Optional: Add a column that turns the numbers into a currency format
#   # (makes it in the plot optically more appealing)
#   # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))
#
# Step 2 - Visualize

sales_by_year_tbl %>%

  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +

 
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "State", # Override defaults for x and y
    y = "Revenue"
  )
# 
# 
# 
# 
# # 6.2 Sales by Year and Category 2 ----
# 
# # Step 1 - Manipulate
# sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
#   
#   # Select columns and add a year
#   select(order_date, total_price, category_1) %>%
#   mutate(year = year(order_date)) %>%
#   
#   # Group by and summarize year and main catgegory
#   group_by(year, category_1) %>%
#   summarise(sales = sum(total_price)) %>%
#   ungroup() %>%
#   
#   # Format $ Text
#   mutate(sales_text = scales::dollar(sales, big.mark = ".", 
#                                      decimal.mark = ",", 
#                                      prefix = "", 
#                                      suffix = " €"))
# 
# 
# 
# # Step 2 - Visualize
# sales_by_year_cat_1_tbl %>%
#   
#   # Set up x, y, fill
#   ggplot(aes(x = year, y = sales, fill = category_1)) +
#   
#   # Geometries
#   geom_col() + # Run up to here to get a stacked bar plot
#   
#   # Facet
#   facet_wrap(~ category_1) +
#   
#   # Formatting
#   scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
#                                                     decimal.mark = ",", 
#                                                     prefix = "", 
#                                                     suffix = " €")) +
#   labs(
#     title = "Revenue by year and main category",
#     subtitle = "Each product category has an upward trend",
#     fill = "Main category" # Changes the legend name
#   )
# 
# 
# # 7.0 Writing Files ----
# 
# # 7.1 Excel ----
# install.packages("writexl")
# library("writexl")
# bike_orderlines_wrangled_tbl %>%
#   write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 
# # 7.2 CSV ----
# bike_orderlines_wrangled_tbl %>% 
#   write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 
# # 7.3 RDS ----
# bike_orderlines_wrangled_tbl %>% 
#   write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
