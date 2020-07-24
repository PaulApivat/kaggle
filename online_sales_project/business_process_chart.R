# session info
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# library ----
library(tidyverse)
library(lubridate)

# load: retail_sales2 ----
retail_sales2 <- read_csv("./csv/business.retailsales2.csv")
glimpse(retail_sales2)

str(retail_sales2)
View(retail_sales2)

# data manipulation ----

# create column month_year
total_order_by_year <- retail_sales2 %>%
    mutate(
        Day = 1,
        month_year = paste(Year, Month, Day),
        month_year = month_year %>% ymd(),
        month = month(month_year)
    ) %>%
    rename(
        total_orders = `Total Orders`
    ) %>% 
    select(month_year, total_orders) 


# data visualization ----

total_order_by_year %>%
    ggplot(aes(x = month_year, y = total_orders)) +
    geom_line()

# add average ----
total_order_by_year %>% view()
