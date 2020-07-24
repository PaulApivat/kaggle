# session info
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# library ----
library(tidyverse)
library(lubridate)
library(zoo)        # lagging difference (moving range)


# load: retail_sales2 ----
retail_sales2 <- read_csv("./csv/business.retailsales2.csv")
glimpse(retail_sales2)

str(retail_sales2)
View(retail_sales2)

# data manipulation: general ----

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


# data visualization: general ----

total_order_by_year %>%
    ggplot(aes(x = month_year, y = total_orders)) +
    geom_line()

# data manipulation: business process chart ----

# add average column, moving_range
# moving_range is lagging difference

business_process_chart_data <- total_order_by_year %>%
    mutate(
        avg_orders = mean(total_orders),
        # calculate lagging difference
        moving_range = diff(as.zoo(total_orders), na.pad=TRUE),
        # get absolute value
        moving_range = abs(moving_range),
        # change NA to 0
        moving_range = ifelse(row_number()==1, 0, moving_range),
        avg_moving_range = mean(moving_range),
        lnpl = avg_orders - (2.66*avg_moving_range),
        lower_25 = avg_orders - (1.33*avg_moving_range),
        upper_25 = avg_orders + (1.33*avg_moving_range),
        unpl = avg_orders + (2.66*avg_moving_range)
    )

business_process_chart_data %>% view()




