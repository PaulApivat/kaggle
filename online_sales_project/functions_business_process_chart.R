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
    select(month_year, total_orders, Year, Month) 

## FUNCTION ----
data <- total_order_by_year

create_ymd_function <- function(data) {
    data %>%
        mutate(
            Day = 1,
            month_year = paste(Year, Month, Day),
            month_year = month_year %>% ymd(),
            month = month(month_year)
        )
}

# NOTE: data must have Year and Month columns
create_ymd_function(data)

# data visualization: general ----
## FUNCTIONS: Plots ----
total_order_by_year %>%
    ggplot(aes(x = month_year, y = total_orders)) +
    geom_line()


create_line_chart <- function(data){
    data %>%
    ggplot(aes(x = month_year, y = total_orders)) +
    geom_line()   
}

# NOTE: data must have Year and Month columns
create_line_chart(data)

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

## FUNCTIONS: create necessary columns for BPC ----

create_bpc_columns <- function(data){
    bpc_data <- data %>%
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
    
    return(bpc_data)
}

create_bpc_columns(data)

bpc_data <- create_bpc_columns(data)

# data visualization: business process chart ----

business_process_chart_total_orders_viz <- business_process_chart_data %>% 
    ggplot(aes(month_year, total_orders)) +
    geom_line() +
    geom_hline(yintercept = business_process_chart_data$avg_orders, color = 'green') +
    geom_hline(yintercept = business_process_chart_data$unpl, color = 'red', linetype = 'dashed') +
    geom_hline(yintercept = business_process_chart_data$lnpl, color = 'red', linetype = 'dashed') +
    geom_hline(yintercept = business_process_chart_data$upper_25, color = 'orange') +
    geom_hline(yintercept = business_process_chart_data$lower_25, color = 'orange') +
    # break x-axis into quarters
    scale_x_date(breaks = '3 month') +
    # note: place before theme()
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Total Orders: Business Process Chart",
        subtitle = "2017 - 2019",
        x = "",
        y = "Total Orders",
        caption = "4th Quarter of 2018 indicates first time surpassing Upper 25% in Total Sales"
        ) 

# add annotation
# note: x-axis Date data type
business_process_chart_total_orders_viz +
    annotate("text", x = as.Date("2017-02-01"), y = 170, color = 'red', label = "UNLP") +
    annotate("text", x = as.Date("2017-02-01"), y = 25, color = 'red', label = "LNLP") +
    annotate("text", x = as.Date("2017-02-01"), y = 137, color = 'orange', label = "Upper 25%") +
    annotate("text", x = as.Date("2017-02-01"), y = 105, color = 'green', label = "Avg = 97")

## FUNCTION: Visualize BPC w/ annotation ----

create_bpc_visualization <- function(data){
    data %>%
        ggplot(aes(month_year, total_orders)) +
        geom_line() +
        geom_hline(yintercept = business_process_chart_data$avg_orders, color = 'green') +
        geom_hline(yintercept = business_process_chart_data$unpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = business_process_chart_data$lnpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = business_process_chart_data$upper_25, color = 'orange') +
        geom_hline(yintercept = business_process_chart_data$lower_25, color = 'orange') +
        # break x-axis into quarters
        scale_x_date(breaks = '3 month') +
        # note: place before theme()
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Total Orders: Business Process Chart",
            subtitle = "2017 - 2019",
            x = "",
            y = "Total Orders",
            caption = "4th Quarter of 2018 indicates first time surpassing Upper 25% in Total Sales"
        ) +
        annotate("text", x = as.Date("2017-02-01"), y = 170, color = 'red', label = "UNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = 25, color = 'red', label = "LNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = 137, color = 'orange', label = "Upper 25%") +
        annotate("text", x = as.Date("2017-02-01"), y = 105, color = 'green', label = "Avg = 97")
}

create_bpc_visualization(bpc_data)
