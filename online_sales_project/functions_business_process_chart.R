# session info
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# library ----
library(tidyverse)
library(lubridate)
library(zoo)        # lagging difference (moving range)
library(glue)       # string interpolation 


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

bpc_data

create_bpc_visualization(bpc_data)

# Four Functions ----

# Starting Point:
data
# In this order:

# FIRST
# NOTE: data must have Year and Month columns
create_ymd_function(data)

# SECOND
# NOTE: data must have Year and Month columns
create_line_chart(data)

# THIRD
create_bpc_columns(data)

# FOURTH
bpc_data <- create_bpc_columns(data)

# FIFTH
create_bpc_visualization(bpc_data)

## GROSS SALES ----
retail_sales2


gross_sales_year_month_data <- retail_sales2 %>%
    select(`Gross Sales`, Year, Month) %>%
    rename(gross_sales = `Gross Sales`)

# First
gross_sales_year_month_data2 <- create_ymd_function(gross_sales_year_month_data)

# Second

# note: function specific to 'gross_sales'
create_line_chart_gross_sales <- function(data){
    data %>%
        ggplot(aes(x = month_year, y = gross_sales)) +
        geom_line()
}

create_line_chart_gross_sales(gross_sales_year_month_data2)

# Third

# note: function specific to gross_sales
create_bpc_columns_gross_sales <- function(data){
    bpc_data <- data %>%
        mutate(
            avg_orders = mean(gross_sales),
            # calculate lagging difference
            moving_range = diff(as.zoo(gross_sales), na.pad=TRUE),
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

create_bpc_columns_gross_sales(gross_sales_year_month_data2)

# Fourth
gross_sales_bpc_data <- create_bpc_columns_gross_sales(gross_sales_year_month_data2)

# Fifth

create_bpc_visualization_gross_sales <- function(data){
    data %>%
        ggplot(aes(month_year, gross_sales)) +
        geom_line() +
        geom_hline(yintercept = gross_sales_bpc_data$avg_orders, color = 'green') +
        geom_hline(yintercept = gross_sales_bpc_data$unpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = gross_sales_bpc_data$lnpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = gross_sales_bpc_data$upper_25, color = 'orange') +
        geom_hline(yintercept = gross_sales_bpc_data$lower_25, color = 'orange') +
        # break x-axis into quarters
        scale_x_date(breaks = '3 month') +
        # note: place before theme()
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Gross Sales: Business Process Chart",
            subtitle = "2017 - 2019",
            x = "",
            y = "Gross Sales",
            caption = "----"
        ) +
        annotate("text", x = as.Date("2017-02-01"), y = gross_sales_bpc_data$unpl, color = 'red', label = "UNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = gross_sales_bpc_data$lnpl, color = 'red', label = "LNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = gross_sales_bpc_data$upper_25, color = 'orange', label = "Upper 25%") +
        annotate("text", x = as.Date("2017-02-01"), y = gross_sales_bpc_data$avg_orders, color = 'green', label = "Avg = 97")
}

create_bpc_visualization_gross_sales(gross_sales_bpc_data)

### NET SALES ----
# Objective: Write function for Net Sales that can generalize beyond Net Sales

retail_sales2

# Step 1: subset dataframe for Net Sales column ----

net_sales_year_month <- retail_sales2 %>%
    select(`Net Sales`, Year, Month) %>%
    rename(net_sales = `Net Sales`)

net_sales_year_month

# Step 1a: run create_ymd_function on net_sales_year_month ----
net_sales_year_month_2 <- create_ymd_function(net_sales_year_month)


# Step 2: Generalized function for creating line chart ----

# NOTE: create_line_chart_general takes data and two columns

create_line_chart_general <- function(dataset, col_name_1, col_name_2){
    col_name_1 <- enquo(col_name_1)
    col_name_2 <- enquo(col_name_2)
    dataset %>%
        ggplot(aes(x = !!(col_name_1), y = !!(col_name_2))) +
        geom_line()   
}

create_line_chart_general(net_sales_year_month_2, month_year, net_sales)

# Step 3: Generalized function for creating BPC columns ----

# NOTE: 

create_bpc_columns_general <- function(dataset, col_name){
    col_name <- enquo(col_name)
    bpc_data <- dataset %>%
        mutate(
            avg_orders = mean(!!(col_name)),
            # calculate lagging difference
            moving_range = diff(as.zoo(!!(col_name)), na.pad=TRUE),
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

net_sales_year_month_2

create_bpc_columns_general(net_sales_year_month_2, net_sales)

# Step 3a: Assign to net_sales_bpc_data ----

net_sales_bpc_data <- create_bpc_columns_general(net_sales_year_month_2, net_sales)

# Step 4: NOT General function for BPC Visualiation ----
library(glue)
# NOTE: NOT a general function

create_bpc_visualization_general <- function(dataset, col_x, col_y, col_avg, col_unpl, col_lnpl, col_upper_25, col_lower_25){
    col_x <- enquo(col_x) # month_year
    col_y <- enquo(col_y) # net_sales
    
    col_avg <- dataset$avg_orders
    col_unpl <- dataset$unpl
    col_lnpl <- dataset$lnpl
    col_upper_25 <- dataset$upper_25
    col_lower_25 <- dataset$lower_25
    
    dataset %>%
        ggplot(aes(x = !!(col_x), y = !!(col_y))) +
        geom_line() +
        geom_hline(yintercept = col_avg, color = 'green') +
        geom_hline(yintercept = col_unpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = col_lnpl, color = 'red', linetype = 'dashed') +
        geom_hline(yintercept = col_upper_25, color = 'orange') +
        geom_hline(yintercept = col_lower_25, color = 'orange') +
        
        # break x-axis into quarters
        scale_x_date(breaks = '3 month') +
        # note: place before theme()
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = glue('{names(dataset[,1])}: Business Process Chart'),
            subtitle = "2017 - 2019",
            x = "",
            y = glue('{names(dataset[,1])}'),
            caption = "----"
        ) +
        annotate("text", x = as.Date("2017-02-01"), y = col_unpl, color = 'red', label = "UNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = col_lnpl, color = 'red', label = "LNLP") +
        annotate("text", x = as.Date("2017-02-01"), y = col_upper_25, color = 'orange', label = "Upper 25%") +
        annotate("text", x = as.Date("2017-02-01"), y = col_avg, color = 'green', label = "Avg = 97")
    
}

create_bpc_visualization_general(net_sales_bpc_data, month_year, net_sales, avg_orders, unpl, lnpl, upper_25, lower_25)

names(net_sales_bpc_data[,1])

###

 +
    # note: place before theme()
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = glue('{dataset}: Business Process Chart'),
        subtitle = "2017 - 2019",
        x = "",
        y = glue('{dataset}'),
        caption = "----"
    ) +
    annotate("text", x = as.Date("2017-02-01"), y = col_unpl, color = 'red', label = "UNLP") +
    annotate("text", x = as.Date("2017-02-01"), y = col_lnpl, color = 'red', label = "LNLP") +
    annotate("text", x = as.Date("2017-02-01"), y = col_upper_25, color = 'orange', label = "Upper 25%") +
    annotate("text", x = as.Date("2017-02-01"), y = col_avg, color = 'green', label = "Avg = 97")




