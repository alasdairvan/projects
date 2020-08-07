# During which months are forest fires most common?
#On which days of the week are forest fires most common?
# To answer these questions, create two bar charts:
#One showing the number of forest fires occuring during each month
#Another showing the number of forest fires occurring on each day of the week
#install packages
install.packages("rmarkdown")

#load libraries
library(dplyr)
library(ggplot2)
library(purrr)

#Group by month&day
forestfires_month <- forestfires %>% 
    group_by(month) %>%
    summarize(total = n())
forestfires_day <- forestfires %>%
    group_by(day) %>%
    summarize(total = n())

# change data type of month&day to factor

forestfires <- forestfires %>%
    mutate(month = factor(month, levels = c("jan", "feb", "mar", 
                                            "apr", "may", "jun", "aug", "sep", "oct", "nov", 
                                            "dec")),
           day = factor(day, levels = c("sun", "mon", "tue", "wed", 
                                        "thu", "fri", "sat"))
    ) 

# create barplots

month_barplot <- ggplot(data = forestfires_month,
       aes(x = month, y = total)) +
        geom_bar(stat = 'identity', fill = "orangered1") + 
        theme(panel.background = element_rect(fill = "white")
        ) +
        labs(
          title = "Forest Fires by Month",
          x = "Month",
          y = "Number of Fires"
          )

fires_barplot + theme(plot.title = element_text(hjust = 0.5))

day_barplot <- ggplot(data = forestfires_day,
           aes(
               x = day, y = total
               )
       ) +
        geom_bar(
            stat = 'identity', fill = "orangered1"
        ) + 
        labs(
            title = "Forest Fires by Month",
            x = "Day",
            y = "Number of Fires"
        ) +
        theme(
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5)
            )

# create functions for month&day boxplots

create_boxplot <- function(x,y) {
    ggplot(data = forestfires,
           aes_string(x = x, y = y)) + 
        geom_boxplot() +
        theme(panel.background = element_rect(fill = "white"))
}

# create variables 

x_days <- names(forestfires_day)[1]
x_months <- names(forestfires_month)[1]
y_var <- names(forestfires)[5:12]

#use functionals to make boxplots

boxplots_days <- map2(x_days, y_var, create_boxplot)
boxplots_months <- map2(x_months, y_var, create_boxplot)
# filter data frame by values of area

forestfires3 <- forestfires %>% filter(area > 1)
# create functions for scatterplots showing area(severity of fire damage) and diff variables 

create_scatter <- function (x,y) {
    ggplot(data = forestfires3,
           aes_string(x = x, y = y)) +
        ylim(0,200) +
        geom_point() +
        theme(panel.background = element_rect(fill = "white"))
}

# create variables 

x_scatter <- names(forestfires3)[5:12]
y_scatter <- names(forestfires3)[13]

# use functional to make scatter plots

scatter_fires <- map2(x_scatter, y_scatter, create_scatter)
scatter_fires
