library (dplyr)
library(purrr)
library(stringr)
library("ggplot2")

??group_by
num_of_countries <- world_happiness %>% group_by(Region) %>% 
  summarize(total = n()) 
num_of_countries
#tibble with Regions and total num of entries in each
avg_world_happiness <- world_happiness %>% group_by(Region) %>% 
  summarize(avg = mean(`Happiness Score`))  
#tibble with Regions and average happiness score
world_happiness %>% group_by(Region) %>% 
  summarize(difference_of_happiness_scores = max(`Happiness Score`) - min(`Happiness Score`)) 
#tibble with the difference of of happiness scores from highest and lowest countries
region_summ <- world_happiness %>% group_by(Region) %>% 
  summarize(avg = mean(`Happiness Score`),
            diff_of_scores = max(`Happiness Score`) - min(`Happiness Score`),
            Max = max(`Happiness Score`),
            Min = min(`Happiness Score`),
            "Num of Countries" = n(),
            diff_per_country = diff_of_scores/n())
#tibble with summary of all
proportion_of_country <- function(x) {
  x/sum(x)
}

proportion_of_country(num_of_countries$total)
ggplot(data = avg_world_happiness,
       aes(x = Region, y = avg, fill = "red")) +
      geom_col() +
      labs(title = "Average Happiness Score By Region", y = "Average Happiness Score")
help("geom_bar")
ggplot(data = world_happiness,
       aes(x =`Happiness Score`)) +
      geom_histogram(bins = 25) +
      facet_wrap(~Region, nrow = 4)
ggplot(data = world_happiness,
       aes(x =`Happiness Score`, fill = Region)) +
      geom_histogram(bins = 10)
ggplot(data = world_happiness,
       aes(x =Region, y = `Happiness Score`)) +
        geom_boxplot() +
        labs(title = "Happiness Scores by Region") +
        theme(panel.background = element_rect(fill = "white"))
 
