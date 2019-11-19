library(tidyverse)
library(ggthemes)

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

jobs_gender %>% 
    select(year:workers_female) %>% 
    mutate(workers_male = workers_male * -1) %>% 
    gather(workers, number, -(year:total_workers)) %>%
    mutate(workers = as_factor(workers),
           workers = fct_rev(workers)) %>% 
    group_by(major_category, minor_category, workers) %>%
    summarise(number = mean(number)) %>% 
    spread(workers, number) %>% 
    mutate(col = ifelse(workers_female > abs(workers_male), "F", "M")) %>% 
    gather(workers, number, -c(major_category, minor_category, col)) %>% 
    ungroup() %>% 
    group_by(minor_category) %>%
    arrange(minor_category) %>% 
    mutate(number2 = abs(number),
           col = case_when(number2 == min(number2) ~ "A", 
                           TRUE ~ "B")) %>% 
    ggplot(aes(minor_category, number)) +
    geom_bar(stat = "identity", aes(fill = col), colour = "grey") +
    theme_fivethirtyeight() +
    coord_flip(clip = "off") +
    scale_fill_manual(values=c("A" = "#f7f7f7", "B" = "#2166ac")) +
    #annotate("text", y = -400000, x = 24, label = "MALE") +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    labs(title = "Total earnings",
         subtitle = "Plot shows the difference in earnings between\nmale and female in different job categories.",
         caption = "Dark blue bars indicate the higher earnings between the two gender.\n\nData: US Census Bureau | @marpello1980") +
    theme(plot.margin = unit(c(1,1,5,1), "lines"),
          legend.position="none") +
    #annotation_custom(text_low,xmin=-4,xmax=-4,ymin=-500000,ymax=-500000) +
    annotate("text", label = "MALE",x = 25, y = -200000, fontface = 2, size = 8) +
    annotate("text", label = "FEMALE",x = 25, y = 200000, fontface = 2, size = 8) +
   # ylim(c(-4e+05, 4e+05)) +
    scale_y_continuous(breaks = seq(-4e+05, 4e+05, 1e+05),
                         labels = paste0(c("40", "30", "20", "10", "0", "10", "20", "30", "40"), "K $")) +
    scale_x_discrete(expand = expand_scale(add = 3)) +
    geom_hline(yintercept = 0, linetype = "dashed")
    

ggsave("delete.pdf", units = "cm", width = 40, height = 21, dpi = 300)

