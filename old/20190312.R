library(tidyverse)
library()

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

saveRDS(board_games, "Documents/003_sideProjects/tidyTuesday/20190319.Rds")

cat <- board_games %>% 
    separate_rows(category, sep = ",") %>% 
    count(category) %>% 
    arrange(desc(n)) %>% 
    top_n(10) %>% 
    pull(category) 


board_games %>% 
    separate_rows(category, sep = ",") %>% 
    filter(category == "Card Game")

board_games %>% 
    separate_rows(category, sep = ",") %>% 
    group_by(category) %>% 
    summarise(Max = max(playing_time),
              Min = min(playing_time)) %>% 
    filter(Min != 0) %>% 
    gather(key, value, -category) %>% 
    ggplot(aes(category, value)) +
    geom_point()


board_games %>% 
    separate_rows(category, sep = ",") %>% 
    group_by(category) %>%
    filter(category %in% as_factor(cat)) %>% 
    ggplot(aes(fct_reorder(category, playing_time, median), playing_time, color = mean(average_rating))) +
    geom_boxplot() +
    coord_flip() +
    scale_y_log10()
