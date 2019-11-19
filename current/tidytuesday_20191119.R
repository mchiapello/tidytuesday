# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(jpeg)




# Load data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)
nz_bird <- tuesdata$nz_bird

a1 <- nz_bird %>%
    filter(!is.na(bird_breed)) %>%
    mutate(vote_rank = as.numeric(str_extract(vote_rank, "\\d$"))) %>%
    group_by(bird_breed, vote_rank) %>%
    summarise(s = sum(vote_rank)) %>%
    mutate(SUM = sum(s)) %>%
    arrange(desc(SUM), bird_breed, vote_rank, desc(s)) %>%
    ungroup %>%
    slice(1:25) %>%
    mutate(bird_breed = str_replace_all(bird_breed, "\\\u101", "a"),
           bird_breed = str_replace_all(bird_breed, "\\\u014d", "o"),
           bird_breed = str_replace_all(bird_breed, "\\\u016b", "u"),
           bird_breed = str_replace_all(bird_breed, "\\\u012b", "i"),
           color = ifelse(vote_rank == 5, "red", "black"))

# Birds pictures
tmp <- readJPEG("kaka.jpg")
kaka <- rasterGrob(tmp, interpolate=TRUE)
tmp <- readJPEG("BlackRobin.jpg")
black <- rasterGrob(tmp, interpolate=TRUE)
tmp <- readJPEG("kakapo.jpg")
kakapo <- rasterGrob(tmp, interpolate=TRUE)
tmp <- readJPEG("tui.jpg")
tui <- rasterGrob(tmp, interpolate=TRUE)
tmp <- readJPEG("Yellow-eyedPenguin.jpg")
yell <- rasterGrob(tmp, interpolate=TRUE)

title = "Most votes birds",
subtitle = "All the top five birds scored more points in lower category",

# KAKA
b1 <- a1 %>%
    filter(bird_breed == "Kaka") %>%
    ggplot(aes(x = vote_rank,
               y = s,
               fill = color)) +
    geom_col() +
    theme_base() +
    theme(legend.position = "none") +
    facet_wrap(~bird_breed) +
    labs(x = "Vote Rank",
         y = "") +
    annotation_custom(kaka, xmin=.5, xmax=2, ymin=5000, ymax=Inf)
# KAKAPO
b2 <- a1 %>%
    filter(bird_breed == "Kakapo") %>%
    ggplot(aes(x = vote_rank,
               y = s,
               fill = color)) +
    geom_col() +
    theme_base() +
    theme(legend.position = "none") +
    facet_wrap(~bird_breed) +
    labs(x = "Vote Rank",
         y = "") +
    annotation_custom(kakapo, xmin=.5, xmax=2, ymin=5000, ymax=Inf)
# Black Robin
b3 <- a1 %>%
    filter(bird_breed == "Black Robin") %>%
    ggplot(aes(x = vote_rank,
               y = s,
               fill = color)) +
    geom_col() +
    theme_base() +
    theme(legend.position = "none") +
    facet_wrap(~bird_breed) +
    labs(x = "Vote Rank",
         y = "") +
    annotation_custom(black, xmin=.5, xmax=2, ymin=5000, ymax=Inf)
# tui
b4 <- a1 %>%
    filter(bird_breed == "Tui") %>%
    ggplot(aes(x = vote_rank,
               y = s,
               fill = color)) +
    geom_col() +
    theme_base() +
    theme(legend.position = "none") +
    facet_wrap(~bird_breed) +
    labs(x = "Vote Rank",
         y = "") +
    annotation_custom(tui, xmin=.5, xmax=2, ymin=5000, ymax=Inf)
# Yellow-eyed penguin
b5 <- a1 %>%
    filter(bird_breed == "Yellow-eyed penguin") %>%
    ggplot(aes(x = vote_rank,
               y = s,
               fill = color)) +
    geom_col() +
    theme_base() +
    theme(legend.position = "none") +
    facet_wrap(~bird_breed) +
    labs(x = "Vote Rank",
         y = "") +
    annotation_custom(yell, xmin=.5, xmax=2, ymin=5500, ymax=Inf)

plot_grid(b1, b2, b3, b4, b5, ncol = 2)

























