library(tidyverse)
library(data.table)
library(janitor)
library(ggrepel)

theme_set(theme_bw(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "white")))


data <- fread("git_log.csv")
data <- as_tibble(data)
data <- clean_names(data)
data <- data %>% mutate(author_date = as.Date(author_date))

# add days with 0 commits
commit_counts <- data %>% 
  group_by(author_date) %>% 
  summarize(count = n())

ggplot(commit_counts, aes(author_date, count)) +
  geom_line(col = "white") +
  geom_point(col = "white") +
  labs(x = "", y = "Commit count") +
  scale_y_continuous(breaks = seq(1, 10, 2)) 
  
  