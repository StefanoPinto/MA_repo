
library(tidyverse)

metrics_dataset
pca_metric_dataset
pca_slope_dataset

# explained variance over time
pca_metric_dataset |> 
  select(essround, expl_var_pc1) |> 
  group_by(essround) |> 
  summarize(mean_expl_var_pc1 = mean(expl_var_pc1, na.rm = T)) |> 
  ggplot(aes(essround, mean_expl_var_pc1)) +
  geom_col() +
  scale_x_continuous(breaks = seq(2002, 2022, 2))

# loadings over time
pca_metric_dataset |> 
  select(-c(country, expl_var_pc1)) |> 
  gather(key, value, -essround) |> 
  group_by(essround, key) |> 
  summarize(value = mean(value, na.rm = T)) |> 
  ggplot(aes(key, value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~essround) +
  coord_flip() +
  labs(x = "", y = "Loading value") +
  scale_x_discrete(labels = c("imbgeco", "imueclt", "imwbcnt"))
  



# loadings over time again
pca_metric_dataset |> 
  select(-c(country, expl_var_pc1)) |> 
  gather(key, value, -essround) |> 
  group_by(essround, key) |> 
  summarize(value = mean(value, na.rm = T)) |> 
  ggplot(aes(essround, value, col = key)) +
  geom_point(size = 3) +
  geom_line(lwd = 1) + 
  ylim(c(0.55,0.60)) +
  labs(x = "Year", y = "Mean loading (PC1)") +
  scale_color_brewer(palette = "Set1",
                        labels = c(
                          "imbgeco",
                          "imueclt",
                          "imwbcnt")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = ""))


# loadings over time for each country (appendix)
pca_metric_dataset |> 
  select(-expl_var_pc1) |> 
  gather(key, value, -c(essround, country)) |> 
  ggplot(aes(essround, value, col = key)) +
  geom_point(size = 3) +
  geom_line(lwd = 1) +
  facet_wrap(~country, ncol = 8) + 
  ylim(c(0.55,0.60)) +
  labs(x = "Year", y = "loading (PC1)") +
  scale_color_brewer(palette = "Set1",
                     labels = c(
                       "imbgeco",
                       "imueclt",
                       "imwbcnt")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(title = "")) 



