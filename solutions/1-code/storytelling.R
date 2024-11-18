library(babynames)
library(ggplot2)
library(dplyr)
library(ggtext)

babynames |>
  filter(name %in% c("Mark") & sex=='M') |>
  ggplot(aes(x = year, y = n)) +
  geom_line(size = 2, color = '#38686a') +
  labs(
    title = "<span style='color:#38686a;'>**Mark**</span> peaked in the 1960s",
    subtitle = "Baby name popularity over time",
    x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_text(size = 12, color = "grey50")
  )
