###############################################################
# Project:  National Health Survey 2022 Analysis
# Purpose:  Plot self-assessed health data by state
# Inputs:   clean-data/vis1.Rda
# Outputs:  plots/yyyy-mm-dd-self-assessed-health.png
# Author:   Mark Hanly
###############################################################

###############
# Preparation #
###############

# Load libraries
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(colorspace)
library(ggnewscale)

# Define health status levels
healthStatus <- c("Poor", "Fair", "Good", "Very good", "Excellent")

# Load cleaned data
load('clean-data/vis1.Rda')

# Create variable indicating % very good or excelent health
df1Plot <- df1Clean |>
  group_by(state) |>
  slice(4:5) |>
  summarise(tot = sum(percent)) |>
  left_join(df1Clean, by = 'state')

# custom bar colours
barCols <- RColorBrewer::brewer.pal(5, 'RdYlGn')
# RColorBrewer::display.brewer.pal(name='RdYlGn', n=5) # To preview

# Custom colors for bar labels using the colorspace lighten() and darken functions
label_text_colors <- c('transparent',
                       darken(barCols[2], 0.4),
                       darken(barCols[3], 0.4),
                       darken(barCols[4], 0.4),
                       lighten(barCols[5], 0.6))

# Custom colors for legend text (includes red)
label_text_colors2 <- c(lighten(barCols[1], 0.6),
                        darken(barCols[2], 0.4),
                        darken(barCols[3], 0.4),
                        darken(barCols[4], 0.4),
                        lighten(barCols[5], 0.6))

# Data frame for legend
legendDf <- data.frame(
  y = rep(10.5, 5),
  x = 10 + 0:4 * 20,
  label = factor(healthStatus, levels=healthStatus),
  fill = barCols,
  color = label_text_colors2
)



#####################
# Plotting the data #
#####################

ggplot() +
  geom_col( # The horizontal columns
    data = df1Plot,
    aes(
      x=percent,
      y=fct_reorder(state, tot),
      group=status,
      fill=status),
    position = position_stack(reverse = TRUE),
    color = 'white'
  ) +
  geom_text( # The column labels
    data = df1Plot,
    aes(
      x=percent,
      y=fct_reorder(state, tot),
      label = paste0(format(percent, nsmall=1), '%'),
      color = status),
    position = position_stack(reverse = TRUE),
    size = 8/.pt, hjust = 1.1, show.legend = FALSE
  ) +
  geom_tile( # The legend box
    data = legendDf,
    aes(x=x, y=y, fill = label),
      show.legend = FALSE, height=0.65) +
      scale_fill_manual("",
      labels = healthStatus,
      values = barCols) +
  scale_color_manual("", values = label_text_colors) +
  new_scale_colour() +
  geom_text( # The legend text
    data = legendDf,
    aes(x=x, y=y, label = label, color=label),
    show.legend = FALSE, size = 8/.pt
  ) +
  scale_colour_manual("", values = label_text_colors2) +
  scale_y_discrete(NULL) +
  scale_x_continuous(NULL) +
  labs(
    title = 'Victorians most likely to rate their health as <span style="color: #1A9641;">**Excellent**</span> or <span style="color: #A6D96A;">**Very Good**</span>',
    subtitle = 'Self-assessed health status by states and territories',
    caption = 'Source: National Health Survey, 2022') +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(size=12, vjust=2),
    plot.subtitle = element_text(size=10, color = 'grey30', face = 'bold', vjust=2),
    plot.caption = element_text(size=8, color = 'grey60'),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size=8, color = 'grey30'),
    panel.background = element_blank(),
    legend.position = 'none'
  )

# Save the plot as .png
ggsave(filename = paste0('plots/', Sys.Date(),"-self-assessed-health.png"),
       width = 6,
       height = 4,
       units = 'in')

