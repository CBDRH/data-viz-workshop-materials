# https://www.ahajournals.org/doi/full/10.1161/CIRCULATIONAHA.123.065770

df <- read.csv('raw-data/vis3.csv') |>
  mutate(
    order = n() - row_number(),
    inset=c(0, rep(c(0,1,1), 7), 1)
    )

dfHeader <- data.frame(
  y = rep(24, 6),
  x = exp(c(-6.2, -4.5:-1.5, 2.0)),
  row = c(
    'Subgroup',
    'PCI\nTotal N',
    'CABG\nTotal N',
    'PCI 3-yr\nEvent Rate',
    'CABG 3-yr\nEvent Rate',
    'Interaction\nP Value'
  )
)

ggplot(data = df) +
  geom_text(data=dfHeader,
            aes(x=x, y=y, label=row),
            vjust = 0, hjust = 1, size = 8/.pt, fontface = 'bold') +
  geom_text(
    aes(x = exp(-7) + .0002 * inset, y = order, label = Subgroup),
    size = 8/.pt, hjust = 0
    ) +
  geom_text(
    aes(x = exp(-4.5), y = order, label = PCI),
    size = 8/.pt, hjust = 1
  ) +
  geom_text(
    aes(x = exp(-3.5), y = order, label = CABG),
    size = 8/.pt, hjust = 1
  ) +
  geom_text(
    aes(x = exp(-2.5), y = order, label = ifelse(is.na(PCI3), '', format(PCI3, nsmall=0))),
    size = 8/.pt, hjust = 1
  ) +
  geom_text(
    aes(x = exp(-1.5), y = order, label = ifelse(is.na(CABG3), '', format(CABG3, nsmall=0))),
    size = 8/.pt, hjust = 1
  ) +
  geom_text(
    aes(x = exp(1.5), y = order, label = p),
    size = 8/.pt, hjust = 0
  ) +
  geom_segment(aes(x=exp(-7), y=23, xend=8, yend=23),
               color='grey30', linewidth=0.4) +
  geom_segment(aes(x=1, y=-Inf, xend=1, yend=23),
               color='grey30', linewidth=0.4) +
  geom_segment(aes(x=0.49, y=-Inf, xend=4.05, yend=-Inf),
               color='grey30', linewidth=1) +
  geom_errorbarh(aes(xmin=lcl, xmax=ucl, y=order),
                 height=0) +
  geom_point(aes(x=hr, y=order), shape=22, fill='#6c91cb') +
  annotate("text", x = 0.75, y = 0,
           label = "← Favours PCI", vjust = 10, hjust = 1, size = 6/.pt, color='grey30') +
  annotate("text", x = 1.5, y = 0,
           label = "Favours CABG →", vjust = 10, hjust = 0, size = 6/.pt, color='grey30') +
  scale_x_log10("Adjusted Hazard Ratio", breaks = c(0.5, 1, 2, 4)) +
  coord_cartesian(clip = "off") +  # Allow annotations outside the plot area
  theme(
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = 'grey30', size = 8, hjust=0.8),
    plot.margin = margin(t=10, r=2, b=20, l=2)
  )

ggsave(filename = paste0('plots/', Sys.Date(),"-forest-plot.png"),
       width = 7,
       height = 5,
       units = 'in')

