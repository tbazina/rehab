# Visualization of DH parameters for soft robot kinematics for index/little fingers

# Tidyverse
library(tidyverse)

# Plotting
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggsci)
library(scales)
library(processx)

## Loading data
dh <- read_csv("DH_modified_soft_robot_little.csv") %>% 
  mutate(robot = "little") %>% 
  bind_rows(
    read_csv("DH_modified_soft_robot_index.csv") %>%  mutate(robot = "index")
    ) %>% 
  select(-`...1`) %>% 
  # convert th_values to degrees
  mutate(th_values = th_values * 180 / pi)

# FE plane plot
dh_lines_fe <- dh %>% 
  filter(robot == 'index') %>%
  # filter(robot == 'little') %>%
  filter(pressure %in% abs(0:7 - 0.01)) %>%
  arrange(pressure, joint)

dh %>%
  filter(robot == 'index') %>%
  # filter(robot == 'little') %>%
  ggplot(aes(y = z, x = x)) +
  facet_wrap(. ~ robot, scales = 'fixed') +
  coord_fixed() +
  geom_path(
    aes(color = pressure, group = joint),
    linewidth = 1.5,
    lineend = 'round'
  ) +
  geom_path(
    data = dh_lines_fe,
    aes(group = pressure),
    color = 'black',
    lineend = 'round',
    size = 0.3,
  ) +
  geom_point(
    data = dh_lines_fe,
    # aes(fill = th_mcp_fe),
    shape = 21,
    colour = "black",
    fill = 'black',
    size = 1.5,
    stroke = 0.05,
    alpha = 1.0
    ) +
  # Index annotations
  annotate(geom = 'text', x = 9, y = 20, label = 'Joint 1', size = 2.3) +
  annotate(geom = 'text', x = 6, y = 69, label = 'Joint 2', size = 2.3) +
  annotate(geom = 'text', x = 0, y = 108, label = 'Joint 3', size = 2.3) +
  annotate(geom = 'text', x = -9, y = 136, label = 'Tip', size = 2.3) +
  # Little annotations
  # annotate(geom = 'text', x = 9, y = 18, label = 'Joint 1', size = 2.3) +
  # annotate(geom = 'text', x = 8, y = 65, label = 'Joint 2', size = 2.3) +
  # annotate(geom = 'text', x = 6, y = 99, label = 'Joint 3', size = 2.3) +
  # annotate(geom = 'text', x = 0, y = 123, label = 'Tip', size = 2.3) +
  scale_y_continuous(
    name = expression(italic(z)*' [mm]'),
    breaks = seq(-20, 150, 20),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression(italic(x)*' [mm]'),
    expand = c(0, 8),
    breaks = seq(-100, 20, 20),
    # Change minus sign to en dash
    labels = ~sub("-", "\u2212", .x) 
    # limits = c(-4, 10),
    # trans = 'reverse'
    ) +
  scale_color_viridis_c(
    name = expression('pressure '*italic(p)*' [bar]'),
    breaks = seq(0, 6.99, length.out=8),
    labels = number_format(accuracy = 1.0),
    # values = c("dodgerblue3", "darksalmon")
  ) +
  scale_fill_nejm(
    # name = expression(theta[CMC]*','[FE]*' [°]')
    # values = c("dodgerblue3", "darksalmon")
  ) +
  theme_bw() + theme(
    text = element_text(size = 7),
    panel.border = element_rect(),
    panel.background = element_blank(),
    panel.spacing = unit(1, 'mm'),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 7, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    legend.direction = 'horizontal',
    legend.position = c(0.5,  0.18),
    legend.box.spacing = unit(1, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0.0, 0.0, 0.0, 0.5, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(4.5, 'mm'),
    legend.background = element_blank(),
    legend.title = element_text(size = 7),
    legend.title.position = 'top',
    axis.title = element_text(size = 7, margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(colour="black", size = 7),
    axis.text.y = element_text(colour="black", size = 7),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(
      hjust = 0.5, vjust = 0, size = 7,
      margin = margin(0, 0, 0.0, 0, 'mm')
      ),
    plot.margin = margin(0.1, 0.1, 0.1, 0.2, 'mm'),
  )

ggsave(
  filename = 'workspace_soft_actuator_index_fe_plane.png',
  # filename = 'workspace_soft_actuator_little_fe_plane.png',
  width = 8, height = 9.4, units = 'cm', dpi = 320, pointsize = 12)
