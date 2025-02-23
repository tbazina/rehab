# FEM experiment little and index fingers constraint fitting
library(tidyverse)
library(magrittr)
library(ggsci)
library(xtable)
library(clipr)
library(datapasta)

# Load saved data for index and little finger
pos_data_wide <- 
  # Load index finger data
  read_csv('data/FEM_position_data_index_arc_approx_joint_elongation_angles_resample.csv') %>% 
  mutate(finger = 'index') %>% 
  # Bind rows with little finger data
  bind_rows(
    read_csv('data/FEM_position_data_little_arc_approx_joint_elongation_angles_resample.csv') %>% 
      mutate(finger = 'little')
  ) %>% 
  # Select finger, pressure, joint, theta_joint, delta_L
  select(finger, pressure, joint, theta_joint, delta_L)

# Plot revolut joints and lm with CI for all repetitions and fingers
plt_rev <- pos_data_wide %>%
  # Capitalize first letter of finger column
  mutate(finger = str_to_title(finger)) %>%
  mutate(joint = paste0('vartheta[', joint, ']')) %>%
  # joint as factor with
  mutate(joint = factor(joint, levels = unique(joint))) %>%
  # plot the data
  ggplot(aes(x = pressure*10, y = theta_joint, color=finger)) +
  facet_grid(~joint, scales = 'fixed', labeller = label_parsed) +
  # Smooth line with CI
  geom_smooth(
    aes(color = finger),
    method = 'lm',
    formula = y ~ 0 + x + I(x^2),
    se = F,
    # color = 'black',
    linewidth = 1.3,
    # show.legend = F
  ) +
  geom_point(
    # aes(color = finger),
    size = 0.3,
    stroke = 0.2,
    shape = 21,
    # shape = 16,
    color = 'black',
    fill = 'black',
    # fill = pal_nejm()(8)[1],
    show.legend = F
    ) +
  scale_color_nejm(
    labels = scales::label_parse(),
    alpha = 0.4
  ) +
  # scale_color_discrete(
  #   type = pal_nejm()(8)[c(8, 7)],
  # ) +
  scale_fill_nejm(
    labels = scales::label_parse(),
  ) +
  scale_x_continuous(
    breaks = seq(0, 3, 0.5),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0.04)
      ) +
  scale_y_continuous(
    breaks = seq(0, 90, 10),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0)
      ) +
  labs(
    x = expression("Pressure "*italic(p)*" [bar]"),
    y = expression("Bending angle "*italic(vartheta)*" [°]")
  ) +
  # Customize legend
  guides(
    # Remove legend for color
    color = guide_legend(
      title = 'Finger actuator:',
      byrow = F,
      reverse = F
    )
  ) +
  theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_text(
      size = 10, margin = margin(0, 0, 1, 0, 'mm')),
    legend.title.position = 'left',
    legend.box = 'horizontal',
    legend.direction = 'horizontal',
    legend.box.spacing = unit(0.5, 'mm'),
    legend.spacing.y = unit(0.5, 'mm'),
    legend.spacing.x = unit(0.5, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.key.spacing = unit(0.5, 'mm'),
    legend.key.size = unit(3, 'mm'),
    legend.text = element_text(
      colour="black", size = 9, margin = margin(0, 0., 1, 0, 'mm')
      ),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'mm'),
    panel.background = element_blank(),
    panel.spacing.y = unit(0, 'mm'),
    panel.spacing.x = unit(1.5, 'mm'),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_line(
    #   color = 'azure4', linewidth = 0.15, linetype = 'solid'
    #   ),
    axis.title = element_text(face="bold", size = 10),
    axis.text = element_text(
      color="black", size = 9, margin = margin(0.0, 0.0, 0.0, 0.0, 'mm'),
      ),
    # axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.),
    # Remove x axis text and title
    axis.line = element_line(linewidth = 0.2, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 5),
    panel.grid = element_line(colour = 'grey', linewidth = 0.1),
    panel.border = element_rect(linewidth = 0.2),
    strip.background = element_rect(linewidth = 0.01),
    strip.text = element_text(
      color = 'black', size = 10, margin = margin(b = 0.3, t = 0.3, unit='mm')
    ),
    axis.ticks = element_line(linewidth = 0.2),
    # axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.1, 'lines')
  )
plt_rev
ggsave(
  filename = 'plots/Figure_4_FEM_bellow_end_angles.png',
  plot = plt_rev,
  device = grDevices::png,
  width = 16.5, height = 7, units = 'cm', dpi = 360
  )

# Plot bellow elongations and lm with CI for all repetitions and fingers
plt_elong <- pos_data_wide %>%
  # Capitalize first letter of finger column
  mutate(finger = str_to_title(finger)) %>%
  mutate(joint = paste0('Delta*italic(L)[total~', '-~', joint, ']')) %>%
  # joint as factor
  mutate(joint = factor(joint, levels = unique(joint))) %>%
  # plot the data
  ggplot(aes(x = pressure*10, y = delta_L, color=finger)) +
  facet_grid(~joint, scales = 'fixed', labeller = label_parsed) +
  # Smooth line with CI
  geom_smooth(
    aes(color = finger),
    method = 'lm',
    formula = y ~ 0 + x + I(x^2),
    se = F,
    # color = 'black',
    linewidth = 1.3,
    # show.legend = F
  ) +
  geom_point(
    # aes(color = finger),
    size = 0.3,
    stroke = 0.2,
    shape = 21,
    # shape = 16,
    color = 'black',
    fill = 'black',
    # fill = pal_nejm()(8)[1],
    show.legend = F
    ) +
  scale_color_nejm(
    labels = scales::label_parse(),
    alpha = 0.4
  ) +
  # scale_color_discrete(
  #   type = pal_nejm()(8)[c(8, 7)],
  # ) +
  scale_fill_nejm(
    labels = scales::label_parse(),
  ) +
  scale_x_continuous(
    breaks = seq(0, 3, 0.5),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0.04)
      ) +
  scale_y_continuous(
    breaks = seq(0, 20, 1),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0)
      ) +
  labs(
    x = expression("Pressure "*italic(p)*" [bar]"),
    y = expression("Elongation "*Delta*italic(L)*" [mm]")
  ) +
  # Customize legend
  guides(
    # Remove legend for color
    color = guide_legend(
      title = 'Finger actuator:',
      byrow = F,
      reverse = F
    )
  ) +
  theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_text(
      size = 10, margin = margin(0, 0, 1, 0, 'mm')),
    legend.title.position = 'left',
    legend.box = 'horizontal',
    legend.direction = 'horizontal',
    legend.box.spacing = unit(0.5, 'mm'),
    legend.spacing.y = unit(0.5, 'mm'),
    legend.spacing.x = unit(0.5, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.key.spacing = unit(0.5, 'mm'),
    legend.key.size = unit(3, 'mm'),
    legend.text = element_text(
      colour="black", size = 9, margin = margin(0, 0., 1, 0, 'mm')
      ),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'mm'),
    panel.background = element_blank(),
    panel.spacing.y = unit(0, 'mm'),
    panel.spacing.x = unit(1.5, 'mm'),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_line(
    #   color = 'azure4', linewidth = 0.15, linetype = 'solid'
    #   ),
    axis.title = element_text(face="bold", size = 10),
    axis.text = element_text(
      color="black", size = 9, margin = margin(0.0, 0.0, 0.0, 0.0, 'mm'),
      ),
    # axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.),
    # Remove x axis text and title
    axis.line = element_line(linewidth = 0.2, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 5),
    panel.grid = element_line(colour = 'grey', linewidth = 0.1),
    panel.border = element_rect(linewidth = 0.2),
    strip.background = element_rect(linewidth = 0.01),
    strip.text = element_text(
      color = 'black', size = 10, margin = margin(b = 0.3, t = 0.3, unit='mm')
    ),
    axis.ticks = element_line(linewidth = 0.2),
    # axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.1, 'lines')
  )
plt_elong
ggsave(
  filename = 'plots/Figure_4_FEM_bellow_elongations.png',
  plot = plt_elong,,
  device = grDevices::png,
  width = 16.5, height = 7, units = 'cm', dpi = 360
  )

# Regression analysis for bending angles
pos_data_wide %>% 
  filter(finger == 'little') %>%
  filter(joint == 'DIP') %>% 
  # Transform pressure to bar
  mutate(
    pressure = pressure * 10,
  ) %>% 
  # lm(theta_joint ~ 0 + pressure + I(pressure^2), data = .) %>%
  # lm(theta_joint ~ 0 + I(pressure^2), data = .) %>%
  # lm(theta_joint ~ 0 + pressure, data = .) %>%
  lm(delta_L ~ 0 + pressure + I(pressure^2), data = .) %>%
  summary()

# Linear regression for all prismatic joints and robots (no repetitions)
lm_d3 <- robot_angl %>% 
  drop_na() %>%
  # d is link offset, subtract minimum of L to compute d range
  group_by(rob) %>%
  mutate(
    d1 = L1 - min(L1),
    d3 = L2 - min(L2),
    d5 = L3 - min(L3),
    d7 = L4 - min(L4)
    ) %>%
  ungroup() %>%
  # filter(rob == 'left') %>%
  filter(rob == 'right') %>%
  # lm(d1 ~ pressure-1, data = .) %>%
  lm(d3 ~ pressure-1, data = .)
  # lm(d5 ~ pressure-1, data = .) %>%
  # lm(d7 ~ pressure-1, data = .) %>%
  # summary()
