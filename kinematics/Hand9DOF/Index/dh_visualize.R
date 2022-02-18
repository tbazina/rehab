# Visualization of DH parameters for index finger using approx planes

# Tidyverse
library(tidyverse)

# Plotting
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggsci)

## Loading data
dh <-
  read_csv('DH_index_circular.csv') %>%
  mutate(
    grasp = 'circular'
  ) %>% 
  full_join(
    y = read_csv('DH_index_prismatic.csv') %>% mutate(grasp = 'prismatic')
  ) %>% 
  mutate(
    z = -z,
    th_cmc_fe = th_cmc_fe * 180 / pi,
    th_mcp_fe = th_mcp_fe * 180 / pi,
    th_mcp_aa = th_mcp_aa * 180 / pi,
    # round all values since grid is spaced 1 °
    th_cmc_fe = round(th_cmc_fe),
    th_mcp_fe = round(th_mcp_fe),
    th_mcp_aa = round(th_mcp_aa),
  )

dh_lines <- dh %>% 
  filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  filter(th_cmc_fe == 0) %>%
  filter(th_mcp_aa == 0) %>%
  group_by(joint) %>% 
  filter(row_number() %% 25 == 1) %>% 
  ungroup() %>%
  arrange(th_cmc_fe, th_mcp_aa, th_mcp_fe, joint) %>% 
  group_by(th_cmc_fe, th_mcp_aa, th_mcp_fe)

# 3D workspace scatter plot
fig <- dh %>% 
  filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  filter(th_cmc_fe == 0) %>%
  mutate(
    joint = (joint+1.5)^-1*10,
    grasp = factor(grasp, ordered = T, levels = c('circular', 'prismatic'))
    ) %>%
  plot_ly(
    x = ~z,
    y = ~y,
    z = ~x,
    marker = list(
      size = ~joint,
      # color = ~joint,
      color = ~th_mcp_fe,
      # color = ~th_cmc_fe,
      # color = ~grasp,
      colorbar = list(
        title = 'colorbar_title'
      ),
      colorscale = 'Viridis',
      showscale = T,
      line = list(color = '#FFFFFF', width = 0)
      )
    ) %>% 
  add_markers(
  ) %>% 
  add_paths(
    data = dh_lines,
    x = ~z,
    y = ~y,
    z = ~x,
    line = list(color = '#000000', width = 5, showscale = F),
    inherit = F
    ) %>% 
  layout( 
    # title = 'Index Finger workspace',
    scene = list(
      title = list(text = 'title'),
      xaxis = list(
        title='z [cm]', nticks=20, range=c(-3, 3)
        ),
      yaxis = list(
        title='y [cm]', nticks=20, range=c(10, -4)
        ),
      zaxis = list(
        title='x [cm]', nticks=20, range=c(0, 16)
        ),
      aspectmode='manual',
      aspectratio=list(x=0.6, y=1, z=1.2)
    ),
    # legend=list(title=list(text='<b> Trend </b>')),
    margin = list(
      l = 10,
      r = 10,
      b = 10,
      t = 20,
      pad = 0
    )
  )
fig


# FE plane plot - circular + prismatic
dh_lines_fe <- dh %>% 
  # filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  # filter(th_cmc_fe == 0) %>%
  filter(th_mcp_aa == 0) %>%
  group_by(joint) %>%
  filter((row_number() %% 110 == 1) | (th_mcp_fe == 0 & th_cmc_fe == 0)) %>%
  ungroup() %>%
  arrange(th_cmc_fe, th_mcp_aa, th_mcp_fe, joint) %>% 
  group_by(th_cmc_fe, th_mcp_aa, th_mcp_fe) %>% 
  mutate(
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
      )
    )

dh %>%
  filter(th_mcp_aa == 0) %>%
  mutate(
    th_cmc_fe_joint = paste(th_cmc_fe, joint),
    linesize = factor(th_cmc_fe),
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
    )
    ) %>%
  ggplot(aes(y = x, x = y)) +
  facet_wrap(. ~ grasp, scales = 'fixed') +
  coord_fixed() +
  geom_path(
    aes(group = th_cmc_fe_joint, color = factor(th_cmc_fe)),
    size = 0.5,
    lineend = 'round'
  ) +
  geom_path(
    data = dh_lines_fe,
    aes(group = th_mcp_fe),
    color = 'black',
    lineend = 'round',
    size = 0.3,
  ) +
  geom_point(
    # aes(fill = th_mcp_fe),
    shape = 21,
    colour = "black",
    size = 0.5,
    stroke = 0.05,
    alpha = 0.6
    ) +
  annotate(geom = 'text', y = 0, x = -1, label = 'CMC', size = 1.5) +
  annotate(geom = 'text', y = 6.8, x = -1.2, label = 'MCP', size = 1.5) +
  annotate(geom = 'text', y = 10.5, x = -1.6, label = 'PIP', size = 1.5) +
  annotate(geom = 'text', y = 13.6, x = -1, label = 'DIP', size = 1.5) +
  annotate(geom = 'text', y = 15.2, x = -2, label = 'TIP', size = 1.5) +
  annotate(geom = 'text', y = 15.3, x = 0, label = 'Neutral', size = 1.5) +
  scale_y_continuous(
    name = expression(italic(x)*' [cm]'),
    breaks = seq(0, 30, 2),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression(italic(y)*' [cm]'),
    breaks = seq(-10, 15, 2),
    # limits = c(-4, 10),
    trans = 'reverse'
    ) +
  scale_fill_viridis_c(
    name = expression(theta)
    # values = c("dodgerblue3", "darksalmon")
  ) +
  scale_color_nejm(
    name = expression(theta[CMC]*','[FE]*' [°]')
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle(expression('Workspace of index finger in FE plane '*(theta[MCP]*','[AA]*' = 0°'))) +
  theme_bw() + theme(
    text = element_text(size = 5),
    panel.border = element_rect(),
    panel.background = element_blank(),
    panel.spacing = unit(1, 'mm'),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 6, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    legend.position = c(0.46,  0.24),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0.5, 0.5, 0.0, 0.5, 'mm'),
    legend.text = element_text(size = 5),
    legend.key.size = unit(3, 'mm'),
    legend.title = element_text(size = 5),
    axis.title = element_text(
      size = 7,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(
      colour="black", size = 6,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.y = element_text(colour="black", size = 6),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(
      hjust = 0.5, vjust = 0, size = 7,
      margin = margin(0, 0, 0.5, 0, 'mm')
      ),
    plot.margin = margin(0, 1, 0, 1, 'mm'),
  )

ggsave(filename = 'workspace_fe_plane.png',
       width = 10, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# AA plane plot - circular
dh_lines_aa <- dh %>% 
  # filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  # filter(th_mcp_fe %in% seq(0, 90, length.out = 3)) %>%
  filter(th_mcp_fe == 0) %>%
  filter(th_cmc_fe == 0) %>%
  group_by(joint) %>%
  filter(row_number() %% 3 == 1) %>%
  ungroup() %>%
  arrange(th_cmc_fe, th_mcp_aa, th_mcp_fe, joint) %>% 
  group_by(th_cmc_fe, th_mcp_aa, th_mcp_fe) %>% 
  mutate(
    th_mcp_fe_joint = paste(th_mcp_fe, joint),
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
      ),
    joint = factor(case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'MCP_AA',
      joint == 2 ~ 'MCP_FE',
      joint == 3 ~ 'PIP',
      joint == 4 ~ 'DIP',
      joint == 5 ~ 'TIP',
    ))
    )

dh_joints_aa <- dh %>% 
  # filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  # filter(th_mcp_fe %in% seq(0, 90, length.out = 2)) %>%
  # filter(joint %in% c(1, 3, 4, 5)) %>%
  filter((th_mcp_fe == 0 & joint %in% c(1, 2, 3, 4, 5)) | 
           (th_mcp_fe == 90 & joint == 5)) %>% 
  # filter(th_mcp_fe == 0) %>%
  filter(th_cmc_fe == 0) %>%
  arrange(joint, th_mcp_fe, th_mcp_aa) %>% 
  mutate(
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
      ),
    joint = factor(case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'MCP_AA',
      joint == 2 ~ 'MCP_FE',
      joint == 3 ~ 'PIP',
      joint == 4 ~ 'DIP',
      joint == 5 ~ 'TIP',
    )),
    joint_th_mcp_fe = paste0(joint,  ' - ', th_mcp_fe, '°')
    )

dh %>%
  filter(th_cmc_fe == 0) %>%
  # filter(th_mcp_fe %in% seq(0, 90, length.out = 3)) %>%
  filter(joint == 5) %>% 
  # filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  mutate(
    th_mcp_fe_joint = paste(th_mcp_fe, joint),
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
      ),
    joint = factor(case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'MCP_AA',
      joint == 2 ~ 'MCP_FE',
      joint == 3 ~ 'PIP',
      joint == 4 ~ 'DIP',
      joint == 5 ~ 'TIP',
    ))
    ) %>%
  ggplot(aes(y = x, x = z)) +
  facet_wrap(. ~ grasp, scales = 'fixed') +
  coord_fixed() +
  geom_path(
    # aes(group = th_mcp_fe_joint, color = factor(joint)),
    aes(color = th_mcp_aa),
    size = 0.2,
    lineend = 'round'
  ) +
  geom_path(
    data = dh_lines_aa,
    aes(group = th_mcp_aa),
    color = 'black',
    lineend = 'round',
    size = 0.2,
  ) +
  geom_point(
    data = dh_joints_aa,
    aes(group = joint_th_mcp_fe, fill = factor(th_mcp_fe)),
    # color = 'black',
    size = 0.8,
    shape = 21,
    stroke = 0.0
  ) +
  # geom_point(
  #   data = dh_lines_aa,
  #   aes(fill = th_mcp_aa),
  #   shape = 21,
  #   # colour = "black",
  #   size = 1,
  #   stroke = 0.0
  #   ) +
  annotate(geom = 'text', y = 0, x = -0.8, label = 'CMC', size = 1.5) +
  annotate(geom = 'text', y = 6.5, x = -0.8, label = 'MCP', size = 1.5) +
  # annotate(geom = 'text', y = 6.5, x = 1.2, label = 'PIP - 90°', size = 1.5) +
  annotate(geom = 'text', y = 10.5, x = -1.7, label = 'PIP', size = 1.5) +
  annotate(geom = 'text', y = 12.8, x = -2.3, label = 'DIP', size = 1.5) +
  # annotate(geom = 'text', y = 4.2, x = 1.6, label = 'DIP - 90°', size = 1.5) +
  annotate(geom = 'text', y = 15.2, x = -2.0, label = 'TIP', size = 1.5) +
  annotate(geom = 'text', y = 2.0, x = 1.2, label = 'TIP - 90°', size = 1.5) +
  scale_y_continuous(
    name = expression(italic(x)*' [cm]'),
    breaks = seq(0, 30, 2),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression(italic(z)*' [cm]'),
    breaks = seq(-10, 15, 1),
    limits = c(2.5, -2.5),
    trans = 'reverse'
    ) +
  # scale_fill_viridis_c(
  #   name = expression(theta[MCP]*','[AA]*' [°]')
  # ) +
  scale_color_viridis_c(
    name = expression(theta[MCP]*','[AA]*' [°]')
  ) +
  scale_fill_nejm(
    # name = expression(theta[MCP]*','[FE]*' [°]')
    name = expression(theta[MCP]*','[FE]*' [°]')
  ) +
  ggtitle(expression('Workspace of index finger in AA plane '*(theta[CMC]*','[FE]*' = 0°'))) +
  theme_bw() + theme(
    text = element_text(size = 5),
    panel.border = element_rect(),
    panel.background = element_blank(),
    panel.spacing = unit(1, 'mm'),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 6, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    legend.position = 'right',
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0.5, 0.5, 1, 0.5, 'mm'),
    legend.text = element_text(size = 5),
    legend.key.size = unit(3, 'mm'),
    legend.title = element_text(size = 5),
    axis.title = element_text(
      size = 7,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(
      colour="black", size = 6,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.y = element_text(colour="black", size = 6),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(
      hjust = 0.5, vjust = 0, size = 7,
      margin = margin(0, 0, 0.5, 0, 'mm')
      ),
    plot.margin = margin(0, 1, 0, 1, 'mm'),
  )

ggsave(filename = 'workspace_aa_plane.png',
       width = 10, height = 8, units = 'cm', dpi = 320, pointsize = 12)
