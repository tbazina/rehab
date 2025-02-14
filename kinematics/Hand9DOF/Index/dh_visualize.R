# Visualization of DH parameters for index finger using approx planes

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
dh <-
  read_csv('DH_modified_index_circular.csv') %>%
  mutate(grasp = 'circular', beta = 'real') %>% 
  full_join(
    y = read_csv('DH_modified_index_prismatic.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'real')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_index_circular_no_beta.csv') %>% 
      mutate(grasp = 'circular', beta = 'zero')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_index_prismatic_no_beta.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'zero')
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
  filter(beta == 'real') %>%
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
  filter(beta == 'real') %>%
  # filter(grasp == 'prismatic') %>%
  filter(th_cmc_fe == 0) %>%
  # filter(joint == 5) %>% 
  mutate(
    # joint = (joint+1.5)^-1*10,
    joint = case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP',
      joint == 3 ~ 'MCP',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
      T ~ 'WRONG'
    ),
    joint = ordered(joint, levels = c('TIP', 'DIP', 'PIP', 'MCP', 'CMC')),
    grasp = factor(grasp, ordered = T, levels = c('circular', 'prismatic'))
    ) %>%
  plot_ly(
    x = ~z,
    y = ~y,
    z = ~x,
    color = ~joint,
    colors = pal_nejm('default', alpha = 1)(5)
    ) %>% 
  add_markers(
    marker = list(
      size = ~5,
      line = list(color = '#FFFFFF', width = 0)
      )
  ) %>%
  add_paths(
    data = dh_lines,
    x = ~z,
    y = ~y,
    z = ~x,
    line = list(color = '#000000', width = 7, showscale = F),
    inherit = F,
    name = 'Pose'
    ) %>% 
  layout( 
    # title = 'Index Finger workspace',
    scene = list(
      title = list(text = 'title'),
      xaxis = list(
        title = list(text = '<b><i>z</i></b> [cm]',
                  font = list(size = 32)),
        tickfont = list(size = 18),
        nticks= 10, range = c(-2.5, 2.5)
        ),
      yaxis = list(
        title = list(text = '<b><i>y</i></b> [cm]',
                  font = list(size = 32)),
        tickfont = list(size = 18),
        nticks=20, range=c(8, -3)
        ),
      zaxis = list(
        title = list(text = '<b><i>x</i></b> [cm]',
                  font = list(size = 32)),
        tickfont = list(size = 18),
        nticks=20, range=c(0, 16)
        ),
      aspectmode='manual',
      aspectratio=list(x=0.454545, y=1, z=1.454545)
    ),
    legend=list(
      title= list(text = '<b>Joint</b>',
                  font = list(size = 30)),
      font = list(size = 30),
      yanchor="top",
      y=0.98,
      xanchor="left",
      x=0.12
      ),
    margin = list(
      l = 10,
      r = 10,
      b = 10,
      t = 20,
      pad = 0
    )
  )
fig

config(fig, toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                        filename= 'newplot',
                                        height= 800,
                                        width= 1300,
                                        scale= 1 ))%>%layout(plot_bgcolor='#e5ecf6',
                                                             xaxis = list(
                                                               zerolinecolor = '#ffff',
                                                               zerolinewidth = 2,
                                                               gridcolor = 'ffff'),
                                                             yaxis = list(
                                                               zerolinecolor = '#ffff',
                                                               zerolinewidth = 2,
                                                               gridcolor = 'ffff')
                                        )


# Actuators plot - only circular, converted to mm, flipepd axes
# FE plane plot - circular + prismatic
dh_lines_fe <- dh %>% 
  filter(beta == 'real') %>%
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
      ),
    # Convert cm to mm for x, y, z
    # make y negative
    x = x*10,
    y = -y*10,
    z = z*10,
    )

dh %>%
  filter(beta == 'real') %>%
  filter(th_mcp_aa == 0) %>%
  filter(grasp == 'circular') %>%
  mutate(
    th_cmc_fe_joint = paste(th_cmc_fe, joint),
    linesize = factor(th_cmc_fe),
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
    ),
    # Convert cm to mm for x, y, z
    # make y negative
    x = x*10,
    y = -y*10,
    z = z*10,
    ) %>%
  ggplot(aes(y = x, x = y)) +
  # facet_wrap(. ~ grasp, scales = 'fixed') +
  coord_fixed() +
  geom_path(
    aes(group = th_cmc_fe_joint, color = factor(th_cmc_fe)),
    size = 0.5,
    lineend = 'round'
  ) +
  geom_path(
    data = dh_lines_fe %>% filter(grasp == 'Circular'),
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
  annotate(geom = 'text', y = 3, x = 8, label = 'CMC', size = 2.3) +
  annotate(geom = 'text', y = 64, x = 10, label = 'MCP', size = 2.3) +
  annotate(geom = 'text', y = 105, x = 16, label = 'PIP', size = 2.3) +
  annotate(geom = 'text', y = 136, x = 10, label = 'DIP', size = 2.3) +
  annotate(geom = 'text', y = 152, x = 19, label = 'TIP', size = 2.3) +
  annotate(geom = 'text', y = 153, x = 0, label = 'Neutral', size = 2.3) +
  scale_y_continuous(
    name = expression(italic(z)*' [mm]'),
    breaks = seq(0, 160, 20),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression(italic(x)*' [mm]'),
    breaks = seq(-100, 20, 20),
    labels = ~sub("-", "\u2212", .x) 
    # limits = c(-4, 10),
    # trans = 'reverse'
    ) +
  scale_fill_viridis_c(
    name = expression(theta)
    # values = c("dodgerblue3", "darksalmon")
  ) +
  scale_color_nejm(
    name = expression(theta[CMC]*','[FE]*' [°]')
    # values = c("dodgerblue3", "darksalmon")
  ) +
  # ggtitle(expression('Workspace of index finger in FE plane '*(theta[MCP]*','[AA]*' = 0°'))) +
  theme_bw() + theme(
    text = element_text(size = 7),
    panel.border = element_rect(),
    panel.background = element_blank(),
    panel.spacing = unit(1, 'mm'),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 7, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    legend.position = 'right',
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0.5, 0.5, 0.0, 0.5, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(3, 'mm'),
    legend.title = element_text(size = 7),
    axis.title = element_text(
      size = 7,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(
      colour="black", size = 7,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.y = element_text(colour="black", size = 7),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.margin = margin(0.9, 0.2, 0.2, 0.2, 'mm'),
  )

ggsave(
  filename = 'workspace_fe_plane_index_circular.png',
  device = grDevices::png, width = 8, height = 9.2, units = 'cm', dpi = 320,
  pointsize = 12
  )

# AA plane plot - circular
dh_lines_aa <- dh %>% 
  filter(beta == 'real') %>%
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
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP_AA',
      joint == 3 ~ 'MCP_FE',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
    ))
    )

dh_joints_aa <- dh %>% 
  filter(beta == 'real') %>%
  # filter(grasp == 'circular') %>%
  # filter(grasp == 'prismatic') %>%
  # filter(th_mcp_fe %in% seq(0, 90, length.out = 2)) %>%
  # filter(joint %in% c(1, 3, 4, 5)) %>%
  filter((th_mcp_fe == 0 & joint %in% c(1, 2, 3, 4, 5, 6)) | 
           (th_mcp_fe == 90 & joint == 6)) %>% 
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
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP_AA',
      joint == 3 ~ 'MCP_FE',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
    )),
    joint_th_mcp_fe = paste0(joint,  ' - ', th_mcp_fe, '°')
    )

dh %>%
  filter(beta == 'real') %>%
  filter(th_cmc_fe == 0) %>%
  # filter(th_mcp_fe %in% seq(0, 90, length.out = 3)) %>%
  filter(joint == 6) %>% 
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
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP_AA',
      joint == 3 ~ 'MCP_FE',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
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

# Calculate error (euclidean distance) between real and zero beta angles
dh_error <- dh %>% select(-...1, -th_values, -a_values) %>%
  group_by(grasp, joint, th_mcp_fe, th_mcp_aa, th_cmc_fe) %>% 
  pivot_wider(names_from = beta, values_from = c(x, y, z)) %>%
  mutate(
    e_dist = sqrt((x_real - x_zero)^2 + (y_real - y_zero)^2 + (z_real - z_zero)^2),
    rel_err = case_when(
      (x_real == 0 & y_real == 0 & z_real == 0) ~ NA_real_,
      TRUE ~ e_dist / sqrt(x_real^2 + y_real^2 + z_real^2)
    )
  ) %>% ungroup() %>% select(-ends_with(c('real', 'zero')))
dh_error %>% arrange(desc(e_dist)) %>% View()

# Min and max errors
dh_error %>%
  mutate(
    joint = factor(case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP',
      joint == 3 ~ 'MCP',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
      ), levels = c('CMC', 'MCP', 'PIP', 'DIP', 'TIP'))
  ) %>% 
  group_by(joint, grasp) %>%
  summarise(
    abs_min = min(e_dist),
    abs_max = max(e_dist),
    abs_mean = mean(e_dist),
    rel_min = min(rel_err*100),
    rel_max = max(rel_err*100),
    rel_mean = mean(rel_err*100)
  )
  

# Significance analysis - only th_mcp_fe, joint, grasp and interactions significant
fit <- lm(
  e_dist ~ (th_mcp_fe + joint + grasp)^2,
  data = dh_error %>% mutate(joint = as.factor(joint), grasp = as.factor(grasp)))

summary(fit)
anova(fit)

# Visualize error
dh_error %>%
  mutate(rel_err = rel_err * 100) %>% 
  rename('AE [cm]' = e_dist, 'RE [%]' = rel_err) %>% 
  pivot_longer(
    cols = c('AE [cm]', 'RE [%]'), 
    names_to = 'err_metric',
    values_to = 'err_value'
    ) %>% 
  mutate(
    grasp = case_when(
      grasp == 'circular' ~ 'Circular',
      grasp == 'prismatic' ~ 'Prismatic',
      T ~ as.character('a')
      ),
    joint = factor(case_when(
      joint == 0 ~ 'CMC',
      joint == 1 ~ 'CMC',
      joint == 2 ~ 'MCP',
      joint == 3 ~ 'MCP',
      joint == 4 ~ 'PIP',
      joint == 5 ~ 'DIP',
      joint == 6 ~ 'TIP',
    ), levels = c('CMC', 'MCP', 'PIP', 'DIP', 'TIP'))
    ) %>%
  filter(joint %in% c('MCP', 'PIP', 'DIP', 'TIP')) %>%
  distinct() %>% arrange(joint, th_mcp_fe) %>% 
  ggplot(aes(y = err_value, x = th_mcp_fe)) +
  facet_grid(err_metric ~ grasp, scales = 'free_y', switch = 'y') +
  geom_line(
    aes(color = joint),
    stat = 'smooth',
    formula = y ~ s(x, bs = "cs"),
    method = 'gam',
    se = T,
    alpha = 0.4,
    size = 0.8
    # method = 'loess',
    # formula = y ~ x, span = 0.5
  ) +
  geom_point(
    aes(fill = joint),
    # color = 'black',
    size = 0.6,
    shape = 21,
    stroke = 0.0
  ) +
  scale_y_continuous(
    # name = 'Euclidean distance (error) [cm]',
    # breaks = seq(0, 1, 0.1),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression(theta[MCP]*','[FE]*' [°]'),
    breaks = seq(-10, 90, 10),
    expand = c(0.02, 0.02)
    # limits = c(2.5, -2.5),
    ) +
  scale_fill_nejm(
    guide = 'none'
    # name = expression('Joint')
  ) +
  scale_color_nejm(
    name = expression('Joint')
  ) +
  # ggtitle(expression('Model comparisons - real vs zero '*italic(beta))) +
  theme_bw() + theme(
    text = element_text(size = 9),
    panel.border = element_rect(),
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 9, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    legend.position = 'top',
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0.5, 0.5, 1, 0.5, 'mm'),
    legend.text = element_text(size = 9),
    legend.key.size = unit(3, 'mm'),
    legend.title = element_text(face = 'bold', size = 10),
    axis.title = element_text(
      size = 10,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(
      colour="black", size = 8,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.y = element_text(colour="black", size = 8),
    axis.title.y = element_blank(),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(
      hjust = 0.5, vjust = 0, size = 12,
      margin = margin(0, 0, 0.5, 0, 'mm')
      ),
    plot.margin = margin(0, 1, 0, 1, 'mm'),
  )

ggsave(filename = 'position_error_mcp_fe.png',
       width = 12, height = 7, units = 'cm', dpi = 320, pointsize = 12)
