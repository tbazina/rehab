# FEM soft robot preparation of position data during bending
library(tidyverse)
library(magrittr)
library(ggsci)
library(ggforce)

# Load relative positin data for each FEM measured point
pos_dat <- read_csv('data/FEM_index_finger.csv')
# Load absolute x coordinate for each FEM measured point
pos_abs_init <- read_csv('data/Init_pos_index_finger.csv')

# Separate joint and abs coordinates for merging data
pos_abs_init <- pos_abs_init %>% 
  rename(joint = name) %>% 
  mutate(
    # Add _mid between joint and coordinate if if doesn't contain start or end
    joint = case_when(
      str_detect(joint, 'start') ~ joint,
      str_detect(joint, 'end') ~ joint,
      T ~ str_replace(joint, '_', '_mid_')
    )
  ) %>% 
  separate_wider_delim(
    joint, delim = '_', names = c('joint', 'coord_segment', 'coord')
    ) %>% 
  mutate(
    joint = str_to_upper(joint),
    joint = factor(joint, levels = c('MCP', 'PIP', 'DIP', 'TIP')),
    coord_segment = factor(coord_segment, levels = c('start', 'mid', 'end'))
    ) %>% 
  select(-coord, u1_init_offset = init_pos_x)

# Prepare for plotting - separate joint and coordinate
pos_dat <- pos_dat %>%
  pivot_longer(
    !c(time, pressure), names_to = 'joint', values_to = 'value',
    ) %>% 
  mutate(
    # Add _mid between joint and coordinate if if doesn't contain start or end
    joint = case_when(
      str_detect(joint, 'start') ~ joint,
      str_detect(joint, 'end') ~ joint,
      T ~ str_replace(joint, '_', '_mid_')
    )
  ) %>% 
  separate_wider_delim(
    joint, delim = '_', names = c('joint', 'coord_segment', 'coord')
    ) %>% 
  pivot_wider(names_from = coord, values_from = value) %>% 
  mutate(
    joint = str_to_upper(joint),
    joint = factor(joint, levels = c('MCP', 'PIP', 'DIP', 'TIP')),
    coord_segment = factor(coord_segment, levels = c('start', 'mid', 'end'))
    )

# Left join with absolute position data
pos_dat <- left_join(pos_dat, pos_abs_init, by = c('joint', 'coord_segment')) %>% 
  mutate(
    u1 = u1 + u1_init_offset
  )

# Plot u1 vs u2 for all joints and segment point measures
pos_bend_plt <- pos_dat %>% 
  ggplot(aes(x = u1, y = u2, color=joint)) +
  coord_fixed(
    xlim = c(0, 135),
    ylim = c(-95, 5),
  ) +
  geom_point(aes(fill = joint),
    size = 1.0,
    stroke = 0.2,
    shape = 21,
    alpha = 0.9,
    color = 'black',
    # fill = pal_nejm()(8)[1],
    show.legend = T
    ) +
  geom_path(
    # Downsample data to keep every 5th row after grouping per joint and
    # coord_segment
    data = pos_dat %>% 
      group_by(joint, coord_segment) %>%
      filter(row_number() %% 10 == 1) %>% 
      ungroup() %>%
      # Keep only MCP and PIP parts without bellows
      filter(
        (joint == 'MCP' & coord_segment == 'end') |
          (joint == 'PIP' & coord_segment == 'start')
          ),
    aes(group = pressure),
    linewidth = 0.3,
    alpha = 0.9,
    color = 'black',
    show.legend = F
  ) +
  geom_path(
    # Downsample data to keep every 5th row after grouping per joint and
    # coord_segment
    data = pos_dat %>% 
      group_by(joint, coord_segment) %>%
      filter(row_number() %% 10 == 1) %>% 
      ungroup() %>%
      # Keep only PIP and DIP parts without bellows
      filter(
        (joint == 'PIP' & coord_segment == 'end') |
          (joint == 'DIP' & coord_segment == 'start')
          ),
    aes(group = pressure),
    linewidth = 0.3,
    alpha = 0.9,
    color = 'black',
    show.legend = F
  ) +
  geom_path(
    # Downsample data to keep every 5th row after grouping per joint and
    # coord_segment
    data = pos_dat %>% 
      group_by(joint, coord_segment) %>%
      filter(row_number() %% 10 == 1) %>% 
      ungroup() %>%
      # Keep only DIP and TIP parts without bellows
      filter(
        (joint == 'DIP' & coord_segment == 'end') |
          (joint == 'TIP')
          ),
    aes(group = pressure),
    linewidth = 0.3,
    alpha = 0.9,
    color = 'black',
    show.legend = F
  ) +
  geom_path(
    # Downsample data to keep every 5th row after grouping per joint and
    # coord_segment
    data = pos_dat %>% 
      group_by(joint, coord_segment) %>%
      # Keep only 0 pressure
      filter(row_number() == 1),
    aes(group = pressure),
    linewidth = 0.3,
    alpha = 0.9,
    color = 'black',
    show.legend = F
  ) +
  # Label each path with pressure at tip coordinates
  geom_text(
    data = pos_dat %>% 
      group_by(joint, coord_segment) %>%
      filter(row_number() %% 10 == 1) %>% 
      filter(joint == 'TIP'),
    # Convert pressure to bar
    aes(label = pressure*10),
    nudge_x = 4,
    nudge_y = -4,
    hjust = 0,
    vjust = 0,
    size = 2,
    color = 'black',
    show.legend = F
  ) +
  # Add curved arrow to show bending direction
  annotate(
    geom = 'curve',
    x = 130, y = -55, xend = 100, yend = -85,
    curvature = -0.2,
    arrow = arrow(type = 'open', length = unit(0.2, 'cm')),
    color = 'black',
    linewidth = 0.3
  ) +
  geom_arc0(
    data = pos_dat_wide_arc %>% 
      # Keep only MCP joints and downsample to keep every 10th row
      # no 0 bar data, so different than top
      group_by(joint) %>%
      # filter(joint == 'MCP') %>% 
      filter(row_number() %% 10 == 0) %>% 
      # Add mock u1 and u2 variables equal to zero
      mutate(u1 = 0, u2 = 0),
    aes(
      x0 = u1_center_arc, y0 = u2_center_arc, r = R_arc, 
      start = theta_arc_start, end = theta_arc_end,
      group = joint
      ),
    color = 'black',
    linewidth = 0.3
  ) +
  # Add text near arrow to signify it's pressure in bar
  annotate(
    geom = 'text',
    x = 90, y = -90,
    label = "'Pressure '*italic(p)*' [bar]'",
    parse = T,
    hjust = 0,
    vjust = 0,
    size = 2,
    color = 'black',
  ) +
  scale_color_nejm() +
  # scale_color_discrete(
  #   type = pal_nejm()(8)[c(8, 7)],
  # ) +
  scale_fill_nejm(
    labels = scales::label_parse(),
  ) +
  scale_x_continuous(
    breaks = seq(0, 200, 10),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0.04)
      ) +
  scale_y_continuous(
    breaks = seq(-200, 50, 10),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    # expand = c(0, 0)
      ) +
  labs(
    x = expression(italic(x)*" [mm]"),
    y = expression(italic(y)*" [mm]")
  ) +
  # Customize legend
  guides(
    # Remove legend for color
    # color = 'none',
    fill = guide_legend(
      title = 'Bellow: ',
      byrow = F,
      reverse = F
    )
  ) +
  theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_text(
      size = 7, margin = margin(0, 0, 1, 0, 'mm')),
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
      colour="black", size = 7, margin = margin(0, 0., 0, 0, 'mm')
      ),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'mm'),
    panel.background = element_blank(),
    panel.spacing.y = unit(0, 'mm'),
    panel.spacing.x = unit(1.5, 'mm'),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_line(
    #   color = 'azure4', linewidth = 0.15, linetype = 'solid'
    #   ),
    axis.title = element_text(face="bold", size = 8),
    axis.text = element_text(
      color="black", size = 7, margin = margin(0.0, 0.0, 0.0, 0.0, 'mm'),
      ),
    # axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.),
    # Remove x axis text and title
    axis.line = element_line(linewidth = 0.2, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 5),
    panel.grid = element_line(colour = 'grey', linewidth = 0.1),
    panel.border = element_rect(linewidth = 0.2),
    strip.background = element_rect(linewidth = 0.01),
    strip.text = element_text(
      color = 'black', size = 7.0, margin = margin(b = 0.3, t = 0.3, unit='mm')
    ),
    axis.ticks = element_line(linewidth = 0.2),
    # axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.1, 'lines')
  )
pos_bend_plt
ggsave(
  filename = 'plots/FEM_index_finger_movement.png',
  plot = pos_bend_plt,
  device = grDevices::png,
  width = 9, height = 7, units = 'cm', dpi = 360
  )

# Compute each joint angles. Take xy coord difference between the following
# joint start, and previous joint end points. USe atan2 to compute angle.
joint_angles <- pos_dat %>% 
  # Remove u1_init_offset
  select(-u1_init_offset) %>%
  # Remove coord_segment == 'mid
  # filter(coord_segment != 'mid') %>% 
  # pivot wider using joint and coord_segment
  pivot_wider(
    names_from = c(joint, coord_segment),
    values_from = c(u1, u2),
    names_glue = '{joint}_{coord_segment}_{.value}'
  ) %>% 
  # Compute neighbouring joint x and y differences
  mutate(
    MCP_diff_u1 = PIP_start_u1 - MCP_end_u1,
    MCP_diff_u2 = PIP_start_u2 - MCP_end_u2,
    PIP_diff_u1 = DIP_start_u1 - PIP_end_u1,
    PIP_diff_u2 = DIP_start_u2 - PIP_end_u2,
    # For tip there is only mid
    DIP_diff_u1 = TIP_mid_u1 - DIP_end_u1,
    DIP_diff_u2 = TIP_mid_u2 - DIP_end_u2
  ) %>% 
  # Compute cumulative joint angles
  mutate(
    MCP_cum_angle = atan2(MCP_diff_u2, MCP_diff_u1) * 180 / pi,
    PIP_cum_angle = atan2(PIP_diff_u2, PIP_diff_u1) * 180 / pi,
    DIP_cum_angle = atan2(DIP_diff_u2, DIP_diff_u1) * 180 / pi
  ) %>% 
  # Change the sign of the cumulative angles, since axes are reverted
  mutate(
    MCP_cum_angle = -MCP_cum_angle,
    PIP_cum_angle = -PIP_cum_angle,
    DIP_cum_angle = -DIP_cum_angle
  ) %>%
  # Convert large negative angles (>90 degrees) to positive
  mutate(
    MCP_cum_angle = ifelse(MCP_cum_angle < -90, 360 + MCP_cum_angle, MCP_cum_angle),
    PIP_cum_angle = ifelse(PIP_cum_angle < -90, 360 + PIP_cum_angle, PIP_cum_angle),
    DIP_cum_angle = ifelse(DIP_cum_angle < -90, 360 + DIP_cum_angle, DIP_cum_angle)
  ) %>%
  # From cumulative angles compute joint angles by subtracting those earlier
  # in the chain
  mutate(
    MCP_angle = MCP_cum_angle,
    PIP_angle = PIP_cum_angle - MCP_cum_angle,
    DIP_angle = DIP_cum_angle - PIP_cum_angle
  ) %>% 
  select(pressure, MCP_angle, PIP_angle, DIP_angle)

# Plot joint angles vs pressure
joint_angle_plt <- joint_angles %>% 
  pivot_longer(
    cols = c(MCP_angle, PIP_angle, DIP_angle),
    names_to = 'joint',
    values_to = 'angle'
  ) %>% 
  ggplot(aes(x = pressure, y = angle, color = joint)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~joint, scales = 'free_y') +
  labs(
    x = 'Pressure (kPa)',
    y = 'Joint angle (degrees)',
    color = 'Joint'
  ) +
  theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_text(
      size = 7, margin = margin(0, 0, 1, 0, 'mm')),
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
      colour="black", size = 7, margin = margin(0, 0., 0, 0, 'mm')
      ),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'mm'),
    panel.background = element_blank(),
    panel.spacing.y = unit(0, 'mm'),
    panel.spacing.x = unit(1.5, 'mm'),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_line(
    #   color = 'azure4', linewidth = 0.15, linetype = 'solid'
    #   ),
    axis.title = element_text(face="bold", size = 8),
    axis.text = element_text(
      color="black", size = 7, margin = margin(0.0, 0.0, 0.0, 0.0, 'mm'),
      ),
    # axis.text.x = element_text(angle = 0
  )
joint_angle_plt

compute_arc_params <- function(x1, y1, x2, y2, x3, y3) {
  # Step 1: Solve for circle center (h, k) and radius R
  A <- matrix(
    c(
      2 * (x2 - x1), 2 * (y2 - y1),
      2 * (x3 - x1), 2 * (y3 - y1)
    ),
    nrow = 2,
    byrow = TRUE
  )
  B <- c(
    x2^2 + y2^2 - x1^2 - y1^2,
    x3^2 + y3^2 - x1^2 - y1^2
  )
  
  # Solve linear system A * [h, k] = B
  solution <- solve(A, B)
  h <- solution[1]
  k <- solution[2]
  
  # Radius R
  R <- sqrt((x1 - h)^2 + (y1 - k)^2)
  
  # Step 2: Compute central angle theta (radians)
  vec_SC <- c(x1 - h, y1 - k)
  vec_EC <- c(x3 - h, y3 - k)
  dot_product <- sum(vec_SC * vec_EC)
  theta <- acos(dot_product / (R^2))  # Central angle in radians
  
  
  # Step 3: Arc length and original chord length
  L_arc <- R * theta
  
  
  # Compute vectors from center to points to compute start and end angles
  # From positivie y-axis to those vectors in clockwise direction (plotting)
  dx1 <- x1 - h
  dy1 <- y1 - k
  dx3 <- x3 - h
  dy3 <- y3 - k
  
  # Compute angles (in radians)
  theta_start <- atan2(dx1, dy1)
  theta_end <- atan2(dx3, dy3)
  
  return(list(
    L_arc = L_arc, R_arc = R, u1_center_arc = h, u2_center_arc = k, 
    theta_arc = theta, theta_arc_start = theta_start,
    theta_arc_end = theta_end
    ))
  
}

pos_dat %>% filter(joint == 'PIP') %>% 
  View()

# Example usage
x1 <- 47.73659; y1 <- -30.468400   # Start point (fixed)
x2 <- 54.14667; y2 <- -42.296700   # Midpoint
x3 <- 54.80970; y3 <- -55.961800   # End point
L_original <- 73.667 - 53.267

# Step 4: Total elongation
arc_approx <- compute_arc_params(x1, y1, x2, y2, x3, y3)
delta_L <- arc_approx$L_arc - L_original
cat(sprintf("Total beam elongation: %.4f\n", delta_L))
arc_approx$theta * 180 / pi

# Compute elongations of entire dataset and all parameters for arc approximations
pos_dat_wide_arc <- pos_dat %>%
  # Remove TIP
  filter(joint != 'TIP') %>%
  # Remove 0 pre
  # Pivot wider using coord_segment and values from u1 and u2 and u1_init_offset
  pivot_wider(
    names_from = coord_segment,
    values_from = c(u1, u2, u1_init_offset)
  ) %>%
  # Approximate initial length of each segment by subtracting u1_start from u1_end
  mutate(
    L_init = u1_init_offset_end - u1_init_offset_start
  ) %>% 
  # Remove 0 bar pressure values, since the correspond to initial lengths
  filter(pressure > 1e-10) %>%
  rowwise() %>% 
  # Compute arc parameters for each segment
  mutate(
    arc_params = list(
      compute_arc_params(
        u1_start, u2_start, u1_mid, u2_mid, u1_end, u2_end
        ))
  ) %>% 
  # Return to columnwise
  ungroup() %>% 
  unnest_wider(arc_params) %>% 
  # Compute elongations
  mutate(
    delta_L = L_arc - L_init
  )
  

