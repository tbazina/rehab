# FEM soft robot preparation of position data during bending
library(tidyverse)
library(magrittr)
library(ggsci)
library(ggforce)
library(skimr)
library(xtable)
library(clipr)

compute_arc_params <- function(x1, y1, x2, y2, x3, y3) {
  # Compute all 3-point arc parameters for plotting and computing elongations
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
  # IF theta_end is negative, add 2*pi to make it positive
  theta_end <- ifelse(theta_end < 0, theta_end + 2 * pi, theta_end)
  
  return(list(
    L_arc = L_arc, R_arc = R, u1_center_arc = h, u2_center_arc = k, 
    theta_arc = theta, theta_arc_start = theta_start,
    theta_arc_end = theta_end
    ))
  
}

# Load relative position data for each FEM measured point
pos_dat <- read_csv('data/FEM_index_finger.csv')
# pos_dat <- read_csv('data/FEM_little_finger.csv')
# Load absolute x coordinate for each FEM measured point
pos_abs_init <- read_csv('data/Init_pos_index_finger.csv')
# pos_abs_init <- read_csv('data/Init_pos_little_finger.csv')

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

# Compute inter-joint distance to validate they don't change
pos_dat_inter_joint <- pos_dat %>% 
  select(pressure, joint, coord_segment, u1, u2) %>% 
  pivot_wider(names_from = c(joint, coord_segment), values_from = c(u1, u2)) %>% 
  # Rename column name TIP_mid to TIP_start
  rename_with(~str_replace(., 'TIP_mid', 'TIP_start'), contains('TIP_mid')) %>% 
  # Remove all columns with _mid in name
  select(-contains('mid')) %>%
  # Compute euclidean distance from MCP_end to PIP_start, PIP_end to DIP_start
  # and DIP_end to TIP_start using both u1 and u2
  mutate(
    MCP_PIP_inter_dist = sqrt((u1_PIP_start - u1_MCP_end)^2 + (u2_PIP_start - u2_MCP_end)^2),
    PIP_DIP_inter_dist = sqrt((u1_DIP_start - u1_PIP_end)^2 + (u2_DIP_start - u2_PIP_end)^2),
    DIP_TIP_inter_dist = sqrt((u1_TIP_start - u1_DIP_end)^2 + (u2_TIP_start - u2_DIP_end)^2)
    ) %>% 
  select(contains('_inter_dist')) %>%
  # Rename columns to remove _inter_dist
  rename_with(~str_replace(., '_inter_dist', ''), everything()) %>%
  # Compute mean and range for each inter-joint distance and divide them
  summarise(
    across(everything(), list(max = max, min = min, range = ~max(.x) - min(.x)))
  )  %>% 
  mutate(
    MCP_PIP_range_ratio = MCP_PIP_range / MCP_PIP_max,
    PIP_DIP_range_ratio = PIP_DIP_range / PIP_DIP_max,
    DIP_TIP_range_ratio = DIP_TIP_range / DIP_TIP_max
  ) %>% 
  pivot_longer(
    everything(), names_to = 'inter_joint_dist', values_to = 'value'
  ) %>% 
  separate_wider_delim(
    inter_joint_dist, delim = '_', names = c('joint1', 'joint2', 'measure'),
    too_many = 'merge'
  ) %>% 
  # Merge joint1 and joint2 into joints
  unite('joints', joint1, joint2, sep = '_') %>%
  pivot_wider(names_from = measure, values_from = value)

# Save to csv
pos_dat_inter_joint %>%
  # write_csv('data/inter_joint_distance_index.csv')
  write_csv('data/inter_joint_distance_little.csv')

# Copy to latex table
pos_dat_inter_joint %>%
  # Convert range ratio to percentage
  mutate(
    range_ratio = scales::percent(range_ratio)
  ) %>%
  xtable(
    caption = 'Compression of inter-joint distance.',
    label = 'tab:inter-finger-distance',
    ) %>% 
  print() %>% 
  write_clip(object_type = 'character')

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
  # Remove _angle from joint names
  mutate(
    joint = str_remove(joint, '_angle'),
    joint = factor(joint, levels = c('MCP', 'PIP', 'DIP'))
    ) %>%
  ggplot(aes(x = pressure, y = angle, color = joint)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~joint, scales = 'fixed') +
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

# Plot elongation delta_l vs pressure for each joint
elongation_plt <-pos_dat_wide_arc %>% 
  ggplot(aes(x = pressure, y = delta_L, color = joint)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~joint, scales = 'fixed') +
  labs(
    x = 'Pressure (kPa)',
    y = 'Joint elongation (mm)',
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
elongation_plt
  
# Plot u1 vs u2 for all joints and segment point measures
pos_bend_plt <- pos_dat %>% 
  # Plot original data
  ggplot(aes(x = u1, y = u2, color=joint)) +
  # Plot resampled data
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
      # Add mock u1, u2, resample_u1, resample_u2 variables equal to zero
      mutate(u1 = 0, u2 = 0, resample_u1 = 0, resample_u2 = 0),
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
  # filename = 'plots/FEM_index_finger_movement.png',
  filename = 'plots/FEM_little_finger_movement.png',
  plot = pos_bend_plt,
  device = grDevices::png,
  width = 9, height = 7, units = 'cm', dpi = 360
  )

# Save merged pos_data_wide with arc approximation parameters and joint_angles
pos_dat_wide_arc %>% 
  left_join(
    joint_angles %>% 
      rename(MCP = MCP_angle, PIP = PIP_angle, DIP = DIP_angle) %>%
      pivot_longer(
        cols = c(MCP, PIP, DIP),
        names_to = 'joint',
        values_to = 'theta_joint'
        ),
    by = c('pressure', 'joint')
    ) %>% 
  # write_csv('data/FEM_position_data_index_arc_approx_joint_elongation_angles.csv')
  write_csv('data/FEM_position_data_little_arc_approx_joint_elongation_angles.csv')

# Compute uniformly resampled x and y coordinates using LOESS
pos_dat_resample <- pos_dat %>% 
  group_by(joint, coord_segment) %>%
  nest() %>% 
  # Fit loess model for each joint and coord_segment
  mutate(
    loess_model_u1 = map(data, ~loess(u1 ~ pressure, data = .x, span = 0.5)),
    loess_model_u2 = map(data, ~loess(u2 ~ pressure, data = .x, span = 0.5))
  ) %>%
  # Compute resamples for each loess model
  mutate(
    resample_u1 = map2(loess_model_u1, data, ~predict(.x, newdata = .y)),
    resample_u2 = map2(loess_model_u2, data, ~predict(.x, newdata = .y))
  ) %>%
  # Unnest data and predictions
  unnest(c(data, resample_u1, resample_u2)) %>% 
  # Drop loess models
  select(
    time, pressure, joint, coord_segment, resample_u1, resample_u2, u1, u2,
    u1_init_offset
    ) %>% 
  ungroup()

# Plot resampled vs original data
pos_dat_resample %>%
  pivot_longer(
    cols = c(u1, u2, resample_u1, resample_u2),
    names_to = 'coord',
    values_to = 'coord_val'
  ) %>%
  ggplot(aes(
    x = pressure, y = coord_val, color = interaction(joint, coord_segment))) +
  facet_wrap(~coord, scales = 'free_y') +
  geom_point() +
  geom_smooth(
    aes(group = interaction(joint, coord_segment)),
    method = 'loess', se = F, color = 'black', span = 0.5
    ) +
  theme_minimal()

# Compute elongations of entire dataset and all parameters for arc approximations
pos_dat_wide_arc_resample <- pos_dat_resample %>%
  # Remove u1 and u2
  select(-u1, -u2) %>%
  # Remove TIP
  filter(joint != 'TIP') %>%
  # Remove 0 pre
  # Pivot wider using coord_segment and values from u1 and u2 and u1_init_offset
  pivot_wider(
    names_from = coord_segment,
    values_from = c(resample_u1, resample_u2, u1_init_offset)
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
        resample_u1_start, resample_u2_start, resample_u1_mid, resample_u2_mid,
        resample_u1_end, resample_u2_end
        ))
  ) %>% 
  # Return to columnwise
  ungroup() %>% 
  unnest_wider(arc_params) %>% 
  # Compute elongations
  mutate(
    delta_L = L_arc - L_init
  )

# Plot u1 vs u2 for all joints and segment point measures
pos_bend_resample_plt <- pos_dat_resample %>% 
  # Remove u1 and u2
  select(-u1, -u2) %>%
  # Plot original data
  ggplot(aes(x = resample_u1, y = resample_u2, color=joint)) +
  # Plot resampled data
  coord_fixed(
    xlim = c(0, 122),
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
    data = pos_dat_resample %>% 
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
    data = pos_dat_resample %>% 
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
    data = pos_dat_resample %>% 
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
    data = pos_dat_resample %>% 
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
    data = pos_dat_resample %>% 
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
    # For index
    # x = 130, y = -55, xend = 100, yend = -85,
    # For little
    x = 120, y = -55, xend = 90, yend = -85,
    curvature = -0.2,
    arrow = arrow(type = 'open', length = unit(0.2, 'cm')),
    color = 'black',
    linewidth = 0.3
  ) +
  geom_arc0(
    data = pos_dat_wide_arc_resample %>% 
      # Keep only MCP joints and downsample to keep every 10th row
      # no 0 bar data, so different than top
      group_by(joint) %>%
      # filter(joint == 'MCP') %>% 
      filter(row_number() %% 10 == 0) %>% 
      # Add mock u1, u2, resample_u1, resample_u2 variables equal to zero
      mutate(u1 = 0, u2 = 0, resample_u1 = 0, resample_u2 = 0),
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
    # Index
    # x = 90, y = -90,
    # Little
    x = 80, y = -90,
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
pos_bend_resample_plt
ggsave(
  # filename = 'plots/FEM_index_finger_movement_resample.png',
  filename = 'plots/FEM_little_finger_movement_resample.png',
  plot = pos_bend_resample_plt,
  device = grDevices::png,
  # width = 9, height = 7, units = 'cm', dpi = 360
  width = 8, height = 7, units = 'cm', dpi = 360
  )

# Compute each joint angles. Take xy coord difference between the following
# joint start, and previous joint end points. USe atan2 to compute angle.
joint_angles_resample <-
  pos_dat_resample %>% 
  # Remove u1, u2 and u1_init_offset
  select(-u1, -u2, -u1_init_offset) %>%
  # Remove coord_segment == 'mid
  # filter(coord_segment != 'mid') %>% 
  # pivot wider using joint and coord_segment
  pivot_wider(
    names_from = c(joint, coord_segment),
    values_from = c(resample_u1, resample_u2),
    names_glue = '{joint}_{coord_segment}_{.value}'
  ) %>% 
  # Compute neighbouring joint x and y differences
  mutate(
    MCP_diff_u1 = PIP_start_resample_u1 - MCP_end_resample_u1,
    MCP_diff_u2 = PIP_start_resample_u2 - MCP_end_resample_u2,
    PIP_diff_u1 = DIP_start_resample_u1 - PIP_end_resample_u1,
    PIP_diff_u2 = DIP_start_resample_u2 - PIP_end_resample_u2,
    # For tip there is only mid
    DIP_diff_u1 = TIP_mid_resample_u1 - DIP_end_resample_u1,
    DIP_diff_u2 = TIP_mid_resample_u2 - DIP_end_resample_u2
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
joint_angle_resample_plt <- joint_angles_resample %>% 
  pivot_longer(
    cols = c(MCP_angle, PIP_angle, DIP_angle),
    names_to = 'joint',
    values_to = 'value'
  ) %>% 
  # Remove _angle from joint names
  mutate(
    joint = str_remove(joint, '_angle'),
    joint = factor(joint, levels = c('MCP', 'PIP', 'DIP')),
    # Pressure to bars from Mpa
    pressure = pressure * 10
    ) %>%
  ggplot(aes(x = pressure, y = value, color = joint)) +
  facet_grid(~joint, scales = 'fixed',
             labeller = label_parsed
             ) +
  geom_point(
    aes(
      fill = joint, #interaction(rob, joint, sep=':'),
      # color = rep
      ),
    size = 1.0,
    stroke = 0.2,
    shape = 21,
    alpha = 0.9,
    color = 'black',
    # fill = pal_nejm()(8)[1],
    show.legend = T
    ) +
  # Smooth line with CI
  geom_smooth(
    # aes(color = joint), #interaction(rob, joint, sep=':')),
    color = 'black',
    method = 'lm',
    formula = y ~ poly(x, 2),
    se = T,
    # color = 'black',
    linewidth = 0.3,
    alpha = 0.4,
    show.legend = F
  ) +
  scale_color_nejm() +
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
    breaks = seq(0, 90, 5),
    # minor_breaks = seq(0, 1, length.out = 22)^1.8,
    expand = c(0, 0)
      ) +
  labs(
    x = expression("Pressure "*italic(p)*" [bar]"),
    y = expression("Joint Angle "*italic(theta)*" [°]")
  ) +
  # Customize legend
  guides(
    # Remove legend for color
    color = 'none',
    fill = guide_legend(
      title = 'Bellow:',
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
      colour="black", size = 7, margin = margin(0, 0., 1, 0, 'mm')
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
joint_angle_resample_plt
ggsave(
  # filename = 'plots/FEM_joint_angles_index.png',
  filename = 'plots/FEM_joint_angles_little.png',
  plot = joint_angle_resample_plt,
  device = grDevices::png,
  width = 16.5, height = 7, units = 'cm', dpi = 360
  )

# Plot elongation delta_l vs pressure for each joint
elongation_plt_resample <- pos_dat_wide_arc_resample %>% 
  ggplot(aes(x = pressure, y = delta_L, color = joint)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~joint, scales = 'fixed') +
  labs(
    x = 'Pressure (kPa)',
    y = 'Joint elongation (mm)',
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
elongation_plt_resample

# Save merged pos_data_wide with arc approximation parameters and joint_angles
pos_dat_wide_arc_resample %>% 
  left_join(
    joint_angles_resample %>% 
      rename(MCP = MCP_angle, PIP = PIP_angle, DIP = DIP_angle) %>%
      pivot_longer(
        cols = c(MCP, PIP, DIP),
        names_to = 'joint',
        values_to = 'theta_joint'
        ),
    by = c('pressure', 'joint')
    ) %>% 
  # write_csv('data/FEM_position_data_index_arc_approx_joint_elongation_angles_resample.csv')
  write_csv('data/FEM_position_data_little_arc_approx_joint_elongation_angles_resample.csv')

# Load model PRB data
model_prb_dat <- 
  # read_csv('data/PRB_position_data_little.csv') %>% 
  read_csv('data/PRB_position_data_index.csv') %>%
  separate(joint, into = c('joint', 'coord_segment'), sep = '_') %>% 
  rename(resample_u1 = u1, resample_u2 = u2)

# Plot u1 vs u2 for all joints and segment point measures
pos_bend_model_compare_plt <- pos_dat_resample %>% 
  select(pressure, joint, coord_segment, resample_u1, resample_u2) %>%
  mutate(source = 'FEM') %>%
  # Remove data for TIP joint
  filter(joint != 'TIP') %>%
  # Remove coord_segment mid
  filter(coord_segment != 'mid') %>%
  # Remove 0 pressure data
  filter(pressure >= 1e-6) %>%
  bind_rows(model_prb_dat %>% mutate(source = 'PRB')) %>% 
  # Compute L2 norm for each point between FEM and PRB data using 
  # resample_u1 and resample_u2
  group_by(pressure, joint, coord_segment) %>%
  mutate(
    L2_norm = sqrt((max(resample_u1) - min(resample_u1))^2 + (max(resample_u2) - min(resample_u2))^2)
  )  %>% 
  ungroup() %>% 
  # summarise(
  #   mean_L2_norm = mean(L2_norm),
  #   max_L2_norm = max(L2_norm)
  # ) %>% 
  # Plot data
  ggplot(aes(x = resample_u1, y = resample_u2)) +
  # Plot resampled data
  coord_fixed(
    # Index
    xlim = c(0, 125),
    # Little
    # xlim = c(0, 110),
    ylim = c(-90, 0),
  ) +
  geom_point(
    aes(fill = L2_norm, shape = source),
    size = 1.0,
    stroke = 0.0,
    # shape = 21,
    alpha = 0.6,
    # fill = pal_nejm()(8)[1],
    show.legend = T
    ) +
  # Label each path with pressure at tip coordinates
  geom_text(
    data = pos_dat_resample %>% 
      group_by(joint, coord_segment) %>%
      filter(row_number() %% 10 == 1) %>% 
      filter(joint == 'DIP' & coord_segment == 'end'),
    # Convert pressure to bar
    aes(label = pressure*10),
    nudge_x = 4,
    nudge_y = -6,
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'black',
    show.legend = F
  ) +
  # Add curved arrow to show bending direction
  annotate(
    geom = 'curve',
    # For index
    x = 120, y = -55, xend = 90, yend = -85,
    # For little
    # x = 105, y = -55, xend = 75, yend = -85,
    curvature = -0.2,
    arrow = arrow(type = 'open', length = unit(0.2, 'cm')),
    color = 'black',
    linewidth = 0.3
  ) +
  # Add text near arrow to signify it's pressure in bar
  annotate(
    geom = 'text',
    # Index
    x = 80, y = -92,
    # Little
    # x = 65, y = -92,
    label = "'Pressure '*italic(p)*' [bar]'",
    parse = T,
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'black',
  ) +
  # Add MCP, PIP, and DIP joint angles
  annotate(
    geom = 'text',
    x = 12, y = 0, # Little + Index
    label = "MCP",
    parse = T,
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'black',
  ) +
  annotate(
    geom = 'text',
    x = 60, y = 0, # Index
    # x = 57, y = 0, # Little
    label = "PIP",
    parse = T,
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'black',
  ) +
  annotate(
    geom = 'text',
    x = 98, y = 0, # Index
    # x = 90, y = 0, # Little
    label = "DIP",
    parse = T,
    hjust = 0,
    vjust = 0,
    size = 3,
    color = 'black',
  ) +
  scale_fill_viridis_c(
    name = 'L2 norm:',
    option = 'plasma'
    ) +
  scale_shape_manual(
    name = 'Data:',
    values = c(21, 23)
  ) +
  scale_color_nejm(
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
    # Shape override aestetics to see stroke
    shape = guide_legend(
      override.aes = list(
        fill = 'black',
        size = 1.5,
        stroke = 0.5
      )
    )
  ) +
  theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_text(
      size = 10, margin = margin(2, 2, 2, 6, 'mm')),
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
      colour="black", size = 8, margin = margin(0, 0., 0, 0, 'mm')
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
      color = 'black', size = 9.0, margin = margin(b = 0.3, t = 0.3, unit='mm')
    ),
    axis.ticks = element_line(linewidth = 0.2),
    # axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.1, 'lines')
  )
pos_bend_model_compare_plt
ggsave(
  filename = 'plots/Figure_4_FEM_PRB_index_finger_movement_resample_model_compare.png',
  # filename = 'plots/Figure_4_FEM_PRB_little_finger_movement_resample_model_compare.png',
  plot = pos_bend_model_compare_plt,
  device = grDevices::png,
  width = 8.6, height = 7, units = 'cm', dpi = 360
  # width = 7.7, height = 7, units = 'cm', dpi = 360
  )
