# Visualization of DH parameters for index finger using approx planes
# FILE IS FOR COMPARISON OF FEMALE/MALE BONE SIZE WITH MODULAR APPROACH TO CALCULATE
# PHANALGE LENGTHS FORM HAND SIZE AND RATIOS

# Tidyverse
library(tidyverse)

# Plotting
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggsci)
library(scales)
library(processx)

# MANUAL index finger length from Python script
index_length_male = 153.2807799
index_length_female = 140.1043696

## Loading data
dh <-
  read_csv('DH_modified_modular_index_circular_male.csv') %>%
  mutate(grasp = 'circular', beta = 'real', sex='male') %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_circular_female.csv') %>% 
      mutate(grasp = 'circular', beta = 'real', sex='female')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_prismatic_male.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'real', sex='male')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_prismatic_female.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'real', sex='female')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_circular_male_no_beta.csv') %>% 
      mutate(grasp = 'circular', beta = 'zero', sex='male')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_circular_female_no_beta.csv') %>% 
      mutate(grasp = 'circular', beta = 'zero', sex='female')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_prismatic_male_no_beta.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'zero', sex='male')
  ) %>% 
  full_join(
    y = read_csv('DH_modified_modular_index_prismatic_female_no_beta.csv') %>% 
      mutate(grasp = 'prismatic', beta = 'zero', sex='female')
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
  # Index length
    ind_len = case_when(
      sex == 'male' ~ index_length_male,
      sex == 'female' ~ index_length_female,
      T ~ NA_real_
    )
  )

# Calculate error (euclidean distance) between real and zero beta angles
dh_error <- dh %>% select(-...1, -th_values, -a_values) %>%
  group_by(grasp, joint, th_mcp_fe, th_mcp_aa, th_cmc_fe, sex) %>% 
  pivot_wider(names_from = beta, values_from = c(x, y, z)) %>%
  mutate(
    e_dist = sqrt((x_real - x_zero)^2 + (y_real - y_zero)^2 + (z_real - z_zero)^2),
    rel_err = case_when(
      (x_real == 0 & y_real == 0 & z_real == 0) ~ NA_real_,
      # TRUE ~ e_dist / sqrt(x_real^2 + y_real^2 + z_real^2)
      TRUE ~ e_dist / ind_len
    )
  ) %>% ungroup() %>% select(-ends_with(c('real', 'zero')))
dh_error %>% select(sex, e_dist, rel_err) %>% distinct() %>% 
  arrange(desc(rel_err)) %>% View()

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
  group_by(joint, grasp, sex) %>%
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
  rel_err ~ (th_mcp_fe + th_mcp_aa + th_cmc_fe + joint + grasp + sex)^2,
  data = dh_error %>% mutate(joint = as.factor(joint), grasp = as.factor(grasp)))

summary(fit)
anova(fit)
## Conclusions 
### - on absolute error, significant effect of th_mcp_fe, joint, grasp and sex
### - on relative error, effect of sex not significant

# Visualize error
dh_error %>%
  mutate(rel_err = rel_err * 100) %>% 
  mutate(
    rel_err = case_when(
      sex == 'male' ~ rel_err + 0.05,
      T ~ rel_err
    )
  ) %>% 
  rename('AE/mm' = e_dist, 'RE/%' = rel_err) %>% 
  pivot_longer(
    cols = c('AE/mm', 'RE/%'), 
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
  group_by(joint, sex, th_cmc_fe, grasp) %>% 
  # slice(which(row_number() %% 7 == 1)) %>% 
  ungroup() %>% 
  distinct() %>% arrange(joint, th_mcp_fe) %>% 
  ggplot(aes(y = err_value, x = th_mcp_fe)) +
  facet_grid(err_metric ~ grasp, scales = 'free_y', switch = 'y') +
  geom_line(
    aes(color = joint, linetype = sex),
    stat = 'smooth',
    formula = y ~ s(x, bs = "cs"),
    method = 'gam',
    se = T,
    # alpha = 0.4,
    linewidth = 0.4
    # method = 'loess',
    # formula = y ~ x, span = 0.5
  ) +
  # geom_point(
  #   aes(fill=sex),
  #   # color = 'black',
  #   size = 0.3,
  #   # alpha=0.7,
  #   shape = 21,
  #   stroke = 0.0
  # ) +
  scale_y_continuous() +
  scale_x_continuous(
    name = expression(theta[MCP]*','[FE]*'/°'),
    breaks = seq(-10, 90, 10),
    expand = c(0.02, 0.02)
    # limits = c(2.5, -2.5),
    ) +
  scale_fill_manual(
    values = c('male' ='#000000', 'female' = '#6421a3'),
    # guide = 'none'
    name = 'Sex:'
  ) +
  scale_color_nejm(
    name = 'Joint:'
  ) +
  scale_linetype_manual(
    values = c('male' = '1242', 'female' = 'solid'),
    name = 'Sex:'
  ) +
  # ggtitle(expression('Model comparisons - real vs zero '*italic(beta))) +
  theme_bw() + theme(
    text = element_text(size = 5),
    # panel.border = element_rect(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.spacing = unit(1, 'mm'),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = 5, margin = margin(0.5, 1, 0.5, 1, 'mm')),
    strip.background = element_rect(fill = 'white'),
    legend.position = 'top',
    legend.box.spacing = unit(0.1, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1, 'mm'),
    legend.text = element_text(size = 5),
    legend.key.size = unit(3, 'mm'),
    legend.title = element_text(size = 6),
    legend.box.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(
      size = 6,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.x = element_text(
      colour="black", size = 5,
      margin = margin(0, 0, 0, 0, 'mm')
      ),
    axis.text.y = element_text(colour="black", size = 5),
    axis.title.y = element_blank(),
    axis.line = element_line(size=0.3, colour = "black"),
    # plot.title = element_text(
    #   hjust = 0.5, vjust = 0, size = 12,
    #   margin = margin(0, 0, 0.5, 0, 'mm')
    #   ),
    plot.margin = margin(0, 1, 0, 1, 'mm'),
  )

ggsave(filename = 'position_error_mcp_fe_modular_sex.png',
       width = 8.23, height = 4, units = 'cm', dpi = 320, pointsize = 12)
