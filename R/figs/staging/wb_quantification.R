

# Fig 7A:  p-prkcda high mw vs low mw
prkcda_highlow_plot <- wb_quant |> 
  filter(md5sum == "9cc30") |> 
  pivot_wider(names_from = "band", values_from = "area") |> 
  mutate(ratio = top/bottom) |> 
  mutate(l2r = log2(ratio)) |> 
  mutate(treatment = recode(lane, "1" = "0", "2" = "30'", "3" = "60'", "4" = "120'", "5" = "0", "6" = "30'", "7" = "60'", "8" = "120'")) |> 
  mutate(celltype = ifelse(lane %in% c("1", "2", "3", "4"), "E4-HUVEC", "HUVEC")) |> 
  ggplot(mapping = aes(x = treatment, y = l2r)) +
  geom_col() +
  facet_wrap(~celltype, scales = "free_x") +
  theme(strip.background.x = ggh4x::element_part_rect(side = "b", color = "black", fill = "transparent")) +
  labs(x = "rhCXCL8", y = "log<sub>2</sub> band ratio") +
  theme(axis.title.y = element_markdown())


# pulldown

pulldown_data <- wb_quant |> 
  filter(str_detect(description, "pulldown")) |> 
  mutate(celltype = ifelse(str_detect(description, "e4"), "E4-HUVEC", "HUVEC")) |>
  mutate(blot = str_extract(description, "for .*")) |> 
  mutate(blot = str_remove(blot, "for ")) |> 
  mutate(blot = factor(blot, levels = c("p-paxillin", "actin"))) |> 
  mutate(treatment = ifelse(lane %in% c("1", "2", "3"), "rhCXCL8", "PBS")) |> 
  mutate(treatment = factor(treatment, levels = c("rhCXCL8", "PBS"))) |> 
  mutate(condition = recode(lane, "1" = "+", "2" = "-", "3" = "IP", "4" = "+", "5" = "-", "6" = "IP")) |> 
  mutate(condition = factor(condition, levels = c("+", "-", "IP")))

pulldown_plot <- left_join(pulldown_data, pulldown_data |> 
                             filter(condition == "+") |> 
                             select(md5sum, treatment, area), 
                           by = c("md5sum", "treatment")) |> 
  mutate(percent_input = area.x/area.y * 100) |> 
  mutate(row_label = paste0(celltype, " ", blot)) |> 
  ggplot(mapping = aes(x = condition, y = percent_input)) +
  geom_col() + 
  ggh4x::facet_nested(celltype+blot~treatment) +
  theme(strip.background.x = ggh4x::element_part_rect(side = "b", color = "black", fill = "transparent")) +
  theme(strip.background.y = ggh4x::element_part_rect(side = "l", color = "black", fill = "transparent")) +
  labs(y = "Percent Input", x = NULL)


# phospho erk; cxcl8 treatment
# instead need to normalize to actin and compare each lane directly
# get Z scores for normalizing


erk_cxcl8_rd <- wb_quant |> 
  filter(md5sum %in% c("00ca8", "89ee5", "0207f")) |> 
  mutate(blot = recode(md5sum, "00ca8" = "p-ERK", "89ee5" = "ERK", "0207f" = "actin")) |> 
  select(blot, lane, area) |>
  group_by(blot) |> 
  mutate(max_area = max(area)) |> 
  mutate(rel_density = area/max_area)

cxcl8_treatment_erk_plot <- erk_cxcl8_rd |> 
  left_join(erk_cxcl8_rd |> ungroup() |> filter(blot == "actin") |> select(lane, actin_rel_density = rel_density)) |> 
  mutate(adj_density = rel_density/actin_rel_density) |> 
  mutate(condition = recode(lane, "1" = "PBS", "2" = "rhCXCL8", "3" = "PBS", "4" = "rhCXCL8")) |> 
  mutate(celltype = recode(lane, "1" = "E4-HUVEC", "2" = "E4-HUVEC", "3" = "HUVEC", "4" = "HUVEC")) |>
  filter(blot != "actin") |> 
  mutate(blot = factor(blot, levels = c("p-ERK", "ERK"))) |> 
  ggplot(mapping = aes(x = condition, y = adj_density)) +
  geom_col() +
  facet_grid(cols = vars(celltype), rows = vars(blot)) +
  theme(strip.background.x = ggh4x::element_part_rect(side = "b", color = "black", fill = "transparent")) +
  theme(strip.background.y = ggh4x::element_part_rect(side = "l", color = "black", fill = "transparent")) +
  labs(y = "Adjusted Density", x = NULL) 

# phospho erk; prkcd knockdown
  
erk_knockdown_rd <-  wb_quant |> 
  filter(md5sum %in% c("049d5", "d52b6", "eff45")) |> 
  mutate(blot = recode(md5sum, "049d5" = "p-ERK", "d52b6" = "ERK", "eff45" = "actin")) |> 
  select(blot, lane, area) |>
  group_by(blot) |> 
  mutate(max_area = max(area)) |> 
  mutate(rel_density = area/max_area)

prkcda_knockdown_erk_plot <- erk_knockdown_rd |> 
  left_join(erk_knockdown_rd |> ungroup() |> filter(blot == "actin") |> select(lane, actin_rel_density = rel_density)) |> 
  mutate(adj_density = rel_density/actin_rel_density) |> 
  mutate(condition = recode(lane, "1" = "PBS", "2" = "rhCXCL8", "3" = "PBS", "4" = "rhCXCL88")) |> 
  mutate(celltype = recode(lane, "1" = "scramble", "2" = "scramble", "3" = "shPRKCD", "4" = "shPRKCD")) |>
  filter(blot != "actin") |> 
  mutate(blot = factor(blot, levels = c("p-ERK", "ERK"))) |> 
  ggplot(mapping = aes(x = condition, y = adj_density)) +
  geom_col() +
  facet_grid(cols = vars(celltype), rows = vars(blot), scales = "free") +
  theme(strip.background.x = ggh4x::element_part_rect(side = "b", color = "black", fill = "transparent")) +
  theme(strip.background.y = ggh4x::element_part_rect(side = "l", color = "black", fill = "transparent")) +
  labs(y = "Adjusted Density", x = NULL) 


# knockdown efficiency
kd_efficiency <- wb_quant |> 
  filter(md5sum %in% c("dfec6", "6b131")) |> 
  mutate(blot = recode(md5sum, "dfec6" = "prkcd", "6b131" = "actin")) |> 
  select(blot, lane, area) |> 
  group_by(blot) |> 
  mutate(max_area = max(area)) |> 
  mutate(rel_density = area/max_area)

prkcda_kd_efficiency_plot <- kd_efficiency |> 
  left_join(kd_efficiency |> 
              ungroup() |> 
              filter(blot == "actin") |> 
              select(lane, actin_rel_density = rel_density)) |>  
  mutate(adj_density = rel_density/actin_rel_density) |> 
  mutate(celltype = recode(lane, "1" = "scramble", "2" = "shPRKCD")) |>
  ggplot(mapping = aes(x = celltype, y = adj_density)) +
  geom_col() +
  labs(y = "Adjusted Density", x = NULL)
