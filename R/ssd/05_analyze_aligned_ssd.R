# POPULATION OVERLAP stats ----
plans_ren_ssd <- match_numbers(plans_final_ssd, plan = 'Enacted') %>%
  group_by(draw) %>% 
  mutate(total_pop_overlap = sum(pop_overlap * total_pop) / sum(map_ssd$pop)) %>% 
  ungroup()

plans_ren_ssd %>% 
  subset_sampled() %>% 
  pull(total_pop_overlap) %>% 
  summary()

# POPULATION OVERLAP stats ----
plans_ren_ssd %>% 
  subset_sampled() %>% 
  pull(total_pop_overlap) %>% 
  summary()

# POLSBY POPPER stats ----
plans_final_ssd %>% 
  subset_ref() %>% 
  group_by(draw) %>% 
  summarize(comp_polsby = mean(comp_polsby)) %>% 
  pull(comp_polsby)

plans_final_ssd %>% 
  subset_sampled() %>% 
  group_by(draw) %>% 
  summarize(comp_polsby = mean(comp_polsby)) %>% 
  pull(comp_polsby) %>% 
  mean()

# REOCK stats ----
enact_reock <- plans_final_ssd %>% 
  subset_ref() %>% 
  mutate(comp_reock = comp_reock(., map_ssd))

enact_reock %>% 
  pull(comp_reock) %>% 
  mean()

mean_sample_reock <- plans_final_ssd_a %>% 
  subset_sampled() %>% 
  group_by(draw) %>% 
  summarize(comp_reock = mean(comp_reock)) %>% 
  pull(comp_reock) %>% 
  mean()

mean_enact_reock <- enact_reock %>% 
  filter(!district %in% redraws_ssd) %>% 
  pull(comp_reock) %>% 
  mean()

((27 * mean_enact_reock) + (29 * mean_sample_reock)) / 56

# VTD SPLITS stats ----
plans_final_ssd %>% 
  subset_ref() %>% 
  pull(vtd_splits) %>% 
  mean()

plans_final_ssd %>% 
  subset_sampled() %>% 
  pull(vtd_splits) %>% 
  mean()

# COUNTY SPLITS stats ----
plans_final_ssd %>% 
  subset_ref() %>% 
  pull(county_splits) %>% 
  mean()

plans_final_ssd %>% 
  subset_sampled() %>% 
  pull(county_splits) %>% 
  mean()
