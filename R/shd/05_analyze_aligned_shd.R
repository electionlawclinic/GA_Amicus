# POPULATION OVERLAP stats ----
plans_ren_shd  <- match_numbers(plans_final_shd, plan = 'Enacted') %>%
  group_by(draw) %>% 
  mutate(total_pop_overlap = sum(pop_overlap * total_pop) / sum(map_shd$pop)) %>% 
  ungroup()

# POLSBY POPPER stats ----
plans_final_shd %>% 
  subset_ref() %>% 
  group_by(draw) %>% 
  summarize(comp_polsby = mean(comp_polsby)) %>% 
  pull(comp_polsby)

plans_final_shd %>% 
  subset_sampled() %>% 
  group_by(draw) %>% 
  summarize(comp_polsby = mean(comp_polsby)) %>% 
  pull(comp_polsby) %>% 
  mean()

# REOCK stats ----
enact_reock <- plans_final_shd %>% 
  subset_ref() %>% 
  mutate(comp_reock = comp_reock(., map_shd))

enact_reock %>% 
  pull(comp_reock) %>% 
  mean()

mean_sample_reock <- plans_final_shd_a %>% 
  subset_sampled() %>% 
  group_by(draw) %>% 
  summarize(comp_reock = mean(comp_reock)) %>% 
  pull(comp_reock) %>% 
  mean()

mean_enact_reock <- enact_reock %>% 
  filter(!district %in% redraws_shd) %>% 
  pull(comp_reock) %>% 
  mean()

((103 * mean_enact_reock) + (77 * mean_sample_reock)) / 180


# VTD SPLITS stats ----
plans_final_shd %>% 
  subset_ref() %>% 
  pull(vtd_splits) %>% 
  mean()

plans_final_shd %>% 
  subset_sampled() %>% 
  pull(vtd_splits) %>% 
  mean()

# COUNTY SPLITS stats ----
plans_final_shd %>% 
  subset_ref() %>% 
  pull(county_splits) %>% 
  mean()

plans_final_shd %>% 
  subset_sampled() %>% 
  pull(county_splits) %>% 
  mean()