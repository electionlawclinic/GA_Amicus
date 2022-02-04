cli_process_start("Preparing ssd map objects")
cli_ul()

cli_li('Finding MMDs.')
cooper_mmd_ssd <- shp_ga_ssd %>% 
  st_drop_geometry() %>% 
  group_by(cooper_senate) %>% 
  summarise(pct_vap_ap_black = sum(vap_ap_black) / sum(vap),
            pct_cvap_black = sum(cvap_black) / sum(cvap)) %>% 
  filter(pct_vap_ap_black > 0.5) %>% 
  pull(cooper_senate)

redraws_ssd <- shp_ga_ssd %>% 
  filter(cooper_senate %in% cooper_mmd_ssd) %>% 
  pull(senate) %>% 
  unique()

cli_li('Making {.cls redist_map}s' )

map_ssd <- redist_map(
  shp_ga_ssd %>% mutate(rn = row_number()),
  total_pop = pop,
  adj = shp_ga_ssd$adj,
  existing_plan = senate
)

# considered contiguous in enacted
map_ssd$adj <- add_edge(adj = map_ssd$adj, 
                        v1 = which(map_ssd$vtd_comp == "027000659-2"), 
                        v2 = which(map_ssd$vtd_comp == "027000659-1")
)

map_ssd_a <- map_ssd %>% 
  filter(senate %in% redraws_ssd)

map_ssd_a$senate[map_ssd_a$vtd_comp == "293000555-2"] <- 16

map_ssd_a_mrg <- map_ssd_a %>% 
  merge_by(vtd_comp)

if (!file_exists(path_perims_ssd_a <- here('data/perims_ssd_a.rds'))) {
  perim_df_ssd_a <- prep_perims(map_ssd_a, perim_path = path_perims_ssd_a)
} else {
  perim_df_ssd_a <- read_rds(path_perims_ssd_a)
}

cli_end()
cli_process_done()