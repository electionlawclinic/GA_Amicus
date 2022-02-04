cli_process_start("Preparing shd map objects")
cli_ul()

cli_li('Finding MMDs.')
cooper_mmd_shd <- shp_ga %>% 
  st_drop_geometry() %>% 
  group_by(cooper_house) %>% 
  summarise(pct_vap_ap_black = sum(vap_ap_black) / sum(vap)) %>% 
  filter(pct_vap_ap_black > 0.5) %>% 
  pull(cooper_house)

redraws_shd <- shp_ga %>% 
  filter(cooper_house %in% cooper_mmd_shd) %>% 
  pull(house) %>% 
  unique()

cli_li('Making {.cls redist_map}s' )

map_shd <- redist_map(
  shp_ga %>% mutate(rn = row_number()),
  total_pop = pop,
  adj = shp_ga$adj,
  existing_plan = house
)

map_shd_a <- map_shd %>% 
  filter(house %in% (redraws_shd %>% setdiff(c(161, 163, 165))))

map_shd_a_mrg <- map_shd_a %>% 
  merge_by(vtd_comp)

if (!file_exists(path_perims_shd_a <- here('data/perims_shd_a.rds'))) {
  perim_df_shd_a <- prep_perims(map_shd_a, perim_path = path_perims_shd_a)
} else {
  perim_df_shd_a <- read_rds(path_perims_shd_a)
}

cli_end()
cli_process_done()