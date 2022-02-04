path_blk <- here('data/blk.rds')
path_blk_adj <- here('data/blk_adj')
path_shp_ssd <- here('data/shp_ssd.rds')
path_perims_ssd <- here('data/perims_ssd.rds')

if (file_exists(path_shp_ssd)) {
  blk <- read_rds(path_blk)
  blk_adj <- read_rds(path_blk_adj)
  shp_ga_ssd <- read_rds(path_shp_ssd)
  perim_df <- read_rds(path_perims_ssd)
  
} else {
  # From 01_prep_data ----
  blk <- read_rds(path_blk)
  blk_adj <- read_rds(path_blk_adj)
  
  # VTD level data ----
  blk <- blk %>% mutate(vtd_dist = paste0(vtd, '_', senate))
  
  blk <- blk %>% 
    mutate(vtd_comp = paste0(vtd, '-', check_contiguity(blk_adj, blk$vtd_dist)$component))
  
  shp_ga_ssd <- blk %>%
    group_by(vtd_comp, cooper_house, cooper_senate, house, senate) %>%
    summarize(
      state = state[1],
      county = county[1],
      vtd = vtd[1],
      muni = Mode(na.omit(muni)), # avoid the more NAs than muni issue
      across(starts_with(c('pop', 'vap', 'cvap')), sum),
      across(ends_with(c('rep', 'dem')), sum), 
      geometry = geos::geos_unary_union(geos::geos_make_collection(geometry)),
      .groups = 'drop'
    )
  
  # set up adj ----
  adj <- adjacency(shp_ga_ssd)
  
  shp_ga_ssd <- shp_ga_ssd %>% 
    mutate(
      adj = adj
    )
  
  write_rds(shp_ga_ssd, path_shp_ssd, compress = 'xz')
  
  perim_df <- prep_perims(shp = shp_ga_ssd, perim_path = path_perims_ssd)
}
