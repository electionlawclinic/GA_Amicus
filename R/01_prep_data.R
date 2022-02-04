path_blk <- here('data/blk.rds')
path_cvap <- here('data/cvap.rds')
path_shp <- here('data/shp.rds')
path_perims <- here('data/perims.rds')
path_baf <- here('data/baf.rds')
path_blk_adj <- here('data/blk_adj')

if (file_exists(path_blk)) {
  blk <- read_rds(path_blk)
  cvap <- read_rds(path_cvap)
  shp_ga <- read_rds(path_shp)
  perim_df <- read_rds(path_perims)
  bafs <- read_rds(path_baf)
} else {
  # Census Data ----
  blk <- build_dec('block', 'GA') %>%
    breakdown_geoid() %>%
    st_transform(3857)
  
  if (!file_exists(path_blk_adj)) {
    blk_adj <- adjacency(blk)
    write_rds(blk_adj, path_blk_adj)
  } else {
    blk_adj <- read_rds(path_blk_adj)
  }
  
  vars <- tidycensus::load_variables(2020, dataset = 'pl') %>% 
    filter(str_detect(label, 'Black or African American'))
  vars_ap_black_pop <- vars %>% filter(startsWith(name, 'P1')) %>% pull(name)
  vars_ap_black_vap <- vars %>% filter(startsWith(name, 'P3')) %>% pull(name)
  
  blk_ap_black_pop <- tidycensus::get_decennial(
    geography = 'block', variables = vars_ap_black_pop,
    state = 'GA', year = 2020, output = 'wide'
  )
  blk_ap_black_pop_sum <- blk_ap_black_pop %>% 
    transmute(
      GEOID = GEOID, 
      pop_ap_black = rowSums(select(., starts_with('P')), na.rm = TRUE)
    )
  
  blk_ap_black_vap <- tidycensus::get_decennial(
    geography = 'block', variables = vars_ap_black_vap,
    state = 'GA', year = 2020, output = 'wide'
  )
  
  blk_ap_black_vap_sum <- blk_ap_black_vap %>% 
    transmute(
      GEOID = GEOID, 
      vap_ap_black = rowSums(select(., starts_with('P')), na.rm = TRUE)
    )
  
  baf <- PL94171::pl_get_baf('GA', 'VTD')[[1]] %>%
    mutate(
      STATEFP = censable::match_fips('GA'),
      vtd = paste0(STATEFP, COUNTYFP, DISTRICT)
    ) %>% 
    select(GEOID = BLOCKID, vtd)
  
  # CVAP Data ----
  if (!file_exists(path_cvap)) {
    cvap <- cvap::cvap_distribute_censable('GA') %>% 
      select(GEOID, starts_with('cvap'))
    vest_cw <- cvap::vest_crosswalk('GA')
    cvap <- PL94171::pl_retally(cvap, crosswalk = vest_cw)
    cvap <- cvap %>%
      select(GEOID, starts_with('cvap'))
    write_rds(cvap, path_cvap, compress = 'xz')
  } else {
    cvap <- read_rds(path_cvap)
  }
  
  # plan inputs ----
  if (!file_exists(path_baf)) {
    baf_l <- dir_ls(here('data'), glob = '*block-assignments.csv') %>% 
      map(~read_csv(.x, col_types = 'c'))
    baf_l <- map(names(baf_l), function(x) {
      name <- x %>% 
        path_file() %>% 
        path_ext_remove() %>% 
        str_remove('-block-assignments') %>% 
        str_replace('-', '_')
      baf_l[[x]] %>% 
        rename(GEOID = GEOID20,
               {{ name }} := District)
    })
    bafs <- purrr::reduce(baf_l, left_join, by = "GEOID") 
    write_rds(bafs, path_baf)
  } else {
    bafs <- read_rds(path_baf)
  }
  
  # Census labels ----
  c_baf <- PL94171::pl_get_baf("GA")
  muni_baf <- c_baf$INCPLACE_CDP %>%
    rename(GEOID = BLOCKID, muni = PLACEFP)
  vtd_baf <- c_baf$VTD %>% 
    transmute(
      GEOID = BLOCKID,
      vtd = paste0(COUNTYFP, DISTRICT)
    )
  
  # join ----
  blk <- blk %>%
    left_join(blk_ap_black_pop_sum, by = 'GEOID') %>%
    left_join(blk_ap_black_vap_sum, by = 'GEOID') %>% 
    left_join(vtd_baf, by = 'GEOID') %>%
    left_join(muni_baf, by = 'GEOID') %>% 
    left_join(bafs, by = 'GEOID') %>%
    left_join(cvap, by = 'GEOID')
  
  write_rds(blk, path_blk, compress = 'xz')
  
  # VTD level data ----
  blk <- blk %>% mutate(vtd_dist = paste0(vtd, '_', house))
  
  blk <- blk %>% 
    mutate(vtd_comp = paste0(vtd, '-', check_contiguity(blk_adj, blk$vtd_dist)$component))
  
  shp_ga <- blk %>%
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
  adj <- adjacency(shp_ga)
  
  shp_ga <- shp_ga %>% 
    mutate(
      adj = adj
    )
  
  write_rds(shp_ga, path_shp, compress = 'xz')
  
  perim_df <- prep_perims(shp = shp_ga, perim_path = path_perims)
}