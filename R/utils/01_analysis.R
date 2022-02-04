Mode <- function(v) {
  uv <- unique(v)
  uv[which.max(tabulate(match(v, uv)))][1]
}

sum_pl <- function(plans, map, perim_df) {
  
  plans <- plans %>% select(-ends_with(c('.x', '.y')))

    m <- get_plans_matrix(plans)
  
  if (missing(perim_df)) {
    cli_abort('{.arg perim_df} required.')
  }
  
  plans <- plans %>%
    mutate(
      dev = plan_parity(map),
      comp_frac = distr_compactness(map),
      comp_polsby = comp_polsby(., map, perim_df = perim_df),
      vap_ap_black = group_frac(map, vap_ap_black, vap),
      county_splits = splits_admin(., map, county),
      vtd_splits = splits_admin(., map, vtd)
    ) %>% 
    group_by(draw) %>% 
    mutate(n_bvap_md = sum(vap_ap_black > 0.5)) %>% 
    ungroup()
}