N <- 5e3
w <- 1e4
thin <- 10
cli_process_start('Simulating {N} plans.')
cli_ul()

cli_li('Setting up constraints.')

cons <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 1500, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = 1, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .5, admin = map_ssd_a_mrg$county) %>% 
  add_constr_custom(
    strength = 1000,
    fn = function(plan, distr) {
      x <- tapply(X = map_ssd_a_mrg$vap_ap_black, INDEX = plan, FUN = sum) /
        tapply(X = map_ssd_a_mrg$vap, INDEX = plan, sum)
      if (sum(x > 0.5) > 19) {
        500
      } else {
        0
      }
    }
  )

cli_li('Simulating.')
set.seed(14853)

plans_ssd_a <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons
)

cli_li('Scoring.')
plans_ssd_a_thin <- plans_ssd_a %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

set.seed(14854)

plans_ssd_b <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons, init_plan = last_plan(plans_ssd_a)
)

plans_ssd_b_thin <- plans_ssd_b %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

cons_c <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 1800, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = .1, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .25, admin = map_ssd_a_mrg$county)

set.seed(14855)
w <- 1e4
plans_ssd_c <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_c, init_plan = last_plan(plans_ssd_b)
)

plans_ssd_c_thin <- plans_ssd_c %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

cons_d <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 1800, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = .1, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .25, admin = map_ssd_a_mrg$county)

set.seed(14856)
w <- 1e4
plans_ssd_d <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_d, init_plan = last_plan(plans_ssd_c)
)

plans_ssd_d_thin <- plans_ssd_d %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

cons_e <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 1800, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = .1, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .25, admin = map_ssd_a_mrg$county)

set.seed(14856)
w <- 1e4
plans_ssd_e <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_e, init_plan = last_plan(plans_ssd_d)
)

plans_ssd_e_thin <- plans_ssd_e %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)


cons_f <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 1800, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = .1, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .2, admin = map_ssd_a_mrg$county) %>% 
  add_constr_grp_inv_hinge(strength = 300, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.48)

set.seed(14857)
w <- 1e4
plans_ssd_f <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_f, init_plan = last_plan(plans_ssd_e)
)

plans_ssd_f_thin <- plans_ssd_f %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

cons_g <- redist_constr(map_ssd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 2300, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.6) %>% 
  add_constr_status_quo(strength = .05, current = map_ssd_a_mrg$house) %>% 
  add_constr_splits(strength = .05, admin = map_ssd_a_mrg$county)

set.seed(14858)
w <- 3e4
plans_ssd_g <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_g, init_plan = last_plan(plans_ssd_f)
)

plans_ssd_g_thin <- plans_ssd_g %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

set.seed(14859)
w <- 3e4
plans_ssd_h <- redist_mergesplit(
  map = map_ssd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_g, init_plan = last_plan(plans_ssd_g)
)

plans_ssd_h_thin <- plans_ssd_h %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted') %>%
  sum_pl(map_ssd_a, perim_df = perim_df_ssd_a)

# final output ----
plans_final_ssd_a <- plans_ssd_h_thin %>% 
  `attr<-`('prec_pop', map_ssd_a$pop) %>% 
  subset_sampled() %>% 
  add_reference(ref_plan = redist.sink.plan(map_ssd_a$senate), 'Enacted')

plans_final_ssd_a <- plans_final_ssd_a %>%
  mutate(comp_reock = comp_reock(., map_ssd_a))

plans_ren_ssd_a <- match_numbers(plans_final_ssd_a, plan = 'Enacted') %>%
  group_by(draw) %>% 
  mutate(total_pop_overlap = sum(pop_overlap * total_pop) / sum(map_ssd_a$pop)) %>% 
  ungroup()

cli_process_done()