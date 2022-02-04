N <- 5e3
w <- 5e4
thin <- 10
cli_process_start('Simulating {N} plans.')
cli_ul()

cli_li('Simulating A.')

set.seed(14853)

cons <- redist_constr(map_shd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 2250, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.72) %>% 
  add_constr_status_quo(strength = 3, current = map_shd_a_mrg$house) %>% 
  add_constr_splits(strength = .4, admin = map_shd_a_mrg$county) %>% 
  add_constr_custom(
    strength = 1000,
    fn = function(plan, distr) {
      x <- tapply(X = map_shd_a_mrg$vap_ap_black, INDEX = plan, FUN = sum) /
        tapply(X = map_shd_a_mrg$vap, INDEX = plan, sum)
      if (sum(x > 0.5) > 53) {
        500
      } else {
        0
      }
    }
  )

plans_shd_a <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons
)

cli_li('Scoring.')
plans_shd_a_thin <- plans_shd_a %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)

set.seed(14854)
plans_shd_b <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin, warmup = 0,
  counties = county, constraints = cons, init_plan = last_plan(plans_shd_a)
)

plans_shd_b_thin <- plans_shd_b %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)

cons_c <- redist_constr(map_shd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 2500, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.70) %>% 
  add_constr_status_quo(strength = 3, current = map_shd_a_mrg$house) %>% 
  add_constr_splits(strength = .25, admin = map_shd_a_mrg$county) %>% 
  add_constr_custom(
    strength = 1000,
    fn = function(plan, distr) {
      x <- tapply(X = map_shd_a_mrg$vap_ap_black, INDEX = plan, FUN = sum) /
        tapply(X = map_shd_a_mrg$vap, INDEX = plan, sum)
      if (sum(x > 0.5) > 53) {
        500
      } else {
        0
      }
    }
  )

set.seed(14855)
w <- 1e4
plans_shd_c <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_c, init_plan = last_plan(plans_shd_b)
)

plans_shd_c_thin <- plans_shd_c %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)


cons_d <- redist_constr(map_shd_a_mrg) %>% 
  add_constr_grp_hinge(strength = 2000, group_pop = vap_ap_black, total_pop = vap,
                       tgts_group = 0.501) %>%
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.70) %>% 
  add_constr_status_quo(strength = 1, current = map_shd_a_mrg$house) %>% 
  add_constr_splits(strength = .1, admin = map_shd_a_mrg$county)


set.seed(14856)
w <- 1e4
plans_shd_d <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_d, init_plan = last_plan(plans_shd_c)
)

plans_shd_d_thin <- plans_shd_d %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)

cons_e <- redist_constr(map_shd_a_mrg) %>% 
  add_constr_grp_inv_hinge(strength = 100, group_pop = vap_ap_black, total_pop = vap,
                           tgts_group = 0.68) %>% 
  add_constr_custom(
    strength = 1000,
    fn = function(plan, distr) {
      x <- tapply(X = map_shd_a_mrg$vap_ap_black, INDEX = plan, FUN = sum) /
        tapply(X = map_shd_a_mrg$vap, INDEX = plan, sum)
      if (sum(x > 0.5) < 52 || sum(x > 0.5) > 53) {
        500
      } else {
        0
      }
    }
  ) %>% 
  add_constr_custom(
    strength = 2000,
    fn = function(plan, distr) {
      x <- tapply(X = map_shd_a_mrg$vap_ap_black, INDEX = plan, FUN = sum) /
        tapply(X = map_shd_a_mrg$vap, INDEX = plan, sum)
      max(0, .5 - sort(x, TRUE)[54])
    }
  )


set.seed(14857)
w <- 1e4
plans_shd_e <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_e, init_plan = last_plan(plans_shd_d)
)

plans_shd_e_thin <- plans_shd_e %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)

set.seed(14858)
w <- 0
plans_shd_f <- redist_mergesplit(
  map = map_shd_a_mrg, nsims = N * thin + w, warmup = w,
  counties = county, constraints = cons_e, init_plan = last_plan(plans_shd_e)
)

plans_shd_f_thin <- plans_shd_f %>% 
  pullback() %>% 
  subset_sampled() %>% 
  filter(draw %in% seq(1, N * thin, by = thin)) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a)


# combine after long chain ----
plans_final_shd_a <- rbind(plans_shd_e_thin %>% filter(n_bvap_md == 53),
                           plans_shd_f_thin %>% filter(n_bvap_md == 53)) %>% 
  slice(1: (77 * 5000)) %>% 
  select(-chain) %>% 
  mutate(draw = factor(rep(1:5000, each = 77))) %>% 
  `attr<-`('prec_pop', map_shd_a$pop) %>% 
  add_reference(ref_plan = redist.sink.plan(map_shd_a$house), 'Enacted') %>%
  sum_pl(map_shd_a, perim_df = perim_df_shd_a) 

plans_final_shd_a <- plans_final_shd_a %>%
  mutate(comp_reock = comp_reock(., map_shd_a))

plans_ren_shd_a  <- match_numbers(plans_final_shd_a, plan = 'Enacted') %>%
  group_by(draw) %>% 
  mutate(total_pop_overlap = sum(pop_overlap * total_pop) / sum(map_shd_a$pop)) %>% 
  ungroup()

cli_process_done()