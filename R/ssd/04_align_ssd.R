plans_ssd <- matrix(NA_integer_, nrow = nrow(map_ssd), ncol = 5001)
enacted <- map_ssd$senate
idx <- which(map_ssd$rn %in% map_ssd_a$rn)
plans_sub <- get_plans_matrix(plans_ren_ssd_a)

plans_sub <- plans_sub + 1000

key <- tibble(enacted = enacted[idx], 
              sampled = plans_sub[, 1]) %>% 
  distinct()

plans_ssd[, ] <- enacted

plans_sub_ren <- plans_sub

for (i in seq_len(nrow(key))) {
  plans_sub_ren[plans_sub_ren == key$sampled[i]] <- key$enacted[i]
}
stopifnot(all(plans_sub_ren[, 1] == enacted[idx]))

plans_ssd[idx, ] <- plans_sub_ren

stopifnot(all(plans_ssd[, 1] == enacted))

plans_ssd <- plans_ssd[, -1]

plans_final_ssd <- redist_plans(
  plans_ssd,
  map = map_ssd,
  algorithm = 'smc'
) %>% 
  add_reference(ref_plan = map_ssd$senate, 'Enacted')

plans_final_ssd <- sum_pl(plans_final_ssd, map = map_ssd, perim_df = perim_df)