plans_shd <- matrix(NA_integer_, nrow = nrow(map_shd), ncol = 5001)
enacted <- map_shd$house
idx <- which(map_shd$rn %in% map_shd_a$rn)
plans_sub <- get_plans_matrix(plans_ren_shd_a)

plans_sub <- plans_sub + 1000

key <- tibble(enacted = enacted[idx], 
              sampled = plans_sub[, 1]) %>% 
  distinct()

plans_shd[, ] <- enacted

plans_sub_ren <- plans_sub

for (i in seq_len(nrow(key))) {
  plans_sub_ren[plans_sub_ren == key$sampled[i]] <- key$enacted[i]
}
stopifnot(all(plans_sub_ren[, 1] == enacted[idx]))

plans_shd[idx, ] <- plans_sub_ren

stopifnot(all(plans_shd[, 1] == enacted))

plans_shd <- plans_shd[, -1]

plans_final_shd <- redist_plans(
  plans_shd,
  map = map_shd,
  algorithm = 'smc'
) %>% 
  add_reference(ref_plan = map_shd$house, 'Enacted')

plans_final_shd <- sum_pl(plans_final_shd, map = map_shd, perim_df = perim_df)
