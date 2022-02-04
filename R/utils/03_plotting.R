plot_hist <- function(plans, qty, title = NULL, fill = '#1e1e1e',
                      color = 'black', size = 0.25, ...) {
  cust_hist(plans, {{ qty }}, fill = fill, size = size, ...) +
    scale_color_plan() + 
    labs(title = title) + 
    theme_r21()
}

cust_hist <- function(plans, qty, bins = 25, ...) {
  v <- rlang::eval_tidy(rlang::enquo(qty), plans)
  
  is_int_small <- rlang::is_integerish(v) & (max(v) - min(v) <= 100)
  if (is_int_small) {
    p <- plans %>% 
      subset_sampled() %>% 
      ggplot(aes(x = {{ qty }})) +
      geom_histogram(aes(y = after_stat(density * width)),
                     ..., binwidth = 1, boundary = 0.5) +
      scale_y_continuous(name = 'Fraction of plans', labels = scales::percent) +
      scale_x_continuous(breaks = seq(0, 100, by = 2))
  } else {
    p <- plans %>% 
      subset_sampled() %>% 
      ggplot(aes(x = {{ qty }})) +
      geom_histogram(aes(y = after_stat(density * width)),
                     ..., bins = bins, boundary = 0) +
      scale_y_continuous(name = 'Fraction of plans', labels = scales::percent)
  }
  
  if (redist:::get_n_ref(plans) > 0) {
    p <- p +
      ggplot2::geom_vline(aes(xintercept = {{ qty }}, color = .data$draw), 
                          size = 0.75, data = subset_ref(plans)) +
      labs(color = 'Plan')
  }
  
  p
}
