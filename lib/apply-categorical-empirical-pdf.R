apply_categorical_empirical_pdf <- function(x,
                                            class_variable,
                                            class_value){
  require(dplyr)
  group_rates <- data.frame(x = x,
                            class = class_variable) %>%
    filter(!is.na(x)) %>%
    group_by(x, class) %>%
    dplyr::count() %>%
    ungroup %>%
    group_by(x) %>%
    mutate(N = sum(n),
           p = n/sum(n)) %>%
    ungroup %>%
    filter(class == class_value)

  group_rates <- group_rates %>%
    mutate(p = (p - min(group_rates$p))/(max(group_rates$p) - min(group_rates$p)))
  prob <- data.frame(x = x) %>%
    left_join(., group_rates %>% dplyr::select(x, p)) %>%
    .$p
  prob
}
