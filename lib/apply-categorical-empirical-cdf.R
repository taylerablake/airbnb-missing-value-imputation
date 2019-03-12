apply_categorical_empirical_cdf <- function(x,
                                        class_variable,
                                        class_value){
  require(dplyr)
  group_rates <- data.frame(x = x,
                            class = class_variable) %>%
    group_by(x, class) %>%
    dplyr::count() %>%
    ungroup %>%
    group_by(x) %>%
    mutate(N = sum(n),
           p = n/sum(n)) %>%
    ungroup %>%
    filter(class == class_value & !is.na(x))
  
  Fn <- rep(NA, length(unique(group_rates$x)))
  for (this_group in 1:length(group_rates$x)){
    group_set <- group_rates$x[group_rates$p <= group_rates$p[this_group]]
    Fn[this_group] <-
      sum(x[class_variable == class_value] %in% group_set,
          na.rm = TRUE)/sum(class_variable == class_value & !is.na(x))
  }
  data.frame(x = x) %>%
    inner_join(., data.frame(x = group_rates$x,
                             Fn = Fn)) %>%
    .$Fn
}