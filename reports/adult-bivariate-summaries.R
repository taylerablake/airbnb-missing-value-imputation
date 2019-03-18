column_types <- data.frame(column = names(adult),
           type = sapply(adult, class)) %>%
  as_tibble

column_pairs <- expand.grid(var1 = names(adult),
                            var2 = names(adult)) %>%
  inner_join(., column_types, by = c("var1" = "column")) %>%
  rename(type1 = type) %>%
  inner_join(., column_types, by = c("var2" = "column")) %>%
  rename(type2 = type)
column_pairs

ggpairs(adult, columns = which(sapply(adult, class) == "numeric"),
        ggplot2::aes(colour=exceeds_50k),
        upper = list(continuous = wrap("points", alpha = 0.5), combo = "box_no_facet"),
        lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)))







