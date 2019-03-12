apply_numeric_empirical_cdf <- function(x,
                                          class_variable,
                                          class_value){
  Fn <- ecdf(x[class_variable == class_value])
  Fn(x)
}