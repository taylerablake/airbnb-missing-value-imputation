adult <- read_csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
                  col_names = FALSE)
names(adult) <- c("age", "workclass",
                  "fnlwgt", "education",
                  "education_num",
                  "marital_status",
                  "occupation",
                  "relationship",
                  "race",
                  "sex",
                  "capital_gain",
                  "capital_loss",
                  "hours_per_week",
                  "native_country",
                  "exceeds_50k")
cache("adult")