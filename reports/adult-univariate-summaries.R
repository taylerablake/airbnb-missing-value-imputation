makeDataReport(adult, output = "html", render = TRUE,
               onlyProblematic = FALSE,
               labelled_as = c("factor"),
               mode = c("summarize", "visualize", "check"),
               checks = setChecks(),
               listChecks = TRUE, maxProbVals = 10,
               file = "adult-train-summary.Rmd",
               maxDecimals = 2, addSummaryTable = TRUE,
               replace = TRUE,
               twoCol = TRUE,
               visuals = setVisuals(
                 factor = "barChartVisual",
                 character = "barChartVisual",
                 numeric = "histogramVisual",
                 integer = "histogramVisual", 
                 Date = "histogramVisual"))



