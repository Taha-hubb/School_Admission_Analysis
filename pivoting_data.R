library(tidyr)
# install.packages("datapasta")
gdp <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c(1L, 2L, 3L, 4L, 5L),
                V2 = c("Qatar", "Macao", "Luxembourg", "Singapore", "Brunei"),
                V3 = c("$128,647", "$115,367", "$107,641", "$94,105", "$79,003"),
                V4 = c("$61,264", "$80,890", "$105,280", "$56,746", "$28,572"),
                V5 = c("752%", "675%", "629%", "550%", "462%")
)
