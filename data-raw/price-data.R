set.seed(1234)
#---- Matched-sample data ----
ms_prices <- data.frame(
  period = rep(sprintf("%d%02d", 2020, 1:4), c(10, 9, 11, 10)),
    business = paste0("B",
                      c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3,
                        1, 1, 2, 2, 2, 3, 3, 3, 3,
                        1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4,
                        1, 2, 2, 2, 3, 3, 3, 3, 4, 4)),
    product = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                2, 3, 4, 5, 6, 7, 8, 9, 10,
                2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    price = round(runif(40) * 10, 2)
)

ms_weights <- data.frame(
  business = paste0("B", 1:5),
  classification = c("11", "11", "11", "12", "12"),
  weight = round(runif(5) * 1000, 0)
)

ms_prices$price[c(2, 13:15, 21, 31)] <- NA

#---- Fixed-sample data ----
fs_prices <- data.frame(
  period = rep(sprintf("%d%02d", 2020, 1:4), c(11, 11, 9, 9)),
  business = paste0("B", c(1:11, 1:11, 3:11, 3:11)),
  classification = c(
    "111", "111", "112", "112", "112", "112", "121", "121", "122", "122", "131",
    "111", "111", "112", "112", "112", "112", "121", "121", "122", "122", "131",
    "112", "112", "112", "112", "121", "121", "122", "122", "131",
    "112", "112", "112", "112", "121", "121", "122", "122", "131"
  ),
  price = round(runif(40) * 10, 2)
)

fs_prices$weight <- round(runif(11) * 1000, 0)[c(1:11, 1:11, 3:11, 3:11)]

fs_prices$price[c(2, 20:21, 23, 40)] <- NA

fs_weights <- data.frame(classification = c("111", "112", "121", "122", "131"),
                         weight = round(runif(5) * 1000, 0))
