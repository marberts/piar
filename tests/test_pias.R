library(piar)

# corner cases
unclass(aggregation_structure(list()))

weights(aggregation_structure(list()))

weights(aggregation_structure(list()), ea_only = TRUE)

unclass(aggregation_structure(list(1:5)))

weights(aggregation_structure(list(1:5)))

weights(aggregation_structure(list(1:5)), ea_only = TRUE)

# should give a warning about improper parent nodes
aggregation_structure(list(1:2, c(3, 3), c(4, 5)))

# a real-ish example
x1 <- c("1", "1", "1", "2")
x2 <- c("11", "11", "12", "21")
x3 <- c("111", "112", "121", "211")

(agg1 <- aggregation_structure(list(x1, x2, x3)))

unclass(agg1)

all.equal(agg1, aggregation_structure(expand_classification(x3)))

# calculate weights
weights(agg1)

weights(agg1, ea_only = TRUE)

weights(aggregation_structure(list(x1, x2, x3), 1:4))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)), na.rm = TRUE)

# change delimiter
(agg2 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(2, 2, 1))))

unclass(agg2)

# change start
(agg3 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(4, 1))))

unclass(agg3)
