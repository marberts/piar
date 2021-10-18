#---- Tests for aggregation_structure() and associated methods ----
library(piar)

# Build a pias for two corner cases and make sure the weights.pias() method works
unclass(aggregation_structure(list()))

weights(aggregation_structure(list()))

weights(aggregation_structure(list()), ea_only = TRUE)

unclass(aggregation_structure(list(1:5)))

weights(aggregation_structure(list(1:5)))

weights(aggregation_structure(list(1:5)), ea_only = TRUE)

# Build an improper pias
# Should give a warning about improper parent nodes
aggregation_structure(list(1:2, c(3, 3), c(4, 5)))

# A real-ish example
x1 <- c("1", "1", "1", "2")
x2 <- c("11", "11", "12", "21")
x3 <- c("111", "112", "121", "211")

# Test the print.pias() method
(agg1 <- aggregation_structure(list(x1, x2, x3)))

unclass(agg1)

# Using expand classification should give the same result
all.equal(agg1, aggregation_structure(expand_classification(x3)))

# Calculate weights with the weights.pias() method
weights(agg1)

weights(agg1, ea_only = TRUE)

# Unequal weights
weights(aggregation_structure(list(x1, x2, x3), 1:4))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)), na.rm = TRUE)

# Update with the update.aggregate() method
# Updating with a length-0 index should make the weights NA
epr <- elemental_index(integer(0))
index <- aggregate(epr, agg1)
all.equal(update(index)[-5], agg1[-5])
weights(agg1)
weights(update(index))

# Updating with an index that doesn't line up with the pias introduces NA weights
# These should carry up the aggregation structure
epr <- elemental_index(1, ea = "111")
index <- aggregate(epr, agg1)
weights(update(index))

# Accommodate a delimiter when expanding the classification by setting the width
(agg2 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 
                                                     c(2, 2, 1))))

unclass(agg2)

# Change start by setting the width
(agg3 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 
                                                     c(4, 1))))

unclass(agg3)
