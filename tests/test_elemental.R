#---- Tests for elemental_index(), superlative_elemental_index(), and index methods ----
library(piar)

set.seed(1234)

# Make sure index objects are correctly structured with length-0 inputs
unclass(elemental_index(integer(0), integer(0), integer(0)))

unclass(superlative_elemental_index(integer(0), integer(0), integer(0), integer(0)))

# Make sure matrix/data.frame methods work with length-0 indexes
as.matrix(elemental_index(numeric(0)))

all.equal(as_elemental_index(as.matrix(elemental_index(numeric(0)))),
          elemental_index(numeric(0)))

as.matrix(elemental_index(numeric(0)), type = "contributions")

all.equal(as.data.frame(elemental_index(numeric(0))),
          data.frame(period = character(0), level = character(0), value = numeric(0)))

all.equal(as.data.frame(elemental_index(numeric(0)), type = "contributions"),
          data.frame(period = character(0), level = character(0), value = numeric(0)))

# Make indexes with some random data
dat <- data.frame(rel = replace(rlnorm(1e4), sample(1e4, 10), NA),
                  period = sample(letters, 1e4, TRUE),
                  ea = sample(1:5, 1e4, TRUE),
                  w1 = replace(rlnorm(1e4), sample(1e4, 10), NA),
                  w2 = runif(1e4))

epr1 <- with(
  dat, 
  elemental_index(rel, period, ea, contrib = TRUE)
)
epr2 <- with(
  dat, 
  elemental_index(rel, period, ea, r = -1, contrib = TRUE, na.rm = TRUE)
)
epr3 <- with(
  dat, 
  superlative_elemental_index(rel, period, ea, w1, w2, contrib = TRUE, na.rm = TRUE)
)

# Compare with an alternate implementation
epr11 <- aggregate(rel ~ as.character(ea) + period, dat, 
                   function(x) exp(weighted.mean(log(x))), 
                   na.action = na.pass)
epr22 <- aggregate(rel ~ as.character(ea) + period, dat, 
                   function(x) 1 / weighted.mean(1 / x),
                   na.action = na.omit)

all.equal(as.data.frame(epr1), epr11[c(2, 1, 3)], check.attributes = FALSE)
all.equal(as.data.frame(epr2), epr22[c(2, 1, 3)], check.attributes = FALSE)

# cumprod.index() method should be the same as using apply
all.equal(cumprod(epr1), t(apply(as.matrix(epr1), 1, cumprod)))

# Contributions should add up
all.equal(epr1$index, 
          lapply(epr1$contributions, function(x) sapply(x, sum) + 1))
all.equal(epr2$index, 
          lapply(epr2$contributions, function(x) sapply(x, sum, na.rm = TRUE) + 1))
all.equal(epr3$index, 
          lapply(epr3$contributions, function(x) sapply(x, sum, na.rm = TRUE) + 1))

# Compare Fisher index with the manual calculation
l <- with(dat, elemental_index(rel, period, ea, w1, r = 1, na.rm = TRUE))
p <- with(dat, elemental_index(rel, period, ea, w2, r = -1, na.rm = TRUE))
all.equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(epr3))

# Should work for other kinds of superlative indexes
sepr <- with(dat, superlative_elemental_index(rel, period, ea, w2 = w2, na.rm = TRUE, s = 3))
l <- with(dat, elemental_index(rel, period, ea, r = 1.5, na.rm = TRUE))
p <- with(dat, elemental_index(rel, period, ea, w2, r = -1.5, na.rm = TRUE))
all.equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(sepr))

# Test merge.index() method
epr3 <- merge(epr1, epr2)
all.equal(epr3[], rbind(epr1[], epr2[]))
all.equal(epr3$index$a, sapply(epr3$contributions$a, sum, na.rm = TRUE) + 1)
epr3$levels
epr3$periods

# Merging length-0 indexes does nothing
all.equal(merge(elemental_index(integer(0), integer(0), integer(0)), 
                elemental_index(integer(0), integer(0), integer(0))),
          elemental_index(integer(0), integer(0), integer(0)))

# Test stack.index() method
epr2 <- with(
  dat, 
  elemental_index(rel, toupper(period), ea, r = -1, contrib = TRUE, na.rm = TRUE)
)
epr3 <- stack(epr1, epr2)
all.equal(epr3[], cbind(epr1[], epr2[]))
all.equal(epr3$index$A, sapply(epr3$contributions$A, sum, na.rm = TRUE) + 1)
epr3$levels
epr3$periods

# Stacking and unstacking are opposite operations
all.equal(epr1, Reduce(stack, unstack(epr1)))

# Stacking/unstacking length-0 indexes does nothing
all.equal(stack(elemental_index(integer(0), integer(0), integer(0)), 
                elemental_index(integer(0), integer(0), integer(0))),
          elemental_index(integer(0), integer(0), integer(0)))

all.equal(unstack(stack(elemental_index(integer(0), integer(0), integer(0)), 
                        elemental_index(integer(0), integer(0), integer(0)))),
          elemental_index(integer(0), integer(0), integer(0)))

# Toy example that can be easily verified
dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

(epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE)))
unclass(epr)
as.matrix(epr)
as.matrix(epr, type = "contributions")
as.data.frame(epr)
as.data.frame(epr, type = "contributions")
epr[, 1]
epr[1, ]
cumprod(epr)

epr2 <- with(dat, elemental_index(rel, period, ea))

all.equal(epr[], epr2[])
all.equal(epr2, with(as.data.frame(epr2), elemental_index(value, period, level)))
all.equal(epr$levels, epr2$levels)
all.equal(epr$periods, epr2$periods)

as.matrix(epr2, type = "contributions")
as.data.frame(epr2, type = "contributions")
