library(piar)

set.seed(1234)

#---- Length 0 inputs ----
unclass(elemental_index(integer(0), integer(0), integer(0)))

as.matrix(elemental_index(numeric(0)))

as.matrix(elemental_index(numeric(0)), type = "contributions")

all.equal(as.data.frame(elemental_index(numeric(0))),
          data.frame(period = character(0), level = character(0), value = numeric(0)))

all.equal(as.data.frame(elemental_index(numeric(0)), type = "contributions"),
          data.frame(period = character(0), level = character(0), value = numeric(0)))

#---- Adding up contributions ----
dat <- data.frame(rel = replace(rlnorm(1e4), sample(1e4, 10), NA),
                  period = sample(letters, 1e4, TRUE),
                  ea = sample(1:5, 1e4, TRUE))

(epr1 <- with(dat, elemental_index(rel, period, ea, contrib = TRUE)))
(epr2 <- with(dat, elemental_index(rel, period, ea, r = -1, contrib = TRUE, na.rm = TRUE)))
all.equal(cumprod(epr1), t(apply(as.matrix(epr1), 1, cumprod)))
all.equal(epr1$index, lapply(epr1$contributions, function(x) sapply(x, sum) + 1))
all.equal(epr2$index, lapply(epr2$contributions, function(x) sapply(x, sum, na.rm = TRUE) + 1))

#---- Merging ----
epr3 <- merge(epr1, epr2)
all.equal(epr3[], rbind(epr1[], epr2[]))
all.equal(epr3$index$a, sapply(epr3$contributions$a, sum, na.rm = TRUE) + 1)
epr3$levels

all.equal(merge(elemental_index(integer(0), integer(0), integer(0)), 
                elemental_index(integer(0), integer(0), integer(0))),
          elemental_index(integer(0), integer(0), integer(0)))

#---- Stacking ----
epr2 <- with(dat, elemental_index(rel, toupper(period), ea, r = -1, contrib = TRUE, na.rm = TRUE))
epr3 <- stack(epr1, epr2)
all.equal(epr3[], cbind(epr1[], epr2[]))
all.equal(epr3$index$A, sapply(epr3$contributions$A, sum, na.rm = TRUE) + 1)
epr3$periods

all.equal(stack(elemental_index(integer(0), integer(0), integer(0)), 
                elemental_index(integer(0), integer(0), integer(0))),
          elemental_index(integer(0), integer(0), integer(0)))

all.equal(unstack(stack(elemental_index(integer(0), integer(0), integer(0)), 
                        elemental_index(integer(0), integer(0), integer(0)))),
          elemental_index(integer(0), integer(0), integer(0)))

#---- Toy example ----
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
