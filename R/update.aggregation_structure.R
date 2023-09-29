update.piar_aggregation_structure <- function(object, index,
                                              period = end(index), r = NULL,
                                              ...) {
  index <- as_index(index)
  period <- as.character(period)
  period <- match.arg(period, index$time)
  if (is.null(r)) {
    r <- if (is.null(index$r)) 1 else index$r
  }
  price_update <- factor_weights(r)
  eas <- match(object$eas, index$levels)
  if (anyNA(eas)) {
    warning("not all weights in 'object' have a corresponding index value")
  }
  epr <- chain(index)$index[[match(period, index$time)]]
  weights(object) <- price_update(epr[eas], object$weights)
  object
}
