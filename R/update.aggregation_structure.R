update.piar_aggregation_structure <- function(object, index,
                                              period = end(index), r = NULL,
                                              ...) {
  index <- as_index(index)
  if (is.null(r)) {
    r <- if (is.null(index$r)) 1 else index$r
  }
  price_update <- factor_weights(r)
  if (!all(object$eas %in% index$levels)) {
    warning("not all weights in 'object' have a corresponding index value")
  }
  epr <- chain(index)$index[[period]]
  weights(object) <- price_update(epr[object$eas], object$weights)
  object
}
