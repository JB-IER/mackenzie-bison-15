plot_estimates <- function (object, x, plus = NULL, width = 50, height = width) {
  
  stopifnot(is.data.frame(object) || is.jags_analysis(object))
  stopifnot(is.character(x) && length(x) == 1)
  
  if(is.data.frame(object)) {
    data <- object
  } else {
    data <- predict(object, newdata = x)
  }
  
  rm(object)
  stopifnot(all(c(x, "estimate", "lower", "upper") %in% colnames(data)))
  
  gp <- ggplot(data = data, aes_string(x = x, y = "estimate"))
  if(is.factor(data[[x]])) {
    gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
  } else {
    gp <- gp + geom_line()
    gp <- gp + geom_line(aes(y = lower), linetype = "dotted")
    gp <- gp + geom_line(aes(y = upper), linetype = "dotted")
  }
  if(!is.null(plus))
    gp <- gp + plus
  
  gwindow(width = width, height = height)
  print(gp)
  invisible(gp)
}
