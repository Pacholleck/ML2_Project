getVarImpPlot4Ranger <- function (x, 
                                  sort = TRUE, 
                                  n.var = min(30, nrow(x$variable.importance)),
                                  main) {
  
  imp <- x$variable.importance
  
  if (sort) {
    ord <- rev(order(imp, decreasing = TRUE)[1:n.var])
  } else {
    ord <- 1:n.var
  }
  
  xmin <- 0
  dotchart(imp[ord], 
           xlab = "importance", 
           ylab = "", 
           main = main, 
           xlim = c(xmin, max(imp))
  )
  
  invisible(imp)
}
