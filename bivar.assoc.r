bivar.assoc <- function(x, y = NULL) {
  
  if (is.null(y)) {
    ### If no y is supplied, x should be a formula
    if (is.formula(formula)) {
      if (length(as.list(attr(terms.formula(x), 'variables'))) == 3) {
        xVar <- as.list(attr(terms.formula(x), 'variables'))[3];
        yVar <- as.list(attr(terms.formula(x), 'variables'))[2];
      }
      else {
        stop("The formula has more than two variables! ",
             "Use a formula of the form 'y ~ x', or specify two variables.");
      }
    }
    else {
      stop("Either supply a formula of the form 'y ~ x', ",
           "or supply two variables.");
    }
  }
  else {
    ### We also have a y
  }
  
}