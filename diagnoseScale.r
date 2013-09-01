### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### Functions for factor analysis
safeRequire('psych');
### Plots
safeRequire('ggplot2');
### Easier plots
safeRequire('GGally');

### Scale Diagnosis
diagnoseScale <- function(dat, variableNames) {

  ### Create object to store results
  res <- list();
  
  ### Extract dataframe and select only complete cases
  dat <- dat[complete.cases(dat[, variableNames]), variableNames];
  
  ### Basic univariate descriptives
  res$describe <- describe(dat);
  
  ### Bivariate correlations
  res$cor <- cor(dat, use="complete.obs");
  
  ### Visual representation of bivariate correlations
  ### First generate a normal scattermatrix with histograms
  ### on the diagonal
  res$ggpairs.normal <- ggpairs(dat, diag=list(continuous="bar", discrete="bar"),
                                axisLabels="none");
  ### Then generate one with jittered points
  res$ggpairs.jittered <- ggpairs(dat, params=c(position="jitter"), axisLabels="none");
  ### Then place the histograms on the diagonal of
  ### the jittered scattermatrix
  res$ggpairs.combined <- res$ggpairs.jittered;
  for (currentVar in 1:length(variableNames)) {
    res$ggpairs.combined <-
      putPlot(res$ggpairs.combined,
              getPlot(res$ggpairs.normal, currentVar, currentVar),
              currentVar, currentVar);
  }

  ### Exploratory factor analysis
  #pa.out <- factor.pa(r = bfi, nfactors = 5, residuals = FALSE,
  #                    + rotate = "varimax", n.obs = NA, scores = FALSE, SMC = TRUE,
  #                    + missing = FALSE, impute = "median", min.err = 0.001, digits = 2,
  #                    + max.iter = 100, symmetric = TRUE, warnings = TRUE, fm = "pa")
  
  ### Internal consistency measures
  res$scale.ic <- scale.ic(dataframe=dat, itemnames=variableNames);

  ### Return results
  class(res) <- c('diagnoseScale');
  return(res);
}

print.diagnoseScale <- function(x) {
  print(x$describe);
  print(x$ggpairs.combined);
  print(x$scale.ic);
}
