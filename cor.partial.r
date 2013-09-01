
### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### Partial and semipartial correlations
safeRequire('ppcor');
### Different package allowing calculations of partial correlations
safeRequire('psych');

### Convenient wrapper for the partial.r function from the
### psych package, which computes partial correlations
cor.partial <- function(dat, varsToCorrelate, varsToCorrectFor) {
  allVars <- c(varsToCorrelate, varsToCorrectFor);
  print(allVars);
  corMatrix <- cor(dat[, allVars]);
  varsToCorrelate.indices <- which(colnames(corMatrix) %in% varsToCorrelate);
  varsToCorrectFor.indices <- which(colnames(corMatrix) %in% varsToCorrectFor);
  corMatrix.partial <- partial.r(corMatrix, varsToCorrelate.indices, varsToCorrectFor.indices);
  return(corMatrix.partial);
}
