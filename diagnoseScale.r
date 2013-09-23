###########################################################
###########################################################
###
### Function to generate an object with several useful
### statistics and a plot to assess how the elements
### (usually items) in a scale relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}


### This function checks whether the file for a
### self-made function exists; if so, it's loaded;
### if not, the file is downloaded from GitHub
loadOwnFunction <- function(fileName) {
  sourceLoaded <- FALSE;
  if (exists('libraryPath')) {
    if (file.exists(paste0(libraryPath, paste0(fileName, ".r")))) {
      source(paste0(libraryPath, paste0(fileName, ".r")));
      sourceLoaded <- TRUE;
    }
  }
  
  if (!sourceLoaded) {
    ### Note: I took this from
    ### https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/source_https.R
    ### and edited it to fit in here.
    
    # Filename: source_https.R
    # Purpose: function to source raw code from github project
    # Author: Tony Bryal
    # Date: 2011-12-10
    # http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
    safeRequire('RCurl');
    # read script lines from website using a security certificate
    script <- getURL(paste0("http://github.com/Matherion/library/raw/master/", fileName, ".r"),
                     followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem",
                                                                 package = "RCurl"));
    # parse lines and evaluate in the global environment
    eval(parse(text = script), envir= .GlobalEnv);
  }
}

### Functions for factor analysis
safeRequire('psych');
### Plots
safeRequire('ggplot2');
### Easier plots
safeRequire('GGally');
### For computing the GLB and Omega(total)
loadOwnFunction('scale.ic');

### Scale Diagnosis
diagnoseScale <- function(dat, variableNames) {

  ### Create object to store results
  res <- list();
  res$variableNames <- variableNames;
  
  ### Extract dataframe and select only complete cases
  res$dat <- dat[complete.cases(dat[, variableNames]), variableNames];
  res$n <- nrow(res$dat);
  
  ### Basic univariate descriptives
  res$describe <- describe(res$dat);
  
  ### Bivariate correlations
  res$cor <- cor(res$dat, use="complete.obs");
  
  ### Visual representation of bivariate correlations
  ### First generate a normal scattermatrix with histograms
  ### on the diagonal
  res$ggpairs.normal <- ggpairs(res$dat, diag=list(continuous="bar", discrete="bar"),
                                axisLabels="none");
  ### Then generate one with jittered points
  res$ggpairs.jittered <- ggpairs(res$dat, params=c(position="jitter"), axisLabels="none");
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
  
  ### Extract eigen values
  res$eigen <- eigen(res$cor);
  ### Determine how many factors have eigenvalues
  ### over 1 - note that we're not doing a real
  ### exploratory factor analysis, we're just interested
  ### in whether this scale works out (it's not
  ### unidimensional if more than one factor has an
  ### eigenvalue a lot over 1)
  res$factors <- sum(res$eigen$values > 1);
  
  ### If there are more than two items, do a principal
  ### component analysis and a factor analysis
  if (ncol(res$cor) > 2) {
    ### Principal components analysis
    res$pca <- principal(r = res$cor, n.obs = res$n, rotate="oblimin",
                         nfactors=res$factors);
    ### Exploratory factor analysis
    res$fa <- fa(r = res$cor, n.obs = res$n, rotate="oblimin",
                 fm="pa", nfactors=res$factors);
  }
  
  ### Internal consistency measures
  res$scale.ic <- scale.ic(dataframe=res$dat, itemnames=variableNames);
  
  ### Return results
  class(res) <- c('diagnoseScale');
  return(res);
}

print.diagnoseScale <- function(x) {
  print(x$describe);
  print(x$ggpairs.combined);
  print(x$scale.ic);
}
