###########################################################
###########################################################
###
### R file for simulating regression analyses and computing
### venn diagrams and ballantines.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### Note that these first two functions don't actually
### do anything; they're just for loading packages and
### functions.

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
  if (file.exists(paste0(libraryPath, paste0(fileName, ".r")))) {
    source(paste0(libraryPath, paste0(fileName, ".r")));
  } else {
    
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
    # parse lines and evaluate in the global environement
    eval(parse(text = script), envir= .GlobalEnv);
  }
}

###########################################################
### Load packages and functions
###########################################################

### Wrapper for computing partial correlations
loadOwnFunction('cor.partial');
### Plots
safeRequire('ggplot2');
### Easier plots
safeRequire('GGally');
### Venn diagrams
safeRequire('VennDiagram');
safeRequire('venneuler');

###########################################################
### Real function definitions
###########################################################

regSim <- function(meansVector, covarianceMatrix, n = 1000, digits=3) {
  ### Note: the dependent variable has to be the last
  ### variable!
  
  if (ncol(covarianceMatrix)>3) {
    stop("Sorry, only three variables are currently supported!");
  }
  
  ### Create object to return results and store
  ### original input
  res <- list(meansVector = meansVector,
              covarianceMatrix = covarianceMatrix,
              n = n,
              digits=digits);
  
  ### Generate variable names if none exist yet
  if (is.null(colnames(covarianceMatrix))) {
    colnames(covarianceMatrix) <-
      c(paste0('x', 1:(ncol(covarianceMatrix) - 1)), 'y');
  }
  
  ### Extract variable names
  res$varNames <- colnames(covarianceMatrix);
  
  ### Generate data
  res$dat <- as.data.frame(mvrnorm(n = n,  mu = means,
                                   Sigma = covarianceMatrix,
                                   empirical=TRUE));
  ### Set variable names
  names(res$dat) <- res$varNames;
  ### Store dependent and independent variables and
  ### generate expression for the regression
  res$criterion <- res$varNames[length(res$varNames)];
  res$covariates <- res$varNames[1:(length(res$varNames)-1)];
  res$lm.formula <- as.formula(paste(res$criterion, "~",
                                     paste(res$covariates, collapse= "+")));
  
  ### Generate scattermatrices
  res$ggpairs <- ggpairs(res$dat, diag=list(continuous="bar", discrete="bar"),
                         axisLabels="none");
  
  ### Compute R squared
  res$lm <- lm(res$lm.formula, data=res$dat);
  res$r.squared <- summary(res$lm)$r.squared;
  
  ### Compute zero-order correlations
  res$cor <- cor(res$dat);
  
  ### Compute partial correlations
  res$cor.partial <- pcor(res$dat);

  ### Compute semi-partial correlations
  res$cor.semipartial <- spcor(res$dat);
  
  ### Square correlations
  res$sq.cor <- res$cor^2;
  res$sq.cor.partial <- res$cor.partial$estimate^2;
  res$sq.cor.semipartial <- res$cor.semipartial$estimate^2;
  
  ### Compute variance shared by all variables
  res$sq.cor.common <- res$sq.cor - res$sq.cor.semipartial;
  
  ### Build Ballantines
  res$venn.charVector <- c();
  res$venn.weightsVector <- c();
  res$venn.diagram.parameter <- list();
  for (currentFirstVarIndex in 1:length(res$varNames)) {
    ### Add name of area to character vector for VennEuler
    res$venn.charVector <- c(res$venn.charVector, res$varNames[currentFirstVarIndex]);
    ### Add weight of area to character vector for VennEuler
    res$venn.weightsVector <- c(res$venn.weightsVector, 1);
    ### Add weight of area to list for Venn.Diagram
    res$venn.diagram.parameter[[paste0('area', currentFirstVarIndex)]] <- 1;
    
    if ((currentFirstVarIndex+1) <= length(res$varNames)) {
      for (currentSecondVarIndex in (currentFirstVarIndex+1):length(res$varNames)) {
        ### Add name of intersection to character vector for VennEuler
        res$venn.charVector <- c(res$venn.charVector,
                                 paste0(res$varNames[currentFirstVarIndex],
                                        "&",
                                        res$varNames[currentSecondVarIndex]));
        ### Add weight of intersection to character vector for VennEuler
        res$venn.weightsVector <- c(res$venn.weightsVector,
                                    res$sq.cor.semipartial[res$varNames[currentFirstVarIndex],
                                                           res$varNames[currentSecondVarIndex]]);
        ### Add weight of intersection to list for Venn.Diagram
        res$venn.diagram.parameter[[paste0('n',
                                           currentFirstVarIndex,
                                           currentSecondVarIndex)]] <-
          res$sq.cor[res$varNames[currentFirstVarIndex],
                     res$varNames[currentSecondVarIndex]];
        
        if ((currentSecondVarIndex+1) <= length(res$varNames)) {
          for (currentThirdVarIndex in (currentSecondVarIndex+1):length(res$varNames)) {
            ### Add name of intersection to character vector for VennEuler
            res$venn.charVector <- c(res$venn.charVector,
                                     paste0(res$varNames[currentFirstVarIndex],
                                            "&",
                                            res$varNames[currentSecondVarIndex],
                                            "&",
                                            res$varNames[currentThirdVarIndex]));
            ### Add weight of intersection to character vector for VennEuler
            res$venn.weightsVector <- c(res$venn.weightsVector,
                                        res$sq.cor.common[res$varNames[currentFirstVarIndex],
                                                          res$varNames[currentSecondVarIndex]]);
            ### Add weight of intersection to list for Venn.Diagram
            res$venn.diagram.parameter[[paste0('n',
                                               currentFirstVarIndex,
                                               currentSecondVarIndex,
                                               currentThirdVarIndex)]] <-
              res$sq.cor.common[res$varNames[currentFirstVarIndex],
                                res$varNames[currentSecondVarIndex]];
          } 
        }
      }
    }
  }
  
  res$venn.venneulerplot <- venneuler(combinations=res$venn.charVector,
                             weights=res$venn.weightsVector);
  
  res$venn.diagram.parameter <- lapply(res$venn.diagram.parameter, round, digits);
  
  tryCatch(
    res$venn.diagram <- draw.triple.venn(area1=res$venn.diagram.parameter$area1,
                                         area2=res$venn.diagram.parameter$area2,
                                         area3=res$venn.diagram.parameter$area3,
                                         n12=res$venn.diagram.parameter$n12,
                                         n13=res$venn.diagram.parameter$n13,
                                         n23=res$venn.diagram.parameter$n23,
                                         n123=res$venn.diagram.parameter$n123,
                                         category=res$varNames,
                                         ind=FALSE,
                                         fontfamily = rep("sans", 7),
                                         cat.fontfamily = rep("sans", 3),
                                         fill=c('#FF8888', '#8888FF', '#88FF88'),
                                         overrideTriple = TRUE
                                         ), error = function(e) {
                                           cat(paste("Error returned by draw.triple.venn: ",
                                                       e));
                                           cat("\nZero-order correlation matrix:\n");
                                           print(res$cor)
                                           cat("\nSemi-partial Correlation matrix:\n");
                                           print(res$cor.semipartial$estimate)
                                           cat("\nPartial Correlation matrix:\n");
                                           print(res$cor.partial$estimate)
                                           cat(paste("\nR square:", res$r.square, "\n"));
                                           cat("\nSquared zero-order correlation matrix:\n");
                                           print(res$sq.cor)
                                           cat("\nSquared semi-partial Correlation matrix:\n");
                                           print(res$sq.cor.semipartial)
                                           cat("\nSquared partial Correlation matrix:\n");
                                           print(res$sq.cor.partial)
                                           cat("\nSquared common variance matrix:\n");
                                           print(res$sq.cor.common)
                                         });

  class(res) <- c('regSim');
  return(res);
}

print.regSim <- function(x) {
  grid.newpage();
  grid.draw(x$venn.diagram);
  cat(paste0("\nSquared zero-order correlations:\n"));
  print(x$sq.cor, digits=x$digits);
  cat(paste0("\nR squared (", deparse(x$lm.formula), ") = ", round(x$r.squared, digits=x$digits), "\n"));
  cat("\nSquared semi-partial Correlation matrix:\n");
  print(round(x$sq.cor.semipartial, digits=x$digits));
  invisible();
}

###########################################################
### Start of simulations
###########################################################

means <- c(0, 0, 0);

covarianceMatrix <- matrix(c(  1,  .4,  .4,
                              .4,   1,  .4,
                              .4,  .4,   1), nrow=3, byrow=TRUE)
a <- regSim(means, covarianceMatrix);

covarianceMatrix <- matrix(c(  1, -.4,  .4,
                             -.4,   1,  .4,
                              .4,  .4,   1), nrow=3, byrow=TRUE)
b <- regSim(means, covarianceMatrix);

covarianceMatrix <- matrix(c( 1.00, 0.05, 0.30,
                              0.05, 1.00, 0.30,
                              0.30, 0.30, 1.00), nrow=3, byrow=TRUE)
c <- regSim(means, covarianceMatrix);

c;

a;

