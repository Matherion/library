###########################################################
###########################################################
###
### R function for simulating regression analyses and
### computing venn diagrams and ballantines.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### For loading own functions
libraryPath <- "B:/Data/statistics/R/library/";

### Note that these first two functions don't actually
### do anything; they're just for loading packages and
### functions.

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE, quietly=TRUE);
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
### Contains mvrnorm, for multivariate normal simulation
safeRequire('MASS');

###########################################################
### Real function definitions
###########################################################

regSim <- function(meansVector, covarianceMatrix, n = 1000, digits=3) {
  ### Note: the dependent variable has to be the last
  ### variable!
  
  if !((ncol(covarianceMatrix)==3) & (length(meansVector)==3)) {
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
  
  ### Explanation of Manfred te Grotenhuis at
  ### http://www.linkedin.com/groupAnswers?viewQuestionAndAnswers=&discussionID=264309996&gid=4292855&commentID=156991012&trk=view_disc&fromEmail=&ut=2omBOax7qn5BY1
  ###
  ### Maybe it helps if you do the multiple regression
  ### yourself. Say we have Y and X and Z. We like to
  ### know the unique effect of X taking into account Z
  ### (and unique effect Z taking into account X).
  ### Now suppose you have only simple OLS regression
  ### (so only one predictor). First thing is that we
  ### like to eliminate from X everything that Z can
  ### explain. So we run OLS on X = a + b * Z. From
  ### this equation we take residuals X*
  ### (that is X minus (a + b * Z)). We also eliminate
  ### everything from Y that Z can explain: Y= a + b * Z.
  ### Again we take residuals Y* Now run an OLS with
  ### y* = a + b * X* this b is equal to the b1 in
  ### Y = a + b1 * X + b2 * Z! I have always found this
  ### explanation very useful to understand what is going on.
  
  ### Regressions of y on x1, y on x2, x1 on x2, and x2 on x1,
  ### and saving residuals
  
  ### y corrected for x1
  res$dat[[paste0(res$criterion, "_", res$covariates[1])]] <-
    lm(res$dat[[res$criterion]] ~ res$dat[[res$covariates[1]]])$residuals;
  ### y corrected for x2
  res$dat[[paste0(res$criterion, "_", res$covariates[2])]] <-
    lm(res$dat[[res$criterion]] ~ res$dat[[res$covariates[2]]])$residuals;
  ### x1 corrected for x2
  res$dat[[paste0(res$covariates[1], "_", res$covariates[2])]] <-
    lm(res$dat[[res$covariates[1]]] ~ res$dat[[res$covariates[2]]])$residuals;
  ### x2 corrected for x1
  res$dat[[paste0(res$covariates[2], "_", res$covariates[1])]] <-
    lm(res$dat[[res$covariates[2]]] ~ res$dat[[res$covariates[1]]])$residuals;
  
  ### Run the model predicting y from x1, both corrected for x2
  res[[paste0('lm.residuals_', res$covariates[1])]] <-
    lm(formula = formula(paste0(res$criterion, "_", res$covariates[2], " ~ ",
                                res$covariates[1], "_", res$covariates[2])),
       data = res$dat);
  ### Run the model predicting y from x2, both corrected for x1
  res[[paste0('lm.residuals_', res$covariates[2])]] <-
    lm(formula = formula(paste0(res$criterion, "_", res$covariates[1], " ~ ", 
                                res$covariates[2], "_", res$covariates[1])),
       data = res$dat);
  
  ### Function to store dataset for use in other software
  res$dat.export <- function(filename, digits=12, ...) {
    ### Note that SPSS cannot import text files with too many
    ### decimals. 12 seems to work; 16 doesn't seem to.
    write.table(round(res$dat, digits), filename, sep = "\t", row.names=FALSE, ...);
  }
  
  class(res) <- c('regSim');
  return(res);
}

print.regSim <- function(x, digits = x$digits, showPlot=TRUE) {
  if(showPlot) {
    grid.newpage();
    grid.draw(x$venn.diagram);
  }
  
  pred1 <- x$covariates[1];
  pred2 <- x$covariates[2];
  crit <- x$criterion;
  
  cat("Generated a sample of", x$n, "datapoints. Input means are",
      paste0(x$meansVector, collapse=", "), ", input variance/covariance matrix:\n");
  print(x$covarianceMatrix);
  cat("\n");
  
  cat(paste0("Zero-order r^2 between ", crit," and ", pred1," = ",
             round(x$sq.cor[pred1, crit], digits),
             " (r = ", round(x$cor[pred1, crit], digits), ", univariate regr. coeff = ",
             round(x$cor[pred1, crit] / (sd(x$dat[[pred1]]) * sd(x$dat[[crit]])), digits),
             ")\n"));
  cat(paste0("Zero-order r^2 between ", crit," and ", pred2," = ",
             round(x$sq.cor[pred2, crit], digits),
             " (r = ", round(x$cor[pred2, crit], digits), ", univariate regr. coeff = ",
             round(x$cor[pred2, crit] / (sd(x$dat[[pred2]]) * sd(x$dat[[crit]])), digits),
             ")\n"));
  cat(paste0("Variance in ", crit," only explained by ", pred1," (semi-partial r^2) = ",
             round(x$sq.cor.semipartial[crit, pred1], digits), "\n"));
  cat(paste0("Variance in ", crit," only explained by ", pred2," (semi-partial r^2) = ",
             round(x$sq.cor.semipartial[crit, pred2], digits), "\n"));
  cat(paste0("Variance in ", crit," explained by both ", pred1," and ", pred2," = ",
             round(x$sq.cor[crit, pred1], digits), " - ",
             round(x$sq.cor.semipartial[crit, pred1], digits), " = ",
             round(x$sq.cor[crit, pred1] - x$sq.cor.semipartial[crit, pred1], digits), "; or ",
             round(x$sq.cor[crit, pred2], digits), " - ",
             round(x$sq.cor.semipartial[crit, pred2], digits), " = ",
             round(x$sq.cor[crit, pred2] - x$sq.cor.semipartial[crit, pred2], digits), "\n"));
  cat(paste0("Total variance in ", crit," explained by both ", pred1," and ", pred2," = ",
             round(x$sq.cor[crit, pred1], digits), " + ",
             round(x$sq.cor.semipartial[crit, pred2], digits), " = ",
             round(x$sq.cor[crit, pred1] + x$sq.cor.semipartial[crit, pred2], digits), "; or ",
             round(x$sq.cor[crit, pred2], digits), " + ",
             round(x$sq.cor.semipartial[crit, pred1], digits), " = ",
             round(x$sq.cor[crit, pred2] + x$sq.cor.semipartial[crit, pred1], digits)), "\n");
  cat("\n");
  cat("--- Multivariate model, predicting", crit, "from both", pred1,"and", pred2,"---\n");
  cat(paste0("R^2 for ", deparse(x$lm.formula), " = ", round(x$r.squared, digits=digits), "\n"));
  cat(paste0("Regression coefficient for ", pred1," = ", round(x$lm$coefficients[[pred1]], digits),
             ', SE = ',
             round(coef(summary(x$lm))[pred1, 'Std. Error'], digits=digits),
             ' (t = ',
             round(coef(summary(x$lm))[pred1, 't value'], digits=digits),
             ', p = ',
             format.pval(coef(summary(x$lm))[pred1, 'Pr(>|t|)'], digits=digits),
             ")\n"));
  cat(paste0("Regression coefficient for ", pred2," = ", round(x$lm$coefficients[[pred2]], digits),
             ', SE = ',
             round(coef(summary(x$lm))[pred2, 'Std. Error'], digits=digits),
             ' (t = ',
             round(coef(summary(x$lm))[pred2, 't value'], digits=digits),
             ', p = ',
             format.pval(coef(summary(x$lm))[pred2, 'Pr(>|t|)'], digits=digits),
             ")\n"));
  cat("\n");
  
  cat("--- Bivariate model using residuals,\n---",
      "predicting", crit, "from", pred1,"after", pred2,"was removed from both\n");
  cat(paste0("R^2 = ", 
             round(summary(x[[paste0('lm.residuals_', pred1)]])$r.squared, digits=digits), "\n"));
  cat(paste0("Regression coefficient for ", pred1," = ",
             round(x[[paste0('lm.residuals_', pred1)]]$coefficients[[paste0(pred1, "_", pred2)]], digits=digits),
             ', SE = ',
             round(coef(summary(x[[paste0('lm.residuals_', pred1)]]))[paste0(pred1, "_", pred2), 'Std. Error'], digits=digits),
             ' (t = ',
             round(coef(summary(x[[paste0('lm.residuals_', pred1)]]))[paste0(pred1, "_", pred2), 't value'], digits=digits),
             ', p = ',
             format.pval(coef(summary(x[[paste0('lm.residuals_', pred1)]]))[paste0(pred1, "_", pred2), 'Pr(>|t|)'], digits=digits),
             ")\n"));
  
  cat("\n");
  cat("--- Bivariate model using residuals,\n---",
      "predicting", crit, "from", pred2,"after", pred1,"was removed from both\n");
  cat(paste0("R^2 = ", 
             round(summary(x[[paste0('lm.residuals_', pred2)]])$r.squared, digits=digits), "\n"));
  cat(paste0("Regression coefficient for ", pred2," = ",
             round(x[[paste0('lm.residuals_', pred2)]]$coefficients[[paste0(pred2, "_", pred1)]], digits=digits),
             ', SE = ',
             round(coef(summary(x[[paste0('lm.residuals_', pred2)]]))[paste0(pred2, "_", pred1), 'Std. Error'], digits=digits),
             ' (t = ',
             round(coef(summary(x[[paste0('lm.residuals_', pred2)]]))[paste0(pred2, "_", pred1), 't value'], digits=digits),
             ', p = ',
             format.pval(coef(summary(x[[paste0('lm.residuals_', pred2)]]))[paste0(pred2, "_", pred1), 'Pr(>|t|)'], digits=digits),
             ")\n"));
  
  invisible();
}
