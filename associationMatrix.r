###########################################################
###
### R file with the function associationMatrix, which
### provides a symmatric or asymmetric matrix of
### associations, their confidence intervals, and p-values.
### The p-values can be corrected for multiple testing.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################

libraryPath <- "B:/Data/statistics/R/library/";

###########################################################
### Define 'preliminary' functions
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
}

### This function checks whether the file for a
### self-made function exists; if so, it's loaded;
### if not, the file is downloaded from GitHub
loadOwnFunction <- function(fileName, librarypath = NULL) {
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
    # parse lines and evaluate in the global environment
    eval(parse(text = script), envir= .GlobalEnv);
  }
}

### For computing cohen's d
loadOwnFunction('meanDiff', libraryPath);
loadOwnFunction('es.ci', libraryPath);

### For eta squared confidence interval
safeRequire("MBESS");

###########################################################
### Define main functions
###########################################################

### First, we define the functions that will compute the
### results. We have functions for the different combinations
### of measurement levels of the two variables. We store
### these functions in an object, so that they can be
### called dynamically depending on the preferred statistics
### and effect size measures of the user.

computeStatistic <- list();

### Function for the t-test
computeStatistic[['t']] <- function(var1, var2, conf.level=.95) {
  if (nlevels(as.factor(var1)) == 2) {
    dichotomous <- factor(var1);
    interval <- var2;
  }
  else if (nlevels(as.factor(var2)) == 2) {
    dichotomous <- factor(var2);
    interval <- var1;
  }
  else {
    stop("Error: none of the two variables has only two levels!");
  }
  res <- list();
  res$object <- meanDiff(interval ~ dichotomous, conf.level = conf.level,
                         envir=environment());
  res$statistic <- res$object$t;
  res$statistic.type <- "t";
  res$parameter <- res$object$df;
  res$p.raw <- res$object$p;
  return(res);
}

### Function for the Pearson correlation (r)
computeStatistic[['r']] <- function(var1, var2, conf.level=.95) {
  res <- list();
  res$object <- cor.test(var1, var2, use="complete.obs");
  res$statistic <- res$object$statistic;
  res$statistic.type <- "r";
  res$parameter <- res$object$parameter;
  res$p.raw <- res$object$p.value;
  return(res);
}

### Function for Anova (f)
computeStatistic[['f']] <- function(var1, var2, conf.level=.95) {
  if (nlevels(as.factor(var1)) < nlevels(as.factor(var2))) {
    ## We will treat var1 as factor
    factor <- factor(var1);
    dependent <- var2;
  }
  else {
    factor <- factor(var2);
    dependent <- var1;
  }
  
  ### In the future perhaps include tests of
  ### homogeneity of variances:
  #bartlett.test(dependent ~ factor);
  #fligner.test(dependent ~ factor);
  #safeRequire('car');
  #levene.test(dependent ~ factor);
  
  res <- list();
  res$object <- aov(dependent ~ factor);
  res$statistic <- summary(res$object)[[1]][['F value']][1];
  res$statistic.type <- "f";
  res$parameter <- c(summary(res$object)[[1]][['Df']]);
#                    summary(res$object)[[2]][['Df']]);
  res$p.raw <- summary(res$object)[[1]][['Pr(>F)']][1];
  return(res);
}

### Function for chi-square (chisq)
computeStatistic[['chisq']] <- function(var1, var2, conf.level=.95) {
  res <- list();
  res$object <- chisq.test(var1, var2, correct=FALSE);
  res$statistic <- res$object$statistic;
  res$statistic.type <- "chisq";
  res$parameter <- res$object$parameter;
  res$p.raw <- res$object$p.value;
  return(res);
}

computeEffectSize <- list();

### Effect size Cohens d
computeEffectSize[['d']] <- function(var1, var2, conf.level=.95) {
  if (nlevels(as.factor(var1)) == 2) {
    dichotomous <- factor(var1);
    interval <- var2;
  }
  else if (nlevels(as.factor(var2)) == 2) {
    dichotomous <- factor(var2);
    interval <- var1;
  }
  else {
    stop("Error: none of the two variables has only two levels!");
  }
  res <- list();
  res$object <- meanDiff(interval ~ dichotomous, conf.level = conf.level,
                         envir=environment());
  res$es <- res$object$meanDiff.g;
  res$es.type <- "g";
  res$ci <- c(res$object$meanDiff.g.ci.lower,
              res$object$meanDiff.g.ci.upper);
  return(res);
}

### Effect size Pearson's r
computeEffectSize[['r']] <- function(var1, var2, conf.level=.95) {
  res <- list();
  res$object <- cor.test(var1, var2, use="complete.obs");
  res$es <- res$object$estimate;
  res$es.type <- "r";
  res$ci <- res$object$conf.int;
  return(res);
}

### Function for eta squared (etasq)
computeEffectSize[['etasq']] <- function(var1, var2, conf.level=.95) {
  if (nlevels(as.factor(var1)) < nlevels(as.factor(var2))) {
    ## We will treat var1 as factor
    factor <- factor(var1);
    dependent <- var2;
  }
  else {
    factor <- factor(var2);
    dependent <- var1;
  }
  res <- list();
  res$object <- aov(dependent ~ factor);
  
  df_num <- summary(res$object)[[1]][1,1];
  df_den <- summary(res$object)[[1]][2,1];
  f_val <- summary(res$object)[[1]][1,4];
  
  ### This is suggested by the page at
  ### http://yatani.jp/HCIstats/ANOVA#RCodeOneWay
  ### (capture.output used because this function for
  ###  some reason very tenaciously outputs results)
  ### (also note that we double the 'unconfidence' level,
  ###  e.g. conf.level=.95 becomes conf.level=.90, to
  ###  retain consistency with the NHST p-value; see
  ###  the Word doc by Karl Wuensch references above,
  ###  or the paper he cites:
  ###    Steiger, J. H. (2004). Beyond the F test:
  ###      Effect size confidence intervals and tests
  ###      of close fit in the analysis of variance and
  ###      contrast analysis. Psychological methods, 9(2),
  ###      164-82. doi:10.1037/1082-989X.9.2.164
  
  res$es <- df_num*f_val/(df_den + df_num*f_val);
  res$es.type <- "etasq";
  res$ci <-  es.ci(distribution='f', statistic=f_val, df1=df_num, df2=df_den,
                   conf.level=1-((1-conf.level)*2));
  
  return(res);
}  

### Function for Cramers V effect size (v)
computeEffectSize[['v']] <- function(var1, var2, conf.level=.95) {
  res <- list();
  suppressWarnings(res$object <- chisq.test(var1, var2, correct=FALSE));
  phi <- sqrt(res$object$statistic/sum(res$object$observed));
  res$es <- sqrt(phi^2/(min(dim(res$object$observed))-1));

  ### Cramer's V can be computed by:
  ###   V = sqrt(chisq / [nobs * (min(ncols, nrows) - 1)])
  #res$es <- 
  
  ### Page 40 of the book 'Confidence Intervals' by Michael Smithson
  ### states that these noncentral chisquare values can be
  ### converted into Cramer's V values using the following
  ### formula:
  ###    V = sqrt((nc.chisq + df) / (N * (K - 1)))
  ###
  ### Where nc.chisq is the noncentral chisquare value,
  ### df is the degrees of freedom of the chi-square
  ### statistic, or (columns - 1) * (rows - 1), N is
  ### the sample size, and K is either the number of
  ### columns or the number of rows, whichever is smaller.
  ### So, to make things easier:
  chisq <- as.numeric(res$object$statistic);
  Df <- as.numeric(res$object$parameter);
  N <- sum(res$object$observed);
  K <- min(dim(res$object$observed))-1;
  
  cat(chisq, "\n", Df, "\n", N, "\n", K);
  
  suppressWarnings(noncentralchisq.lower <-
    conf.limits.nc.chisq(Chi.Square = chisq,
                         conf.level = conf.level,
                         df = Df)$Lower.Limit);
  suppressWarnings(noncentralchisq.upper <-
    conf.limits.nc.chisq(Chi.Square = chisq,
                         conf.level = conf.level,
                         df = Df)$Upper.Limit);
  
  v.lower <- sqrt((noncentralchisq.lower + Df) / (N * (K - 1)))
  v.upper <- sqrt((noncentralchisq.upper + Df) / (N * (K - 1)))
  
  res$ci <- c(v.lower, v.upper);
  
  res$es.type <- "V";
  res$ci <- c(1,1);
  return(res);
}

### This is the function that calls the functions
### to compute statistics and effect sizes, and
### organises the resulting objects in sets of
### lists. The elements of the first list are
### the 'rows' of the matrix. Each element (each
### 'row') is itself again a list, where each
### element corresponds to a 'cell' in the
### final 'matrix'. Each of these element (each
### of these cells) contains two objects; the one
### containing the statistic and the one
### containing the effect size.

associationMatrix <- function(dat, x, y=NULL, conf.level = .95,
                              correction = "fdr", digits = 2,
                              pval=FALSE, colspace=2, rowspace=0,
                              colNames ="numbers",
                              output="R",
                              env.LaTeX = 'tabular',
                              pboxWidthMultiplier = 1,
                              statistic =
                                list(dichotomous =
                                       list(dichotomous = "chisq",
                                            nominal = "chisq",
                                            ordinal = "chisq",
                                            numeric = "t"),
                                     nominal =
                                       list(dichotomous = "chisq",
                                            nominal = "chisq",
                                            ordinal = "chisq",
                                            numeric = "f"),
                                     ordinal =
                                       list(dichotomous = "chisq",
                                            nominal = "chisq",
                                            ordinal = "chisq",
                                            numeric = "f"),
                                     numeric =
                                       list(dichotomous = "t",
                                            nominal = "f",
                                            ordinal = "f",
                                            numeric = "r")),                                     
                              effectSize =
                                list(dichotomous =
                                       list(dichotomous = "v",
                                            nominal = "v",
                                            ordinal = "v",
                                            numeric = "d"),
                                     nominal =
                                       list(dichotomous = "v",
                                            nominal = "v",
                                            ordinal = "v",
                                            numeric = "etasq"),
                                     ordinal =
                                       list(dichotomous = "v",
                                            nominal = "v",
                                            ordinal = "v",
                                            numeric = "etasq"),
                                     numeric =
                                       list(dichotomous = "d",
                                            nominal = "etasq",
                                            ordinal = "etasq",
                                            numeric = "r"))) {
  ### This function takes the following parameters:
  ###   dat        = dataframe
  ###   x          = vector of 1+ variable names
  ###   y          = vector of 1+ variable names; if this is left empty, a symmetric matrix
  ###                is created; if this is filled, the matrix will have the x variables
  ###                defining the rows and the y variables defining the columns.
  ###   conf.level = confidence of confidence intervals
  ###   correction = correction for multiple testing: an element out of the vector
  ###                  c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  ###                NOTE: the p-values are corrected for multiple testing;
  ###                The confidence intervals are not!
  ###   digits     = with what precision do you want the results to print
  ###   pval       = determines whether format.pval is used to display the p-value.
  ###                This will add three characters to the width of columns in case
  ###                p-values require scientific notation.
  ###   colspace   = number of spaces between columns
  ###   rowspace   = number of rows between table rows (note: one table row is 2 rows)
  ###
  ### PARAMETERS FOR PRINT METHOD:
  ###
  ### output ("R" or "LaTeX"), env.LaTeX, and pboxWidthMultiplier
  ###
  ### If output is set to "LaTeX", the result is a LaTeX table (e.g. for use
  ### in knitr). In this case, the environment can be set with env.LaTeX.
  ### When using LaTeX, pboxWidthMultiplier can be used to make the cells
  ### narrower or wider (1 works for anything up until 4 or 5 digits).
  ###
  ### colNames can be "numbers" or "names". "Names" cause variables names
  ### to be printed in the heading; "numbers" causes the rows to become
  ### numbered and the numbers to be printed in the heading.
  ###
  ### statistic & effectSize: these are lists of lists that
  ### indicate, for each bivariate combination of variables,
  ### which statistic and effect size to compute. Note that the
  ### corresponding functions should exist in the function lists
  ### (see above the definition of this function).
  
  ### Check whether the first vector of vectors has 
  if (length(x) < 1) {
    stop(paste0("Error: x vector has 0 elements or less; ",
                "make sure to specify at least one variable name!."));
  }  

  ### Check and store the measurement level of the variables:
  ### dichotomous, nominal, ordinal, or interval
  measurementLevelsX <- vector();
  xCounter <- 1;
  for(curXvar in x) {
    if (is.numeric(dat[,curXvar])) {
      measurementLevelsX[xCounter] <- "numeric";
    }
    else if (is.factor(dat[,curXvar])) {
      if (length(levels(dat[,curXvar])) == 2) {
        measurementLevelsX[xCounter] <- "dichotomous";
      }        
      else if (is.ordered(dat[,curXvar])) {
        measurementLevelsX[xCounter] <- "ordinal";
      }
      else {
        measurementLevelsX[xCounter] <- "nominal";      
      }
    }
    else {
      stop(paste0("Error: variable '", curXvar, "'' does not have ",
                  "nominal, ordinal, or interval measurement level!"));
    }
    xCounter <- xCounter + 1;
  }
  
  ### Check whether we have a second set of variables
  if (!is.null(y)) {
    if (length(y) < 1) {
      stop(paste0("Error: y vector has 0 elements or less; ",
                  "make sure to specify at least one variable name!."));
    }
    symmetric <- FALSE;
    ### Check and store the measurement level of the variables:
    ### dichotomous, nominal, ordinal, or interval
    measurementLevelsY <- vector();
    yCounter <- 1;
    for(curYvar in x) {
      if (is.numeric(dat[,curYvar])) {
        measurementLevelsY[yCounter] <- "numeric";
      }
      else if (is.factor(dat[,curYvar])) {
        if (length(levels(dat[,curYvar])) == 2) {
          measurementLevelsY[yCounter] <- "dichotomous";
        }        
        else if (is.ordered(dat[,curYvar])) {
          measurementLevelsY[yCounter] <- "ordinal";
        }
        else {
          measurementLevelsY[yCounter] <- "nominal";      
        }
      }
      else {
        stop(paste0("Error: variable '", curYvar, "'' does not have ",
                    "nominal, ordinal, or interval measurement level!"));
      }
      xCounter <- xCounter + 1;
    }
  }
  else {
    symmetric <- TRUE;
    y <- x;
    measurementLevelsY <- measurementLevelsX;
  }
  
  ### Create object to return, store variable names, confidence of
  ### confidence interval, digits etc, and create matrices for storing
  ### results
  res <- list();
  res$variables.rows <- x;
  res$variables.cols <- y;
  res$variables.rows.levels <- measurementLevelsX
  res$variables.cols.levels <- measurementLevelsY
  res$ci.confidence <- conf.level;
  res$correction <- correction;
  res$digits <- digits;
  res$pval <- pval;
  res$colspace <- colspace;
  res$rowspace <- rowspace;
  res$colNames <- colNames;
  res$output <- output;
  res$env.LaTeX <-env.LaTeX;
  res$pboxWidthMultiplier <- pboxWidthMultiplier;
  res$statistics <- list();
  res$effectSizes <- list();
    
  xCounter <- 1;
  for(curXvar in x) {
    ### For each row, create the object (list) that will
    ### contain the cells
    res$statistics[[curXvar]] <- list();
    res$effectSizes[[curXvar]] <- list();
    yCounter <- 1;
    for(curYvar in y) {
      ### If a symmetric table was requested, don't do
      ### anything unless we're in the lower left half.
      if (!symmetric | (yCounter < xCounter)) {
        
        ### Call the function to compute the statistic.
        ### Which function this is, depends on the preferences
        ### of the user (or the defaults). The functions are
        ### stored in a 'list of lists', where measurement
        ### levels are used as indices. For example, by default,
        ### statistic[['interval']][['dichotomous']] contains 't'.
        ### In that case, the function we'd end up calling is
        ### computeStatistic[['t']], which computes the t-test.
        res$statistics[[curXvar]][[curYvar]] <- 
        computeStatistic[[statistic
                            [[measurementLevelsX[xCounter]]]
                            [[measurementLevelsY[yCounter]]]
                          ]](dat[,curXvar], dat[,curYvar], conf.level = conf.level);
        ### We repeat the same trick for the effect sizes.
        res$effectSizes[[curXvar]][[curYvar]] <- 
          computeEffectSize[[effectSize
                             [[measurementLevelsX[xCounter]]]
                             [[measurementLevelsY[yCounter]]]
                           ]](dat[,curXvar], dat[,curYvar], conf.level = conf.level);
      }
      yCounter <- yCounter + 1;
    }
    xCounter <- xCounter + 1;
  }
  
  ### Correct p-values for multiple testing
  ### First build a matrix with the raw p-values
  res$pvalMatrix <- matrix(nrow=length(x), ncol=length(y), dimnames=list(x, y));
  for(curXvar in x) {
    for(curYvar in y) {
      if (!is.null(res$statistics[[curXvar]][[curYvar]]$p.raw)) {
        res$pvalMatrix[curXvar, curYvar] <- res$statistics[[curXvar]][[curYvar]]$p.raw;
      }
    }
  }
  ### Adjust p-values
  res$pvalMatrix.adj <- matrix(p.adjust(res$pvalMatrix, method=correction),
                               nrow(res$pvalMatrix), ncol(res$pvalMatrix),
                               dimnames=dimnames(res$pvalMatrix));
  ### Store adjusted p-values in objects
  for(curXvar in x) {
    for(curYvar in y) {
      if (!is.null(res$statistics[[curXvar]][[curYvar]]$p.raw)) {
        res$statistics[[curXvar]][[curYvar]]$p.adj <- res$pvalMatrix.adj[curXvar, curYvar];
      }
    }
  }
  
  ### Set class & return result
  class(res) <- c("associationMatrix");
  return(res);
}

repeatStr <- function (str = " ", n = 1) {
  if (n < 1) {
    return("");
  }
  else if (n == 1) {
    return(str);
  }
  else {
    res <- str;
    for(i in c(1:(n-1))) {
      res <- paste0(res, str);
    }
    return(res);
  }
}

noZero <- function (str) {
  return(gsub("0\\.", ".", str));  
}

formatR <- function (r, digits) {
  return(noZero(round(r, digits)));
}

### Function to escape special latex characters, based on
### http://stackoverflow.com/questions/5406071/r-sweave-latex-escape-variables-to-be-printed-in-latex
sanitizeLatexString <- function(str) {
  str <- gsub('([#$%&~_\\^\\\\{}])', '\\\\\\1', str, perl = TRUE);
}

print.associationMatrix <- function (x, digits=x$digits, output=x$output,
                                     env.LaTeX = x$env.LaTeX,
                                     pboxWidthMultiplier = x$pboxWidthMultiplier,
                                     colNames = x$colNames, pval=x$pval, ...) {

  if (output=="R") {
        
    ### We want multiple lines per cell, so we'll need to print manually.
    ### We first print the confidence interval on the first line; then,
    ### on the next line, the point estimate; and finally, on the last line,
    ### the p-value (corrected for multiple testing).
      
    ### Compute how wide the columns should be. This depends on
    ### 1) the width of the confidence intervals, and 2) the width
    ### of the variable name in each column
    ### The maximum length of confidence intervals is:
    ### [-1.X; -1.X]
    ### Where the number of X's is determined by digits.
    ### Thus, 8 + digits * 2 represents the max length of
    ### confidence interval.

    maxConfIntLength <- 10 + digits * 2;

    if (colNames=="numbers") {
      ### The columns contain numbers instead of names;
      ### calculate the max width of these numbers
      numColSize <- nchar(length(x$variables.rows)) + x$colspace;
      colSizes <- rep(numColSize, length(x$variables.cols));
    }
    else {
      ### Otherwise, for each column, store the length of the variable name
      ### of that column.
      colSizes <- nchar(x$variables.cols);
    }
  
    ### Then, compare these to the maxConfIntLength, and store the
    ### larger of the two
    colSizes <- ifelse(colSizes > maxConfIntLength, colSizes, maxConfIntLength);
  
    ### If pval is TRUE, we use the p-value function to format the p-values.
    ### This means that the columns need to be three characters wider, in case
    ### we'll need the scientific notation somewhere.
    if(x$pval) {
      colSizes <- colSizes + 3;
    }

    if (colNames=="numbers") {
      ### Print spaces in first cell of first row as wide as
      ### the widest number
      cat(repeatStr(' ', numColSize + x$colspace));
    }
    
    ### First print column names. This, however, requires knowing
    ### how long the row names are going to be, so first look for
    ### the longest row name and get its length; add one as
    ### separation between the columns; and then print that
    ### number of spaces.
    leftColSize <- max(nchar(x$variables.rows)) + x$colspace;
    cat(repeatStr(" ", leftColSize));
  
    ### We'll need to print the column names with a loop (see
    ### explanation below)
    for(j in (1:length(x$variables.cols))) {
      if (colNames=="numbers") {
        ### Print column number
        cat(paste0(j, repeatStr(' ', colSizes[j] - nchar(j) + x$colspace)));
      } else {
        ### Print the column name
        cat(x$variables.cols[j]);
        ### Print trailing spaces (+x$colspace to have space between columns)
        cat(repeatStr(" ", colSizes[j] - nchar(x$variables.cols[j]) + x$colspace));
      }
    }
    
    ### Print newline character
    cat("\n");
  }
  else if (output=="LaTeX") {
    
    ### Start table
    if (colNames=="numbers") {
      cat(paste0("\\begin{", env.LaTeX, "}{rl",
                 paste0(rep('c', length(x$variables.cols)), collapse=""),
                 "}\n\\hline\n"));
    }
    else {
      cat(paste0("\\begin{", env.LaTeX, "}{l",
                 paste0(rep('c', length(x$variables.cols)), collapse=""),
                 "}\n\\hline\n"));
    }
    
    if (colNames=="numbers") {
      ### Replace variable names for the columns with numbers
      colVarNames <- x$variables.cols;
      x$variables.cols <- c(1:length(x$variables.cols));
      ### Add first empty cell for the column with the row numbers
      cat(' &');
    }
    
    ### Print variable names, close line (needs four backslashes; each of the two
    ### backslashes needs to be escaped), and print a horizontal line
    cat(paste0(" & ", paste0(sanitizeLatexString(x$variables.cols), collapse=" & "), " \\\\ \n\\hline\n"));
    
    ### Compute width for pBoxes in cells (see below)
    pboxWidth <- paste0(7 + pboxWidthMultiplier * digits, "em");
    
  }
    
  ### Now we'll start printing the rows, starting with the variable
  ### name and the confidence interval.
  rowCounter <- 1;
  for(rowVar in x$variables.rows) {
    
    if (output=="R") {
      
      if (colNames=="numbers") {
        ### Print row number
        cat(paste0(repeatStr(' ', numColSize - nchar(rowCounter)), rowCounter, repeatStr(' ', x$colspace)));
      }
      
      ### Print variable name for this row
      cat(rowVar);
      
      ### Print spaces needed to line up second column
      cat(repeatStr(" ", leftColSize - nchar(rowVar)));
      
      ### Now we need two loops (one for each line) to create the cells.
      ### Normally, we could provide paste0 (or paste) with a vector,
      ### and it would concatenate the elements for us, but in this case,
      ### every column can have a different width, so we need a different
      ### number of leading spaces.

      ### First, the confidence intervals
      colCounter <- 1;
      for(colVar in x$variables.cols) {
        ### If the point estimate is NULL, don't display anything
        if (is.null(x$effectSizes[[rowVar]][[colVar]]$es)) {
          cat(repeatStr(" ", colSizes[colCounter] + x$colspace));
        }
        else {
          ### Create confidence interval for this column
          confInt <- paste0("[",
                            round(x$effectSizes[[rowVar]][[colVar]]$ci[1], digits),
                            "; ",
                            round(x$effectSizes[[rowVar]][[colVar]]$ci[2], digits),
                            "]");
          ### Print confidence interval
          cat(confInt);
          ### Print trailing spaces (+ x$colspace to have space between columns)
          cat(repeatStr(" ", colSizes[colCounter] - nchar(confInt) + x$colspace));
        }
        colCounter <- colCounter + 1;
      }
      
      ### Print newline character
      cat("\n");
      
      if (colNames=="numbers") {
        ### Print spaces of width of longest row number
        cat(paste0(repeatStr(' ', numColSize + x$colspace)));
      }
      
      ### Start in second column
      cat(repeatStr(" ", leftColSize));
      
      ### Then, the point estimate and p-value
      colCounter <- 1;
      for(colVar in x$variables.cols) {
        ### If the point estimate is NULL, don't display anything
        if (is.null(x$effectSizes[[rowVar]][[colVar]]$es)) {
          cat(repeatStr(" ", colSizes[colCounter] + x$colspace));
        }
        else {
          ### Create point estimate of effect size & p
          if(x$pval) {
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", p=",
                              round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
          }
          else {
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", p=",
                              round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
          }
          ### Print point estimate and p-value
          cat(content);
          ### Print trailing spaces (+x$colspace to have space between columns)
          cat(repeatStr(" ", colSizes[colCounter] - nchar(content) + x$colspace));
        }
        colCounter <- colCounter + 1;
      }
      
      ### Print newline character
      cat("\n");
      
      ### x$rowspace indicated how many empty rows should be printed between
      ### every table row
      if (x$rowspace > 1) {
        for(i in c(1:x$rowspace)) {
          cat("\n");      
        }
      }
      
    }
    else if (output=="LaTeX") {
      
      if (colNames=="numbers") {
        ### Print row number
        cat(paste0(rowCounter, ' & '));
      }
      
      ### Print variable name for this row
      cat(sanitizeLatexString(rowVar));
      
      ### Loop through columns
      colCounter <- 1;
      for(colVar in colVarNames) {
        ### Only print if the point estimate is not NA
        if (is.null(x$effectSizes[[rowVar]][[colVar]]$es)) {
          cat(" & ");
        }
        else {
          ### We need to put the contents in a parbox,
          ### because we need two lines. The width of this
          ### parbox depends on the number of digits to
          ### display, which we calculated earlier.
          cat(paste0(" & \\parbox[t]{", pboxWidth, "}{ \\centering "));
          ### Create confidence interval for this column
          ### Create confidence interval for this column
          confInt <- paste0("[",
                            round(x$effectSizes[[rowVar]][[colVar]]$ci[1], digits),
                            "; ",
                            round(x$effectSizes[[rowVar]][[colVar]]$ci[2], digits),
                            "]");
          ### Print confidence interval and newline character
          cat(paste0(confInt, " \\\\ "));

          if(x$pval) {
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", p=",
                              round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
          }
          else {
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", p=",
                              round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
          }
          
          
          ### Print point estimate
          if(pval) {
            pValue <- noZero(format.pval(x$statistics[[rowVar]][[colVar]]$p.adj,
                                         digits=digits));
            if (substring(pValue, 1, 1) == "<") {
              pValue <- paste0("p", "\\textless", substring(pValue, 2, nchar(pValue)));
            }
            else {
              pValue <- paste0("p=", pValue);
            }
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", ",
                              pValue);
          }
          else {
            content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
                              "=",
                              round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
                              ", p=",
                              formatR(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
          }
          ### Print point estimate and p-value and close cell
          cat(content);
          ### Close cell
          cat("\\\\ } ");
        }
      }
      
      ### Print '\\' and go to next line - note that we need to
      ### escape each backslash with a backslash.
      cat(" \\\\\n");
      
    }
    
    rowCounter <- rowCounter + 1;
  }
  
  if (output=="LaTeX") {
    cat(paste0("\n\\hline\n\\end{", env.LaTeX, "}\n"));
  }
  
  invisible();
}

      
      
#       
#       
#       
#       
#       
#       ### First, the confidence intervals
#       colCounter <- 1;
#       for(colVar in x$variables.cols) {
#         ### If the point estimate is NULL, don't display anything
#         if (is.null(x$effectSizes[[rowVar]][[colVar]]$es)) {
#           cat(repeatStr(" ", colSizes[colCounter] + x$colspace));
#         }
#         else {
#           ### Create confidence interval for this column
#           confInt <- paste0("[",
#                             round(x$effectSizes[[rowVar]][[colVar]]$ci[1], digits),
#                             "; ",
#                             round(x$effectSizes[[rowVar]][[colVar]]$ci[2], digits),
#                             "]");
#           ### Print confidence interval
#           cat(confInt);
#           ### Print trailing spaces (+ x$colspace to have space between columns)
#           cat(repeatStr(" ", colSizes[colCounter] - nchar(confInt) + x$colspace));
#         }
#         colCounter <- colCounter + 1;
#       }
#       
#       ### Print newline character
#       cat("\n");
#       ### Start in second column
#       cat(repeatStr(" ", leftColSize));
#       
#       ### Then, the point estimate and p-value
#       colCounter <- 1;
#       for(colVar in x$variables.cols) {
#         ### If the point estimate is NULL, don't display anything
#         if (is.null(x$effectSizes[[rowVar]][[colVar]]$es)) {
#           cat(repeatStr(" ", colSizes[colCounter] + x$colspace));
#         }
#         else {
#           ### Create point estimate of effect size & p
#           if(x$pval) {
#             content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
#                               "=",
#                               round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
#                               ", p=",
#                               round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
#           }
#           else {
#             content <- paste0(substr(x$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1),
#                               "=",
#                               round(x$effectSizes[[rowVar]][[colVar]]$es, digits),
#                               ", p=",
#                               round(x$statistics[[rowVar]][[colVar]]$p.adj, digits));
#           }
#           ### Print point estimate and p-value
#           cat(content);
#           ### Print trailing spaces (+x$colspace to have space between columns)
#           cat(repeatStr(" ", colSizes[colCounter] - nchar(content) + x$colspace));
#         }
#         colCounter <- colCounter + 1;
#       }
#       
#       ### Print newline character
#       cat("\n");
#       
#       ### x$rowspace indicated how many empty rows should be printed between
#       ### every table row
#       if (x$rowspace > 1) {
#         for(i in c(1:x$rowspace)) {
#           cat("\n");      
#         }
#       }
#       
#     }
#     
#     invisible();
#   }
#   
#   