###########################################################
###
### R file with the function meanDiff, which is partly
### a wrapper for the basic t-test function, but which
### also computes Cohen's d and g and the respective
### confidence interval.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################

###########################################################
### Define functions
###########################################################

meanDiff <- function(x, y=NULL, paired = FALSE, var.equal = "test", conf.level = .95, digits = 2) {
  ### This function takes the following parameters:
  ###   x          = numeric vector: variable 1; can also be a formula of the form y ~ x,
  ###                where x must be a factor with two levels (i.e. dichotomous)
  ###   y          = numeric vector: variable 2; can be empty if x is a formula
  ###   paired     = boolean; are x & y independent or dependent? Note that if
  ###                x & y are dependent, they need to have the same length
  ###   var.equal  = string; only relevant if x & y are independent; can be:
  ###                  "test" (default): test whether x & y have different
  ###                                    variances
  ###                  "no":             assume x & y have different variances
  ###                  "yes":            assume x & y have the same variance
  ###   conf.level = confidence of confidence intervals
  ###   digits     = with what precision do you want the ress to print

  ### This function uses the formulae from Borenstein,
  ### Hedges, Higgins & Rothstein (2009) (pages 25-32)
  
  ### Check whether we need to extract the variables from a formula
  if (is.null(y)) {
    if (length(x) != 3) {
      stop(paste0("Error: if no y vector is specified,\nthe x parameter must be a formula of the form y ~ x,\nwhere x is dichotomous. The formula contains more than three elements (", length(x)," elements)."));
    }
    else if (as.character(x[1]) != '~') {
      stop(paste0("Error: if no y vector is specified,\nthe x parameter must be a formula of the form y ~ x,\nwhere x is dichotomous. The formula lacks a tilde ('~'; the elements are '",x[1],"', '",x[2],"' and '",x[3],"')."));
    }
    if (paired) {
      stop("Error: when doing a paired t-test, you have to provide both variables as vectors (of the same length), instead of using the formula specification!");
    }
    ### Split variable names into dataset and column name
    depVarData <- unlist(strsplit(as.character(x[2]), "\\$"))[1];
    depVar <- unlist(strsplit(as.character(x[2]), "\\$"))[2];
    groupingVarData <- unlist(strsplit(as.character(x[3]), "\\$"))[1];
    groupingVar <- unlist(strsplit(as.character(x[3]), "\\$"))[2];
    ### Create temporary dataframe to select relevant datapoints
    temp <- data.frame(dv = get(depVarData)[[depVar]], group = get(groupingVarData)[[groupingVar]]);
    temp$group <- as.factor(temp$group);
    ### Check number of levels of grouping variable
    if (length(levels(temp[['group']])) != 2) {
      stop(paste0("Error: if no y vector is specified, the x parameter must be a formula of the form y ~ x, where x is dichotomous. However, x has more than two levels: x (", groupingVar,") has ", length(levels(temp[['group']])), " levels."));
    }
    varNames <- c(paste(x[2],"(dependent variable)"), paste(x[3],"(grouping variable)"));
    ### Get values for one level and for the other level and store in x and y
    x <- temp[['dv']][temp[['group']] == levels(temp[['group']])[1]];
    y <- temp[['dv']][temp[['group']] == levels(temp[['group']])[2]];
  }
  else {
    varNames <- c(deparse(substitute(x)), deparse(substitute(y)));
  }

  ### Create object to return, and store variable names, confidence of
  ### confidence interval, and digits
  res <- list();
  res$variables <- varNames;
  res$ci.confidence <- conf.level;
  res$digits <- digits;
  
  if (paired) {
    ### Matched pairs t-test
    res$type <- "Matched pairs t-test";
    
    if (length(x) != length(y)) {
      stop(paste0("Error: for a paired sample t-test, both variables must have the same number of cases!\n",
                  res$variables[1], " has ", length(x), " cases, ", res$variables[2], " has ", length(y), " cases."));
    }
  
    ### Remove missing values
    completeCases <- complete.cases(x, y);
    x <- x[completeCases];
    y <- y[completeCases];
    
    ### Store sample size, means and standard deviations
    res$n    <- c(length(x));
    res$mean <- c(mean(x), mean(y));
    res$sd   <- c(sqrt(var(x)), sqrt(var(y)));

    ### Variance of difference
    res$variance <- var(x - y);
    
    ### Paired samples t-test
    res$objects$t_test <- t.test(x, y, paired = TRUE);
    
    ### Correlation between both variables; we'll need it
    ### for Cohen's d (see Borenstein et al., p. 29)
    res$correlation <- cor(x, y);
    
    ### Standard deviation of the difference (again, see p. 29)
    res$diff.sd <- sqrt(var(x - y));
        
    ### Standard deviation within groups (see p. 29)
    res$sd.withingroups <- res$diff.sd / (sqrt(2*(1-res$correlation)));
    
    ### Compute raw difference between means
    res$meanDiff <- res$mean[1] - res$mean[2];
    ### Compute Cohen's d (formula from p. 29 of Borenstein et al.)
    res$meanDiff.d <- res$meanDiff/res$diff.sd;
    ### Compute variance of Cohen's d (formula from p. 29 of Borenstein et al.)
    res$meanDiff.d.var <-
      ((1/res$n) + (res$meanDiff.d ^2 / (2*res$n))) * 2 * (1 - res$correlation);
    ### Compute standard error of Cohen's d (formula from p. 29 of Borenstein et al.)
    res$meanDiff.d.se <- sqrt(res$meanDiff.d.var);
    ### Compute J (to compute g, the unbiased d; formula from p. 27 & 29 of Borenstein et al.)
    res$meanDiff.J <- 1 - (3 / (4 * (res$n - 1) - 1));
    
  }
  else {
    ### Independent samples t-test
    res$type <- "Independent samples t-test (";

    ### Remove missing values
    x <- na.omit(x);
    y <- na.omit(y);

    ### Store sample size, means and standard deviations
    res$n    <- c(length(x), length(y));
    res$mean <- c(mean(x), mean(y));
    res$sd   <- c(sqrt(var(x)), sqrt(var(y)));
    
    ### Test for equal variances if necessary
    if (!paired & (var.equal == "test")) {
      res$objects$equal.var_test <- var.test(x, y);
      res$type = paste0(res$type, "tested for equal variances, p = ", format.pval(res$objects$equal.var_test$p.value, 3), ", so ");
      if (res$objects$equal.var_test$p.value < .05) {
        var.equal <- "no";
      }
      else {
        var.equal <- "yes";
      }
    }
    
    ### Which variance we use and which t-test we use
    ### depends on whether we have equal variances
    if (var.equal == "no") {
      res$type = paste0(res$type, "unequal variances)");
      ### Welch's t-test
      res$objects$t_test <- t.test(x, y, var.equal = FALSE);
      ### Use variance from largest sample
      if (res$n[1]> res$n[2]) {
        res$variance <- var(x);
      }
      else {
        res$variance <- var(y);
      }
    }
    else if (var.equal == "yes") {
      res$type = paste0(res$type, "equal variances)");
      ### Student t-test
      res$objects$t_test <- t.test(x, y, var.equal = TRUE);
      ### Compute pooled variance
      x.ss <- var(x) * (res$n[1] - 1);
      y.ss <- var(y) * (res$n[2] - 1);
      res$variance <- (x.ss + y.ss) / (res$n[1] + res$n[2] - 2);
    }

    ### Compute raw difference between means
    res$meanDiff <- res$mean[1] - res$mean[2];
    ### Compute Cohen's d
    res$meanDiff.d <- res$meanDiff/sqrt(res$variance);
    ### Compute variance of Cohen's d (formula from p. 27 of Borenstein et al.)
    res$meanDiff.d.var <-
      ((res$n[1] + res$n[2]) / (res$n[1] * res$n[2])) +
      ((res$meanDiff.d^2) / (2 * (res$n[1] + res$n[2])));
    ### Compute standard error of Cohen's d (formula from p. 27 of Borenstein et al.)
    res$meanDiff.d.se <- sqrt(res$meanDiff.d.var);
    ### Compute J (to compute g, the unbiased d; formula from p. 27 of Borenstein et al.)
    res$meanDiff.J <- 1 - (3 / (4 * (res$n[1] + res$n[2] - 2) - 1));
    
  }

  ### Compute g and variance & standard error of g (formulae from p. 27 of Borenstein et al.)
  res$meanDiff.g     <- res$meanDiff.J * res$meanDiff.d;
  res$meanDiff.g.var <- res$meanDiff.J^2 * res$meanDiff.d.var;
  res$meanDiff.g.se  <- sqrt(res$meanDiff.g.var);

  ### Find z for calculating confidence intervals
  res$ci.usedZ <- abs(qnorm((1-conf.level)/2));
  
  ### Compute confidence interval for d
  res$meanDiff.d.ci.lower <- res$meanDiff.d - res$ci.usedZ * res$meanDiff.d.se;
  res$meanDiff.d.ci.upper <- res$meanDiff.d + res$ci.usedZ * res$meanDiff.d.se;

  ### Compute confidence interval for g
  res$meanDiff.g.ci.lower <- res$meanDiff.g - res$ci.usedZ * res$meanDiff.g.se;
  res$meanDiff.g.ci.upper <- res$meanDiff.g + res$ci.usedZ * res$meanDiff.g.se;
  
  ### Store confidence interval for meanDiff
  res$meanDiff.ci.lower  <- res$objects$t_test$conf.int[1];
  res$meanDiff.ci.upper  <- res$objects$t_test$conf.int[2];
  
  ### Store secondary (NHST) information
  res$t  <- res$objects$t_test$statistic;
  res$df <- res$objects$t_test$parameter;
  res$p  <- res$objects$t_test$p.value;
  
  ### Set class & return result
  class(res) <- c("diff.means");
  return(res);
}

print.diff.means <- function (x, digits=x$digits, ...) {
  if (regexpr("Matched pairs", x$type) > -1) {
    variableInfo <- paste0("\n  ", x$variables[1], " (mean = ", round(x$mean[1], digits), ", sd = ", round(x$sd[1], digits), ", n = ", x$n, ")",
                           "\n  ", x$variables[2], " (mean = ", round(x$mean[2], digits), ", sd = ", round(x$sd[2], digits), ", n = ", x$n, ")");
    varianceInfo <- paste0(x$type, "\n  (standard deviation of the difference: ", round(sqrt(x$variance), digits), ")");
  }
  else if (regexpr("Independent samples", x$type)) {
    variableInfo <- paste0("\n  ", x$variables[1], " (mean = ", round(x$mean[1], digits), ", sd = ", round(x$sd[1], digits), ", n = ", x$n[1], ")",
                           "\n  ", x$variables[2], " (mean = ", round(x$mean[2], digits), ", sd = ", round(x$sd[2], digits), ", n = ", x$n[2], ")");
    if (regexpr("equal variances", x$type)) {
      varianceInfo <- paste0(x$type, "\n  (pooled standard deviation used, ", round(sqrt(x$variance), digits), ")");
    }
    else if (regexpr("unequal variances", x$type)) {
      varianceInfo <- paste0(x$type, "\n  (standard deviation used of largest sample, ", round(sqrt(x$variance), digits), ")");
    }
  }
  cat(paste0("Variables:", 
           variableInfo,
           "\n\n", varianceInfo,
           "\n\n", round(x$ci.confidence * 100, digits), "% confidence intervals:",
           "\n  Absolute mean difference: [", round(x$meanDiff.ci.lower, digits), ", ", round(x$meanDiff.ci.upper, digits), "]",
             " (Absolute mean difference: ", round(x$meanDiff, digits), ")",
           "\n  Cohen's d for difference: [", round(x$meanDiff.d.ci.lower, digits), ", ", round(x$meanDiff.d.ci.upper, digits), "]",
             " (Cohen's d point estimate: ", round(x$meanDiff.d, digits), ")",
           "\n  Hedges g for difference:  [", round(x$meanDiff.g.ci.lower, digits), ", ", round(x$meanDiff.g.ci.upper, digits), "]",
             " (Hedges g point estimate:  ", round(x$meanDiff.g, digits), ")",
           "\n\n(secondary information (NHST): t = ", round(x$t, digits), "[", round(x$df, digits),"], p = ", format.pval(x$p, digits), ")\n"));
  invisible();
}
