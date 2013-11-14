nonCentral.probability <- function(distribution, statistic, df1, df2 = NULL,
                                   ncp, showPlot = FALSE) {
  
  ### This function simply returns the probability of obtaining
  ### a value as high as, or higher than, the relevant
  ### statistic (type specified with 'distribution', one of
  ### 'f', 'x', or 't', corresponding to the F-, Chi squared,
  ### and t-distributions, respectively, with the relevant degrees
  ### of freedom and noncentrality parameter).
  
  if (distribution == 'f') {
    return(1 - pf(statistic, df1 = df1, df2 = df2, ncp = ncp));
  }
  if (distribution == 'x') {
    return(1 - pchisq(statistic, df = df1, ncp = ncp));
  }
  if (distribution == 't') {
    return(1 - pt(statistic, df = df1, ncp = ncp));
  }
}

criticalNonCentral <- function(distribution, statistic, df1, df2 = NULL,
                               prob, step = 1E-07) {

  ### This function takes a statistic from a given distribution
  ### (F, t of chi square) and returns the values of the
  ### noncentrality parameter that specifies the distribution
  ### of the relevant statistic where the probability, of
  ### attaining the value of the statistic or a larger value,
  ### is equal to the probability specified in prob.
  
  nonCentral.values <- vector();
  nonCentral.probability.gt.f <- vector();
  
  ### Set initial values
  nonCentral.current <- 0;
  nonCentral.values[1] <- nonCentral.current;
  nonCentral.probability.gt.f[1] <-
    nonCentral.probability(distribution, statistic, df1 = df1, df2 = df2,
                           ncp = nonCentral.values[length(nonCentral.values)]);
  
  currentStep <- 1;
  
  while (currentStep > step) {  
    currentStep <- currentStep / 10;
    currentStep <- max(currentStep, step);
    cat(tail(nonCentral.probability.gt.f, 1), "\n");
    cat(prob, "\n");
    while (tail(nonCentral.probability.gt.f, 1) < prob) {
      ### Store value of the noncentral distribution
      nonCentral.values[length(nonCentral.values) + 1] <-
        nonCentral.current;
      ### Store probability
      nonCentral.probability.gt.f[length(nonCentral.probability.gt.f) + 1] <-
        nonCentral.probability(distribution, statistic, df1 = df1, df2 = df2,
                               ncp = nonCentral.values[length(nonCentral.values)]);
      ### Increase value of noncentral distribution
      nonCentral.current <- nonCentral.current + currentStep;
    }
    
    nonCentral.values <-
      head(nonCentral.values, length(nonCentral.values) - 1);
    nonCentral.probability.gt.f <-
      head(nonCentral.probability.gt.f, length(nonCentral.probability.gt.f) - 1);
    nonCentral.current <- tail(nonCentral.values, 1);
  }
     
  res <- list();
  res$dat <- data.frame(ncv = nonCentral.values,
                        prob = nonCentral.probability.gt.f);
  
  return(res);
  
}

noncentral.ci <- function(distribution, statistic, df1, df2 = NULL,
                          conf.level = .95, ci.bounds = NULL,
                          step = 1E-07) {
  
  ### This function takes a statistic from a given distribution
  ### (F, t of chi square) and returns the two values of the
  ### noncentrality parameter that specify distributions of the
  ### relevant statistic where the probability, of attaining the
  ### value of the statistic or a larger value, is equal to the
  ### probability specified in ci.bounds.
  
  if (is.null(ci.bounds) & !is.null(conf.level)) {
    ci.bounds <- (1 - conf.level) / 2;
    ci.bounds <- c(ci.bounds, 1 - ci.bounds);
  }
  
  res <- list();
  
  res$ncv.ci.lo <- criticalNonCentral(distribution = distribution,
                                  statistic = statistic,
                                  df1 = df1, df2 = df2,
                                  prob = ci.bounds[1],
                                  step = step);
  res$ncv.ci.hi <- criticalNonCentral(distribution = distribution,
                                  statistic = statistic,
                                  df1 = df1, df2 = df2,
                                  prob = ci.bounds[2],
                                  step = step);
  
  res$ncv.ci <- c(tail(res$ncv.ci.lo$dat$ncv, 1),
                  tail(res$ncv.ci.hi$dat$ncv, 1));

  res$prob.ci <- c(tail(res$ncv.ci.lo$dat$prob, 1),
                  tail(res$ncv.ci.hi$dat$prob, 1));
  
  return(res);
  
}

es.ci <- function(distribution, statistic, df1, df2 = NULL,
                  conf.level=.95, ci.bounds=NULL, step = 1E-07) {
  
  ### This function takes a statistic from a given distribution
  ### (F, t of chi square) and returns the confidence interval
  ### for the corresponding effect size.
  
  res <- list();
  
  ### We take the absolute value of the statistic (the
  ### found value of F, chi square, or t), because
  ### F and chi square are always positive anyway, and
  ### the t-distribution is always symmetric. This way,
  ### the criticalNonCentral function can deal with all
  ### distribution types the same way.
  res$ncv <- noncentral.ci(distribution = distribution,
                           statistic = abs(statistic),
                           df1 = df1, df2 = df2,
                           conf.level = conf.level,
                           ci.bounds = ci.bounds,
                           step = step);
  
  ### Now, convert the noncentrality values to the
  ### relevant effect sizes.
  if (distribution == 'f') {
    res$es.type <- "etasq";
    res$es.ci <- c(res$ncv$ncv.ci[1] / (res$ncv$ncv.ci[1] + df1 + df2 + 1),
                   res$ncv$ncv.ci[2] / (res$ncv$ncv.ci[2] + df1 + df2 + 1));
  }    
  if (distribution == "x") {
    res$es.type <- "v";
    res$es.ci <- c((res$ncv$ncv.ci[1] + df) / (n * min(rows - 1, cols - 1)),
                   (res$ncv$ncv.ci[2] + df) / (n * min(rows - 1, cols - 1)));
  }
  
  return(res);
  
}

plot(dchisq(seq(0, 10, by=.1), df=2, ncp=0), type="l");
lines(dchisq(seq(0, 10, by=.1), df=2, ncp=.5));
lines(dchisq(seq(0, 10, by=.1), df=2, ncp=1));
lines(dchisq(seq(0, 10, by=.1), df=2, ncp=5));
lines(dchisq(seq(0, 10, by=.1), df=2, ncp=10));
lines(dchisq(seq(0, 10, by=.1), df=2, ncp=20));
