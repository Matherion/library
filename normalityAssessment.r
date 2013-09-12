### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### e1071 contains the kurtosis and skewness functions
safeRequire('e1071');
### For layout of two plots next to each other
safeRequire('grid');
### For a suite of normality testing functions
safeRequire('fBasics');

### The regular ifelse cannot return objects
ifelseObj <- function(condition, ifTrue, ifFalse) {
  if (condition) {
    return(ifTrue);
  }
  else {
    return(ifFalse);
  }
}

### Function definition
normalityAssessment <- function(sampleVector, samples = 5000, digits=3,
                         samplingDistColor = "#2222CC",
                         normalColor = "#00CC00",
                         samplingDistLineSize = 2,
                         normalLineSize = 1) {
  
  
  
  ### Create object for returning results
  res <- list(sampleVector.raw = sampleVector,
              sampleVector = sampleVector[complete.cases(sampleVector)],
              sampleSize = length(sampleVector[complete.cases(sampleVector)]),
              samples = samples,
              digits = digits);
  
  ### Temporarily store scientific notation threshold
  tempSciPen <- getOption("scipen");
  ### Set at 10 + digits
  options(scipen=10+digits);
  
  ### Construct temporary dataset for
  ### plotting sample distribution
  normalX <- c(seq(min(res$sampleVector), max(res$sampleVector),
                   by=(max(res$sampleVector) - min(res$sampleVector))/(res$sampleSize-1)));
  normalY <- dnorm(normalX, mean=mean(res$sampleVector),
                   sd=sd(res$sampleVector));
  sampleDistY <- res$sampleVector;
  tempDat <- data.frame(normalX = normalX, normalY = normalY, sampleDist = sampleDistY);
  tempBinWidth <- (max(res$sampleVector) - min(res$sampleVector)) / 30;
  
  ### Plot sample distribution
  res$plot.sampleDist <- ggplot(data=tempDat,
         aes(x=sampleDist)) +
    xlab(paste0('Value of ', deparse(substitute(sampleVector)))) +
    ylab(paste0('Density for n=', res$sampleSize)) +
    geom_histogram(aes(y=..density..), color=NA, fill=samplingDistColor, alpha=.25, binwidth=tempBinWidth) +
    geom_density(color=samplingDistColor, size=samplingDistLineSize, alpha=.5) +
    geom_line(aes(x=normalX, y=normalY), color=normalColor, size=normalLineSize);

  ### Take 'samples' samples of sampleSize people and store the means
  ### (first generate an empty vector to store the means)
  res$samplingDistribution <- c();
  for (i in 1:samples) {
    res$samplingDistribution[i] <- mean(sample(res$sampleVector, size=res$sampleSize,
                                               replace=TRUE));
  }
  
  ### Construct temporary dataset for
  ### plotting sampling distribution  
  normalX <- c(seq(min(res$samplingDistribution), max(res$samplingDistribution),
                   by=(max(res$samplingDistribution) - min(res$samplingDistribution))/(res$samples-1)));
  normalY <- dnorm(normalX, mean=mean(res$samplingDistribution),
                   sd=sd(res$samplingDistribution));
  samplingDistY <- res$samplingDistribution;
  tempDat <- data.frame(normalX = normalX, normalY = normalY, samplingDist = samplingDistY);
  tempBinWidth <- (max(res$samplingDistribution) - min(res$samplingDistribution)) / 30;
  
  ### Plot sampling distribution
  res$plot.samplingDist <- ggplot(data=tempDat,
                                  aes(x=samplingDist)) +
    xlab(paste0('Value of ', deparse(substitute(sampleVector)))) +
    ylab(paste0('Density for ', res$samples, ' samples of n=', res$sampleSize)) +
    geom_histogram(aes(y=..density..), color=NA, fill=samplingDistColor, alpha=.25, binwidth=tempBinWidth) +
    geom_density(color=samplingDistColor, size=samplingDistLineSize, alpha=.5) +
    geom_line(aes(x=normalX, y=normalY), color=normalColor, size=normalLineSize);
  
  ### Shapiro Wilk test - if there are more than 5000
  ### datapoints, only use the first 5000 datapoints
  res$sw.sampleDist <- ifelseObj(res$sampleSize > 5000,
                              shapiro.test(res$sampleVector[1:5000]),
                              shapiro.test(res$sampleVector));
  res$sw.samplingDist <- ifelseObj(res$samples > 5000,
                                shapiro.test(res$samplingDistribution[1:5000]),
                                shapiro.test(res$samplingDistribution));
  
  ### Anderson-Darling test
  res$ad.sampleDist <- adTest(res$sampleVector);
  res$ad.samplingDist <- adTest(res$samplingDistribution);
  
  ### Kolomogorov-Smirnof test
  suppressWarnings(res$ks.sampleDist <-
                     ks.test(res$sampleVector, "pnorm", alternative = "two.sided"));
  suppressWarnings(res$ks.samplingDist <-
                     ks.test(res$samplingDistribution, "pnorm", alternative = "two.sided"));

  ### Store simple kurtosis and skewness
  res$skewness.sampleDist <- skewness(res$sampleVector);
  res$kurtosis.sampleDist <- kurtosis(res$sampleVector);
  res$skewness.samplingDist <- skewness(res$samplingDistribution);
  res$kurtosis.samplingDist <- kurtosis(res$samplingDistribution);
  
  ### Restore scientific notation settings
  options(scipen=tempSciPen);

  ### Set class for returnable object and return it
  class(res) <- 'normalityAssessment';
  return(res);
  
}

print.normalityAssessment <- function (x) {

  if (x$sampleSize > 5000) {
    sw.sampleDist <- paste0("Shapiro-Wilk: p=", round(x$sw.sampleDist$p.value, x$digits),
                                   " (W=", round(x$sw.sampleDist$statistic, x$digits),
                                   "; NOTE: based on the first 5000 of ",
                                 x$sampleSize, " observations)");
  }
  else {
    sw.sampleDist <- paste0("Shapiro-Wilk: p=", round(x$sw.sampleDist$p.value, x$digits),
                                   " (W=", round(x$sw.sampleDist$statistic, x$digits),
                                   "; based on ", x$sampleSize, " observations)");
  }
  
  if (x$samples > 5000) {
    sw.samplingDist <- paste0("Shapiro-Wilk: p=", round(x$sw.samplingDist$p.value, x$digits),
                       " (W=", round(x$sw.samplingDist$statistic, x$digits),
                       "; NOTE: based on the first 5000 of ",
                       x$samples, " observations)");
  }
  else {
    sw.samplingDist <- paste0("Shapiro-Wilk: p=", round(x$sw.samplingDist$p.value, x$digits),
                           " (W=", round(x$sw.samplingDist$statistic, x$digits),
                           "; based on ", x$samples, " observations)");
  }
  
  ### Show output
  cat("## SAMPLE DISTRIBUTION ###\n");
  cat(paste0("Sample distribution of ", x$sampleSize,
             " observations\n",
             "Mean=", round(mean(x$sampleVector), x$digits),
             ", median=", round(median(x$sampleVector), x$digits),
             ", SD=", round(sd(x$sampleVector), x$digits),
             ", and therefore SE of the mean = ",
             round(sd(x$sampleVector)/sqrt(x$sampleSize), x$digits),
             ",\nKurtosis=", round(x$kurtosis.samplingDist, x$digits),
             ", Skewness=", round(x$skewness.samplingDist, x$digits),
             "\n", sw.sampleDist, "\n",
             "Anderson-Darling: p=", round(1-x$ad.sampleDist@test$p.value, x$digits),
             " (A=", round(x$ad.sampleDist@test$statistic, x$digits), ")\n",
             "Kolmogorov-Smirnof: p=", round(x$ks.sampleDist$p.value, x$digits),
             " (D=", round(x$ks.sampleDist$statistic, x$digits), ")"));
  
  cat("\n\n## SAMPLING DISTRIBUTION FOR THE MEAN ###\n");
  cat(paste0("Sampling distribution of ", x$samples, " samples of n=", x$sampleSize, "\n",
             "Mean=", round(mean(x$samplingDistribution), x$digits),
             ", median=", round(median(x$samplingDistribution), x$digits),
             ", SD=", round(sqrt(var(x$samplingDistribution)), x$digits),
             ",\nKurtosis=", round(x$kurtosis.samplingDist, x$digits),
             ", Skewness=", round(x$skewness.samplingDist, x$digits),
             ".\n", sw.samplingDist, "\n",
             "Anderson-Darling: p=", round(1-x$ad.samplingDist@test$p.value, x$digits),
             " (A=", round(x$ad.samplingDist@test$statistic, x$digits), ")\n",
             "Kolmogorov-Smirnof: p=", round(x$ks.samplingDist$p.value, x$digits),
             " (D=", round(x$ks.samplingDist$statistic, x$digits), ")"));

  ### Plots
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow=1, ncol=2)));
  suppressWarnings(print(x$plot.sampleDist, vp=viewport(layout.pos.row = 1, layout.pos.col = 1)));
  suppressWarnings(print(x$plot.samplingDist, vp=viewport(layout.pos.row = 1, layout.pos.col = 2)));

  invisible(); 
}