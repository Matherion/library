### e1071 contains the kurtosis and skewness functions
if (!is.element("e1071", installed.packages()[,1])) {
  install.packages("e1071");
}
### Load package
require(e1071);

### Function definition
samplingDistribution <- function(popValues = c(0, 1), popFrequencies = c(50, 50),
                                 sampleSize = 100, samples = 100,
                                 showSingleSample=FALSE, savePlot=FALSE, digits=3) {
  
  ### Temporarily store scientific notation threshold
  tempSciPen <- getOption("scipen");
  ### Set at 10 + digits
  options(scipen=10+digits);

  ### Take samples samples of sampleSize people and store the means
  ### (first generate an empty vector to store the means)
  samplingDistribution <- c();
  for (i in 1:samples) {
    samplingDistribution[i] <- mean(sample(popValues, size=sampleSize,
                                           replace=TRUE, prob=popFrequencies));
  }
  
  ### We need to tell the hist() function where to put
  ### the breaks. We will determine this by computing
  ### the distances between the different possible
  ### population values.
  firstVector <- popValues[-length(popValues)];
  secondVector <- popValues[-1];
  diffVector <- secondVector - firstVector;
  ### We can now divide these distances by two. When we then add the resulting
  ### vector to each population value (except the last one of course), we get
  ### the break points.
  diffVector <- diffVector/ 2;
  breaksVector <- popValues[-length(popValues)] + diffVector;
  ### Add the minimum value to the beginning
  breaksVector <- c(popValues[1] - diffVector[1], breaksVector);
  ### Add the maximum value to the end
  breaksVector[length(breaksVector)+1] <- popValues[length(popValues)] + diffVector[length(diffVector)];

  if (showSingleSample) {
    ### Take a sample of sampleSize people to show the distribution
    singleSample <- sample(popValues, size=sampleSize,
                           replace=TRUE, prob=popFrequencies);
    
    ### Maybe save plot to file
    if (savePlot) {
      png(filename=paste0("sample of n=", sampleSize, ".png"), height=300, width=600,
          bg="white");
    }
    ### Show or save the sample distribution
    hist(singleSample, breaks=breaksVector,
         main=paste0("sample of n=", sampleSize));
    if (savePlot) {
      dev.off();
    }
    
    ### If there are more than 5000 datapoints, only use the first 5000 datapoints
    if (length(singleSample) < 5000) {
      normality <- shapiro.test(singleSample);
      normTest <- paste0("Shapiro-Wilk normality test: p=", round(normality$p.value, digits),
                  " (W=", round(normality$statistic, digits),
                  "; based on ", sampleSize, " observations)");
    }
    else {
      normality <- shapiro.test(singleSample[1:5000]);
      normTest <- paste0("Shapiro-Wilk normality test: p=", round(normality$p.value, digits),
                         " (W=", round(normality$statistic, digits),
                         "; NOTE: based on the first 5000 of ",
                         sampleSize, " observations)");
    }
    ### Show output
    cat(paste0("Sample of n=", sampleSize, "\n",
               "Mean=", round(mean(singleSample), digits),
               ", SD=", round(sqrt(var(singleSample)), digits),
               ", Kurtosis=", round(kurtosis(singleSample), digits),
               ", Skewness=", round(skewness(singleSample), digits),
               "\nEstimated standard error (n = ",  sampleSize,"): ",
               round(sqrt(var(singleSample)) / sqrt(sampleSize), digits),
               "\n", normTest, "\n\n"));
    
  }
  
  ### Maybe save plot to file
  if (savePlot) {
    png(filename=paste0("sampling distribution (n=", sampleSize, ", ",
                        samples, " samples).png"), height=300, width=600,
        bg="white");
  }
  ### Show or save the sampling distribution
  ### Note that we don't use "breaks=breaksVector" because
  ### the sampling distribution contains mans, which have
  ### much higher granularity than the population values.
  ### However, we do maintain the minimum and maximum scores.
  hist(samplingDistribution, xlim=range(breaksVector),
       main=paste0("sampling distribution (n=", sampleSize, ", ", samples, " samples)"));
  if (savePlot) {
    dev.off();
  }
  
  ### If there are more than 5000 datapoints, only use the first 5000 datapoints
  if (length(samplingDistribution) < 5000) {
    normality <- shapiro.test(samplingDistribution);
    normTest <- paste0("Shapiro-Wilk normality test: p=", round(normality$p.value, digits),
                       " (W=", round(normality$statistic, digits),
                       "; based on ", samples, " observations)");
  }
  else {
    normality <- shapiro.test(samplingDistribution[1:5000]);
    normTest <- paste0("Shapiro-Wilk normality test: p=", round(normality$p.value, digits),
                       " (W=", round(normality$statistic, digits),
                       "; NOTE: based on the first 5000 of ",
                       samples, " observations)");
  }
  ### Show output
  cat(paste0("Sampling distribution of ", samples, " samples of n=", sampleSize, "\n",
             "Mean=", round(mean(samplingDistribution), digits),
             ", SD=", round(sqrt(var(samplingDistribution)), digits),
             ", Kurtosis=", round(kurtosis(samplingDistribution), digits),
             ", Skewness=", round(skewness(samplingDistribution), digits),
             "\n", normTest, "\n\n"));
  
  options(scipen=tempSciPen);
  
}

### Create a distribution of three possible values
popValues <- c(1, 2, 3);
popFrequencies <- c(20, 50, 30);

### Show the sampling distribution for samples of 5, 10, and 50 people
samplingDistribution(popValues = popValues, popFrequencies = popFrequencies,
                     sampleSize = 200, samples = 100000, showSingleSample=TRUE, digits=5);

### Create a very skewed distribution of ten possible values
popValues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
popFrequencies <- c(2, 4, 8, 6, 10, 15, 12, 200, 350, 400);

### Show the sampling distribution for samples of 5, 10, and 50 people
samplingDistribution(popValues = popValues, popFrequencies = popFrequencies,
                     sampleSize = 200, samples = 100000, showSingleSample=TRUE, digits=5);

### Create a skewed distribution of seven possible values
popValues <- c(1, 2, 3, 4, 5, 6, 7);
popFrequencies <- c(70, 15, 5, 4, 3, 2, 1);

### Show the sampling distribution for a sample of 144 people
samplingDistribution(popValues = popValues, popFrequencies = popFrequencies,
                     sampleSize = 144, samples = 100000, showSingleSample=TRUE, digits=5);

### Input a very skewed sample
popValues <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
popFrequencies <- c(37, 20, 28, 18, 10, 8, 4, 1, 3, 6, 4, 3);

### Show the sampling distribution for this sample
samplingDistribution(popValues = popValues, popFrequencies = popFrequencies,
                     sampleSize = sum(popFrequencies), samples = 50000, showSingleSample=TRUE, digits=5);

### Input sample from paper to review for HRJ
popValues <- c(1:15);
popFrequencies <- c(150, 200, 100, 50, 40, 30, 20, 10, 0, 5, 3, 2, 1, 2, 1);

### Show the sampling distribution for this sample
samplingDistribution(popValues = popValues, popFrequencies = popFrequencies,
                     sampleSize = 50, samples = 50000, showSingleSample=TRUE, digits=5);
