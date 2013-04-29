### This function conducts a logistic regression and
### plots the predicted values and the confidence
### interval.
###
### Note: this function is based on the very helpful answer
### that smillig and user21010 gave to a question on the
### CrossValidated StackExchange page at
### http://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

logisticPlot <- function(criterion, predictor,
                         probabilities = c(0, .1, .3, .5, .7, .9, 1),
                         xAxisRange = NULL, granularity = 1) {
  ### granularity            determines which steps to take when
  ###                        creating the x-axis vector for the plot (which
  ###                        runs from the xAxisRange minimum to its maximum)
  
  ### Create an object to store the results in
  res <- list();
  
  ### Store parameters
  res$parameters <- list;
  res$parameters$criterion <- deparse(substitute(criterion));
  res$parameters$predictor <- deparse(substitute(predictor));
  res$parameters$probabilities <- probabilities;
  res$parameters$probabilities.accuracy <- probabilities.accuracy;
  #res$parameters$xAxisRange <- xAxisRange;
  res$parameters$granularity <- granularity;
  
  ### Generate the model
  model <- glm(criterion ~ predictor,family=binomial(link="logit"));
  res$model <- model;
  
  ### Now we configure from where to where the x-axis of the
  ### plot should go - we may not be able to derive this from the
  ### data after all, because observed values may only cover a
  ### subset of the scale.
  ###  (Note that
  ###     min(dat$ALEIDSSC, na.rm=TRUE);
  ###   results in 0 and
  ###     max(dat$ALEIDSSC, na.rm=TRUE);
  ###  results in 98. However, the LEIDSSC has a theoretical maximum
  ###  valueof 136.)
  
  if (is.null(xAxisRange)) {
    xAxisMin <- min(predictor);
    xAxisMax <- max(predictor);
  }
  else {
    xAxisMin <- min(xAxisRange, na.rm=TRUE);
    xAxisMax <- max(xAxisRange, na.rm=TRUE);
  }
  res$xAxisMin <- xAxisMin;
  res$xAxisMax <- xAxisMax;
  
  ### Now we generate a data frame that contains a variable that
  ### runs from the specified minimum to the specified maximum.
  ### We will then generate predicted probabilities for every
  ### value in that variable.
  
  fittedData <- data.frame("predictor"=seq(xAxisMin, xAxisMax, by=granularity));
  
  ### This command takes our model and a dataframe (the dataframe we
  ### just generates). It then computes predicted values, using the
  ### variable in the dataframe as input and our model as the
  ### function. Because we set parameter se.fit to TRUE, the standard
  ### error for every prediction will be stored as well - we will
  ### need this to plot the confidence interval.
  
  predictions <- predict(model, newdata=fittedData, se.fit=TRUE);
  res$predictions <- predictions;
  
  ### Now, we convert the logarithmic values back to probabilities.
  ### We use the formula
  ###
  ###   P = exp(value) / (1 + exp(value))
  ###
  ### for that.
  
  fittedData$predictedProbability <- exp(predictions$fit) /
    (1 + exp(predictions$fit));
  fittedData$lowerCI <- exp(predictions$fit - 1.96 * predictions$se.fit) /
    (1 + exp(predictions$fit - 1.96 * predictions$se.fit));
  fittedData$upperCI <- exp(predictions$fit + 1.96 * predictions$se.fit) /
    (1 + exp(predictions$fit + 1.96 * predictions$se.fit));
  
  ### Store in object with results to return
  res$fittedData <- fittedData;
  
  ### Now find the points where the predicted probability matches
  ### the probabilities for which we want the values of the
  ### predictor variable (stored in probabilities).
  
  ### This fragment is based on
  ### http://r.789695.n4.nabble.com/Find-the-closest-value-in-a-list-or-matrix-td838131.html
  
  ### Find and store the row numbers
  rowNumber <- c();
  for (currentProbability in probabilities) {
    rowNumber <- append(rowNumber, c(which(abs(fittedData$predictedProbability - currentProbability) ==
                                             min(abs(fittedData$predictedProbability - currentProbability)))));
  }
  
  ### Get the corresponding values for the independent variable
  ### and the lower and upper bounds of the confidence interval
  selectedPredictions <- list();
  selectedPredictions.dat <- data.frame();
  for (currentRow in rowNumber) {
    selectedPredictions$closest.predictor <- append(selectedPredictions$predictor,
                                                    fittedData$predictor[currentRow]);
    selectedPredictions$closest.lowerCI <- append(selectedPredictions$lowerCI,
                                                  fittedData$lowerCI[currentRow]);
    selectedPredictions$closest.probability <- append(selectedPredictions$probability,
                                                      fittedData$predictedProbability[currentRow]);
    selectedPredictions$closest.upperCI <- append(selectedPredictions$upperCI,
                                                  fittedData$upperCI[currentRow]);
  }
  
  for (currentProbability in probabilities) {
    selectedPredictions$exact.predictor <- append(selectedPredictions$exact.predictor,
                                                  (log(currentProbability/(1-currentProbability)) - coef(model)[["(Intercept)"]]) /
                                                    coef(model)[['predictor']]);
  }
  
  selectedPredictions.dat <- data.frame(probabilities,
                                        selectedPredictions$exact.predictor,
                                        selectedPredictions$closest.predictor,
                                        selectedPredictions$closest.lowerCI,
                                        selectedPredictions$closest.probability,
                                        selectedPredictions$closest.upperCI);
  ### Store in object to return
  res$selectedPredictions.dat <- selectedPredictions.dat;
  
  ###########################################################
  ### Plotting
  ###########################################################
  
  ### First create a mapping to the default windows font (not
  ### sure what this does on a mac; you may be able to comment
  ### it out). In Windows, to change the font used in the plot,
  ### just change it here.
  
  windowsFonts(plotFont=windowsFont("TT Trebuchet MS"))
  
  ### Specify the plot
  
  res$plot <- ggplot(fittedData, aes(predictor, predictedProbability)) +
    geom_line() +
    geom_ribbon(aes(ymin=lowerCI,ymax=upperCI),alpha=0.3) +
    scale_x_continuous(limits = c(xAxisMin, xAxisMax),
                       expand = c(0,0),
                       breaks = selectedPredictions$predictor) +
    scale_y_continuous(limits = c(0,1),
                       expand = c(0,0),
                       selectedPredictions$probability) +
    theme_bw() +
    theme(plot.title   = element_text(family = "plotFont", face="bold", size=30)
          , axis.title   = element_text(family = "plotFont", face="bold", size=30)
          , axis.title.y = element_text(angle=90)
          , plot.margin  = unit(c(1, 1, 1, 1), "cm")
    );
  
  return(res);
  
}  
