### This function conducts a logistic regression and
### plots the predicted values and the confidence
### interval.
###
### Note: this function is based on the very helpful answer
### that smillig and user21010 gave to a question on the
### CrossValidated StackExchange page at
### http://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

logisticPlot <- function(criterion, predictor,
                         probabilities = c(.25, .5, .75),
                         xAxisRange = NULL, granularity = 1,
                         xdigits = 2, ydigits=2, xlab = NULL, ylab = NULL,
                         savePlots = "both", plotFormat = "svg", savePredictorValues = FALSE) {
  ### criterion     = the dependent variable (must be dichotomous)
  ### predictor     = the independent variable (should normally be interval)
  ### probabilities = the probabilities for which the generate specific
  ###                 predictions
  ### xAxisRange    = range of X axis (can't always be determined from data,
  ###                 because the data may only cover part of the scale)
  ### granularity            determines which steps to take when
  ###                        creating the x-axis vector for the plot (which
  ###                        runs from the xAxisRange minimum to its maximum)
  ### xdigits       = precision of labels on x axes of plots
  ### ydigits       = precision of labels on y axes of plots
  ### xlab          = label of x axes of plots
  ### ylab          = label of y axes of plots
  ### savePlots     = determines which plots are saved: can be "none",
  ###                 "exact", "closest" or "both"
  ### plotFormat    = determines format to save plots: can be "svg" or "png"
  ### savePredictorValues = whether to save the selected predictor values
  
  ### Create an object to store the results in
  res <- list();
  
  ### Store parameters
  res$parameters <- list();
  res$parameters$criterion <- deparse(substitute(criterion));
  res$parameters$predictor <- deparse(substitute(predictor));
  res$parameters$probabilities <- probabilities;
  res$parameters$xAxisRange <- xAxisRange;
  res$parameters$granularity <- granularity;

  ### If the variables are from a dataset, extract variable names
  if (grep("$", res$parameters$criterion)) {
    criterionName <- unlist(strsplit(res$parameters$criterion, "\\$"))[2];
  }
  else {
    criterionName <- res$parameters$criterion;
  }
  if (grep("$", res$parameters$predictor)) {
    predictorName <- unlist(strsplit(res$parameters$predictor, "\\$"))[2];
  }
  else {
    predictorName <- res$parameters$predictor;
  }
  
  ### Generate the model
  model <- glm(criterion ~ predictor,family=binomial(link="logit"));
  res$model <- model;
  
  ### Now we configure from where to where the x-axis of the
  ### plot should go - we may not be able to derive this from the
  ### data after all, because observed values may only cover a
  ### subset of the scale.
  if (is.null(xAxisRange)) {
    xAxisMin <- min(predictor, na.rm=TRUE);
    xAxisMax <- max(predictor, na.rm=TRUE);
  }
  else {
    xAxisMin <- min(xAxisRange);
    xAxisMax <- max(xAxisRange);
  }
  
  xAxisMin <- round(xAxisMin, xdigits);
  xAxisMax <- round(xAxisMax, xdigits);
  
  res$xAxisMin <- xAxisMin;
  res$xAxisMax <- xAxisMax;
  
  ### Now we generate a data frame that contains a variable that
  ### runs from the specified minimum to the specified maximum.
  ### We will then generate predicted probabilities for every
  ### value in that variable.
  
  fittedData <- data.frame(seq(xAxisMin, xAxisMax, by=granularity));
  names(fittedData)[1] = "predictor";
  
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
    selectedPredictions$closest.predictor <- append(selectedPredictions$closest.predictor,
                                                    fittedData$predictor[currentRow]);
    selectedPredictions$closest.lowerCI <- append(selectedPredictions$closest.lowerCI,
                                                  fittedData$lowerCI[currentRow]);
    selectedPredictions$closest.probability <- append(selectedPredictions$closest.probability,
                                                      fittedData$predictedProbability[currentRow]);
    selectedPredictions$closest.upperCI <- append(selectedPredictions$closest.upperCI,
                                                  fittedData$upperCI[currentRow]);
  }
  
  ### Now calculate the exact (interpolated or extrapolated) values of the
  ### predictor (note: these may be unrealistic)
  for (currentProbability in probabilities) {
    selectedPredictions$exact.predictor <- append(selectedPredictions$exact.predictor,
                                                  (log(currentProbability/(1-currentProbability)) - coef(model)[["(Intercept)"]]) /
                                                    coef(model)[['predictor']]);
  }  

  exactPredictor <- data.frame(selectedPredictions$exact.predictor);
  names(exactPredictor)[1]= "predictor";
  
  ### Get the exact predicted logodds for these values
  #exactPredictions <- predict(model, newData=exactPredictor, se.fit=TRUE);
  exactPredictions <- predict(model, newdata=exactPredictor, se.fit=TRUE);
  
  ### Convert these logodds to probablities
  exactPredictions$predictedProbability <- exp(exactPredictions$fit) /
    (1 + exp(exactPredictions$fit));
  exactPredictions$lowerCI <- exp(exactPredictions$fit - 1.96 * exactPredictions$se.fit) /
    (1 + exp(exactPredictions$fit - 1.96 * exactPredictions$se.fit));
  exactPredictions$upperCI <- exp(exactPredictions$fit + 1.96 * exactPredictions$se.fit) /
    (1 + exp(exactPredictions$fit + 1.96 * exactPredictions$se.fit));  

  ### Build a dataframe with all this information
  selectedPredictions.dat <- data.frame("probability" = probabilities,
                                        "exact predictor" = selectedPredictions$exact.predictor,
                                        "exact lower CI" = exactPredictions$lowerCI,
                                        "exact probability" = exactPredictions$predictedProbability,
                                        "exact upper CI" =exactPredictions$upperCI,
                                        "closest predictor" = selectedPredictions$closest.predictor,
                                        "closest lower CI" = selectedPredictions$closest.lowerCI,
                                        "closest probability" = selectedPredictions$closest.probability,
                                        "closest upper CI" = selectedPredictions$closest.upperCI);
  ### Store in object to return
  res$selectedPredictions.dat <- selectedPredictions.dat;
  
  ###########################################################
  ### Plotting
  ###########################################################
  
  ### First create a mapping to the default windows font (not
  ### sure what this does on a mac; you may be able to comment
  ### it out). In Windows, to change the font used in the plot,
  ### just change it here.
  
  windowsFonts(plotFont=windowsFont("TT Arial"))
  
  ### Set x and y labels
  if (is.null(xlab)) {
    xlabel <- predictorName;
  }
  else {
    xlabel <- xlab;
  }
  if (is.null(ylab)) {
    ylabel <- paste0("Probability at ", criterionName);
  }
  else {
    ylabel <- ylab;
  }
  
  ### Specify the plots

  ### Determine whether we need to add 0 and 1 to the
  ### yBreaks vector
  yBreaks.closest <- as.vector(selectedPredictions$closest.probability);
  if (0 < min(yBreaks.closest)) {
    yBreaks.closest <- append(0, yBreaks.closest);
  }
  if (max(yBreaks.closest) < 1) {
    yBreaks.closest <- append(yBreaks.closest, 1);
  }
  
  ### Set the xBreaks vector and perhaps add breaks if the
  ### scale minimum and maximum are not yet included.
  xLimits.closest <- c(xAxisMin, xAxisMax);
  xBreaks.closest <- selectedPredictions$closest.predictor;
  if (xAxisMin < min(selectedPredictions$closest.predictor)) {
    xBreaks.closest <- append(xAxisMin, xBreaks.closest);
  }
  if (max(selectedPredictions$closest.predictor) < xAxisMax) {
    xBreaks.closest <- append(xBreaks.closest, xAxisMax);
  }

  ### Round breaks
  xBreaks.closest <- round(xBreaks.closest, xdigits);
  xLimits.closest <- round(xLimits.closest, xdigits);
  yBreaks.closest <- round(yBreaks.closest, ydigits);
  
  res$plot.closest <- ggplot(fittedData, aes(predictor, predictedProbability)) +
    geom_line() +
    geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), alpha=0.3) +
    xlab(xlabel) +
    ylab(ylabel) +
    scale_x_continuous(limits = xLimits.closest,
                       expand = c(0,0),
                       breaks = xBreaks.closest) +
    scale_y_continuous(limits = c(0,1),
                       expand = c(0,0),
                       breaks = yBreaks.closest) +
    theme_bw() +
    theme(plot.title     = element_text(family = "plotFont", face="bold", size=15)
          , axis.title   = element_text(family = "plotFont", face="bold", size=15)
          , axis.title.y = element_text(angle=90, vjust = -0.1)
          , axis.title.x = element_text(vjust = -0.5)
          , axis.text    = element_text(family = "plotFont", face="bold", size=10)
          , plot.margin  = unit(c(1, 1, 1, 1), "cm")
    );

  ### Determine whether we need to add 0 and 1 to the
  ### yBreaks vector
  yBreaks.exact <- as.vector(exactPredictions$predictedProbability);
  if (0 < min(yBreaks.exact)) {
    yBreaks.exact <- append(0, yBreaks.exact);
  }
  if (max(yBreaks.exact) < 1) {
    yBreaks.exact <- append(yBreaks.exact, 1);
  }
  
  ### Since the exact predictions may be interpolated or extrapolated,
  ### they may contain nonsensical values that may exceed the min/max
  ### scale anchors. Thus, we may need to redefine the min/max of the
  ### X scale. Also, if we don't redefine, add the scale min/max to
  ### the xBreaks vector.
  xLimits.exact <- c(xAxisMin, xAxisMax);
  xBreaks.exact <- selectedPredictions$exact.predictor;
  if (min(selectedPredictions$exact.predictor) < xAxisMin) {
    xLimits.exact[1] <- min(selectedPredictions$exact.predictor)
  }
  else {
    xBreaks.exact <- append(xAxisMin, xBreaks.exact);
  }
  if (max(selectedPredictions$exact.predictor) > xAxisMax) {
    xLimits.exact[2] <- max(selectedPredictions$exact.predictor)
  }
  else {
    xBreaks.exact <- append(xBreaks.exact, xAxisMax);
  }
  
  ### Round breaks
  xBreaks.exact <- round(xBreaks.exact, xdigits);
  xLimits.exact <- round(xLimits.exact, xdigits);
  yBreaks.exact <- round(yBreaks.exact, ydigits);
  
  ### If there was extrapolation, our predictions don't extend
  ### to the whole x axis, so we need a new set of predictions.
  if ((min(xLimits.exact) < xAxisMin) | (xAxisMax < max(xLimits.exact))) {

    ### Generate dataframe with new predictor values
    newFit <- data.frame(seq(min(xLimits.exact), max(xLimits.exact), by=granularity));
    names(newFit)[1] = "predictor";
    
    ### Get new predictions
    newPred <- predict(model, newdata=newFit, se.fit=TRUE);
    res$predictions.exact <- newPred;  
    
    ### Convert the logarithmic values to probabilities and get CI.    
    newFit$predictedProbability <- exp(newPred$fit) /
      (1 + exp(newPred$fit));
    newFit$lowerCI <- exp(newPred$fit - 1.96 * newPred$se.fit) /
      (1 + exp(newPred$fit - 1.96 * newPred$se.fit));
    newFit$upperCI <- exp(newPred$fit + 1.96 * newPred$se.fit) /
      (1 + exp(newPred$fit + 1.96 * newPred$se.fit));
    
    ### Store new data in returnable object
    res$fittedData.exact <- newFit;
    
    data.exact <- newFit;
    
  }
  else {
    data.exact <- fittedData;
  }

  res$plot.exact <- ggplot(data.exact, aes(predictor, predictedProbability)) +
    geom_line() +
    geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), alpha=0.3) +
    xlab(xlabel) +
    ylab(ylabel) +
    scale_x_continuous(limits = xLimits.exact,
                       expand = c(0,0),
                       breaks = xBreaks.exact) +
    scale_y_continuous(limits = c(0,1),
                       expand = c(0,0),
                       breaks = yBreaks.exact) +
    theme_bw() +
    theme(plot.title   = element_text(family = "plotFont", face="bold", size=15)
          , axis.title   = element_text(family = "plotFont", face="bold", size=15)
          , axis.title.y = element_text(angle=90, vjust = -0.1)
          , axis.title.x = element_text(vjust = -0.5)
          , axis.text    = element_text(family = "plotFont", face="bold", size=10)
          , plot.margin  = unit(c(1, 1, 1, 1), "cm")
    );

  ### Save plots as scalable vector graphics file, for further editing
  ### and eventually rasterization. Note that width and height are
  ### specified in centimeters.
  
  if ((savePlots == "both") | (savePlots == "closest")) {
    plotFilename <- paste0(predictorName, " predicting ", criterionName, " (closest).", plotFormat);
    ggsave(file=plotFilename, plot=res$plot.closest, width=6, height=6);
  }

  if ((savePlots == "both") | (savePlots == "exact")) {
    plotFilename <- paste0(predictorName, " predicting ", criterionName, " (exact).", plotFormat);
    ggsave(file=plotFilename, plot=res$plot.exact, width=6, height=6);
  }
  
  ### Save selected predictor values
  if (savePredictorValues) {
    predictorValuesFilename <- paste0("predictor values & probabilities for ", predictorName,
                                      " predicting ", criterionName, ".txt");
    write.table(selectedPredictions.dat, predictorValuesFilename, sep="\t", row.names=FALSE);
  }
  
  return(res);
  
}

