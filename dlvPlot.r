### XKCD styled plots
#require(xkcd)
#vignette("xkcd-intro")

### Histogram with dots
# http://stackoverflow.com/questions/16216312/how-to-plot-stacked-point-histograms-in-ggplot2-in-r

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### Load the plyr package, which has ddply, for processing
### chunks of a dataframe and then combining the results
safeRequire("plyr");
### Loads the ggplot2 package, which is used for plotting
safeRequire("ggplot2");
### For using units in panel margin specification etc
safeRequire("grid");

### Theme used for the plots
dlvTheme <- function(base_size = 14, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = element_text(colour="#000000", size = rel(0.8)),
      axis.ticks        = element_line(colour = "black"),
      axis.title        = element_blank(),
      legend.text       = element_text(size = rel(0.6)),
      legend.key        = element_rect(colour = "grey80"),
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.key.size   = unit(6, "mm"),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      strip.background  = element_rect(fill = "grey80", colour = "grey50"),
      strip.background  = element_rect(fill = "grey80", colour = "grey50"),
      panel.margin      = unit(c(.5), "cm")
    )
}

dlvPlot <- function(dat, x = NULL, y, z = NULL, jitter = "FALSE",
                    error="lines", dotsize="density", densityDotBaseSize=3,
                    ...) {
  ### This function constructs a dot-line-violin plot.
  ###
  ### dat    = [dataframe] dataframe containing x, y and z
  ### x      = [character] name of predictor ('independent') variable,
  ###          must be categorical (i.e. a factor)
  ### y      = [character vector] criterion ('dependent') variable,
  ###          must be numeric
  ### z      = [character] moderator variable, must be
  ###          categorical (i.e. a factor)
  ### jitter = [logical] whether or not to jitter individual datapoints
  ### error  = [character] "none", "lines" or "whiskers"; whether to
  ###          show the confidence interval as lines with (whiskers) or
  ###          without (lines) horizontal whiskers or not at all (none)
  ### dotsize= [character] "density" or "normal"; when "density", the
  ###          size of each dot corresponds to the density of the
  ###          distribution at that point.
  ### densityDotBaseSize = base size of dots when their size corresponds
  ###                      to the density (bigger = larger dots)
  ###
  ### The behavior of this function depends on the parameters:
  ###
  ### x    y         z    Outcome
  ### NULL char      NULL Univariate plot for numerical y variable
  ### NULL ch vector NULL Multiple Univariate plots, with variable
  ###                     names determining categories on x-axis and
  ###                     with numerical y variables on y-axis
  ### char char      NULL Bivariate plot where factor x determines
  ###                     categories on x-axis with numerical
  ###                     variable y on the y-axis (roughly a line
  ###                     plot with a single line)
  ### char char      char Multivariate plot where factor x determines
  ###                     categories on x-axis, factor z determines
  ###                     the different lines, and with the numerical
  ###                     y variable on the y-axis
  
  ### Create object to return results
  res <- list();
  
  ### Store data
  res$dat.raw <- dat;
  ### Remove incomplete cases
  res$dat <- dat[complete.cases(dat), ];
  
  if(!is.null(x) & !(is.factor(dat[, x]))) {
    warning("Error: variable x (', x,') is not of type factor. X must be a categorical ",
            "variable with a limited number of categories. If this is the case, but it's ",
            "simply stored as a numerical vector, use the 'factor' function to convert ",
            "it (see '?factor'). Trying to convert x myself now.");
    res$dat[[x]] <- factor(res$dat[[x]]);
  }

  if(!is.null(z) & !(is.factor(dat[, z]))) {
    warning("Error: variable z (', z,') is not of type factor. Z must be a categorical ",
            "variable with a limited number of categories. If this is the case, but it's ",
            "simply stored as a numerical vector, use the 'factor' function to convert ",
            "it (see '?factor'). Trying to convert z myself now.");
    res$dat[[z]] <- factor(res$dat[[z]]);
  }
  
  if(is.null(x)) {
    ### We have no predictor variable - this means we construct univariate plots.

    ### Now check whether we have to construct one or several.
    if(length(y)==1) {
      
      ###############################################################
      ### Constructing one univariate plot                        ###
      ###############################################################
      
      ### Store variable name in dataframe
      if (is.null(res$dat$variable)) {
        res$dat$variable <- y;
        xVarName <- 'variable';
      }
      else {
        res$dat$variable_dvlPlot <- y;
        xVarName <- 'variable_dvlPlot';
      }
      
      ### Store density at y value
      dens <- density(res$dat[[y]], na.rm=TRUE);
      res$dat$y_density <- approx(dens$x, dens$y, xout=res$dat[[y]])$y;
      ### Multiply so that points at average density have size 1
      res$dat$y_density <- res$dat$y_density *
        (densityDotBaseSize/mean(res$dat$y_density, na.rm=TRUE));
      
      ### Construct dataframe with confidence interval info
      mean <- mean(res$dat[, y]);
      sd <- sd(res$dat[, y]);
      se <- sd / sqrt(nrow(res$dat));
      ci.lo <- mean - 1.96 * se;
      ci.hi <- mean + 1.96 * se;
      res$descr <- data.frame(y = y,
                              n = nrow(res$dat),
                              mean = mean, sd = sd,
                              se = se,
                              ci.lo = ci.lo,
                              ci.hi = ci.hi);
      res$yRange=c(min(res$dat[[y]][!is.na(dat[[y]])]),
                   max(res$dat[[y]][!is.na(dat[[y]])]));
      res$plot <- ggplot(data=res$dat, aes_string(x=xVarName, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=.2, fill="#BBBBBB", linetype="blank");
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01), alpha=.4);
      }
      else {
        if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes(size=y_density), color='grey60',
                                            alpha=.1, show_guide=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=.1, size=1);
        }
      }
      if (error == "lines") {
        res$plot <- res$plot + geom_pointrange(data=res$descr,
                                               aes(x=y, y=mean, ymin=ci.lo, ymax=ci.hi),
                                               size = 1);
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes(x=y, y=mean, ymin=ci.lo, ymax=ci.hi),
                                             size = 1, width=.1);
      }
      res$plot <- res$plot + geom_point(data=res$descr,
                                        aes(x=y, y=mean), size=5);
      
    }
    else {
      
      ###############################################################
      ### Constructing several univariate plots                   ###
      ###############################################################
      
      ### Apparently, we have to construct several plots.
      ### First generate a dataframe where the variables names
      ### are stored in another variable that we can use to
      ### make categories on the x axis
      
      ### Store original dataframe
      res$dat.original <- res$dat;
      res$dat <- data.frame();
      ### Create empty descriptives dataframe
      res$descr <- data.frame();
      
      ### Loop through original dataframe and construct new one
      for (currentVar in y) {
        tempDf <- data.frame(y = res$dat.original[, currentVar]);
        tempDf$x <-  currentVar;
        ### Store density for at y value
        dens <- density(tempDf$y, na.rm=TRUE);
        tempDf$y_density <- approx(dens$x, dens$y, xout=tempDf$y)$y;
        tempDf$y_density <- tempDf$y_density *
          (densityDotBaseSize/mean(tempDf$y_density, na.rm=TRUE));
        ### Store y values and name of y variable in res$dat dataframe
        res$dat <- rbind(res$dat, tempDf);
        ### Get mean and confidence interval for descriptives table
        mean <- mean(tempDf$y);
        sd <- sd(tempDf$y);
        se <- sd / sqrt(nrow(tempDf));
        ci.lo <- mean - 1.96 * se;
        ci.hi <- mean + 1.96 * se;
        ### Add descriptives
        res$descr <- rbind(res$descr, data.frame(y = currentVar,
                                                 n = nrow(tempDf),
                                                 mean = mean, sd = sd,
                                                 se = se,
                                                 ci.lo = ci.lo,
                                                 ci.hi = ci.hi));
      }
      
      #res$yRange=c(min(res$dat[[y]][!is.na(dat[[y]])]),
      #             max(res$dat[[y]][!is.na(dat[[y]])]));
      
      res$plot <- ggplot(data=res$dat, aes(x=x, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=.2, fill="#BBBBBB", linetype="blank");
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01), alpha=.4);
      }
      else {
        if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes(size=y_density), color='grey60',
                                            alpha=.1, show_guide=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=.1, size=1);
        }
      }
      if (error == "lines") {
        res$plot <- res$plot + geom_pointrange(data=res$descr,
                                               aes(x=y, y=mean, ymin=ci.lo, ymax=ci.hi),
                                               size = 1);
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes(x=y, y=mean, ymin=ci.lo, ymax=ci.hi),
                                             size = 1, width=.1);
      }
      res$plot <- res$plot + geom_point(data=res$descr,
                                        aes(x=y, y=mean), size=5);
      
    }
  }
  else {
    ### We have a predictor variable, so check whether we have a moderator
    if (is.null(z)) {
      
      ###############################################################
      ### Constructing multivariate plot without moderator        ###
      ###############################################################
      
      ### Construct dataframe with confidence interval info
      res$descr <- ddply(.data = res$dat, .variables = c(x),
                         .fun = function (dat) {
                           dat <- dat[complete.cases(dat), ];
                           mean <- mean(dat[, y]);
                           sd <- sd(dat[, y]);
                           se <- sd / sqrt(nrow(dat));
                           ci.lo <- mean - 1.96 * se;
                           ci.hi <- mean + 1.96 * se;
                           rslt <- data.frame(x = dat[1, x],
                                              y = y,
                                              n = nrow(dat),
                                              mean = mean, sd = sd,
                                              se = se, ci.lo = ci.lo,
                                              ci.hi = ci.hi);
                           rslt <- rslt[complete.cases(rslt), ];
                           return(rslt);
                         });
      ### Store densities; must be done for each group (value of x)
      ### separately
      res$dat <- ddply(.data = res$dat, .variables = c(x),
                       .fun = function (dat) {
                         ### Store density for at y value
                         dens <- density(dat[[y]], na.rm=TRUE);
                         dat$y_density <- approx(dens$x, dens$y, xout=dat[[y]])$y;
                         ### Multiply with densityDotBaseSize / mean (this allows
                         ### control over the size of the dots)
                         dat$y_density <- dat$y_density *
                           (densityDotBaseSize/mean(dat$y_density, na.rm=TRUE));
                         return(dat);
                       });
      
      res$yRange=c(min(res$dat[[y]][!is.na(dat[[y]])]),
                   max(res$dat[[y]][!is.na(dat[[y]])]));
      res$plot <- ggplot(data=res$dat, aes_string(x=x, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=.2, fill="#BBBBBB", linetype="blank", position=position_identity());
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01), alpha=.4);
      }
      else {
        if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes(size=y_density),
                                            alpha=.1, show_guide=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=.1, size=1);
        }
      }
      if (error == "lines") {
        res$plot <- res$plot + geom_pointrange(data=res$descr,
                                               aes(x=x, y=mean, ymin=ci.lo, ymax=ci.hi),
                                               size = 1);
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes(x=x, y=mean, ymin=ci.lo, ymax=ci.hi),
                                             size = 1, width=.1);
      }
      res$plot <- res$plot + stat_summary(fun.y=mean, geom="point", size=1);
      res$plot <- res$plot + geom_line(data=res$descr,
                                       aes(x=as.numeric(x), y=mean), size=1);
    }
    else {
      
      ###############################################################
      ### Constructing multivariate plot with moderator           ###
      ###############################################################

      ### Construct dataframe with confidence interval info
      res$descr <- ddply(.data = res$dat, .variables = c(x, z),
                     .fun = function (dat) {
                       dat <- dat[complete.cases(dat), ];
                       mean <- mean(dat[, y]);
                       sd <- sd(dat[, y]);
                       se <- sd / sqrt(nrow(dat));
                       ci.lo <- mean - 1.96 * se;
                       ci.hi <- mean + 1.96 * se;
                       res <- data.frame(x = dat[1, x],
                                         y = y,
                                         z = dat[1, z],
                                         n = nrow(dat),
                                         mean = mean, sd = sd,
                                         se = se, ci.lo = ci.lo,
                                         ci.hi = ci.hi);
                       return(res[complete.cases(res), ]);
                     });
      ### Store densities; must be done for each group (value of x)
      ### separately
      res$dat <- ddply(.data = res$dat, .variables = c(x, z),
                       .fun = function (dat) {
                         ### Store density for at y value
                         dens <- density(dat[[y]], na.rm=TRUE);
                         dat$y_density <- approx(dens$x, dens$y, xout=dat[[y]])$y;
                         ### Multiply with densityDotBaseSize / mean (this allows
                         ### control over the size of the dots)
                         dat$y_density <- dat$y_density *
                           (densityDotBaseSize/mean(dat$y_density, na.rm=TRUE));
                         return(dat);
                       });
      
      res$yRange=c(min(res$dat[[y]][!is.na(dat[[y]])]),
                   max(res$dat[[y]][!is.na(dat[[y]])]));
      
      res$plot <- ggplot(data=res$dat, aes_string(x=x, y=y, z=z, colour=z, group=paste0(x,":",z)));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(data=res$dat, aes_string(fill=z), trim=FALSE, alpha=.1, linetype="blank", position=position_identity());
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01), alpha=.4);
      }
      else {
        if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes(size=y_density),
                                            alpha=.1, show_guide=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=.1, size=1);
        }
      }
      if (error == "lines") {
        res$plot <- res$plot + geom_pointrange(data=res$descr,
                                               aes(x=x, y=mean, ymin=ci.lo, ymax=ci.hi, group=z),
                                               size = 1);
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes(x=x, y=mean, ymin=ci.lo, ymax=ci.hi, group=z),
                                             size = 1, width=.1);
      }
      res$plot <- res$plot + stat_summary(fun.y=mean, geom="point", size=1);
      res$plot <- res$plot + geom_line(data=res$descr, aes(x=x, y=mean, group=z), size=1);
    }
  }
  
  ### Return result
  return(res);
}

