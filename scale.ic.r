###########################################################
###
### R file with the function scale.ic, which is basically
### just a convenient wrapper for functions in three other
### packages (psych, GPArotation, and ltm) that compute
### Cronbach's alpha, the GLB, and omega(total).
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################

###########################################################
### 
### Example of the use of the function for SPSS users
###
### The easiest way is to simply save a datafile with only
### the items in the scale you're interested in (repeat
### this if there are multiple such scales, of course).
### This is possible by selecting "Save As" in SPSS's
### File menu and then clicking the button marked
### "Variables". This allows the user to select which
### variables to store by 'keeping' or 'dropping'
### variables.
###
### Then use the commands below to load the the resulting
### datafile into R and obtain the internal consistency
### measures.
###
### To use these commands, remove the three hash symbols
### at the start of each line below, and replace
### [[FOLDER&FILENAME]] with the folder and filename of
### the datafile, for example:
### "C:/Users/Gjalt-Jorn/Desktop/datafile.sav"
### Note that in R, instead of backslashes, you have to
### use slashes like in this example.
### Then, simply paste these lines into R:
###
############ START OF R CODE
### if (!is.element("foreign", installed.packages()[,1])) {
###    install.packages("foreign");
### }
### require('foreign');
### dat <- read.spss("[[FOLDER&FILENAME]]",
###                  use.value.labels=FALSE,
###                  to.data.frame=TRUE);
### scale.ic(dataframe = dat);
############ END OF R CODE
###
### R will then report Cronbach's alpha, the GLB,
### and Omega(total).
### 
###########################################################

###########################################################
### Installing the required packages
###########################################################

### The psych package contains the omega function and the glb function
if (!is.element('psych', installed.packages()[,1])) {
  install.packages('psych');
}
### The omega function in the psych package requires the GPArotation package
if (!is.element('GPArotation', installed.packages()[,1])) {
  install.packages('GPArotation');
}
### The ltm package contains the cronbach.alpha function
if (!is.element('ltm', installed.packages()[,1])) {
  install.packages('ltm');
}

###########################################################
### Loading the required packages
###########################################################
require("psych");
require("GPArotation");
require("ltm");

###########################################################
### Define functions
###########################################################

scale.ic <- function (dataframe, itemnames = 'all', digits = 2) {
  ### Make object to store results
  res <- list();
  
  ### if itemnames contains only 1 element (or less), we
  ### include all items.
  if (length(itemnames) <= 1) {
    ### Remove all cases with missing data (listwise deletion)
    res$dat <- na.omit(dataframe);
  }
  else {
    ### Select relevant items and remove all cases with
    ### missing data (listwise deletion)
    res$dat <- na.omit(subset(dataframe, select=itemnames));
  }

  res$dat.name <- deparse(substitute(dataframe));
  res$n.items <- ncol(res$dat);
  res$n.observations <- nrow(res$dat);
  res$items <- itemnames;
  res$digits <- digits;
  ### Store results in a convenient list
  res$output <- list();
  ### Also generate a dataframe (useful when
  ### requesting measures for many scales)
  res$output.dataframe <- data.frame(n.items        = res$n.items,
                                     n.observations = res$n.observations);
                                     
  ### Get correlation matrix (input for omega & glb)
  res$cor <- cor(res$dat, use="complete.obs");
  ### Cronbach's alpha
  res$alpha <- cronbach.alpha(res$dat, na.rm=TRUE);
  res$output$cronbach.alpha <- res$alpha$alpha;
  res$output.dataframe$cronbach.alpha <- res$alpha$alpha;
  
  ### GLB and Onega can only be computed if the number
  ### of items exceeds two
  
  if (res$n.items > 2) {
    ### GLB
    res$glb <- glb(res$dat);
    res$output$glb.max  <- res$glb$glb.max;
    res$output.dataframe$glb.max  <- res$glb$glb.max;
    ### Omega
    res$omega <- omega(res$dat, plot=FALSE);
    res$output$omega.total <- res$omega$omega.tot;
    res$output.dataframe$omega.total <- res$omega$omega.tot;
  }
  else if (res$n.items == 2) {
    ### Otherwise, compute Spearman Brown coefficient
    ### (see Eisinga, te Grotenhuis & Pelzer (2013). The reliability
    ### of a two-item scale: Pearson, Cronbach, or Spearman-Brown?
    ### International journal of public health, 58(4), 637-42.
    ### doi:10.1007/s00038-012-0416-3)
    ### Get r in numeric variable for convenience
    r <- res$cor[1,2];
    res$spearman.brown <- 1 / (1 + (1 / ((r/(1-r)) + (r/(1-r)))));
    res$output$spearman.brown <- res$spearman.brown;
    res$output.dataframe$spearman.brown <- res$spearman.brown;
  }
  
  ### Set result class (to enable nice printing)
  class(res) <- c("scale.ic");
  ### Return result
  return(res);
}

print.scale.ic <- function (x, digits=x$digits, ...) {
  cat(paste0("Dataframe: ", x$dat.name,
           "\nItems: ", paste(x$items, collapse=", "),
           "\nObservations: ", x$n.observations));
  if (x$n.items > 2) {
    cat(paste0("\nOmega(total): ", round(x$output$omega.total, digits=digits),
               "\nGreatest Lower Bound (GLB): ", round(x$output$glb.max, digits=digits),
               "\nCronbach's alpha: ", round(x$output$cronbach.alpha, digits=digits), "\n"));
  } else if (x$n.items == 2) {
    cat(paste0("\nSpearman Brown coefficient: ", round(x$output$spearman.brown, digits=digits),
               "\nCronbach's alpha: ", round(x$output$cronbach.alpha, digits=digits),
               "\nPearson Correlation: ", round(x$cor[1, 2], digits=digits), "\n"));
  }
  invisible();
}
