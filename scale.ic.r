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
library("psych");
library("GPArotation");
library("ltm");

###########################################################
### Define functions
###########################################################

scale.ic <- function (dataframe, itemnames = 'all', digits = 2) {
  ### if itemnames contains only 1 element (or less), we
  ### include all items.
  if (length(itemnames) <= 1) {
    ### Remove all cases with missing data (listwise deletion)
    dat <- na.omit(dataframe);
  }
  else {
    ### Select relevant items and remove all cases with
    ### missing data (listwise deletion)
    dat <- na.omit(subset(dataframe, select=itemnames));
  }
  ### Get correlation matrix (input for omega & glb)
  dat.cor <- cor(dat, use="complete.obs");
  ### Cronbach's alpha
  dat.alpha <- cronbach.alpha(dat, na.rm=TRUE);
  ### GLB
  dat.glb <- glb(dat);
  ### Omega
  dat.omega <- omega(dat);
  ### Store results in a list to return
  result = list(output = list(dataframe      = deparse(substitute(dataframe)),
                              items          = names(dat),
                              n.items        = ncol(dat),
                              n.observations = nrow(dat),
                              cronbach.alpha = dat.alpha$alpha,
                              glb.max        = dat.glb$glb.max,
                              omega.total    = dat.omega$omega.tot,
                              digits         = digits),
                output.dataframe = data.frame(n.items        = ncol(dat),
                                              n.observations = nrow(dat),
                                              cronbach.alpha = dat.alpha$alpha,
                                              glb.max        = dat.glb$glb.max,
                                              omega.total    = dat.omega$omega.tot),
                object.alpha  = dat.alpha,
                object.glb    = dat.glb,
                object.omega  = dat.omega);
  ### Set result class (to enable nice printing)
  class(result) <- c("scale.ic");
  ### Return result
  return(result);
}

print.scale.ic <- function (x, digits=x$output$digits, ...) {
  cat(paste0("Dataframe: ", x$output$dataframe,
           "\nItems: ", paste(x$output$items, collapse=", "),
           "\nObservations: ", x$output$n.observations,
           "\nCronbach's alpha: ", round(x$output$cronbach.alpha, digits=digits),
           "\nGreatest Lower Bound (GLB): ", round(x$output$glb.max, digits=digits),
           "\nOmega(total): ", round(x$output$omega.total, digits=digits), "\n"));
  invisible();
}
