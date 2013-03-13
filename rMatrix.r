###########################################################
###
### R file with the function rMatrix, which provides a
### symmatric or asymmetric matrix of correlations,
### their confidence intervals, and p-values. The p-values
### can be corrected for multiple testing.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################

###########################################################
### Define functions
###########################################################

rMatrix <- function(dat, x, y=NULL, conf.level = .95, correction = "fdr",
                    digits = 2, pval=FALSE, colspace=2, rowspace=0) {
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
  
  ### Check whether the first vector of vectors has 
  if (length(x) < 1) {
    stop(paste0("Error: x vector has 0 elements or less; ",
                "make sure to specify at least one variable name!."));
  }  
  
  ### Check whether we have a second set of variables
  if (!is.null(y)) {
    if (length(y) < 1) {
      stop(paste0("Error: y vector has 0 elements or less; ",
                  "make sure to specify at least one variable name!."));
    }
    symmetric <- FALSE;
  }
  else {
    y <- x;
    symmetric <- TRUE;
  }
  
  ### Create object to return, and store variable names, confidence of
  ### confidence interval, and digits
  res <- list();
  res$variables.rows <-x;
  res$variables.cols <- y;
  res$ci.confidence <- conf.level;
  res$correction <- correction;
  res$digits <- digits;
  res$pval <- pval;
  res$colspace <- colspace;
  res$rowspace <- rowspace;
  res$r <- matrix(nrow = length(x), ncol = length(y));
  res$parameter <- matrix(nrow = length(x), ncol = length(y));
  res$ci.lo <- matrix(nrow = length(x), ncol = length(y));
  res$ci.hi <- matrix(nrow = length(x), ncol = length(y));
  res$p.raw <- matrix(nrow = length(x), ncol = length(y));
  res$p.adj <- matrix(nrow = length(x), ncol = length(y));
  
  xCounter <- 1;
  for(curXvar in x) {
    yCounter <- 1;
    for(curYvar in y) {
      curTest <- cor.test(dat[,curXvar], dat[,curYvar], use="complete.obs");
      res$r[xCounter, yCounter] <- curTest$estimate;
      res$parameter[xCounter, yCounter] <- curTest$parameter;
      res$ci.lo[xCounter, yCounter] <- curTest$conf.int[1];
      res$ci.hi[xCounter, yCounter] <- curTest$conf.int[2];
      res$p.raw[xCounter, yCounter] <- curTest$p.value;
      yCounter <- yCounter + 1;
    }
    xCounter <- xCounter + 1;
  }
  
  ### If a symmetric table was requested, remove half
  ### the correlations and the diagonal
  if (symmetric) {
    ### Remove lower half of the matrices
    res$r[lower.tri(res$r)] = NA;
    res$parameter[lower.tri(res$parameter)] = NA;
    res$ci.lo[lower.tri(res$ci.lo)] = NA;
    res$ci.hi[lower.tri(res$ci.hi)] = NA;
    res$p.raw[lower.tri(res$p.raw)] = NA;
    ### Remove diagonal in matrices
    for(diagonal in c(1:nrow(res$r))) {
      res$r[diagonal, diagonal] <- NA;
      res$parameter[diagonal, diagonal] <- NA;
      res$ci.lo[diagonal, diagonal] <- NA;
      res$ci.hi[diagonal, diagonal] <- NA;
      res$p.raw[diagonal, diagonal] <- NA;
    }
  }
  
  ### Correct p-values for multiple testing
  res$p.adj <- matrix(p.adjust(res$p.raw, method=correction), nrow(res$p.raw), ncol(res$p.raw));
  
  ### Set row and column names
  rownames(res$r) <- x;         colnames(res$r) <- y;
  rownames(res$parameter) <- x; colnames(res$parameter) <- y;
  rownames(res$ci.lo) <- x;     colnames(res$ci.lo) <- y;
  rownames(res$ci.hi) <- x;     colnames(res$ci.hi) <- y;
  rownames(res$p.raw) <- x;     colnames(res$p.raw) <- y;
  rownames(res$p.adj) <- x;     colnames(res$p.adj) <- y;
  
  ### Set class & return result
  class(res) <- c("correlationMatrix");
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

print.correlationMatrix <- function (x, digits=x$digits, ...) {
  ### We want multiple lines per cell, so we'll need to print manually.
  ### We first print the confidence interval on the first line; then,
  ### on the next line, the point estimate; and finally, on the last line,
  ### the p-value (corrected for multiple testing).
    
  ### Compute how wide the columns should be. This depends on
  ### 1) the width of the confidence intervals, and 2) the width
  ### of the variable name in each column
  ### The maximum length of confidence intervals is:
  ### [-.X; -.X]
  ### Where the number of X's is determined by digits.
  ### Thus, 8 + digits * 2 represents the max length of
  ### confidence interval.
  maxConfIntLength <- 8 + digits * 2;
  
  ### Now, for each column, store the length of the variable name
  ### of that column.
  colSizes <- nchar(x$variables.cols);
  
  ### Then, compare these to the maxConfIntLength, and store the
  ### larger of the two
  colSizes <- ifelse(colSizes > maxConfIntLength, colSizes, maxConfIntLength);
  
  ### If pval is TRUE, we use the p-value function to format the p-values.
  ### This means that the columns need to be three characters wider, in case
  ### we'll need the scientific notation somewhere.
  if(x$pval) {
    colSizes <- colSizes + 3;
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
    ### Print the column name
    cat(x$variables.cols[j]);
    ### Print trailing spaces (+x$colspace to have space between columns)
    cat(repeatStr(" ", colSizes[j] - nchar(x$variables.cols[j]) + x$colspace));
  }
  ### Print newline character
  cat("\n");
  
  ### Now we'll start printing the rows, starting with the variable
  ### name and the confidence interval.
  for(i in (1:length(x$variables.rows))) {
    
    ### Print variable name for this row
    cat(x$variables.rows[i]);
    ### Print spaces needed to line up second column
    cat(repeatStr(" ", leftColSize - nchar(x$variables.rows[i])));
    
    ### Now we need two loops (one for each line) to create the cells.
    ### Normally, we could provide paste0 (or paste) with a vector,
    ### and it would concatenate the elements for us, but in this case,
    ### every column can have a different width, so we need a different
    ### number of leading spaces.
    
    ### First, the confidence intervals
    for(j in (1:length(x$variables.cols))) {
      ### If the point estimate is NA, don't display anything
      if (is.na(x$r[i,j])) {
        cat(repeatStr(" ", colSizes[j] + x$colspace));
      }
      else {
        ### Create confidence interval for this column
        confInt <- paste0("[", formatR(x$ci.lo[i,j], digits), "; ", formatR(x$ci.hi[i,j], digits), "]");
        ### Print confidence interval
        cat(confInt);
        ### Print trailing spaces (+ x$colspace to have space between columns)
        cat(repeatStr(" ", colSizes[j] - nchar(confInt) + x$colspace));
      }
    }
    
    ### Print newline character
    cat("\n");
    ### Start in second column
    cat(repeatStr(" ", leftColSize));
    
    ### Then, the point estimate and p-value
    for(j in (1:length(x$variables.cols))) {
      ### If the point estimate is NA, don't display anything
      if (is.na(x$r[i,j])) {
        cat(repeatStr(" ", colSizes[j] + x$colspace));
      }
      else {
        ### Create r & p
        if(x$pval) {
          content <- paste0("r=", formatR(x$r[i,j], digits), ", p=", noZero(format.pval(x$p.adj[i,j], digits)));
        }
        else {
          content <- paste0("r=", formatR(x$r[i,j], digits), ", p=", formatR(x$p.adj[i,j], digits));
        }
        ### Print point estimate and p-value
        cat(content);
        ### Print trailing spaces (+x$colspace to have space between columns)
        cat(repeatStr(" ", colSizes[j] - nchar(content) + x$colspace));
      }
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
  
  invisible();
}

