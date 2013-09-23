###########################################################
###########################################################
###
### Function to generate a PDF with some diagnostics to
### assess how the elements (usually items) in a scale
### relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### This function checks whether the file for a
### self-made function exists; if so, it's loaded;
### if not, the file is downloaded from GitHub
loadOwnFunction <- function(fileName) {
  sourceLoaded <- FALSE;
  if (exists('libraryPath')) {
    if (file.exists(paste0(libraryPath, paste0(fileName, ".r")))) {
      source(paste0(libraryPath, paste0(fileName, ".r")));
      sourceLoaded <- TRUE;
    }
  }
  
  if (!sourceLoaded) {
    ### Note: I took this from
    ### https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/source_https.R
    ### and edited it to fit in here.
    
    # Filename: source_https.R
    # Purpose: function to source raw code from github project
    # Author: Tony Bryal
    # Date: 2011-12-10
    # http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
    safeRequire('RCurl');
    # read script lines from website using a security certificate
    script <- getURL(paste0("http://github.com/Matherion/library/raw/master/", fileName, ".r"),
                     followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem",
                                                                 package = "RCurl"));
    # parse lines and evaluate in the global environment
    eval(parse(text = script), envir= .GlobalEnv);
  }
}

### Function to escape special latex characters, based on
### http://stackoverflow.com/questions/5406071/r-sweave-latex-escape-variables-to-be-printed-in-latex
sanitizeLatexS <- function(str) {
  gsub('([#$%&~_\\^\\\\{}])', '\\\\\\1', str, perl = TRUE);
}

### Load the ggplot2 package, for plotting graphics
safeRequire("ggplot2");
### Load the knitr package, for generating latex and PDF files
safeRequire("knitr");
### Load the xtable package, for making tex tables
safeRequire("xtable");
### Loas function to assess a variable's normality
loadOwnFunction('normalityAssessment');

### This function generates a pdf file with a report
### describing the variables.
scaleInspection <- function(dat, items, pdfLaTexPath, filename="scaleInspection", digits=4) {
  ### dat          : dataframe containing the items to inspect
  ### items        : either a character vector with the itemnames, or,
  ###                if the items are organised in scales, a list of
  ###                character vectors with the items in each scale.
  ### pdfLaTexPath : the path to PdfLaTex. This file is part of a LaTeX
  ###                installation that creates a pdf out of a .tex file.
  ###
  ###                In Windows, you can download (portable) MikTex from
  ###                  http://miktex.org/portable. You then decide yourself
  ###                  where to install MikTex; pdflatex will end up in a
  ###                  subfolder 'miktex\bin', so if you installed MikTex
  ###                  in, for example, 'C:\Program Files\MikTex', the total
  ###                  path becomes 'C:\Program Files\MikTex\miktex\bin'. Note
  ###                  that R uses slashes instead of backslashes to separate
  ###                  folders, so in this example, pdfLatexPath should be
  ###                  'C:/Program Files/MikTex/miktex/bin'
  ###
  ###                In MacOS, you can install MacTex from http://tug.org/mactex/
  ###                  or a smaller distribution called BasicTex from
  ###                  http://www.tug.org/mactex/morepackages.html
  ###                  In MacOs, by default, pdflatex ends up in folder '/user/texbin',
  ###                  which is what pdfLaTexPath should be in that default case.
  ###
  ###                In Ubuntu, you can install TexLive base by using your package
  ###                  manager to install texlive-latex-base, or using the terminal:
  ###                  'sudo apt-get install texlive-latex-base'
  ###                  In Ubuntu, by default pdflatex ends up in folder '/usr/bin',
  ###                  which is what pdfLaTexPath should be in that default case.
  ###                
  ### filename     : the filename to use to save the pdf
  ### digits       : the number of digits to use in the tables

  if (!is.list(items)) {
    items <- list('none' <- items);
  }
  if (FALSE %in% sapply(items, is.vector)) {
    ### If this happens, that means there 's non-vector
    ### element in the list of items, which is a problem
    stop("There is a non-vector element in the list with items:\n",
         print(sapply(items, is.vector)));
  }
  if (FALSE %in% sapply(items, is.character)) {
    ### If this happens, that means there 's non-character 
    ### vector element in the list of items, which is a problem
    stop("There is a non-character vector element in the list with items.\n",
         "Here follows a list of booleans indicating which of the list elements ",
         "are character vectors (TRUE) and which aren't (FALSE):\n",
         paste0(names(lapply(items, is.character)), ': ', lapply(items, is.character), '\n'));
  }
  
  if ((!file.exists(paste0(pdfLaTexPath, "/pdflatex.exe"))) &
        (!file.exists(paste0(pdfLaTexPath, "/pdflatex")))) {
    stop('In path "', pdfLaTexPath, '", the file pdflatex.exe (Windows) or ',
         'pdflatex (MacOS or Ubuntu (Linux)) does not exist! Please ',
         'locate the file and provide its path (without the last ',
         'slash).');
  }

  res <- list();
  res$items <- items;
  res$scales <- list();
  res$describe <- list();
  res$scaleDiagnostics <- list();
  res$scaleDiagnostics.errors <- list();
  res$normality.plots <- list();
  res$rnwBit <- list();
  res$normality.sampleDist <- list();
  res$normality.samplingDist <- list();
  
  res$rnw <- "\\documentclass[a4paper,portrait,11pt]{article}

% For adjusting margins
\\usepackage[margin=15mm]{geometry}

% !Rnw weave = knitr

\\title{Chatbot Study Scale Inspection}
\\author{Gjalt-Jorn Peters}
\\begin{document}
\\raggedright
\\noindent
";  
  
  ### Process each scale separately
  for (currentScale in names(items)) {
    res$scales[[currentScale]] <- rowMeans(dat[, items[[currentScale]]], na.rm=TRUE);
    ### Extract univariate descriptives to show
    res$describe[[currentScale]] <-
      describe(res$scales[[currentScale]])[, c('n', 'mean', 'sd', 'median', 'min', 'max', 'skew', 'kurtosis')];
    ### Generate and store plots for assessment of normality
    res$normality.plots[[currentScale]] <-
      normalityAssessment(res$scales[[currentScale]],
                          xLabel.sampleDist = 'Sample Distribution',
                          yLabel.sampleDist = 'Density',
                          xLabel.samplingDist = 'Sampling Distribution',
                          yLabel.samplingDist = 'Density',
                          samplingDistLineSize = .5,
                          normalLineSize = .2);
    ### Create dataframe with normality statistics for the
    ### sample distribution
    res$normality.sampleDist[[currentScale]] <-
      data.frame(sw = c(round(res$normality.plots[[currentScale]]$sw.sampleDist$statistic, 4),
                        round(res$normality.plots[[currentScale]]$sw.sampleDist$p.value, 4)),
                 ad = c(round(res$normality.plots[[currentScale]]$ad.sampleDist@test$statistic, 4),
                        round(res$normality.plots[[currentScale]]$ad.sampleDist@test$p.value, 4)),
                 ks = c(round(res$normality.plots[[currentScale]]$ks.sampleDist$statistic, 4),
                        round(res$normality.plots[[currentScale]]$ks.sampleDist$p.value, 4)));
    row.names(res$normality.sampleDist[[currentScale]]) <- c('value', 'p-val');
    ### Create dataframe with normality statistics for the
    ### sampling distribution
    res$normality.samplingDist[[currentScale]] <-
      data.frame(sw = c(round(res$normality.plots[[currentScale]]$sw.samplingDist$statistic, 4),
                        round(res$normality.plots[[currentScale]]$sw.samplingDist$p.value, 4)),
                 ad = c(round(res$normality.plots[[currentScale]]$ad.samplingDist@test$statistic, 4),
                        round(res$normality.plots[[currentScale]]$ad.samplingDist@test$p.value, 4)),
                 ks = c(round(res$normality.plots[[currentScale]]$ks.samplingDist$statistic, 4),
                        round(res$normality.plots[[currentScale]]$ks.samplingDist$p.value, 4)));
    row.names(res$normality.samplingDist[[currentScale]]) <- c('value', 'p-val');
    ### Generate scale diagnosis
    
    tryCatch(
      res$scaleDiagnostics[[currentScale]] <- diagnoseScale(dat, as.vector(items[[currentScale]]))
      , error = function(e) {
        res$scaleDiagnostics.errors[[currentScale]] <- e;
      }
    );    
    
    ### Generate the content:
    ###  - name of measure (scale)
    ###  - list of items in scale
    ###  - table with univariate descriptives of scale
    ###  - minipage with:
    ###     - plotted sample distribution
    ###     - normality statistics for sample distribution
    ###  - minipage with:
    ###     - plotted sampling distribution
    ###     - normality statistics for sampling distribution
    ###  - internal consistency coefficients
    ###  - principal component analysis
    ###  - ggpairs plot of scatterplots, correlations, and histograms
    
    res$rnwBit[[currentScale]] <-
      paste0('\\newpage\n',
             'SCALE: ',
             sanitizeLatexS(currentScale),
             '\n\\newline\nITEMS: ',
             sanitizeLatexS(paste(items[[currentScale]], collapse=", ")),
             '\n\\newline\n',
             '<< echo=FALSE, results="asis" >>=\n',
             '  print(xtable(res$describe[["',
             currentScale,
             '"]], digits=c(0, 0, rep(digits, 7))), tabular.environment="tabular",
             print.rownames=FALSE, floating=FALSE);\n',
             '@\n',
             '\\begin{minipage}[t]{80mm}\n',
             '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=8/2.54, fig.height=8/2.54 >>=\n',
             'res$normality.plots[["', currentScale, '"]]$plot.sampleDist;\n',
             '@\n',
             '<< echo=FALSE, results="asis" >>=\n',
             '  print(xtable(res$normality.sampleDist[["',
             currentScale,
             '"]], digits=digits), tabular.environment="tabular",
             floating=FALSE);\n',
             '@\n',
             '\\end{minipage}%\n',
             '\\begin{minipage}[t]{80mm}\n',
             '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=8/2.54, fig.height=8/2.54 >>=\n',
             'res$normality.plots[["', currentScale, '"]]$plot.samplingDist;\n',
             '@\n',
             '<< echo=FALSE, results="asis" >>=\n',
             '  print(xtable(res$normality.samplingDist[["',
             currentScale,
             '"]], digits=digits), tabular.environment="tabular",
             floating=FALSE);\n',
             '@\n',
             '\\end{minipage}%\n\\newline\n');
    
    if (res$scaleDiagnostics[[currentScale]]$scale.ic$n.items > 2) {
      res$rnwBit[[currentScale]] <-
        paste0(res$rnwBit[[currentScale]],
               '<< echo=FALSE, results="asis" >>=\n',
               'cat(paste0("\n\\newline\nOmega(total): ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$output$omega.total, digits), "\n\\newline\nGreatest Lower Bound (GLB): ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$output$glb.max, digits), "\n\\newline\nCronbach\'s alpha: ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$output$cronbach.alpha, digits), "\n\\newline\n',
               'Eigen values: ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$eigen$values, digits), "\n\\newline\n',
               'Number of factors with Eigen value > 1: ", res$scaleDiagnostics[["',
               currentScale,
               '"]]$factors, "\n\\newline\n", res$scaleDiagnostics[["',
               currentScale,
               '"]]$fa$loadings, "\n\\newline\n));',
               '@\n');
    } else if (res$scaleDiagnostics[[currentScale]]$scale.ic$n.items == 2) {
      res$rnwBit[[currentScale]] <-
        paste0(res$rnwBit[[currentScale]],
               '<< echo=FALSE, results="asis" >>=\n',
               '  cat(paste0("\n\nSpearman Brown coefficient: ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$output$spearman.brown, digits), "\n\nCronbach\'s alpha: ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$output$cronbach.alpha, digits), "\n\nPearson correlation: ", round(res$scaleDiagnostics[["',
               currentScale,
               '"]]$scale.ic$cor[1,2], digits), "\n\n"));\n',
               '@\n');
    }
    
  }

  ### Combine all pages generated for each scale
  for (currentScale in names(items)) {
    res$rnwPanels <- paste0(res$rnwPanels,
                            res$rnwBit[[currentScale]],
                            '\n\\newpage');
  }
  
  res$rnw <- c(res$rnw, '\\section{Item Inspection}
GENERATED ON ', date(),'\n
CONTENTS: ', length(names(items)), ' measures (scales).\n
\\newpage');

  ### Combine all pieces
  res$rnw <- c(res$rnw, res$rnwPanels, "\n\\end{document}");
  
  ### Write the knitr text to a file
  fileConn<-file(paste0(filename, ".rnw"));
  writeLines(res$rnw, fileConn);
  close(fileConn);
  
  ### Knit the .tex file
  knit(paste0(getwd(), "/", filename, ".rnw"), paste0(getwd(), "/", filename, ".tex"));
  
  ### Convert the .tex file to a pdf
  tryCatch(
    res$texOutput <- system(paste0('"', pdfLaTexPath, '/pdflatex" "',
                                   getwd(), '/', filename, '.tex" ',
                                   '-output-directory "', getwd(), '"'),
                            intern=TRUE)
    , error = function(e) {
      cat(paste("Error returned by pdflatex: ", e));
    }
  );
  
  ### Store result for later inspection
  class(res) <- c('itemInspection');
  return(res);
  
}