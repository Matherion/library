###########################################################
###########################################################
###
### Function to generate a PDF with some descriptives of
### variables and their associations.
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
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
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
sanitizeLatexString <- function(str) {
  str <- gsub('([#$%&~_\\^\\\\{}])', '\\\\\\1', str, perl = TRUE);
}

### Load the ggplot2 package, for plotting graphics
safeRequire("ggplot2");
### Load the GGally package, for more
### convenient interaction with ggplot
safeRequire("GGally");
### Load the knitr package, for generating latex and PDF files
safeRequire("knitr");
### Load the xtable package, for making tex tables
safeRequire("xtable");
### Create correlation matrix with confidence intervals
loadOwnFunction('associationMatrix');

### This function generates a pdf file with a report
### describing the variables.
variableInspection <- function(dat, sets= NULL,
                               docTitle = "Variable inspection", docAuthor = "Author",
                               pdfLaTexPath,
                               filename = "scaleInspection", digits=2,
                               associationMatrixColsLandscape = 6,
                               pboxWidthMultiplier = 1,
                               scatterPlotBaseSize = 4,
                               pageMargins=15,
                               pval=TRUE) {
  ### dat          : dataframe containing the sets to inspect
  ### sets         : either a character vector with the variable names, or,
  ###                if the variables are organised in sets, a list of
  ###                character vectors with the variables in each set.
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
  ### filename             : the filename to use to save the pdf
  ### digits               : the number of digits to use in the tables
  ### associationMatrixColsLandscape : at how many columns (or rather,
  ###                        variables) or more should
  ###                        associationMatrices be printed landscape?
  ### pboxWidthMultiplier  : used for print.rMatrix; used to tweak the width of
  ###                        columns in the correlation matrix
  ### scatterPlotBaseSize  : size of one scatterplot in the scattermatrix in
  ###                        centimeters. If the total scattermatrix becomes
  ###                        larger than 18 cm, it's scaled down to 18 cm.
  ### pageMargins          : pageMargins: margins of the page in millimeters
  ### pval                 : whether to print p-values as p-values in
  ###                        correlation matrix

  if (is.null(sets)) {
    sets <- names(dat);
  }
  
  if (!is.list(sets)) {
    sets <- list(unspecified = sets);
  }

  if (FALSE %in% sapply(sets, is.vector)) {
    ### If this happens, that means there 's non-vector
    ### element in the list of sets, which is a problem
    stop("There is a non-vector element in the list with sets:\n",
         print(sapply(sets, is.vector)));
  }
  
  if (FALSE %in% sapply(sets, is.character)) {
    ### If this happens, that means there 's non-character 
    ### vector element in the list of sets, which is a problem
    stop("There is a non-character vector element in the list with sets.\n",
         "Here follows a list of booleans indicating which of the list elements ",
         "are character vectors (TRUE) and which aren't (FALSE):\n",
         paste0(names(lapply(sets, is.character)), ': ', lapply(sets, is.character), '\n'));
  }
  
  if ((!file.exists(paste0(pdfLaTexPath, "/pdflatex.exe"))) &
        (!file.exists(paste0(pdfLaTexPath, "/pdflatex")))) {
    stop('In path "', pdfLaTexPath, '", the file pdflatex.exe (Windows) or ',
         'pdflatex (MacOS or Ubuntu (Linux)) does not exist! Please ',
         'locate the file and provide its path (without the last ',
         'slash).');
  }

  res <- list();
  res$sets <- sets;
  res$describe <- list();
  res$associationMatrix <- list();
  res$ggpairs.normal <- list();
  res$ggpairs.jittered <- list();
  res$ggpairs.combined <- list();
  res$rnwBit <- list();
  
  res$rnw <- paste0("\\documentclass[a4paper,portrait,10pt]{article}

% For adjusting margins
\\usepackage[margin=", pageMargins, "mm]{geometry}
% For printing correlation table on rotated page
\\usepackage{pdflscape}
% For resizing correlationtables that become too large
\\usepackage{adjustbox}

% !Rnw weave = knitr

\\title{", docTitle, "}
\\author{", docAuthor, "}
\\begin{document}
\\raggedright
\\noindent
");
  
  ### Process each set of variables separately
  for (currentSet in names(sets)) {
    ### Extract univariate descriptives to show
    res$describe[[currentSet]] <-
      describe(dat[, res$sets[[currentSet]]])[, c('n', 'mean', 'sd', 'median', 'min', 'max', 'skew', 'kurtosis')];
    ### Generate correlation table
    res$associationMatrix[[currentSet]] <- associationMatrix(dat, sets[[currentSet]]);

    ### Generate 'scattermatrix' with ggpairs
    
    ### Visual representation of bivariate correlations
    ### First generate a normal scattermatrix with histograms
    ### on the diagonal

    tempDat <- dat[, res$sets[[currentSet]]];
    
    res$ggpairs.normal[[currentSet]] <-
      ggpairs(data=tempDat,
               diag=list(continuous="bar", discrete="bar"),
               lower=list(continuous="points", combo="dot", discrete="facetbar"),
               upper=list(continuous="smooth", combo="denstrip", discrete="ratio"),
              axisLabels="none");
    ### Then generate one with jittered points
#     res$ggpairs.jittered[[currentSet]] <-
#       ggpairs(data=res$dat[, res$sets[[currentSet]]],
#               params=c(position="jitter"),
#               diag=list(continuous="bar", discrete="bar"),
#               lower=list(continuous="points", combo="dot", discrete="facetbar"),
#               upper=list(continuous="smooth", combo="denstrip", discrete="ratio"),
#               axisLabels="none");
    ### Then place the histograms on the diagonal of
    ### the jittered scattermatrix
#     res$ggpairs.combined[[currentSet]] <- res$ggpairs.jittered;
#     for (currentVar in 1:length(variableNames)) {
#       res$ggpairs.combined[[currentSet]] <-
#         putPlot(res$ggpairs.combined[[currentSet]],
#                 getPlot(res$ggpairs.normal[[currentSet]], currentVar, currentVar),
#                 currentVar, currentVar);
#     }
#     
#     for (currentRowFromTop in 1:length(variableNames)) {
#       for (currentColumnFromLeft in 1:length(variableNames)) {
#         res$ggpairs.combined <-
#           putPlot(res$ggpairs.combined,
#                   getPlot(res$ggpairs.combined, currentRowFromTop, currentColumnFromLeft) + plotSettings,
#                   currentRowFromTop, currentColumnFromLeft);
#       }
#     }
    
    ### Generate the content:
    ###  - name of set
    ###  - list of variables in set
    ###  - table with univariate descriptives of scale
    ###  - ggpairs plot
    ###  - associationMatrix
    
    res$rnwBit[[currentSet]] <-
      paste0('\\newpage\n',
             '\\section{SET: ',
             sanitizeLatexString(currentSet),
             '}\n',
             sanitizeLatexString(paste(sets[[currentSet]], collapse=", ")),
             '\n\n\\vspace{1ex}\n',
             '<< echo=FALSE, results="asis" >>=\n',
             '  print(xtable(res$describe[["',
             currentSet,
             '"]], digits=c(0, 0, rep(digits, 7))), tabular.environment="tabular",
             print.rownames=FALSE, floating=FALSE);\n',
             '@\n');
        
    ### Include association table;
    ### whether to print on a portrait page or
    ### on a landscape page depends on number of
    ### columns and associationMatrixColsLandscape
    if (length(res$associationMatrix[[currentSet]]$variables.cols) < associationMatrixColsLandscape) {
      res$rnwBit[[currentSet]] <-
        paste0(res$rnwBit[[currentSet]],
               '\n\\begin{minipage}{\\textwidth}\n\\maxsizebox{\\textwidth}{\\textheight}{\n');
    }
    else {
      res$rnwBit[[currentSet]] <-
        paste0(res$rnwBit[[currentSet]],
               '\\begin{landscape}\n\\maxsizebox{', 297 - 2*pageMargins, 'mm}{', 210 - 2*pageMargins, 'mm}{\n');
    }
    res$rnwBit[[currentSet]] <-
      paste0(res$rnwBit[[currentSet]],
             '<< echo=FALSE, results="asis" >>=\n',
             'print(res$associationMatrix[["',
             currentSet,
             '"]], digits=digits, output="LaTeX", pboxWidthMultiplier=pboxWidthMultiplier, pval=pval);\n',
             '@\n');
    if (length(res$associationMatrix[[currentSet]]$variables.cols) < associationMatrixColsLandscape) {
      res$rnwBit[[currentSet]] <-
        paste0(res$rnwBit[[currentSet]],
               '}\n\\end{minipage}\n');
    }
    else {
      res$rnwBit[[currentSet]] <-
        paste0(res$rnwBit[[currentSet]],
               '}\n\\end{landscape}\n');
    }
    
    ### The size of each panel in the scattermatrix depends
    ### on the number of variables - therefore, we need to adjust
    ### the plot sizes to the number of variables. This is mainly
    ### necessary because in ggpairs.print (which you can
    ### view with "getAnywhere('print.ggpairs');"), the
    ### fontsize is fixed at 15.
    ### knitr wants unit for outputsize, no unit for figure draw
    ### size (but this must be specified in inches).
    if (length(res$associationMatrix[[currentSet]]$variables.cols) * scatterPlotBaseSize > 18) {
      figSizeInOutput <- 18;
    }
    else {
      figSizeInOutput <- length(res$associationMatrix[[currentSet]]$variables.cols) * scatterPlotBaseSize;
    }
    ### For two sets on a page (i.e. plots of roughly 9x9 cm),
    ### the labels of the plots have roughly the right size,
    ### so we multiply 9 cm with the number of sets.
    figSizeToDraw <- (9 / 2.54) * length(res$associationMatrix[[currentSet]]$variables.cols);
    ### If figSizeToDraw is smaller than output size, set to output size
    if (figSizeToDraw < (figSizeInOutput / 2.54)) {
      figSizeToDraw <- figSizeInOutput / 2.54;
    }
    ### Add unit to size in output
    figSizeInOutput <- paste0(figSizeInOutput, "cm");
    
    res$rnwBit[[currentSet]] <-
      paste0(res$rnwBit[[currentSet]],
             '\\begin{minipage}{180mm}\n',
             '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=', figSizeToDraw, ', fig.height=', figSizeToDraw, ', out.width="', figSizeInOutput, '", out.height="', figSizeInOutput, '" >>=\n',
             'print(res$ggpairs.normal[["', currentSet, '"]]);\n',
             '@\n',
             '<< echo=FALSE, results="asis" >>=\n',
             '@\n',
             '\\end{minipage}%\n');

  }

  ### Combine all pages generated for each scale
  for (currentSet in names(sets)) {
    res$rnwPanels <- paste(res$rnwPanels, res$rnwBit[[currentSet]]);
  }
  
  res$rnw <- c(res$rnw, '\\maketitle\n
GENERATED ON ', date(),'\n
CONTENTS: ', length(names(sets)), ' measures (scales):\n\\tableofcontents');

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

  ### Run second time to generate TOC in PDF
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
  class(res) <- c('variableInspection');
  return(res);
  
}