###########################################################
###########################################################
###
### Function to generate a PDF with four panels per page,
### showing some basic item characteristics.
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
itemInspection <- function(dat, items, pdfLaTexPath, filename="itemInspection", digits=4) {
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
  ###                  folders, so in this example, pdfLaTexPath should be
  ###                  'C:/Program Files/MikTex/miktex/bin'
  ###
  ###                In MacOS, you can install MacTex from http://tug.org/mactex/
  ###                  By default, pdflatex ends up in folder '/user/texbin', which
  ###                  is what pdfLaTexPath should be in that default case.
  ###
  ###                In Ubuntu, you can install TexLive base by using your package
  ###                  manager to install texlive-latex-base, or using the terminal:
  ###                  'sudo apt-get install texlive-latex-base'
  ###                  In ubuntu, by default pdflatex ends un in folder '/usr/bin',
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
  
  res$describe <- list();
  res$plot <- list();
  res$rnwBit <- list();
  res$normality.sampleDist <- list();
  res$normality.samplingDist <- list();
  
  res$rnw <- "\\documentclass[a4paper,landscape,11pt]{article}

% For adjusting margins
\\usepackage[margin=15mm]{geometry}

% !Rnw weave = knitr

\\title{Chatbot Study Item Inspection}
\\author{Gjalt-Jorn Peters}
\\begin{document}
\\raggedright
\\noindent
";  
  
  ### Process items per scale
  for (currentScale in names(items)) {
    ### Create lists for each scale to store results
    res$describe[[currentScale]] <- list();
    res$plot[[currentScale]] <- list();
    res$rnwBit[[currentScale]] <- list();
    res$normality.sampleDist[[currentScale]] <- list();
    res$normality.samplingDist[[currentScale]] <- list();
    ### Process items one by one
    for (currentItem in items[[currentScale]]) {
      ### Extract univariate descriptives to show
      res$describe[[currentScale]][[currentItem]] <-
        describe(dat[, currentItem])[, c('n', 'mean', 'sd', 'median', 'min', 'max', 'skew', 'kurtosis')];
      ### Generate and store plots for assessment of normality
      res$plot[[currentScale]][[currentItem]] <-
        normalityAssessment(dat[, currentItem],
                            xLabel.sampleDist = 'Sample Distribution',
                            yLabel.sampleDist = 'Density',
                            xLabel.samplingDist = 'Sampling Distribution',
                            yLabel.samplingDist = 'Density',
                            samplingDistLineSize = .5,
                            normalLineSize = .2);
      ### Create dataframe with normality statistics for the
      ### sample distribution
      res$normality.sampleDist[[currentScale]][[currentItem]] <-
        data.frame(sw = c(round(res$plot[[currentScale]][[currentItem]]$sw.sampleDist$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$sw.sampleDist$p.value, 4)),
                   ad = c(round(res$plot[[currentScale]][[currentItem]]$ad.sampleDist@test$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$ad.sampleDist@test$p.value, 4)),
                   ks = c(round(res$plot[[currentScale]][[currentItem]]$ks.sampleDist$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$ks.sampleDist$p.value, 4)));
      row.names(res$normality.sampleDist[[currentScale]][[currentItem]]) <- c('value', 'p-val');
      ### Create dataframe with normality statistics for the
      ### sampling distribution
      res$normality.samplingDist[[currentScale]][[currentItem]] <-
        data.frame(sw = c(round(res$plot[[currentScale]][[currentItem]]$sw.samplingDist$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$sw.samplingDist$p.value, 4)),
                   ad = c(round(res$plot[[currentScale]][[currentItem]]$ad.samplingDist@test$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$ad.samplingDist@test$p.value, 4)),
                   ks = c(round(res$plot[[currentScale]][[currentItem]]$ks.samplingDist$statistic, 4),
                          round(res$plot[[currentScale]][[currentItem]]$ks.samplingDist$p.value, 4)));
      row.names(res$normality.samplingDist[[currentScale]][[currentItem]]) <- c('value', 'p-val');
      ### Generate the minipage for this item, consisting of:
      ###  - name of measure (scale)
      ###  - name of measurement (item)
      ###  - table with univariate descriptives
      ###  - minipage with:
      ###     - plotted sample distribution
      ###     - normality statistics for sample distribution
      ###  - minipage with:
      ###     - plotted sampling distribution
      ###     - normality statistics for sampling distribution
      res$rnwBit[[currentScale]][[currentItem]] <-
        paste0('\\begin{minipage}[t][90mm][t]{133.5mm}\n',
               'MEASURE: ',
               sanitizeLatexS(currentScale),
               '\n\\newline\nMEASUREMENT: ',
               sanitizeLatexS(currentItem),
               '\n\\newline\n',
               '<< echo=FALSE, results="asis" >>=\n',
               '  print(xtable(res$describe[["',
               currentScale, '"]][["', currentItem,
               '"]], digits=c(0, 0, rep(digits, 7))), tabular.environment="tabular",
               print.rownames=FALSE, floating=FALSE);\n',
               '@\n',
               '\\begin{minipage}[t][50mm][t]{60mm}\n',
               '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=6/2.54, fig.height=5/2.54 >>=\n',
               'res$plot[["', currentScale, '"]][["', currentItem, '"]]$plot.sampleDist;\n',
               '@\n',
               '<< echo=FALSE, results="asis" >>=\n',
               '  print(xtable(res$normality.sampleDist[["',
               currentScale, '"]][["', currentItem,
               '"]], digits=digits), tabular.environment="tabular",
               floating=FALSE);\n',
               '@\n',
               '\\end{minipage}%\n',
               '\\begin{minipage}[t][50mm][t]{60mm}\n',
               '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=6/2.54, fig.height=5/2.54 >>=\n',
               'res$plot[["', currentScale, '"]][["', currentItem, '"]]$plot.samplingDist;\n',
               '@\n',
               '<< echo=FALSE, results="asis" >>=\n',
               '  print(xtable(res$normality.samplingDist[["',
               currentScale, '"]][["', currentItem,
               '"]], digits=digits), tabular.environment="tabular",
               floating=FALSE);\n',
               '@\n',
               '\\end{minipage}%\n\\\\\n',
               '\\end{minipage}');
      
    }
  }
  
  ### Combine all minipages into one character vector.
  ### Every two minipages, go to the next line;
  ### every four minipages, go to the next page
  panelCounter <- 0;
  for (currentScale in names(items)) {
    for (currentItem in chatbotScales[[currentScale]]) {
      panelCounter <- panelCounter + 1;
      res$rnwPanels <- paste0(res$rnwPanels,
                        res$rnwBit[[currentScale]][[currentItem]]);
      if (panelCounter %% 2 == 0) {
        res$rnwPanels <- paste0(res$rnwPanels,
                          '\n');
      }
      if (panelCounter %% 4 == 0) {
        res$rnwPanels <- paste0(res$rnwPanels,
                          '\n\\newpage');
      }
    }
  }
  
  res$rnw <- c(res$rnw, '\\section{Item Inspection}
GENERATED ON ', date(),'\n
CONTENTS: ', panelCounter, ' panels (measurements/items) in ', length(names(items)), ' measures (scales).\n
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