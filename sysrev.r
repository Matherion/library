###########################################################
###########################################################
###
### Collection of function to read and process bibtex
### files - useful for systematic reviews (and, therefore,
### for meta-analyses)
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

sysrev.read.bibtex <- function(filename, encoding="unknown") {
  ### Read lines from bibtex file
  sourceLines <- readLines(con = filename, encoding = encoding);
  
  ### Set up variables we'll need
  pos <- 1;
  nrOfRecords <- 0;
  status.readingPreamble <- FALSE;
  status.readingRecord <- FALSE;
  res <- list();
  res$records <- list();
  
  ### Loop through each line in the file
  while(pos <= length(sourceLines)) {
    ### Check whether we're reading the preable
    if (status.readingPreamble) {
      ### Check whether we should finish reading
      ### the preamble
      if (grepl("}", sourceLines[pos])) {
        ### There is a closing brace, so just store the line
        ### up until the closing brace
        res$preamble <-
          paste0(res$preamble,
                 substr(sourceLines[pos], 1, regexpr("}", sourceLines[pos])[1] - 1));
        ### And stop reading the preamble on the next lines
        status.readingPreamble <- FALSE;
      }
      ### No closing brace on this line, so just add the line to the preamble
      else {
        res$preamble <- paste0(res$preamble, sourceLines[pos]);
      }
    }
    ### We're not readin a preamble, so check
    ### whether we're reading a record
    else if (!status.readingRecord) {
      ### We're not reading a record, so
      ### check whether we should start
      ### reading a preamble or a record
      if (grepl("@PREAMBLE{", sourceLines[pos])) {
        ### Check whether the preamble is only one line long
        if (grepl("}", sourceLines[pos])) {
          ### There is a closing brace, so just store the line
          ### up until the closing brace and we're done
          res$preamble <- substr(sourceLines[pos], 1, regexpr("}", sourceLines[pos])[1] - 1);
        }
        else {
          ### No closing brace, so keep on reading
          ### the preamble on the next line
          status.readingPreamble <- TRUE;
          ### And store this line in the preamble
          res$preamble <- sourceLines[pos];
        }
      }"/@([A-Z]+)\{/"
      else if (grepl("@[A-Z]+{", sourceLines[pos])) {
        ### We have to start reading a record. Start by
        ### storing the record type
        nrOfRecords <- nrOfRecords + 1;
        res$records[[nrOfRecords]] <-
          list(recordType = gsub("@([A-Z]+){", "\\1", sourceLines[pos]));
        ### Check for a bibtexkey and store it if we have one
        if (grepl("@[A-Z]+{.+,", sourceLines[pos])) {
          res$records[[nrOfRecords]]$bibtexkey <-
            gsub("@([A-Z]+){(.*),", "\\1", sourceLines[pos]);
        }
        ### Store status so we'll keep on reading the record
        ### on the next lines
        status.readingRecord <- TRUE;
      }
    }
    ### We're reading a record
    else {
      
    }
    pos <- pos + 1;
  }
  
  class(res) <- 'BibTeX Library';
  return(res);
  
}