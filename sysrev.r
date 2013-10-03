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

### trim simply trims spaces from the start and end of a string
trim <- function(str) {
  ### Based on 'trim' in package Gdata by
  ### Gregory R. Warnes <greg at warnes.net> and others
  str <- sub(pattern="^ +", replacement="", x=str)
  str <- sub(pattern=" +$", replacement="", x=str)
  str <- sub(pattern="^\t+", replacement="", x=str)
  str <- sub(pattern="\t+$", replacement="", x=str)
  return(str);
}

sysrev.read.bibtex <- function(filename, encoding="unknown") {
  ### Read lines from bibtex file
  sourceLines <- readLines(con = filename, encoding = encoding);
  
  ### Set up variables we'll need
  pos <- 1;
  nrOfRecords <- 0;
  status.readingPreamble <- FALSE;
  status.readingRecord <- FALSE;
  status.currentField <- '';
  res <- list(preamble='',
              records = data.frame());
  
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
      if (grepl("@PREAMBLE\\{", sourceLines[pos])) {
        ### Check whether the preamble is only one line long
        if (grepl("\\}", sourceLines[pos])) {
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
      }
      else if (grepl("@[A-Z]+\\{", sourceLines[pos])) {
        ### We have to start reading a record. Start by
        ### storing the record type
        nrOfRecords <- nrOfRecords + 1;
        res$records[nrOfRecords, 'recordType'] <-
          trim(gsub("@([A-Z]+)\\{.*", "\\1", sourceLines[pos]));
        ### Check for a bibtexkey and store it if we have one
        if (grepl("@[A-Z]+\\{.+,", sourceLines[pos])) {
          res$records[nrOfRecords, 'bibtexkey'] <-
            trim(gsub("@([A-Z]+)\\{(.*),", "\\2", sourceLines[pos]));
        }
        ### Store status so we'll keep on reading the record
        ### on the next lines
        status.readingRecord <- TRUE;
      }
    }
    ### We're reading a record
    else {
      ### Check whether we're finishing a record
      if (sourceLines[pos] == "}") {
        status.readingRecord <- FALSE;
        status.currentField <- '';
      }
      else {
        ### First check whether this line contains a field name and the complete contents
        if (grepl("([A-Za-z0-9_-]+) = \\{(.+)\\},?", sourceLines[pos])) {
          status.currentField <- trim(gsub("([A-Za-z0-9_-]+) = \\{(.+)\\},?", "\\1", sourceLines[pos]));
          res$records[nrOfRecords, status.currentField] <-
            trim(gsub("([A-Za-z0-9_-]+) = \\{(.+)\\},?", "\\2", sourceLines[pos]));
        }
        ### Annoyingly enough, JabRef doesn't use curly braces around months
        else if (grepl("month = (.+),", sourceLines[pos])) {
          res$records[nrOfRecords, 'month'] <-
            trim(gsub("month = (.+),", "\\1", sourceLines[pos]));
        }
        ### If not, check whether this line contains a
        ### field name and the start of the contents
        else if (grepl("([A-Za-z0-9_-]+) = \\{(.+)", sourceLines[pos])) {
          status.currentField <- trim(gsub("([A-Za-z0-9_-]+) = \\{(.+)", "\\1", sourceLines[pos]));
          res$records[nrOfRecords, status.currentField] <-
            trim(gsub("([A-Za-z0-9_-]+) = \\{(.+)", "\\2", sourceLines[pos]));
        }
        ### If not, check whether this line contains additional
        ### contents for whichever field we're reading now,
        ### and ends the reading of this field (i.e. closes it)
        else if (grepl("(.*)\\},?", sourceLines[pos])) {
          res$records[nrOfRecords, status.currentField] <-
            paste(res$records[nrOfRecords, status.currentField],
                   trim(gsub("(.*)\\},?", "\\1", sourceLines[pos])));
          status.currentField <- '';
        }
        ### This line just contains additional contents
        ### for whichever field we're reading now
        else {
          res$records[nrOfRecords, status.currentField] <-
            paste(res$records[nrOfRecords, status.currentField],
                   trim(sourceLines[pos]));
        }
      }
    }
    pos <- pos + 1;
  }
  
  class(res) <- 'BibTeX Library';
  return(res);
  
}

sysrev.export <- function (libraryObject, filename, drop=NULL, keep=NULL,
                           sep="\t", row.names = FALSE, ...) {
  if (FALSE %in% (drop %in% names(libraryObject$records))) {
    warning("The following columns (fields) to drop do not exist in the reference library: ",
            paste(drop[!(drop %in% names(libraryObject$records))],
                  collapse=", "));
  }
  if (FALSE %in% (keep %in% names(libraryObject$records))) {
    warning("The following columns (fields) to keep do not exist in the reference library: ",
            paste(keep[!(keep %in% names(libraryObject$records))],
                  collapse=", "));
  }
  if (!is.null(drop) & !(is.null(keep))) {
    stop("Cannot both keep and drop columns (fields)! When you specify columns (fields) to ",
         "drop, all other fields will be kept; when you specify columns (fields) to keep, ",
         "all other fields will be dropped.");
  }
  if(!is.null(drop)) {
    ### Write records to file, dropping columns (fields) to drop
    write.table(libraryObject$records[ , setdiff(names(libraryObject$records), drop)],
                filename, sep = sep, row.names = row.names, ...);
  }
  if(!is.null(keep)) {
    ### Write records to file, dropping columns (fields) to drop
    write.table(libraryObject$records[ , keep],
                filename, sep = sep, row.names = row.names, ...);
  }
}

