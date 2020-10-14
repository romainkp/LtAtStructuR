utils::globalVariables(c("private"))

`%+%` <- function(a, b) paste0(a, b)

readOnly <- function(privateFieldName) {
  res <- function(value) {
    if (missing(value)) {
      private$privateFieldName
    } else {
      stop(gsub(".", "", privateFieldName, fixed = TRUE), " is read only",
        call. = FALSE
      )
    }
  }
  return(pryr::unenclose(res))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Data Storage Class for cohort dataset
#'
#'
#' Class that defines the standard format for the cohort dataset, i.e. the table
#' that specifies for each subject in the cohort:
#' 1) a unique subject identifier,
#' 2) the date of study entry,
#' 3) the date of end of follow-up,
#' 4) the reason for end of follow-up (failure or right-censoring), and
#' 5) baseline measurements of time-dependent or time-independent covariates.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom pryr unenclose
#' @importFrom data.table is.data.table copy setkeyv
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that is.string
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{cohortData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the input cohort dataset to be wrapped in and processed.
#'           The table must contain a single row for each subject in the cohort.
#'           Cannot have columns named 'IDvar', 'index_date', 'EOF_date',
#'           'EOF_type', or 'L0'.
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{index_date}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the date of study entry.
#'     }
#'     \item{\code{EOF_date}:}{\code{character} providing the name of the column
#'           of \code{data} that contains the date of end of follow-up. All observations
#'           with the end of follow-up date equal to the study entry date will be ignored
#'           (i.e., excluded from the cohort).
#'     }
#'     \item{\code{EOF_type}:}{\code{character} providing the name of the column
#'           of \code{data} corresponding to the reason for end of follow-up.
#'     }
#'     \item{\code{Y_name}:}{\code{character} or \code{integer}  providing the
#'           unique value in column \code{EOF_type} that encodes the end of
#'           follow-up due to failure (i.e., occurrence of the outcome event of
#'           interest).
#'     }
#'     \item{\code{L0}:}{vector of \code{character} providing the names of the
#'           columns of \code{data} that contain baseline covariate
#'           measurements. Covariate values must be encoded by a character or
#'           numeric vector (e.g., factors are not allowed).
#'     }
#'     \item{\code{L0_timeIndep}:}{named list specifying, for each time-independent
#'           covariates in \code{L0}, a sublist with only the following three named
#'           elements:
#'           \enumerate{
#'           \item \code{categorical}: \code{logical} indicating
#'           whether the time-independent covariate is
#'           continuous ('FALSE') or categorical ('TRUE'). Cannot be missing.
#'           \item \code{impute}: \code{character} specifying the imputation
#'            method for
#'            missing measurements of the time-independent covariate. Possible values are
#'            'default', 'mean', 'mode', 'median'.
#'            If missing, imputation with
#'            the 'mean' and 'mode' is used for continuous and categorical covariates,
#'            respectively. Imputation with 'mean', 'mode', or 'median' is based on
#'            measurements in \code{data} from subjects with observed covariate values.
#'            'mean' and 'median' can only be used for continuous covariates.
#'            'mode' can only be used for categorical covariates. Imputation with 'default'
#'            replaces missing values with 0 if the covariate is numeric and with 'Unknown'
#'            otherwise. 
#'           \item \code{impute_default_level}: \code{character} or 
#'            \code{numeric} specifying the
#'            imputation value to be used when \code{impute}='default'. The value must be
#'            a length 1 \code{character} (resp. \code{numeric}) for a covariate encoded by a
#'            \code{character} (resp. \code{numeric}) vector.
#'            If missing, the default
#'            values 0 and 'Unknown' are used for numeric and character covariates,
#'            respectively.
#'           }
#'        Each element of the list \code{L0_timeIndep} must be named with the time-independent
#'        covariate in \code{L0} to which the sublist information applies. \code{L0_timeIndep}
#'        can be missing if there is no time-independent covariate in \code{data}.
#'     }
#' }

cohortData <- R6::R6Class(
  classname = "cohortData",
  private = list(
    .data = NA,
    .IDvar = NA,
    .index_date = NA,
    .EOF_date = NA,
    .EOF_type = NA,
    .Y_name = NA,
    .L0 = NA,
    .L0_timeIndep = NA,
    .IDs_ignore = NA
  ),
  active = list(
    data = readOnly(".data"),
    IDvar = readOnly(".IDvar"),
    index_date = readOnly(".index_date"),
    EOF_date = readOnly(".EOF_date"),
    EOF_type = readOnly(".EOF_type"),
    Y_name = readOnly(".Y_name"),
    L0 = readOnly(".L0"),
    L0_timeIndep = readOnly(".L0_timeIndep"),
    IDs_ignore = readOnly(".IDs_ignore")
  ),
  public = list(
    initialize = function(data, IDvar, index_date, EOF_date, EOF_type, Y_name,
                              L0, L0_timeIndep) {

      ## Check valid argument types and make copy of data to avoid overwritting
      ## user data
      assertthat::assert_that(data.table::is.data.table(data),
        msg = "data is not a data.table object."
      )
      dataDT <- data.table::copy(data)

      assertthat::assert_that(is.string(IDvar))
      assertthat::assert_that(is.string(index_date))
      assertthat::assert_that(is.string(EOF_date))
      assertthat::assert_that(is.string(EOF_type))
      assertthat::assert_that(is.string(Y_name) |
        (is.integer(Y_name) & length(Y_name) == 1),
      msg = paste(
        "Y_name is neither a length 1",
        "character nor a length 1 integer."
      )
      )
      assertthat::assert_that(is.character(L0))
      ## Check table contains all columns referenced
      assertthat::assert_that(all(c(IDvar, index_date, EOF_date, EOF_type) %in%
        names(dataDT)))
      assertthat::assert_that(all(L0 %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assertthat::assert_that(!any(c(
        "IDvar", "index_date", "EOF_date",
        "EOF_type", "L0"
      ) %in% names(dataDT)),
      msg = paste(
        "Columns of data cannot be named",
        "'IDvar', 'index_date', 'EOF_date',",
        "'EOF_type', or 'L0'."
      )
      )
      ## delete rows with missing IDvar, index date, eof date, eof type
      ignoredRow <- dataDT[, which(is.na(get(IDvar)) | is.na(get(index_date)) |
        is.na(get(EOF_date)) | is.na(get(EOF_type)))]
      if (length(ignoredRow) > 0) {
        dataDT <- dataDT[-ignoredRow, ]
        warning(
          paste(
            "Ignoring the following row(s) in data due to missing",
            "values in IDvar, index_date, EOF_date, or EOF_type:"
          ),
          paste(ignoredRow, collapse = ","), "."
        )
      }

      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c(
          "factor", "numeric", "integer",
          "character"
        ),
        msg = paste(
          "Class of IDvar column in data is not valid:",
          "unique subject identifiers must be encoded",
          "by a factor, numeric, integer, or character",
          "vector."
        )
        )
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass,
          " to character\n",
          call. = FALSE
        )
      }
      ## Check one row per ID requirement
      assertthat::assert_that(dataDT[, length(unique(get(IDvar)))] ==
        dataDT[, length(get(IDvar))],
      msg = "Duplicate values in IDvar are not allowed."
      )

      assertthat::assert_that(lubridate::is.Date(dataDT[, get(index_date)]),
        msg = paste(
          "Class of index_date in data is not",
          "valid: dates of study entry must be",
          "encoded by a date vector."
        )
      )
      assertthat::assert_that(lubridate::is.Date(dataDT[, get(EOF_date)]),
        msg = paste(
          "Class of EOF_date in data is not",
          "valid: dates of end of follow-up",
          "must be encoded by a date vector."
        )
      )
      assertthat::assert_that(all(dataDT[, get(index_date)] <=
        dataDT[, get(EOF_date)]),
      msg = paste(
        "Each value in the index_date column",
        "of data must be lower than or equal",
        "to that in the same row of the",
        "EOF_date colummn of data."
      )
      )
      ignoredObs <- dataDT[, which(get(index_date) == get(EOF_date))]
      if (length(ignoredObs) > 0) {
        IDs_ignore <- as.vector(dataDT[ignoredObs,get(IDvar)])
        dataDT <- dataDT[-ignoredObs, ]
        warning(paste("Removing", length(ignoredObs), "observations because end of follow-up date is equal to study entry date"))
      } else IDs_ignore <- NA
      assertthat::assert_that(class(dataDT[, get(EOF_type)]) %in% c("character", "integer"),
        msg = paste(
          "Class of EOF_type column in data is",
          "not valid: reasons for end of",
          "follow-up must be encoded by a",
          "character or integer vector."
        )
      )
      assertthat::assert_that(Y_name %in% dataDT[, unique(get(EOF_type))],
        msg = paste(
          "Y_name must be a value of the",
          "EOF_type column of data."
        )
      )
      for(L0.i in L0)assert_that(class(dataDT[, get(L0.i)]) %in% c("numeric", "character"), msg = "Class of "%+%L0.i%+%" in data is not valid: covariate values must be encoded by a numeric or character vector.")

      ## Validate L0_timeIndep
      if(noNA(L0_timeIndep)){
          assertthat::assert_that(all(names(L0_timeIndep)%in%L0))
          for(L0.i in seq_along(L0_timeIndep)){
              assert_that( identical( sort(names(L0_timeIndep[[L0.i]])) , sort(c("categorical","impute","impute_default_level")) ) ,msg = "The sublist of L0_timeIndep for "%+%names(L0_timeIndep)[L0.i]%+%" must contain exactly three elements wih names 'categorical', 'impute, and 'impute_default_level'.")
              assert_that(noNA(L0_timeIndep[[L0.i]]$categorical), msg = "categorical must be set to TRUE or FALSE for "%+%names(L0_timeIndep)[L0.i])
              assert_that(is.flag(L0_timeIndep[[L0.i]]$categorical), msg = "categorical must be a logical value for "%+%names(L0_timeIndep)[L0.i])
              if (noNA(L0_timeIndep[[L0.i]]$impute)) {
                  assert_that(is.string(L0_timeIndep[[L0.i]]$impute), msg = "impute must be set to a character string for "%+%names(L0_timeIndep)[L0.i])
                  assert_that(L0_timeIndep[[L0.i]]$impute %in% c("default", "mean", "mode", "median"), msg = "impute must be set to 'default', 'mean', 'mode', or 'median' for "%+%names(L0_timeIndep)[L0.i])
              }
              if (noNA(L0_timeIndep[[L0.i]]$impute_default_level)) assert_that(is.string(L0_timeIndep[[L0.i]]$impute_default_level) | (is.numeric(L0_timeIndep[[L0.i]]$impute_default_level) & length(L0_timeIndep[[L0.i]]$impute_default_level) == 1), msg = "impute_default_level is neither a length 1 character nor a length 1 numeric for "%+%names(L0_timeIndep)[L0.i])              
              if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") {
                  assert_that(L0_timeIndep[[L0.i]]$categorical, msg = "a covariate ("%+%names(L0_timeIndep)[L0.i]%+%") encoded by a character vector must be categorical (categorical cannot be set to FALSE).")
              }
              if (is.na(L0_timeIndep[[L0.i]]$impute) ) {
                  if (L0_timeIndep[[L0.i]]$categorical) {
                      L0_timeIndep[[L0.i]]$impute <- "mode"
                  } else {
                      L0_timeIndep[[L0.i]]$impute <- "mean"
                  }
              }
              if (L0_timeIndep[[L0.i]]$categorical) {
                  assert_that(!L0_timeIndep[[L0.i]]$impute %in% c("mean", "median"), msg = "Cannot use imputation with mean or median for a categorical covariate ("%+%names(L0_timeIndep)[L0.i]%+%").")
              } else {
                  assert_that(!L0_timeIndep[[L0.i]]$impute %in% c("mode"), msg = "Cannot use imputation with mode for a continuous covariate ("%+%names(L0_timeIndep)[L0.i]%+%").")
              }
              if (is.na(L0_timeIndep[[L0.i]]$impute_default_level)) {
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") {
                      L0_timeIndep[[L0.i]]$impute_default_level <- "Unknown"
                  } else {
                      L0_timeIndep[[L0.i]]$impute_default_level <- 0
                  }
              }
              if (noNA(L0_timeIndep[[L0.i]]$impute_default_level)) {
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") assert_that(is.character(L0_timeIndep[[L0.i]]$impute_default_level), msg = "impute_level_default specifies a value that is not a string while "%+%names(L0_timeIndep)[L0.i]%+%" points to a character vector.")
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "numeric") assert_that(is.numeric(L0_timeIndep[[L0.i]]$impute_default_level), msg = "impute_level_default specifies a value that is not a real scalar while "%+%names(L0_timeIndep)[L0.i]%+%" points to a numeric vector.")
              }
          }
      }else{
        warning("No time-independent covariates were specified.")
      }

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, index_date, EOF_date, EOF_type, sort(L0)), with = FALSE]
      ## order by ID
      data.table::setkeyv(dataDT, IDvar)
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(
        names(dataDT),
        names(data)
      )]
      if (length(ignoredCol) > 0) {
        warning(
          "Ignoring the following column(s) in data: ",
          paste(ignoredCol, collapse = " "), "."
        )
      }

      private$.data <- dataDT
      private$.IDvar <- IDvar
      private$.index_date <- index_date
      private$.EOF_date <- EOF_date
      private$.EOF_type <- EOF_type
      private$.Y_name <- Y_name
      private$.L0 <- L0
      private$.L0_timeIndep <- L0_timeIndep
      private$.IDs_ignore <- IDs_ignore
    }
  )
)

#' Data Storage Class for exposure dataset
#'
#'
#' Class that defines the standard format for the exposure dataset, i.e. the
#' table that specifies all follow-up time intervals during which a study
#' subject is exposed to an exposure level other than a reference level
#' specified by the analyst. For each episode of exposure to a non-reference
#' level, the table specifies:
#' 1) a unique subject identifier,
#' 2) the exposure episode start date,
#' 3) the exposure episode end date, and
#' 4) the non-reference exposure level (required only when the exposure is not
#'    binary).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom pryr unenclose
#' @importFrom data.table is.data.table copy setkeyv shift
#' @importFrom lubridate is.Date now
#' @importFrom assertthat assert_that is.string noNA
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{expData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the input exposure dataset to be wrapped in and processed.
#'           The table can contain multiple rows per subject. There
#'           should be no row for subjects who are only
#'           exposed to the reference exposure level during follow-up.
#'           Exposure levels must be encoded by a character vector or an integer
#'           vector. There cannot be any overlapping exposure episodes. Cannot
#'           contain missing values. Cannot have columns named 'IDvar',
#'           'start_date', 'end_date', 'exposure', or 'exp_level'.
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{start_date}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the exposure episode start
#'           date.
#'     }
#'     \item{\code{end_date}:}{\code{character} providing the name of the column
#'           of \code{data} that contains the exposure episode end date.
#'     }
#'     \item{\code{exp_level}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the non-reference exposure
#'           level. Can be missing only if \code{exp_ref} is also missing. If
#'           missing, the exposure is assumed to be binary and its reference
#'           level is encoded by 0.
#'    }
#'    \item{\code{exp_ref}:}{\code{character} or \code{integer} identifying the
#'          exposure reference level. Cannot be a value of the exp_level column
#'          of data. Can be missing only if \code{exp_level} is also missing.
#'     }
#'     }

expData <- R6::R6Class(
  classname = "expData",
  private = list(
    .data = NA,
    .IDvar = NA,
    .start_date = NA,
    .end_date = NA,
    .exp_level = NA,
    .exp_ref = NA
  ),
  active = list(
    data = readOnly(".data"),
    IDvar = readOnly(".IDvar"),
    start_date = readOnly(".start_date"),
    end_date = readOnly(".end_date"),
    exp_level = readOnly(".exp_level"),
    exp_ref = readOnly(".exp_ref")
  ),
  public = list(
    initialize = function(data, IDvar, start_date, end_date, exp_level = NA,
                              exp_ref = NA) {
      
      ## Check valid argument types and make copy of data to avoid overwritting user data
      assertthat::assert_that(data.table::is.data.table(data),
        msg = "data is not a data.table object."
      )
      dataDT <- data.table::copy(data)

      assert_that(is.string(IDvar))
      assert_that(is.string(start_date))
      assert_that(is.string(end_date))
      if (noNA(exp_level)) {
        assert_that(is.string(exp_level))
      } else { # define a unique column name for exp_level
        expCol <- "expLevel"
        while (expCol %in% names(dataDT))
          expCol <- expCol %+% as.integer(now("UTC")) %+% "UTC"
      }
      if (noNA(exp_ref)) {
        assert_that(is.string(exp_ref) | (is.integer(exp_ref) &
          length(exp_ref) == 1),
        msg = paste(
          "exp_ref is neither a length 1 character nor",
          "a length 1 integer."
        )
        )
      }
      
      ## Handle missing elements
      if (is.na(exp_level) & is.na(exp_ref)) {
        exp_ref <- 0L
        exp_level <- expCol
        dataDT[, eval(exp_level) := 1L]
        warning("exp_level and exp_ref were missing: the reference and non-reference levels for the binary exposure are set to 0 and 1, respectively.")
      } else {
        assert_that(noNA(c(exp_level, exp_ref)), msg = "exp_level cannot be missing if exp_ref is missing and vice versa.")
      }

      ## Check table contains all columns referenced
      assert_that(all(c(IDvar, start_date, end_date, exp_level) %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assert_that(!any(c("IDvar", "start_date", "end_date", "exp_level", "exposure") %in% names(dataDT)), msg = "Columns of data cannot be named 'IDvar', 'start_date', 'end_date', 'exposure', or 'exp_level'.")
        
      ## Check table content
      assert_that(noNA(dataDT), msg = "data contains missing values.")
      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c("factor", "numeric", "integer", "character"), msg = "Class of IDvar column in data is not valid: unique subject identifiers must be encoded by a factor, numeric, integer, or character vector.")
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass, " to character\n", call. = FALSE)
      }
      assert_that(lubridate::is.Date(dataDT[, get(start_date)]), msg = "Class of start_date in data is not valid: start dates of exposure episodes must be encoded by a date vector.")
      assert_that(lubridate::is.Date(dataDT[, get(end_date)]), msg = "Class of end_date in data is not valid: end dates of exposure episodes must be encoded by a date vector.")
      assert_that(all(dataDT[, get(start_date)] <= dataDT[, get(end_date)]), msg = "The start date of an exposure episode must precede or equal its end date. Each value in the start_date column of data must be lower than or equal to that in the same row of the end_date column of data.")
      assert_that(class(dataDT[, get(exp_level)]) %in% c("character", "integer"), msg = "Class of exp_level column in data is not valid: non-reference exposure levels must be encoded by a character or integer vector.")
      ## consistency between the class and values of the ref and nonRef exposure levels
      if (is.integer(dataDT[, get(exp_level)])) assert_that(is.integer(exp_ref), msg = "exp_ref must be an integer when the column exp_level of data is an integer vector.")
      if (is.character(dataDT[, get(exp_level)])) assert_that(is.string(exp_ref), msg = "exp_ref must be a string (a length one character vector) when the column exp_level of data is a character vector.")
      assert_that(!exp_ref %in% dataDT[, unique(get(exp_level))], msg = "exp_ref cannot be a value of the exp_level column of data.")

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, start_date, end_date, exp_level), with = FALSE]
      ## order by IDvar and start date
      data.table::setkeyv(dataDT, c(IDvar, start_date))
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(names(dataDT), names(data))]
      if (length(ignoredCol) > 0) warning("Ignoring the following column(s) in data: ", paste(ignoredCol, collapse = " "), ".")

      ## check no overlapping columns - works only because we orderd by ID and start date above
      end_date_lag <- end_date %+% "_lag"
      dataDT[, eval(end_date_lag) := shift(.SD, n = 1L, fill = NA, type = "lag"), by = eval(IDvar), .SDcols = eval(end_date)]
      assert_that(dataDT[!is.na(get(end_date_lag)), all(get(start_date) > get(end_date_lag))], msg = "Overlapping exposure episodes are not allowed.")
      dataDT[, eval(end_date_lag) := NULL]

      private$.data <- dataDT
      private$.IDvar <- IDvar
      private$.start_date <- start_date
      private$.end_date <- end_date
      private$.exp_level <- exp_level
      private$.exp_ref <- exp_ref
    },
    checkAgainst = function(otherData) {
      if ("cohortData" %in% class(otherData)) {
        assert_that(private$.IDvar == otherData$IDvar, msg = "exposure and cohort datasets must use the same column name to store unique subject identifiers.")
        assert_that(all(private$.data[!get(private$.IDvar)%in%otherData$IDs_ignore, get(private$.IDvar)] %in% otherData$data[, get(otherData$IDvar)]), msg = "Subject identifiers in the exposure dataset must also be found in the cohort dataset.")
        ## Identify exposure rows that need to be modified or removed (index/eof date):
        private$.data <- merge(private$.data, otherData$data[, c(otherData$IDvar, otherData$index_date, otherData$EOF_date), with = FALSE], by = private$.IDvar, all.x = TRUE, all.y = FALSE)
        expEpisode.ignored <- private$.data[get(private$.end_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) | (get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)) | (get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), .N]
        if (expEpisode.ignored > 0) {
          warning("Found at least one exposure episode that indicates exposed days before a subject's index date or after a subject's end of follow-up. Information on such exposed days will be ignored.")
          ## remove episodes with end dates strictly before index dates
          private$.data <- private$.data[get(private$.end_date) >= get(otherData$index_date), ]
          ## remove episodes with start dates strictly after EOF dates
          private$.data <- private$.data[get(private$.start_date) <= get(otherData$EOF_date), ]
          ## remove days prior to index date from episodes that straddle index date
          private$.data[(get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)), eval(private$.start_date) := get(otherData$index_date)]
          ## remove days after EOF date from episodes that straddle EOF date
          private$.data[(get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), eval(private$.end_date) := get(otherData$EOF_date)]

          assert_that(private$.data[get(private$.end_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) | (get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)) | (get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), .N] == 0)
          data.table::setkeyv(private$.data, c(private$.IDvar, private$.start_date))
        }
        ## remove columns added from merge
        private$.data[, ":="(c(otherData$index_date, otherData$EOF_date), vector("list", 2))]
      }
      invisible(self)
    }
  )
)


#' Data Storage Class for a time-dependent covariate dataset
#'
#'
#' Class that defines the standard format for a dataset that encodes all follow-up measurements for
#' a single time-dependent covariate, i.e. the table that specifies for each subject in the
#' cohort: 1) a unique subject identifier,  2) the date of the covariate measurement, 3) the
#' covariate value.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom pryr unenclose
#' @importFrom data.table is.data.table copy setkeyv shift
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that is.string noNA is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{timeDepCovData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           an input covariate dataset to be wrapped in and processed.
#'           The table can contain multiple rows per subject. There
#'           should be no row for subjects with no follow-up covariate measurements. The table
#'           must contain at most one covariate measurement on a given date for any given subject.
#'           Cannot contain missing values. Covariate values must be encoded by a character or
#'           numeric vector (e.g., factors are not allowed).
#'           Cannot have columns named 'IDvar', 'L_date', or 'L_name'.
#'     }
#'     \item{\code{type}:}{\code{character} specifying the covariate type: 'binary monotone
#'           increasing' (e.g., history of a diagnosis or procedure), 'interval' (e.g., hospital
#'           stay or prescription coverage), 'sporadic' (e.g., laboratory measurements),
#'           'indicator' (e.g., occurrence of a repeatable event).
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{L_date}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the date of the follow-up covariate measurement.
#'     }
#'     \item{\code{L_name}:}{\code{character} providing the name of
#'          the column of \code{data} that contains the covariate values. For all covariate types,
#'          \code{L_name} must also be the name of the column of the cohort dataset that contains
#'          the baseline measurements of the time-dependent covariate. Baseline measurements of a
#'          covariate of type 'binary monotone increasing' can only be encoded with values 0 and 1 in
#'          the cohort dataset. All values in the column \code{L_name} of \code{data} must be set
#'          to 1 for a covariate of type 'binary monotone increasing'.
#'          The column \code{L_name} in \code{data} cannot contain
#'          the value 'None' for a covariate of type 'indicator' that is \code{character}.
#'          The column \code{L_name} in \code{data} and in the cohort dataset cannot contain
#'          the value 0 for a covariate of type 'indicator' that is \code{numeric}.
#'    }
#'     \item{\code{categorical}:}{\code{logical} indicating whether the covariate is continuous
#'           ('FALSE') or categorical ('TRUE'). Must be 'TRUE' for a covariate of type 'binary
#'           monotone increasing' or 'indicator'. Cannot be missing.
#'     }
#'     \item{\code{impute}:}{\code{character} specifying imputation method for missing baseline
#'             measurements: 'default', 'mean', 'mode', 'median'. If missing, imputation with
#'             the 'mean' and 'mode' is used for continuous and categorical covariates,
#'             respectively. Imputation with 'mean', 'mode', or 'median' is based on baseline
#'             measurements from subjects with observed baseline covariate values (stored in the
#'             cohort dataset). 'mean' and 'median' can only be used for continuous covariates.
#'             'mode' can only be used for categorical covariates. Imputation with 'default'
#'             replaces missing values with 0 if the covariate is numeric and with 'Unknown'
#'             otherwise. Ignored for a covariate of type 'binary monotone increasing', 'interval',
#'             or 'indicator'.
#'    }
#'    \item{\code{impute_default_level}:}{ \code{character} or \code{numeric} specifying the
#'          imputation value to be used when \code{impute}='default'. The value must be
#'          a length 1 \code{character} (resp. \code{numeric}) for a covariate encoded by a
#'          \code{character} (resp. \code{numeric}) vector.
#'          If missing, the default
#'          values 0 and 'Unknown' are used for numeric and character covariates,
#'          respectively. Ignored for a
#'          covariate of type 'binary monotone increasing', 'interval', or 'indicator'.
#'    }
#'     \item{\code{acute_change}:}{\code{logical} indicating whether a covariate measurement
#'           collected on the date of an exposure change can be impacted by the change.
#'           The default value 'FALSE' indicates that the covariate measurement can be assumed to
#'           have preceded and possibly triggered the change in exposure. Cannot be missing.
#'     }
#'    }

timeDepCovData <- R6::R6Class(
  classname = "timeDepCovData",
  private = list(
    .data = NA,
    .type = NA,
    .IDvar = NA,
    .L_date = NA,
    .L_name = NA,
    .categorical = NA,
    .impute = NA,
    .impute_default_level = NA,
    .acute_change = NA
  ),
  active = list(
    data = readOnly(".data"),
    type = readOnly(".type"),
    IDvar = readOnly(".IDvar"),
    L_date = readOnly(".L_date"),
    L_name = readOnly(".L_name"),
    categorical = readOnly(".categorical"),
    impute = readOnly(".impute"),
    impute_default_level = readOnly(".impute_default_level"),
    acute_change = readOnly(".acute_change")
  ),
  public = list(
    initialize = function(data, type, IDvar, L_date, L_name, categorical, impute = NA, impute_default_level = NA, acute_change = FALSE) {

      ## Check valid argument types and make copy of data to avoid overwritting user data
      assert_that(data.table::is.data.table(data), msg = "data is not a data.table object.")
      dataDT <- data.table::copy(data)

      assert_that(is.string(type))
      assert_that(type %in% c("binary monotone increasing", "interval", "sporadic", "indicator"), msg = "type must be set to 'binary monotone increasing', 'interval', 'sporadic', or 'indicator'.")
      assert_that(is.string(IDvar))
      assert_that(is.string(L_date))
      assert_that(is.string(L_name))
      assert_that(noNA(categorical))
      assert_that(is.flag(categorical))
      if (noNA(impute)) {
        assert_that(is.string(impute))
        assert_that(impute %in% c("default", "mean", "mode", "median"), msg = "impute must be set to 'default', 'mean', 'mode', or 'median'.")
      }
      if (noNA(impute_default_level)) assert_that(is.string(impute_default_level) | (is.numeric(impute_default_level) & length(impute_default_level) == 1), msg = "impute_default_level is neither a length 1 character nor a length 1 numeric.")
      assert_that(noNA(acute_change))
      assert_that(is.flag(acute_change))

      ## Check table contains all columns referenced
      assert_that(all(c(IDvar, L_date, L_name) %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assert_that(!any(c("IDvar", "L_date", "L_name") %in% names(dataDT)), msg = "Columns of data cannot be named 'IDvar', 'L_date', or 'L_name'.")

      ## Check table content
      assert_that(noNA(dataDT), msg = "data contains missing values.")
      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c("factor", "numeric", "integer", "character"), msg = "Class of IDvar column in data is not valid: unique subject identifiers must be encoded by a factor, numeric, integer, or character vector.")
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass, " to character\n", call. = FALSE)
      }
      assert_that(lubridate::is.Date(dataDT[, get(L_date)]), msg = "Class of L_date in data is not valid: dates of covariate measurements must be encoded by a date vector.")
      assert_that(class(dataDT[, get(L_name)]) %in% c("numeric", "character"), msg = "Class of L_name in data is not valid: covariate values must be encoded by a numeric or character vector.")
      if (type == "binary monotone increasing") assert_that(identical(dataDT[, unique(get(L_name))], 1), msg = "For a covariate of type 'binary monotone increasing', the column L_name of data cannot contain values other than 1.")
      if (type == "indicator" & dataDT[, class(get(L_name))] == "character") assert_that(!"None" %in% dataDT[, unique(get(L_name))], msg = "For a covariate with character values of type 'indicator', the column L_name of data cannot contain the value 'None'.")
      if (type == "indicator" & dataDT[, class(get(L_name))] == "numeric") assert_that(!0 %in% dataDT[, unique(get(L_name))], msg = "For a covariate with numeric values of type 'indicator', the column L_name of data cannot contain the value 0.")
      if (type %in% c("binary monotone increasing", "indicator")) assert_that(categorical, msg = "categorical must be set to 'TRUE' for a covariate of type 'binary monotone increasing' or 'indicator'.")
      if (dataDT[, class(get(L_name))] == "character") {
        assert_that(categorical, msg = "a covariate encoded by a character vector must be categorical (categorical cannot be set to FALSE).")
      }

      if (noNA(impute) & type %in% c("binary monotone increasing", "interval", "indicator")) {
        warning("impute was specified for a variable of type 'binary monotone increasing','interval', or 'indicator': argument is ignored.")
        impute <- NA
      }
      if (is.na(impute) & (!type %in% c("binary monotone increasing", "interval", "indicator"))) {
        if (categorical) {
          impute <- "mode"
        } else {
          impute <- "mean"
        }
      }
      if (categorical) {
        assert_that(!impute %in% c("mean", "median"), msg = "Cannot use imputation with mean or median for a categorical covariate.")
      } else {
        assert_that(!impute %in% c("mode"), msg = "Cannot use imputation with mode for a continuous covariate.")
      }

      if (noNA(impute_default_level) & type %in% c("binary monotone increasing", "interval", "indicator")) {
        warning("impute_default_level was specified for a variable of type 'binary monotone increasing','interval', or 'indicator': argument is ignored.")
        impute_default_level <- NA
      }
      if (is.na(impute_default_level) & (!type %in% c("binary monotone increasing", "interval", "indicator"))) {
        if (dataDT[, class(get(L_name))] == "character") {
          impute_default_level <- "Unknown"
        } else {
          impute_default_level <- 0
        }
      }
      if (noNA(impute_default_level)) {
        if (dataDT[, class(get(L_name))] == "character") assert_that(is.character(impute_default_level), msg = "impute_level_default specifies a value that is not a string while L_name points to a character vector.")
        if (dataDT[, class(get(L_name))] == "numeric") assert_that(is.numeric(impute_default_level), msg = "impute_level_default specifies a value that is not a real scalar while L_name points to a numeric vector.")
      }

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, L_date, L_name), with = FALSE]
      ## order by IDvar and L_date
      data.table::setkeyv(dataDT, c(IDvar, L_date))
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(names(dataDT), names(data))]
      if (length(ignoredCol) > 0) warning("Ignoring the following column(s) in data: ", paste(ignoredCol, collapse = " "), ".")

      ## Check that there is no more than one row with the same IDvar and L_date
      assert_that(anyDuplicated(dataDT[, .(get(IDvar), get(L_date))]) == 0, msg = "data must contain at most one covariate measurement on a given date for any given subject.")

      private$.data <- dataDT
      private$.type <- type
      private$.IDvar <- IDvar
      private$.L_date <- L_date
      private$.L_name <- L_name
      private$.categorical <- categorical
      private$.impute <- impute
      private$.impute_default_level <- impute_default_level
      private$.acute_change <- acute_change
    },
    checkAgainst = function(otherData) {
      if ("list" %in% class(otherData) && "timeDepCovData" %in% class(otherData[[1]])) {
        assert_that(!private$.L_name %in% names(otherData), msg = "Time-varying covariates cannot have the same name. Check that the same covariate dataset was not added twice.")
      }

      if ("cohortData" %in% class(otherData)) {
        assert_that(private$.L_name %in% otherData$L0, msg = "The time-dependent covariate " %+% private$.L_name %+% " must have its baseline measurement stored in the cohort dataset.")
        assert_that(!private$.L_name %in% names(otherData$L0_timeIndep), msg = "The time-dependent covariate " %+% private$.L_name %+% " was specified as a time-independent covariate in the cohort dataset.")
        assert_that( class(private$.data[,get(private$.L_name)]) == class(otherData$data[,get(private$.L_name)]) , msg = "The type (numeric or character) of the time-dependent covariate " %+% private$.L_name %+% " does not match the type for its baseline values stored in the cohort dataset")
        assert_that(private$.IDvar == otherData$IDvar, msg = "The cohort dataset and the time-dependent covariate dataset for " %+% private$.L_name %+% " must use the same column name to store unique subject identifiers.")
        assert_that(all(private$.data[!get(private$.IDvar)%in%otherData$IDs_ignore, get(private$.IDvar)] %in% otherData$data[, get(otherData$IDvar)]), msg = "Subject identifiers in the time-dependent covariate dataset for " %+% private$.L_name %+% " must also be found in the cohort dataset.")

        ## Identify covariate rows that need to be modified or removed (index/eof date):
        private$.data <- merge(private$.data, otherData$data[, c(otherData$IDvar, otherData$index_date, otherData$EOF_date), with = FALSE], by = private$.IDvar, all.x = TRUE, all.y = FALSE)
        covVal.ignored <- private$.data[get(private$.L_date) < get(otherData$index_date) | get(private$.L_date) > get(otherData$EOF_date), .N]

        if (covVal.ignored > 0) {
          warning("Found one or more ", private$.L_name, " measurement(s) before a subject's index date or after a subject's end of follow-up. Such measurements will be ignored.")

          if (private$.type %in% c("sporadic", "indicator", "binary monotone increasing")) {
            if (private$.type %in% c("binary monotone increasing")) {
              subject.Lt.before.index <- private$.data[get(private$.L_date) < get(otherData$index_date), unique(get(private$.IDvar))]
              if (any(otherData$data[ get(otherData$IDvar) %in% subject.Lt.before.index, get(private$.L_name)] %in% 0)) warning("Found at least one subject with a measurement of the time-dependent covariate dataset for ", private$.L_name, " (binary monotone increasing) which was collected before the subject's index date while its baseline measurement in the cohort dataset is 0. This may indicate an error in the information encoded for ", private$.L_name, ". Time-dependent measurements before the index date are ignored.")
            }
            ## remove measurements with dates strictly before index dates
            private$.data <- private$.data[get(private$.L_date) >= get(otherData$index_date), ]
            ## remove measurements with dates strictly after EOF dates
            private$.data <- private$.data[get(private$.L_date) <= get(otherData$EOF_date), ]
          }

          if (private$.type %in% c("interval")) {
            Lt.before.or.at.index <- private$.data[get(private$.L_date) <= get(otherData$index_date), ]
            Lt.keep <- Lt.before.or.at.index[, closest.before.index := as.numeric(get(private$.L_date) == max(get(private$.L_date))), by = c(private$.IDvar)][closest.before.index == 1, ]
            Lt.keep[, eval(private$.L_date) := get(otherData$index_date)][, closest.before.index := NULL]
            ## remove Lts before/on index and bring back Lts closest before or on index
            assert_that(identical(names(private$.data), names(Lt.keep)))
            private$.data <- rbind(private$.data[get(private$.L_date) > get(otherData$index_date), ], Lt.keep)
            ## remove measurements with dates strictly after EOF dates
            private$.data <- private$.data[get(private$.L_date) <= get(otherData$EOF_date), ]
          }

          assert_that(private$.data[get(private$.L_date) < get(otherData$index_date) | get(private$.L_date) > get(otherData$EOF_date), .N] == 0)
          data.table::setkeyv(private$.data, c(private$.IDvar, private$.start_date))
        }
        ## remove columns added from merge
        private$.data[, ":="(c(otherData$index_date, otherData$EOF_date), vector("list", 2))]
      }
      invisible(self)
    }
  )
)

#' Data Storage Class for an LtAt dataset
#'
#'
#' Class that defines the specifications for constructing an LtAt dataset and the dataset itself.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom pryr unenclose
#' @importFrom data.table is.data.table copy setkeyv shift rbindlist setcolorder setorderv data.table
#' @importFrom lubridate is.Date %within% interval as_date int_start int_end ymd interval int_overlaps intersect
#' @importFrom assertthat assert_that is.string noNA is.flag
#' @importFrom future.apply future_lapply
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{LtAtData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the LtAt dataset. NULL by default before construction.
#'     }
#'     \item{\code{cohort_data}:}{\code{cohortData} specifying the cohort dataset.
#'     }
#'     \item{\code{exp_data}:}{\code{expData} specifying the time-varying exposure dataset.
#'           Its \code{IDvar} field must contain the same value as that of the
#'           \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of the exposure and cohort
#'           datasets). The unique subject identifiers in the \code{data} field of \code{exp_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in the exposure dataset must also be present in the cohort dataset.)
#'           An exposure measurement stored in the \code{data} field of \code{exp_data}
#'           will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data} (i.e., only
#'           exposure measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'     \item{\code{cov_data}:}{a list of \code{timeDepCovData} that
#'           specifies time-varying covariate(s). Each element of the list must have a distinct
#'           value stored in its \code{L_name} field and this value must also be found in the
#'           \code{L0} field of \code{cohort_data} (i.e., time-varying covariates cannot have
#'           the same name and must have their baseline measurements stored in the cohort dataset).
#'           The \code{IDvar} field of each element \code{timeDepCovData} of \code{cov_data}
#'           must contain the same value as
#'           that of the \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of each covariate dataset and
#'           that of the cohort dataset). The unique subject identifiers in the \code{data}
#'           field of each element
#'           \code{timeDepCovData} of \code{cov_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in a covariate dataset must also be present in the cohort dataset.)
#'           A covariate measurement stored in the \code{data} field of an element
#'           \code{timeDepCovData}
#'           of \code{cov_data} will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data}. (i.e., only
#'           covariate measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'    }

LtAtData <- R6::R6Class(
  classname = "LtAtData",
  private = list(
    .data = NA,
    .cohort_data = NA,
    .exp_data = NA,
    .cov_data = NA
  ),
  active = list(
    data = readOnly(".data"),
    cohort_data = readOnly(".cohort_data"),
    exp_data = readOnly(".exp_data"),
    cov_data = readOnly(".cov_data")
  ),
  public = list(
    initialize = function() {
      private$.data <- NA
      private$.cohort_data <- NA
      private$.exp_data <- NA
      private$.cov_data <- NA
      invisible(self) # critical to allow method chaining
    },
    addSpec = function(spec) {
      assertthat::assert_that(any(class(spec) %in%
        c("cohortData", "expData", "timeDepCovData")))
      if ("cohortData" %in% class(spec)) {
        assertthat::assert_that(!"cohortData" %in% class(private$.cohort_data),
          msg = paste(
            "cohort_data field already defined",
            "- will not overwrite it."
          )
        )
        if ("expData" %in% class(private$.exp_data)) {
          private$.exp_data <- private$.exp_data$checkAgainst(spec)
        }
        if ("list" %in% class(private$.cov_data) && "timeDepCovData" %in%
          class(private$.cov_data[[1]])) {
          for (cov.i in names(private$.cov_data)) {
            private$.cov_data[[cov.i]] <-
              private$.cov_data[[cov.i]]$checkAgainst(spec)
          }
        }
        private$.cohort_data <- spec
      }
      if ("expData" %in% class(spec)) {
        assertthat::assert_that(!"expData" %in% class(private$.exp_data),
          msg = paste(
            "exp_data field already defined",
            "- will not overwrite it."
          )
        )
        private$.exp_data <- spec$checkAgainst(private$.cohort_data)
      }
      if ("timeDepCovData" %in% class(spec)) {
        if ("timeDepCovData" %in% class(private$.cov_data[[1]])) {
          spec$checkAgainst(private$.cov_data)
          if ("cohortData" %in% class(private$.cohort_data)) {
            spec$checkAgainst(private$.cohort_data)
          }
          spec.list <- list(spec)
          names(spec.list) <- spec$L_name
          spec.list <- c(private$.cov_data, spec.list)
          spec.list <- spec.list[order(names(spec.list))]
          private$.cov_data <- spec.list
        } else {
          if ("cohortData" %in% class(private$.cohort_data)) {
            spec$checkAgainst(private$.cohort_data)
          }
          spec.list <- list(spec)
          names(spec.list) <- spec$L_name
          private$.cov_data <- spec.list
        }
      }
      invisible(self) # critical to allow method chaining
    },
    createIntervals = function(time_unit) {
      ## Confirm that all time-dependent variables were added
      missingTimeDep <-   sort(private$.cohort_data$L0)[!sort(private$.cohort_data$L0)%in%sort(c(names(private$.cohort_data$L0_timeIndep),names(private$.cov_data)))]
      assertthat::assert_that( identical( sort(private$.cohort_data$L0),
                                         sort(c(names(private$.cohort_data$L0_timeIndep),names(private$.cov_data)))
                                         ),
                              msg = "Data construction cannot start until the measurements post index date for all time-dependent covariates included in the cohort dataset are specified. Either correct the value for L0_timeIndep used to specify the cohort dataset or add the time-dependent measurements for the following covariates: "%+%paste(missingTimeDep,collapse=", ")%+%".")
        
      ## Gather relevant elements for long-format construction (for readability)
      IDvar <- private$.cohort_data$IDvar
      index_date <- private$.cohort_data$index_date
      EOF_date <- private$.cohort_data$EOF_date
      EOF_type <- private$.cohort_data$EOF_type
      Y_name <- private$.cohort_data$Y_name
      outData <- private$.cohort_data$data[, c(
        IDvar, index_date, EOF_date,
        EOF_type
      ), with = FALSE]
      ## Construct long-format
      outData[, maxInt := ceiling((julian(get(EOF_date)) -
        julian(get(index_date)) + 1) / time_unit)]

      ## add extra row for outcome
      outData[get(EOF_type) == Y_name, maxInt := maxInt + 1]

      ## duplicate rows based on max f/up by patient
      outData <- outData[rep(1:.N, maxInt)][, intnum := 0:(.N - 1),
        by = eval(IDvar)
      ]
      ## set up start and end date for each consecutive bins of size t_unit
      outData[, intstart := get(index_date) + time_unit * intnum]
      outData[, intend := intstart + time_unit - 1]

      ## add outcome flag and clean-up
      outData[, outcome := 0]
      outData[get(EOF_type) == Y_name & (intnum + 1) == maxInt, outcome := 1]
      assert_that(identical(
        outData[get(EOF_type) == Y_name & (intnum + 1) ==
          (maxInt - 1), all(get(EOF_date) %within%
          interval(intstart, intend))],
        TRUE
      ))

      outData[, `:=`(
        c(index_date, EOF_date, EOF_type, "maxInt"),
        vector("list", 4)
      )]
      ## order by ID and intnum
      setkeyv(outData, c(IDvar, "intnum"))
      ## save long-format data structure
      private$.data <- outData
      ##
      invisible(self)
    },
    assignAC = function(firs_exp_rule = 1, exp_threshold = 0.5) {

      # pointer to data sources for convenience
      exp_data <- private$.exp_data$data
      cohort_data <- private$.cohort_data$data
      outcome_data <- private$.data

      # pointers for exposure data
      start_date <- private$.exp_data$start_date
      end_date <- private$.exp_data$end_date
      exp_level <- private$.exp_data$exp_level
      exp_ref <- private$.exp_data$exp_ref

      # pointers for cohort data
      id_var <- private$.cohort_data$IDvar
      eof_date <- private$.cohort_data$EOF_date
      eof_type <- private$.cohort_data$EOF_type
      y_name <- private$.cohort_data$Y_name

      # loop over individuals
      data_sliced_by_id <-
        #lapply(
        future.apply::future_lapply(
          unique(outcome_data[, get(id_var)]),
          function(id) {
            ## message(paste0(
            ##   "Processing ", id, ": subject ",
            ##   which(unique(outcome_data[, get(id_var)]) %in% id),
            ##   " of ", length(unique(outcome_data[, get(id_var)]))
            ## ))

            # reduce data to just one subject
            out_data_slice <- outcome_data[get(id_var) == id, ]
            data.table::setkeyv(out_data_slice, id_var)

            # join data.tables based on date columns and reduce
            exp_data_this_id <- exp_data[get(id_var) == id, ]
            data.table::setkeyv(exp_data_this_id, id_var)

            # subset cohort data by ID and key by ID for later merging
            cohort_data_this_id <- cohort_data[get(id_var) == id, ]
            data.table::setkeyv(cohort_data_this_id, id_var)

            # merge out_data_slice and cohort data by ID
            out_data_slice <- merge(out_data_slice, cohort_data_this_id,
              by.x = id_var, by.y = id_var
            )

            # unit of time
            unit_time <- out_data_slice[, unique(intend - intstart + 1)]

            if (nrow(exp_data_this_id) > 0) {
              # find exposure dates and lengths
              exp_overlap <- merge(out_data_slice,
                exp_data_this_id,
                allow.cartesian = TRUE
              )
              # define new exposure time for use in 12a/13a (based on Z-days)
              z_bin_int <- exp_overlap[, lubridate::interval(
                get(eof_date) -
                  unit_time + 1,
                get(eof_date)
              )]
              z_exp_int <- exp_overlap[, lubridate::interval(
                get(start_date),
                get(end_date)
              )]
              z_overlap <- lubridate::int_overlaps(z_bin_int, z_exp_int)
              z_exp_overlap <- copy(exp_overlap)
              z_exp_overlap[z_overlap == FALSE, z_exp_time := 0]
              z_exp_overlap[
                z_overlap == TRUE,
                z_exp_time := (pmin(get(eof_date), get(end_date)) -
                  pmax(
                    get(eof_date) - unit_time + 1,
                    get(start_date)
                  ) + 1)
              ]
              
              z_exp_overlap[, z_exp_time_by_unique_exp := .(sum(z_exp_time)), by = .(get(exp_level),intnum)] ### Obtains respective exposure times by intnum and exposure level
              z_exp_overlap[z_overlap == FALSE, z_exp_time_by_unique_exp := 0]  ###  0s out all rows except rows in which patients outcome overlaps with exposure(s)
              z_exp_overlap[, z_exp_time_by_freq_exp := max(z_exp_time_by_unique_exp), by = intnum] ### Obtains most frequent exposure time; will aid in determining a tie amongst exposures
              z_exp_overlap[, z_exp_time := sum(z_exp_time), by = intnum]
              assertthat::assert_that(assertthat::are_equal(z_exp_overlap,z_exp_overlap[order(intnum,get(start_date)),]),
                                      msg = "rows in z_exp_overlap are not ranked by startA")
              z_exp_overlap[, z_tie := ifelse(length(unique(get(exp_level)[z_exp_time_by_unique_exp%in%z_exp_time_by_freq_exp]))>1,1,0), by = intnum] ### Initiating z_tie column; if the length of the exposure vector in which z_exp_time_by_unique_exp (i.e. exposure time by unique exposure) is equal to z_exp_time_by_freq_exp (i.e. exposure time by the most frequent exposure) is greater than 1, then there exists a tie amongst those exposure levels and no tie otherwise
              z_exp_overlap[z_tie == 0, z_max_freq_exp := unique(get(exp_level)[z_exp_time_by_unique_exp%in%z_exp_time_by_freq_exp]), by = intnum] ### If there is no tie at interval t, returns most frequent exposure level in interval t as described by algorithm (i.e. most frequent exposure)
              z_exp_overlap[z_tie == 1, z_max_freq_exp := tail(get(exp_level)[z_exp_time_by_unique_exp%in%z_exp_time_by_freq_exp],1) ,by = intnum] ### Obtains level involved in the tie that the patient experienced last during the interval t
              #### ATTENTION: z_exposure_times will later be used for case 12a's in which z_tie==1
              z_exposure_times <- copy(z_exp_overlap) 
              z_exposure_times <- z_exposure_times[z_exp_time_by_unique_exp%in%z_exp_time_by_freq_exp,]
              z_exposure_times <- z_exposure_times[intnum==intnum[.N],]
              z_exp_overlap <- unique(z_exp_overlap[, mget(c(
                id_var, "intnum",
                "z_exp_time",
                "z_exp_time_by_freq_exp",
                "z_tie",
                "z_max_freq_exp"
              ))])
              # define exposure time for all other cases
              bin_int <- exp_overlap[, lubridate::interval(
                intstart,
                pmin(
                  intend,
                  get(eof_date)
                )
              )]
              exp_int <- exp_overlap[, lubridate::interval(
                get(start_date),
                get(end_date)
              )]
              exp_overlap[, overlap := lubridate::int_overlaps(
                bin_int,
                exp_int
              )]
              exp_overlap <- exp_overlap[overlap == TRUE, ]
              exp_overlap[, exp_intervals := (pmin(intend, get(end_date)) -
                pmax(
                  intstart,
                  get(start_date)
                ) + 1)]

              exp_overlap[, exp_time := sum(exp_intervals), by = intnum]
              exp_overlap[, exp_time_by_unique_exp := .(sum(exp_intervals)), by = .(get(exp_level),intnum)] ### Obtains respective exposure times by intnum and exposure level
              exp_overlap[, exp_time_by_freq_exp := max(exp_time_by_unique_exp), by = intnum] ### Obtains most frequent exposure time; will aid in determining a tie amongst exposures
              assertthat::assert_that(assertthat::are_equal(exp_overlap,exp_overlap[order(intnum,get(start_date)),]),
                                      msg = "rows in exp_overlap are not ranked by startA")
              exp_overlap[, tie := ifelse(length(unique(get(exp_level)[exp_time_by_unique_exp%in%exp_time_by_freq_exp]))>1,1,0), by = intnum] ### Initiating tie column; if the length of the exposure vector in which exp_time_by_unique_exp (i.e. exposure time by unique exposure) is equal to exp_time_by_freq_exp (i.e. exposure time by the most frequent exposure) is greater than 1, then there exists a tie amongst those exposure levels and no tie otherwise
              exp_overlap[tie == 0, max_freq_exp := unique(get(exp_level)[exp_time_by_unique_exp%in%exp_time_by_freq_exp]), by = intnum] ### If there is no tie at interval t, returns most frequent exposure level in interval t as described by the algorithm (i.e. most frequent exposure)
              exp_overlap[tie == 1, max_freq_exp := tail(get(exp_level)[exp_time_by_unique_exp%in%exp_time_by_freq_exp],1), by = intnum] ### Obtains level involved in the tie that the patient experienced last during the interval t
              exp_overlap[, eval(start_date) := NULL]
              exp_overlap[, eval(end_date) := NULL]
              exp_overlap[, exp_intervals := NULL]
              exp_overlap <- unique(exp_overlap)
              data.table::setkey(exp_overlap, NULL)
              data.table::setkey(out_data_slice, NULL)
              
              exp_overlap[, final := ifelse(exp_time_by_unique_exp == exp_time_by_freq_exp, TRUE, FALSE)] ### Keeping rows that with most frequent exposure and/or tied exposures, by intnum 
              exp_overlap[intnum==0, final := ifelse(max_freq_exp == get(exp_level), TRUE, FALSE)] ### If there exists a tie at t==0, keeping row with max frequent exposure
              exp_overlap <- exp_overlap[(final),] 
              exp_overlap[, final := NULL]
              exp_overlap[, A0.warn := 0] ### Initiazting A0.warn
              
              # merging exposure/overlap and baseline data sequentially
              overlap_data <- merge(out_data_slice, exp_overlap, all.x = TRUE)
              overlap_data <- merge(overlap_data, z_exp_overlap,
                by = c(id_var, "intnum"), all.x = TRUE
              )
            } else {
              # NOTE: when unexposed, special handling of lack of exp_data
              overlap_data <- data.table::copy(out_data_slice)
              overlap_data[, eval(exp_level) := NA]
              overlap_data[, overLap := NA]
              overlap_data[, exp_time := difftime(NA, NA, units = "days")]
              overlap_data[, z_exp_time := difftime(NA, NA, units = "days")]
              overlap_data[, z_exp_time_by_freq_exp := difftime(NA, NA, units = "days")]
              overlap_data[, exp_time_by_unique_exp := difftime(NA, NA, units = "days")]
              overlap_data[, exp_time_by_freq_exp := difftime(NA, NA, units = "days")]
              overlap_data[, tie := 0]
              overlap_data[, A0.warn := 0]
            }
            overlap_data[
              is.na(exp_time),
              exp_time := as.difftime(0, units = "days")
            ]
            overlap_data[
              is.na(z_exp_time),
              z_exp_time := as.difftime(0, units = "days")
            ]
            overlap_data[
              is.na(exp_time_by_unique_exp),
              exp_time_by_unique_exp := as.difftime(0, units = "days")
              ]
            overlap_data[
              is.na(tie),
              tie := 0
              ]
            overlap_data[
              is.na(A0.warn),
              A0.warn := 0
              ]
            overlap_data[
              is.na(exp_time_by_freq_exp),
              exp_time_by_freq_exp := as.difftime(0, units = "days")
              ]
            overlap_data[
              is.na(z_exp_time_by_freq_exp),
              z_exp_time_by_freq_exp := as.difftime(0, units = "days")
              ]
            data.table::setkeyv(overlap_data, id_var)

            # initializing exposure and case columns that will be edited below
            overlap_data[, exposure := rep(0, .N)]
            overlap_data[, censor := rep(0, .N)]
            overlap_data[, case := "tmp"]

            # find censoring event by examining cohort data
            overlap_data[, obs_interval :=
              lubridate::interval(
                lubridate::ymd(intstart),
                lubridate::ymd(intend)
              )]
            overlap_data[
              get(eof_type) != y_name,
              censor := as.numeric(lubridate::ymd(get(eof_date))
              %within% obs_interval)
            ]
            overlap_data[, obs_interval := NULL]

            # LOGIC 1 ONLY
            if (firs_exp_rule == 1) {
              # case 1a: create exposure indicator and add to patient-level data
              #overlap_data[exp_time > 0, exposure := 1] OLD
              overlap_data[exp_time_by_freq_exp > 0, exposure := 1]
              if (overlap_data[, sum(exposure)] > 0) {
                #overlap_data[min(which(exposure == 1)), case := "1a"] OLD
                ## ATTENTION: Assigns case 1a to all rows that need be assigned (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                overlap_data[
                  intnum==overlap_data[min(which(exposure == 1)),intnum],
                  case := "1a"]
              }
              overlap_data[, Part1 := !cumsum(shift(exposure,
                n = 1L, fill = 0,
                type = "lag"
              )) > 0]

              overlap_data[case=="1a",Part1:=TRUE] ### Sets Part1=TRUE for all case 1as
              
              # case 2a: no exposure in interval t and not the last interval
              overlap_data[Part1 == TRUE & exposure != 1, case := "2a"]

              # case 3a: failure in interval t and no exposure before failure
              # NOTE: by definition, the failure event should be the last event
              #       SO if this subset exists then case 3a should be assigned
              #       since it means that exposure has occurred before failure
              if (overlap_data[Part1 == TRUE, sum(outcome) > 0]) {
                overlap_data[which(outcome == 1) - 1, `:=`(
                  exposure = 0,
                  censor = 0,
                  outcome = 0,
                  case = "3a"
                )]
                overlap_data[(outcome == 1), `:=`(
                  exposure = NA,
                  censor = NA,
                  case = "3a"
                )]
              }

              # case 4a: right-censoring in interval t, no exposure prior
              if (overlap_data[Part1 == TRUE & exposure == 0, 1 %in% censor]) {
                overlap_data[as.logical(censor), `:=`(
                  outcome = 0, exposure = 0,
                  case = "4a"
                )]
              }

              # case 5a: right-censoring in interval, exposure before censoring
              # NOTE: censoring time should be max time if administrative end
              if (overlap_data[Part1 == TRUE & exposure == 1, 1 %in% censor]) {
                #overlap_data[.N, `:=`(outcome = 0, exposure = 1, case = "5a")] OLD
                ### ATTENTION: Assigns case 5a to all rows that need be assigned (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                overlap_data[censor==1, `:=`(outcome = 0, exposure = 1, case = "5a")] 
              }

              # case 6a: failure in interval and exposure before failure event
              overlap_data[, lagPart1 := shift(Part1,
                n = 1L, fill = TRUE,
                type = "lag"
              )]
              if (overlap_data[Part1 == TRUE, 1 %in% exposure] &
                overlap_data[lagPart1 == TRUE, 1 %in% outcome]) {
                intnum.before.outcome <- overlap_data[which(outcome == 1),intnum-1]
                overlap_data[intnum%in%intnum.before.outcome, `:=`( #which(outcome == 1) - 1
                  exposure = 1,
                  censor = 0,
                  outcome = 0,
                  case = "6a"
                )]
                overlap_data[(outcome == 1), `:=`(
                  exposure = NA,
                  censor = NA,
                  case = "6a"
                )]
              }
              overlap_data[outcome == 1 & lagPart1 == TRUE, Part1 := TRUE]
            }

            # set Part1 flag to FALSE if in LOGIC 2
            if (firs_exp_rule == 0) {
              overlap_data[, Part1 := FALSE]
            }

            # NOTE: PART 2: the following only occur AFTER first exposure t
            # case 8a/1b: t is NOT the last interval and avg exposure during t
            #             is \geq X%, then define j = 1 and A(t) = j
            ### ATTENTION: Line below was modified from "exp_time" to "exp_time_by_freq_exp" to account for proper exposure time by exposure level for categorical outcome; still holds true for binary outcome
            overlap_data[, exp_beyond_threshold := as.numeric(exp_time_by_freq_exp) /
              as.numeric(pmin(intend, get(eof_date)) - intstart + 1) >=
              exp_threshold]
            ### ATTENTION: If the sum of the proportions of days exposed to any possible level is greater than or equal to exp_threshold AND 
            ###            if the proportion of days exposed during interval t to level j < exp_threshold then a warning indicator is set to 1.
            overlap_data[, Awarn_exp_beyond_threshold := as.numeric(exp_time) /
                           as.numeric(pmin(intend, get(eof_date)) - intstart + 1) >=
                           exp_threshold]
            overlap_data[
              #overlap_data[, .I < .N]  & OLD
              ### ATTENTION: TRUE for intnum in which overlap_data[, .I < .N] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
              overlap_data[,intnum < overlap_data[overlap_data[, !.I < .N],intnum]] &
                exp_beyond_threshold & !Part1,
              `:=`(
                exposure = 1,
                outcome = 0,
                censor = 0,
                case = ifelse(firs_exp_rule == 0, "1b", "8a")
              )
            ]

            # case 9a/2b: t is NOT the last interval and avg exposure during t
            #             is less than X%, then define j = 0 and A(t) = j
            overlap_data[
              #overlap_data[, .I < .N] == TRUE & OLD
              ### ATTENTION: TRUE for intnum in which overlap_data[, .I < .N] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
              overlap_data[,intnum < overlap_data[overlap_data[, !.I < .N],intnum]] &
              exp_beyond_threshold == FALSE & !Part1, `:=`(
              exposure = 0,
              outcome = 0,
              censor = 0,
              tie = 0,
              A0.warn = ifelse(Awarn_exp_beyond_threshold, 1, 0),
              case = ifelse(firs_exp_rule == 0, "2b", "9a")
            )]

            # LOGIC 1: cases 10a/11a: t is last interval and a right-censoring
            #                         event occurs during the interval t...
            # LOGIC 2: cases 3b/4b: t is last interval and a right-censoring
            #                       event occurs during the interval t...
            if (overlap_data[.N, 1 %in% censor & Part1 == FALSE]) {
              # case 10a/3b: ...then if avg exposure up to + including time C is
              #              greater than or equal to X%, let j = 1, A(t) = j
              overlap_data[
                #overlap_data[, .I == .N] & OLD
                ### ATTENTION: TRUE for intnum in which overlap_data[, .I == .N] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                  overlap_data[overlap_data[, .I == .N],intnum] == intnum &
                  exp_beyond_threshold == TRUE,
                `:=`(
                  censor = 1, outcome = 0,
                  exposure = 1,
                  case = ifelse(firs_exp_rule == 0, "3b", "10a")
                )
              ]
              ## }
              # case 11a/4b: ...then if avg exposure up to + including time C is
              #              less than X%, let j = 0 and A(t) = j
              overlap_data[
                #overlap_data[, .I == .N] & OLD
                ### ATTENTION: TRUE for intnum in which overlap_data[, .I == .N] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                  overlap_data[overlap_data[, .I == .N],intnum] == intnum &
                  exp_beyond_threshold == FALSE,
                `:=`(
                  censor = 1, outcome = 0,
                  exposure = 0,
                  tie = 0,
                  A0.warn = ifelse(Awarn_exp_beyond_threshold, 1, 0),
                  case = ifelse(firs_exp_rule == 0, "4b", "11a")
                )
              ]
            }

            # LOGIC 1: cases 12a/13a: t is last interval and the failure event
            #                         occurs during the interval t, then let Z
            #                         be number of days in each interval and
            #                         compute the average exposure (weighted
            #                         mean) during Z days up to and including
            #                         the day when the failure event occurs
            # LOGIC 2: cases 5b/6b: t is last interval and the failure event
            #                       occurs during the interval t, then let Z
            #                       be number of days in each interval and
            #                       compute the average exposure (weighted
            #                       mean) during Z days up to and including
            #                       the day when the failure event occurs
            if (overlap_data[.N, outcome == 1 & Part1 == FALSE]) {
              # NOTE: greater than 2 rows corresponds to the case where the
              #       event does not occur in the first interval. As per the
              #       case def'n, the case where the event occurs in the first
              #       observed interval must be handled differently.
              if (nrow(overlap_data) > 2) {
                # re-define exposure time based on Z-days interval
                overlap_data[, z_exp_beyond_threshold :=
                  as.numeric(z_exp_time_by_freq_exp) / unit_time >= exp_threshold]
                overlap_data[.N, z_exp_beyond_threshold :=
                  overlap_data[.N - 1, z_exp_beyond_threshold]]
                overlap_data[, Awarn_z_exp_beyond_threshold :=
                  as.numeric(z_exp_time) / unit_time >= exp_threshold]
              } else {
                # in this setting, use eof_date - intstart as reference
                ref_time <- as.numeric(overlap_data[1, get(eof_date) -
                  intstart + 1])
                # re-define exposure time based on Z-days interval
                overlap_data[, z_exp_beyond_threshold :=
                  as.numeric(z_exp_time_by_freq_exp) / ref_time >= exp_threshold]
                overlap_data[.N, z_exp_beyond_threshold :=
                  overlap_data[.N - 1, z_exp_beyond_threshold]]
                overlap_data[, Awarn_z_exp_beyond_threshold :=
                  as.numeric(exp_time) / ref_time >= exp_threshold]
              }

              # case 12a/5b: ...then if average exposure up to and including
              #                 occurrence of the event is greater than or equal
              #                 to X%, let j=1 and A(t)=j
              overlap_data[
                #overlap_data[, .I == (.N - 1)] == TRUE & OLD
                ### ATTENTION: TRUE for intnum in which overlap_data[, .I == (.N - 1)] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                overlap_data[overlap_data[, .I == (.N - 1)],intnum] == intnum &
                  z_exp_beyond_threshold == TRUE,
                `:=`(
                  censor = 0, outcome = 0,
                  exposure = 1,
                  tie = unique(z_tie),
                  A0.warn = 0,
                  case = ifelse(firs_exp_rule == 0, "5b", "12a")
                )
              ]
              overlap_data[
                overlap_data[, .I == .N] == TRUE &
                  z_exp_beyond_threshold == TRUE,
                `:=`(
                  censor = NA, outcome = 1,
                  exposure = NA,
                  case = ifelse(firs_exp_rule == 0, "5b", "12a")
                )
              ]
              # case 13a/6b: ...then if average exposure up to and including
              #                 occurrence of event is less than X%, let j = 1
              #                 and A(t)=j
              overlap_data[
                #overlap_data[, .I == (.N - 1)] == TRUE & OLD
                ### ATTENTION: TRUE for intnum in which overlap_data[, .I == (.N - 1)] (i.e. there may exist multiple rows for any time point t due to carrying tied exposure levels by intnum)
                overlap_data[overlap_data[, .I == (.N - 1)],intnum] == intnum &
                  z_exp_beyond_threshold == FALSE,
                `:=`(
                  censor = 0, outcome = 0,
                  exposure = 0,
                  tie = 0,
                  A0.warn = ifelse(Awarn_z_exp_beyond_threshold,1,0),
                  case = ifelse(firs_exp_rule == 0, "6b", "13a")
                )
              ]
              overlap_data[
                overlap_data[, .I == .N] == TRUE &
                  z_exp_beyond_threshold == FALSE,
                `:=`(
                  censor = NA, outcome = 1,
                  exposure = NA,
                  tie = 0,
                  case = ifelse(firs_exp_rule == 0, "6b", "13a")
                )
              ]
            }

            #####################################
            ######## Non-binary exposure ########
            #####################################
            if(length(unique(exp_data[,get(exp_level)])) > 1){
              ### Creating exposureTMP to hold temporary exposure levels, will later overwrite exposure column with exposureTMP.
              overlap_data[exposure%in%0, exposureTMP := exp_ref]
              overlap_data[exposure%in%1, exposureTMP := get(exp_level)]
              overlap_data[outcome == 1, exposureTMP := "",]
              overlap_data[, eval(exp_level) := as.character(get(exp_level))]
              
              ##### Logic 1 #####
              if(firs_exp_rule == 1){
                overlap_data[tie==1 & case%in%c("1a","5a","6a"), exposureTMP := max_freq_exp ] ### Properly assigning exposures for instances in which tie==1 and case={1a,5a,6a} (m. max_freq_exp holds the exposure level involved in the tie that the patient experienced last during the interval t)
                overlap_data[exposure==1 & case=="12a" & tie!=1, exposureTMP := unique(z_exposure_times[,get(exp_level)])] ### Assigns most frequent exposure for case 12a w/no tie
                
                overlap_data[,TIE_8A:=ifelse(case=="8a" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[, TIE_8A])){
                  for(tie.index in overlap_data[TIE_8A==1, unique(intnum)]){
                    TIE_8A_test <- any(overlap_data[intnum%in%tie.index, exposureTMP]%in%overlap_data[intnum%in%(tie.index-1), exposureTMP]) ## testing if tied exposures are in previous bin (i.e. A(t-1))
                    At.minus1 <- unique(overlap_data[intnum%in%(tie.index-1), exposureTMP])
                    overlap_data[intnum%in%tie.index, exposureTMP := ifelse(TIE_8A_test, At.minus1, unique(max_freq_exp))] # Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                  }
                }
                
                overlap_data[,TIE_10A:=ifelse(case=="10a" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[, TIE_10A])){
                  overlap_data[,TIE_10A_LEAD := shift(TIE_10A, n = 1L, fill = NA, type = "lead")]
                  TIE_10A_test <- any(overlap_data[(TIE_10A), exposureTMP]%in%overlap_data[intnum%in%overlap_data[!TIE_10A & TIE_10A_LEAD, intnum], exposureTMP]) ## testing if tied exposures are in previous bin (i.e. A(t-1))
                  At.minus1 <- overlap_data[TIE_10A_LEAD & !TIE_10A, exposureTMP]
                  overlap_data[TIE_10A == TRUE, exposureTMP := ifelse(TIE_10A_test, At.minus1, unique(max_freq_exp))] # Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                }

                overlap_data[,TIE_12A := ifelse(case=="12a" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[,TIE_12A])){
                  overlap_data[,TIE_12A_LEAD := shift(TIE_12A, n = 1L, fill = NA, type = "lead")]
                  TIE_12A_test <- any(z_exposure_times[, get(exp_level)]%in%overlap_data[intnum%in%overlap_data[!TIE_12A & TIE_12A_LEAD, intnum], exposureTMP]) ### testing if tied exposures are in previous bin (i.e. A(t-1))
                  At.minus1 <- overlap_data[TIE_12A_LEAD == TRUE, exposureTMP]
                  overlap_data[TIE_12A == TRUE, exposureTMP := ifelse(TIE_12A_test, At.minus1, unique(z_max_freq_exp))] ### Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                }
              
              ##### Logic 2 #####  
              } else if(firs_exp_rule == 0){
                overlap_data[exposure==1 & case=="5b" & tie!=1, exposureTMP:=ifelse((z_exp_time_by_freq_exp/z_exp_time)>=exp_threshold,z_max_freq_exp,exposureTMP)] ### Assigns most frequent exposure for case 5b w/no tie
                overlap_data[is.na(get(exp_level)) & is.na(exposureTMP) & case=="5b" & tie!=1, exposureTMP:=z_max_freq_exp]  ### Assigns proper exposure level to patients who overlap==FALSE Z-days up to and including the day the failure event occurs
                
                overlap_data[,TIE_1B:=ifelse(case=="1b" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[, TIE_1B])){
                  for(tie.index in overlap_data[TIE_1B==1, unique(intnum)]){
                    TIE_1B_test <- any(overlap_data[intnum%in%tie.index, exposureTMP]%in%overlap_data[intnum%in%(tie.index-1), exposureTMP]) ### testing if tied exposures are in previous bin (i.e. A(t-1))
                    At.minus1 <- unique(overlap_data[intnum%in%(tie.index-1), exposureTMP])
                    overlap_data[intnum%in%tie.index, exposureTMP := ifelse(TIE_1B_test, At.minus1, unique(max_freq_exp))] ### Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                  }
                }
                
                overlap_data[,TIE_3B:=ifelse(case=="3b" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[, TIE_3B])){
                  overlap_data[,TIE_3B_LEAD := shift(TIE_3B, n = 1L, fill = NA, type = "lead")]
                  TIE_3B_test <- any(overlap_data[(TIE_3B), exposureTMP]%in%overlap_data[intnum%in%overlap_data[!TIE_3B & TIE_3B_LEAD, intnum], exposureTMP]) ### testing if tied exposures are in previous bin (i.e. A(t-1))
                  At.minus1 <- overlap_data[TIE_3B_LEAD & !TIE_3B, exposureTMP]
                  overlap_data[TIE_3B == TRUE, exposureTMP := ifelse(TIE_3B_test, At.minus1, unique(max_freq_exp))] ### Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                }
                
                overlap_data[,TIE_5B := ifelse(case=="5b" & tie==1,TRUE,FALSE)]
                if(any(overlap_data[,TIE_5B])){
                  overlap_data[,TIE_5B_LEAD := shift(TIE_5B, n = 1L, fill = NA, type = "lead")]
                  TIE_5B_test <- any(z_exposure_times[,get(exp_level)]%in%overlap_data[intnum%in%overlap_data[!TIE_5B & TIE_5B_LEAD,intnum], exposureTMP]) ### testing if tied exposures are in previous bin (i.e. A(t-1))
                  At.minus1 <- overlap_data[TIE_5B_LEAD == TRUE, exposureTMP]
                  overlap_data[TIE_5B == TRUE, exposureTMP := ifelse(TIE_5B_test, At.minus1, unique(z_max_freq_exp))] ### Level 'k' is defined as A(t-1) if A(t-1) was set to one of the levels involved in the tie at time t and otherwise 'k' is set to the level involved in the tie that the patient experienced last during the interval t.
                }
              }
              
              overlap_data[, exposure := NULL]
              overlap_data[, exposure := exposureTMP]
              overlap_data[, final := TRUE]
              overlap_data[duplicated(intnum), final:= FALSE]
              overlap_data <- overlap_data[(final),]
              # final output for current case
              return(overlap_data[, c(
                id_var, "intnum", "intstart",
                "intend", "exposure", "outcome",
                "censor", "case", eof_type, "tie", "A0.warn"
              ),
              with = FALSE
              ])
            }
            
            #######################################
            ########### Binary exposure ###########
            #######################################
            else {
              # final output for current case
              return(overlap_data[, c(
                id_var, "intnum", "intstart",
                "intend", "exposure", "outcome",
                "censor", "case", eof_type
              ),
              with = FALSE
              ])
            }
          }
        )
        
      # combined list of data.tables together and sort by ID
      data_assigned_ac <- data.table::rbindlist(data_sliced_by_id)
      if(exp_data[,length(unique(get(exp_level)))] > 1){
        data.table::setcolorder(data_assigned_ac, c(
          id_var, "intnum", "intstart", "intend",
          "exposure", "outcome", "censor",
          "case", eof_type, "tie", "A0.warn"
        ))
      }
      else {
        data.table::setcolorder(data_assigned_ac, c(
          id_var, "intnum", "intstart", "intend",
          "exposure", "outcome", "censor",
          "case", eof_type
        ))
      }
      data.table::setorderv(data_assigned_ac, id_var)[]

      ## # comparison with gold standard / model output data set for LOGIC 1
      ## if (firs_exp_rule == 1) {
      ##   model_output <- expDT_15_f1_p75[, c(
      ##     "ID", "intnum", "intstart", "intend",
      ##     "exposure", "outcome", "censor",
      ##     "case", "EOFtype",
      ##     "daysexposed1", "intervalpercent1", "lastintpercent1"
      ##   ), with = FALSE]
      ##   gs_diff <- sum(model_output$case != data_assigned_ac$case) /
      ##     length(data_assigned_ac$case) * 100
      ##   message(paste0(
      ##     "Detected difference from gold standard: ", gs_diff,
      ##     "%."
      ##   ))
      ## }

      ## # comparison with gold standard / model output data set for LOGIC 2
      ## if (firs_exp_rule == 0) {
      ##   model_output <- expDT_15_f0_p75[, c(
      ##     "ID", "intnum", "intstart",
      ##     "intend", "exposure", "outcome",
      ##     "censor", "case", "EOFtype"
      ##   )]
      ##   gs_diff <- sum(model_output$case != data_assigned_ac$case) /
      ##     length(data_assigned_ac$case) * 100
      ##   message(paste0(
      ##     "Detected difference from gold standard: ", gs_diff,
      ##     "%."
      ##   ))
      ## }

      #data.table::setcolorder(data_assigned_ac, c(
      #  id_var, "intnum", "intstart", "intend",
      #  "exposure", "outcome", "censor",
      #  "case", eof_type, "tie", "A0.warn"
      #))
      ## data.table::setorderv(data_assigned_ac, id_var)[]

      ## Noel: we need your output to match this (including the new column  tie and A0.warn not created by Nima):
      #model_output <- expDT4_30_f1_p25[, c("ID", "intnum", "intstart", "intend","exposure", "outcome", "censor","case", "EOFtype","tie","A0.warn"), with = FALSE]
      ## Noel: this measures the discrepancy between your output and the GS output:
      #print(sum(model_output$case != data_assigned_ac$case) /length(data_assigned_ac$case) * 100)
      #print(sum(model_output$tie != data_assigned_ac$tie) /length(data_assigned_ac$tie) * 100)
      #print(sum(model_output$A0.warn != data_assigned_ac$A0.warn) /length(data_assigned_ac$A0.warn) * 100)
      #print(sum(model_output$exposure != data_assigned_ac$exposure) /length(data_assigned_ac$exposure) * 100)
      ## Noel: this is a problematic id - you can define it and then got back to the begining of the lapply above to understand where the discrepancy with the GS output comes from:
      #(id <- model_output$ID[model_output$case != data_assigned_ac$case][1])
      #(id <- model_output$ID[model_output$tie != data_assigned_ac$tie][1])
      #(id <- model_output$ID[model_output$A0.warn != data_assigned_ac$A0.warn][1])
      #(id <- model_output$ID[model_output$exposure != data_assigned_ac$exposure][1])
      ## # comparison with gold standard / model output data set for LOGIC 1
      ## if (firs_exp_rule == 1) {
      ##   model_output <- expDT_15_f1_p75[, c(
      ##     "ID", "intnum", "intstart", "intend",
      ##     "exposure", "outcome", "censor",
      ##     "case", "EOFtype",
      ##     "daysexposed1", "intervalpercent1", "lastintpercent1"
      ##   ), with = FALSE]
      ##   gs_diff <- sum(model_output$case != data_assigned_ac$case) /
      ##     length(data_assigned_ac$case) * 100
      ##   message(paste0(
      ##     "Detected difference from gold standard: ", gs_diff,
      ##     "%."
      ##   ))
      ## }

      ## # comparison with gold standard / model output data set for LOGIC 2
      ## if (firs_exp_rule == 0) {
      ##   model_output <- expDT_15_f0_p75[, c(
      ##     "ID", "intnum", "intstart",
      ##     "intend", "exposure", "outcome",
      ##     "censor", "case", "EOFtype"
      ##   )]
      ##   gs_diff <- sum(model_output$case != data_assigned_ac$case) /
      ##     length(data_assigned_ac$case) * 100
      ##   message(paste0(
      ##     "Detected difference from gold standard: ", gs_diff,
      ##     "%."
      ##   ))
      ## }
        
      # output
      private$.data <- data_assigned_ac
      invisible(self)

      # model_output <- expDT_15_f1_p75[, c(
      # "ID", "intnum", "intstart", "intend",
      # "exposure", "outcome", "censor",
      # "case", "EOFtype",
      # "daysexposed1", "intervalpercent1", "lastintpercent1"
      # ), with = FALSE]

      # print(sum(model_output$case != data_assigned_ac$case) /
      # length(data_assigned_ac$case) * 100)
    },
    assignL = function(first_exp_rule = 1) {

      # pointer to data sources for convenience
      outcome_data <- private$.data
      cohort_data <- private$.cohort_data$data
      exp_data <- private$.exp_data$data
      id_var <- private$.cohort_data$IDvar
      eof_date_var <- private$.cohort_data$EOF_date
      eof_type_var <- private$.cohort_data$EOF_type
      index_date_var <- private$.cohort_data$index_date
      start_date_var <- private$.exp_data$start_date
      end_date_var <- private$.exp_data$end_date
      A_level <- private$.exp_data$exp_level
      t_ind_cov <- private$.cohort_data$L0[!private$.cohort_data$L0%in%names(private$.cov_data)]

      # Eventually we will loop over elements of the covariate list:
      # private$.cov_data. For now, we focus on a single covariate of type
      # "sporadic": A1c in GS2

        ## looping over time-dependent covariates

        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
          for(cov_position in seq_along(private$.cov_data)){
            
            # extract data for the current covariate only
            cov_data <- private$.cov_data[[cov_position]]
            cov_name <- private$.cov_data[[cov_position]]$L_name
            cov_acute <- private$.cov_data[[cov_position]]$acute_change
            cov_date <- private$.cov_data[[cov_position]]$L_date
            
            # iterative over subject IDs in parallel to assign L(t)
            data_assigned_lt_by_id <-
              future.apply::future_lapply(unique(outcome_data[, get(id_var)]),
                                          #lapply(unique(outcome_data[, get(id_var)]),
                                          function(id) {
                                            ## message(paste("Current ID:", id))
                                            # subset to just the current subject
                                            index_cov_this_id <- cohort_data[get(id_var) == id, get(cov_name)]
                                            index_date_this_id <- cohort_data[get(id_var) == id, get(index_date_var)]
                                            eof_date_this_id <- lubridate::ymd(cohort_data[get(id_var) == id, get(eof_date_var)])
                                            exp_data_this_id <- exp_data[get(id_var) == id, ]
                                            exp_start_this_id <- lubridate::ymd(exp_data_this_id[, get(start_date_var)])
                                            cov_data_this_id <- cov_data$data[get(id_var) == id, ]
                                            cov_dates_this_id <- cov_data_this_id[,get(cov_date)]
                                            cov_values_this_id <- cov_data_this_id[,get(cov_name)]
                                            cov_data_this_id[, cov_date := lubridate::ymd(get(cov_date))]
                                            exp_character <- is.character(exp_data[,get(A_level)])
                                            exp_ref <- private$.exp_data$exp_ref
                                            exp_level <- private$.exp_data$exp_level
                                            # keep only the time-dependent values strictly after index date:
                                            cov_data_this_id <-
                                              cov_data_this_id[eval(cov_date) > index_date_this_id, ]
                                            outcome_data_this_id <- suppressWarnings(outcome_data[get(id_var) == id, ])
                                            data.table::setnames(outcome_data_this_id, "case", "caseExp")
                                            
                                            # use intervals to check where time-dependent covariate falls
                                            # overlap finding technique inspired by
                                            # https://stackoverflow.com/questions/41132081/find-which-interval-row-in-a-data-frame-that-each-element-of-a-vector-belongs-in
                                            overlaps <- outcome_data_this_id[data.table::data.table(cov_data_this_id),
                                                                             on = .(intstart <= cov_date,
                                                                                    intend >= cov_date)]
                                            suppressWarnings(overlaps[, `:=`(intstart = NULL, intend = NULL, #i.ID = NULL, 
                                                                             exposure = NULL, outcome = NULL, censor = NULL,
                                                                             caseExp = NULL, tie = NULL, A0.warn = NULL)]) #, EOFtype = NULL)]
                                            overlaps[, eval(eof_type_var):=NULL]
                                            overlaps[,eval("i."%+%id_var):=NULL]
                                            cov_data_this_id[, cov_date := NULL]
                                            
                                            # in general, L(t) is defined as occuring before the start of
                                            # interval t (i.e., in bin t-1)
                                            # a covariate occuring in bin 7 will most often be mapped to L(8)
                                            if (nrow(outcome_data_this_id) > 1) {
                                              overlaps[, intnum := intnum + 1]
                                            }
                                            
                                            # use only most recent covariate measurement assigned to interval
                                            overlaps <- overlaps[, .SD[which.max(get(cov_date))],
                                                                 by = intnum]
                                            
                                            # combined (reduced) overlap data with outcomes data
                                            combined_data <- merge(outcome_data_this_id, overlaps,
                                                                   by = c(id_var, "intnum"), all.x = TRUE)
                                            
                                            # define Z days as the number of days in each interval
                                            z_days <- unique(combined_data[, intend - intstart + 1])
                                            
                                            # add cases and flag indicating exposure change
                                            # add flag indicating continous non-exposure so far
                                            combined_data[, case := NA_character_]
                                            combined_data[, exp_change :=
                                                            (exposure != shift(exposure, n = 1L, type = "lag"))]
                                            combined_data[1, exp_change := FALSE]
                                            combined_data[intnum == 0 & (exposure == 1 | exposure!=exp_ref), exp_change := TRUE]
                                            if(exp_character){
                                              combined_data[,exposure_binary:=ifelse(exposure==exp_ref,0,1)]
                                              combined_data[, part1 := !(cumsum(shift(exposure_binary,
                                                                                      n = 1L, fill = 0,
                                                                                      type = "lag")) > 0)]
                                              combined_data$exposure_binary <- NULL
                                            } else{
                                              combined_data[, part1 := !(cumsum(shift(exposure,
                                                                                      n = 1L, fill = 0,
                                                                                      type = "lag")) > 0)]
                                            }
                                            
                                            # case 1/2: t is the first and not the last interval and the unit is
                                            #           unexposed (case 1) or exposed (case 2)
                                            if (nrow(combined_data) > 1) {
                                              # case 1: unexposed at t = 0
                                              combined_data[part1 & intnum == 0 & (exposure == 0 | exposure == exp_ref), case := "1"]
                                              combined_data[part1 & intnum == 0 & (exposure == 0 | exposure == exp_ref),
                                                            eval(cov_name) := index_cov_this_id]
                                              combined_data[part1 & intnum == 0 & (exposure == 0 | exposure == exp_ref),
                                                            eval(cov_date) := index_date_this_id]
                                              
                                              # case 2: exposed at t = 0
                                              combined_data[part1 & intnum == 0 & (exposure == 1 | exposure != exp_ref), case := "2"]
                                              # cov_int_dates <- lubridate::ymd(overlaps[[cov_date]])
                                              # NOTE: need to check whether first measurement after index date
                                              # occurs prior to first exposure date
                                              
                                              if (combined_data[intnum == 0, case == "2"]) {
                                                interval.final <- lubridate::interval(index_date_this_id+1,exp_start_this_id[1]-cov_acute)
                                                if(exp_character) {
                                                  At.0 <- combined_data[intnum==0,exposure]
                                                  exp_start_j <- exp_data_this_id[get(exp_level)%in%At.0,get(start_date_var)][1]
                                                  interval.final <- lubridate::interval(index_date_this_id+1,exp_start_j-cov_acute)
                                                }
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==0,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==0,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                } else {
                                                  combined_data[intnum==0,eval(cov_date):=index_date_this_id]
                                                  combined_data[intnum==0,eval(cov_name):=index_cov_this_id]
                                                }
                                              }
                                            }
                                            
                                            # case 3/4: t is not the first interval or the last interval and
                                            #           the unit does not change exposure status (case 3) or
                                            #           does change exposure status (case 4)
                                            if (any(combined_data$outcome == 1)) {
                                              combined_data[!(intnum %in% c(0, .N-2, .N-1)) &
                                                              exp_change == FALSE & part1 == TRUE, case := "3"]
                                              combined_data[!(intnum %in% c(0, .N-2, .N-1)) &
                                                              exp_change == TRUE & part1 == TRUE, case := "4"]
                                            } else if (any(combined_data$censor == 1, na.rm = TRUE)) {
                                              combined_data[!(intnum %in% c(0, .N-1)) & exp_change == FALSE
                                                            & part1 == TRUE, case := "3"]
                                              combined_data[!(intnum %in% c(0, .N-1)) & exp_change == TRUE
                                                            & part1 == TRUE, case := "4"]
                                            }
                                            
                                            if(any(combined_data$case %in% "4")){
                                              t.int <- combined_data[case==4,intnum]
                                              interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int-1,get(cov_name)]), combined_data[intnum==t.int-1,intstart], combined_data[intnum==t.int-1,get(cov_date)+1]) ### Check if there exists a date for L(t-1); if not, starting point is intstart date; otherwise, starting point is L(t-1) date + 1
                                              
                                              if(exp_character){
                                                At <- combined_data[case==4,exposure]
                                                ints_overlaps <- lubridate::interval(combined_data[case == "4", intstart],
                                                                                     combined_data[case == "4", intend])
                                                exp1_int <- lubridate::interval(exp_data_this_id[get(exp_level)%in%At, get(start_date_var)],
                                                                                exp_data_this_id[get(exp_level)%in%At, get(end_date_var)])
                                              } else{
                                                ints_overlaps <- lubridate::interval(combined_data[case == "4", intstart],
                                                                                     combined_data[case == "4", intend])
                                                exp1_int <- lubridate::interval(exp_data_this_id[, get(start_date_var)],
                                                                                exp_data_this_id[, get(end_date_var)])
                                              }
                                              
                                              d1_case11 <- do.call("c", lapply(ints_overlaps, function(x) {
                                                suppressWarnings(as_date(min(lubridate::int_start(lubridate::intersect(x, exp1_int)),
                                                                             na.rm = TRUE)))
                                              }))
                                              interval.end <- d1_case11 - cov_acute
                                              #interval.end <- exp_start_this_id[1] - cov_acute              
                                              assigned_L <- cov_data_this_id[cov_data_this_id[, get(cov_date)]
                                                                             %within% suppressWarnings(lubridate::interval(interval.start,interval.end)), ][, .SD[which.max(get(cov_date))]]              
                                              if(nrow(assigned_L)==0) {
                                                combined_data[case==4,eval(cov_name):=NA]
                                                combined_data[case==4,eval(cov_date):=NA]
                                              } else {
                                                combined_data[case==4,eval(cov_name):=assigned_L[,get(cov_name)]]
                                                combined_data[case==4,eval(cov_date):=assigned_L[,get(cov_date)]]
                                              }
                                            }
                                            
                                            # case 5/7: t is the last interval and the unit does not change
                                            #           exposure status (case 5) or does change exposure status
                                            #           (case 7) and a right-censoring event occurs. Either may
                                            #           apply when t is the first and the last interval.
                                            combined_data[intnum == .N - 1 & exp_change == FALSE
                                                          & part1 == TRUE & censor == 1, case := "5"]
                                            combined_data[intnum == .N - 1 & exp_change == TRUE
                                                          & part1 == TRUE & censor == 1, case := "7"]
                                            
                                            if(any(combined_data$case %in% c("5","7"))) {
                                              t.int <- combined_data[case%in%c("5","7"),intnum]
                                              if(t.int==0){
                                                interval.start <- index_date_this_id
                                              } else{
                                                interval.start <- dplyr::if_else(combined_data[intnum==t.int-1,is.na(get(cov_name)) & is.na(get(cov_date))], combined_data[intnum==t.int-1,intstart], combined_data[intnum==t.int-1,get(cov_date)+1]) ### Check if there exists a date and cov value for L(t-1); if not, starting point is intstart date, if yes, starting point is L(t-1) date + 1
                                              }
                                              interval.end <- eof_date_this_id[1]-cov_acute
                                              interval.final <- lubridate::interval(interval.start,interval.end)
                                              ### This case is particular to when t.int==0
                                              cov_dates_this_id_t0_tmp <- c(index_date_this_id,cov_dates_this_id)
                                              cov_values_this_id_t0_tmp <- c(index_cov_this_id,cov_values_this_id)
                                              ### index date and value supersedes any covariate measurement on same day
                                              cov_dates_this_id_t0 <- cov_dates_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                              cov_values_this_id_t0 <- cov_values_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                              if(any(cov_dates_this_id_t0%within%interval.final)){
                                                combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                                combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                              }
                                              else{ ### L(t) is set to missing if no such measurements exists
                                                combined_data[intnum==t.int,eval(cov_date):=NA]
                                                combined_data[intnum==t.int,eval(cov_name):=NA]
                                              }
                                            }
                                            #if (any(combined_data$case %in% c("5", "7")) && nrow(cov_data_this_id) > 0) {
                                            # if (any(combined_data$case %in% c("7")) && nrow(cov_data_this_id) > 0) {
                                            #   # simply overwrite the shifted covariate value and date with the
                                            #   # last observed entry; it is fine to ignore whether the date goes
                                            #   # past the censoring time since there should not be measurements
                                            #   # after censoring, by definition.
                                            #   #if (nrow(combined_data) == 1) {
                                            #     #overwrite_last_cov <-
                                            #       #which.min(abs(eof_date_this_id -
                                            #                     #cov_data_this_id[[cov_date]]))
                                            #     #overwrite_cov <- cov_data_this_id[overwrite_last_cov,
                                            #                                       #..cov_name]
                                            #     #overwrite_date <- cov_data_this_id[overwrite_last_cov,
                                            #                                        #..cov_date]
                                            #     #combined_data[case %in% c("5", "7"),
                                            #                   #eval(cov_name) := overwrite_cov]
                                            #     #combined_data[case %in% c("5", "7"),
                                            #                   #eval(cov_date) := overwrite_date]
                                            #   #} else {
                                            #     eof_int <- combined_data[eof_date_this_id %within%
                                            #                              lubridate::interval(intstart, intend), intnum]
                                            #     prev_intstart <- combined_data[intnum == eof_int - 1, intstart]
                                            #     overwrite_last_cov <-
                                            #       which.min(abs(eof_date_this_id -
                                            #                     cov_data_this_id[[cov_date]]))
                                            #     overwrite_date <- cov_data_this_id[overwrite_last_cov,
                                            #                                        ..cov_date]
                                            #     check_prev_int <- lubridate::as_date(overwrite_date[[1]]) > prev_intstart
                                            #     if (isTRUE(check_prev_int)) {
                                            #       overwrite_cov <- cov_data_this_id[overwrite_last_cov,
                                            #                                         ..cov_name]
                                            #       combined_data[case %in% c("5", "7"),
                                            #                     eval(cov_name) := overwrite_cov]
                                            #       combined_data[case %in% c("5", "7"),
                                            #                     eval(cov_date) := overwrite_date]
                                            #     }
                                            #   #}
                                            # }
                                            
                                            # case 6/8: t is the last interval and the unit does not change
                                            #           exposure status (case 6) or does change exposure status
                                            #           (case 8) and the outcome event occurs. Either may apply
                                            #           when t is the first and the last interval.
                                            # NOTE: when there's an outcome, there exists an artificial last row
                                            #       note also that `intnum` is zero-indexed so need to use .N-2
                                            #       to catch the actually last observed data row.
                                            if (any(combined_data$outcome == 1)) {
                                              combined_data[intnum == .N-2 & exp_change == FALSE &
                                                              part1 == TRUE, case := "6"]
                                              combined_data[intnum == .N-2 & exp_change == TRUE &
                                                              part1 == TRUE, case := "8"]
                                            }
                                            
                                            if (any(combined_data$case %in% "8") & first_exp_rule==1) {
                                              ### L(t) assignment
                                              t.int <- combined_data[case==8,intnum]
                                              interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int-1,get(cov_name)]), combined_data[intnum==t.int-1,intstart], combined_data[intnum==t.int-1,get(cov_date)+1]) ### Check if there exists an assigned date for L(t-1); if not, starting point is intstart date; otherwise, starting point is L(t-1) date + 1
                                              if(t.int==0) interval.start <- index_date_this_id ### For t.int=0, the start of the interval is index date
                                              if(exp_character){
                                                At <- combined_data[intnum==t.int,exposure]
                                                exp_start_j <- exp_data_this_id[get(exp_level)%in%At,get(start_date_var)][1]
                                                interval.end <- exp_start_j-cov_acute
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                              } else{
                                                interval.end <- exp_start_this_id[1] - cov_acute
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                              }
                                              ### This case is particular to when t.int==0
                                              cov_dates_this_id_t0_tmp <- c(index_date_this_id,cov_dates_this_id)
                                              cov_values_this_id_t0_tmp <- c(index_cov_this_id,cov_values_this_id)
                                              ### index date and value supersedes any covariate measurement on same day
                                              cov_dates_this_id_t0 <- cov_dates_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                              cov_values_this_id_t0 <- cov_values_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                              if(t.int==0){
                                                if(any(cov_dates_this_id_t0%within%interval.final)){
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                                } else if(interval.start>interval.end){
                                                  combined_data[intnum==t.int,eval(cov_date):=index_date_this_id]
                                                  combined_data[intnum==t.int,eval(cov_name):=index_cov_this_id]
                                                } else { 
                                                  combined_data[intnum==t.int,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int,eval(cov_name):=NA]
                                                }
                                              }
                                              else {
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                }
                                                else{ ### L(t) is set to missing if no such measurements exists
                                                  combined_data[intnum==t.int,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int,eval(cov_name):=NA]
                                                }
                                              }
                                              
                                              ### L(t+1) assignment
                                              #is.na(combined_data[intnum==t.int,get(cov_date)])
                                              #acute.exp.date <- dplyr::if_else(cov_acute,exp_start_this_id,exp_start_this_id+1)
                                              acute.exp.date <- exp_start_this_id[1]+!cov_acute
                                              interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int,get(cov_date)]), acute.exp.date, combined_data[intnum==t.int,get(cov_date)]+1) ### interval.start is strictly after the date when L(t) is measured, if L(t) is present, or, else,(strictly after the first day the exposure changes(i.e. cov_acute==FALSE) or on or after the first day when the exposure changes(i.e. cov_acute==TRUE))
                                              interval.end <- eof_date_this_id
                                              interval.final <- lubridate::interval(interval.start,interval.end)
                                              if(any(cov_dates_this_id%within%interval.final)){
                                                combined_data[intnum==t.int+1,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                combined_data[intnum==t.int+1,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                              }
                                              else{
                                                combined_data[intnum==t.int+1,eval(cov_date):=NA]
                                                combined_data[intnum==t.int+1,eval(cov_name):=NA]
                                              }
                                            }
                                            
                                            # NOTE: cases 5--8 all apply the same logic when there is just a
                                            #       single observed interval --- that is, replace w/e the
                                            #       currently assigned values with closest values prior to the
                                            #       failure/censoring date, inclusive of index measurements
                                            if (nrow(combined_data) == 1 & !(any(combined_data[intnum==0,case]==c("5","7","8")))) { ### excluding cases 5,7, 8 because code above accounts for value assignmentat t=0
                                              index_entry <- list(id_var = id, cov_name = index_cov_this_id,
                                                                  cov_date = index_date_this_id)
                                              names(index_entry) <- c(id_var, cov_name, cov_date)
                                              index_cov_data_this_id <-
                                                data.table::rbindlist(list(index_entry, cov_data_this_id),
                                                                      use.names = TRUE)
                                              index_overwrite <- index_cov_data_this_id[get(cov_date) <
                                                                                          eof_date_this_id]
                                              index_overwrite <-
                                                index_overwrite[, .SD[which.max(get(cov_date))]]
                                              combined_data[, eval(cov_name) := index_overwrite[[cov_name]]]
                                              combined_data[, eval(cov_date) := index_overwrite[[cov_date]]]
                                            }
                                            
                                            # NOTE: now, we're in "part 2", defined only as all intervals that
                                            #       follow the first period in which exposure status changes
                                            # part II has two case types: 1) last bin (cases 12-14), 2) not a
                                            # last bin (cases 10-11)
                                            
                                            # case 10/11: t is not the last interval and the unit does not
                                            #             change exposure status (like case 3) or does change
                                            #             exposure status (like case 4)
                                            if (any(combined_data$censor == 1, na.rm = TRUE)) {
                                              combined_data[intnum != .N-1 & exp_change == FALSE &
                                                              part1 == FALSE, case := "10"]
                                              combined_data[intnum != .N-1 & exp_change == TRUE &
                                                              part1 == FALSE, case := "11"]
                                            } else {
                                              # NOTE: again, note that the last row of observed data is .N-2,
                                              #       but there's an artificial last row absent censoring, so
                                              #       we need to ignore both .N-2 and .N-1 here...
                                              combined_data[!(intnum %in% c(.N-2, .N-1)) &
                                                              exp_change == FALSE & part1 == FALSE, case := "10"]
                                              combined_data[!(intnum %in% c(.N-2, .N-1)) & exp_change == TRUE &
                                                              part1 == FALSE, case := "11"]
                                            }
                                            if(z_days!=1){
                                              if(any(combined_data$case %in% c("10","11"))){
                                                t.int <- combined_data[case%in%c("10","11"),intnum]
                                                for(t.i in t.int){
                                                  t.i.case <- combined_data[intnum==t.i,case]
                                                  if(t.i.case==10){
                                                    interval.start <- dplyr::if_else(combined_data[intnum==t.i-1,is.na(get(cov_name)) & is.na(get(cov_date))], combined_data[intnum==t.i-1,intstart], combined_data[intnum==t.i-1,get(cov_date)+1]) ### Check if there exists a date for L(t-1); if not, starting point is intstart date, if yes, starting point is L(t-1) date + 1
                                                    interval.end <- combined_data[intnum==t.i-1,intend]
                                                    interval.final <- lubridate::interval(interval.start,interval.end)
                                                    if(any(cov_dates_this_id%within%interval.final)){
                                                      combined_data[intnum==t.i,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                      combined_data[intnum==t.i,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                    }
                                                    else{ ### L(t) is set to missing if no such measurements exists
                                                      combined_data[intnum==t.i,eval(cov_date):=NA]
                                                      combined_data[intnum==t.i,eval(cov_name):=NA]
                                                    }
                                                  }
                                                  else{
                                                    interval.start <- dplyr::if_else(combined_data[intnum==t.i-1,is.na(get(cov_name)) & is.na(get(cov_date))], combined_data[intnum==t.i-1,intstart], combined_data[intnum==t.i-1,get(cov_date)+1]) ### Check if there exists a date for L(t-1); if not, starting point is intstart date, if yes, starting point is L(t-1) date + 1
                                                    ints_overlaps <- lubridate::interval(combined_data[intnum == t.i, intstart],
                                                                                         combined_data[intnum == t.i, intend])
                                                    A.t <- combined_data[intnum==t.i,exposure]
                                                    exp1_int <- lubridate::interval(exp_data_this_id[get(exp_level)==A.t, get(start_date_var)],
                                                                                    exp_data_this_id[get(exp_level)==A.t, get(end_date_var)])
                                                    exp0_int <-
                                                      data.table::data.table(exp0Start = exp_data_this_id[, get(end_date_var)] + 1,
                                                                             exp0End = shift(exp_data_this_id[, get(start_date_var) - 1],
                                                                                             n = 1L, fill = eof_date_this_id,
                                                                                             type = "lead"))
                                                    exp0_int <-
                                                      rbind(data.table::data.table(exp0Start = index_date_this_id,
                                                                                   exp0End = exp_data_this_id[1, get(start_date_var) - 1]),
                                                            exp0_int)
                                                    exp0_int <- exp0_int[exp0End>=exp0Start]
                                                    ### Final intervals of non-exposure dates
                                                    exp0_int <- lubridate::interval(exp0_int[,exp0Start],exp0_int[,exp0End])
                                                    
                                                    d1_case11 <- do.call("c", lapply(ints_overlaps, function(x) {
                                                      suppressWarnings(as_date(min(lubridate::int_start(lubridate::intersect(x, exp1_int)),
                                                                                   na.rm = TRUE)))
                                                    }))
                                                    d0_case11 <- do.call("c", lapply(ints_overlaps, function(x) {
                                                      suppressWarnings(as_date(min(lubridate::int_start(lubridate::intersect(x, exp0_int)),
                                                                                   na.rm = TRUE)))
                                                    }))
                                                    d_case11 <-
                                                      dplyr::if_else(combined_data[intnum == t.i, (exposure == 0 | exposure == exp_ref)], #dplyr::if_else(combined_data[case == "11", exposure] == 0,
                                                                     d0_case11, d1_case11)
                                                    ### Note that if A(t-1)=j (exposed status) and A(t)=0/"not exposed" then the start
                                                    ### of the non-exposure status  is defined as the first day of the interval when the
                                                    ### unit is unexposed to any exposure levels. If the patients is always exposed during
                                                    ### the interval then the start of non-exposure is defined as the first day of the interval
                                                    ### by default. (i.e. if d_case11 is NA then we use the start of the interval as day d)
                                                    if(is.na(as.character(d_case11))) d_case11 <- combined_data[intnum==t.i,intstart]
                                                    interval.end <- d_case11 - cov_acute
                                                    interval.final <- lubridate::interval(interval.start,interval.end)
                                                    if(any(cov_dates_this_id%within%interval.final)){
                                                      combined_data[intnum==t.i,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                      combined_data[intnum==t.i,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                    }
                                                    else{ ### L(t) is set to missing if no such measurements exists
                                                      combined_data[intnum==t.i,eval(cov_date):=NA]
                                                      combined_data[intnum==t.i,eval(cov_name):=NA]
                                                    }
                                                  }
                                                }
                                              }
                                            } else {
                                              if(any(combined_data$case %in% "10")){
                                                t.int <- combined_data[case==10,intnum][1]
                                                if(exp_character) t.int <- combined_data[exposure!=exp_ref & case==10,intnum][1]
                                                interval.start <- dplyr::if_else(combined_data[intnum==t.int-1,is.na(get(cov_name)) & is.na(get(cov_date))], combined_data[intnum==t.int-1,intstart], combined_data[intnum==t.int-1,get(cov_date)+1]) ### Check if there exists a date for L(t-1); if not, starting point is intstart date, if yes, starting point is L(t-1) date + 1
                                                interval.end <- combined_data[intnum==t.int-1,intend]
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                }
                                                else{ ### L(t) is set to missing if no such measurements exists
                                                  combined_data[intnum==t.int,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int,eval(cov_name):=NA]
                                                }
                                              }
                                              # Case 11 is similar to case 2:
                                              if (any(combined_data$case %in% "11")) {
                                                ints_overlaps <- lubridate::interval(combined_data[case == "11", intstart],
                                                                                     combined_data[case == "11", intend])
                                                exp1_int <- lubridate::interval(exp_data_this_id[, get(start_date_var)],
                                                                                exp_data_this_id[, get(end_date_var)])
                                                exp0_int <-
                                                  data.table::data.table(exp0Start = exp_data_this_id[, get(end_date_var)] + 1,
                                                                         exp0End = shift(exp_data_this_id[, get(start_date_var) - 1],
                                                                                         n = 1L, fill = eof_date_this_id,
                                                                                         type = "lead"))
                                                exp0_int <-
                                                  rbind(data.table::data.table(exp0Start = index_date_this_id,
                                                                               exp0End = exp_data_this_id[1, get(start_date_var) - 1]),
                                                        exp0_int)
                                                exp0_int <- exp0_int[exp0End>=exp0Start]
                                                ### Final intervals of non-exposure dates
                                                exp0_int <- lubridate::interval(exp0_int[,exp0Start],exp0_int[,exp0End])
                                                
                                                d1_case11 <- do.call("c", lapply(ints_overlaps, function(x) {
                                                  suppressWarnings(as_date(min(lubridate::int_start(lubridate::intersect(x, exp1_int)),
                                                                               na.rm = TRUE)))
                                                }))
                                                d0_case11 <- do.call("c", lapply(ints_overlaps, function(x) {
                                                  suppressWarnings(as_date(min(lubridate::int_start(lubridate::intersect(x, exp0_int)),
                                                                               na.rm = TRUE)))
                                                }))
                                                d_case11 <-
                                                  dplyr::if_else(combined_data[case == "11", (exposure == 0 | exposure == exp_ref)], #dplyr::if_else(combined_data[case == "11", exposure] == 0,
                                                                 d0_case11, d1_case11)
                                                # If cov_acute=TRUE, we should not look for covariates measured on day d
                                                # Ltm1_tint <- combined_data[case==11,intnum-1]
                                                # combined_data[intnum%in%Ltm1_tint & is.na(get(cov_date)),Ltm1_instart:=intstart]
                                                # combined_data[intnum%in%Ltm1_tint & !is.na(get(cov_date)),Ltm1_instart:=get(cov_date)+1]
                                                # combined_data[,Ltm1_instart:=shift(Ltm1_instart, n = 1L, type = "lag")]
                                                ints_overlaps <- lubridate::interval(combined_data[case == "11", intstart], ### ATTENTION: ints_overlaps has to include previous interval strictly after when L(t-1) is measured; right now, it's starting at current interval
                                                                                     d_case11 - cov_acute)
                                                # ints_overlaps <- interval(combined_data[case == "11", Ltm1_instart], d_case11 - cov_acute) ### ATTENTION: Trying to implement a fix here that accounts for starting at the previous interval
                                                overwrite_cov <- rbindlist(lapply(ints_overlaps, function(x)
                                                  cov_data_this_id[cov_data_this_id[, get(cov_date)]
                                                                   %within% x, ][, .SD[which.max(get(cov_date))]]),
                                                  idcol = TRUE)
                                                overwrite_cov[, intnum := combined_data[case == "11",
                                                                                        intnum][overwrite_cov[, .id]]]
                                                ## Overwrite index covariate value with the one before exposure
                                                combined_data[intnum %in% overwrite_cov$intnum,
                                                              eval(cov_name) := overwrite_cov[[cov_name]]]
                                                combined_data[intnum %in% overwrite_cov$intnum,
                                                              eval(cov_date) := overwrite_cov[[cov_date]]]
                                                ## The value used for overwriting was stored in next interval
                                                ## remove it since used now in current interval
                                                #combined_data[intnum %in% (overwrite_cov$intnum+1),
                                                #            eval(cov_name) := NA_real_]
                                                #combined_data[intnum %in% (overwrite_cov$intnum+1),
                                                #            eval(cov_date) := ymd(NA_real_)]
                                                combined_data[!is.na(get(cov_date)),duplicated:=duplicated(get(cov_date))]
                                                combined_data[duplicated%in%TRUE,eval(cov_date):=NA]
                                                combined_data[duplicated%in%TRUE,eval(cov_name):=NA]
                                                combined_data[,duplicated:=NULL]
                                              }
                                            }
                                            
                                            # case 12/13/14: t is the last interval and the unit experiences a
                                            #                right-censoring event (case 12), or does not change
                                            #                exposure status and the outcome event occurs (case
                                            #                13), or does change exposure status and the outcome
                                            #                event occurs (case 14)
                                            
                                            # first, handle case 12, since most trivial (NOTE: use .N-1 here as
                                            # there is no artificial final row when censoring occurs)
                                            combined_data[intnum == .N-1 & part1 == FALSE & censor == 1,
                                                          case := "12"]
                                            
                                            if (any(combined_data$case %in% "12")){
                                              int.start <- dplyr::if_else(is.na(lubridate::ymd(combined_data[.N-1, get(cov_date)])), lubridate::ymd(combined_data[.N-1, intstart]), lubridate::ymd(combined_data[.N-1, get(cov_date)+1]))
                                              int.end <- eof_date_this_id-cov_acute
                                              
                                              assigned_L <- cov_data_this_id[cov_data_this_id[, get(cov_date)]
                                                                             %within% suppressWarnings(lubridate::interval(int.start,int.end)), ][, .SD[which.max(get(cov_date))]]
                                              
                                              if(nrow(assigned_L)==0) {
                                                combined_data[case==12,eval(cov_name):=NA]
                                                combined_data[case==12,eval(cov_date):=NA]
                                              } else {
                                                combined_data[case==12,eval(cov_name):=assigned_L[,get(cov_name)]]
                                                combined_data[case==12,eval(cov_date):=assigned_L[,get(cov_date)]]
                                              }
                                              
                                            }
                                            
                                            # now, it gets a bit more complicated with cases 13 and 14...
                                            # let Z be the number of days in each interval and call the Z-day
                                            # interval before the event 'Zlast' and the preceding Z-day
                                            # interval 'Zpenultimate'
                                            if (combined_data[.N, outcome] == 1) {
                                              combined_data[intnum == .N-2 & exp_change == FALSE &
                                                              part1 == FALSE, case := "13"]
                                              combined_data[intnum == .N-2 & exp_change == TRUE &
                                                              part1 == FALSE, case := "14"]
                                            }
                                            
                                            if(any(combined_data$case %in% "13") | (any(combined_data$case %in% "6") & first_exp_rule==0)){ 
                                              t.int <- combined_data[case==13 | case==6, intnum]
                                              if(t.int!=0){
                                                Zpenultimate <- lubridate::interval(eof_date_this_id-(2*z_days)+1,eof_date_this_id-z_days)
                                                Zlast <- lubridate::interval(eof_date_this_id-z_days+1,eof_date_this_id)
                                                ### L(t) is set to the last measurement in 'Zpenultimate' strictly after the date when L(t-1) is measured; so, modifying Zpenultimate to begin with (L(t-1) date + 1) if L(t) exists
                                                if(!is.na(combined_data[intnum==t.int-1,get(cov_name)])) lubridate::int_start(Zpenultimate) <- combined_data[intnum==t.int-1,get(cov_date)] + 1
                                                ### ATTENTION: Found bug with logic for case 13 - I feel L(t) should be set to the last measurment in 'Zpenultimate' strictly after the date when L(t-1) is measured or strictly after date of the last non-missing measurment
                                                if(any(combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)]%within%Zpenultimate)){
                                                  Zpenultimate.modified.start <- combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)][combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)]%within%Zpenultimate] + 1
                                                  lubridate::int_start(Zpenultimate) <- Zpenultimate.modified.start 
                                                }
                                                if(any(cov_dates_this_id%within%Zpenultimate)){  
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%Zpenultimate],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%Zpenultimate],1)]
                                                } else{
                                                  combined_data[intnum==t.int,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int,eval(cov_name):=NA]
                                                }
                                                
                                                ### L(t+1) assignment
                                                interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int,get(cov_date)]), max(lubridate::as_date(lubridate::int_end(Zpenultimate))+1, combined_data[!is.na(get(cov_date)) & intnum<t.int,][.N,get(cov_date)+1]), combined_data[intnum==t.int,get(cov_date)]+1) ### interval.start is strictly after the date when L(t) is measured, if L(t) is present, or, else,(strictly after the first day the exposure changes(i.e. cov_acute==FALSE) or on or after the first day when the exposure changes(i.e. cov_acute==TRUE))
                                                interval.end <- eof_date_this_id
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==t.int+1,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                } else{
                                                  combined_data[intnum==t.int+1,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=NA]
                                                }
                                              }
                                            }
                                            
                                            if(any(combined_data$case %in% "14") | (any(combined_data$case %in% "8") & first_exp_rule==0)){
                                              t.int <- combined_data[case==14 | case==8, intnum]
                                              if(any(combined_data$case %in% "8") & t.int==0){ ### Particular case for when case==8 and t.int==0
                                                interval.start <- index_date_this_id
                                                if(exp_character){
                                                  At <- combined_data[intnum==t.int,exposure]
                                                  exp_start_j <- exp_data_this_id[get(exp_level)%in%At,get(start_date_var)][1]
                                                  interval.end <- exp_start_j-cov_acute
                                                } else{
                                                  interval.end <- exp_start_this_id[1]-cov_acute
                                                }
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                                ### This case is particular to when t.int==0
                                                cov_dates_this_id_t0_tmp <- c(index_date_this_id,cov_dates_this_id)
                                                cov_values_this_id_t0_tmp <- c(index_cov_this_id,cov_values_this_id)
                                                ### index date and value supersedes any covariate measurement on same day
                                                cov_dates_this_id_t0 <- cov_dates_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                                cov_values_this_id_t0 <- cov_values_this_id_t0_tmp[!duplicated(cov_dates_this_id_t0_tmp)]
                                                if(any(cov_dates_this_id_t0%within%interval.final)){
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id_t0[cov_dates_this_id_t0%within%interval.final],1)]
                                                } else if(interval.start>interval.end) { ## if d=index date and cov_acute=TRUE, then L(t) is set to the measurment on the index date (if this is true then interval.start>interval.end)
                                                  combined_data[intnum==t.int,eval(cov_date):=index_date_this_id]
                                                  combined_data[intnum==t.int,eval(cov_name):=index_cov_this_id]
                                                } else {
                                                  combined_data[intnum==0,eval(cov_date):=NA]
                                                  combined_data[intnum==0,eval(cov_name):=NA]
                                                }
                                                ### L(t+1) assignment
                                                interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int,get(cov_date)]), exp_start_this_id[1]+!cov_acute, combined_data[intnum==t.int,get(cov_date)]+1) ### interval.start is strictly after the date when L(t) is measured, if L(t) is present, or; else,(strictly after the first day the exposure changes(i.e. cov_acute==FALSE) or on or after the first day when the exposure changes(i.e. cov_acute==TRUE))
                                                interval.end <- eof_date_this_id
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==t.int+1,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                }
                                                else{
                                                  combined_data[intnum==t.int+1,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=NA]
                                                }
                                              }
                                              else {
                                                exp0_int <-
                                                  data.table::data.table(exp0Start = exp_data_this_id[, get(end_date_var)] + 1,
                                                                         exp0End = shift(exp_data_this_id[, get(start_date_var) - 1],
                                                                                         n = 1L, fill = eof_date_this_id,
                                                                                         type = "lead"))
                                                exp0_int <-
                                                  rbind(data.table::data.table(exp0Start = index_date_this_id,
                                                                               exp0End = exp_data_this_id[1, get(start_date_var) - 1]),
                                                        exp0_int)
                                                exp0_int <- exp0_int[exp0End>=exp0Start]
                                                
                                                Zlast <- lubridate::interval(eof_date_this_id-z_days+1,eof_date_this_id)
                                                Zpenultimate <- lubridate::interval(eof_date_this_id-(2*z_days)+1,eof_date_this_id-z_days)
                                                d.day <- NA
                                                ### 'd' is the first day when the exposure status changes in the Z-day interval preceding Y
                                                ### Note that if A(t-1)=j (exposed status) and A(t)=0/"not exposed" then the start of non-exposure status is defined as the first day of the Zlast interval when the unit is unexposed to any exposure levels
                                                if(combined_data[intnum==t.int, (exposure == 0 | exposure == exp_ref)]) { 
                                                  #if(any(exp_start_this_id%within%Zlast)) {
                                                  if(any(lubridate::int_start(Zlast)%within%lubridate::interval(exp0_int[,exp0Start],
                                                                                                                exp0_int[,exp0End]))) {
                                                    d.day <- lubridate::int_start(Zlast)
                                                  }
                                                  else {
                                                    d.day <- exp0_int[,exp0Start][exp0_int[,exp0Start]%within%Zlast][1]
                                                  }
                                                  #}
                                                }
                                                if(combined_data[intnum==t.int, (exposure == 1 | exposure != exp_ref)])  {
                                                  if(exp_character){
                                                    A.t <- combined_data[intnum==t.int,exposure]
                                                    if(any(lubridate::int_start(Zlast)%within%lubridate::interval(exp_data_this_id[get(exp_level)==A.t,get(start_date_var)],
                                                                                                                  exp_data_this_id[get(exp_level)==A.t,get(end_date_var)]))) {
                                                      d.day <- lubridate::int_start(Zlast)
                                                    }
                                                    else {
                                                      exp_j_start_this_id <- exp_data_this_id[get(exp_level)==A.t,get(start_date_var)]
                                                      d.day <- exp_j_start_this_id[exp_j_start_this_id%within%Zlast][1]
                                                    }
                                                  } else {
                                                    if(any(lubridate::int_start(Zlast)%within%lubridate::interval(exp_data_this_id[,get(start_date_var)],
                                                                                                                  exp_data_this_id[,get(end_date_var)]))) {
                                                      d.day <- lubridate::int_start(Zlast)
                                                    }
                                                    else {
                                                      d.day <- exp_start_this_id[exp_start_this_id%within%Zlast][1]
                                                    }
                                                  }
                                                }
                                                d.day <- d.day - cov_acute
                                                if(is.na(d.day)) d.day <- int_end(Zpenultimate)  ### If d day doesn't exist then we simply set the interval end to the end of Zpenultimate ### ATTENTION: Discovered that may have to set d.day to last day in Zpenultimate if there is no first day in exposure change in Zlast based on observing GS
                                                Zpenultimate.to.d.day <- lubridate::interval(lubridate::int_start(Zpenultimate),d.day)
                                                ### L(t) is set to the last measurement in 'Zpenultimate' strictly after the date when L(t-1) is measured; so, modifying Zpenultimate to begin with (L(t-1) date + 1) if L(t) exists, or either in 'Zlast' up to and including (cov_acute==FALSE), or not including, day 'd' in Zlast
                                                if(!is.na(combined_data[intnum==t.int-1,get(cov_name)])) lubridate::int_start(Zpenultimate.to.d.day) <- combined_data[intnum==t.int-1,get(cov_date)] + 1
                                                # ### ATTENTION: Found bug with logic for case 14 - I feel L(t) should be set to the last measurment in 'Zpenultimate' strictly after the date when L(t-1) is measured or strictly after date of the last non-missing measurment
                                                if(any(combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)]%within%Zpenultimate)){
                                                  Zpenultimate.modified.start <- tail(combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)][combined_data[!is.na(get(cov_date)) & intnum<t.int,get(cov_date)]%within%Zpenultimate],1) + 1
                                                  lubridate::int_start(Zpenultimate.to.d.day) <- Zpenultimate.modified.start
                                                }
                                                if(any(cov_dates_this_id%within%Zpenultimate.to.d.day)){
                                                  combined_data[intnum==t.int,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%Zpenultimate.to.d.day],1)]
                                                  combined_data[intnum==t.int,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%Zpenultimate.to.d.day],1)]
                                                }
                                                else{
                                                  combined_data[intnum==t.int,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int,eval(cov_name):=NA]
                                                }
                                                
                                                ### L(t+1) assignment
                                                interval.start <- dplyr::if_else(is.na(combined_data[intnum==t.int,get(cov_date)]), max(lubridate::as_date(lubridate::int_end(Zpenultimate.to.d.day))+1, combined_data[!is.na(get(cov_date)) & intnum<t.int,][.N,get(cov_date)+1]), combined_data[intnum==t.int,get(cov_date)]+1) ### interval.start is strictly after the date when L(t) is measured, if L(t) is present, or, else,(strictly after the first day the exposure changes(i.e. cov_acute==FALSE) or on or after the first day when the exposure changes(i.e. cov_acute==TRUE))
                                                interval.end <- eof_date_this_id
                                                interval.final <- lubridate::interval(interval.start,interval.end)
                                                if(any(cov_dates_this_id%within%interval.final)){
                                                  combined_data[intnum==t.int+1,eval(cov_date):=tail(cov_dates_this_id[cov_dates_this_id%within%interval.final],1)]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=tail(cov_values_this_id[cov_dates_this_id%within%interval.final],1)]
                                                }
                                                else{
                                                  combined_data[intnum==t.int+1,eval(cov_date):=NA]
                                                  combined_data[intnum==t.int+1,eval(cov_name):=NA]
                                                }
                                              }
                                            }
                                            
                                            ## subject-specific output
                                            #combined_data[, "case_"%+%cov_name := as.numeric(case)]
                                            combined_data[, case:= as.numeric(case)]
                                            combined_data[, `:=`(exp_change = NULL, part1 = NULL)] #, case = NULL
                                            
                                            return(combined_data)
                                          })  # END: loop over subjects
            ## merge ID-specific data sets
            data_assigned_lt_by_id <- data.table::rbindlist(data_assigned_lt_by_id, fill = TRUE)            
            if(cov_position==1){
              data_assigned_lt <- copy(data_assigned_lt_by_id)
              data.table::setcolorder(data_assigned_lt, c(id_var, "intnum", "intstart", "intend",
                                                          "exposure", "outcome", "censor",
                                                          "caseExp", eof_type_var, "case", cov_name, cov_date
              ))
              data.table::setnames(data_assigned_lt, cov_date,
                                   paste0("dt", cov_name))
            }else{
              data_assigned_lt_by_id <- data_assigned_lt_by_id[,c(id_var,"intnum",cov_name,cov_date),with=FALSE]
              data.table::setnames(data_assigned_lt_by_id, cov_date,
                                   paste0("dt", cov_name))
              data_assigned_lt <- merge(data_assigned_lt,data_assigned_lt_by_id,by=c(id_var,"intnum"),all=TRUE)
            }
          }  # END: loop over covariates
        } else {
          data_assigned_lt <- outcome_data
        }
    
      ## For loop for time-indepent
      
      data.table::setkeyv(data_assigned_lt, c(id_var,"intnum"))
      
      # Assign time-independent covariates
      t_ind_cov_data <- cohort_data[,c(id_var,t_ind_cov),with=FALSE]
      data_assigned_lt <- merge(data_assigned_lt, t_ind_cov_data, by=id_var, all.x = TRUE, all.y=FALSE)
      for(cov.i in t_ind_cov){
        data_assigned_lt[get(cov.i)%in%"",eval(cov.i):=NA]
      }

      # output
      private$.data <- data_assigned_lt
      invisible(self)
    },
    imputeL = function() {
        ## identify all time-dep and time-indep covariates
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
          covariate_data <- c(private$.cov_data,private$.cohort_data$L0_timeIndep)
        } else{
          covariate_data <- private$.cohort_data$L0_timeIndep
        }
        #covariate_data <- c(private$.cov_data,private$.cohort_data$L0_timeIndep)

        ## Only sporadic time-dep and time-indep variables first need baseline imputation and imputation indicators
        ## All time-indep variables need LOVCF except "indicator" variables
        for(cov.i in seq_along(covariate_data)){
            cov_name <- names(covariate_data)[cov.i]
            ## Baseline imputation and creation of imputation indicator
            if(is.null(covariate_data[[cov.i]]$type) || covariate_data[[cov.i]]$type=="sporadic"){ # null is only possible for time-indep vars
                ## add indicator of imputation to output data
                private$.data[,"I."%+%cov_name:=0]
                private$.data[is.na(get(cov_name)),eval("I."%+%cov_name):=1]
                ## figure out baseline imputation value
                impute <- covariate_data[[cov.i]]$impute
                if(impute=="default") {
                    imputed_value <- covariate_data[[cov.i]]$impute_default_level
                } else if(impute=="mode"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),getmode(get(cov_name))]
                } else if(impute=="mean"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),mean(get(cov_name))]
                } else if(impute=="median"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),median(get(cov_name))]
                }
                ## impute baseline values 
                private$.data[intnum==0 & is.na(get(cov_name)) , eval(cov_name):=imputed_value]
            }
            ## Implement LOVCF 
            if(is.null(covariate_data[[cov.i]]$type) || covariate_data[[cov.i]]$type!="indicator"){ # null is only possible for time-indep vars
                private$.data[, eval(cov_name):= get(cov_name)[1], by = .(get(private$.cohort_data$IDvar), cumsum(!is.na(get(cov_name)))) ]
                ## private$.data[,eval(cov_name):=zoo::na.locf(get(cov_name)),by=id_var]
            }else{
                if(private$.data[ , class(get(cov_name)) ]=="character")private$.data[ is.na(get(cov_name)) , eval(cov_name) := "None" ]
                else private$.data[ is.na(get(cov_name)) , eval(cov_name) := 0 ]
            }
        }
        ## reorder data before exiting
        data.table::setkeyv(private$.data, c(private$.cohort_data$IDvar,"intnum"))
        invisible(self)
    },
    cleanUp = function(format, dates){

        ## Create object that will be used to reorder covariates in a standardized fashion
        timeIndep <- names(private$.cohort_data$L0_timeIndep)
        names(timeIndep) <- timeIndep
        I.timeIndep <- "I."%+%timeIndep
        names(I.timeIndep) <- timeIndep%+%".1"
        timeIndepOrdered <- as.character(c(timeIndep,I.timeIndep)[sort(names(c(timeIndep,I.timeIndep)))])
            
        timeDep <- names(private$.cov_data)
        names(timeDep) <- timeDep
        dt.timeDep <- "dt"%+%timeDep
        names(dt.timeDep) <- timeDep%+%".1"
        I.timeDep <- "I."%+%timeDep
        names(I.timeDep) <- timeDep%+%".2"
        timeDepOrdered <- as.character(c(timeDep,dt.timeDep,I.timeDep)[sort(names(c(timeDep,dt.timeDep,I.timeDep)))])
        timeDepOrdered <- timeDepOrdered[timeDepOrdered%in%names(private$.data)]
        orderedCol <- c(private$.cohort_data$IDvar,"intnum","intstart","intend",private$.cohort_data$EOF_type,"outcome","censor",timeIndepOrdered,timeDepOrdered,"exposure")
        if(all(c("tie","A0.warn")%in%names(private$.data)))orderedCol <- c(orderedCol,"tie","A0.warn")
        
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
          assert_that( all(sort(names(private$.data)[ !names(private$.data)%in%orderedCol ])==c("case","caseExp")) , msg = "some columns might need exporting")
        } else{
          assert_that( all(sort(names(private$.data)[ !names(private$.data)%in%orderedCol ])==c("case")) , msg = "some columns might need exporting")
        }
        #assert_that( all(sort(names(private$.data)[ !names(private$.data)%in%orderedCol ])==c("case","caseExp")) , msg = "some columns might need exporting")
        
        ## Remove unneeded columns
        private$.data <- suppressWarnings(private$.data[ , -c("caseExp","case"), with=FALSE])
        if(!dates){
            col2remove <- unlist(lapply(private$.data,class))=="Date"
            col2remove <- names(col2remove)[col2remove]
            private$.data <- private$.data[ , -col2remove, with=FALSE]
            orderedCol <- orderedCol[!orderedCol%in%c("intstart","intend",dt.timeDep)]
        }

        ## Reformat
        if(format=="MSM SAS macro"){
            ## remove patients censored in first f/up interval
            IDsCensoredBin0 <- private$.data[ censor==1 & intnum==0, unique(get(private$.cohort_data$IDvar)) ]
            private$.data <- private$.data[ !get(private$.cohort_data$IDvar)%in%IDsCensoredBin0,  ]
            ## shift outcome and censor up by one row
            private$.data[ , outcome := shift(outcome, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            private$.data[ , censor := shift(censor, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            ## remove last row of each ID
            private$.data <- private$.data[ , .SD[-.N,] , by = get(private$.cohort_data$IDvar) ][, -"get", with=FALSE ]
            ## set outcome to NA in last row when censor = 1
            private$.data[ censor %in% 1, outcome := NA ]
        }
        
        ## Reorder data before exiting
        setcolorder(private$.data, orderedCol)
        data.table::setkeyv(private$.data, c(private$.cohort_data$IDvar,"intnum"))
        invisible(self)
    }
  )
)
