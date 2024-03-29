#' @export
`+.LtAtObject` <- function(spec1, spec2) {
  if (any(c(class(spec1), class(spec2)) %in% "LtAtData")) {
    if (("LtAtData" %in% class(spec1) & any(c("cohortData", "expData", "timeDepCovData") %in% class(spec2))) | ("LtAtData" %in% class(spec2) & any(c("cohortData", "expData", "timeDepCovData") %in% class(spec1)))) {
      if ("LtAtData" %in% class(spec1)) {
          if("instExpData"%in%class(spec2) & !"LtAtDataInst"%in%class(spec1))spec1 <- LtAtDataInst$new()$copyLtAtData(spec1)
          return(spec1$addSpec(spec2))
      } else {
          if("instExpData"%in%class(spec1) & !"LtAtDataInst"%in%class(spec2))spec2 <- LtAtDataInst$new()$copyLtAtData(spec2)
          return(spec2$addSpec(spec1))
      }
    } else {
      stop("Cannot add an object of class LtAtData with an object that is not of class cohortData, expData, or timeDepCovData.")
    }
  } else {
    if (any(c("cohortData", "expData", "timeDepCovData") %in% class(spec1)) & any(c("cohortData", "expData", "timeDepCovData") %in% class(spec2))) {
        if("instExpData"%in%c(class(spec1),class(spec2))){
            res <- LtAtDataInst$new()
            attributes(res)$class <- c(attributes(res)$class, "LtAtObject") # required so '+.LtAtObject' can operate on it later on with another object that might not be of class LtAtObject
            return(res$addSpec(spec1)$addSpec(spec2))
        }
        else{
            res <- LtAtData$new()
            attributes(res)$class <- c(attributes(res)$class, "LtAtObject") # required so '+.LtAtObject' can operate on it later on with another object that might not be of class LtAtObject
            return(res$addSpec(spec1)$addSpec(spec2))
        }
    } else {
      stop("Cannot add an object of class cohortData, expData, or timeDepCovData with an object of another class.")
    }
  }
}

#' Definition of cohort dataset
#'
#' The cohort dataset specifies for each subject in the cohort:
#' 1) a unique subject identifier,
#' 2) the date of study entry,
#' 3) the date of end of follow-up,
#' 4) the reason for end of follow-up (failure or right-censoring), and
#' 5) baseline measurements of time-dependent or time-independent covariates.
#'
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{cohortData} object
#'
#' @param data \code{data.table} containing the input cohort dataset to be
#'             wrapped in and processed. The table must contain a single row for
#'             each subject in the cohort. Cannot have columns named 'IDvar',
#'             'index_date', 'EOF_date', 'EOF_type', or 'L0'.
#'
#' @param IDvar \code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'
#' @param index_date \code{character} providing the name of the column of
#'            \code{data} that contains the date of study entry.
#'
#' @param EOF_date \code{character} providing the name of the column of
#'            \code{data} that contains the date of end of follow-up. All observations
#'           with the end of follow-up date equal to the study entry date will be ignored
#'           (i.e., excluded from the cohort).
#'
#' @param EOF_type \code{character} providing the name of the column of
#'            \code{data} corresponding to the reason for end of follow-up.
#'
#' @param Y_name \code{character} or \code{integer}  providing the unique value
#'           in column \code{EOF_type} that encodes the end of follow-up due to failure
#'           (i.e., occurrence of the outcome event of interest).
#'
#' @param L0 vector of \code{character} providing the names of the columns of
#'           \code{data} that contain baseline covariate measurements.
#' 
#' @param L0_timeIndep named list specifying, for each time-independent covariates in
#'        \code{L0}, a sublist with only the following three named elements:
#'        \enumerate{
#'        \item \code{categorical}: specifies whether the covariate is
#'        continuous ('FALSE') or categorical ('TRUE'). Cannot be missing.
#'        \item \code{impute}: specifies the imputation method for missing
#'        measurements: 'default', 'mean', 'mode', 'median'. If missing, imputation with
#'        the 'mean' and 'mode' is used for continuous and categorical covariates,
#'        respectively. Imputation with 'mean', 'mode', or 'median' is based on
#'        measurements from subjects with observed covariate values in \code{data}.
#'        'mean' and 'median'
#'        can only be used for continuous covariates. 'mode' can only be used for
#'        categorical covariates. Imputation with 'default' replaces missing values with
#'        0 if the covariate is numeric and with 'Unknown' otherwise.
#'        \item \code{impute_default_level} imputation value to be used when
#'        the imputation method is 'default'. The value must be
#'        a length 1 \code{character} (resp. \code{numeric}) for a covariate encoded by a
#'        \code{character} (resp. \code{numeric}) vector.
#'        If missing, the default values 0 and 'Unknown' are used for continuous and
#'        categorical covariates, respectively.
#'        }
#'        Each element of the list \code{L0_timeIndep} must be named with the time-independent
#'        covariate in \code{L0} to which the sublist information applies.

#' 
#' @seealso [cohortData]
#'
#' @examples
#' cohort <- setCohort(cohortDT, "ID", "IndexDate", "EOFDate", "EOFtype",
#'                     "AMI", c("ageEntry", "sex", "race", "A1c", "eGFR"),
#'                     list("ageEntry"=list("categorical"=FALSE,
#'                                          "impute"=NA,
#'                                          "impute_default_level"=NA),
#'                          "sex"=list("categorical"=TRUE,
#'                                     "impute"=NA,
#'                                     "impute_default_level"=NA),
#'                          "race"=list("categorical"=TRUE,
#'                                      "impute"=NA,
#'                                      "impute_default_level"=NA)) ) 
setCohort <- function(data, IDvar, index_date, EOF_date, EOF_type, Y_name, L0, L0_timeIndep=NA) {
  outData <- cohortData$new(data, IDvar, index_date, EOF_date, EOF_type,
                            Y_name, L0, L0_timeIndep)
  attributes(outData)$class <- c(attributes(outData)$class, "LtAtObject")
  return(outData)
}

#' Definition of exposure dataset
#'
#' The exposure dataset specifies all follow-up time intervals during which a
#' study subject is exposed to an exposure level other than a reference level
#' specified by the analyst. For each episode of exposure to a non-reference
#' level, the table specifies:
#' 1) a unique subject identifier,
#' 2) the exposure episode start date,
#' 3) the exposure episode end date, and
#' 4) the non-reference exposure level (required only when the exposure is not
#'    binary).
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{expData} object
#'
#' @param data \code{data.table} containing the input exposure dataset to be
#'             wrapped in and processed. The table can contain multiple rows per
#'             subject. There should be no row for subjects who are only
#'             exposed to the reference exposure level during follow-up.
#'             Exposure levels must be encoded by a character vector or an
#'             integer vector. There cannot be any overlapping exposure
#'             episodes. Cannot contain missing values. Cannot have columns
#'             named 'IDvar', 'start_date', 'end_date', 'exposure', or 'exp_level'.
#'
#' @param IDvar \code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'
#' @param start_date \code{character} providing the name of the column of
#'            \code{data} that contains the exposure episode start date.
#'
#' @param end_date \code{character} providing the name of the column of
#'            \code{data} that contains the exposure episode end date.
#'
#' @param exp_level \code{character} providing the name of the column of
#'            \code{data} that contains the non-reference exposure level.
#'            Can be missing only if \code{exp_ref} is also missing. If missing,
#'            the exposure is assumed to be binary and its reference level is
#'            encoded by 0.
#'
#' @param exp_ref \code{character} or \code{integer} identifying the exposure
#'                reference level. Cannot be a value of the exp_level column of
#'                data. Can be missing only if \code{exp_level} is also missing.
#'
#' @seealso [expData]
#'
#' @examples
#' exposure <- setExposure(expDT, "ID", "startA", "endA")
setExposure <- function(data, IDvar, start_date, end_date, exp_level = NA,
                        exp_ref = NA) {
  outData <- expData$new(data, IDvar, start_date, end_date, exp_level,
                         exp_ref)
  attributes(outData)$class <- c(attributes(outData)$class, "LtAtObject")
  return(outData)
}

#' Definition of instant exposure dataset
#'
#' The instant exposure dataset specifies all follow-up dates when a
#' study subject is exposed to a non-reference exposure level.
#' For each instanteneous exposures, the table specifies:
#' 1) a unique subject identifier,
#' 2) the exposure date,
#' 3) the exposure level (required only when the exposure is not
#'    binary).
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{instExpData} object
#'
#' @param data \code{data.table} containing the input exposure dataset to be
#'             wrapped in and processed. The table can contain multiple rows per
#'             subject. There should be no row for subjects who are only
#'             exposed to the reference exposure level during follow-up.
#'             Exposure levels must be encoded by one or more character, 
#'             integer, or numeric vector(s). Multiple rows per subject and date
#'             are permitted. Cannot contain missing values. Cannot have columns
#'             named 'IDvar', 'start_date', or 'exp_level'.
#'
#' @param IDvar \code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'
#' @param exp_date \code{character} providing the name of the column of
#'            \code{data} that contains the exposure date.
#'
#' @param exp_level \code{character} vector providing the name(s) of the column(s) of
#'            \code{data} that encode(s) the non-reference exposure level(s).
#'            Can be missing if there is only one non-reference exposure level. If missing,
#'            the exposure is assumed to be binary and its reference level is
#'            encoded by 0.
#'
#' @seealso [instantExpData]
#'
#' @examples
#' exposure <- setInstantExposure(expDT6, "ID", "fillDate", c("D.t","Q.t"))
setInstantExposure <- function(data, IDvar, exp_date, exp_level = NA) {
  outData <- instExpData$new(data, IDvar, exp_date, exp_level)
  attributes(outData)$class <- c(attributes(outData)$class, "LtAtObject")
  return(outData)
}

#' Definition of a time-dependent covariate dataset
#'
#'
#' A time-dependent covariate dataset specifies all follow-up measurements of
#' a single variable. For each subject in the cohort, the table specifies:
#' 1) a unique subject identifier,
#' 2) the date of the covariate measurement,
#' 3) the covariate value.
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{timeDepCovData} object
#'
#' @param data \code{data.table} containing an input covariate dataset to be
#'             wrapped in and processed. The table can contain multiple rows per
#'             subject. There should be no row for subjects with no follow-up
#'             covariate measurements. The table must contain at most one
#'             covariate measurement on a given date for any given subject.
#'             Cannot contain missing values. Covariate values must be encoded
#'             by a character or numeric vector (e.g., factors are not allowed).
#'             Cannot have columns named 'IDvar', 'L_date', or 'L_name'.
#'
#' @param type \code{character} specifying the covariate type: 'binary monotone
#'             increasing' (e.g., history of a diagnosis or procedure),
#'             'interval' (e.g., hospital stay or prescription coverage),
#'             'sporadic' (e.g., laboratory measurements), 'indicator' (e.g.,
#'             occurrence of a repeatable event).
#'
#' @param IDvar \code{character} providing the name of the column of \code{data}
#'              that contains the unique subject identifier.
#'
#' @param L_date \code{character} providing the name of the column of
#'               \code{data} that contains the date of the follow-up covariate
#'               measurement.
#'
#' @param L_name \code{character} providing the name of the column of
#'               \code{data} that contains the covariate values. For all
#'               covariate types, \code{L_name} must also be the name of the
#'               column of the cohort dataset that contains the baseline
#'               measurements of the time-dependent covariate. Baseline
#'               measurements of a covariate of type 'binary monotone
#'               increasing' can only be encoded with values 0 and 1 in the
#'               cohort dataset. All values in the column \code{L_name} of
#'               \code{data} must be set to 1 for a covariate of type 'binary
#'               monotone increasing'. The column \code{L_name} in \code{data}
#'               cannot contain the value 'None' for a covariate of type
#'               'indicator' that is \code{character}. The column \code{L_name}
#'               in \code{data} and in the cohort dataset cannot contain the
#'               value 0 for a covariate of type 'indicator' that is
#'               \code{numeric}.
#'
#' @param categorical \code{logical} indicating whether the covariate is
#'                    continuous ('FALSE') or categorical ('TRUE'). Must be
#'                    'TRUE' for a covariate of type 'binary monotone
#'                    increasing' or 'indicator'. Cannot be missing.
#'
#' @param impute \code{character} specifying imputation method for missing
#'               baseline measurements: 'default', 'mean', 'mode', 'median'. If
#'               missing, imputation with the 'mean' and 'mode' is used for
#'               continuous and categorical covariates, respectively. Imputation
#'               with 'mean', 'mode', or 'median' is based on baseline
#'               measurements from subjects with observed baseline covariate
#'               values (stored in the cohort dataset). 'mean' and 'median' can
#'               only be used for continuous covariates. 'mode' can only be used
#'               for categorical covariates. Imputation with 'default' replaces
#'               missing values with 0 if the covariate is numeric and with
#'               'Unknown' otherwise. Ignored for a covariate of type 'binary
#'               monotone increasing', 'interval', or 'indicator'.
#'
#' @param impute_default_level \code{character} or \code{numeric} specifying the
#'          imputation value to be used when \code{impute}='default'. The value
#'          must be a length 1 \code{character} (resp. \code{numeric}) for a
#'          covariate encoded by a \code{character} (resp. \code{numeric})
#'          vector. If missing, the default values 0 and 'Unknown' are used for
#'          continuous and categorical covariates, respectively. Ignored for a
#'          covariate of type 'binary monotone increasing', 'interval', or
#'          'indicator'.
#'
#' @param acute_change \code{logical} indicating whether a covariate measurement
#'                     collected on the date of an exposure change can be
#'                     impacted by the change. The default value 'FALSE'
#'                     indicates that the covariate measurement can be assumed
#'                     to have preceded and possibly triggered the change in
#'                     exposure. Cannot be missing.
#'
#' @seealso [timeDepCovData]
#'
#' @examples
#' covariate1 <- setCovariate(a1cDT, "sporadic", "ID", "A1cDate", "A1c",
#'                            categorical = FALSE)
#' covariate2 <- setCovariate(egfrDT, "sporadic", "ID", "eGFRDate", "eGFR",
#'                            categorical = TRUE)
setCovariate <- function(data, type, IDvar, L_date, L_name, categorical,
                         impute = NA, impute_default_level = NA,
                         acute_change = FALSE) {
  outData <- timeDepCovData$new(data, type, IDvar, L_date, L_name, categorical,
                                impute, impute_default_level,
                                acute_change)
  attributes(outData)$class <- c(attributes(outData)$class, "LtAtObject")
  return(outData)
}


#' Main function of the package 
#'
#' Implements a coarsening algorithm to map input exposure and covariate data
#' into a structured analytic data set that encodes complex, discrete-time,
#' longitudinal data. \cr
#' The encoded data on each experimental unit is a temporally ordered sequence
#' of covariates L(t) and exposures A(t) where covariates at time t occur
#' before the exposure A(t) a time t:\cr
#'                       O=(L(0),A(0),...,L(K),A(K),L(K+1)),  \cr
#' where each t represents one of the consecutive time intervals that result
#' from discretizing, separately for each experimental unit, the elapsed time
#' between an index date and end of follow-up date using a fixed unit of time
#' specified by the analyst. \cr
#' In particular, the resulting data set can be used for the evaluation of
#' the causal effects of various exposure regimens on a time-to-event
#' (survival) outcome subject to right-censoring in a cohort study.
#'
#' @export
#'
#' @keywords function
#'
#' @return \code{data.table} object
#'
#' @param LtAtspec specifies an object of class \code{LtAtData} created by
#'                 assembling cohort (required), exposure (required), and
#'                 time-dependent covariate (optional) data objects.
#'                 Cannot be missing.
#'
#' @param time_unit specifies the unit of time (expressed in days) for
#'                  discretizing follow-time into consecutive time intervals
#'                  of the same length. Must specify a strictly positive
#'                  integer value. Cannot be missing.
#'
#' @param format specifies the format of the output data set. Current possible
#'               values are "standard", or "MSM SAS macro". Default value is
#'               "standard". 
#'
#' @param dates indicates whether the output dataset should contain dates.
#'              Default value is FALSE.
#'
#' @param first_exp_rule specifies a binary value that indicates which of
#'                       two algorithms is used when the exposure is defined using
#'                       \code{setExposure}
#'                       to assign
#'                       the first change in exposure status during follow-up.
#'                       Default value is 1. Can be missing (or will be ignored)
#'                       if the exposure is defined using
#'                       \code{setInstantExposure}.
#'
#' @param exp_threshold specifies a threshold used to assign an exposure level
#'                      at each time interval when the exposure is defined using
#'                      \code{setExposure}. Must be a value
#'                      between 0 and 1. Default value is 0.5.
#'                      Can be missing (or will be ignored)
#'                      if the exposure is defined using
#'                      \code{setInstantExposure}.
#'
#' @param max_exp_var sets the limit for the maximum number of exposure
#'                    variables that is expected to be created by the routine
#'                    to encode the exposure levels when the exposure is defined using
#'                    \code{setInstantExposure}.
#'                    An error will be
#'                    produced
#'                    if the input exposure dataset requires the creation of a larger
#'                    than expected number of
#'                    exposure variables. 
#'                    Default value is 100. Must be an integer.
#'                    Can be missing (or will be ignored)
#'                    if the exposure is defined using
#'                    \code{setExposure}.
#'
#' @param max_cov_var sets the limit for the maximum number of 
#'                    variables that is expected to be created by the routine
#'                    to encode the levels of each time-dependent covariate
#'                    when the exposure is defined using \code{setInstantExposure}.
#'                    An error will be
#'                    produced
#'                    if an input time-dependent covariate dataset requires the
#'                    creation of a larger
#'                    than expected number of
#'                    covariate variables. 
#'                    Default value is 100. Must be an integer.
#'                    Can be missing (or will be ignored)
#'                    if the exposure is defined using
#'                    \code{setExposure}.
#' 
#' @param summary_cov_var indicates the coarsening method applied in each interval
#'                    to summarize multiple measurements of a
#'                    time-dependent covariate into a single summary measure.
#'                    Current possible values are "last" or "all". "last" will result
#'                    in retaining only the last observed measurement of the covariate in
#'                    each interval. "all" will result in retaining all observed measurements.
#'                    Default value is "last". 
#' 
#' @seealso [LtAtData]
#' 
#' @examples
#' ## Define one cohort dataset, one exposure dataset, and one or more covariate
#' ## datasets
#' cohort <- setCohort(cohortDT, "ID", "IndexDate", "EOFDate", "EOFtype",
#'                     "AMI", c("ageEntry", "sex", "race", "A1c", "eGFR"),
#'                     list("ageEntry"=list("categorical"=FALSE,
#'                                          "impute"=NA,
#'                                          "impute_default_level"=NA),
#'                          "sex"=list("categorical"=TRUE,
#'                                     "impute"=NA,
#'                                     "impute_default_level"=NA),
#'                          "race"=list("categorical"=TRUE,
#'                                      "impute"=NA,
#'                                      "impute_default_level"=NA)) )
#' exposure <- setExposure(expDT, "ID", "startA", "endA")
#' covariate1 <- setCovariate(a1cDT, "sporadic", "ID", "A1cDate", "A1c",
#'                            categorical = FALSE)
#' covariate2 <- setCovariate(egfrDT, "sporadic", "ID", "eGFRDate", "eGFR",
#'                            categorical = TRUE)
#'
#' ## Gather each input dataset into a single object that specifies the content of
#' ## the output dataset to be constructed
#' LtAt.specification <- cohort + exposure + covariate1 + covariate2
#'
#' ## Construct the output dataset
#' LtAt.data <- construct(LtAt.specification, time_unit = 15, first_exp_rule = 1,
#'                        exp_threshold = 0.75)
#' 
construct <- function(LtAtspec, time_unit, ...) {

    assert_that("LtAtData"%in%class(LtAtspec) , msg = "LtAtspec must be an object of class LtAtData" )
    assert_that( (!"logical"%in%class(LtAtspec$cohort_data) & !"logical"%in%class(LtAtspec$exp_data)), 
                msg = if("logical"%in%class(LtAtspec$cohort_data) & "logical"%in%class(LtAtspec$exp_data)){
                  "LtAtspec must include a valid input cohort and exposure dataset"} 
                else if("logical"%in%class(LtAtspec$cohort_data)){
                  "LtAtspec must include a valid input cohort dataset"} 
                else if ("logical"%in%class(LtAtspec$exp_data)){
                  "LtAtspec must include a valid input exposure dataset"
                })

    LtAtspec$construct(time_unit, ...)
    return(LtAtspec$data)
}

