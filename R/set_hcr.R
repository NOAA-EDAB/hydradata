#' sets up Harvest Control Rules (HCR)
#'
#' Harvest control rules are evaluated using either a step function or a continuous linear function. For a continuous function all that is required
#' is \code{minMaxThreshold} and \code{minMaxExploitation}. For a step function, \code{Nthresholds}, \code{thresholds}, \code{exploitationOptions} are also required.
#' This function with create the step function \code{thresholds} and \code{exploitationOptions} given \code{minMaxThreshold}, \code{minMaxThreshold} and \code{Nthresholds}
#'
#' @param currentData List: The current hydraData in which changes need to be made
#' @param Nthresholds Numeric scalar: Number of thresholds in the step function (Default = 5)
#' @param minMaxThresholds Numericric vector (1x2): Specifying the minimum and maximum threshold levels (proportion of B0) for management
#' @param minMaxExploitations Numeric vector (1x2): Specifying the minimum and maximum exploitation levels at \code{minMaxThresholds}
#' @param increment Numeric Scalar: increment to evaluate the number of scenarios between \code{minMaxExploitations}
#'
#' @return A list identical to \code{\link{hydraDataList}} with the fields below updated
#'
#' @section Updated fields:
#'
#' \code{exploitationOptions} - Numeric matrix: (\code{Nthresholds} x NScenarios). Range of possible scenarios for harvest control rule
#'
#' \code{thresholds} - Numeric vector (1x \code{Nthresholds}): Specifying the threshold levels for which a change in exploitation will occur
#'
#' \code{Nthresholds} - Numeric scalar: Specifying the number of thesholds
#'
#' \code{minMaxThresholds} - Numeric vector (1x2): Specifying the minimum and maximum threshold levels (proportion of B0) for management
#'
#'
#' @section Usage:
#'
#'Run this function to change the default set of step function scenarios.
#'
#'
#' @export

set_hcr <- function(currentData=hydradata::hydraDataList, Nthresholds=5, minMaxThresholds=c(0.1,0.4), minMaxExploitations=c(0.05,0.4), increment=.05) {

  # update thresholds used
  currentData$thresholds <- c(seq(from=minMaxThresholds[1],to=minMaxThresholds[2],length.out=Nthresholds-1),1e06)
  currentData$Nthresholds <- Nthresholds
  currentData$minMaxThresholds <- minMaxThresholds

  # update the matrix of exploitations at eaxch threshold
  exploitations <- seq(minMaxExploitations[1],minMaxExploitations[2],by=increment)
  nExploitations <- length(exploitations)
  exploitationMat <- matrix(NA,nrow=Nthresholds,ncol = nExploitations)
  for (imax in 1:nExploitations) {
    stepRamp <- seq(from=minMaxExploitations[1], to=exploitations[imax],length.out=Nthresholds)
    exploitationMat[,imax] <- stepRamp
  }
  currentData$exploitationOptions <- exploitationMat

  return(currentData)

}
