#' creates default input values for data file creation
#'
#' You must run this function to set up the list needed to pass to \code{\link{create_datpin_files}}.
#' Once created any of the values can be changes to suit the intended model run
#'
#'@param scenarioFlag Character string: Speciying a scenario of either "historical" or "assessment". Default = "historical"
#'@param scenarioType Character string: Type of assessment scenario. "fixed" or "ramp". (Only used if scenarioFlag is "assessment"). Default = "fixed"
#'@param maxExploitationRate Integer: The level of the maximum exploitation in either "ramp" or "fixed". Valid choices are 5,10,15,20,25,30.(Only used if scenarioFlag is "assessment"). Default value = 30
#'@param assessmentSpeciesFlag Character string: The level of species protection "none", "low", "high". (Only used if scenarioFlag is "assessment").
#' "none" = specific species are not protected during an assessment, "low" = species are protected at same level as the complex, "high" = elasmobranchs get increased protection".
#' Default = "none"
#' @param outputFilename Character string: Name of the (.dat and .pin) output files. Default = "hydra_sim"
#' @param temperatureFlag Character string: Determins whether to use the observed temperature data (true) or the mean (mean). Default = "true"
#' @param fillLengthInteger number of character that exist on a line in the output files. If too small. The data wrap to following line. This causes an error at run time. Default = 2000
#' @param outDir Character string: Path to where output will be saved. Default = current working directory
#'
#'
#'
#' @return A list containing the same variables
#'
#'@section Usage:
#'
#'Run this function to initially set up the list. If any changes are needed, make them.
#'If any other changes are requrired to other parts of the input data, also make those changes,
#'then run \code{\link{create_datpin_files}} to generate the .dat and .pin files required for the model run.
#'
#'
#' @export

setup_default_inputs <- function(outDir = getwd(),scenarioFlag="historical",temperatureFlag="true",scenarioType="fixed",maxExploitationRate=30,
                                 assessmentSpeciesFlag="none",outputFilename="hydra_sim",fillLength=2000){

  listOfParameters <- list()
  listOfParameters$scenarioFlag <- scenarioFlag
  listOfParameters$temperatureFlag <- temperatureFlag
  listOfParameters$scenarioType <- scenarioType
  listOfParameters$maxExploitationRate <- maxExploitationRate
  listOfParameters$assessmentSpeciesFlag <- assessmentSpeciesFlag
  listOfParameters$outputFilename <- outputFilename
  listOfParameters$fillLength <- fillLength # length of line to write to. if not long enough data wraps to next line
  listOfParameters$outDir <- outDir

  return(listOfParameters)

}
