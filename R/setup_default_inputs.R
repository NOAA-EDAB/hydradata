#' creates default inputs for use with \code{\link{create_data_files}}
#'
#' You must run this function to set up the list needed to pass to \code{\link{create_data_files}}.
#' Once created any of the values can be changes to suit the intended model run
#'
#'
#' @return A list containing:
#'
#' \item{scenarioFlag}{Character string: Speciying a scenario of either "historical" or "assessment". Default = "historical"}
#' \item{scenarioType}{Charachter string: Type of assessment scenario. "fixed" or "ramp". (Only used if scenarioFlag is "assessment"). Default = historical"}
#' \item{exploitationRate}{Integer: The level of the maximum exploitation in either "ramp" or "fixed". (valid choices c(5,10,15,20,25,30)).(Only used if scenarioFlag is "assessment") Default value = 30}
#' \item{assessmentSpeciesFlag}{Charachter string: The level of species protection "none", "low", "high". (Only used if scenarioFlag is "assessment").
#' "none" = specific species are not protected during an assessment, "low" = species are protected at same level as the complex, "high" = elasmobranchs get increased protection".
#' Default = "none" }
#' \item{outputFilename}{Characher string: Name of the (.dat and .pin) output files. Default = "hydraSim" }
#' \item{temperatureFlag}{Charachter string: Determins whether to use the observed temperature data (true) or the mean (mean)}
#' \item{fillLength}{Integer: number of character that exist on a line in the output files. If too small. The data wrap to following line. This causes an error at run time}
#'
#' @export

setup_default_inputs <- function(){

  listOfParameters <- list()
  listOfParameters$scenarioFlag <- "historical"
  listOfParameters$temperatureFlag <- "true"
  listOfParameters$scenarioType <- "fixed"
  listOfParameters$exploitationRate <- 30
  listOfParameters$assessmentSpeciesFlag <- "none"
  listOfParameters$outputFilename <- "hydra_sim"
  listOfParameters$fillLength <- 2000 # length of line to write to. if not long enough data wraps to next line

  return(listOfParameters)

}
