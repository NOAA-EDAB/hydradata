#' Calls each plotting routine in order to plot specific set of inputs used to run Hydra
#'
#'This is an internal function that isn't exported
#'



main_plot_inputs <- function(outPath){
  ######################## plot observational data #############################
  ##############################################################################
  plot_Y1N_Biomass_lengthweight_bins(outPath)
  plot_Temperature(outPath)
  plot_CovariateData(outPath)
  plot_observed_Biomass_Catch(outPath) # currently from atlantis
  #plot_Catch() # currently from atlantis

  ######################### plots fishing stuff ################################
  ##############################################################################
  plot_Selectivities(outPath)
  plot_FishingEffort(outPath) # exploitation, fishery_q, effort
  #plot_Discards_Survival(outPath)

  ########################## plots biological stuff #############################
  ##############################################################################
  plot_Growth(outPath)
  plot_Fecundity(outPath)
  plot_Maturity(outPath)
  plot_StockRecruitment(outPath)
  plot_Intake(outPath)
  plot_Foodweb(outPath)
  plot_SizePreference_weightRatio(outPath) & # plot_weightRatio

  ########################### plots management stuff #############################
  ##############################################################################
  plot_Assessment_Step(outPath)
  #plot_ErrorSources()
  #plot_q_Assessment() # which species effect change in Explitation rate
}
