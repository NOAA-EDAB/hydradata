# Calls each plotting routine in order to plot specific set of inputs used to run Hydra
#
#This is an internal function that isn't exported
#



main_plot_inputs <- function(outPath,inputData){
  ######################## plot observational data #############################
  ##############################################################################
  plot_Y1N_Biomass_lengthweight_bins(outPath,inputData)
  plot_Temperature(outPath,inputData)
  plot_CovariateData(outPath,inputData)
  plot_observed_Biomass_Catch(outPath,inputData) # currently from atlantis
  #plot_Catch() # currently from atlantis

  ######################### plots fishing stuff ################################
  ##############################################################################
  plot_Selectivities(outPath,inputData)
  plot_FishingEffort(outPath,inputData) # exploitation, fishery_q, effort
  #plot_Discards_Survival(outPath)

  ########################## plots biological stuff #############################
  ##############################################################################
  plot_Growth(outPath,inputData)
  plot_Fecundity(outPath,inputData)
  plot_Maturity(outPath,inputData)
  plot_StockRecruitment(outPath,inputData)
  plot_Intake(outPath,inputData)
  plot_Foodweb(outPath,inputData)
  plot_SizePreference_weightRatio(outPath,inputData) & # plot_weightRatio

  ########################### plots management stuff #############################
  ##############################################################################
  plot_Assessment_Step(outPath,inputData)
  #plot_ErrorSources()
  #plot_q_Assessment() # which species effect change in Explitation rate
}
