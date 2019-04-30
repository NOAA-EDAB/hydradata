#' Create data files (.dat and .pin) for use with multispecies model, Hydra.
#'
#' Initial data files have been lazily loaded. To view their contents please run the \code{\link{create_documentation}} rmd file
#'
#'@param listOfParameters see \code{\link{setup_default_inputs}}
#'@param hydraData A data set comprising all necessary inputs. This data set can be the lazily loaded data set or it can be modified in any way
#'to represent a faux assessment. See \code{\link{hydraData}} for details
#'
#'@return updated list of lazily loaded data "hydraData"
#'
#'@section Output:
#'
#'Two data files are output. A ".dat" and a ".pin" file. These are required by the Hydra code to run. The name of the file is determined by the variable \code{outputFilename} in the
#'list \code{listOfParameters}. By default the files are saved in the current working directory. You can specify a differnt location in \code{listofParameters}.
#'See \code{\link{setup_default_inputs}}
#'
#'
#'
#' @export

create_datpin_files <- function(listOfParameters,hydraData){

  if (!file.exists(listOfParameters$outDir)) {stop(paste0("Directory ",listOfParameters$outDir," doesn't exist. Create it!"))}
  # complete error checks to make sure all data conforms
  ######################################
 ###### NOT DONE YET ###################
  ######################################
  # options respesent additions to hydraData
  options <- list()

  if (tolower(listOfParameters$temperatureFlag) == "mean") { # take the mean of the temp time series
    hydraData$observedTemperature["temperature",] <- rep(mean(hydraData$observedTemperature["temperature",]),hydraData$Nyrs)
  } else {
    # do nothing since observed temperature is already read in
  }


  if ((tolower(listOfParameters$scenarioFlag) == "historical") | (tolower(listOfParameters$scenarioFlag) == "darwin")) {
    # we assume a historical run. True Temp, True Effort, No asessment, Rec error, no survey error
    options$assessmentOn <- 0
    options$assessmentWithSpeciesOn <- 0 # this is also ignored
    # if assessmentOn = 0. exploitationoption are ignored but still need to be read in .
    options$exploitationLevels <- hydraData$exploitationOptions[,1] # never used when assessment is off but needs a placeholder
    options$minMaxExploitation <- c(min(options$exploitationLevels),max(options$exploitationLevels)) # never used, just a placeholder

  } else if  (tolower(listOfParameters$scenarioFlag) == "assessment") {
    options$assessmentOn <- 1
    maxRates <- round(hydraData$exploitationOptions[hydraData$Nthresholds,]*100)  # picks out the last row which holds the max exploitation rate for each scenario
    options$exploitationLevels <- hydraData$exploitationOptions[,(maxRates == listOfParameters$maxExploitationRate)] # grabs the whole profile
    maxRampExRate <- max(options$exploitationLevels)


    if (tolower(listOfParameters$scenarioType) == "fixed") {
      # all exploitations are the same
      options$exploitationLevels <- rep(listOfParameters$maxExploitationRate/100,hydraData$Nthresholds)
      options$minMaxExploitation <- rep(listOfParameters$maxExploitationRate/100,2)
    } else {
      options$minMaxExploitation <- c(min(options$exploitationLevels),max(options$exploitationLevels))
      # we have a ramp down scenario and the values in option$exploitation reflect this
    }

    if ((listOfParameters$assessmentSpeciesFlag == "none") | (listOfParameters$assessmentSpeciesFlag == "low")) {
      options$thresholdSpecies <- hydraData$thresholdSpecies*0
    }
    if (listOfParameters$assessmentSpeciesFlag == "none") {
      options$assessmentWithSpeciesOn <- 0
    } else {
      options$assessmentWithSpeciesOn <- 1
    }

    # effort needs to change to represent exploitation rate equal to max(rampdown rate)
    for (ifleet in 1:hydraData$Nfleets) {
      fE <- as.numeric(hydraData$fisheryq[,ifleet]) # pick out fishery q's
      indicator <- hydraData$indicatorFisheryq[,ifleet] # pick out species to be used in mean
      fEInd <- fE*indicator
      #      ind <- as.numeric(p$fishery_q[,ifleet]) > 1e-29 # find all > 1e-29
      ind <- as.numeric(fEInd) > 1e-29 # find all > 1e-29
      hydraData$observedEffort[ifleet+1,] <- rep(maxRampExRate/(sum(fEInd[ind])/sum(ind)),hydraData$Nyrs) # Effort = ex/mean(q)

    }
    #SMALL MESH OVERWITE. EVENTUALLY REMOVE THIS
    print("********************* SMALL MESH & gillnet HARD CODED ****************************")
    hydraData$observedEffort[5,] <- rep(1E-6,hydraData$Nyrs) # Effort = ex/mean(q)
    hydraData$observedEffort[6,] <- rep(1E-6,hydraData$Nyrs) # Effort = ex/mean(q)

  } else if (tolower(listOfParameters$scenarioFlag) == "custom") {
    # code this part if needed
  } else {
    stop(paste0("scenarioFlag can only be \"historic\" ,\"darwin\", or \"assessment\". You have = ",listOfParameters$scenarioFlag))
  }

  # some error checks
  if ((tolower(listOfParameters$scenarioFlag) == "historical") & (hydraData$Nyrs !=53)) {
    stop(paste("Can not have a historical run > ",hydraData$Nyrs,"years. Not enough data!"))
  }

  # updata data based on options
  hydraData <- modifyList(hydraData,options)

  # write out dat file
  write_DatFile(hydraData,listOfParameters)
  write_PinFile(hydraData,listOfParameters)

  return(hydraData)

}

## subfunctions get_pinData, get_DatData, write_DatFile,write_PinFile


write_DatFile <- function(hydraData,listOfParameters) {

  outPath <- paste0(listOfParameters$outDir,"/",listOfParameters$outputFilename)
  outputFileName <-  paste0(outPath,".dat")
  # write explanation of how this file was formed
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=TRUE)


  # write all inputs to file with comment headers
  cat("# init_int debug",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$debugState),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nyrs",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nyrs),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nspecies",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nspecies),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nsizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nsizebins),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nareas",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nareas),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nfleets",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nfleets),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_number wtconv",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$wtconv),file=outputFileName,fill=TRUE,append=TRUE)
  # speciesList
  cat("#",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# List of Species in Model",file=outputFileName,fill=TRUE,append=TRUE)
  for (sp in hydraData$speciesList) {
    cat(c("#",sp),file=outputFileName,fill=TRUE,append=TRUE)
  }
  cat("#",file=outputFileName,fill=TRUE,append=TRUE)

  # length bins
  cat("# init_matrix binwidth(1,Nspecies,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  #write.table(d$binwidth, file=outputFileName, row.names=FALSE, col.names=FALSE,append=TRUE,sep="\t")
  for(sp in 1:hydraData$Nspecies) {
    cat(c(" ",as.matrix(hydraData$binwidth[sp,])), file=outputFileName, fill=TRUE,append=TRUE,sep="\t")
  }

  # length- weight relationship. w = aL^b
  cat("# init_vector lenwt_a(1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$lenwta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_vector lenwt_b(1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$lenwtb),file=outputFileName,fill=TRUE,append=TRUE)
  # covariate information, number of covariates
  cat("# init_int Nrecruitment_cov",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$NrecruitmentCov),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int Nmaturity_cov ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$NmaturityCov),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int Ngrowth_cov",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$NgrowthCov),file=outputFileName,fill=TRUE,append=TRUE)

  # covariate time series  - recruitment, maturity, growth
  cat("#  init_matrix recruitment_cov(1,Nrecruitment_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:hydraData$NrecruitmentCov){
    cat(c(" ",hydraData$recruitmentCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("#  init_matrix maturity_cov(1,Nmaturity_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:hydraData$NmaturityCov){
    cat(c(" ",hydraData$maturityCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  cat("#  init_matrix growth_cov(1,Ngrowth_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:hydraData$NgrowthCov){
    cat(c(" ",hydraData$growthCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # observed (survey) biomass
  cat("#   init_3darray obs_survey_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   THESE ARE FROM ATLANTIS AND SHOULD NOT BE USED IN FITTING: REPLACE WITH SURVEY DATA",file=outputFileName,fill=TRUE,append=TRUE)
  for (idata in 1:hydraData$Nspecies){
    cat(c(" ",hydraData$observedBiomass[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  # observed catch
  cat("#   init_3darray obs_catch_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   THESE ARE FROM ASSESSMENTS see Catches.xls placeholder for real catch data",file=outputFileName,fill=TRUE,append=TRUE)
  for (idata in 1:hydraData$Nspecies){
    cat(c(" ",hydraData$observedCatch[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  # observed effort
  cat("#  init_3darray obs_effort(1,Nareas,1,Nfleets,1,Nyrs)  ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c("# fleet types",hydraData$fleetNames),file=outputFileName,fill=TRUE,append=TRUE)
  if (listOfParameters$scenarioFlag =="assessment") {
    cat("#  effort based on q with Exploitation Rate = ",listOfParameters$maxExploitationRate/100,". Manufactured effort for simulation runs",file=outputFileName,fill=TRUE,append=TRUE)
  }else{
    cat("#  Observed effort. No assessment",file=outputFileName,fill=TRUE,append=TRUE)
  }
  for (idata in 1:hydraData$Nfleets){
    cat(c(" ",hydraData$observedEffort[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # intake stomach content
  cat("#  init_4darray area1_stomwt(1,Nareas,1,Nspecies,1,Nyrs,Nsizebins)   ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  read in mean stomach content weight time series from .dat file for intake calculation   ",file=outputFileName,fill=TRUE,append=TRUE)

  for (sp in 1:hydraData$Nspecies) {
    cat(c("# ",hydraData$speciesList[sp]),file=outputFileName,fill=TRUE,append=TRUE)
    for (iy in 1:hydraData$Nyrs) {
      cat(c(" ",hydraData$intakeStomach[sp,]),file=outputFileName,fill=TRUE,append=TRUE)
    }

  }

  # observed temperature
  cat("#   init_matrix obs_temp(1,Nareas,1,Nyrs)      ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  Either observed temperature  or manufactured temperature for simulation runs",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1977 to 1997 Georges Bank bottom temp from 2011 ESR (1964-1976 set to 8.0) and 1998 to 2010 Georges Bank bottom temp from 2011 ESR",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$observedTemperature[2,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # estimation phases
  cat("#  init_int yr1Nphase            //year 1 N at size estimation phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$yr1Nphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int recphase				//recruitment parameter estimation phas",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$recphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int avg_rec_phase		//average recruitment estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$avgRecPhase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int avg_F_phase			//average fishing mort estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$avgFPhase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int dev_rec_phase		//recruitment deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$devRecPhase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int dev_F_phase			//fishing mort deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$devFPhase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int fqphase              //fishery q estimation phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$fqphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int sqphase              //survey q estimation phase ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$sqphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   init_int ssig_phase           //survey sigma (obs error) phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$ssigPhase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int csig_phase           //catch sigma (obs error) phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$csigPhase),file=outputFileName,fill=TRUE,append=TRUE)

  # stock recruitment parameters
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamma_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamma_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_alpha(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_shape(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_beta(1,Nareas,1,Nspecies)			//SSB Deriso-Schnute model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_alpha(1,Nareas,1,Nspecies)		//SSB gamma alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_shape(1,Nareas,1,Nspecies)		//SSB gamma shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_beta(1,Nareas,1,Nspecies)			//SSB gamma model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_alpha(1,Nareas,1,Nspecies)		//SSB Ricker model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_shape(1,Nareas,1,Nspecies)		//SSB Ricker model shape parameter=1.0, not used",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_beta(1,Nareas,1,Nspecies)			//SSB Ricker model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_alpha(1,Nareas,1,Nspecies)		//SSB Beverton Holt model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_shape(1,Nareas,1,Nspecies)		//SSB Beverton Holt model shape parameter=1.0, not used",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_beta(1,Nareas,1,Nspecies)			//SSB Beverton Holt model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_alpha //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_shape //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_beta //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#  init_matrix recSHockey_alpha //SSB S-R Hockey 2 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recHpockey_shape //SSB S-R Hockey. S* breakpoint",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recHockey_beta //SSB S-R Hockey 2 param. This is not used",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#  init_matrix recSegmented_alpha //SSB S-R Segmented 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$alphaSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recSegmented_shape //SSB S-R Segmented 3 param. Breakpoint",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$shapeSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recSegmented_beta //SSB S-R Segmented 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$betaSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)



  # recruitment type 1,2,3,4,5,6
  cat("#  init_ivector rectype(1,Nspecies)  //switch for alternate recruitment functions 1=gamma/Ricker, 2=Deriso-Schnute, 9=avg+devs
# 3=SSB gamma, 4=SSB Ricker, 5=SSB Beverton Holt added April 2014,6=Shepherd (added Beet Mar 2017)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$recType),file=outputFileName,fill=TRUE,append=TRUE)

  # recruitment stochasticity
  cat("#   init_ivector stochrec(1,Nspecies)  //switch for stochastic recruitment. 1 = add error, 0= no error",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$recStochastic),file=outputFileName,fill=TRUE,append=TRUE)

  # sex ratio
  cat("#  init_matrix sexratio(1,Nareas,1,Nspecies)  // this is proportion females",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$sexRatio),file=outputFileName,fill=TRUE,append=TRUE)

  # recruitment effects
  cat(" #  init_matrix recruitment_covwt(1,Nspecies,1,Nrecruitment_cov)	//recruitment covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$recruitCovEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # fecundity
  cat("#//fecundity parameters from .dat file and calculate fecundity at length",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fecund_d(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$fecundityd),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fecund_h(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$fecundityh),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray fecund_theta(1,Nareas,1,Nspecies,1,Nsizebins))",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$fecundityTheta[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # maturity
  cat("#  init_matrix maturity_nu(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$maturityNu),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix maturity_omega(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$maturityOmega),file=outputFileName,fill=TRUE,append=TRUE)

  # maturity covariate effects
  cat("#  init_matrix maturity_covwt(1,Nspecies,1,Nmaturity_cov) //maturity covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$maturityCovEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # growth
  cat("#//growth parameters from .dat file and calculate simple (no cov) prob of growing through length interval",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix growth_psi(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$growthPsi),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix growth_kappa(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$growthKappa),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # growth covariate effects
  cat("#  init_matrix growth_covwt(1,Nspecies,1,Ngrowth_cov)// growth covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$growthCovEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  cat("#  init_matrix vonB_Linf(1,Nareas,1,Nspecies)    //alternate parameterization, vonB growth",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$growthLinf),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix vonB_k(1,Nareas,1,Nspecies)       //alternate parameterization, vonB growth",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$growthK),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_vector growthtype                           //switch for alternate growth types,",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1 power, 2 power/covariates, 3 vonB, 4 vonB covariates",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$growthType),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # phimax
  cat("#  init_number phimax",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$phimax),file=outputFileName,fill=TRUE,append=TRUE)

  # intake
  cat("#  init_matrix intake_alpha(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$intakeAlpha),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix intake_beta(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$intakeBeta),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # M1 - natural mortality (not explained by model)
  cat(" # M1 - natural mortality (not explained by model)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray M1(1,Nareas,1,Nspecies,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)

  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$M1[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # foodweb
  cat("#  init_3darray isprey(1,Nareas,1,Nspecies,1,Nspecies)     //preds in columns, prey in rows",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$foodweb[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # M2 size preference function
  cat("#  init_matrix preferred_wtratio(1,Nareas,1,Nspecies)     //pred sizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$M2sizePrefMu),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_vector sd_sizepref(1,Nspecies)              //pred sizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$M2sizePrefSigma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)


  # fishery selectivity
  # need to reformat for cat function
  fisherySelectc <- format(as.matrix(hydraData$fisherySelectivityc),digits=5)
  fisherySelectd <- format(as.matrix(hydraData$fisherySelectivityd),digits=5)

  cat("#  //fishery selectivity pars from dat file, for now not area specific",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fishsel_c(1,Nspecies,1,Nfleets)  //fishery selectivity c par",file=outputFileName,fill=TRUE,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",fisherySelectc[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  cat("#  init_matrix fishsel_d(1,Nspecies,1,Nfleets)  //fishery selectivity d par",file=outputFileName,fill=TRUE,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",fisherySelectd[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


  # Following content added after ICES publication by Gaichas et al. 2014
  # Made by Andy Beet from Dec 2016 onward

  # equilibrium Biomass
  cat("# Following content added after ICES publication by Gaichas et al. 2014",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# Made by Andy Beet from Dec 2016 onward",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# Equilibrium Biomass. B0(1,Nspecies). Tthese values are obtained by running hydra_sim without any error and zero fishing effort",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$B0),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # guild number + membership
  cat("#number of Guilds numGuilds.",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$numGuilds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat(c("#Guild Membership guildMembership. "),file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$guildMembership),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # assessment thresholds and exploiation and step/linear ramp
  cat("# AssessmentPeriod. Time period (yrs) to assess guild biomass level",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$assessmentPeriod),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# init_int flagLinearRamp. // 0 = step function, 1 = linear function",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$flagLinearRamp),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# init_vector minMaxExploitation(1,2) - [MinExploitation, MaxExploitation",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$minMaxExploitation),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_vector minMaxThreshold(1,2) - [MinThreshold, MaxThreshold",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$minMaxThresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)




  cat("# Nthresholds. number of thresholds used for change in exploitation/fishing - Step function",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$Nthresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# threshold_percent(1,Nthresholds) threshold %ages (of biomass) when action is taken - Step function",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# note that must appear in ascending order",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$thresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# exploitation_levels(1,Nthresholds). these must pair with the threshold_percent values - Step function",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$exploitationLevels),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # species specific addition to threshold
  cat("# threshold_species(1,Nspecies). Species level detection threshold",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$thresholdSpecies),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # assessment switches
  cat("# int AssessmentOn. Assessment On or Off",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$assessmentOn),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# int speciesDetection. include species (in addition to guild) in assessment on or off",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$assessmentWithSpeciesOn),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

    # large fish index cut off for large fish (cm)
  cat("# int LFI_size. (cm). Threshold to determin a large fish. used in LFI metric",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$LFISize),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_number scaleInitialN.  used to scale initial yr1N abundances found in .pin file",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$scaleInitialN),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# other food term",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$otherFood),file=outputFileName,fill=TRUE,append=TRUE)

  # scaling effort due to NEUS shel effort and not GB effort.
  cat("#init_matrix effortScaled(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$scaledEffort),file=outputFileName,fill=TRUE,append=TRUE)

  # discard coeffiecients
  cat("# init_4darray discard_Coef(1,Nareas,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# proportion of each species that is discarded for each fleet(Bottom, Pelagic, Fixed)",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(paste0("# ",hydraData$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=TRUE,append=TRUE)
    for (ifleet in 1:hydraData$Nfleets) {
      rowNum <- ((isp-1)*hydraData$Nfleets) + ifleet
      cat(c(" ",hydraData$discardCoef[rowNum,]),file=outputFileName,fill=TRUE,append=TRUE)
    }
  }
  # survival coefficients
  cat("# init_4darray discardSurvival_Coef(1,Nareas,1,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# proportion of discards that survive being thrown back",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(paste0("# ",hydraData$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=TRUE,append=TRUE)
    for (ifleet in 1:hydraData$Nfleets) {
      rowNum <- ((isp-1)*hydraData$Nfleets) + ifleet
      cat(c(" ",hydraData$discardSurvival[rowNum,]),file=outputFileName,fill=TRUE,append=TRUE)
    }
  }

  # predator or prey - binary - for indices
  cat("# predOrPrey(1,Nspecies). binary vector indicating predators. inverse = prey",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$predOrPrey),file=outputFileName,fill=TRUE,append=TRUE)

  # bandidth for smoother for catch sd.
  cat("# bandwidth_metric. (in yrs) for variance estimate of catch - moving window",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$bandwidthMetric),file=outputFileName,fill=TRUE,append=TRUE)

  # flag for MSE or not. Determins output files only
  cat("# init_number baseline_threshold // value of threshold that we stop landing catch. Typically 0.2",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$baselineThreshold),file=outputFileName,fill=TRUE,append=TRUE)

  # fishery q indicator
  cat("# init_3darray indicator_fishery_q(1,Nareas,1,Nspecies,1,Nfleets)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# which species represent targeted catch. These are used to estmate exploitation rate in assessment",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$indicatorFisheryq[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # AR parameters for  Survey, recruitment and Catch
  cat("# AR_parameters(1,3) Survey, recruitment, Catch ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$ARParameters),file=outputFileName,fill=TRUE,append=TRUE)

  # flag for MSE or not. Determins output files only
  cat("# init_int flagMSE determins level of output (0 or 1)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$flagMSE),file=outputFileName,fill=TRUE,append=TRUE)


  # end of file
  cat("# eof",file=outputFileName,fill=TRUE,append=TRUE)
  cat("54321",file=outputFileName,fill=TRUE,append=TRUE)


}


write_PinFile <- function(hydraData,listOfParameters){

  outPath <- paste0(listOfParameters$outDir,"/",listOfParameters$outputFilename)
  outputFileName <-  paste0(outPath,".pin")

  # write explanation of how this file was formed
  cat("#hydra_sim.pin for 10 species, 1 area (Georges Bank) for simulation, May 2013
      #note that species 9 and 10 have changed from ms3am test model",file=outputFileName,fill=TRUE)
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=TRUE,append=TRUE)

  cat("#species are based on pars from LeMANS model, see LeMANSpars_fortesting.xlsx and other lit values",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1: spinydog",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#2: winterskate",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#3: Aherring",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#4: Acod",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#5: haddock",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#6: yellowtailfl",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#7: winterfl",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#8: Amackerel",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#9: silverhake",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#10: goosefish",file=outputFileName,fill=TRUE,append=TRUE)

  # year 1 initial values of N
  cat("#//Initial N year 1",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray yr1N(1,Nareas,1,Nspecies,1,Nsizebins)       //initial year N at size, millions",file=outputFileName,fill=TRUE,append=TRUE)

  # need to reformat for cat function
  Y1Nformat <- format(as.matrix(hydraData$Y1N),digits=7)
  for (sp in 1:hydraData$Nspecies) {
    cat(c(" ",Y1Nformat[sp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }

  # recruitment. these are not used. If we delete then all previous code not compatible.
  # When we decide to forgo backward compatibility this will be deleted.
  cat("#//recruitment parameters from .pin file (now alts by spp read in from dat file; these defaults replaced in preliminary calcs)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",hydraData$alphaEggRicker),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix eggRicker_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",hydraData$shapeEggRicker),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix eggRicker_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",hydraData$betaEggRicker),file=outputFileName,fill=TRUE,append=TRUE)

  # redundant inputs recruitment Devs etc. never used.
  cat("#  //recruitment: average annual, annual devs, actual (avg+dev)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix avg_recruitment(1,Nareas,1,Nspecies,avg_rec_phase)  //average annual recruitment by area, species ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#logspace (scaled by eye to produce flatline pops with pred mort but no fishing) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$redundantAvgRec),file=outputFileName,fill=TRUE,append=TRUE)

  cat("#  init_3darray recruitment_devs(1,Nareas,1,Nspecies,1,Nyrs,dev_rec_phase)  //recruitment deviations by area, species",file=outputFileName,fill=TRUE,append=TRUE)
  for (sp in 1:hydraData$Nspecies) {
    cat(c(" ",hydraData$redundantRecDevs[sp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # recruitment sigma from S-R fits
  cat("#   init_matrix recsigma(1,Nareas,1,Nspecies)  //recruitment sigma",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",format(hydraData$recSigma,digits=8)),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # fishery Q's
 # format for cat function
  fishQ <- format(as.matrix(hydraData$fisheryq),digits=7)
  cat("# Fishery qs",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray fishery_q(1,Nareas,1,Nspecies,1,Nfleets,fqphase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# area1.btrawl ptrawl longline)",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",fishQ[isp,]," #",rownames(hydraData$fisheryq)[isp]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


  # survey q
  cat("#  init_matrix survey_q(1,Nareas,1,Nspecies,sqphase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$surveyq),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  # survey sigma (observation error)
  cat("#  init_matrix surv_sigma(1,Nareas,1,Nspecies,ssig_phase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",hydraData$surveySigma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # fishery sigma (catch obs eror)
  cat("#  init_3darray catch_sigma(1,Nareas,1,Nspecies,1,Nfleets,csig_phase)",file=outputFileName,fill=TRUE,append=TRUE)
  fishSigq <- format(as.matrix(hydraData$fisherySigma),digits=NULL)
  for (isp in 1:hydraData$Nspecies) {
    cat(c(" ",fishSigq[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


}

