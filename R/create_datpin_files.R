#' Create data files (.dat and .pin) for use with multispecies model, Hydra.
#'
#' Initial data files have been lazily loaded. To view their contents please run the \code{\link{create_documentation}} rmd file
#'
#'@param listOfParameters see \code{\link{setup_default_inputs}}
#'@param dataList A data set comprising all necessary inputs. This data set can be the lazily loaded data set or it can be modified in any way
#'to represent a faux assessment. See \code{\link{hydraDataList}} for details
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

create_datpin_files <- function(listOfParameters,dataList){

  if (!file.exists(listOfParameters$outDir)) {stop(paste0("Directory ",listOfParameters$outDir," doesn't exist. Create it!"))}

# Error Checks ------------------------------------------------------------
  # complete error checks to make sure all data conforms

  # options respesent additions to dataList
  options <- list()

  if (tolower(listOfParameters$temperatureFlag) == "mean") { # take the mean of the temp time series
    dataList$observedTemperature["temperature",] <- rep(mean(dataList$observedTemperature["temperature",]),dataList$Nyrs)
  } else {
    # do nothing since observed temperature is already read in
  }


# Historical --------------------------------------------------------------


  if ((tolower(listOfParameters$scenarioFlag) == "historical") | (tolower(listOfParameters$scenarioFlag) == "darwin")) {
    # we assume a historical run. True Temp, True Effort, No asessment, Rec error, no survey error
    options$assessmentOn <- 0
    options$assessmentWithSpeciesOn <- 0 # this is also ignored
    # if assessmentOn = 0. exploitationoption are ignored but still need to be read in .
    options$exploitationLevels <- dataList$exploitationOptions[,1] # never used when assessment is off but needs a placeholder
    options$minMaxExploitation <- c(min(options$exploitationLevels),max(options$exploitationLevels)) # never used, just a placeholder

# Assessment --------------------------------------------------------------

  } else if  (tolower(listOfParameters$scenarioFlag) == "assessment") {
    options$assessmentOn <- 1
    maxRates <- round(dataList$exploitationOptions[dataList$Nthresholds,]*100)  # picks out the last row which holds the max exploitation rate for each scenario
    options$exploitationLevels <- dataList$exploitationOptions[,(maxRates == listOfParameters$maxExploitationRate)] # grabs the whole profile
    maxRampExRate <- max(options$exploitationLevels)

    if (tolower(listOfParameters$scenarioType) == "fixed") {
      # all exploitations are the same
      options$exploitationLevels <- rep(listOfParameters$maxExploitationRate/100,dataList$Nthresholds)
      options$minMaxExploitation <- rep(listOfParameters$maxExploitationRate/100,2)
    } else {
      options$minMaxExploitation <- c(min(options$exploitationLevels),max(options$exploitationLevels))
      # we have a ramp down scenario and the values in option$exploitation reflect this
    }

    if ((listOfParameters$assessmentSpeciesFlag == "none") | (listOfParameters$assessmentSpeciesFlag == "low")) {
      options$thresholdSpecies <- dataList$thresholdSpecies*0
    }
    if (listOfParameters$assessmentSpeciesFlag == "none") {
      options$assessmentWithSpeciesOn <- 0
    } else {
      options$assessmentWithSpeciesOn <- 1
    }

    # effort needs to change to represent exploitation rate equal to max(rampdown rate)
    for (ifleet in 1:dataList$Nfleets) {
      fE <- as.numeric(dataList$fisheryq[,ifleet]) # pick out fishery q's
      indicator <- dataList$indicatorFisheryq[,ifleet] # pick out species to be used in mean
      fEInd <- fE*indicator
      #      ind <- as.numeric(p$fishery_q[,ifleet]) > 1e-29 # find all > 1e-29
      ind <- as.numeric(fEInd) > 1e-29 # find all > 1e-29
      dataList$observedEffort[ifleet+1,] <- rep(maxRampExRate/(sum(fEInd[ind])/sum(ind)),dataList$Nyrs) # Effort = ex/mean(q)
    }
    #SMALL MESH OVERWITE. EVENTUALLY REMOVE THIS
    print("********************* SMALL MESH & gillnet HARD CODED ****************************")
    dataList$observedEffort[5,] <- rep(1E-6,dataList$Nyrs) # Effort = ex/mean(q)
    dataList$observedEffort[6,] <- rep(1E-6,dataList$Nyrs) # Effort = ex/mean(q)

# Effort ------------------------------------------------------------------
  # Allows independent fleet effort in assessment

  } else if (tolower(listOfParameters$scenarioFlag) == "effort") {
    options$assessmentOn <- 1
    # if(length(listOfParameters$fleetEffortRates) < dataList$Nfleets) {
    #   nMissing <- dataList$Nfleets - length(listOfParameters$fleetEffortRates)
    #   listOfParameters$fleetEffortRates <- c(listOfParameters$fleetEffortRates,rep(0.05,nMissing))
    # }
    if (listOfParameters$assessmentSpeciesFlag == "none") {
      options$assessmentWithSpeciesOn <- 0
    } else {
      options$assessmentWithSpeciesOn <- 1
    }
    # place holder for redundant code
    options$exploitationLevels <- rep(0.05,dataList$Nthresholds)
    options$minMaxExploitation <- c(.05,.05)
    # all exploitation is controlled by minExploitation, maxExploitation (length = Nfleets).
    # This is now part of dataList

    # effort needs to change to represent exploitation rate equal to rate specified by minExploitation,maxExploitation.
    for (ifleet in 1:dataList$Nfleets) {
      fE <- as.numeric(dataList$fisheryq[,ifleet]) # pick out fishery q's
      indicator <- dataList$indicatorFisheryq[,ifleet] # pick out species to be used in mean
      fEInd <- fE*indicator
      ind <- as.numeric(fEInd) > 1e-29 # find all > 1e-29
      # first row is year
      fleetRate <- dataList$maxExploitation[ifleet] # for fixed minExploitation = maxExploitation
      dataList$observedEffort[ifleet+1,] <- rep(fleetRate/(sum(fEInd[ind])/sum(ind)),dataList$Nyrs) # Effort = ex/mean(q)
    }
    #SMALL MESH OVERWITE. EVENTUALLY REMOVE THIS
    print("********************* SMALL MESH & gillnet HARD CODED ****************************")
    dataList$observedEffort[5,] <- rep(1E-6,dataList$Nyrs) # Effort = ex/mean(q)
    dataList$observedEffort[6,] <- rep(1E-6,dataList$Nyrs) # Effort = ex/mean(q)






    # code this part if needed
  } else {
    stop(paste0("scenarioFlag can only be \"historic\" ,\"darwin\", or \"assessment\". You have = ",listOfParameters$scenarioFlag))
  }

  # some error checks
  if ((tolower(listOfParameters$scenarioFlag) == "historical") & (dataList$Nyrs !=53)) {
    stop(paste("Can not have a historical run > ",dataList$Nyrs,"years. Not enough data!"))
  }

  # updata data based on options
  dataList <- modifyList(dataList,options)

  # write out dat file
  write_DatFile(dataList,listOfParameters)
  write_PinFile(dataList,listOfParameters)

  return(dataList)

}

## subfunctions get_pinData, get_DatData, write_DatFile,write_PinFile


write_DatFile <- function(dataList,listOfParameters) {

  outPath <- paste0(listOfParameters$outDir,"/",listOfParameters$outputFilename)
  outputFileName <-  paste0(outPath,".dat")
  # write explanation of how this file was formed
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=listOfParameters$fillLength)


  # write all inputs to file with comment headers
  cat("# init_int debug",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$debugState),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_int Nyrs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nyrs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_int Nspecies",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nspecies),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_int Nsizebins",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nsizebins),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_int Nareas",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nareas),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_int Nfleets",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nfleets),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_number wtconv",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$wtconv),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  # speciesList
  cat("#",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# List of Species in Model",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (sp in dataList$speciesList) {
    cat(c("#",sp),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("#",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # length bins
  cat("# init_matrix binwidth(1,Nspecies,1,Nsizebins)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  #write.table(d$binwidth, file=outputFileName, row.names=FALSE, col.names=FALSE,append=TRUE,sep="\t")
  for(sp in 1:dataList$Nspecies) {
    cat(c(" ",as.matrix(dataList$binwidth[sp,])), file=outputFileName, fill=listOfParameters$fillLength,append=TRUE,sep="\t")
  }

  # length- weight relationship. w = aL^b
  cat("# init_vector lenwt_a(1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$lenwta),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_vector lenwt_b(1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$lenwtb),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  # covariate information, number of covariates
  cat("# init_int Nrecruitment_cov",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$NrecruitmentCov),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int Nmaturity_cov ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$NmaturityCov),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int Ngrowth_cov",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$NgrowthCov),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # covariate time series  - recruitment, maturity, growth
  cat("#  init_matrix recruitment_cov(1,Nrecruitment_cov,1,Nyrs)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (icov in 1:dataList$NrecruitmentCov){
    cat(c(" ",dataList$recruitmentCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("#  init_matrix maturity_cov(1,Nmaturity_cov,1,Nyrs)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (icov in 1:dataList$NmaturityCov){
    cat(c(" ",dataList$maturityCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  cat("#  init_matrix growth_cov(1,Ngrowth_cov,1,Nyrs)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (icov in 1:dataList$NgrowthCov){
    cat(c(" ",dataList$growthCov[icov,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # observed (survey) biomass
  cat("#   init_3darray obs_survey_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#   THESE ARE FROM ATLANTIS AND SHOULD NOT BE USED IN FITTING: REPLACE WITH SURVEY DATA",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Nspecies){
    cat(c(" ",dataList$observedBiomass[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  # observed catch
  cat("#   init_3darray obs_catch_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#   THESE ARE FROM ASSESSMENTS see Catches.xls placeholder for real catch data",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Nspecies){
    cat(c(" ",dataList$observedCatch[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  # observed effort
  cat("#  init_3darray obs_effort(1,Nareas,1,Nfleets,1,Nyrs)  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c("# fleet types",dataList$fleetNames),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  if (listOfParameters$scenarioFlag =="assessment") {
    cat("#  effort based on q with Exploitation Rate = ",listOfParameters$maxExploitationRate/100,". Manufactured effort for simulation runs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }else{
    cat("#  Observed effort. No assessment",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  for (idata in 1:dataList$Nfleets){
    cat(c(" ",dataList$observedEffort[idata+1,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # intake stomach content
  cat("#  init_4darray area1_stomwt(1,Nareas,1,Nspecies,1,Nyrs,Nsizebins)   ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  read in mean stomach content weight time series from .dat file for intake calculation   ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  for (sp in 1:dataList$Nspecies) {
    cat(c("# ",dataList$speciesList[sp]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    for (iy in 1:dataList$Nyrs) {
      cat(c(" ",dataList$intakeStomach[sp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    }

  }

  # observed temperature
  cat("#   init_matrix obs_temp(1,Nareas,1,Nyrs)      ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  Either observed temperature  or manufactured temperature for simulation runs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#1977 to 1997 Georges Bank bottom temp from 2011 ESR (1964-1976 set to 8.0) and 1998 to 2010 Georges Bank bottom temp from 2011 ESR",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$observedTemperature[2,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # estimation phases
  cat("#  init_int yr1Nphase            //year 1 N at size estimation phase",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$yr1Nphase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int recphase				//recruitment parameter estimation phas",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$recphase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int avg_rec_phase		//average recruitment estimation phase (could make species specific, currently global)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$avgRecPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int avg_F_phase			//average fishing mort estimation phase (could make species specific, currently global)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$avgFPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int dev_rec_phase		//recruitment deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$devRecPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int dev_F_phase			//fishing mort deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$devFPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int fqphase              //fishery q estimation phase",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$fqphase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int sqphase              //survey q estimation phase ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$sqphase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#   init_int ssig_phase           //survey sigma (obs error) phase",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$ssigPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_int csig_phase           //catch sigma (obs error) phase",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$csigPhase),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # stock recruitment parameters
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamma_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model shape parameter",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamma_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model beta",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_alpha(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model alpha",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_shape(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model shape parameter",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recDS_beta(1,Nareas,1,Nspecies)			//SSB Deriso-Schnute model beta",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaDS),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_alpha(1,Nareas,1,Nspecies)		//SSB gamma alpha",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_shape(1,Nareas,1,Nspecies)		//SSB gamma shape parameter",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamSSB_beta(1,Nareas,1,Nspecies)			//SSB gamma model beta",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaGamma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_alpha(1,Nareas,1,Nspecies)		//SSB Ricker model alpha",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_shape(1,Nareas,1,Nspecies)		//SSB Ricker model shape parameter=1.0, not used",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recRicker_beta(1,Nareas,1,Nspecies)			//SSB Ricker model beta",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_alpha(1,Nareas,1,Nspecies)		//SSB Beverton Holt model alpha",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_shape(1,Nareas,1,Nspecies)		//SSB Beverton Holt model shape parameter=1.0, not used",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recBH_beta(1,Nareas,1,Nspecies)			//SSB Beverton Holt model beta",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaBH),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_alpha //SSB S-R Shepherd 3 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_shape //SSB S-R Shepherd 3 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recShepherd_beta //SSB S-R Shepherd 3 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaShepherd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#  init_matrix recSHockey_alpha //SSB S-R Hockey 2 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recHpockey_shape //SSB S-R Hockey. S* breakpoint",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recHockey_beta //SSB S-R Hockey 2 param. This is not used",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaHockey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#  init_matrix recSegmented_alpha //SSB S-R Segmented 3 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recSegmented_shape //SSB S-R Segmented 3 param. Breakpoint",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recSegmented_beta //SSB S-R Segmented 3 param",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaSegmented),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)



  # recruitment type 1,2,3,4,5,6
  cat("#  init_ivector rectype(1,Nspecies)  //switch for alternate recruitment functions 1=gamma/Ricker, 2=Deriso-Schnute, 9=avg+devs
# 3=SSB gamma, 4=SSB Ricker, 5=SSB Beverton Holt added April 2014,6=Shepherd (added Beet Mar 2017)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$recType),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # recruitment stochasticity
  cat("#   init_ivector stochrec(1,Nspecies)  //switch for stochastic recruitment. 1 = add error, 0= no error",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$recStochastic),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # sex ratio
  cat("#  init_matrix sexratio(1,Nareas,1,Nspecies)  // this is proportion females",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$sexRatio),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # recruitment effects
  cat(" #  init_matrix recruitment_covwt(1,Nspecies,1,Nrecruitment_cov)	//recruitment covariate weighting factor",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$recruitCovEffects[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # fecundity
  cat("#//fecundity parameters from .dat file and calculate fecundity at length",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix fecund_d(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$fecundityd),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix fecund_h(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$fecundityh),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_3darray fecund_theta(1,Nareas,1,Nspecies,1,Nsizebins))",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$fecundityTheta[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # maturity
  cat("#  init_matrix maturity_nu(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$maturityNu),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix maturity_omega(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$maturityOmega),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # maturity covariate effects
  cat("#  init_matrix maturity_covwt(1,Nspecies,1,Nmaturity_cov) //maturity covariate weighting factor",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$maturityCovEffects[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # growth
  cat("#//growth parameters from .dat file and calculate simple (no cov) prob of growing through length interval",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix growth_psi(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$growthPsi),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix growth_kappa(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$growthKappa),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # growth covariate effects
  cat("#  init_matrix growth_covwt(1,Nspecies,1,Ngrowth_cov)// growth covariate weighting factor",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$growthCovEffects[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  cat("#  init_matrix vonB_Linf(1,Nareas,1,Nspecies)    //alternate parameterization, vonB growth",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$growthLinf),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix vonB_k(1,Nareas,1,Nspecies)       //alternate parameterization, vonB growth",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$growthK),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_vector growthtype                           //switch for alternate growth types,",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#1 power, 2 power/covariates, 3 vonB, 4 vonB covariates",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$growthType),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # phimax
  cat("#  init_number phimax",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$phimax),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # intake
  cat("#  init_matrix intake_alpha(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$intakeAlpha),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix intake_beta(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$intakeBeta),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # M1 - natural mortality (not explained by model)
  cat(" # M1 - natural mortality (not explained by model)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_3darray M1(1,Nareas,1,Nspecies,1,Nsizebins)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$M1[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # foodweb
  cat("#  init_3darray isprey(1,Nareas,1,Nspecies,1,Nspecies)     //preds in columns, prey in rows",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$foodweb[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # M2 size preference function
  cat("#  init_matrix preferred_wtratio(1,Nareas,1,Nspecies)     //pred sizebins",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$M2sizePrefMu),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_vector sd_sizepref(1,Nspecies)              //pred sizebins",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$M2sizePrefSigma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)


  # fishery selectivity
  # need to reformat for cat function
  fisherySelectc <- format(as.matrix(dataList$fisherySelectivityc),digits=5)
  fisherySelectd <- format(as.matrix(dataList$fisherySelectivityd),digits=5)

  cat("#  //fishery selectivity pars from dat file, for now not area specific",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix fishsel_c(1,Nspecies,1,Nfleets)  //fishery selectivity c par",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",fisherySelectc[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  cat("#  init_matrix fishsel_d(1,Nspecies,1,Nfleets)  //fishery selectivity d par",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",fisherySelectd[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


  # Following content added after ICES publication by Gaichas et al. 2014
  # Made by Andy Beet from Dec 2016 onward

  # equilibrium Biomass
  cat("# Following content added after ICES publication by Gaichas et al. 2014",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Made by Andy Beet from Dec 2016 onward",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Equilibrium Biomass. B0(1,Nspecies). Tthese values are obtained by running hydra_sim without any error and zero fishing effort",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$B0),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # guild number + membership
  cat("#number of Guilds numGuilds.",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$numGuilds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat(c("#Guild Membership guildMembership. "),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$guildMembership),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat(c("#Fleet Membership fleetMembers(1,Nfleets) "),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$fleetMembership),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # assessment thresholds and exploiation and step/linear ramp
  cat("# AssessmentPeriod. Time period (yrs) to assess guild biomass level",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$assessmentPeriod),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# init_int flagLinearRamp. // 0 = step function, 1 = linear function",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$flagLinearRamp),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)


  cat("#init_vector minExploitation(1,Nfleets) minimum Exploitation rates imposed by each fleet",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$minExploitation),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#init_vector maxExploitation(1,Nfleets) maximum Exploitation rates imposed by each fleet",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$maxExploitation),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# init_vector minMaxExploitation(1,2) - [MinExploitation, MaxExploitation",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$minMaxExploitation),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_vector minMaxThreshold(1,2) - [MinThreshold, MaxThreshold",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$minMaxThresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)




  cat("# Nthresholds. number of thresholds used for change in exploitation/fishing - Step function",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nthresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("# threshold_percent(1,Nthresholds) threshold %ages (of biomass) when action is taken - Step function",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# note that must appear in ascending order",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$thresholds),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# exploitation_levels(1,Nthresholds). these must pair with the threshold_percent values - Step function",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$exploitationLevels),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # species specific addition to threshold
  cat("# threshold_species(1,Nspecies). Species level detection threshold",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$thresholdSpecies),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # assessment switches
  cat("# int AssessmentOn. Assessment On or Off",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$assessmentOn),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# int speciesDetection. include species (in addition to guild) in assessment on or off",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$assessmentWithSpeciesOn),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

    # large fish index cut off for large fish (cm)
  cat("# int LFI_size. (cm). Threshold to determin a large fish. used in LFI metric",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$LFISize),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_number scaleInitialN.  used to scale initial yr1N abundances found in .pin file",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$scaleInitialN),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# other food term",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$otherFood),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # scaling effort due to NEUS shel effort and not GB effort.
  cat("#init_matrix effortScaled(1,Nareas,1,Nspecies)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$scaledEffort),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # discard coeffiecients
  cat("# init_4darray discard_Coef(1,Nareas,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# proportion of each species that is discarded for each fleet(Bottom, Pelagic, Fixed)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(paste0("# ",dataList$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    for (ifleet in 1:dataList$Nfleets) {
      rowNum <- ((isp-1)*dataList$Nfleets) + ifleet
      cat(c(" ",dataList$discardCoef[rowNum,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    }
  }
  # survival coefficients
  cat("# init_4darray discardSurvival_Coef(1,Nareas,1,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# proportion of discards that survive being thrown back",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(paste0("# ",dataList$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    for (ifleet in 1:dataList$Nfleets) {
      rowNum <- ((isp-1)*dataList$Nfleets) + ifleet
      cat(c(" ",dataList$discardSurvival[rowNum,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
    }
  }

  # predator or prey - binary - for indices
  cat("# predOrPrey(1,Nspecies). binary vector indicating predators. inverse = prey",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$predOrPrey),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # bandidth for smoother for catch sd.
  cat("# bandwidth_metric. (in yrs) for variance estimate of catch - moving window",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$bandwidthMetric),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # flag for MSE or not. Determins output files only
  cat("# init_number baseline_threshold // value of threshold that we stop landing catch. Typically 0.2",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$baselineThreshold),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # fishery q indicator
  cat("# init_3darray indicator_fishery_q(1,Nareas,1,Nspecies,1,Nfleets)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# which species represent targeted catch. These are used to estmate exploitation rate in assessment",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$indicatorFisheryq[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # AR parameters for  Survey, recruitment and Catch
  cat("# AR_parameters(1,3) Survey, recruitment, Catch ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$ARParameters),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # flag for MSE or not. Determins output files only
  cat("# init_int flagMSE determins level of output (0 or 1)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$flagMSE),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)


  # end of file
  cat("# eof",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("54321",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)


}


write_PinFile <- function(dataList,listOfParameters){

  outPath <- paste0(listOfParameters$outDir,"/",listOfParameters$outputFilename)
  outputFileName <-  paste0(outPath,".pin")

  # write explanation of how this file was formed
  cat("#hydra_sim.pin for 10 species, 1 area (Georges Bank) for simulation, May 2013
      #note that species 9 and 10 have changed from ms3am test model",file=outputFileName,fill=listOfParameters$fillLength)
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#species are based on pars from LeMANS model, see LeMANSpars_fortesting.xlsx and other lit values",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#1: spinydog",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#2: winterskate",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#3: Aherring",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#4: Acod",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#5: haddock",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#6: yellowtailfl",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#7: winterfl",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#8: Amackerel",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#9: silverhake",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#10: goosefish",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # year 1 initial values of N
  cat("#//Initial N year 1",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_3darray yr1N(1,Nareas,1,Nspecies,1,Nsizebins)       //initial year N at size, millions",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # need to reformat for cat function
  Y1Nformat <- format(as.matrix(dataList$Y1N),digits=7)
  for (sp in 1:dataList$Nspecies) {
    cat(c(" ",Y1Nformat[sp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # recruitment. these are not used. If we delete then all previous code not compatible.
  # When we decide to forgo backward compatibility this will be deleted.
  cat("#//recruitment parameters from .pin file (now alts by spp read in from dat file; these defaults replaced in preliminary calcs)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$alphaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix eggRicker_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$shapeEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix eggRicker_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$betaEggRicker),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # redundant inputs recruitment Devs etc. never used.
  cat("#  //recruitment: average annual, annual devs, actual (avg+dev)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_matrix avg_recruitment(1,Nareas,1,Nspecies,avg_rec_phase)  //average annual recruitment by area, species ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#logspace (scaled by eye to produce flatline pops with pred mort but no fishing) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$redundantAvgRec),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  cat("#  init_3darray recruitment_devs(1,Nareas,1,Nspecies,1,Nyrs,dev_rec_phase)  //recruitment deviations by area, species",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (sp in 1:dataList$Nspecies) {
    cat(c(" ",dataList$redundantRecDevs[sp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }

  # recruitment sigma from S-R fits
  cat("#   init_matrix recsigma(1,Nareas,1,Nspecies)  //recruitment sigma",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",format(dataList$recSigma,digits=8)),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # fishery Q's
 # format for cat function
  fishQ <- format(as.matrix(dataList$fisheryq),digits=7)
  cat("# Fishery qs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  init_3darray fishery_q(1,Nareas,1,Nspecies,1,Nfleets,fqphase)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# area1.btrawl ptrawl longline)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",fishQ[isp,]," #",rownames(dataList$fisheryq)[isp]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


  # survey q
  cat("#  init_matrix survey_q(1,Nareas,1,Nspecies,sqphase)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$surveyq),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  # survey sigma (observation error)
  cat("#  init_matrix surv_sigma(1,Nareas,1,Nspecies,ssig_phase)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$surveySigma),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # fishery sigma (catch obs eror)
  cat("#  init_3darray catch_sigma(1,Nareas,1,Nspecies,1,Nfleets,csig_phase)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  fishSigq <- format(as.matrix(dataList$fisherySigma),digits=NULL)
  for (isp in 1:dataList$Nspecies) {
    cat(c(" ",fishSigq[isp,]),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }


}

