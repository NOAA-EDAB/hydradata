# Read in all of the initial csv files and create RData files to lazily load in package

create_RData <- function() {
  path <- "data-raw"
  # read in data files associated with pin and dat file
  d <- get_DatData(path)
  p <- get_PinData(path)

  d$recName <- rep("segmented",d$Nspecies) # this is a hack. NEED to sort out data inputs

  # add all of fields to hydraData
  hydraDataList <- d
  hydraDataList <- modifyList(hydraDataList,p)

  # save each one as an RData file to lazily load with package
  # for (eachName in names(d)){
  #   #assigns the list variable to its own variable
  #   assign(eachName,d[[eachName]])
  #   save(list=eachName,file=paste0("data/",eachName,".rda"))
  # }
  # for (eachName in names(p)){
  #   #assigns the list variable to its own variable
  #   assign(eachName,p[[eachName]])
  #   save(list=eachName,file=paste0("data/",eachName,".rda"))
  # }

  usethis::use_data(hydraDataList,overwrite = TRUE)

}

## subfunctions get_pinData, get_DatData

get_DatData <- function(path){
  # We stipulate all of the input data needed to write to the .dat file
  # Eventually it will be better to read all of these in from text files or a GUI. For now this will suffice
  d <- list() # set up list for data storage

  # path to data
  #path <- paste0(getwd(),"/",options$pathToDataFiles)

  # list of species and guilds (Functional Groups)
  speciesList <- read.csv(paste0(path,"/speciesList.csv"),header=TRUE)
  d$speciesList <- as.character(speciesList$species)
  d$guildNames <- as.character(speciesList$guild)
  d$numGuilds <- length(unique(d$guildNames))
  d$guildMembership <- speciesList$guildmember
  d$predOrPrey <- speciesList$predOrPrey


  singletons <- read.csv(paste0(path,"/singletons.csv"),header=FALSE,row.names = 1)
  d$debugState <- singletons["debug",]
  d$Nyrs <- singletons["Nyrs",]
  d$Nspecies <- singletons["Nspecies",]
  d$Nsizebins <- singletons["Nsizebins",]
  d$Nareas <- singletons["Nareas",]
  d$Nfleets <- singletons["Nfleets",]
  d$wtconv <- singletons["wtconv",]
  d$yr1Nphase <- singletons["yr1Nphase",]
  d$recphase <- singletons["recphase",]
  d$avgRecPhase <- singletons["avg_rec_phase",]
  d$avgFPhase <- singletons["avg_F_phase",]
  d$devRecPhase <- singletons["dev_rec_phase",]
  d$devFPhase <- singletons["dev_F_phase",]
  d$fqphase <- singletons["fqphase",]
  d$sqphase <- singletons["sqphase",]
  d$ssigPhase <- singletons["ssig_phase",]
  d$csigPhase <- singletons["csig_phase",]
  d$phimax <- singletons["phimax",]
  d$scaleInitialN <- singletons["scaleInitialN",]
  d$otherFood <- singletons["otherFood",]
  d$LFISize <- singletons["LFI_size",]
  d$bandwidthMetric <- singletons["bandwidth_metric",]
  d$assessmentPeriod <- unlist(singletons["assessmentPeriod",])
  d$flagMSE<- unlist(singletons["flagMSE",])
  d$flagLinearRamp <- unlist(singletons["flagLinearRamp",])
  d$baselineThreshold <- unlist(singletons["baseline_threshold",])


  # sizebin lengths
  binwidth <- read.csv(paste0(path,"/length_sizebins.csv"),header=TRUE)
  d$binwidth <- binwidth[1:d$Nspecies,1:d$Nsizebins]
  row.names(d$binwidth) <- binwidth[,ncol(binwidth)]

  # length to weight coefficients/parameters
  lenwt <- read.csv(paste0(path,"/lengthweight_species.csv"),header=TRUE)
  d$lenwta <- lenwt$a
  d$lenwtb <- lenwt$b

  # covariate information relating to recruitment, growth and maturity
  recruitmentCovs <- read.csv(paste0(path,"/recruitment_covariates.csv"),header=TRUE)
  maturityCovs <- read.csv(paste0(path,"/maturity_covariates.csv"),header=TRUE)
  growthCovs <- read.csv(paste0(path,"/growth_covariates.csv"),header=TRUE)

  d$recruitmentCov <- t(recruitmentCovs)
  d$maturityCov <- t(maturityCovs)
  d$growthCov <- t(growthCovs)
  # number of covariates
  d$NrecruitmentCov <- dim(d$recruitmentCov)[1]
  d$NmaturityCov <- dim(d$maturityCov)[1]
  d$NgrowthCov <- dim(d$growthCov)[1]

  # observed survey biomass
  obsBio <- read.csv(paste0(path,"/observation_biomass.csv"),header=TRUE)
  d$observedBiomass <- t(obsBio)

  # observed survey biomass
  obsCatch <- read.csv(paste0(path,"/observation_catch.csv"),header=TRUE)
  d$observedCatch <- t(obsCatch)

  # observed effort by fleet
  obsEffort <- read.csv(paste0(path,"/observation_effort.csv"),header=TRUE)
  d$observedEffort <- t(obsEffort)
  d$fleetNames <- (names(obsEffort)[2:(d$Nfleets+1)])

  # observed temperature
  obsTemp <- read.csv(paste0(path,"/observation_temperature.csv"),header=TRUE)
  d$observedTemperature <- t(obsTemp)

  # stomach weight
  stomachContent <- read.csv(paste0(path,"/intake_stomachContent.csv"),header=TRUE)
  d$intakeStomach <- as.matrix(stomachContent[,2:(d$Nsizebins+1)])

  # recruitment parameters
  stockRecruit <- read.csv(paste0(path,"/recruitment_species.csv"),header=TRUE,row.names=1)

  d$alphaEggRicker <- unlist(stockRecruit["eggRicker_alpha",])
  d$shapeEggRicker <- unlist(stockRecruit["eggRicker_shape",])
  d$betaEggRicker <- unlist(stockRecruit["eggRicker_beta",])
  d$alphaDS <- unlist(stockRecruit["DS_alpha",])
  d$shapeDS <- unlist(stockRecruit["DS_shape",])
  d$betaDS <- unlist(stockRecruit["DS_beta",])
  d$alphaGamma <- unlist(stockRecruit["gamma_alpha",])
  d$shapeGamma <- unlist(stockRecruit["gamma_shape",])
  d$betaGamma <- unlist(stockRecruit["gamma_beta",])
  d$alphaRicker <- unlist(stockRecruit["ricker_alpha",])
  d$shapeRicker <- unlist(stockRecruit["ricker_shape",])
  d$betaRicker <- unlist(stockRecruit["ricker_beta",])
  d$alphaBH <- unlist(stockRecruit["BH_alpha",])
  d$shapeBH <- unlist(stockRecruit["BH_shape",])
  d$betaBH <- unlist(stockRecruit["BH_beta",])
  d$alphaShepherd <- unlist(stockRecruit["shepherd_alpha",])
  d$shapeShepherd <- unlist(stockRecruit["shepherd_shape",])
  d$betaShepherd<- unlist(stockRecruit["shepherd_beta",])
  d$alphaHockey <- unlist(stockRecruit["hockey_alpha",])
  d$shapeHockey <- unlist(stockRecruit["hockey_shape",])
  d$betaHockey <- unlist(stockRecruit["hockey_beta",])
  d$alphaSegmented <- unlist(stockRecruit["segmented_alpha",])
  d$shapeSegmented <- unlist(stockRecruit["segmented_shape",])
  d$betaSegmented <- unlist(stockRecruit["segmented_beta",])

  d$recSigma <- unlist(stockRecruit["sigma",])
  d$recType <- unlist(stockRecruit["type",])
  d$recStochastic <- unlist(stockRecruit["stochastic",])


  # sex ratio
  sexRatio <- read.csv(paste0(path,"/sexratio.csv"),header=TRUE,row.names=1)
  d$sexRatio <- unlist(sexRatio)

  # recruitment covariate effects. # columns = d$Nrecruitment_cov
  rec_covEffects <- read.csv(paste0(path,"/recruitment_covariateEffects.csv"),header=TRUE,row.names=1)
  d$recruitCovEffects <- as.matrix(rec_covEffects)

  # fecundity
  fecundity_d_h <- read.csv(paste0(path,"/fecundity_species.csv"),header=TRUE,row.names=1)
  d$fecundityd <- unlist(fecundity_d_h["d",])
  d$fecundityh <- unlist(fecundity_d_h["h",])

  fecundity_Theta <- read.csv(paste0(path,"/fecundity_Theta.csv"),header=TRUE,row.names=1)
  d$fecundityTheta <- format(as.matrix(fecundity_Theta),digits=5)

  # maturity
  maturity <- read.csv(paste0(path,"/maturity_species.csv"),header=TRUE,row.names=1)
  d$maturityNu <- unlist(maturity["nu",])
  d$maturityOmega <- unlist(maturity["omega",])

  maturity_covEffects <- read.csv(paste0(path,"/maturity_covariateEffects.csv"),header=TRUE,row.names=1)
  d$maturityCovEffects <- as.matrix(maturity_covEffects)

  # growth
  growth <- read.csv(paste0(path,"/growth_species.csv"),header=TRUE,row.names=1)
  d$growthPsi <- unlist(growth["psi",])
  d$growthKappa <- unlist(growth["kappa",])
  d$growthLinf <- unlist(growth["Linf",])
  d$growthK <- unlist(growth["k",])
  d$growthType <- unlist(growth["growthType",])

  growth_covEffects <- read.csv(paste0(path,"/growth_covariateEffects.csv"),header=TRUE,row.names=1)
  d$growthCovEffects <- as.matrix(growth_covEffects)

  # intake
  intake <- read.csv(paste0(path,"/intake_species.csv"),header=TRUE,row.names=1)
  d$intakeAlpha <- unlist(intake["alpha",])
  d$intakeBeta <- unlist(intake["beta",])

  # M1
  M1 <- read.csv(paste0(path,"/mortality_M1.csv"),header=TRUE,row.names = 1)
  d$M1 <- as.matrix(M1)

  #foodweb
  foodweb <- read.csv(paste0(path,"/foodweb.csv"),header=TRUE,row.names = 1)
  d$foodweb <- as.matrix(foodweb)

  #M2 size preference
  M2sizePref <- read.csv(paste0(path,"/mortality_M2sizePreference.csv"),header=TRUE,row.names = 1)
  d$M2sizePrefMu <- as.matrix(M2sizePref["mu",])
  d$M2sizePrefSigma <- as.matrix(M2sizePref["sigma",])

  #fishery/fleet selectivity
  fisherySelectivityc<- read.csv(paste0(path,"/fishing_selectivityc.csv"),header=TRUE,row.names = 1)
  d$fisherySelectivityc <- fisherySelectivityc
  fisherySelectivityd<- read.csv(paste0(path,"/fishing_selectivityd.csv"),header=TRUE,row.names = 1)
  d$fisherySelectivityd <- fisherySelectivityd

  # B0 - equilibrium biomass
  B0 <- read.csv(paste0(path,"/B0.csv"),header=TRUE,row.names = 1)
  d$B0 <- unlist(B0)

  # assessment thresholds + exploitations
  assessmentThresholds <-  read.csv(paste0(path,"/assessmentThresholds.csv"),header=TRUE)
  d$thresholds <- assessmentThresholds$thresholds
  thresholds <- d$thresholds[d$thresholds< 1]
  d$Nthresholds <- length(d$thresholds)
  d$exploitationOptions <- assessmentThresholds[,2:dim(assessmentThresholds)[2]]
  d$minMaxThresholds <- c(min(thresholds),max(thresholds))

  # additionl level added to species specific threshold
  assessmentThresholdsSpecies <-  read.csv(paste0(path,"/assessmentThresholdsSpecies.csv"),header=TRUE,row.names = 1)
  d$thresholdSpecies <- unlist(assessmentThresholdsSpecies)

  #scaled Efort - not used
  scaledEffort <-  read.csv(paste0(path,"/observation_effortScaling.csv"),header=TRUE)
  d$scaledEffort <- unlist(scaledEffort)

  # discard coefficient - prob of discard
  discardCoef <-  read.csv(paste0(path,"/fishing_discards.csv"),header=TRUE,skip=3,row.names = 1)
  d$discardCoef <- (unlist(as.matrix(discardCoef)))

  # discard survival probability | discard
  discardSurvival <-  read.csv(paste0(path,"/fishing_discardsSurvival.csv"),header=TRUE,skip=3,row.names = 1)
  d$discardSurvival <- (unlist(as.matrix(discardSurvival)))


  # fishery catchability indicator (q's)
  indicatorFisheryqs<- read.csv(paste0(path,"/fishing_q_indicator.csv"),header=TRUE,row.names = 1)
  d$indicatorFisheryq<- unlist(as.matrix(indicatorFisheryqs))

  # Autoregressive parameters for alternative error structure (AR) for survey, recruitment, catch
  ARparameters<- read.csv(paste0(path,"/observation_error.csv"),header=TRUE)
  d$ARParameters <- unlist(ARparameters)

  return(d)

}



get_PinData <- function(path){
  # Stipulate all information required for the pin data file
  p <- list()
  # path to data
  # list of species and guilds (Functional Groups)
  Y1N <- read.csv(paste0(path,"/observation_Y1N.csv"),header=TRUE,row.names=1)
  p$Y1N <- Y1N


  # redundant Avg recruitemtn and deviations
  redundantAvgRec <- read.csv(paste0(path,"/redundantAvgRecPinData.csv"),header=TRUE,row.names=1)
  p$redundantAvgRec <- unlist(redundantAvgRec)
  redundantRecDevs <- read.csv(paste0(path,"/redundantRecDevsPinData.csv"),header=TRUE,row.names=1)
  p$redundantRecDevs <- unlist(t(as.matrix(redundantRecDevs)))


  # fishery catchability (q's)
  fisheryqs<- read.csv(paste0(path,"/fishing_q.csv"),header=TRUE,row.names = 1)
  p$fisheryq<- fisheryqs


  # survey q and obs error
  survey<- read.csv(paste0(path,"/survey_info.csv"),header=TRUE,row.names = 1)
  p$surveyq<- unlist(survey["q",])
  p$surveySigma<- unlist(survey["obs_error",])

  # fishing error
  fishery_sigma <- read.csv(paste0(path,"/fishing_error.csv"),header=TRUE,row.names = 1)
  p$fisherySigma <- fishery_sigma

  return(p)
}
