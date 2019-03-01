#' Data for Hydra data nd pin files (mshydradata)
#'
#'All the required information for creating data and pin files are stored in this this. This
#'data is used to create documentation and to create dat and fin files for use in simulaion with Hydra
#'
#' @format A list containing 117 elements
#' \describe{
#'   \item{alphaBH}{Numeric Vector: slope at origin (alpha) for Beverton Holt Stock recruitment relationship}
#'   \item{alphaDS}{Numeric Vector: slope at origin (alpha) for Deriso-Schnute Stock recruitment relationship}
#'   \item{alphaEggRicker}{Numeric Vector: slope at origin (alpha) for Ricker stock-egg production relationship}
#'   \item{alphaGamma}{Numeric Vector: slope at origin (alpha) for Gamma Stock recruitment relationship}
#'   \item{alphaHockey}{Numeric Vector: slope at origin (alpha) for Hockey Stick recruitment relationship}
#'   \item{alphaRicker}{Numeric Vector: slope at origin (alpha) for Ricker Stock recruitment relationship}
#'   \item{alphaSegmented}{Numeric Vector: slope at origin (alpha) for Segmented (linear) recruitment relationship}
#'   \item{alphaShepherd}{Numeric Vector: slope at origin (alpha) for Shepherd Stock recruitment relationship}
#'   \item{ARParameters}{Numeric Vector: Auto-regressive parameters for error strucure of Survey, Recruitment, & Catch (Default = (0,0,0))}
#'   \item{assessmentPeriod}{Integer scalar: length of time (years) between assessment of stocks}
#'   \item{avgFPhase}{??}
#'   \item{avgRecPhase}{??}
#'   \item{B0}{Numeric Vector: Levels of unfished biomass (metric tonnes)}
#'   \item{bandwidthMetric}{Integer scalar: number of years used to calculate standard deviation of catch and biomass (past \code{bandwidthMeric} years used in calculation)}
#'   \item{baselineThreshold}{Numeric scalar: Proportion of unfished biomass at which landings are prohibited}
#'   \item{betaBH}{Numeric Vector: (beta) for Beverton Holt Stock recruitment relationship}
#'   \item{betaDS}{Numeric Vector: (beta) for Deriso-Schnute Stock recruitment relationship}
#'   \item{betaEggRicker}{Numeric Vector: (beta) for Ricker stock-egg production relationship}
#'   \item{betaGamma}{Numeric Vector: (beta) for Gamma Stock recruitment relationship}
#'   \item{betaHockey}{Numeric Vector: (beta) for Hockey Stick recruitment relationship (Note: beta = -alpha for Hockey stick)}
#'   \item{betaRicker}{Numeric Vector: (beta) for Ricker Stock recruitment relationship}
#'   \item{betaSegmented}{Numeric Vector: (beta) for Segmented (linear) recruitment relationship}
#'   \item{betaShepherd}{Numeric Vector: (beta) for Shepherd Stock recruitment relationship}
#'   \item{binwidth}{Integer matrix (Nspecies x Nsizebins): width of each size class (cm)}
#'   \item{csigPhase}{??}
#'   \item{debugState}{Integer scalar: Type of debugging. (Values 1, 2, 3, 4). Default = 3}
#'   \item{devFPhase}{??}
#'   \item{devRecPhase}{??}
#'   \item{discardCoef}{Numeric matrix (Nspecies.Nfleets x Nsizebins): Proportion of catch that is discarded for each species/fleet/size class combination}
#'   \item{discardSurvival}{Numeric matrix (Nspecies.Nfleets x Nsizebins): Proportion of catch that survives given it is discarded for each species/fleet/size class combination}
#'   \item{exploitationOptions}{Numeric matrix (Nthresholds x Nscenarios): Step function for assessments for different scenarios. Each column defines exploitation rates for each threshold defined by \code{thresholds}}
#'   \item{fecundityd}{Numeric Vector: parameter (d) used to calculate fecundity at length (cm) }
#'   \item{fecundityh}{Numeric Vector: parameter (h) used to calculate fecundity at length (cm)}
#'   \item{fecundityTheta}{Numeric Vector: parameter (theta) used to calculate fecundity at length (cm)}
#'   \item{fisheryq}{Numeric matrix (Nspecies x Nfleets): Fish caught per fish available per unit effort and per unit time. Worded in a different way it represents the probability of any single fish being caught}
#'   \item{fisherySelectivityc}{Numeric matrix (Nspecies x Nfleets): parameter c. These are a measure of the selection process by fishing gear which causes the catch composition to be different from the general population. Worded in a different way it represents the probability of a fish remaining in the gear as a function of its size. It is used to control the impact of fishing on juveniles}
#'   \item{fisherySelectivityd}{Numeric matrix (Nspecies x Nfleets): parameter d. These are a measure of the selection process by fishing gear which causes the catch composition to be different from the general population. Worded in a different way it represents the probability of a fish remaining in the gear as a function of its size. It is used to control the impact of fishing on juveniles}
#'   \item{fisherySigma}{Numeric matrix (Nspecies x Nfleets): Standard deviation applied to catch (assumed log normal distribution)}
#'   \item{flagLinearRamp}{Integer scalar: Determins if a step function or a linear function is used to calculate exploitation rates during an assessment. Values: 0 (Step function) or 1 (linear function). Default = 1}
#'   \item{flagMSE}{Integer scalar: Determins whether an MSE is being performed. Values: 0 (no, extensive output files) or 1 (yes, limited output files). 2 (yes, Darwinian, catch and biomass only). Default = 0}
#'   \item{fleetNames}{Character vector: Names to characterize the fleets}
#'   \item{foodweb}{Binary matrix (Nspecies x Nspecies): Predators in columns, prey in rows. Value of 1 indicates predator eats prey}
#'   \item{fqphase}{??}
#'   \item{growthCov}{Numeric matrix (NgrowthCov x Nyrs): Covariates that determine species growth}
#'   \item{growthCovEffects}{Numeric matrix (Nspecies x NgrowthCov): Weight of each covariate for growth for each species}
#'   \item{growthK}{Numeric Vector (Nspecies): Values of parameter k assuming von Bertalanfy growth}
#'   \item{growthKappa}{Numeric Vector (Nspecies): Values of parameter kappa assuming exponential growth}
#'   \item{growthLinf}{Numeric Vector (Nspecies): Values of parameter Linf assuming von Bertalanfy growth}
#'   \item{growthPsi}{Numeric Vector (Nspecies): Values of parameter psi assuming exponential growth}
#'   \item{growthType}{Numeric Vector (Nspecies): Type of growth for each species. Values 2 (exponential) or 4 (Von Bertalanfy)}
#'   \item{guildMembership}{Numeric vector: (Nspecies): Determins guild membership for each species. (Values: 1 - numguilds). See \code{guildNames}}
#'   \item{guildNames}{Character vector (numGuilds): Names of the guilds}
#'   \item{indicatorFisheryq}{Binary matrix (Nspecies x Nfleets): Determins which species in each fleet are actually targeted catch (1) or not (0). This is used to determine exploitation rate durin an assessment}
#'   \item{intakeAlpha}{Numeric vector (Nspecies): alpha parameter for functional form of food intake}
#'   \item{intakeBeta}{Numeric vector (Nspecies): beta parameter (exponent) for functional form of food intake}
#'   \item{intakeStomach}{Numeric matrix (Nspecies x Nsizebins): Each row repsents species stomach weight (g) for each size class. Note. It is assumed that stomach weight remains constant through time (only for creating data input files. Hydra is modelled to acept changes through time.) }
#'   \item{lenwta}{Numeric vector (Nspecies): scale parameter for length to weight relationship}
#'   \item{lenwtb}{Numeric vector (Nspecies): shape parameter for length to weight relationship}
#'   \item{LFISize}{Numeric scalar: Length (cm) at which a fish is considered large. Used in health indices module}
#'   \item{M1}{Numeric matrix (Nspecies x Nsizebins): Rate of M1 mortality (natural mortality unexplained by model) for each species in in each size class AT EACH TIME STEP.}
#'   \item{M2sizePrefMu}{Numeric vector (Nspecies): size preference parameter (location parameter (mu) of the log normal distribution) }
#'   \item{M2sizePrefSigma}{Numeric vector (Nspecies): size preference parameter (scale parameter (sigma) of the log normal distribution)}
#'   \item{maturityCov}{Numeric matrix (NmaturityCov x Nyrs): Covariates that determine species maturity}
#'   \item{maturityCovEffects}{Numeric matrix (Nspecies x NmaturityCov): Weight of each covariate for maturity for each species}
#'   \item{maturityNu}{Numeric vector (Nspecies): maturity parameter (location)}
#'   \item{maturityOmega}{Numeric vector (Nspecies): maturity parameter (scale)}
#'   \item{minMaxThresholds}{Numeric vector (2): The minimum and maximum thresholds used during an assessment (as a proportion of unfished biomass). The maximum is the level at which effort is reduced (from maximum exploitation) due to overfishing. The minimum is the level in which minimum exploitation is reached. See \code{minMaxExploitation}}
#'   \item{Nareas}{Integer scalar: Number of spatial regions in the model.}
#'   \item{Nfleets}{Integer scalar: Number of fleets in the model. See \code{fleetNames}}
#'   \item{NgrowthCov}{Integer scalar: Number of growth covariates. See \code{growthCov}}
#'   \item{NmaturityCov}{Integer scalar: Number of maturity covatiates. See \code{maturityCov}}
#'   \item{NrecruitmentCov}{Integer scalar: Number og recruitment covariates. See \code{recruitmentCov}}
#'   \item{Nsizebins}{Integer scalar: Number of size classes (Each species must have same number of size classes, different widths.). See \code{binwidth}}
#'   \item{Nspecies}{Integer scalar: Number of species in the model}
#'   \item{Nthresholds}{Integer scalar: number of assessment threshold. See \code{thresholds} for thresholf values}
#'   \item{numGuilds}{Integer scalar: Number of guilds. See \code{guildNames}}
#'   \item{Nyrs}{Integer scalar: Number of years in the model}
#'   \item{observedBiomass}{Numeric matrix (Nspecies+1) x Nyrs: Real biomass values (from data) for each species over time. First row is year. Currently these are model values from ALTANTIS. Only used in estimation}
#'   \item{observedCatch}{Numeric matrix (Nspecies+1) x Nyrs: Real catch values (from data) for each species over time. First row is year. Currently these are model values from ALTANTIS. Only used in estimation}
#'   \item{observedEffort}{Numeric matrix (Nfleets+1) x Nyrs: Real fishing effort data for each fleet in the model. First row is year. }
#'   \item{observedTemperature}{Numeric matrix (2 x Nyrs): Real temperature values ('C) over time. First row is year.}
#'   \item{otherFood}{Numeric scalar: Amount of other food (metric tonnes) available for species in the model. This represents other food sources not explicity accounted for in the model }
#'   \item{phimax}{Numeric scalar: Scales the growth of fish to the rate of the fasting growing fish. This is set to 1 and shouldn't be a parameter. If the calculated growth rates are all < 1, then no species will grow out of a size class in the given time interval. In that case phimax = 1 is used.}
#'   \item{predOrPrey}{Binary vector: Indicated which species is considered a predator or prey. (This is subjective). This is used in as part of a health index where the ratio of predator to prey is monitored}
#'   \item{recphase}{??}
#'   \item{recruitCovEffects}{Numeric matrix (Nspecies x NmaturityCov): Weight of each covariate for recruitment for each species}
#'   \item{recruitmentCov}{Numeric matrix (NrecruitmentyCov x Nyrs): Covariates that determine species recruitment}
#'   \item{recSigma}{Numeric vector: standard deviation associated with recruitment estimates (based on spawning stock biomass size). (0 = known perfectly, no error). See \code{recStochastic}}
#'   \item{recStochastic}{Binary vector (Nspecies): A switch to turn on (1) and off (0) recruitment error}
#'   \item{recType}{Integer vector (Nspecies): Determins which form of recruitment is to be used. Values (1= Egg production, 2= Deriso-Scnute, 3= Gamma, 4= Ricker, 5= Beverton holt, 6= Shepherd, 7= Hockey Stick, 8= Segmented Linear model, 9= recruitment deviations). Default = 8 (Segmented linear model)}
#'   \item{redundantAvgRec}{Numeric vector (Nspecies): Average recruitment for each species}
#'   \item{redundantRecDevs}{Numeric matrix (Nspecies x Nyrs): Recruitment deviations about the average (\code{redundantAvgRec}) over time}
#'   \item{scaledEffort}{Numeric Vector (Nspecies): Amount by which to scale effort by species. Can be used if stock recruitment functions are calculated on an area larger or smaller than the region of interest. Default = 1's}
#'   \item{scaleInitialN}{Numeric scalar: Amount by which to scale the initial values of \code{Y1N}. Default = 1.}
#'   \item{sexRatio}{Numeric vector (Nspecies): Proportion of females for each species. Default = 0.5 for all species}
#'   \item{shapeBH}{Numeric Vector: (shape) for Beverton Holt Stock recruitment relationship}
#'   \item{shapeDS}{Numeric Vector: (shape) for Deriso-Schnute Stock recruitment relationship }
#'   \item{shapeEggRicker}{Numeric Vector: (shape) for Ricker stock-egg production relationship}
#'   \item{shapeGamma}{Numeric Vector: (shape) for Gamma Stock recruitment relationship}
#'   \item{shapeHockey}{Numeric Vector: (shape) for Hockey Stick recruitment relationship}
#'   \item{shapeRicker}{Numeric Vector: (shape) for Ricker Stock recruitment relationship}
#'   \item{shapeSegmented}{Numeric Vector: (shape) for Segmented (linear) recruitment relationship}
#'   \item{shapeShepherd}{Numeric Vector: (shape) for Shepherd Stock recruitment relationship}
#'   \item{speciesList}{Character vector: names of species in model}
#'   \item{sqphase}{??}
#'   \item{ssigPhase}{??}
#'   \item{surveyq}{Numeric vector (Nspecies): Proportion of true biomass sampled on a survey. (q = 1 equates to known exactly) }
#'   \item{surveySigma}{Numeric vector (Nspecies): The uncertainty (standard deviation) of estimate of survey biomass (0 = known perfectly, no error)}
#'   \item{thresholds}{Numeric vector (Nthresholds): The proportions of unfished biomass at which exploitation is decreased }
#'   \item{thresholdSpecies}{Numeric vector (Nspecies): A proportion in excess of the current \code{thresholds}. These species have reduced exploitaion at higher proportions of B/B0 }
#'   \item{wtconv}{Numeric Scalar: conversion factor from weight (g) to biomass}
#'   \item{Y1N}{Numeric Vector (Nspecies x Nsizebins): initial abundance estimates (milions) for each species in each size class }
#'   \item{yr1Nphase}{??}
#'
#'   \item{minMaxExploitation}{Numeric Vector (2): The minimum and maximum values of exploitation permissable during an assessment (as a proportion of unfished biomass). The maximum is the level at which all stocks are healthy. The minimum is the lowest level due to overfishing. See \code{minMaxThresholds}}
#'   \item{assessmentOn}{Binary Scalar: Assessment scenario on (1) or not (0). This is set in \code{\link{create_datpin_files}}}
#'   \item{assessmentWithSpeciesOn}{Binary Scalar: Assessment with extra protection for selected species. on (1) or not (0). This is set in \code{\link{create_datpin_files}}}
#'   \item{exploitationLevels}{Numeric Vector (Nthresholds): The values of exploitation at each threshold (Only used in step function Ramp). This is set in \code{\link{create_datpin_files}}. See \code{flagLinearRamp}}
#'}
#'
#'@seealso To visualize this data please run \code{create_documentation}
#'
#'@source Gaichas et al. + publication (currently being written up)
"hydraData"
