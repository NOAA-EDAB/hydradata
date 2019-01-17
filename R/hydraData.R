#' Data for Hydra data nd pin files (mshydradata)
#'
#'All the required information for creating data and pin files are stored in this this. This
#'data is used to create documentation and to create dat and fin files for use in simulaion with Hydra
#'
#' @format A list containing 133 elements
#' \describe{
#'   \item{alphaBH}{Numeric Vector: slope at origin (alpha) for Beverton Holt Stock recruitment relationship}
#'   \item{alphaDS}{Numeric Vector: slope at origin (alpha) for Deriso-Schnute Stock recruitment relationship}
#'   \item{alphaEggRicker}{Numeric Vector: slope at origin (alpha) for Ricker stock-egg production relationship}
#'   \item{alphaGamma}{Numeric Vector: slope at origin (alpha) for Gamma Stock recruitment relationship}
#'   \item{alphaHockey}{Numeric Vector: slope at origin (alpha) for Hockey Stick recruitment relationship}
#'   \item{alphaRicker}{Numeric Vector: slope at origin (alpha) for Ricker Stock recruitment relationship}
#'   \item{alphaSegmented}{Numeric Vector: slope at origin (alpha) for Segmented (linear) recruitment relationship}
#'   \item{alphaShepherd}{Numeric Vector: slope at origin (alpha) for Shepherd Stock recruitment relationship}
#'   \item{ARParameters}{Numeric Vector: Auto-regressive parameters for error strucure of Survey, Recruitment, & Catch}
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
#'   \item{flagLinearRamp}{Integer scaler: Determins if a step function or a linear function is used to calculate exploitation rates during an assessment. Values: 0 (Step function) or 1 (linear function). Default = 1}
#'   \item{flagMSE}{Integer scalar: Determins whether an MSE is being performed. Values: 0 (no, extensive output files) or 1 (yes, limited output files). default = 0}
#'   \item{fleetNames}{Character vector: Names to characterize the fleets}
#'   \item{foodweb}{Binary matrix (Nspecies x Nspecies): Predators in columns, prey in rows. Value of 1 indicates predator eats prey}
#'   \item{fqphase}{??}
#'   \item{growthCov}{Numeric matrix (NgrowthCov x Nyrs): Covariates that determine species growth}
#'   \item{growthCovEffects}{Numeric matrix (Nspecies x NgrowthCov): Weight of each covariate in growth for each species}
#'   \item{growthK}{Numeric Vector (Nspecies): Values of parameter k assuming von Bertalanfy growth}
#'   \item{growthKappa}{Numeric Vector (Nspecies): Values of parameter kappa assuming exponential growth}
#'   \item{growthLinf}{Numeric Vector (Nspecies): Values of parameter Linf assuming von Bertalanfy growth}
#'   \item{growthPsi}{Numeric Vector (Nspecies): Values of parameter psi assuming exponential growth}
#'   \item{growthType}{Numeric Vector (Nspecies): Type of growth for each species. Values 2 (exponential) or 4 (Von Bertalanfy)}
#'   \item{guildMembership}{}
#'   \item{guildNames}{}
#'   \item{indicatorFisheryq}{}
#'   \item{intakeAlpha}{}
#'   \item{intakeBeta}{}
#'   \item{intakeStomach}{}
#'   \item{lenwta}{}
#'   \item{lenwtb}{}
#'   \item{LFISize}{}
#'   \item{M1}{}
#'   \item{M2sizePrefMu}{}
#'   \item{M2sizePrefSigma}{}
#'   \item{maturityCov}{}
#'   \item{maturityCovEffects}{}
#'   \item{maturityNu}{}
#'   \item{maturityOmega}{}
#'   \item{minMaxThresholds}{}
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
#'   \item{observedBiomass}{}
#'   \item{observedCatch}{}
#'   \item{observedEffort}{}
#'   \item{observedTemperature}{}
#'   \item{otherFood}{}
#'   \item{phimax}{}
#'   \item{predOrPrey}{}
#'   \item{recphase}{}
#'   \item{recruitCovEffects}{}
#'   \item{recruitmentCov}{}
#'   \item{recSigma}{}
#'   \item{recStochastic}{}
#'   \item{recType}{}
#'   \item{redundantAvgRec}{}
#'   \item{redundantRecDevs}{}
#'   \item{scaledEffort}{}
#'   \item{scaleInitialN}{}
#'   \item{sexRatio}{}
#'   \item{shapeBH}{Numeric Vector: (shape) for Beverton Holt Stock recruitment relationship}
#'   \item{shapeDS}{Numeric Vector: (shape) for Deriso-Schnute Stock recruitment relationship }
#'   \item{shapeEggRicker}{Numeric Vector: (shape) for Ricker stock-egg production relationship}
#'   \item{shapeGamma}{Numeric Vector: (shape) for Gamma Stock recruitment relationship}
#'   \item{shapeHockey}{Numeric Vector: (shape) for Hockey Stick recruitment relationship}
#'   \item{shapeRicker}{Numeric Vector: (shape) for Ricker Stock recruitment relationship}
#'   \item{shapeSegmented}{Numeric Vector: (shape) for Segmented (linear) recruitment relationship}
#'   \item{shapeShepherd}{Numeric Vector: (shape) for Shepherd Stock recruitment relationship}
#'   \item{speciesList}{Character vector: names of species in model}
#'   \item{sqphase}{}
#'   \item{ssigPhase}{}
#'   \item{surveyq}{}
#'   \item{surveySigma}{}
#'   \item{thresholds}{}
#'   \item{thresholdSpecies}{}
#'   \item{wtconv}{}
#'   \item{Y1N}{}
#'   \item{yr1Nphase}{}






#'
#'}
#'
#'@seealso To visualize this data please run \code{create_documentation}
#'
#'@source Gaichas et al. + publication (currently being written up)
"hydraData"
