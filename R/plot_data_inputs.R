#' A plotting suite for Hydra.
#' These functions comprise plotting routines for each component of the data
#'
#'This isnt exported with the package

# plots Intake, stomach content,
binWidths_Calc <- function(){
  #binWidths <- read.table(paste0(inPath,"length_sizebins.csv"),sep=",",header=TRUE)
  binWidths <- binwidth #lazy data
  # finds midpoints of bins for biomass plot
  #binWidths <- t(head(t(binWidths),-1))
  #class(binWidths) <- "numeric"
  cumSumBinWidths <- t(apply(binWidths,1,cumsum))
  v <- rep(0,dim(binWidths)[1])
  cumSumBinWidths <- cbind(v,cumSumBinWidths)
  midPoints <- cumSumBinWidths + (0.5*cbind(binWidths,v))
  midPoints <- t(head(t(midPoints),-1))
  nClass <- dim(binWidths)[2]
  return(list(midPoints=midPoints,cumSumBinWidths=cumSumBinWidths,nClass=nClass))

}

# plots the rates of survival and discrad for species where entries are no zero
plot_Discards_Survival <- function(outPath){
  #singles <- read.table(paste0(inPath,"singletons.csv"),sep=",",row.names=1,header=FALSE)
  #discards <- read.csv(paste0(inPath,"fishing_discards.csv"),sep=",",skip=3,header=TRUE,row.names=1)
  #survival <- read.csv(paste0(inPath,"fishing_discardsSurvival.csv"),sep=",",skip=3,header=TRUE,row.names=1)
  #singletons <- simplify2array(singles)
  #names(singletons) <- row.names(singles)
  nFleets <- Nfleets
  nSpecies <- Nspecies
  nClass <- Nsizebins

  discards <- discardCoef
  survival <- discardSurvival

  for (isp in 1:nSpecies) {
    # take in chunks of nFleets
    istart <- (isp-1)*nFleets + 1
    iend <- istart+nFleets-1
    speciesDataD <- discards[istart:iend,]
    if(sum(as.numeric(as.matrix(speciesDataD))) > 0) {
      print(speciesDataD)
      speciesGearName <- row.names(speciesDataD)[1]
      print(speciesGearName)
      numLetters <- nchar(speciesGearName)
      speciesName <- substr(speciesGearName,1,numLetters-2)
      gearType <- substr(speciesGearName,numLetters-1,numLetters)
      print(speciesName)
      print(gearType)
      matplot(t(speciesDataD),type="l")

   #   print(survival[istart:iend,])

    }

  }
}


plot_observed_Biomass_Catch <- function(outPath){
  #biomass <- read.table(paste0(inPath,"observation_biomass.csv"),sep=",",header=TRUE)
  biomass <- t(observedBiomass)
  #catch <- read.table(paste0(inPath,"observation_catch.csv"),sep=",",header=TRUE)
  catch <- t(observedCatch)
  nCols <- dim(biomass)[2]
  png(paste0(outPath,"/Hydra_Biomass.png"),width=11.5,height=8,units="in",res=300)
  par(oma=c(1,4,1,2))
  matplot(biomass[,"year"],biomass[,c(2:nCols)], type="l",xlab="Year",ylab = "",main="Observed Biomass")
  legend("topright",legend=colnames(biomass)[2:nCols],col=seq_len(nCols-1),lty=seq_len(nCols-1))
  graphics.off()

  png(paste0(outPath,"/Hydra_Catch.png"),width=11.5,height=8,units="in",res=300)
  par(oma=c(1,4,1,2))
  matplot(catch[,"year"],catch[,c(2:nCols)], type="l",xlab="Year",ylab = "",main="Observed Catch")
  legend("topright",legend=colnames(catch)[2:nCols],col=seq_len(nCols-1),lty=seq_len(nCols-1))
  graphics.off()

 # png(paste0(outPath,"/Hydra_biomass.png"),width=11.5,height=8,units="in",res=300)

}

# plots all covariate data
plot_CovariateData <- function(outPath) {
  #growthCov <- read.table(paste0(inPath,"growth_covariates.csv"),sep=",",header=TRUE)
  #maturityCov <- read.table(paste0(inPath,"maturity_covariates.csv"),sep=",",header=TRUE)
  #recruitmentCov <- read.table(paste0(inPath,"recruitment_covariates.csv"),sep=",",header=TRUE)
  growthEffects <- growthCovEffects #read.table(paste0(inPath,"growth_covariateEffects.csv"),sep=",",header=TRUE)
  maturityEffects <- maturityCovEffects #read.table(paste0(inPath,"maturity_covariateEffects.csv"),sep=",",header=TRUE)
  recruitmentEffects <- recruitCovEffects #read.table(paste0(inPath,"recruitment_covariateEffects.csv"),sep=",",header=TRUE)
  nSpecies <- dim(growthEffects)[1]


  effects <- cbind(growthEffects,maturityEffects,recruitmentEffects)
  covariates <- t(rbind(growthCov,maturityCov,recruitmentCov))
  names(covariates) <- c("temperature(Growth)","maturity","recruitment")
  nCovs <- dim(covariates)[2]

  png(paste0(outPath,"/Hydra_Covariates.png"),width=8,height=11.5,units="in",res=300)
  mat <- matrix(c(1,2,3,4,5,6),3,2)
  graphics::layout(mat,widths=c(6,2))
  par(mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+0.5)
  for (icov in 1:nCovs) {
    plot(covariates[,icov],type="l",cex=1.6,lwd=2)
    legend("topleft",legend=names(covariates)[icov],bty="n",cex=1.5)
  }
  for (icov in 1:nCovs) {
    plot(effects[,icov],c(1:nSpecies),type="l",cex=1.6,lwd=2)
    mtext("Species Effect",line=1.9,side=2,cex=1)
  }
 graphics.off()
}

# size preferenmce * weight ratio
plot_SizePreference_weightRatio <- function(outPath) {
  #sizePref <- as.data.frame(t(read.table(paste0(inPath,"mortality_M2sizePreference.csv"),sep=",",row.names=1,header=TRUE)))
  speciesNames <- colnames(M2sizePrefMu)
  speciesNames[speciesNames=="goosefish"] <- "monkfish"
  nSpecies <- length(speciesNames)

  # weight ratio
  out <- binWidths_Calc()
  midPoints <- out$midPoints
  nClass <- out$nClass


  png(paste0(outPath,"/Hydra_SizePreference.png"),width=8,height= 11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)

  weightRatio <- seq(0.0001,10,.0001)
  meanSizePreference <- vector(mode="numeric",length = nSpecies)
  for (isp in 1:nSpecies) {
    meanlog <- M2sizePrefMu[isp]
    sdlog <- M2sizePrefSigma[isp]

    pdfLogN <- (1/(weightRatio*sdlog*sqrt(2*pi))) *exp(-((log(weightRatio)-meanlog)^2)/(2*sdlog^2))
    meanSizePreference[isp] <- weightRatio[which(max(pdfLogN)==pdfLogN)]
    plot(log(weightRatio),pdfLogN,type="l",xlab="",ylab="",yaxt="n",xaxt="n")
    axis(1, at=c(log(c(.0001,.001, 0.01, .1,1,10))), labels=c(.0001,.001,.01,.1,1,10))

    legend("topleft",legend=speciesNames[isp])
  }
  mtext("Weight Ratio",line = 1.7,outer=T, side=1,cex = 1.5)
  mtext("PDF",line = -.5,outer=T, side=2,cex = 1.5)
  graphics.off()

  # weight ratio is calculated for each pair of species. However some species dont interact .We only print ratios of predators
  #foodweb <- as.matrix(read.table(paste0(inPath,"foodweb.csv"),sep=",",header=TRUE,row.names=1))
  predators <- which(colSums(foodweb)>0)
  nPredators <- length(predators)

  # calculate weight at each size class for each species
  #lengthWeight <- read.table(paste0(inPath,"lengthweight_species.csv"),sep=",",header=TRUE,row.names=1)

  weight <- vector(mode = "list", length = nSpecies)
  for (isp in 1:nSpecies) {
    a <- lenwta[isp]
    b <- lenwtb[isp]
    weight[[isp]] <- a*midPoints[isp,]^b
  }
  # for each predator we find ratios for each prey and plot
  for (isp in 1:nPredators) {
    ipred <- predators[isp]
    prey <- which(as.logical(foodweb[,ipred]))
    preyNames <- speciesNames[prey]
    nPrey <- length(prey)
    png(paste0(outPath,"/Hydra_weightRatio_",speciesNames[ipred],".png"),width=8,height= 11.5,units="in",res=300)
    par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)
    for (iprey in 1:nPrey) {
      preyW <- matrix(rep(weight[[prey[iprey]]],nClass),nrow=nClass)
      predW <- matrix(rep(weight[[ipred]],nClass),ncol=nClass,byrow=TRUE)
      wR <- (preyW/predW)
      wR[lower.tri(wR,diag=TRUE)] <- NA

      matplot(t(wR),type="b",ylim=c(0,max(wR,na.rm = TRUE)),xlab="",ylab="")
      abline(meanSizePreference[isp],0,col="black")
      legend("topleft",legend=preyNames[iprey])
      #invisible(readline("Press [Enter] ..."))
    }
    mtext(paste0("Predator: ",speciesNames[ipred]),line = -.5,outer=T, side=3,cex = 1.5)
    mtext("Predator Size Class",line = 1.7,outer=T, side=1,cex = 1.5)
    graphics.off()
  }


}

# plots Fecundity
plot_Maturity <- function(outPath) {
  maturity_cov <- maturityCov #(lazy data)
  maturity_effects <- maturityCovEffects #(lazy data)

  speciesNames <- names(maturityNu)
  speciesNames[speciesNames=="goosefish"] <- "monkfish"
  nSpecies <- length(speciesNames)

  out <- binWidths_Calc()
  midPoints <- out$midPoints
  nClass <- out$nClass

  png(paste0(outPath,"/Hydra_Maturity.png"),width=8,height= 11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)

  for (isp in 1:nSpecies) {
    nu <- maturityNu[isp]
    omega <- maturityOmega[isp]
    maturityCalc <- 1/(1+exp(-(nu + omega*midPoints[isp,])))
    if (speciesNames[isp]=="spinydog") {maturityCalc=c(rep(0,nClass-1),1)} # hardcoded in ADMB
    plot(midPoints[isp,],maturityCalc,type="b")

    legend("topleft",legend=speciesNames[isp])
  }
  mtext("Length (cm)",line = 1.7,outer=T, side=1,cex = 1.5)
  mtext("Proportion Mature",line = -.5,outer=T, side=2,cex = 1.5)
  graphics.off()

}

#  intake
plot_Intake <- function(outPath) {
  speciesNames <- names(intakeAlpha)
  nSpecies <- length(speciesNames)
  nClasses <- dim(intakeStomach)[2]

  png(paste0(outPath,"/Hydra_Intake.png"),width=8,height= 11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)
  temperature <- seq(from=7.97, to=9.97, by=.01)
  for (isp in 1:nSpecies) {
    alpha <- intakeAlpha[isp]
    beta <- intakeBeta[isp]
    StomachContent <- intakeStomach[isp,]
    IntakeCalc <- 24*alpha*exp(beta*temperature)
    minI <- min(IntakeCalc)*min(StomachContent)
    maxI <- max(IntakeCalc)*max(StomachContent)

    if (speciesNames[isp] == "goosefish") {speciesNames[isp] <- "monkfish"}

    for (isize in 1:nClasses) {
      if (isize == 1) {
       # plot(temperature,IntakeCalc,type="l")
        plot(temperature,IntakeCalc*as.numeric(StomachContent[isize]),type="l",ylim=c(minI,maxI))
      } else {
        lines(temperature,IntakeCalc*as.numeric(StomachContent[isize]))
      }
    }

    legend("topleft",legend=speciesNames[isp])
  }
  mtext("Temperature ('C)",line = 1.7,outer=T, side=1,cex = 1.5)
  mtext("Daily Intake (g)",line = -.5,outer=T, side=2,cex = 1.5)
  graphics.off()


}

# plots Fecundity
plot_Fecundity <- function(outPath) {
  #fecundity <- as.data.frame(t(read.table(paste0(inPath,"fecundity_species.csv"),sep=",",row.names=1,header=TRUE)))
  fecundity_theta <- fecundityTheta # (lazy data)

  out <- binWidths_Calc()
  midPoints <- out$midPoints

  speciesNames <- names(fecundityd)
  #fecundity_theta <- as.numeric(fecundity_theta[,-1])
  speciesNames[speciesNames=="goosefish"] <- "monkfish"
  nSpecies <- length(speciesNames)

  png(paste0(outPath,"/Hydra_Fecundity.png"),width=8,height= 11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)

  for (isp in 1:nSpecies) {
    fecund <- fecundityd[isp]*as.numeric(fecundity_theta[isp,])*(midPoints[isp,]^fecundityh[isp])
    plot(midPoints[isp,],fecund,main="",type = "b")
    legend("topleft",legend=speciesNames[isp])
  }
  mtext("Length (cm)",line = 1.7,outer=T, side=1,cex = 1.5)
  mtext("Fecundity",line = -.5,outer=T, side=2,cex = 1.5)
  graphics.off()

}


# plots all paramaterized versions of SR. Shepher, BH, ricker, segmented etc
plot_StockRecruitment <- function(outPath) {
  ##########################################################################3
  # plot stock recruitment
  #SR <- read.table(paste0(inPath,"recruitment_species.csv"),sep=",",row.names=1,header=TRUE)
  speciesNames <- names(recType)
  speciesNames[speciesNames=="goosefish"] <- "monkfish"
  nSpecies <- length(speciesNames)

  SR_types <- c("EggRicker","DS","Gamma","Ricker","BH","Shepherd","Hockey","Segmented")


    png(paste0(outPath,"/Hydra_StockRecruitment.png"),width=8,height= 11.5,units="in",res=300)
    par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)

    for (isp in 1:nSpecies) {
#      currentType <- SR_types[SR["type",isp]]
      currentType <- SR_types[recType[isp]]
      assign("tempAlpha",paste0("alpha",currentType))
      alpha <- eval(as.name(tempAlpha))[isp]

      assign("tempBeta",paste0("beta",currentType))
      beta <- eval(as.name(tempBeta))[isp]

      assign("tempShape",paste0("shape",currentType))
      shape <- eval(as.name(tempShape))[isp]


      breakpoint <- shape # for segmented
      SSB <- seq(0,2.5*breakpoint,breakpoint)
      ind <- SSB > breakpoint
      R <- alpha*SSB  + (beta*(SSB-breakpoint))*ind

      plot(SSB,R,main="",type = "l",ylim=c(0,max(R)))
      legend("topleft",legend=speciesNames[isp],bty="n",cex=1.5)
      legend("bottomright",legend=currentType,bty="n",cex=1)
    }
    mtext("SSB",line = 1.7,outer=T, side=1,cex = 1.5)
    mtext("Recruitment",line = -.5,outer=T, side=2,cex = 1.5)
    graphics.off()
}


# plots observed temperature
plot_Temperature <- function(outPath) {
  temperature <- observedTemperature #read.table(paste0(inPath,"observation_temperature.csv"),sep=",",header=TRUE)
  png(paste0(outPath,"/Hydra_Temperature.png"),width=11.5,height=8,units="in",res=300)
  par(oma=c(1,4,1,2))
  plot(temperature["year",],temperature["temperature",], type="b",xlab="Year",ylab = "Temperature ('C)")
  graphics.off()
}

# plots fooweb structure in matrix and graph format
plot_Foodweb <- function(outPath) {
  #foodweb <- as.matrix(read.table(paste0(inPath,"foodweb.csv"),sep=",",header=TRUE,row.names=1))
  speciesnames <- speciesList #read.table(paste0(inPath,"speciesList.csv"),sep=",",header=TRUE)
  guilds <- unique(as.data.frame(cbind(guildNames,guildMembership))) # (lazy data)
  guilds$guildMembership <- as.integer(guilds$guildMembership)
  guilds <- guilds[with(guilds,order(guildMembership)),]
  nGuilds <- dim(guilds)[1]
  colorVec <- viridis::viridis(nGuilds)

  web <- igraph::graph_from_adjacency_matrix(foodweb,mode="directed") #(lazy data)
  # average-linkage clustering method
  #jaccard <- similarity(web,vids=V(web),mode="all",loops = TRUE,method="jaccard")
  #cc = hclust(1-as.dist(jaccard), method = "average")
  #plot(cc)
  #return(jaccard)

  # make igraph object
  # add attributed for plottinf
  #webAttributes <- data.frame(guild=NULL,guildMember=NULL,color=NULL)
  nSpecies <- dim(foodweb)[1]
  for (isp in 1:nSpecies) {
    #ind <- which(names(igraph::V(web)[isp]) == speciesList$species)
    ind <- which(names(igraph::V(web)[isp]) == speciesnames)
    igraph::V(web)$guildmember[isp] <- guildMembership[ind]
    igraph::V(web)$guild[isp] <- as.character(guildNames[ind])
    igraph::V(web)$color[isp] <- colorVec[guildMembership[isp]]
    igraph::V(web)$predPrey[isp] <- predOrPrey[isp]
  }

  l <- igraph::layout_with_lgl(web)
  tl <- NetIndices::TrophInd(foodweb) # determins tropic level. netIndices package
  l[,2] <- tl$TL # replace the y location column with tropich level

  png(paste0(outPath,"/Hydra_foodWeb_matrix.png"),width=4,height=4,units="in",res=300)
  par(oma=c(1,0,1,1))

  bipartite::visweb(web = foodweb,  prednames = TRUE, preynames = TRUE, labsize = .5,
         type = "none", clear = FALSE)
  graphics.off()

  png(paste0(outPath,"/Hydra_foodWeb_network.png"),width=6,height=6,units="in",res=300)
  par(oma=c(1,1,1,1))


  igraph::plot.igraph(web,edge.arrow.size=.5,vertex.color=igraph::V(web)$color,vertex.size= 20* 2^(igraph::V(web)$predPrey),
              layout=l)
  legend("topright",legend=guilds$guildNames,pch=21,
         pt.bg=colorVec, pt.cex=2, cex=.8, bty="n", ncol=1)

  graphics.off()
}


### plots Y1N and corresponding starting biomass
### also plots length weight relationsships and bin widths
# all connected
plot_Y1N_Biomass_lengthweight_bins <- function(outPath) {
  cex <- 0.8

  #Y1N <- read.table(paste0(inPath,"observation_Y1N.csv"),sep=",",header=TRUE,row.names=1)

  out <- binWidths_Calc()
  midPoints <- out$midPoints
  cumSumBinWidths <- out$cumSumBinWidths

  #singletons <- read.table(paste0(inPath,"singletons.csv"),sep=",",header=TRUE,row.names=1)
  weightConversion <- wtconv # from grams

  #lengthWeight <- read.table(paste0(inPath,"lengthweight_species.csv"),sep=",",header=TRUE,row.names=1)

  nSpecies <- length(row.names(Y1N))
  speciesNames <- row.names(Y1N)
  nBins <- dim(Y1N)[2]

  ## binwidths
  #png(system.file("rmd","Hydra_binWidths.png",package="mshydradata"),width=11.5,height=8,units="in",res=300)
  png(paste0(outPath,"/Hydra_binWidths.png"),width=11.5,height=8,units="in",res=300)
  par(oma=c(1,4,1,2))
  for (isp in 1:nSpecies) {
    if (isp == 1) {
      plot(cumSumBinWidths[isp,],rep(isp,nBins+1),type="b",xlim=c(min(cumSumBinWidths),max(cumSumBinWidths))
           ,ylim=c(0,nSpecies+1),xlab="length (cm)",ylab=NA,yaxt="n",xaxt="n")
      lines(cumSumBinWidths[isp,],rep(isp,nBins+1))
    } else {
      lines(cumSumBinWidths[isp,],rep(isp,nBins+1))
      points(cumSumBinWidths[isp,],rep(isp,nBins+1))
    }
  }
  axis(side=2, at=c(1:10),labels=speciesNames,las=1)
  axis(side=1, at=seq(0,140,20),labels=seq(0,140,20))
  graphics.off()

  ## length-weight relationship
  png(paste0(outPath,"/Hydra_lengthWeight.png"),width=8,height=11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(2,4,0,4)+.0,oma=c(3,1,1,1)+0.0)
  for (isp in 1:nSpecies) {
    lengthOfFish <- seq(0,cumSumBinWidths[isp,nBins+1],1)
    a <- lenwta[isp]
    b <- lenwtb[isp]
    plot(lengthOfFish,a*lengthOfFish^b,type="l",ylab="",xlab="")
    if ((isp %% 2) == 1) {mtext(side=2,line=2.5,"weight (g)",cex=cex)}
    if ((isp==nSpecies) | (isp ==(nSpecies-1))) {mtext(side=1,line=2.5,cex=cex,"length (cm)")}
    legend("topleft",legend=speciesNames[isp])
  }

  graphics.off()

  ## Y1N and biomass
  png(paste0(outPath,"/Hydra_Y1N_Biomass.png"),width=8,height=11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(2,4,0,4)+.0,oma=c(3,1,1,1)+0.0)

  for (isp in 1:nSpecies) {
    a <- lenwta[isp] # (lazy data)
    b <- lenwtb[isp] # (lazy data)

    plot(1:nBins,Y1N[isp,],type="b",ylab="",xlab="")
    if ((isp %% 2) == 1) {mtext(side=2,line=2,"# individuals (mil)",cex=cex)}
    if ((isp==nSpecies) | (isp ==(nSpecies-1))) {mtext(side=1,line=2,cex=cex,"size class")}
    par(new=T)
    # biomass part
    weightG <- a*midPoints[isp,]^b
    plot(1:nBins,weightConversion*Y1N[isp,]*weightG,axes=F,xlab=NA,ylab=NA,type="l",lty=2)
    axis(side=4)
    if ((isp %% 2) == 0) {mtext(side=4,line=2.2,cex=cex,"Biomass (g)")}
    legend("topleft",legend=c(speciesNames[isp], "Y1N","Biomass"),
    lty=c(0,1,2),pch=c(NA,NA,NA), col=c("black", "black","black"))
  }
  graphics.off()

}

# fishing selectivities
plot_Selectivities <- function(outPath) {
  # selectivity
  # requires selectivity_c, selectivity_d and size bins
  png(paste0(outPath,"/Hydra_Selectivities.png"),width=8,height=11.5,units="in",res=300)

  sel_c <- fisherySelectivityc  # lazy data # read.table(paste0(inPath,"fishing_selectivityc.csv"),sep=",",header=TRUE,row.names=1)
  sel_d <- fisherySelectivityd # lazy data   #  read.table(paste0(inPath,"fishing_selectivityd.csv"),sep=",",header=TRUE,row.names=1)
  bins <- binwidth # lazy data #read.table(paste0(inPath,"length_sizebins.csv"),sep=",",header=TRUE)
  bins <- bins[,1:ncol(bins)-1]
  nFleets <- ncol(sel_c)
  nSpecies <- nrow(bins)
  nClasses <- ncol(bins)
  selectivity <- matrix(list(data=NA),nSpecies,nFleets)
  selectivityMid <- matrix(list(data=NA),nSpecies,nFleets)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+.75,oma=c(3,1,1,1)+0.5)
  #colVec <- c("black","green","red","blue")
  colVec <- c("black","blue","red")
  lineTypeVec <- c(1,2,3,4)
  for (isp in 1:nSpecies) {
    binsInterval <- unlist(bins[isp,])
    cum <- c(0,cumsum(binsInterval))
    midBins <- cum[1:nClasses]+(diff(cum)/2)
    for (jfleet in 1:(nFleets-1)) {
      x <- seq(0,tail(cum,1),1)
      #print(1/(1 + exp(-sel_c[isp,jfleet]-sel_d[isp,jfleet]*midBins)))
      selectivity[[isp,jfleet]]  <- 1/(1 + exp(-sel_c[isp,jfleet]-sel_d[isp,jfleet]*x))
      selectivityMid[[isp,jfleet]]  <- 1/(1 + exp(-sel_c[isp,jfleet]-sel_d[isp,jfleet]*midBins))
      if (jfleet == 1) {
        plot(x,selectivity[[isp,jfleet]],type="l",lwd=2,ylab="selectivity",xlab="length(cm)",xlim=c(0,max(midBins)),ylim=c(0,1))  #main=rownames(sel_c)[isp]
        #ext(midBins[1],.95,rownames(sel_c)[isp],pos=4,cex=1.5)
        if (rownames(sel_c)[isp]=="goosefish") {legend("topleft",legend="monkfish",bty="n",cex=1.5)} else {
          legend("topleft",legend=rownames(sel_c)[isp],bty="n",cex=1.5)
          }
        } else {
          lines(x,selectivity[[isp,jfleet]],col=colVec[jfleet],lty=lineTypeVec[jfleet],lwd=2)
          points(midBins,selectivityMid[[isp,jfleet]],col=colVec[jfleet],pch=19)
        }
    }
    }
  mtext("Length (cm)",line = 1.9,outer=T, side=1,cex = 1.5)
  mtext("Selectivity",line = -.3,outer=T, side=2,cex = 1.5)
  # add text as a legend to plot in separate panel
  #plot(1, type="n", axes=FALSE, xlab="", ylab="")
  #legend(.5,1.5,legend=colnames(sel_c)[1:(nFleets-1)],fill=colVec,cex=1.8)
  graphics.off()
}

# effort by fleet
plot_FishingEffort <- function(outPath) {
  ##########################################################################3
  #plot effort
  obs_effort <- t(observedEffort) #lazy data # read.table(paste0(inPath,"observation_effort.csv"),sep=",",header=TRUE)
  nFleets <- dim(obs_effort)[2]-1
  fleetNames <- colnames(obs_effort)

  png(paste0(outPath,"/Hydra_EffortAnomoly.png"),width=8,height=11.5,units="in",res=300)
  par(mfrow=c(nFleets,1),mar=c(0,2,1,1)+0.75,oma=c(2,1,1,1)+0.5)
  for (ifleet in 1:nFleets) {
    #plot(obs_effort[,1],obs_effort[,ifleet+1],type="l",main=colnames(obs_effort)[ifleet+1],xlab="",ylab="Effort")
    anomoly <- (obs_effort[,ifleet+1] - mean(obs_effort[,ifleet+1]))/sd(obs_effort[,ifleet+1])

    if (any(is.nan(anomoly))) {
      # fllet not fully parameterized
      plot(0,0)
      next
    }

    plot(obs_effort[,1],anomoly,type="l", cex=1.6,lwd=2)
    abline(h=0)
    if (fleetNames[ifleet+1] =="benthic") {fleetNames[ifleet+1] <- "Demersal Trawl"}
    if (fleetNames[ifleet+1] =="pelagic") {fleetNames[ifleet+1] <- "Pelagic Trawl"}
    if (fleetNames[ifleet+1] =="longline") {fleetNames[ifleet+1] <- "Fixed Gear"}
    legend("topleft",legend=fleetNames[ifleet+1],bty="n",cex=1.5)
    }
  mtext("Anomoly",line=-.6,outer=T,side=2,cex=1.5)
  graphics.off()

  png(paste0(outPath,"/Hydra_Effort.png"),width=8,height=11.5,units="in",res=300)
  par(mfrow=c(nFleets,1),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+0.5)
  for (ifleet in 1:nFleets) {
    plot(obs_effort[,1],obs_effort[,ifleet+1],type="l",cex=1.6,lwd=2)
    legend("topleft",legend=fleetNames[ifleet+1],bty="n",cex=1.5)
    }
  mtext("Effort (Hours Fished)",line=-.6,outer=T,side=2,cex=1.5)

  graphics.off()


  ############################################################################
  # plot fishing _q's
  #fishing_q <- read.table(paste0(inPath,"fishing_q.csv"),sep=",",row.names=1,header=TRUE)
  fishing_q <- fisheryq # lazy data
  fleetNames <- colnames(fishing_q)
  nSpecies <- dim(fishing_q)[1]
  png(paste0(outPath,"/Hydra_FishingQ.png"),width=8,height=11.5,units="in",res=300)
  #png("HydraFishingQ.png",width=11.5,height=8,units="in",res=300)

  par(mfrow=c(nFleets,1),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+0.5)
  #par(mfrow=c(1,nFleets),mar=c(0,2,5,1)+0.75,oma=c(3,1,5,1)+0.5)

  for (ifleet in 1:nFleets) {
    #plot(fishing_q[,ifleet],ylab="q",xaxt="n",main=colnames(fishing_q)[ifleet])
    barplot(fishing_q[,ifleet],ylab="q",names.arg = rownames(fishing_q),cex.names = 1.1)
    #  legend("topleft",legend=colnames(fishing_q)[ifleet],bty="n",cex=1.7)
    if (fleetNames[ifleet] =="benthic") {fleetNames[ifleet] <- "Demersal Trawl"}
    if (fleetNames[ifleet] =="pelagic") {fleetNames[ifleet] <- "Pelagic Trawl"}
    if (fleetNames[ifleet] =="longline") {fleetNames[ifleet] <- "Fixed Gear"}
    legend("topleft",legend=fleetNames[ifleet],bty="n",cex=1.7)
    #axis(side=1, at=c(1:10), labels=rownames(fishing_q))
    }
  #mtext("Fishery Q",line=-.6,outer=T,side=2,cex=1.5)
  graphics.off()


  ###########################################################################
  # plot exploitation rates as a histogram for each fleet

  for (ifleet in 1:nFleets) {
    png(paste0(paste0(outPath,"/Hydra_ExploitationRate_"),names(fishing_q)[ifleet],".png"),width=8,height=11.5,units="in",res=300)

      par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+0.5)
      effort <- obs_effort[,ifleet+1]
      for (isp in 1:nSpecies) {
        hist(fishing_q[isp,ifleet]*effort,main="",breaks=10,xlab="Exploitation")
        if (rownames(fishing_q)[isp]=="goosefish") {legend("topleft",legend="monkfish",bty="n",cex=1.5)} else {
          legend("topright",legend=rownames(fishing_q)[isp],bty="n",cex=1.5)
        }
      }
      mtext("Exploitation",line = 1.7,outer=T, side=1,cex = 1.5)
      mtext("Frequency (of yrs)",line = -.5,outer=T, side=2,cex = 1.5)
      title(names(fishing_q)[ifleet],outer=TRUE,line=-1,cex.main=2)
      graphics.off()
  }
}

# species growth
plot_Growth <- function(outPath) {
  ##########################################################################3
  # plot growth curves
  #growth <- read.table(paste0(inPath,"growth_species.csv"),sep=",",row.names=1,header=TRUE)
  growthBins <- binwidth # lazy data read.table(paste0(inPath,"length_sizebins.csv"),sep=",",header=TRUE)
  speciesNames <- row.names(binwidth)
  nSpecies <- length(speciesNames)

  speciesNames[speciesNames=="goosefish"] <- "monkfish"

  png(paste0(outPath,"/Hydra_Growth.png"),width=8,height=11.5,units="in",res=300)
  par(mfrow=c(nSpecies/2,2),mar=c(0,2,1,1)+0.75,oma=c(3,1,1,1)+.5)

  growthFunctions <- c("na","exponential","na","von Bertalanffy")

  maxLinf <- max(growthLinf)
  t <- seq(0,40,1)
  for (isp in 1:nSpecies) {
    growthThroughInterval1 <- growthBins[isp,1]
    if(growthType[isp] == 2) {# exponential
      psi <- growthPsi[isp]
      kappa_r <- growthKappa[isp]
      growthFunc <- psi*t^kappa_r
      delta_t <- (growthThroughInterval1/psi)^(1/kappa_r)
      func <- growthFunctions[2]
    } else if (growthType[isp] == 4) {# von Bert
      k <- growthK[isp]
      linf <- growthLinf[isp]
      delta_t <- (1/k)*log(linf/(linf-growthThroughInterval1))
      growthFunc <- linf*(1-exp(-k*t))
      func <- growthFunctions[4]
    }
    plot(t,growthFunc,type="l",xlab="age",ylab="length",ylim=c(0,maxLinf))
    lines(rep(delta_t,2),c(0,linf),col="red",lty=2)
    legend("topleft",legend=speciesNames[isp],bty="n",cex=1.5)
    legend("bottomright",legend=func,bty="n",cex=1)

  }
  mtext("Age (yrs)",line = 1.7,outer=T, side=1,cex = 1.5)
  mtext("Length (cm)",line = -.5,outer=T, side=2,cex = 1.5)
  graphics.off()

}

# step/ramp for assessment module
plot_Assessment_Step <- function(outPath) {
  # plot assessment thresholds - step function
  png(paste0(outPath,"/Hydra_Assessment_Thresholds.png"),width=8,height= 11.5,units="in",res=300)

  #thresholds <- read.table(paste0(inPath,"assessmentThresholds.csv"),sep=",",row.names=1,header=TRUE)

  nExperiments <- dim(exploitationOptions)[2] # lazy data
  nSteps <- dim(thresholds)[1]
  par(mfrow=c(nExperiments/2,2),mar=c(4,4,2,1)+.5,oma=c(1,1,1,1)+.0)
  actionLevels <- thresholds #as.numeric(rownames(thresholds))
  actionLevels <- head(actionLevels,-1) # removes last value
  for (iExp in 1:nExperiments) {
    stepObj <- stepfun(actionLevels,exploitationOptions[,iExp])
    plot.stepfun(stepObj,ylim=c(0,max(exploitationOptions)),main="" ,ylab = "Exploitation Rate",xlab="B/B0")
    abline(exploitationOptions[2,iExp],0,col="red")
    legend("topleft",legend=paste0("Max rate = ",max(exploitationOptions[,iExp])))
  }

  graphics.off()

}
