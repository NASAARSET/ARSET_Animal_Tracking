# Functions used in ARSET training: Integration of animal tracking and remote sensing

# Functions are adapted from: Hazen et al. 2021 "Where did they not go?
# Considerations for generating pseudo-absences for telemetry-based habitat
# models." Movement Ecology 9:5 https://doi.org/10.1186/s40462-021-00240-2

# If desired, you can download all functions from that paper from GitHub
# Navigate to github page: https://github.com/elhazen/PA-paper
# Then click Code --> Download ZIP

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

createbackgroundabsence<-function(tags, tagid=tagid, n.sim = 100){
  
  
  print(tagid)
  tag = tags[which(tags$id==tagid),c('long', 'lat','dTime')]
  
  out.alltags.csv = sprintf('%s/backgroundpts_sim_%s.csv', out.dir,tagid)
  
  # remove non-unique dates
  dupes = which(duplicated(tag$dTime))
  if (length(dupes)!=0){
    tag = tag[-dupes,]
  }
  
  # remove NAs
  tag = tag[!is.na(tag$lat),]
  tag.sp = tag
  tags.sp = tags
  
  
  # Create an object of class "ltraj" (from R-pkg adehabitatLT)
  tr = as.ltraj(cbind(tag$long, tag$lat), date=tag$dTime, id=tagid)
  tr1 = tr[[1]]
  
  head(tr1)
  #  x11()
  #  plot(tr)
  
  coordinates(tag.sp) <- ~long+lat 
  
  # tag.sp<-sf::as_Spatial(tag.sp) # If using spatial dataframes, use this line instead of coordinates(), above
  
  coordinates(tags.sp) <- ~long+lat 
  
  # tags.sp<-sf::as_Spatial(tags.sp) # If using spatial dataframes, use this line instead of coordinates(), above
  
  studyarea=bbox(tags.sp)
  
  for (i in 1:dim(tag)[1]){
    tagi<-tag[i,]
    coordinates(tagi) <- ~long+lat
    # tagi<-sf:::as_Spatial(tagi) # If using spatial dataframes, use this line instead of coordinates(), above
    
    sim <- spsample(tagi, n.sim, type="random", bb=studyarea)
    sim$dTime<-rep(tag$dTime[i],n.sim)
    sim$step<-rep(i,n.sim)
    sim$ID<-rep(tagid,n.sim)
    sim$iteration <- seq(1,n.sim)
    if (exists('sim.allbacktags')){
      sim.allbacktags = rbind(sim.allbacktags, sim)
    } else {
      sim.allbacktags = sim
    }
    
    write.csv(sim.allbacktags, out.alltags.csv, row.names=F)      #OLD (commented out 4ssm)
    
  }
  
  return(sim.allbacktags)
  
  
  
}

makedataset<-function(presence,absencedata){
  #numsims<-max(absencedata$iteration)
  
  datasample<-sample(absencedata$iteration)[1:length(unique(presence$tag))]
  
  presence$presabs<-1; absencedata$presabs<-0
  
  if(exists("spdata") && is.data.frame(get("spdata"))) rm(spdata)
  
  for (x in 1:length(unique(presence$tag))){
    inddata<-rbind(presence[presence$tag==unique(presence$tag)[x],],
                   absencedata[absencedata$tag==unique(presence$tag)[x] & 
                                 absencedata$iteration==datasample[x],])
    if(exists("spdata") && is.data.frame(get("spdata"))) 
      spdata<-rbind(spdata,inddata) else 
        spdata<-inddata
  }
  
  return(spdata)
}

BRTtransformDataFrame<-function(datafr){
  tempdata <- datafr #%>% filter(Species==Species)
  tempdata <- tempdata %>% dplyr::select(tag,Date,lat,long,
                                         presabs,
                                         BathymetryDepth,
                                         ln_chla,
                                         sst)
  tempdata$presabs<-unlist(tempdata$presabs)
  # tempdata$presabs<-factor(tempdata$presabs)
  tempdata$RN<-sample(1000000,size=dim(tempdata)[1])
  return(tempdata)
}

pseudoR2.BRT <- function(x){
  d2 <- 1-(x$self.statistics$resid.deviance/x$self.statistics$null.deviance)
  return(d2)
}

kfolds_eval_brt <- function(dataInput, gbm.x, gbm.y, lr, tc, bf,nt){
  DataInput <- dataInput 
  DataInput$Kset <- dismo::kfold(DataInput,10) #randomly allocate k groups
  Evaluations_kfold <- as.data.frame(matrix(data=0,nrow=10,ncol=4))
  colnames(Evaluations_kfold) <- c("k","AUC","TSS","TPR")
  counter=1
  for (k in 1:10){
    print(k)
    DataInput_train <- DataInput[DataInput$Kset!=k,]
    DataInput_test <- DataInput[DataInput$Kset==k,]
    DataInput.kfolds <- dismo::gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                         family="bernoulli", tree.complexity=tc,
                                         learning.rate = lr, bag.fraction = bf,n.trees = nt)
    preds <- gbm::predict.gbm(DataInput.kfolds, DataInput_test,
                              n.trees=nt, type="response")
    dev <- dismo::calc.deviance(obs=DataInput_test$presabs, pred=preds, calc.mean=TRUE)
    d <- cbind(DataInput_test$presabs, preds)
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    e <- dismo::evaluate(p=pres, a=abs)
    Evaluations_kfold[counter,1] <- k
    Evaluations_kfold[counter,2] <- e@auc
    Evaluations_kfold[counter,3] <- max(e@TPR + e@TNR-1)
    Evaluations_kfold[counter,4] <- mean(e@TPR)
    counter=counter+1 
  }
  return(Evaluations_kfold)
}

# Bhattacharyya's Coefficient ----
# Bhattacharyya's coefficient ranges from 0 to 1, with 0 being no overlap in
# distributions and 1 being perfect overlap.
# source("http://tguillerme.github.io/R/bhatt.coef.R" but link not working May 2025)

# Bhattacharyya Coefficient of two distributions
# Calculates the Bhattacharyya Coefficient (BC) between two distributions:
# BC is the sum of the square root of the multiplication of the elements in bin
# i for both distributions.
# It ranges from 0 (no overlap) to 1 (full overlap).
# v.0.1

# SYNTAX :

# <x>    a numeric vector of length >= 2.

# <y>    a numeric vector of length >= 2.

# <bw>   can be either a fixed value of bins or a function to calculate the
# bandwidth of each bin (see ?bw.nrd). Default is bw.nrd0.

# <...>  any optional arguments to be passed to the given bw function.

# Author: guillert(at)tcd.ie - 28/11/2014
# Requirements: R v 3

bhatt.coeff<-function(x,y, bw=bw.nrd0, ...) {
  #SANITIZING
  #x
  if(class(x) != 'numeric') {
    stop("'x' must be numeric.")
  }
  if(length(x) < 2) {
    stop("'x' need at least two data points.")
  }
  
  #y
  if(class(y) != 'numeric') {
    stop("'y' must be numeric.")
  }
  if(length(y) < 2) {
    stop("'y' need at least two data points.")
  }
  
  #bw
  if(length(bw) != 1) {
    stop("'bw' must be either a single numeric value or a single function.")   
  }
  if(class(bw) != 'function') {
    if(class(bw) != 'numeric') {
      stop("'bw' must be either a single numeric value or a single function.")   
    }
  }
  #Avoiding non-entire numbers
  if(class(bw) == 'numeric') {
    bw<-round(bw)
  }
  
  #BHATTACHARYYA COEFFICIENT
  #sum(sqrt(x relative counts in bin_i * y relative counts in bin_i))
  
  #Setting the right number of bins (i)
  if(class(bw) == 'function') {
    #Bin width
    band.width<-bw(c(x,y), ...)
    #Bin breaks
    #adding an extra bandwith to the max to be sure to include all the data
    bin.breaks<-seq(from=min(c(x,y)), to=max(c(x,y)+band.width), by=band.width) 
    #Number of bins
    bin.n<-length(bin.breaks)-1
  } else {
    #Bin breaks
    bin.breaks<-hist(c(x,y), breaks=bw, plot=F)$breaks
    #Bin width
    band.width<-diff(bin.breaks)[1]
    #Number of bins
    bin.n<-bw
  }
  
  #Counting the number of elements per bin
  histx<-hist(x, breaks=bin.breaks, plot=FALSE)[[2]]
  histy<-hist(y, breaks=bin.breaks, plot=FALSE)[[2]]
  #Relative counts
  rel.histx<-histx/sum(histx)
  rel.histy<-histy/sum(histy)
  
  #Calculating the Bhattacharyya Coefficient (sum of the square root of the
  #multiple of the relative counts of both distributions)
  bhatt.coeff<-sum(sqrt(rel.histx*rel.histy))
  return(bhatt.coeff)
  #End
}

bhattacharyya.stat<-function(data){
  ### test Bhattacharyya's coefficient ranges from 0 to 1, with 0 being no
  #overlap in distributions and 1 being perfect overlap.
  #source("http://tguillerme.github.io/R/bhatt.coef.R")
  # Bathymetric Depth
  data.pres<-(data$BathymetryDepth[data$presabs==1]); data.pres<-data.pres[!is.na(data.pres)]
  data.abs<-(data$BathymetryDepth[data$presabs==0]); data.abs<-data.abs[!is.na(data.abs)]
  bh.BathymetricDepth<-bhatt.coeff(data.pres,data.abs)
  # ln_chla
  data.pres<-(data$ln_chla[data$presabs==1]); data.pres<-data.pres[!is.na(data.pres)]
  data.abs<-(data$ln_chla[data$presabs==0]); data.abs<-data.abs[!is.na(data.abs)]
  bh.ln_chla<-bhatt.coeff(data.pres,data.abs)
  # SST
  data.pres<-(data$sst[data$presabs==1]); data.pres<-data.pres[!is.na(data.pres)]
  data.abs<-(data$sst[data$presabs==0]); data.abs<-data.abs[!is.na(data.abs)]
  bh.sst<-bhatt.coeff(data.pres,data.abs)

  #bhatt.coeff(BRTData[[i]]$dis_O2_sfc[BRTData[[i]]$presabs==1], BRTData[[i]]$dis_O2_sfc[BRTData[[i]]$presabs==0])
  #bhatt.coeff(BRTData[[i]]$MLD[BRTData[[i]]$presabs==1], BRTData[[i]]$MLD[BRTData[[i]]$presabs==0])
  #bhatt.coeff(BRTData[[i]]$SST[BRTData[[i]]$presabs==1], BRTData[[i]]$SST[BRTData[[i]]$presabs==0])
  
  outvect<-data.frame(BathymetricDepth=bh.BathymetricDepth,
                      ln_chla=bh.ln_chla,
                      sst=bh.sst)
  # names(outvect)<-c("bh.depth","bh.chla","bh.O2","bh.MLD",
  #                   "bh.SST","bh.uv_vel","bh.uv_heading360")
  return(outvect)
}
