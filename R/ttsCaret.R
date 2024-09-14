tts.caret <- function(y,x=NULL, method,train.end,arOrder=2,xregOrder=0,
                     type,tuneLength=10,preProcess = NULL,resampling="boot",
                     Number=NULL,Repeat=NULL) {

#  if (!is.zoo(y)) {print("Data had better be a zoo object.")}

  y=timeSeries::as.timeSeries(y)

if (!is.null(x)) {
  x=timeSeries::as.timeSeries(x)
    if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")}
}


if (is.null(train.end)) {print("train.end date must be specified.") }

if (is.null(type)) {type="none" }

  train.start=start(y)
  t0=which(as.character(time(y))==train.end)
  test.start=as.character(time(y))[t0+1]
  test.end=as.character(end(y))

p=max(arOrder,xregOrder)
  colNAMES=c(outer(paste0(names(x),"_L"),0:p,FUN=paste0))
if (p==0) {
  y=timeSeries::as.timeSeries(y)
  datasetX=timeSeries::as.timeSeries(x)
  ar0=NULL
} else {
  datasetY=timeSeries::as.timeSeries(embed(y,p+1),time(y)[-c(1:p)])
  y=datasetY[,1,drop=FALSE]
  ar0=datasetY[,-1,drop=FALSE]
  colnames(ar0)=paste0("ar",1:p)

  if (is.null(x)) {datasetX=NULL
  } else {
  datasetX=timeSeries::as.timeSeries(embed(x,p+1),time(x)[-c(1:p)])

  colnames(datasetX)=colNAMES
  }
}

  colnames(y)="y"


  if (min(arOrder)==0) {ar=NULL
      }  else {ar=ar0[,paste0("ar",arOrder)]}



  if (is.null(x)) {X=datasetX} else {
      L.ID=paste0("L",xregOrder)

    IDx=NULL
    for (i in L.ID) {IDx=c(IDx,grep(colNAMES,pattern=i))}
    X=datasetX[,IDx]
  }


  DF <- na.omit(cbind(y,ar,X))


  #4. Dummies for time features
  trend <- 1:nrow(DF)

  if (timeSeries::isRegular(y)) {
  seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
  DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}


  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}



  trainData0=window(DF,start=train.start,end=train.end)
  if (max(diff(unique(y)))==min(diff(unique(y)))) {
    trainData=as.data.frame(unclass(trainData0))
      if(is.double(trainData$y)) {
      trainData$y=as.factor(trainData$y)
      levels(trainData$y)=LETTERS[seq(length(levels(trainData$y)))]
      } else {trainData$y=as.factor(trainData$y)}

      } else {trainData=trainData0}

  dep=colnames(DF)[1]

  eq=as.formula(paste(dep,"~."))
  resampling=resampling
  if (method == "svm") {
    ### finding optimal value of a tuning parameter
    sigDist <- kernlab::sigest(eq,data = trainData, frac = 0.5)
    ### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
    svmTuneGrid <- data.frame(.sigma = rep(sigDist[2],10), .C = 2^(-2:7))
#    set.seed(1056)
        output <- caret::train(eq,data = trainData,
                          method = "svmRadial",
                          preProcess = preProcess,
                          tuneGrid = svmTuneGrid,
                          trControl = trainControl(
                          method = resampling,
                          number= if (is.null(Number)) {ifelse(resampling=="cv", 10, 25)} else {Number},
                          repeats= if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                          savePredictions = TRUE,
                          classProbs = ifelse(max(diff(unique(y)))==min(diff(unique(y))),TRUE,FALSE))
                          )

  } else if (method == "gbm") {
    gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                            n.trees = seq(50, 750, by = 50),
                            shrinkage = c(0.01, 0.1),
                           n.minobsinnode = 15)

    output <- caret::train(eq,data = trainData,
                           method = method,
                           preProcess = preProcess,
                           tuneGrid = gbmGrid,
                           verbose=FALSE,
                           trControl = trainControl(
                           method = resampling,
                           number= if (is.null(Number)) {ifelse(resampling=="cv", 10, 25)} else {Number},
                           repeats= if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                           savePredictions = TRUE,
                           classProbs = ifelse(max(diff(unique(y)))==min(diff(unique(y))),TRUE,FALSE)))
  } else {output <- caret::train(eq, data = trainData, method=method,
                          preProcess = preProcess,
						              trace=FALSE,
                          trControl = trainControl(
                          method = resampling,
                          number= if (is.null(Number)) {{ifelse(resampling=="cv", 10, 25)}} else {Number},
                          repeats=if (is.null(Repeat)) {ifelse(resampling=="repeatedcv", 1, NA)} else {Repeat},
                          savePredictions = TRUE,
                          classProbs =  ifelse(max(diff(unique(y)))==min(diff(unique(y))),TRUE,FALSE)),
                          tuneLength = ifelse(resampling== "none", 1, tuneLength)
                          )
  }

  trained.Pred=output$pred[order(output$pred$rowIndex),]

  return(list(output=output,arOrder=arOrder,data=cbind(y,DF0),dataused=DF,training.Pred=trained.Pred))

}


