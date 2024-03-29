ttsLSTM <- function (y,x=NULL,train.end,arOrder=1,xregOrder=0,type,
                     memoryLoops=10,shape=NULL,dim3=5,batch.range=2:7,batch.size=NULL){
#  if (!is.zoo(y)) {print("The data had better be a zoo object.")}
if (is.null(batch.range) & is.null(batch.size) | !is.null(batch.range) & !is.null(batch.size)){
  print("Either batch.range or batch.size must be NULL.") }

  y=timeSeries::as.timeSeries(y)

  if (!is.null(x)) {
    x=timeSeries::as.timeSeries(x)

    if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")
    }

  }



  if (is.null(train.end)) {print("The train.end must be specified.") }

  train.start=start(y)
  t0=which(as.character(time(y))==train.end)
  test.start=as.character(time(y))[t0+1]
  test.end=as.character(end(y))

  p=max(arOrder,xregOrder)
  colNAMES=c(outer(paste0(names(x),"_L"),0:p,FUN=paste0))
  if (p==0) {
    y=y
    datasetX=timeSeries::as.timeSeries(x)
    ar0=NULL
  } else {
    datasetY=timeSeries::as.timeSeries(embed(y,p+1),time(y)[-c(1:p)])
    y=datasetY[,1]
    ar0=datasetY[,-1]
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
  trend <- 1:nrow(y)
  if (timeSeries::isRegular(y)) {
    seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
    DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}


  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}


  ## Input variables data
  newData= timeSeries::as.timeSeries(DF)

  trainData=window(newData,start=train.start,end=train.end)

  train0 = data.frame(value = as.numeric(trainData[,1]), trainData[,-1])
  train = train0[complete.cases(train0), ]

  ## Lines below determines batch.size that can be perfectly divided by nrow(train)

  if (is.null(batch.size)){batch.size = batch.range[which(mod(nrow(train),batch.range)==0)[1]]

  } else {batch.size =batch.size}

  if (is.na(batch.size)) {
    train=train[-c(1:batch.range[which.min(mod(nrow(train),batch.range))]),]

  }
  batch.size = batch.range[which(mod(nrow(train),batch.range)==0)[1]]

  ####################################
  ###=====LSTM modeling begins=====###
  ####################################

  train.new=as.matrix(train) #remove date index
  dimnames(train.new)=NULL

  if(is.null(shape)) {SHAPE=ncol(train.new)} else {SHAPE=shape}
  k=dim3
  x.train = array(data = train.new[,-1], dim = c(nrow(train.new), SHAPE, k))
  y.train = array(data = train.new[,1], dim = c(nrow(train.new), 1))


  model <- keras::keras_model_sequential()

if (max(diff(unique(y)))==min(diff(unique(y)))) {

    model %>%
    keras::layer_lstm(units = 128,
               input_shape = c(SHAPE, k),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_lstm(units = 128,
               return_sequences = FALSE,
               stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_dense(units=64, activation = "relu") %>%
    keras::layer_dense(units=32) %>%
    keras::layer_dense(units = 1, activation = "sigmoid")

  model %>%
    keras::compile(optimizer = "adam",
            loss = "binary_crossentropy",
            metrics = "binary_accuracy")

} else {
  model %>%
    keras::layer_lstm(units = 100,
                      input_shape = c(SHAPE, k),
                      batch_size = batch.size,
                      return_sequences = TRUE,
                      stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_lstm(units = 50,
                      return_sequences = FALSE,
                      stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_dense(units = 1)

  model %>%
    keras::compile(loss = 'mae', optimizer = 'adam')


}


  for(i in 1:memoryLoops){
    model %>% keras::fit(x = x.train,
                         y = y.train,
                         batch_size = batch.size,
                         epochs = 1,
                         verbose = 0,
                         shuffle = FALSE)
    model %>% keras::reset_states()}




  return(list(output=model,batch.size=batch.size,k=k,SHAPE=SHAPE,arOrder=arOrder,data=cbind(y,DF0),dataused=DF))
}
