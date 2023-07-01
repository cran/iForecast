rollingWindows <- function (x, estimation = "18m", by = "6m") 
{
  x = timeSeries::as.timeSeries(x)
  period = estimation
  by = by
  periodLength = as.numeric(substr(period, 1, nchar(period) - 
                                     1))
  periodUnit = substr(period, nchar(period), nchar(period))
  byLength = as.numeric(substr(by, 1, nchar(by) - 1))
  byUnit = substr(by, nchar(by), nchar(by))
  stopifnot(periodUnit %in% c("w", "m","d"))
  stopifnot(byUnit %in% c("w", "m","d"))
  stopifnot(periodUnit == byUnit)
  positions = time(x)
  if (periodUnit == "m") {
    startPositions = unique(timeDate::timeFirstDayInMonth(positions))
    endPositions = unique(timeDate::timeLastDayInMonth(positions))
  }
  else if (periodUnit == "w"){
    ID1 = ID2 = NULL
    for (i in unique(lubridate::year(x))) {
      ID1 = c(ID1, !duplicated(subset(lubridate::week(x), 
                                      lubridate::year(x) == i)))
      ID2 = c(ID2, !duplicated(subset(lubridate::week(x), 
                                      lubridate::year(x) == i), fromLast = TRUE))
    }
    startPositions = time(x)[ID1]
    endPositions = time(x)[ID2]
  } else {
    ID1 = ID2 = NULL
    for (i in unique(lubridate::year(x))) {
      ID1 = c(ID1, !duplicated(subset(lubridate::yday(x), 
                                      lubridate::year(x) == i)))
      ID2 = c(ID2, !duplicated(subset(lubridate::yday(x), 
                                      lubridate::year(x) == i), fromLast = TRUE))
      

    }
    startPositions = time(x)[ID1]
    endPositions = time(x)[ID2]
    
    
  }

  
  numberOfPositions = length(startPositions)
  startSeq <- seq(from = 1, to = (numberOfPositions - periodLength + 
                                    1), by = byLength)
  startDates = startPositions[startSeq]
  endSeq <- seq(from = periodLength, to = numberOfPositions, 
                by = byLength)
  endDates = endPositions[endSeq]
  windows = list(from = startDates, to = endDates)
  attr(windows, "control") = list(start = start(positions), 
                                  end = end(positions), period = period, by = by)
  windows
}



