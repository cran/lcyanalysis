#' down trend line down function
#' @description  Down trend line analysis of the down_function is to sort and analysis the stock data
#' @details down_function is to sort the stock data by the set number of days and to filter the data of rising trend
#' @usage down_function(h,day,num)
#' @param h an stock data
#' @param day the number of days to analysis the data
#' @param num select pivot calculation method
#'   1:  UP1<-(2*center)-Lo(h).
#'   2:  UP2<-center+(UP1-DOWN1).
#' @return an analysis of stock data for down function
#' @author Chun-Yu Liu <john401528@gmail.com>
#' @importFrom quantmod Cl Vo Lo Hi
#' @importFrom stats approx
#' @importFrom xts reclass xts
#' @examples
#' library(quantmod)
#' aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
#' down_function(aapl,20,1)
#' @export


down_function<-function(h,day,num){

  #############pivot
  center<-xts(rowSums(HLC(h))/3,order.by=index(h))
  UP1<-(2*center)-Lo(h)
  DOWN1<-(2*center)-Hi(h)
  UP2<-center+(UP1-DOWN1)
  DOWN2<-center-(UP1-DOWN1)

  if(num == 1){
    h<-UP1
  }else if(num == 2){
    h<-UP2
  }

  ###############
  nrow <- floor(length(h)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  y<-0

  d1<-array(x,c(nrow ,day))

  while(x <  floor(length(h)/day) ){

    for(i in h[k:j]){

      d1[m,n]<-i
      n<-n+1


    }


    k<-j+1

    s<-s+1
    y<-y+1

    j<-day*s


    x<-x+1
    n<-1
    m<-m+1


  }


  ############ head
  days<-1
  nrows <-nrow(d1)

  d2<-0

  x<-0
  g<-1
  m<-1
  n<-1

  while(x < nrows ){


    for(i in max(head(d1[g,],3))){

      d2[n]<-i

      n<-n+1


    }

    g<-g+1
    x<-x+1
    m<-m+1

  }

  d2
  #############

  ############# tail
  days<-1
  nrows <-nrow(d1)

  d3<-0

  x<-0
  g<-1
  m<-1
  n<-1

  while(x < nrows ){


    for(i in max(tail(d1[g,],3))){

      d3[n]<-i

      n<-n+1

    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

  }

  d3


  ################ d4
  nn <- NROW(h)
  zz <- rep(0, nn)

  nrow <- floor(length(zz)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  y<-0


  d4<-array(x,c(nrow ,day))

  while(x <  floor(length(zz)/day) ){

    for(i in zz[k:j]){

      d4[m,n]<-i
      n<-n+1

    }

    k<-j+1
    s<-s+1
    y<-y+1
    j<-day*s
    x<-x+1
    n<-1
    m<-m+1

  }
  d4

  d4 <- d4[,-c(1,10)]

  dc<-cbind(d2,d4,d3)



  ############# d5
  days<-1
  nrows <-nrow(dc)

  d5<-0

  x<-0
  g<-1
  m<-1
  n<-1

  while(x < nrows ){


    for(i in dc[g,]){


      d5[n]<-i
      n<-n+1


    }

    g<-g+1
    x<-x+1
    m<-m+1

  }

  d5


  d5<-ifelse( d5==0, NA, d5 )

  nnn <- NROW(d5)

  d5 <- approx( d5, xout=1:nn )$y


  cd<-reclass( d5, h[1:length(d5)] )

  return(cd)
}




