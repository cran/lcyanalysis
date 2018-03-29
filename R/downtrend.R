#' downtrend technical analysis function
#' @description  Down trend line technical analysis function is to analyze the downward trend of stock data
#' @details use the down_function analysis data to analysis the downward trend line
#' @usage downtrend(h,day,num)
#' @param h an stock data
#' @param day the number of days to analysis the data
#' @param num select pivot calculation method
#'   1:  UP1<-(2*center)-Lo(h).
#'   2:  UP2<-center+(UP1-DOWN1).
#' @return an analysis of stock data for down trend technical analysis indicators
#' @author Chun-Yu Liu <john401528@gmail.com>
#' @importFrom quantmod Cl Vo Lo Hi HLC
#' @importFrom stats approx
#' @importFrom xts reclass xts
#' @importFrom utils head tail
#' @importFrom zoo index
#' @examples
#' library(quantmod)
#' aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
#' downtrend(aapl,20,1)
#' @export


#example to use
#if use 'quantmod' chartSeries:
#aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
#addTA(downtrend(aapl,20,2),on=1,col='green')
#or
#if use 'highcharter' highchart:
#hc_add_series(downtrend(aapl,20,2),type = "line",name = "V top", color = hex_to_rgba("green", 1))



downtrend<-function(h, day, num){


  down<-down_function(h,day,num)

  nn <- NROW(h)

  zz <- rep('a', nn)

  aa<-c(down)

  ############# d0
  nrow <- floor(length(down)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  y<-0
  g<-1

  d0<-array(x,c(nrow ,day))

  while(x <  floor(length(down)/day) ){



    for(i in down[k:j]){

      d0[m,n]<-i
      n<-n+1
    }


    k<-j+1

    s<-s+1
    y<-y+1

    j<-day*s

    x<-x+1
    n<-1
    m<-m+1
    g<-g+1

  }
  d0


  ############## d1
  nrow <- floor(length(down)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  y<-0
  g<-1
  f<-2
  u<-1

  d1<-array(x,c(nrow ,day))

  while(x <  floor(length(down)/day) ){



    for(i in down[k:j]){

      d1[m,n]<-i
      n<-n+1

      if(-0.9<(tail(d1[g,],1)-head(d1[g,],1))){

        aa[k:j]<-NA

      }else{
        if(tail(d1[g,],1)>head(d1[g,],1)){

          aa[k:j]<-NA

        }else{

          aa[k:j]<-down[k:j]
        }
      }

    }

    k<-j+1
    s<-s+1
    y<-y+1
    j<-day*s
    x<-x+1
    n<-1
    m<-m+1
    g<-g+1

    while(f < floor(length(down)/day) ){

      f<-f+1
    }

  }
  d1

  ####################


  ########################### d2
  nrow <- floor(length(aa)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  y<-0
  g<-1
  f<-2
  u<-1

  d2<-array(x,c(nrow ,day))

  while(x <  floor(length(aa)/day) ){



    for(i in aa[k:j]){

      d2[m,n]<-i
      n<-n+1
    }


    k<-j+1

    s<-s+1
    y<-y+1

    j<-day*s


    x<-x+1
    n<-1
    m<-m+1
    g<-g+1

    while(f < floor(length(aa)/day) ){

      f<-f+1
    }


  }

  d2
  ####################


  ########################### d3
  nrow <- floor(length(aa)/day)

  x<-0
  k<-1
  j<-day
  s<-1
  m<-1
  n<-1
  g<-1
  f<-2


  d3<-array(x,c(nrow ,day))


  while(x <  floor(length(aa)/day) ){



    for(i in aa[k:j]){

      d3[m,n]<-i
      n<-n+1

    }



    #######
    if(!is.na(aa[j])) {

      if(!is.na(d2[f])){

        aa[j]<-NA

      }else{

        aa[j]<-aa[j]
      }

    }


    k<-j+1

    s<-s+1


    j<-day*s


    x<-x+1
    n<-1
    m<-m+1
    g<-g+1

    if(f==NROW(d2)){

      break

    }else{

      f<-f+1
    }

  }

  d3
  ####################


  ############# d5
  days<-1
  nrows <-nrow(d1)

  d5<-0

  x<-0
  g<-1
  m<-1
  n<-1

  while(x < nrows ){


    for(i in d1[g,]){


      d5[n]<-i

      n<-n+1


    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

  }


  cd<-reclass( aa, h[1:length(aa)] )

  return(cd)

}


