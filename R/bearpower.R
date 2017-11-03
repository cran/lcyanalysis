#' bear power technical analysis function
#' @description  bear power technical analysis function is to analyze the reversal pattern conform to the downward trend and bear market of stock data
#' @details use RSI analysis of the strength of the stock market trend, analyze trends conform to bear power, and RSI function need library 'TTR'
#' @usage bearpower(h,down)
#' @param h an stock data
#' @param down an rsi down horizon value
#' @return an analysis of stock data for bear power technical analysis indicators
#' @author Chun-Yu Liu <john401528@gmail.com>
#' @importFrom quantmod Cl getSymbols
#' @importFrom TTR RSI
#' @importFrom stats approx
#' @importFrom xts reclass
#' @examples
#' library(quantmod)
#' aapl<-getSymbols("AAPL",src="google",auto.assign=FALSE)
#' bearpower(aapl,40)
#' @export


#example to use
#if use 'quantmod' chartSeries:
#aapl<-getSymbols("AAPL",src="google",auto.assign=FALSE)
#addTA(bearpower(aapl,40),on=1,col='green')
#or
#if use 'highcharter' highchart:
#hc_add_series(bearpower(aapl,40),type = "line",name = "bear power", color = hex_to_rgba("green", 1))


bearpower<-function(h,down){

  requireNamespace('quantmod')
  requireNamespace('TTR')
  requireNamespace('xts')
  requireNamespace('stats')

  df<-RSI(Cl(h))
  da<-Cl(h)
  dc<-Cl(h)

  d_num<-matrix(c(1:length(dc)), nrow = nrow(dc), ncol = 1)
  colnames(d_num) <- c("num")

  ############# d1
  datas<-1
  nrows <-nrow(dc)

  d1<-0

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  while(x < nrows ){


    for(i in dc[g,]){


      d1[n]<-i
      n<-n+1

      #######
      if(!is.na(df[g,])){
        if(df[g,]<down){

          dc[g,]<-da[g,]

        }else{

          dc[g,]<-0
        }
      }else{

        dc[g,]<-0
      }


    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

    if(f==NROW(dc)){

      break

    }else{

      f<-f+1
    }

  }

  da2<-data.frame(dc,df,d_num)

  dc2<-matrix(c(dc), nrow = nrow(dc), ncol = 1)

  d_closes<-matrix(c(dc), nrow = nrow(dc), ncol = 1)


  ####status
  d_status<-matrix(c(0), nrow = nrow(dc), ncol = 1)
  colnames(d_status) <- c("status")

  d_buy<-matrix(c(0), nrow = nrow(dc), ncol = 1)


  ############ d_status
  datas<-1
  nrows <-nrow(dc)

  dstatus<-0

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  while(x < nrows ){


    for(i in dc[g,]){


      dstatus[n]<-i
      n<-n+1

      if(g>1){
        if(!is.na(df[g,])){
          if(df[g]<down){
            if((g+2)<NROW(dc)){
              if(dc[g-1]==0 && dc[g+1]!=0 && dc[g+2]!=0 && dc[g+3]!=0){
                d_status[g]<-"X"
                d_buy[g]<-'buy1'
              }
              if(dc[g+1]==0 && dc[g-1]!=0 && dc[g-2]!=0 && dc[g-3]!=0){
                d_status[g]<-"X"
                d_buy[g]<-'buy'
              }
              if(dc[(g-1),]==0 && dc[f,]==0){
                d_status[g]<-"X1"
              }
            }
          }
        }
      }

    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

    if(f==NROW(dc)){

      break

    }else{

      f<-f+1
    }

  }

  dstatus<-data.frame(dc,df,d_status,dc2,d_num)



  ############# d2
  datas<-1
  nrows <-nrow(dc)

  d2<-0

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  while(x < nrows ){


    for(i in dc[g,]){


      d2[n]<-i
      n<-n+1

      if(g>1){

        if(dc[(g-1),]==0 && dc[f,]==0){

          dc2[g,]<-0

        }else{
          if(!is.na(d_status[g])){
            if(dc[(g-1),]==0 && d_status[g]=="X" || dc[f,]==0 && d_status[g]=="X"){

              dc2[g,]<- dc[g,]

            }
          }
        }

      }
      if(dc[g,]!=0 && is.na(d_status[g])){

        dc2[g,]<- NA

      }

    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

    if(f==NROW(dc)){

      break

    }else{

      f<-f+1
    }

  }

  da3<-data.frame(dc,df,dc2,d_status,d_num)




  ############# d3
  datas<-1
  nrows <-nrow(dc)

  d3<-0

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2
  bea1<-0
  bea2<-0
  wength<-0

  while(x < nrows ){


    for(i in dc[g,]){


      d3[n]<-i
      n<-n+1


      if(g>1){
        if(is.na(d_status[g])){
          if(d_closes[(g-1),]==0 && is.na(d_status[g])){
            bea1<-g
          }
          if(d_closes[f,]==0 && is.na(d_status[g])){
            bea2<-g
          }
          wength<-length((bea1+1):(bea2-1))
          if(wength<3){
            dc2[bea1:bea2]<-0
          }
        }
      }


    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

    if(f==NROW(dc)){

      break

    }else{

      f<-f+1
    }

  }
  da4<-data.frame(dc,df,dc2,d_status,d_num)

  bear<-d_status


  nn <- NROW(dc)
  dc2 <- approx( dc2, xout=1:nn )$y

  dc2<-ifelse( dc2==0, NA, dc2 )

  cd<-reclass( dc2, h[1:length(dc2)] )

  return(cd)

}

