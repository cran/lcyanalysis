#' w bottom technical analysis function
#' @description  w bottom technical analysis function is to analyze the reversal pattern conform to the rising trend of stock data
#' @details use RSI analysis of the strength of the stock market trend, analyze trends conform to w bottom, and RSI function need library 'TTR'
#' @usage w_bottom(h,top,down,month,day)
#' @param h an stock data
#' @param top an rsi rise horizon value
#' @param down an rsi down horizon value
#' @param month set the length between the start and end points. Unit:month
#' @param day check the correctness of the end point, set the length between the end and check points. Unit:day
#' @note the month value must be more than one month
#' @return an analysis of stock data for w bottom technical analysis indicators
#' @author Chun-Yu Liu <john401528@gmail.com>
#' @importFrom quantmod Cl Vo getSymbols
#' @importFrom TTR RSI
#' @importFrom stats approx
#' @importFrom xts reclass
#' @examples
#' \dontrun{
#' library(quantmod)
#' aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
#' w_bottom(aapl,60,40,2,20)
#' }
#' @export

#example to use
#if use 'quantmod' chartSeries:
#aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
#addTA(w_bottom(aapl,60,40,20),on=1,col='red')
#or
#if use 'highcharter' highchart:
#hc_add_series(w_bottom(aapl,60,40,3,20),type = "line",name = "W bottom", color = hex_to_rgba("red", 1))



w_bottom<-function(h,top,down,month,day){

  requireNamespace('quantmod')
  requireNamespace('TTR')
  requireNamespace('xts')
  requireNamespace('stats')


  df<-RSI(Cl(h))
  dd<-Cl(h)
  dc<-Cl(h)
  Vo<-Vo(h)

  da<-matrix(c(dd), nrow = nrow(dd), ncol = 1)
  colnames(da) <- c("close")

  ############# d0 volume
  datas<-1
  nrows <-nrow(Vo)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  d0<-array(x,c(nrows ,datas))

  while(x < nrows ){


    for(i in Vo[g,]){


      d0[n]<-i
      n<-n+1

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(Vo)){

      break

    }else{

      f<-f+1
    }

  }
  d0



  ############# d1 close price
  datas<-1
  nrows <-nrow(da)


  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  d1<-array(x,c(nrows ,datas))

  while(x < nrows ){


    for(i in da[g]){


      d1[n]<-i
      n<-n+1

      #######
      if(!is.na(df[g,])){
        if(df[g,]>top || df[g,]<down){

          dc[g]<-da[g]

        }else{

          dc[g,]<-0
        }
      }else{
        dc[g,]<-0
      }

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(da)){

      break

    }else{

      f<-f+1
    }

  }
  d1


  d_status<-matrix(c(NA), nrow = nrow(dc), ncol = 1)
  colnames(d_status) <- c("status")

  d_range<-matrix(c(NA), nrow = nrow(dc), ncol = 1)
  colnames(d_range) <- c("range")

  d_trend<-matrix(c(NA), nrow = nrow(dc), ncol = 1)
  colnames(d_trend) <- c("trend")

  d_num<-matrix(c(1:length(dc)), nrow = nrow(dc), ncol = 1)
  colnames(d_num) <- c("num")

  wlog<-matrix(c(NA), nrow = nrow(dc), ncol = 1)
  colnames(wlog) <- c("log")

  colnames(d0) <- c("Volume")

  d_closes<-matrix(c(da), nrow = nrow(da), ncol = 1)
  colnames(d_closes) <- c("close")


  da1<-cbind(d0,d_num)
  da2<-cbind(dc,df)


  ############# d2 close price2
  datas<-1
  nrows <-nrow(dc)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  d2<-array(x,c(nrows ,datas))

  while(x < nrows ){


    for(i in dc[g,]){


      d2[n]<-i
      n<-n+1

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
  d2


  ############# d3 Hi
  datas<-1
  nrows <-nrow(da)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2
  top1<-0
  top2<-0
  topx<-0
  topg<-0
  topmax<-0

  d3<-array(x,c(nrows ,datas))

  while(x < nrows ){

    for(i in da[g]){

      d3[n]<-i
      n<-n+1

      ###
      if(g>1){
        if(!is.na(df[g,])){
          if(df[g,]>top){
            if(d2[g-1]!=0 && d2[f]==0 && df[g-1]<top){
              d_status[g]<-"O"
              d_range[g]<-"Hi"
            }
            if(d2[g-1]==0 && d2[f]!=0 && df[f]<top){
              d_status[g]<-"O"
              d_range[g]<-"Hi"
            }
            if(d2[g-1]!=0 && d2[f]!=0 && df[g-1]<top && df[f]<top){
              d_status[g]<-"O"
              d_range[g]<-"Hi"
            }
            if(d2[g-1]==0 && d2[f]==0){
              d_status[g]<-"O"
              d_range[g]<-"Hi"

            }else{
              if(d2[g-1]==0 && df[f]>top){
                top1<-(g-1)
              }
              if(d2[g-1]!=0 && df[g-1]<down && df[f]>top){
                top1<-g
              }

              if(d2[f]==0 && df[g-1]>top){
                top2<-f
              }
              if(d2[f]!=0 && df[f]<down && df[g-1]>top){
                top2<-f
              }
              topmax<-max(d2[top1:top2])
              topx<-top1
              topg<-top1
              #####
              while(topx<top2){
                if(d2[topg]<topmax){
                  dc[topg]<-0
                }else{
                  dc[topg]<-da[topg]
                  d_status[topg]<-"O"
                  d_range[(top1+1):(top2-1)]<-"Hi"
                }

                topg<-topg+1
                topx<-topx+1

              }####while
              if(topg==top2){
                topg<-0
              }
              if(topx==top2){
                topx<-1
              }
            }###else
          }
        }
      }###

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(da)){

      break

    }else{

      f<-f+1
    }

  }

  d3
  da3<-data.frame(dc,da2,d_status,d_range,da1)



  ############# d4 Lo
  datas<-1
  nrows <-nrow(da)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2
  low1<-0
  low2<-0
  lowx<-0
  lowg<-0
  lowmin<-0

  d4<-array(x,c(nrows ,datas))

  while(x <= nrows ){


    for(i in da[g]){


      d4[n]<-i
      n<-n+1

      ###
      if(g>1 ){
        if(!is.na(df[g,])){
          if(df[g,] < down){
            if(d2[g-1]!=0 && df[g-1]>top && d2[f]==0){
              d_status[g]<-"X"
              d_range[g]<-"Lo"
            }
            if(d2[g-1]==0 && d2[f]!=0 && df[f]>down){
              d_status[g]<-"X"
              d_range[g]<-"Lo"
            }
            if(d2[g-1]!=0 && d2[f]!=0 && df[g-1]>down && df[f]>down){
              d_status[g]<-"X"
              d_range[g]<-"Lo"
            }
            if(d2[g-1]==0 && d2[f]==0){
              d_status[g]<-"X"
              d_range[g]<-"Lo"

            }else{
              if(d2[g-1]==0 && df[f]<down){
                low1<-g
              }
              if(d2[g-1]!=0 && df[g-1]>top && df[f]<down){
                low1<-g
              }

              if(d2[f]==0 && df[g-1]<down){
                low2<-g
              }
              if(d2[f]!=0 && df[f]>top && df[g-1]<down){
                #low2<-g
              }
              if(low1!=0 && low2!=0){
                lowmin<-min(d2[low1:low2])
                lowx<-low1
                lowg<-low1
                #####
                while(lowx<=low2){
                  if(d2[lowg]>lowmin){
                    dc[lowg]<-0
                  }else{
                    dc[lowg]<-da[lowg]
                    d_status[lowg]<-"X"
                    d_range[low1:low2]<-"Lo"
                  }

                  lowg<-lowg+1
                  lowx<-lowx+1

                }####while
                if(lowg==low2){
                  lowg<-0
                }
                if(lowx==low2){
                  lowx<-0
                }
              }else{
                break
              }
            }###else
          }
        }
      }###

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(da)){

      break

    }else{

      f<-f+1
    }

  }

  d4
  da4<-data.frame(dc,da2,d_status,d_range,da1)


  ############# d5
  datas<-1
  nrows <-nrow(dc)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  d5<-array(x,c(nrows ,datas))

  while(x < nrows ){


    for(i in dc[g,]){


      d5[n]<-i
      n<-n+1

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(dc)){

      break

    }else{

      f<-f+1
    }

  }
  d5



  ############# d6 Lo_trend
  datas<-1
  nrows <-nrow(da)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2
  lotr1<-0
  lotr2<-0
  lotrx<-0
  lotrg<-0
  lotrmax<-0
  rightx<-0
  rightg<-0
  leftx<-0
  leftg<-0
  wength<-0
  wengths<-0
  checkwx<-0
  checkwg<-0
  checkw<-0
  checkrg<-0
  checkrx<-0
  wcheckr<-1
  s<-0
  s2<-1
  t<-1
  t2<-1
  te<-0
  wrun<-0

  d6<-array(x,c(nrows ,datas))

  while(x <= nrows ){

    for(i in da[g]){

      d6[n]<-i
      n<-n+1

      ###
      if(g>1 ){
        if(d5[g,]!=0){

          ###lotr1
          if(s==0 && !is.na(d_status[g])){
            if(d_status[g,]=="X" && is.na(d_trend[g])){
              lotr1<-g
              wlog[lotr1]<-"Lo1"
              s<-1
              t<-0
              wrun<-1
              if(lotr2!=0 && t2==0){
                f<-lotr1+1
                t2<-1
                te<-lotr1+1
              }
            }else{
              s<-0
            }
          }



          ###lotr2
          if(t==0 && !is.na(d_status[f])){
            wength<-round(length(lotr1:f)/30)
            if(wength<month && is.na(d_trend[f])){
              if(d5[f,]!=0 && d_status[f,]=="X" && d5[lotr1]<=d5[f] && "O" %in% c(d_status[lotr1:f]) && lotr1<f){
                lotr2<-f
                wlog[lotr2]<-"Lo2"
                t<-1
              }else if(t==0 && lotr2!=0){
                t<-0
              }
            }else{
              t<-1
              t2<-0
              s<-0
            }
          }



          if(lotr1!=0 && lotr2!=0 && wength<month && "O" %in% c(d_status[(lotr1+1):(lotr2-1)]) && d5[lotr1]<=d5[lotr2] && lotr1<lotr2 && d_status[lotr1]=="X" && d_status[lotr2]=="X"){


            lotrmax<-max(d5[(lotr1+1):(lotr2-1)])

            ###check inverted v
            checkwx<-lotr2+1
            checkwg<-lotr2+1
            while(d_closes[checkwx]<=lotrmax){
              if((length((lotr2+1):checkwg)/30)>month){
                checkw<-0
                break
              }else{
                checkw<-1
              }

              if(checkwx!=nrow(da)){
                checkwx<-checkwx+1
                checkwg<-checkwg+1
              }else{
                checkw<-0
                break
              }
            }#### while check v



            if(checkw==1){

              ###inverted v point
              lotrg<-lotr1
              lotrx<-lotr1
              while(lotrx<lotr2){ ###while

                if(d5[lotrg]<lotrmax || d5[lotrg]==0){
                  dc[lotrg]<-NA
                  d_trend[lotrg]<-"^"
                }else{
                  d_trend[lotrg]<-"O"
                  dc[lotr1]<-da[lotr1]
                  dc[lotr2]<-da[lotr2]
                  d_trend[lotr1]<-"Lo1"
                  d_trend[lotr2]<-"Lo2"
                }


                ###right trend
                rightx<-lotr2+1
                rightg<-lotr2+1
                while(d_closes[rightx]<=lotrmax){

                  dc[rightg]<-NA
                  d_trend[rightg]<-"right"

                  rightx<-rightx+1
                  rightg<-rightg+1

                }###while right
                dc[rightx-1]<-da[rightx-1]


                ###check right trend
                if((rightx+day-1)<nrow(da)){
                  if((sum(df[(rightx:(rightx+day-1))])/day)<top){
                    d_trend[(lotr2):rightx]<-NA
                    d_trend[(lotr1):lotr2]<-NA
                    dc[rightx-1]<-d5[rightx-1]

                    checkrg<-lotr1
                    checkrx<-lotr1
                    while(checkrx<rightx){
                      if(!is.na(d_status[checkrg])){
                        dc[checkrg]<-da[checkrg]
                      }else{
                        dc[checkrg]<-0
                      }
                      checkrg<-checkrg+1
                      checkrx<-checkrx+1
                    }
                  }else{
                    wcheckr<-0
                  }
                }else{

                  d_trend[(lotr2):rightx]<-NA
                  d_trend[(lotr1):lotr2]<-NA
                  dc[rightx-1]<-d5[rightx-1]

                  checkrg<-lotr1
                  checkrx<-lotr1
                  while(checkrx<rightx){
                    if(!is.na(d_status[checkrg])){
                      dc[checkrg]<-da[checkrg]
                    }else{
                      dc[checkrg]<-0
                    }
                    checkrg<-checkrg+1
                    checkrx<-checkrx+1
                  }

                  break
                }


                ###left trend
                if(wcheckr==0){
                  leftx<-lotr1-1
                  leftg<-lotr1-1
                  while(d_closes[leftx]!=lotrmax){

                    dc[leftg]<-NA
                    d_trend[leftg]<-"left"

                    if(!is.na(d_status[leftg])){
                      if(d_status[leftg]=="O"){
                        dc[leftg]<-da[leftg]
                        break
                      }
                    }
                    if(d_closes[leftg]>lotrmax){
                      dc[leftg]<-d5[leftg]
                      d_trend[leftg]<-NA
                      dc[leftg+1]<-da[leftg+1]
                      break
                    }

                    leftg<-leftg-1
                    leftx<-leftx-1

                    if(leftx==0){
                      dc[leftg+1]<-da[leftg+1]
                      break
                    }

                  }###while left
                }###left if


                lotrg<-lotrg+1
                lotrx<-lotrx+1

              } ###while
              if(lotrx==lotr2 && wcheckr==0){
                s<-0
                g<-rightx
                lotrx<-0
                lotr1<-0
                t2<-0
                f<-0
                checkw<-0
                wrun<-0
                wcheckr<-1
              }else{
                s<-0
                f<-0
                t2<-0
              }

            }else{ ###checkw if
              s<-0
              #g<-g+1
              f<-0
              t2<-0
            }
          }### if

        }###if secend
      }###if first

    }### for

    if(s==0){
      g<-g+1
      #f<-0
    }

    if(g>NROW(da)){
      break
    }

    if(wrun==0){
      x<-x+1
      m<-m+1
    }

    if(f==NROW(da)){

      break

    }else{
      if(t==0){
        f<-f+1
      }
    }

  }

  d6
  da6<-data.frame(dc,da2,d_status,d_range,d_trend,da1,wlog)



  ############# d7
  datas<-1
  nrows <-nrow(da)

  x<-0
  g<-1
  m<-1
  n<-1
  f<-2

  d7<-array(x,c(nrows ,datas))

  while(x < nrows ){


    for(i in da[g]){


      d7[n]<-i
      n<-n+1

      ###delete d_trend=NA
      if(d5[g]!=0){
        if(is.na(d_trend[g])){
          dc[g]<-0
        }
      }

    }

    g<-g+1
    x<-x+1
    m<-m+1

    if(f==NROW(da)){

      break

    }else{

      f<-f+1
    }

  }
  d7
  da7<-data.frame(dc,da2,d_status,d_range,d_trend,da1)



  ############# d8
  datas<-1
  nrows <-nrow(dc)


  x<-0
  g<-1
  m<-1
  n<-1

  d8<-array(x,c(nrows ,datas))

  while(x < nrows ){

    for(i in dc[g,]){

      d8[n]<-i
      n<-n+1
    }

    g<-g+1
    x<-x+1
    #n<-1
    m<-m+1

  }

  d8


  nn <- NROW(h)

  d8 <- approx( d8, xout=1:nn )$y

  d8<-ifelse( d8==0, NA, d8 )

  cd<-reclass( d8, h[1:length(d8)] )

  return(cd)


}
