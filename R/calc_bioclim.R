# calc_bioclim() script
#
#' Calculate bioClim variables from CRU data
#'
#'@description
#' @param monthly_df Tidy data frame output from get_cru function, MUST contain at least CRU variables
#' 'tmp', 'pre', perhaps 'tmn' and 'tmx', potentially 'dtr' if we subtitute these values from the dataset
#' instead of calculating them from 'tmp'.
#'
#' @returns
#' @export
#'
#' @examples


# set path and filename
ncpath <- "/Users/ndabagia/Desktop/Programming/data/"
ncname <- "cru_ts4.09.1901.1910.pre.dat"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "pre"  # arg? # note: tmp means temperature (not temporary)
# read in and print the data
ncin <- nc_open(ncfname)
print(ncin)
# # get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
# get time
time <- ncvar_get(ncin,"time")
time
# get time units
tunits <- ncatt_get(ncin,"time","units")
tunits
nt <- dim(time)
nt
# get precip
pre_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(pre_array)
# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")
nc_close(ncin) # close nc connection
#####
# decode time
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf
timestamps <- as_timestamp(cf) # get character-string times
timestamps # characters, but we want actual dates
# do that here
time_cf <- parse_timestamps(cf, timestamps) # parse the string into date components
time_cf

# # replace netCDF fill values with NA's
pre_array[pre_array==fillvalue$value] <- NA
# check that it worked
# head(as.vector(pre_array[,,1])) and it did
length(na.omit(as.vector(pre_array[,,1])))

####################
# do the whole thing, i.e. reshape the whole matrix into a 2x2 df

pre_vec_long <- as.vector(pre_array)
# reshape the vector into a matrix
pre_mat <- matrix(pre_vec_long, nrow=nlon*nlat, ncol=nt)
dim(pre_mat)
head(na.omit(pre_mat))

# create a dataframe
lonlat <- as.matrix(expand.grid(lon, lat))
pre_df02 <- data.frame(cbind(lonlat, pre_mat))

# dynamically generate column names from time_cf year and month columns
month_abbrevs <- c("Jan","Feb","Mar","Apr","May","Jun",
                   "Jul","Aug","Sep","Oct","Nov","Dec")

time_labels <- paste0(month_abbrevs[time_cf$month], time_cf$year)
names(pre_df02) <- c("lon", "lat", time_labels)

head(na.omit(pre_df02), 20)

# pivot long
pre_df_long <- pre_df02 |>
  pivot_longer(
    cols      = -c(lon, lat),
    names_to  = "time",
    values_to = "pre"
  ) |>
  select(lon, lat, time, pre)

head(na.omit(pre_df_long), 20)

pre_df_long
calc_bioclim <- function(x){
  x <- x
}

###################################
########## Calc BioClim Vars ######
###################################


##Create Worldclim variables
calc_bioclim<-function(monthly_df){
  ##BIO1 = Annual Mean Temperature
  Bio1<-ddply(monthly_df$tmp, .(year), summarize,  Tmp=mean(extract))

  ##Bio2 Mean Diurnal Range (Mean of monthly (max temp - min temp))
  # can we do this directly with the CRU "dtr" variable?
  # dtr = diurnal temp range
  Bio2_DF<-cbind.data.frame(monthly_df$tmax[,c(1,2)], extract=monthly_df$tmax$extract - monthly_df$tmin$extract)
  Bio2<-ddply(Bio2_DF , .(year), summarize,  Temp=mean(extract))

  ##Bio5

  #BIO5 = Max Temperature of Warmest Month
  Bio5<-ddply(monthly_df$tmax, .(year), summarize,  Temp=max(extract))

  ##Bio6 #BIO6 = Min Temperature of Coldest Month
  Bio6<-ddply(monthly_df$tmin, .(year), summarize,  Temp=min(extract))

  ##Bio7   #BIO7 = Temperature Annual Range (BIO5-BIO6)
  Bio7<-cbind.data.frame(Year=Bio5$year, Temp= Bio5$Temp-Bio6$Temp)

  #BIO3 = Isothermality (BIO2/BIO7) (×100)
  Bio3<-cbind.data.frame(Year= Bio2$year,Isothermaly=(Bio2$Temp/Bio7$Temp)*100 )

  ##Bio4 #BIO4 = Temperature Seasonality (standard deviation ×100)
  Bio4<-ddply(monthly_df$tmp, .(year), summarize,  Temp=sd(extract))

  ##Bio4a
  Bio4a<-(ddply(monthly_df$tmp, .(year), summarize,  Temp=sd(extract))/(Bio1$Temp+273.15))*100

  ##Function to get wettest, driest, warmest, coldest quarters
  get_est_quarter<-function(x, type){
    years<-levels(factor(x$year))

    if(type=='Wettest' | type=='Warmest'){
      df_qt_year<-list()
      for(i in 1:length(years)){
        sb<-x[x$year == years[i],]
        quarter<-matrix(ncol = 2, nrow=12)
        for(m in 1:10){
          quarter[m,2]<-sum(sb[seq(m,m+2),3])
          quarter[m,1]<-paste(seq(m,m+2), collapse = ',')
        }
        quarter[11,2]<-sum(sb[c(11,12,1),3])
        quarter[12,2]<-sum(sb[c(12,1,2),3])
        quarter[c(11,12),1]<-c(paste(c(11,12,1), collapse = ','),paste(c(12,1,2), collapse = ','))

        val<-quarter[which.max(as.numeric(as.character(quarter[,2]))),1]
        df_qt_year[[i]]<-cbind.data.frame(Year= years[i],Quarter=val)
      }
      quarters<-do.call('rbind.data.frame',df_qt_year)
      return(quarters)
    }else{
      if(type=='Driest' | type=='Coldest'){
        df_qt_year<-list()
        for(i in 1:length(years)){
          sb<-x[x$year == years[i],]
          quarter<-matrix(ncol = 2, nrow=12)
          for(m in 1:10){
            quarter[m,2]<-sum(sb[seq(m,m+2),3])
            quarter[m,1]<-paste(seq(m,m+2), collapse = ',')
          }
          quarter[11,2]<-sum(sb[c(11,12,1),3])
          quarter[12,2]<-sum(sb[c(12,1,2),3])
          quarter[c(11,12),1]<-c(paste(c(11,12,1), collapse = ','),paste(c(12,1,2), collapse = ','))

          val<-quarter[which.min(as.numeric(as.character(quarter[,2]))),1]
          df_qt_year[[i]]<-cbind.data.frame(Year= years[i],Quarter=val)
        }
        quarters<-do.call('rbind.data.frame',df_qt_year)
        return(quarters)
      }else{}

    }
  }

  ##Bio8 #BIO8 = Mean Temperature of Wettest Quarter

  Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Wettest')
  bio8_function<-function(site.avg,WQ){
    years<-levels(factor(site.avg$year))

    bio8_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.avg[site.avg$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio8_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio8_df[,1]<-years
    bio8_df<-as.data.frame(bio8_df)
    names(bio8_df)<-c('Year', 'Bio8')
    return(bio8_df)
  }
  Bio8<-bio8_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )

  #BIO9 = Mean Temperature of Driest Quarter
  Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Driest')
  bio9_function<-function(site.avg,WQ){
    years<-levels(factor(site.avg$year))

    bio9_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.avg[site.avg$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio9_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio9_df[,1]<-years
    bio9_df<-as.data.frame(bio9_df)
    names(bio9_df)<-c('Year', 'Bio9')
    return(bio9_df)
  }
  Bio9<-bio9_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )

  #BIO10 = Mean Temperature of Warmest Quarter
  Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Warmest')
  bio10_function<-function(site.avg,WQ){
    years<-levels(factor(site.avg$year))

    bio10_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.avg[site.avg$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio10_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio10_df[,1]<-years
    bio10_df<-as.data.frame(bio10_df)
    names(bio10_df)<-c('Year', 'Bio10')
    return(bio10_df)
  }
  Bio10<-bio10_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )

  #BIO11 = Mean Temperature of Coldest Quarter
  Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Coldest')
  bio11_function<-function(site.avg,WQ){
    years<-levels(factor(site.avg$year))

    bio11_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.avg[site.avg$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio11_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio11_df[,1]<-years
    bio11_df<-as.data.frame(bio11_df)
    names(bio11_df)<-c('Year', 'Bio11')
    return(bio11_df)
  }
  Bio11<-bio11_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )

  ##Bio12#BIO12 = Annual Precipitation

  Bio12<-summaryBy(extract~year, data=monthly_df$pre, FUN=c(sum))

  ##Bio13   #BIO13 = Precipitation of Wettest Month

  Bio13<-ddply(monthly_df$pre, .(year), summarize,  Temp=max(extract))

  ##Bio14
  #BIO14 = Precipitation of Driest Month
  Bio14<-ddply(monthly_df$pre, .(year), summarize,  Temp=min(extract))

  ##Bio15   #BIO15 = Precipitation Seasonality (Coefficient of Variation)
  Bio15<-cbind.data.frame(Year= unique(monthly_df$pre$year), Bio15=(ddply(monthly_df$pre, .(year), summarize,
                                                                           Temp=sd(extract))[,2]/(1+(summaryBy(extract~year, data=monthly_df$pre, FUN=c(sum))[,2])/12))*100)

  ##Bio16 #BIO16 = Precipitation of Wettest Quarter

  Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Wettest')
  bio16_function<-function(site.pre,WQ){
    years<-levels(factor(site.pre$year))

    bio16_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.pre[site.pre$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio16_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio16_df[,1]<-years
    bio16_df<-as.data.frame(bio16_df)
    names(bio16_df)<-c('Year', 'Bio16')
    return(bio16_df)
  }
  Bio16<-bio16_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )

  ##Bio17
  #BIO17 = Precipitation of Driest Quarter
  Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Driest')
  bio17_function<-function(site.pre,WQ){
    years<-levels(factor(site.pre$year))

    bio17_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.pre[site.pre$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio17_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio17_df[,1]<-years
    bio17_df<-as.data.frame(bio17_df)
    names(bio17_df)<-c('Year', 'Bio17')
    return(bio17_df)
  }
  Bio17<-bio17_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )

  ##Bio18
  #BIO18 = Precipitation of Warmest Quarter

  Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Warmest')
  bio18_function<-function(site.pre,WQ){
    years<-levels(factor(site.pre$year))

    bio18_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.pre[site.pre$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio18_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio18_df[,1]<-years
    bio18_df<-as.data.frame(bio18_df)
    names(bio18_df)<-c('Year', 'Bio18')
    return(bio18_df)
  }
  Bio18<-bio18_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )

  #BIO19 = Precipitation of Coldest Quarter
  Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Coldest')
  bio19_function<-function(site.pre,WQ){
    years<-levels(factor(site.pre$year))

    bio19_df<-matrix(ncol=2, nrow=length(years))
    for(i in 1:length(years)){
      df_avgs<-site.pre[site.pre$year == years[i],]
      qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
      bio19_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
    }
    bio19_df[,1]<-years
    bio19_df<-as.data.frame(bio19_df)
    names(bio19_df)<-c('Year', 'Bio19')
    return(bio19_df)
  }
  Bio19<-bio19_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )


  Bioclim<-list(Bio1,Bio2,Bio3,Bio4,Bio4a,Bio5,Bio6,Bio7,Bio8,Bio9,Bio10,Bio11,Bio12,Bio13,Bio14,Bio15,Bio16,Bio17,Bio18)
  names(Bioclim)<-c('Bio1','Bio2','Bio3','Bio4','Bio4a','Bio5','Bio6','Bio7','Bio8','Bio9','Bio10','Bio11','Bio12','Bio13','Bio14','Bio15','Bio16','Bio17','Bio18')

  return(Bioclim)
}
WCV<-CRU_to_WorldClim(monthly_df=Climate_site)
# run document() or command + shift + D to check that the documentation is working properly
# remember that you need values for @param, @returns, @export, @examples
