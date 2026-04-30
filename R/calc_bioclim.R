#'@description calculates the 19 bioClim variables from monthly climate data, either a dataframe or
#'SpatRasters.
#' @param data Tidy data frame output from get_cru function, MUST contain at least CRU variables
#' 'pre', tmn' and 'tmx'. User may also provide similar data in SpatRaster format.
#' @param start_year Year, contained in the data, that the user wants to start calculating
#' bioclimatic variables from.
#' @param end_year Year where the user wants the calculation to stop.
#' @returns 19 bioclimatic variables.
#' @value
#' Same class as input, but 19 values/variables
#' bio1 = Mean annual temperature
#' bio2 = Mean diurnal range (mean of max temp - min temp)
#' bio3 = Isothermality (bio2/bio7) (* 100)
#' bio4 = Temperature seasonality (standard deviation *100)
#' bio5 = Max temperature of warmest month
#' bio6 = Min temperature of coldest month
#' bio7 = Temperature annual range (bio5-bio6)
#' bio8 = Mean temperature of the wettest quarter
#' bio9 = Mean temperature of driest quarter
#' bio10 = Mean temperature of warmest quarter
#' bio11 = Mean temperature of coldest quarter
#' bio12 = Total (annual) precipitation
#' bio13 = Precipitation of wettest month
#' bio14 = Precipitation of driest month
#' bio15 = Precipitation seasonality (coefficient of variation)
#' bio16 = Precipitation of wettest quarter
#' bio17 = Precipitation of driest quarter
#' bio18 = Precipitation of warmest quarter
#' bio19 = Precipitation of the colder quarter
#' @examples
#' tmin <- c(10,12,14,16,18,20,22,21,19,17,15,12)
#' tmax <- tmin + 5
#' prec <- c(0,2,10,30,80,160,80,20,40,60,20,0)
#' biovars(prec, tmin, tmax)
#' tmn <- tmx <- prc <- rast(nrow=1, ncol=1, nlyr=12)
#' values(tmn) <- t(matrix(c(10,12,14,16,18,20,22,21,19,17,15,12)))
#' tmx <- tmn + 5
#' values(prc) <- t(matrix(c(0,2,10,30,80,160,80,20,40,60,20,0)))
#' b <- biovars(prc, tmn, tmx)
#' as.matrix(b)

###################################
########## Calc BioClim Vars ######
###################################
calc_bioclim <- function(data, start_year, end_year){

  # Extract year from the "time" column explicit in the get_cru output, ex: Jan2005
  year_index <- as.integer(substr(data$time, 4, 7))

  # --- Input validation ---

  if(start_year > end_year){
    stop("start_year must be less than end_year.")
  }

  data_start <- min(year_index)
  data_end   <- max(year_index)

  if(start_year < data_start | start_year > data_end){
    stop(paste("start_year", start_year, "is outside the range of the data (", data_start, "to", data_end, ")."))
  }

  if(end_year < data_start | end_year > data_end){
    stop(paste("end_year", end_year, "is outside the range of the data (", data_start, "to", data_end, ")."))
  }

  # --- Filter and summarize ---

  year_mask <- year_index >= start_year & year_index <= end_year

  prec <- data$pre[year_mask]
  tmin <- data$tmn[year_mask]
  tmax <- data$tmx[year_mask]

  if(length(prec) > 12 | length(tmin) > 12 | length(tmax) > 12){
    month_index <- ((seq_along(prec) - 1) %% 12) + 1

    prec <- tapply(prec, month_index, mean, na.rm = TRUE)
    tmin <- tapply(tmin, month_index, mean, na.rm = TRUE)
    tmax <- tapply(tmax, month_index, mean, na.rm = TRUE)
  }

  bioclims <- biovars(prec = prec, tmin = tmin, tmax = tmax)
  return(bioclims)
}

# #########
# # testing
# #########
#
# library(testthat)
#
# # --- Helper: generate synthetic monthly data ---
# make_data <- function(start_year, end_year){
#   n_months <- (end_year - start_year + 1) * 12
#   month_abbrevs <- c("Jan","Feb","Mar","Apr","May","Jun",
#                      "Jul","Aug","Sep","Oct","Nov","Dec")
#   years  <- rep(start_year:end_year, each = 12)
#   months <- rep(month_abbrevs, times = end_year - start_year + 1)
#    tibble(
#     time = paste0(months, years),
#     pre  = runif(n_months, min = 0,   max = 200),
#     tmn  = runif(n_months, min = -10, max = 10),
#     tmx  = runif(n_months, min = 10,  max = 40)
#   )
# }
#
# # --- Tests ---
#
# test_that("function runs successfully with a single year of data", {
#   data <- make_data(2000, 2000)
#   result <- calc_bioclim(data, start_year = 2000, end_year = 2000)
#   expect_false(is.null(result))
# })
#
# test_that("function runs successfully with multiple years of data", {
#   data <- make_data(2000, 2010)
#   result <- calc_bioclim(data, start_year = 2000, end_year = 2010)
#   expect_false(is.null(result))
# })
#
# test_that("function runs successfully when subsetting years within the data", {
#   data <- make_data(2000, 2010)
#   result <- calc_bioclim(data, start_year = 2003, end_year = 2007)
#   expect_false(is.null(result))
# })
#
# test_that("stop if start_year is greater than end_year", {
#   data <- make_data(2000, 2010)
#   expect_error(
#     calc_bioclim(data, start_year = 2010, end_year = 2000),
#     "start_year must be less than end_year"
#   )
# })
#
# test_that("stop if start_year is outside the data range", {
#   data <- make_data(2000, 2010)
#   expect_error(
#     calc_bioclim(data, start_year = 1990, end_year = 2005),
#     "start_year 1990 is outside the range of the data"
#   )
# })
#
# test_that("stop if end_year is outside the data range", {
#   data <- make_data(2000, 2010)
#   expect_error(
#     calc_bioclim(data, start_year = 2000, end_year = 2020),
#     "end_year 2020 is outside the range of the data"
#   )
# })
#################################################
###### Cristian's code to do the same as biovars()
#################################################

# ##Create Worldclim variables
# calc_bioclim<-function(monthly_df){
#   ##BIO1 = Annual Mean Temperature
#   Bio1<-ddply(monthly_df$tmp, .(year), summarize,  Tmp=mean(extract))
#
#   ##Bio2 Mean Diurnal Range (Mean of monthly (max temp - min temp))
#   # can we do this directly with the CRU "dtr" variable?
#   # dtr = diurnal temp range
#   Bio2_DF<-cbind.data.frame(monthly_df$tmax[,c(1,2)], extract=monthly_df$tmax$extract - monthly_df$tmin$extract)
#   Bio2<-ddply(Bio2_DF , .(year), summarize,  Temp=mean(extract))
#
#   ##Bio5
#
#   #BIO5 = Max Temperature of Warmest Month
#   Bio5<-ddply(monthly_df$tmax, .(year), summarize,  Temp=max(extract))
#
#   ##Bio6 #BIO6 = Min Temperature of Coldest Month
#   Bio6<-ddply(monthly_df$tmin, .(year), summarize,  Temp=min(extract))
#
#   ##Bio7   #BIO7 = Temperature Annual Range (BIO5-BIO6)
#   Bio7<-cbind.data.frame(Year=Bio5$year, Temp= Bio5$Temp-Bio6$Temp)
#
#   #BIO3 = Isothermality (BIO2/BIO7) (×100)
#   Bio3<-cbind.data.frame(Year= Bio2$year,Isothermaly=(Bio2$Temp/Bio7$Temp)*100 )
#
#   ##Bio4 #BIO4 = Temperature Seasonality (standard deviation ×100)
#   Bio4<-ddply(monthly_df$tmp, .(year), summarize,  Temp=sd(extract))
#
#   ##Bio4a
#   Bio4a<-(ddply(monthly_df$tmp, .(year), summarize,  Temp=sd(extract))/(Bio1$Temp+273.15))*100
#
#   ##Function to get wettest, driest, warmest, coldest quarters
#   get_est_quarter<-function(x, type){
#     years<-levels(factor(x$year))
#
#     if(type=='Wettest' | type=='Warmest'){
#       df_qt_year<-list()
#       for(i in 1:length(years)){
#         sb<-x[x$year == years[i],]
#         quarter<-matrix(ncol = 2, nrow=12)
#         for(m in 1:10){
#           quarter[m,2]<-sum(sb[seq(m,m+2),3])
#           quarter[m,1]<-paste(seq(m,m+2), collapse = ',')
#         }
#         quarter[11,2]<-sum(sb[c(11,12,1),3])
#         quarter[12,2]<-sum(sb[c(12,1,2),3])
#         quarter[c(11,12),1]<-c(paste(c(11,12,1), collapse = ','),paste(c(12,1,2), collapse = ','))
#
#         val<-quarter[which.max(as.numeric(as.character(quarter[,2]))),1]
#         df_qt_year[[i]]<-cbind.data.frame(Year= years[i],Quarter=val)
#       }
#       quarters<-do.call('rbind.data.frame',df_qt_year)
#       return(quarters)
#     }else{
#       if(type=='Driest' | type=='Coldest'){
#         df_qt_year<-list()
#         for(i in 1:length(years)){
#           sb<-x[x$year == years[i],]
#           quarter<-matrix(ncol = 2, nrow=12)
#           for(m in 1:10){
#             quarter[m,2]<-sum(sb[seq(m,m+2),3])
#             quarter[m,1]<-paste(seq(m,m+2), collapse = ',')
#           }
#           quarter[11,2]<-sum(sb[c(11,12,1),3])
#           quarter[12,2]<-sum(sb[c(12,1,2),3])
#           quarter[c(11,12),1]<-c(paste(c(11,12,1), collapse = ','),paste(c(12,1,2), collapse = ','))
#
#           val<-quarter[which.min(as.numeric(as.character(quarter[,2]))),1]
#           df_qt_year[[i]]<-cbind.data.frame(Year= years[i],Quarter=val)
#         }
#         quarters<-do.call('rbind.data.frame',df_qt_year)
#         return(quarters)
#       }else{}
#
#     }
#   }
#
#   ##Bio8 #BIO8 = Mean Temperature of Wettest Quarter
#
#   Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Wettest')
#   bio8_function<-function(site.avg,WQ){
#     years<-levels(factor(site.avg$year))
#
#     bio8_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.avg[site.avg$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio8_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio8_df[,1]<-years
#     bio8_df<-as.data.frame(bio8_df)
#     names(bio8_df)<-c('Year', 'Bio8')
#     return(bio8_df)
#   }
#   Bio8<-bio8_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )
#
#   #BIO9 = Mean Temperature of Driest Quarter
#   Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Driest')
#   bio9_function<-function(site.avg,WQ){
#     years<-levels(factor(site.avg$year))
#
#     bio9_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.avg[site.avg$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio9_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio9_df[,1]<-years
#     bio9_df<-as.data.frame(bio9_df)
#     names(bio9_df)<-c('Year', 'Bio9')
#     return(bio9_df)
#   }
#   Bio9<-bio9_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )
#
#   #BIO10 = Mean Temperature of Warmest Quarter
#   Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Warmest')
#   bio10_function<-function(site.avg,WQ){
#     years<-levels(factor(site.avg$year))
#
#     bio10_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.avg[site.avg$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio10_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio10_df[,1]<-years
#     bio10_df<-as.data.frame(bio10_df)
#     names(bio10_df)<-c('Year', 'Bio10')
#     return(bio10_df)
#   }
#   Bio10<-bio10_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )
#
#   #BIO11 = Mean Temperature of Coldest Quarter
#   Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Coldest')
#   bio11_function<-function(site.avg,WQ){
#     years<-levels(factor(site.avg$year))
#
#     bio11_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.avg[site.avg$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio11_df[i,2] <-   mean(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio11_df[,1]<-years
#     bio11_df<-as.data.frame(bio11_df)
#     names(bio11_df)<-c('Year', 'Bio11')
#     return(bio11_df)
#   }
#   Bio11<-bio11_function(site.avg = monthly_df$temp.avg, WQ=Yearly_WQ )
#
#   ##Bio12#BIO12 = Annual Precipitation
#
#   Bio12<-summaryBy(extract~year, data=monthly_df$pre, FUN=c(sum))
#
#   ##Bio13   #BIO13 = Precipitation of Wettest Month
#
#   Bio13<-ddply(monthly_df$pre, .(year), summarize,  Temp=max(extract))
#
#   ##Bio14
#   #BIO14 = Precipitation of Driest Month
#   Bio14<-ddply(monthly_df$pre, .(year), summarize,  Temp=min(extract))
#
#   ##Bio15   #BIO15 = Precipitation Seasonality (Coefficient of Variation)
#   Bio15<-cbind.data.frame(Year= unique(monthly_df$pre$year), Bio15=(ddply(monthly_df$pre, .(year), summarize,
#                                                                            Temp=sd(extract))[,2]/(1+(summaryBy(extract~year, data=monthly_df$pre, FUN=c(sum))[,2])/12))*100)
#
#   ##Bio16 #BIO16 = Precipitation of Wettest Quarter
#
#   Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Wettest')
#   bio16_function<-function(site.pre,WQ){
#     years<-levels(factor(site.pre$year))
#
#     bio16_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.pre[site.pre$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio16_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio16_df[,1]<-years
#     bio16_df<-as.data.frame(bio16_df)
#     names(bio16_df)<-c('Year', 'Bio16')
#     return(bio16_df)
#   }
#   Bio16<-bio16_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )
#
#   ##Bio17
#   #BIO17 = Precipitation of Driest Quarter
#   Yearly_WQ<-get_est_quarter(monthly_df$pre,type='Driest')
#   bio17_function<-function(site.pre,WQ){
#     years<-levels(factor(site.pre$year))
#
#     bio17_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.pre[site.pre$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio17_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio17_df[,1]<-years
#     bio17_df<-as.data.frame(bio17_df)
#     names(bio17_df)<-c('Year', 'Bio17')
#     return(bio17_df)
#   }
#   Bio17<-bio17_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )
#
#   ##Bio18
#   #BIO18 = Precipitation of Warmest Quarter
#
#   Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Warmest')
#   bio18_function<-function(site.pre,WQ){
#     years<-levels(factor(site.pre$year))
#
#     bio18_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.pre[site.pre$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio18_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio18_df[,1]<-years
#     bio18_df<-as.data.frame(bio18_df)
#     names(bio18_df)<-c('Year', 'Bio18')
#     return(bio18_df)
#   }
#   Bio18<-bio18_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )
#
#   #BIO19 = Precipitation of Coldest Quarter
#   Yearly_WQ<-get_est_quarter(monthly_df$temp.avg,type='Coldest')
#   bio19_function<-function(site.pre,WQ){
#     years<-levels(factor(site.pre$year))
#
#     bio19_df<-matrix(ncol=2, nrow=length(years))
#     for(i in 1:length(years)){
#       df_avgs<-site.pre[site.pre$year == years[i],]
#       qts_year_i<-strsplit(as.character(WQ[WQ$Year == years[i],'Quarter']),',')[[1]]
#       bio19_df[i,2] <-   sum(df_avgs[as.numeric(qts_year_i),'extract'])
#     }
#     bio19_df[,1]<-years
#     bio19_df<-as.data.frame(bio19_df)
#     names(bio19_df)<-c('Year', 'Bio19')
#     return(bio19_df)
#   }
#   Bio19<-bio19_function(site.pre = monthly_df$pre, WQ=Yearly_WQ )
#
#
#   Bioclim<-list(Bio1,Bio2,Bio3,Bio4,Bio4a,Bio5,Bio6,Bio7,Bio8,Bio9,Bio10,Bio11,Bio12,Bio13,Bio14,Bio15,Bio16,Bio17,Bio18)
#   names(Bioclim)<-c('Bio1','Bio2','Bio3','Bio4','Bio4a','Bio5','Bio6','Bio7','Bio8','Bio9','Bio10','Bio11','Bio12','Bio13','Bio14','Bio15','Bio16','Bio17','Bio18')
#
#   return(Bioclim)
# }
# WCV<-CRU_to_WorldClim(monthly_df=Climate_site)
# run document() or command + shift + D to check that the documentation is working properly
# remember that you need values for @param, @returns, @export, @examples
