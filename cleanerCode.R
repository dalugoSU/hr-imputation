dev.off()
rm(list = ls())
cat('\014')
setwd('YOUR WORKING DIRECTORY')

library(lubridate)
library(imputeTS)
library(ggplot2)
library(dplyr)
library(forecast)
library(Metrics)
library (data.table)
library(changepoint)
library(plotly)
library(ggplot2)
library(naniar)

# Process/Algorithm

# 1) Calculate month average variance
# 2) Split data into low and high variance
# 3) Process RMSE values with low and high variance data


# Needed functions
# ________________________________________________________________________________________________________

# Load all csv containing all days function
# Formats CSV to match Avin's style
# creates dft and DATA_ALL as global variables
format_csvs <- function(csv_path) 
{
  csv_files <-   list.files (path       = csv_path, 
                             pattern    = "*.csv", 
                             full.names = T)
  
  DATA <-  as_tibble (rbindlist (lapply (csv_files, fread))) #Data frame for july month
  DATA_ALL <- DATA
  Heatmapsdata <- DATA

  ## Formatting time for the aggregation command further
  DATA_ALL$Time <- as.POSIXct(DATA_ALL$Time, tz = "", "%Y-%m-%d %H:%M:%S")
  
  ## Aggregated the HR column for 1 minute intervals
  DATA_ALL <- aggregate(DATA_ALL["HR"], list(time = cut(DATA_ALL$Time, "1 min")),mean)
  ## Created Date Column
  DATA_ALL$date <- DATA_ALL$time
  
  ## Formatting of date and time Columns into Y-M-D and H:M:S format respectively.
  DATA_ALL$time <- format(lubridate::ymd_hms(DATA_ALL$time), "%H:%M:%S")
  DATA_ALL$date <- strptime(DATA_ALL$date,format = "%Y-%m-%d") 
  
  DATA_ALL$AHR <- DATA_ALL$HR
  
  DATA_ALL <<- DATA_ALL
  # SubSetting all the dates
  dft <<- DATA_ALL %>% mutate(year = year(date),  # mutating the date in year, month and day format for function construction
                             month = month(date),
                             day = day(date))
}

# Function takes Year Data data Frame
# Returns a list with each month as an item
# x[1] == march2020
# x[2] == april2020
# Index:
# 1:march2020 - 2:april2020 - 3:may2020 - 4:june2020 - 5:july2020 - 6:august2020 - 7:september2020 - 8:october2020
# 9:november2020 - 10:january2021 - 11:february2021 - 12:march2021
get_months <- function(data_frame)
{
  months <- list()
  
  counter <- 3
  year_tracker <- 2020
  
  for (i in 1:12)
  {
    if (counter == 12) 
    { 
      counter <- 1
      year_tracker <- 2021
    }
    months[[i]] = subset(dft, month==counter & year==year_tracker)
    counter = counter + 1
  }
  
  return(months)
}

# Interval generator for random minute removal
# @param number_of_intervals is the amount of intervals
# @param interval_count: range of interval
# i.e 3 range => 1-3 valid interval
IntervalGenerator <- function(interval_count, range) 
{ 
  
  random_interval <- list() # Master interval list
  index = 1
  
  while (length(random_interval) != interval_count) {
    
    temp_ = c() # Temporary list for interval
    
    while (length(temp_) != 2) {
      # I just need two numbers
      for (i in 1:2) {
        # create a random number between 1 and 100
        rand_num = floor(runif(n=1, min=1, max=101))
        # If the random number is already in temp_
        for (number in temp_) {
          if (number == rand_num) {
            next # If they are skip to next iteration
          }
        }
        # if they are not the same then add
        temp_ <- append(temp_, rand_num)
      }
    }
    
    # At this point I have a vector
    # Vector has two random numbers
    temp_ <- sort(temp_, decreasing=FALSE)
    
    # Now lets check for interval length
    calculation <- temp_[2] - temp_[1]
    
    # Adjust "calculation > 4" to change how big you want the intervals to be
    # The higher the condition the higher the interval possibilities
    if (calculation != range) {
      next
    } else if (calculation == 0) { # If vector numbers are the same skip
      next
    } else { # If interval is good, and not the same then add to master list
      random_interval[[index]] <- c(temp_)
      index = index + 1
    }
  }
  return(random_interval) # after we have 10 intervals then return
}

# Create a progress bar animation
pb = txtProgressBar(min = 0, max = 1000, initial = 0, style = 3)
progress_bar <- function(pb, iteration)
{
  setTxtProgressBar(pb, iteration)
  Sys.sleep(1)
  close(pb)
}

# Percent counts for minute removal
# @param month is target month
# @param percent is how much to remove
# @param minutes is minute range, 2 min, 4 min
get_percents <- function(month, percent, minutes)
{
  percent <- round((sapply(months[month], nrow) * percent) / minutes)
  return(percent)
}

# RMSE for month data
# @param ... passed intervals
# @param data is the month data
missing_data_lowvaraince <- function(data, ...)
{ 
  
  data <- sample(data)
  data$HR[c(...)] <- NA
  
  linear_RMSE = kalman_RMSE = stine_RMSE = spline_RMSE = nainterpolation1 =nainterpolation2 = nainterpolation3 = nakalman= 0
  
  nainterpolation1 <- imputeTS::na_interpolation(data$HR)                     ##Missing Value Imputation by Linear Interpolation
  nainterpolation2 <- imputeTS::na_interpolation(data$HR, option = "stine")   ##Missing Value Imputation by Stine Interpolation    
  nainterpolation3<-  imputeTS::na_interpolation(data$HR, option = "spline")  ##Missing Value Imputation by Spline Interpolation 
  nakalman      <-  imputeTS::na_kalman(data$HR)                              ##Missing Value Imputation by Kalman Smoothing
  
  
  linear_RMSE <-  Metrics::rmse(data$AHR, nainterpolation1)  ## Calculation of RMSE values for linear
  stine_RMSE <-   Metrics::rmse(data$AHR,nainterpolation2)  ## Calculation of RMSE values for stine
  spline_RMSE <-  Metrics::rmse(data$AHR,nainterpolation3)  ## Calculation of RMSE values for spline
  kalman_RMSE <-  Metrics::rmse(data$AHR,nakalman)          ## Calculation of RMSE values for kalman
  
  mean_linear_RMSE <-   round((linear_RMSE/length(data)), digits = 4) ## Mean of Linear RMSE of n no. of N/A Data
  mean_kalman_RMSE <- round((kalman_RMSE/length(data)), digits = 4)   ## Mean of Kalman RMSE of n no. of N/A Data
  mean_stine_RMSE <- round((stine_RMSE/length(data)), digits = 4)     ## Mean of Stine RMSE of n no. of N/A Data
  mean_spline_RMSE <- round((spline_RMSE/length(data)), digits = 4)   ## Mean of Spline RMSE of n no.of N/A Data
  
  return(c(mean_linear_RMSE, mean_kalman_RMSE, mean_stine_RMSE, mean_spline_RMSE))
}


# Get RMSE mean values for x runs; Void return
# @param data is the month data i.e month[1] for march2020
# @param month is which month. i.e 1 = march2020
# @param minutes is minutes vector i.e c(5, 10, 15, 20)
# @param run_count, how many iterations i.e 1000, 100
# Function tailored to -> c(5%, 10%, 15%, 20%, 30%)
get_rmse <- function(dataFrame, month, minutes, run_count)
{
  
  for (interval in minutes)
  {
    print(paste("Processing interval: ", interval))
    
    # Store 5 min 5, 10, 15, 20 percent block results
    template_results_5_percent <- list()
    template_results_10_percent <- list()
    template_results_15_percent <- list()
    template_results_20_percent <- list()
    template_results_30_percent <- list()
    
    framed_month <- dataFrame

    print("5 PERCENT")
    for (j in 1:run_count) {
      random_interval_vectors <- IntervalGenerator(get_percents(month, 0.05, interval), interval)
      unlisted_intervals <- unlist(random_interval_vectors) # Turn intervals into a vector with all together
      
      missing_data_lowvaraince_random <-  missing_data_lowvaraince(framed_month, unlisted_intervals) # Pass all the intervals at once
      template_results_5_percent[[j]] <- missing_data_lowvaraince_random
      progress_bar(pb, j)
    }
    
    print("10 PERCENT")
    for (j in 1:run_count) {
      random_interval_vectors <- IntervalGenerator(get_percents(month, 0.10, interval), interval)
      unlisted_intervals <- unlist(random_interval_vectors) # Turn intervals into a vector with all together
      
      missing_data_lowvaraince_random <-  missing_data_lowvaraince(framed_month, unlisted_intervals) # Pass all the intervals at once
      template_results_10_percent[[j]] <- missing_data_lowvaraince_random
      progress_bar(pb, j)
    }
    
    print("15 PERCENT")
    for (j in 1:run_count) {
      random_interval_vectors <- IntervalGenerator(get_percents(month, 0.15, interval), interval)
      unlisted_intervals <- unlist(random_interval_vectors) # Turn intervals into a vector with all together
      
      missing_data_lowvaraince_random <-  missing_data_lowvaraince(framed_month, unlisted_intervals) # Pass all the intervals at once
      template_results_15_percent[[j]] <- missing_data_lowvaraince_random
      progress_bar(pb, j)
    }
    
    print("20 PERCENT")
    for (j in 1:run_count) {
      random_interval_vectors <- IntervalGenerator(get_percents(month, 0.20, interval), interval)
      unlisted_intervals <- unlist(random_interval_vectors) # Turn intervals into a vector with all together
      
      missing_data_lowvaraince_random <-  missing_data_lowvaraince(framed_month, unlisted_intervals) # Pass all the intervals at once
      template_results_20_percent[[j]] <- missing_data_lowvaraince_random
      progress_bar(pb, j)
    }
    
    print("30 PERCENT")
    for (j in 1:run_count) {
      random_interval_vectors <- IntervalGenerator(get_percents(month, 0.30, interval), interval)
      unlisted_intervals <- unlist(random_interval_vectors) # Turn intervals into a vector with all together
      
      missing_data_lowvaraince_random <-  missing_data_lowvaraince(framed_month, unlisted_intervals) # Pass all the intervals at once
      template_results_30_percent[[j]] <- missing_data_lowvaraince_random
      progress_bar(pb, j)
    }
    
    linear_RMSE_mean_5_percent <- 0
    kalman_RMSE_mean_5_percent <- 0
    stine_RMSE_mean_5_percent <- 0
    spline_RMSE_mean_5_percent <- 0
    
    linear_RMSE_mean_10_percent <- 0
    kalman_RMSE_mean_10_percent <- 0
    stine_RMSE_mean_10_percent <- 0
    spline_RMSE_mean_10_percent <- 0
    
    linear_RMSE_mean_15_percent <- 0
    kalman_RMSE_mean_15_percent <- 0
    stine_RMSE_mean_15_percent <- 0
    spline_RMSE_mean_15_percent <- 0
    
    linear_RMSE_mean_20_percent <- 0
    kalman_RMSE_mean_20_percent <- 0
    stine_RMSE_mean_20_percent <- 0
    spline_RMSE_mean_20_percent <- 0
    
    linear_RMSE_mean_30_percent <- 0
    kalman_RMSE_mean_30_percent <- 0
    stine_RMSE_mean_30_percent <- 0
    spline_RMSE_mean_30_percent <- 0
    
    print("Gathering mean for each percent and test")
    
    # 5 percent mean
    while (TRUE) {
      count <- 0
      
      for (results in template_results_5_percent) {
        linear_RMSE_mean_5_percent <- linear_RMSE_mean_5_percent + results[1]
        count <- count + 1
        progress_bar(pb, count)
      }
      linear_RMSE_mean_5_percent <- linear_RMSE_mean_5_percent/run_count
      count <- 0
      
      for (results in template_results_5_percent) {
        kalman_RMSE_mean_5_percent <- kalman_RMSE_mean_5_percent + results[2]
        count <- count + 1
        progress_bar(pb, count)
      }
      kalman_RMSE_mean_5_percent <- kalman_RMSE_mean_5_percent/run_count
      count <- 0
      
      for (results in template_results_5_percent) {
        stine_RMSE_mean_5_percent <- stine_RMSE_mean_5_percent + results[3]
        count <- count + 1
        progress_bar(pb, count)
      }
      stine_RMSE_mean_5_percent <- stine_RMSE_mean_5_percent/run_count
      count <- 0
      
      for (results in template_results_5_percent) {
        spline_RMSE_mean_5_percent <- spline_RMSE_mean_5_percent + results[4]
        count <- count + 1
        progress_bar(pb, count)
      }
      spline_RMSE_mean_5_percent <- spline_RMSE_mean_5_percent/run_count
      break
    }
    
    # 10 percent mean
    while (TRUE) {
      count <- 0
      for (results in template_results_10_percent) {
        linear_RMSE_mean_10_percent <- linear_RMSE_mean_10_percent + results[1]
        count <- count + 1
        progress_bar(pb, count)
      }
      linear_RMSE_mean_10_percent <- linear_RMSE_mean_10_percent/run_count
      count <- 0
      
      for (results in template_results_10_percent) {
        kalman_RMSE_mean_10_percent <- kalman_RMSE_mean_10_percent + results[2]
        count <- count + 1
        progress_bar(pb, count)
      }
      kalman_RMSE_mean_10_percent <- kalman_RMSE_mean_10_percent/run_count
      count <- 0
      
      for (results in template_results_10_percent) {
        stine_RMSE_mean_10_percent <- stine_RMSE_mean_10_percent + results[3]
        count <- count + 1
        progress_bar(pb, count)
      }
      stine_RMSE_mean_10_percent <- stine_RMSE_mean_10_percent/run_count
      count <- 0
      
      for (results in template_results_10_percent) {
        spline_RMSE_mean_10_percent <- spline_RMSE_mean_10_percent + results[4]
        count <- count + 1
        progress_bar(pb, count)
      }
      spline_RMSE_mean_10_percent <- spline_RMSE_mean_10_percent/run_count
      break
    }
    
    # 15 percent mean
    while (TRUE) {
      count <- 0
      for (results in template_results_15_percent) {
        linear_RMSE_mean_15_percent <- linear_RMSE_mean_15_percent + results[1]
        count <- count + 1
        progress_bar(pb, count)
      }
      linear_RMSE_mean_15_percent <- linear_RMSE_mean_15_percent/run_count
      count <- 0
      
      for (results in template_results_15_percent) {
        kalman_RMSE_mean_15_percent <- kalman_RMSE_mean_15_percent + results[2]
        count <- count + 1
        progress_bar(pb, count)
      }
      kalman_RMSE_mean_15_percent <- kalman_RMSE_mean_15_percent/run_count
      count <- 0
      
      for (results in template_results_15_percent) {
        stine_RMSE_mean_15_percent <- stine_RMSE_mean_15_percent + results[3]
        count <- count + 1
        progress_bar(pb, count)
      }
      stine_RMSE_mean_15_percent <- stine_RMSE_mean_15_percent/run_count
      count <- 0
      
      for (results in template_results_15_percent) {
        spline_RMSE_mean_15_percent <- spline_RMSE_mean_15_percent + results[4]
        count <- count + 1
        progress_bar(pb, count)
      }
      spline_RMSE_mean_15_percent <- spline_RMSE_mean_15_percent/run_count
      break
    }
    
    # 20 percent mean
    while (TRUE) {
      count <- 0
      for (results in template_results_20_percent) {
        linear_RMSE_mean_20_percent <- linear_RMSE_mean_20_percent + results[1]
        count <- count + 1
        progress_bar(pb, count)
      }
      linear_RMSE_mean_20_percent <- linear_RMSE_mean_20_percent/run_count
      count <- 0
      
      for (results in template_results_20_percent) {
        kalman_RMSE_mean_20_percent <- kalman_RMSE_mean_20_percent + results[2]
        count <- count + 1
        progress_bar(pb, count)
      }
      kalman_RMSE_mean_20_percent <- kalman_RMSE_mean_20_percent/run_count
      count <- 0
      
      for (results in template_results_20_percent) {
        stine_RMSE_mean_20_percent <- stine_RMSE_mean_20_percent + results[3]
        count <- count + 1
        progress_bar(pb, count)
      }
      stine_RMSE_mean_20_percent <- stine_RMSE_mean_20_percent/run_count
      count <- 0
      
      for (results in template_results_20_percent) {
        spline_RMSE_mean_20_percent <- spline_RMSE_mean_20_percent + results[4]
        count <- count + 1
        progress_bar(pb, count)
      }
      spline_RMSE_mean_20_percent <- spline_RMSE_mean_20_percent/run_count
      break
    }
    
    # 30 percent mean
    while (TRUE) {
      count <- 0
      for (results in template_results_30_percent) {
        linear_RMSE_mean_30_percent <- linear_RMSE_mean_30_percent + results[1]
        count <- count + 1
        progress_bar(pb, count)
      }
      linear_RMSE_mean_30_percent <- linear_RMSE_mean_30_percent/run_count
      count <- 0
      
      for (results in template_results_30_percent) {
        kalman_RMSE_mean_30_percent <- kalman_RMSE_mean_30_percent + results[2]
        count <- count + 1
        progress_bar(pb, count)
      }
      kalman_RMSE_mean_30_percent <- kalman_RMSE_mean_30_percent/run_count
      count <- 0
      
      for (results in template_results_30_percent) {
        stine_RMSE_mean_30_percent <- stine_RMSE_mean_30_percent + results[3]
        count <- count + 1
        progress_bar(pb, count)
      }
      stine_RMSE_mean_30_percent <- stine_RMSE_mean_30_percent/run_count
      count <- 0
      
      for (results in template_results_30_percent) {
        spline_RMSE_mean_30_percent <- spline_RMSE_mean_30_percent + results[4]
        count <- count + 1
        progress_bar(pb, count)
      }
      spline_RMSE_mean_30_percent <- spline_RMSE_mean_30_percent/run_count
      break
    }
    
    print("Results: \n")
    print(paste("5 percent Linear RMSE:", linear_RMSE_mean_5_percent))
    print(paste("5 percent Kalman RMSE:", kalman_RMSE_mean_5_percent))
    print(paste("5 percent Stine RMSE:", stine_RMSE_mean_5_percent))
    print(paste("5 percent Spline RMSE:", spline_RMSE_mean_5_percent))
    
    print("__________________________________________________________\n")
    print(paste("10 percent Linear RMSE:", linear_RMSE_mean_10_percent))
    print(paste("10 percent Kalman RMSE:", kalman_RMSE_mean_10_percent))
    print(paste("10 percent Stine RMSE:", stine_RMSE_mean_10_percent))
    print(paste("10 percent Spline RMSE:", spline_RMSE_mean_10_percent))
    
    print("__________________________________________________________\n")
    print(paste("15 percent Linear RMSE:", linear_RMSE_mean_15_percent))
    print(paste("15 percent Kalman RMSE:", kalman_RMSE_mean_15_percent))
    print(paste("15 percent Stine RMSE:", stine_RMSE_mean_15_percent))
    print(paste("15 percent Spline RMSE:", spline_RMSE_mean_15_percent))
    
    print("__________________________________________________________\n")
    print(paste("20 percent Linear RMSE:", linear_RMSE_mean_20_percent))
    print(paste("20 percent Kalman RMSE:", kalman_RMSE_mean_20_percent))
    print(paste("20 percent Stine RMSE:", stine_RMSE_mean_20_percent))
    print(paste("20 percent Spline RMSE:", spline_RMSE_mean_20_percent))
    
    print("__________________________________________________________\n")
    print(paste("30 percent Linear RMSE:", linear_RMSE_mean_30_percent))
    print(paste("30 percent Kalman RMSE:", kalman_RMSE_mean_30_percent))
    print(paste("30 percent Stine RMSE:", stine_RMSE_mean_30_percent))
    print(paste("30 percent Spline RMSE:", spline_RMSE_mean_30_percent))
  }
  print("Done")
}

# Function to calculate the change point analysis
change_point <- function(rate)
{
  m.bsm <- changepoint::cpt.var(rate, method = "BinSeg",pen.value="10*log(n)")  
  # 5 Change points in HR 
  return(cpts(m.bsm))
}

# Segment days for changepoint
segment_var<- function(data, cp)
{
  if (length(cp) == 5)
  {
    seg1 <-  data[1:cp[1],]
    var1<- var(seg1$HR)
    
    seg2 <- data[cp[1]:cp[2],]
    var2<- var(seg2$HR)
    
    seg3 <- data[cp[2]:cp[3],]
    var3<- var(seg3$HR)
    
    seg4 <- data[cp[3]:cp[4],]
    var4<- var(seg4$HR)
    
    seg5 <- data[cp[4]:cp[5],]
    var5<- var(seg5$HR)
    
    seg6 <- data[cp[5]:length(cp),]
    var6<- var(seg6$HR)
    
    return (list(var1,var2,var3,var4,var5,var6))
  }
  else if (length(cp) == 4)
  {
    seg1 <-  data[1:cp[1],]
    var1<- var(seg1$HR)
    
    seg2 <- data[cp[1]:cp[2],]
    var2<- var(seg2$HR)
    
    seg3 <- data[cp[2]:cp[3],]
    var3<- var(seg3$HR)
    
    seg4 <- data[cp[3]:cp[4],]
    var4<- var(seg4$HR)

    seg5 <- data[cp[4]:length(cp),]
    var5 <- var(seg5$HR)
    
    return (list(var1,var2,var3,var4,var5))
  }
}  


# ________________________________________________________________________________________________________


# Getting data and splitting months
# ________________________________________________________________________________________________________
format_csvs("/Volumes/myDrive/imupation/filtered_year_data/")
months <- get_months(dft)



# Collecting average variance for march 2020
# ________________________________________________________________________________________________________
twentyMarch <- do.call(rbind.data.frame, months[1])
dates <- unique(twentyMarch$day)


for (i in 1:length(dates))
{
  if (i >= 17) { assign(paste("twentyMarch", i + 6, sep= ""), twentyMarch[twentyMarch$day==i + 6,]) }
  else {assign(paste("twentyMarch", i, sep= ""), twentyMarch[twentyMarch$day==i,]) }
}

# March2020 missing days 17-22, from filtering done

template_1st <- change_point(twentyMarch1$HR)
template_2nd <- change_point(twentyMarch2$HR)
template_3rd <- change_point(twentyMarch3$HR)
template_4th <- change_point(twentyMarch4$HR)
template_5th <- change_point(twentyMarch5$HR)
template_6th <- change_point(twentyMarch6$HR)
template_7th <- change_point(twentyMarch7$HR)
template_8th <- change_point(twentyMarch8$HR)
template_9th <- change_point(twentyMarch9$HR)
template_10th <- change_point(twentyMarch10$HR)
template_11th <- change_point(twentyMarch11$HR)
template_12th <- change_point(twentyMarch12$HR)
template_13th <- change_point(twentyMarch13$HR)
template_14th <- change_point(twentyMarch14$HR)
template_15th <- change_point(twentyMarch15$HR)
template_16th <- change_point(twentyMarch16$HR)
template_23rd <- change_point(twentyMarch23$HR)
template_24th <- change_point(twentyMarch24$HR)
template_25th <- change_point(twentyMarch25$HR)
template_26th <- change_point(twentyMarch26$HR)
template_27th <- change_point(twentyMarch27$HR)
template_28th <- change_point(twentyMarch28$HR)
template_29th <- change_point(twentyMarch29$HR)
template_30th <- change_point(twentyMarch30$HR)
template_31st <- change_point(twentyMarch31$HR)


week_one <- list(segment_var(twentyMarch1,template_1st),
                 segment_var(twentyMarch2,template_2nd),
                 segment_var(twentyMarch3,template_3rd),
                 segment_var(twentyMarch4,template_4th),
                 segment_var(twentyMarch5,template_5th),
                 segment_var(twentyMarch6,template_6th),
                 segment_var(twentyMarch7,template_7th),
                 segment_var(twentyMarch8,template_8th)
                 )

week_two <- list(segment_var(twentyMarch9, template_9th),
                 segment_var(twentyMarch10,template_10th),
                 segment_var(twentyMarch11,template_11th),
                 segment_var(twentyMarch12,template_12th),
                 segment_var(twentyMarch13,template_13th),
                 segment_var(twentyMarch14,template_14th),
                 segment_var(twentyMarch15,template_15th),
                 segment_var(twentyMarch16,template_16th)
                 )

week_three<- list(segment_var(twentyMarch23,template_23rd),
                  segment_var(twentyMarch24,template_24th),
                  segment_var(twentyMarch25,template_25th),
                  segment_var(twentyMarch26,template_26th),
                  segment_var(twentyMarch27,template_27th),
                  segment_var(twentyMarch28,template_28th),
                  segment_var(twentyMarch29,template_29th),
                  segment_var(twentyMarch30,template_30th),
                  segment_var(twentyMarch31,template_31st)
                  )



weekone <- data.frame(matrix(unlist(week_one), nrow = 6))
weektwo <- data.frame(matrix(unlist(week_two), nrow = 6))
weekthree <- data.frame(matrix(unlist(week_three), nrow = 6))



var1<- mean(unlist(week_one))
var2<- mean(unlist(week_two))  
var3<- mean(unlist(week_three))


## Average variance of complete month in different segments 
avg_variance <- (var1+var2+var3) / 3

avg_variance # Avg variance for march 2020: 86.9651
# ___________________________________________________________________________________________________


segment_day_one <- segment_var(twentyMarch1, template_1st)

template_1st
segment_day_one

# Avg variance for march 2020: 86.9651
# Change Points : 340 420 478 547 775
# Variances     : 59.68857 - 60.08963 - 85.83122 - 16.13498 - 11.72243 - 135.5945

timeslot_1st <- twentyMarch1[1:340,] # low

timeslot_2nd <- twentyMarch1[340:420,] # low

timeslot_3rd <- twentyMarch1[420:478,] # low

timeslot_4th <- twentyMarch1[478:547,] # low

timeslot_5th <- twentyMarch1[547:775,] # low

timeslot_6th <- twentyMarch1[775:nrow(twentyMarch1),]  # high


lowvariance_data <- merge(merge(merge(merge(timeslot_1st, timeslot_2nd, all=TRUE), timeslot_3rd, all=TRUE), timeslot_4th, all=TRUE), timeslot_5th, all=TRUE)
highvariance_data <- timeslot_6th

View(lowvariance_data) # 775 Entries
View(highvariance_data) # 935 Entries

# Low variance RMSE for day 1 march 2020
get_rmse(lowvariance_data, 1, c(5, 10, 15, 20, 30), 1000)

# High variance RMSE for day 1 march 2020
get_rmse(highvariance_data, 1, c(5, 10, 15, 20, 30), 1000)
