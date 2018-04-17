rm(land)
library(tidyverse)
library(lubridate)
?group_by

land <- read.csv("/Users/quinngoodman/Desktop/STAT/STAT 480/Project/Global_Landslide_Catalog_Export.csv")

#Using lubridate to add month and year
land$event_date<-mdy_hms(land$event_date)
land$event_month<-month(land$event_date)
land$event_year<-year(land$event_date)

# Creating our time series
land<-land %>% filter(event_year>2006)

# Creating a unique designator for month and year together based on duration
# land$MonthYear<-dweeks(4*land$event_month)+dyears(land$event_year)

land$Date<-date_decimal(land$event_year+(land$event_month-1)/12)

# Creating a count of total 
landCount<-land %>% arrange(event_year) %>% group_by(Date) %>% count()

# Renaming the count
landCount$totalPerMonth<-landCount$n
landCount<-landCount %>% select(-n)
landRT<-landCount %>% select(totalPerMonth)

write.table(landRT, file = "MyData.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


# Whole time series without missing values, which should be zero.  
landCount %>% ggplot(aes(x=Date, y=totalPerMonth))+geom_line(aes(colour='red')) #+geom_text(aes())
# geom_text(aes(x=lastWeek, y=maxGross, label=Movie),
          #data=head(top3,3), hjust=0, colour='red')

# Time series over the different years
land %>% ggplot(aes(x=event_month, y=totalPerMonth, colour=as.factor(event_year)))+geom_line()


# Right now, we have our frequency in for year+month, but now we need 
# to add zeros to our dataframe for those that are not reported (should we even assume zero?)

# ultimately for time series, we need one observation, total,
# for each month. This information needs to be one column long and contain
# only the observations (tally from year+month) in order to use the RTSeries to analyze 
# the process. 

library(RTseries)

landSlide<-read.csv('/Users/quinngoodman/Desktop/STAT/STAT 480/Project/LandSlides.csv')
landSlide.ts<-ts(landSlide, frequency = 12, start= 2007)
landSlide.tsd <- tsd(landSlide.ts, data.title = 'Land Slides (2007-2017)', response.units = 'Count', time.units = 'Year')

iden(landSlide.tsd)


