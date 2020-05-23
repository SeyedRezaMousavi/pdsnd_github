
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

library(ggplot2)

library(lubridate)

city <- readline(prompt="Choose your city: Washington, New York or Chicago?   ")

if (city == 'Washington') {
    date = as.POSIXct(wash$Start.Time,format="%Y-%m-%d %H:%M:%OS")
} else if (city == 'Chicago') {
    date = chi$Start.Time        
} else if (city == 'New York') {
    date = ny$Start.Time    
} else {
    stop('The city you typed is an invalid choice!!! Please try again.')
}

month = month(date, label=TRUE)
month = month[!is.na(month)]
qplot(month, color = I('black'), fill = I('orange'), xlab = 'Month', ylab = 'Count') + ggtitle('Histogram of bike shares over Months')
t <- table(month)
print(paste('The most common month for', city, 'is', names(t)[t == max(t)]))

week_day = wday(date, label=TRUE)
week_day = week_day[!is.na(week_day)]
qplot(week_day, color = I('black'), fill = I('orange'), xlab = 'Week Day', ylab = 'Count') + ggtitle('Histogram of bike shares over Week Days')

t <- table(week_day)
print(paste('The most common week day for', city, 'is', names(t)[t == max(t)]))

hour = hour(date)
hour = hour[!is.na(hour)]
qplot(hour, binwidth=1/24, color = I('black'), xlab='Hour', ylab='Count') + scale_x_continuous(breaks = seq(0,23,1)) + ggtitle('Histogram of bike shares over Hours')
t <- table(hour)
H = paste0(names(t)[t == max(t)], ':00')
print(paste('The most common hour for', city, 'is', H))

library(ggplot2)

city <- readline(prompt="Choose your city: Washington, New York or Chicago?   ")

if (city == 'Washington') {
    UT = wash$User.Type
} else if (city == 'Chicago') {
    UT = chi$User.Type        
} else if (city == 'New York') {
    UT = ny$User.Type    
} else {
    stop('The city you typed is an invalid choice!!! Please try again.')
}

UT[UT==""] <- NA
UT = UT[!is.na(UT)]
qplot(UT, color = I('black'), fill = I('red'), xlab = 'User Type', ylab = 'Count') + ggtitle('Count of each user type')
t <- table(UT)
print('User type counts are:')
t

library(ggplot2)

TD_wash = wash$Trip.Duration
TD_wash = TD_wash[!is.na(TD_wash)]

TD_ny = ny$Trip.Duration
TD_ny = TD_ny[!is.na(TD_ny)]

TD_chi = chi$Trip.Duration
TD_chi = TD_chi[!is.na(TD_chi)]

print(paste('The total travel time for users in Washington is:', round(sum(TD_wash))))
print(paste('The total travel time for users in New York is:', round(sum(TD_ny))))
print(paste('The total travel time for users in Chicago is:', round(sum(TD_chi))))

names = c('Washington', 'New York', 'Chicago')
sums = c(sum(TD_wash), sum(TD_ny), sum(TD_chi))
barplot(height=sums, names=names, col='green', main="Total Travel Time for the Cities", ylab="Total Travel Time")


print(paste('The average travel time for users in Washington is:', round(mean(TD_wash))))
print(paste('The average travel time for users in New York is:', round(mean(TD_ny))))
print(paste('The average travel time for users in Chicago is:', round(mean(TD_chi))))

names = c('Washington', 'New York', 'Chicago')
sums = c(mean(TD_wash), mean(TD_ny), mean(TD_chi))
barplot(height=sums, names=names, col='blue', main="Average Travel Time for the Cities", ylab="Average Travel Time")

ny[ny==""]<-NA
qplot(x=User.Type, y=Trip.Duration, data=subset(ny, !is.na(User.Type)), geom='boxplot')+ ggtitle('Box Plot of Trip Durations for New York')

chi[chi==""]<-NA
qplot(x=User.Type, y=Trip.Duration, data=subset(chi, !is.na(User.Type)), geom='boxplot')+ ggtitle('Box Plot of Trip Durations for Chicago')


system('python -m nbconvert Explore_bikeshare_data.ipynb')
