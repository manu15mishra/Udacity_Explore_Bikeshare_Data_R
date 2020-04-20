# loading required datasets
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

#Specifying the libraries to be used
library(tidyverse)

#removing NA from dataset
remove_NA = function(df)
    {
    df = na.omit(df[,])
    return(table(is.na(df)))
}

remove_NA(ny)

remove_NA(wash)

remove_NA(chi)

#Creating new columns
ny$Duration_min <- ny$Trip.Duration/60
ny$Duration_hr <- ny$Trip.Duration/3600

wash$Duration_min <- wash$Trip.Duration/60
wash$Duration_hr <- wash$Trip.Duration/3600

chi$Duration_min <- chi$Trip.Duration/60
chi$Duration_hr <- chi$Trip.Duration/3600

type = c("Customer","Subscriber")

ny_final = filter(ny, User.Type %in% type)
wash_final = filter(wash, User.Type %in% type)
chi_final = filter(chi, User.Type %in% type)


table(ny_final$User.Type)

ggplot(data=ny_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('User Type')+ylab('Number of Users')

total_ny <- aggregate(Duration_hr ~ User.Type, ny_final, function(x) sum(as.numeric(x)))
total_ny

ggplot(data=total_ny, mapping=aes(x=User.Type, y=Duration_hr, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Total travel duration (in hours)')

table(wash_final$User.Type)

ggplot(data=wash_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('User Type')+ylab('Number of Users')

total_wash <- aggregate(Duration_hr ~ User.Type, wash_final, function(x) sum(as.numeric(x)))
total_wash

ggplot(data=total_wash, mapping=aes(x=User.Type, y=Duration_hr, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Total travel duration (in hours)')

table(chi_final$User.Type)

ggplot(data=chi_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('User Type')+ylab('Number of Users')

total_chi <- aggregate(Duration_hr ~ User.Type, chi_final, function(x) sum(as.numeric(x)))
total_chi


ggplot(data=total_chi, mapping=aes(x=User.Type, y=Duration_hr, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Total travel duration (in hours)')

table(ny_final$User.Type)

ggplot(data=ny_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('User Type')+
    ylab('Number of Users')

avg_ny <- aggregate(Duration_min ~ User.Type, ny_final, function(x) mean(as.numeric(x)))
avg_ny

ggplot(data=avg_ny, mapping=aes(x=User.Type, y=Duration_min, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Average travel duration (in minutes)')

table(wash_final$User.Type)

ggplot(data=wash_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('User Type')+
    ylab('Number of Users')

avg_wash <- aggregate(Duration_min ~ User.Type, wash_final, function(x) mean(as.numeric(x)))
avg_wash

ggplot(data=avg_wash, mapping=aes(x=User.Type, y=Duration_min, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Average travel duration (in minutes)')

table(chi_final$User.Type)

ggplot(data=chi_final, mapping=aes(x=User.Type, fill=User.Type))+
    geom_histogram(stat="count", color='black')+
    xlab('Number of Users')+
    ylab('User Type')

avg_chi <- aggregate(Duration_min ~ User.Type, chi_final, function(x) mean(as.numeric(x)))
avg_chi

ggplot(data=avg_chi, mapping=aes(x=User.Type, y=Duration_min, fill=User.Type))+
    geom_bar(stat="identity", color='black')+
    xlab('User Type')+ylab('Average travel duration (in minutes)')

gen <- c("Male","Female")

ny_final = filter(ny_final, Gender %in% gen)
chi_final = filter(chi_final, Gender %in% gen)


table(ny_final$Gender)

ggplot(data=ny_final, mapping=aes(x=Gender, fill=Gender))+
    geom_histogram(stat="count", color='black')+
    xlab('Gender')+ylab('Number of users')

total_ny_gen <- aggregate(Duration_hr ~ Gender, ny_final, function(x) sum(as.numeric(x)))
total_ny_gen

ggplot(data=ny_final, mapping=aes(x=User.Type, y=Duration_hr, fill=Gender))+
    geom_histogram(stat="identity")+
    facet_wrap(~Gender)+
    xlab('User Type')+ylab('Total travel duration (in hours)')

avg_ny_gen <- aggregate(Duration_min ~ Gender, ny_final, function(x) mean(as.numeric(x)))
avg_ny_gen


ggplot(data=ny_final, mapping=aes(x=User.Type, y=Duration_min, fill=Gender))+
    geom_histogram(stat="identity")+
    facet_wrap(~Gender)+
    xlab('User Type')+ylab('Average travel duration (in minutes)')

table(chi_final$Gender)

ggplot(data=chi_final, mapping=aes(x=Gender, fill=Gender))+
    geom_histogram(stat="count", color='black')+
    xlab('Gender')+ylab('Number of users')

total_chi_gen <- aggregate(Duration_hr ~ Gender, chi_final, function(x) sum(as.numeric(x)))
total_chi_gen

ggplot(data=chi_final, mapping=aes(x=User.Type, y=Duration_hr, fill=Gender))+
    geom_histogram(stat="identity")+
    facet_wrap(~Gender)+
    xlab('User Type')+ylab('Total travel duration (in hours)')

avg_chi_gen <- aggregate(Duration_min ~ Gender, chi_final, function(x) mean(as.numeric(x)))
avg_chi_gen


ggplot(data=chi_final, mapping=aes(x=User.Type, y=Duration_min, fill=Gender))+
    geom_histogram(stat="identity")+
    facet_wrap(~Gender)+
    xlab('User Type')+ylab('Average travel duration (in minutes)')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
