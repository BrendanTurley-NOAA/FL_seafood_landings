library(lubridate)

setwd('~/Documents/R/Github/FL_seafood_landings/data')

### load data, skip first 9 lines because they are metadata
data <- read.csv('fl_landings_year.csv',skip=9)
### make some factors for easier parsing
data$Area.Description <- as.factor(data$Area.Description)
data$Species <- as.factor(data$Species)
### pull out Gag Grouper
ind_gag <- grep('gag',data$Species,ignore.case = T)
### pull out Ft Myers
ind_FM <- grep('fort myers',data$Area.Description,ignore.case = T)
### pull out Gag landed in Fort Myers
ind_FM_gag <- base::intersect(ind_gag,ind_FM)

gag <- data[ind_FM_gag,]

plot(gag$Year,gag$Pounds)
plot(gag$Year,gag$Estimated.Value)


### data by month
data <- read.csv('fl_landings_month.csv',skip=9)
data <- data[data$Year>2013,]
ind_gag <- grep('grouper, red',data$Species,ignore.case = T)
ind_ytd <- which(data$Month=='January' |
                      data$Month=='February' |
                      data$Month=='March' |
                      data$Month=='April' |
                      data$Month=='May' |
                      data$Month=='June' |
                      data$Month=='July')
### pull out Gag landed in year to date
ind_gag_ytd <- base::intersect(ind_gag,ind_ytd)
gag <- data[ind_gag,]

plot(gag$Year,gag$Pounds)
plot(gag$Year,gag$Estimated.Value)

boxplot(gag$Pounds~gag$Year)
boxplot(gag$Estimated.Value~gag$Year)

mths <- unique(gag$Month)
# plot(0,0,xlim=c(2013,2021),ylim=c(1e5,25e5))
par(bg='gray20')
b <- boxplot(gag$Estimated.Value~gag$Year,
             at=2014:2020,
             col='white',variwidth=T,border='gray50',
             xlab='Year',ylab='Estimated Value (USD)',
             staplewex=0,lty=1,lwd=2,axes=F)
axis(1, col="white", col.ticks="white", col.axis="white", cex.axis=2)
axis(2, col="white", col.ticks="white", col.axis="white", cex.axis=2)
box(col='white')
for(i in 1:7){
  temp <- gag[gag$Month==mths[i],]
  points(temp$Year,temp$Estimated.Value,col=i,pch=i,lty=i,typ='b',lwd=2)
}
legend('topright',
       c('Jan','Feb','Mar','Apr','May','Jun','Jul'),
       pch=1:7,col=1:7,bty='n')
# median_agg <- aggregate(gag$Estimated.Value,by=list(gag$Year),median,na.rm=T)
# points(median_agg$Group.1,median_agg$x,pch='-',cex=2,lwd=3)

