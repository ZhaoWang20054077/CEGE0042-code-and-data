library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(sf)
library(nnet)

covid_case <- readOGR(dsn="D:/STUDY/0042/covid_mean.shp", layer="covid_mean", 
                        p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)
uk_case_matrix<-data.matrix(covid_case@data[,14:32])

covid <- read.csv("D:/STUDY/0042/cases.csv")
population <- read.csv("D:/STUDY/0042/population.csv")
covid_matrix<-data.matrix(covid[,2:ncol(covid)])

covid_case$case_rate <- covid[,20]/population[,4]
covid_case$case <- covid[,20]

tm_shape(covid_case)+
  tm_polygons("case_rate", title = "", style="jenks",palette="-RdYlBu",legend.reverse = TRUE)+
  tm_borders("white")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar()

Output.Areas2 <- read_sf("D:/STUDY/0042/covid_mean.shp")
OA.Census_sf <- st_as_sf(Output.Areas2)
tm_shape(OA.Census_sf) + 
  tm_fill("mean_case",
          palette = "Reds", 
          style = "quantile", 
          title = "cases") +
  tm_borders(alpha=.4)


W_cont_el <- poly2nb(OA.Census_sf, queen=FALSE)
W_cont_el_mat <- nb2listw(W_cont_el,style="W", zero.policy=TRUE)

covid_1 <- read.csv("D:/STUDY/0042/mosa_covid_2022.csv")
case_matrix <- data.matrix(covid_1[,2:ncol(covid_1)])
MeanCase <- colSums(covid_1[,2:(ncol(covid_1))])
TimeLagged <- data.frame(week = 1:17, t=MeanCase[2:(length(MeanCase))], t_minus_1=MeanCase[1:(length(MeanCase)-1)])
p1 <- ggplot(TimeLagged, aes(x=week, y=t)) + geom_line()+labs(y="number of cases")
p2 <- ggplot(TimeLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  annotate("text", 8.5, 10, label=paste("r =", round(cor(TimeLagged$t, TimeLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)

acf(MeanCase)

acf(covid_matrix[1,], lag.max=20, main="ACF, E02000001")
pacf(covid_matrix[1,], lag.max=20, main="PACF, E02000001")

W <- nb2listw(poly2nb(OA.Census_sf),style="W", zero.policy = TRUE)
globalMoran <- moran.test(OA.Census_sf$mean_case, W, zero.policy = TRUE)
globalMoran

lm <- localmoran(OA.Census_sf$mean_case, W, zero.policy = TRUE)
lm

source("D:/STUDY/0042/Data/starima_package.R")
Wmat <- listw2mat(W)
stacf(t(uk_case_matrix), Wmat, 19)

stpacf(t(uk_case_matrix), Wmat, 19)

MSOA <- read.csv("D:/STUDY/0042/cases.csv")
MSOA_matrix<-data.matrix(MSOA[,2:19])
pts <- SpatialPoints(MSOA[,21:22], 
                     proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# Years are converted to date format
time <- seq(as.Date("2022-01-02"), length = 18, by = "week")
stfdf <- STFDF(pts, time, data.frame(as.vector(t(MSOA_matrix))))
names(stfdf@data) <- "mean_case"
ChSTVar <- variogram(mean_case~1, stfdf)
plot(ChSTVar)
plot(ChSTVar, wireframe=T)

fit.ar <- arima(case_matrix[1,1:17],order=c(1,0,0))
pre.Ar <- arima(case_matrix[1,13:(ncol(case_matrix))],model=fit.ar)
fit.ar

NRMSE_fit <- NRMSE(res=fit.ar$residuals, obs=case_matrix[1,1:16])
tsdiag(fit.ar)
pre.ar<-predict(fit.ar, n.ahead=17)
matplot(1:17,cbind(case_matrix[1, 1:17],pre.ar$pred),type="l",main="", xlab="week", ylab="Mean case")


X <- t(as.matrix(uk_case_matrix))
y <- as.matrix(X[-1,])
temp.nnet <- nnet(X[1:18, 970:982], y[1:18, 970:982], decay=5e-6, linout = TRUE, size=6)
temp.pred<-predict(temp.nnet, y[1:17, 1:20])
temp.pred[,1]
matplot(cbind(y[1:17,2], temp.pred[,1]),ylab="cases", xlab="week", main="", type="l")
