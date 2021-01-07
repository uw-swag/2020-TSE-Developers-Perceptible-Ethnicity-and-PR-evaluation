library(RMySQL)
library(car)
library(stargazer)
library(texreg)
library(FSA)
library(heplots)
library(xtable)
library(vioplot)
library(plyr)
library(dplyr)

setwd("/Users/g5rodrig/Downloads/replication_package/Data/R scripts")

#############################################per project
data <- read.csv('../datasets/per_project_data.csv')

data = data[complete.cases(data), ]
#data$eth = as.factor(data$eth)
white <- data$success_rate[data$eth=="White"]
un <- data$success_rate[data$eth=="Unknown"]
api <- data$success_rate[data$eth=="API"]
#aian <- data$success_rate[data$eth=="AIAN"]
black <- data$success_rate[data$eth=="Black"]
hispanic <- data$success_rate[data$eth=="Hispanic"]

fd <- data.frame(white)
fd$black <- c(black, rep(NA, nrow(fd)-length(black)))
fd$api <- c(api, rep(NA, nrow(fd)-length(api)))
fd$hispanic <- c(hispanic, rep(NA, nrow(fd)-length(hispanic)))
fd$un <- c(un, rep(NA, nrow(fd)-length(un)))

means <- aggregate(success_rate~eth, data, mean)
cdatmean <- ddply(data, "eth", summarise, success_rate.mean=mean(success_rate))
cdatmedian <- ddply(data, "eth", summarise, success_rate.median=median(success_rate))

library(ggplot2)
ggplot(data=data, aes(x=eth, y=success_rate, fill=success_rate)) + geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per person') +
  geom_text(data = means, aes(label = round(success_rate,digits=3), y = success_rate - 0.05))

ggplot() + geom_density(data=data,
                        aes(x=success_rate,
                            group=eth, colour=eth)) + geom_vline(data=cdatmean,
                               aes(xintercept=success_rate.mean,
                                 colour=eth), linetype="dashed", size=1) + labs(x='Acceptance Rate', y='Density of developers', color='Ethnicity')




kruskal.test(success_rate~eth, data=data)
dunnTest(success_rate~eth, data=data, method="bonferroni")

mean(white)
mean(black)
mean(hispanic)
mean(api)
mean(un)

median(white)
median(black)
median(hispanic)
median(api)
median(un)

#plot success_ratio vs number of pull requests
data = data[complete.cases(data), ]
data = subset(data, total_count >= 5)
data = subset(data, total_count < 500)
data = data[complete.cases(data), ]
#data$eth = as.factor(data$eth)
white <- data$success_rate[data$eth=="White"]
api <- data$success_rate[data$eth=="API"]
#aian <- data$success_rate[data$eth=="AIAN"]
black <- data$success_rate[data$eth=="Black"]
hispanic <- data$success_rate[data$eth=="Hispanic"]

boxplot(success_rate~eth, data=data)


kruskal.test(success_rate~eth, data=data)
dunnTest(success_rate~eth, data=data, method="bonferroni")
mean(white)
mean(black)
mean(hispanic)
mean(api)
median(white)
median(black)
median(api)
median(hispanic)

#############################################per person
data <- read.csv('../datasets/overall.csv')

data = data[complete.cases(data), ]
#data$eth = as.factor(data$eth)
white <- data$success_rate[data$eth=="White"]
un <- data$success_rate[data$eth=="Unknown"]
api <- data$success_rate[data$eth=="API"]
#aian <- data$success_rate[data$eth=="AIAN"]
black <- data$success_rate[data$eth=="Black"]
hispanic <- data$success_rate[data$eth=="Hispanic"]
means <- aggregate(success_rate~eth, data, mean)
library(ggplot2)

ggplot(data=data, aes(x=eth, y=success_rate, fill=success_rate)) + geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per person') +
  geom_text(data = means, aes(label = round(success_rate,digits=3), y = success_rate - 0.05))


cdatmean <- ddply(data, "eth", summarise, success_rate.mean=mean(success_rate))
cdatmedian <- ddply(data, "eth", summarise, success_rate.median=median(success_rate))

ggplot() + geom_density(data=data,
                        aes(x=success_rate,
                            group=eth, colour=eth)) + geom_vline(data=cdatmean,
                              aes(xintercept=success_rate.mean,
                                 colour=eth), linetype="dashed", size=1) + labs(x='Acceptance Rate', y='Density of develoeprs', color='Ethnicity')


kruskal.test(success_rate~eth, data=data)
dunnTest(success_rate~eth, data=data, method="bonferroni")
mean(white)
mean(black)
mean(hispanic)
mean(api)
mean(un)
median(white)
median(black)
median(hispanic)
median(api)
median(un)

######################################## Heat Map


library(ggplot2)
library(hrbrthemes)
library(plotly)
library("TeachingSampling")
# Dummy data


# by country percentage
data <- matrix(c(2.59828727131179	,10.1320228363825	,87.0929025561178	,0.176787336187881,
                 2.52910787261932	,3.37728429331483	,93.8931297709924	,0.200478063073483,
                 1.8839793982109	    ,9.56898888587693	,88.2352941176471	,0.311737598265112,
                 0	                ,2.10526315789474	,26.3157894736842	,71.5789473684211,
                 0	                ,0	                ,49.3506493506493	,50.6493506493507,
                 1.61352781721957	,1.91041693558797	,96.3598812443527	,0.116174002839809,
                 1.43497757847534	,1.47341447789878	,96.8097373478539	,0.281870595771941,
                 0.020889910173386	,95.3624399415083	,4.53311050762482	,0.083559640693545,
                 0.268889486421081	,75.4772788383974	,24.0924979833288	,0.161333691852649,
                 1.1071569790431	    ,72.3210755239225	,26.4926848556742	,0.079082641360222,
                 76.0597496972144	,0.524828421477594	,23.3346790472346	,0.080742834073476,
                 36.6908746447742	,1.79981054625829	,61.5093148089675	,0,
                 84.4036697247706	,0.152905198776758	,15.4434250764526	,0,
                 57.283950617284	    ,0.617283950617284	,41.9753086419753	,0.123456790123457),14, 4, byrow=TRUE)

data = IPFP(data, c(265.895060997518,274.823013007914,735.438404782555,123.843521212013), c(100,100,100,100,100,100,100,100,100,100,100,100,100,100))

# by ethniciy percentage
data <- matrix(c(24.5254133496632	,36.5728001873427	,53.8682235865335	,34.0625,
                 5.02143294549908	,2.56425267841461	,12.2156013001083	,8.125,
                 2.12798530312309	,4.13324746794684	,6.53063681232695	,7.1875,
                 0	                ,0.011708916339793	,0.025079250431363	,21.25,
                 0	                ,0	                ,0.038120460655672	,12.1875,
                 3.82731169626454	,1.73291961828933	,14.97732835761	    ,5.625,
                 1.71463563992652	,0.673262689538083	,7.57995265037519	,6.875,
                 0.015309246785058	,26.725601545577	    ,0.217687893744232	,1.25,
                 0.153092467850582	,16.4334640828991	,0.898840335460054	,1.875,
                 0.428658909981629	,10.7078039927405	,0.672123911560531	,0.625,
                 28.8426209430496	,0.076107956208653	,0.579832269973115	,0.625,
                 17.7893447642376	,0.333704115684093	,1.95417519361181	,0,
                 8.45070422535211	,0.005854458169896	,0.101320171742707	,0,
                 7.10349050826699	,0.029272290849482	,0.341077805866538	,0.3125),14, 4, byrow=TRUE)
data = IPFP(data, c(100,100,100,100), c(149.028937123539,27.926286924022,19.9793695833969,21.2867881667712,12.2256204606557,26.1625596721639,16.8428509798398,28.2085986861062,19.3603968862098,12.4335868142826,30.1235611692314,20.0772240735335,8.55787885526472,7.78634060498301))

# by raw numbers
data <- matrix(c(1602	,6247	,53698	,109,
                 328	    ,438	    ,12177	,26,
                 139	    ,706	    ,6510	,23,
                 0	    ,2	    ,25	    ,68,
                 0	    ,0	    ,38	    ,39,
                 250	    ,296	    ,14930	,18,
                 112	    ,115	    ,7556	,22,
                 1	    ,4565	,217	    ,4,
                 10	    ,2807	,896	    ,6,
                 28	    ,1829	,670	    ,2,
                 1884	,13	    ,578	    ,2,
                 1162	,57	    ,1948	,0,
                 552	    ,1	    ,101	    ,0,
                 464	    ,5	    ,340	    ,1),14, 4, byrow=TRUE)
data = IPFP(data, c(6532,17081,99684,320), c(61656,12969,7378,95,77,15494,7805,4787,3719,2529,2477,3167,654,810))



data <- data[,-5]
data <- data[-15,]
save <- data

ethnicity <- c("Hispanic", "API", "White", "Black")
country <- c("US", "UK", "Canada", "Nigeria", "Kenya", "Germany", "France", "India", "China", "Japan", "Spain", "Brazil", "Mexico", "Argentina")
data <- expand.grid(X=ethnicity, Y=country)
data$percentage <- as.numeric(as.list(t(save)))


# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= percentage)) +
  geom_tile() +
  scale_fill_gradient(low="#56B1F7", high="#132B43") +
  theme_ipsum() + labs(x="", y="", fill='Percentage')


ggplotly(p)

#########################################################taking integrator's ethnicity into account

white <- read.csv('../datasets/white_all.csv')
black <- read.csv('../datasets/black_all.csv')
hispanic <- read.csv('../datasets/hispanic_all.csv')
api <- read.csv('../datasets/api_all.csv')

white_white <- data[ which(data$submitter_eth=="White" & data$integrator_eth=="White"), ]
black_white <- data[ which(data$submitter_eth=="Black" & data$integrator_eth=="White"), ]
hispanic_white <- data[ which(data$submitter_eth=="Hispanic" & data$integrator_eth=="White"), ]
api_white <- data[ which(data$submitter_eth=="API" & data$integrator_eth=="White"), ]

hispanic_hispanic <- data[ which(data$submitter_eth=="Hispanic" & data$integrator_eth=="Hispanic"), ]
black_hispanic <- data[ which(data$submitter_eth=="Black" & data$integrator_eth=="Hispanic"), ]
api_hispanic <- data[ which(data$submitter_eth=="API" & data$integrator_eth=="Hispanic"), ]

white_api <- data[ which(data$submitter_eth=="API" & data$integrator_eth=="API"), ]
api_api <- data[ which(data$submitter_eth=="API" & data$integrator_eth=="API"), ]

black_black <- data[ which(data$submitter_eth=="Black" & data$integrator_eth=="Black"), ]



data1 <- white
data2 <- white_white

data1 <- black
data2 <- black_black

data1 <- hispanic
data2 <- hispanic_hispanic

data1 <- api
data2 <- api_api

data1 <- black
data2 <- black_white

data1 <- hispanic
data2 <- hispanic_white

data1 <- api_api
data2 <- api_white

data1 <- white_white
data2 <- black_white

data1 <- white_white
data2 <- hispanic_white

data1 <- white_white
data2 <- api_white

data1 <- api_hispanic
data2 <- api_white



ggplot()  + geom_density(data=data1,
                aes(x=success_rate, color='White submitter-All integrators')) +
            geom_density(data=data2,
                aes(x=success_rate, color='White submitter-White integrator')) +
            geom_vline(aes(xintercept=mean(data1$success_rate)),
                color="blue", linetype="dashed", size=1) +
            geom_vline(aes(xintercept=mean(mean(data2$success_rate))),
                color="red", linetype="dashed", size=1) + xlab("Acceptance Rate") + ylab("Density") + scale_color_manual(name = "Perceived ethnicity group",
                values = c('White submitter-All integrators' = "blue", 'White submitter-White integrator' = "red"),
                labels = c('White submitter-All integrators', 'White submitter-White integrator')) + theme(text=element_text(size=14))


data1$submitter_eth = "White-All"
data2$submitter_eth = "White-White"
data3 <- rbind(data1, data2)
ggplot(data=data3, aes(x=submitter_eth, y=success_rate, fill=success_rate),inherit.aes = FALSE) +
geom_violin() +
stat_summary(fun.y=mean, colour="darkred", geom="point",
             shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per submitter/integrator pair')


data1 <- api
data2 <- api_api
data3 <- api_white

data1$submitter_eth = "API-All"
data2$submitter_eth = "API-API"
data3$submitter_eth = "API-White"
data4 <- rbind(data1, data2, data3)
ggplot(data=data4, aes(x=submitter_eth, y=success_rate, fill=success_rate),inherit.aes = FALSE) +
  geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per submitter/integrator pair')

data1 <- black
data2 <- black_black
data3 <- black_white

data1$submitter_eth = "Black-All"
data2$submitter_eth = "Black-Black"
data3$submitter_eth = "Black-White"
data4 <- rbind(data1, data2, data3)
ggplot(data=data4, aes(x=submitter_eth, y=success_rate, fill=success_rate),inherit.aes = FALSE) +
  geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per submitter/integrator pair')

data1 <- hispanic
data2 <- hispanic_hispanic
data3 <- hispanic_white

data1$submitter_eth = "Hispanic-All"
data2$submitter_eth = "Hispanic-Hispanic"
data3$submitter_eth = "Hispanic-White"
data4 <- rbind(data1, data2, data3)
ggplot(data=data4, aes(x=submitter_eth, y=success_rate, fill=success_rate),inherit.aes = FALSE) +
  geom_violin() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=5, ) + labs(x='Ethnicity', y='Success rate per submitter/integrator pair')



data1 <- white_white
data2 <- black_white
data3 <- api_white
data4 <- hispanic_white
ggplot()  + geom_density(data=data1,
                         aes(x=success_rate, color='White submitter-White integrator')) +
  geom_density(data=data2,
               aes(x=success_rate, color='Black submitter-White integrator')) +
  geom_density(data=data3,
               aes(x=success_rate, color='API submitter-White integrator')) +
  geom_density(data=data4,
                aes(x=success_rate, color='Hispanic submitter-White integrator')) +
  geom_vline(aes(xintercept=mean(data1$success_rate)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(data2$success_rate)),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(data3$success_rate)),
             color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(mean(data4$success_rate))),
             color="black", linetype="dashed", size=1) + xlab("Success Rate") + ylab("Density") +
  scale_color_manual(name = "Perceived ethnicity group",
             values = c('White submitter-White integrator' = "blue", 'Black submitter-White integrator' = "red", "API submitter-White integrator" = "green", 'Hispanic submitter-White integrator'='black'),
              labels = c('White submitter-White integrator', 'Black submitter-White integrator', "API submitter-White integrator", 'Hispanic submitter-White integrator'))



kruskal.test(success_rate~eth, data=data)
dunnTest(success_rate~eth, data=data, method="bonferroni")

mean(data1$success_rate)
mean(data2$success_rate)

median(data1$success_rate)
median(data2$success_rate)
