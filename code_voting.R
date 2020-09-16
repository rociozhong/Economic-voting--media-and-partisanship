rm(list=ls())
getwd()
setwd("/Users/rociozhong/Library/Mobile Documents/com~apple~CloudDocs/PS549_2015/final_paper/voting")

data_2 <- read.csv("final_dta.csv", header = T, sep = ",")
dta<- read.table("Economic.txt", header=T, sep="\t")
names(dta)

# call data column names without using "$"
attach(dta)

# omit the "NA" in the whole dataset
newdta<- dta[complete.cases(dta),]
#finaldta<- as.matrix(sapply(newdta[,-c(1:3)], as.numeric)) 
#table_1<- cor(finaldta)

## graph of 4 economic indices ##
library(ggplot2)
library(scales)
newdta$datem <- as.Date(data_2$datem)
data_2$datem = as.Date(data_2$datem)

plot_econ <- ggplot(data = data_2, aes(x = datem)) + 
  geom_line(aes(y = hp_cbcoinus, colour = "Coincident")) + 
  geom_line(aes(y = hp_cblagus, colour = "Lagging")) + 
  geom_line(aes(y = hp_cbleadus, colour = "Leading")) + 
  geom_line(aes(y = hp_cb_Leadrevised, colour = "Leading_revised")) +
  labs(x = "Year", y = "Conference Board Indicators") + 
  scale_x_date(labels = date_format("%m-%Y")) 

## graph of tone ##                    
plot_tone1 <- ggplot(data = newdta, aes(x = datem, y = tone1)) +
  geom_line() + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "Mean Monthly article tone (wordcount)")
 
plot_tone3 <- ggplot(data = newdta, aes(x = datem, y = tone3)) +
  geom_line() + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "Mean Monthly article tone (coefficient of imbalance)")

## economic versus partisanship ##
fit_coincidient <- lm(hp_cbcoinus ~ party_id, data = data_2)
summary(fit_coincidient)

fit_lag <- lm(hp_cblagus ~ party_id, data = data_2)
summary(fit_lag)

fit_leading <- lm(hp_cb_Lead ~ party_id, data = data_2)
summary(fit_leading)

fit_lead_re <- lm(hp_cb_Leadrevised ~ party_id, data = data_2)
summary(fit_lead_re) ## not significant ##

fit_unemploy <- lm(unemB ~ party_id, data = data_2)
summary(fit_unemploy) ## positive correlation between unemployment rate and democrat ##

## tables ##
library(stargazer)
stargazer(fit_coincidient, fit_lag, fit_lead_re, fit_unemploy, title = "results")

## Given partisanship and economic performance, the media response ##
fit_par_lag <- lm(tone1 ~ hp_cblagus + unemB + party_id, data = data_2)
summary(fit_par_lag) ## Rep has 0.23024 less tone than Dem ##
## the higher the tone, the better response media has##

## interaction ##
fit_lagparty <- lm(tone1 ~ hp_cblagus* party_id, data = data_2)
summary(fit_lagparty)

fit_coinparty <- lm(tone1 ~ hp_cbcoinus * party_id, data = data_2)
summary(fit_coinparty)

fit3_lagparty <- lm(tone3 ~ hp_cblagus* party_id, data = data_2)
summary(fit3_lagparty)

############
fit_par_coin <- lm(tone1 ~ hp_cbcoinus + unemB + party_id, data= data_2)
summary(fit_par_coin) ## Rep has 0.23053 less tone than Dem ##

## making table ##
stargazer(fit_lagparty, fit_coinparty, rec_lagparty, rec_coinparty,
          title = "results", 
          dep.var.labels= c("Tone", "Recession"), 
          covariate.labels=c("Lag","Coin","Republican", "Interaction: Rep*Lag/Coin"), 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)
          
##  recession count versus party id ##
rec_par_lag <- lm(recession ~ hp_cblagus + unemB + party_id, data = data_2)
summary(rec_par_lag) ## recession word count -- Rep has 64.632 more than Dem ##

## interaction of econ and party ##
rec_lagparty <- lm(recession ~ hp_cblagus*party_id, data = data_2)
summary(rec_lagparty)

rec_coinparty <- lm(recession ~ hp_cbcoinus*party_id, data = data_2)
summary(rec_coinparty)

rec_par_coin <- lm(recession ~ hp_cbcoinus + unemB + party_id, data = data_2)
summary(rec_par_coin) ## Rep has 61.433 recession words than Dem ##

## general negative versus party id ##
neg_par_lag <- lm(generalnegative ~ hp_cblagus + unemB + party_id, data = data_2)
summary(neg_par_lag) ## Rep has 108.150 more than Dem regarding general negative ##

neg_par_coin <- lm(generalnegative ~ hp_cbcoinus + unemB + party_id, data = data_2)
summary(neg_par_coin) ## Rep has 103.878 more than Dem ##

poly_neg <- lm(generalnegative ~ hp_cbcoinus + I(hp_cbcoinus^2) + unemB + party_id, data = data_2)
summary(poly_neg) ## Rep has 98.730 more than Dem ##


## making tables ##

stargazer(neg_par_lag, neg_par_coin, poly_neg, title = "results",
          dep.var.labels= "General Negative", 
          covariate.labels=c("Lag","Coin","Coin^2","Unemployment", "Republican"), 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)
        
## check for high-leverage points ##



## negative versus economic performance for two parties ##
plot_neg <- ggplot(data = data_2, aes(x = hp_cbcoinus, y = generalnegative, color = factor(party_id))) + 
  stat_smooth(method = "lm", size = 1) + geom_point() +
  labs(x = "Coincident Indices", y = "General Negative from Media") 
  

## comparison of figure (party economic performance) and the figure (party negative review) ##
#data_2$datem <- as.Date(data_2$datem)
data_2$colors <- ((data_2$party_id == 'Republican') + 1)
fig1<- ggplot(data_2, aes(x = datem, y = hp_cbcoinus, group=1,color= colors)) + 
  geom_line(color = "red") + scale_x_date(labels = date_format("%m-%Y")) + theme_bw() +
  labs(x = "Year") + scale_colour_continuous(guide = FALSE) +
  theme(axis.title.y=element_blank())

fig3<- ggplot(data_2, aes(x = datem, y = hp_cb_Leadrevised)) + 
  geom_line(colour = "blue") + scale_x_date(labels = date_format("%m-%Y")) + theme_bw() +
  labs(x = "Year") + scale_colour_continuous(guide = FALSE) +
  theme(axis.title.y=element_blank())

fig2<- ggplot(data_2, aes(x = datem, y = generalnegative, group = 1, color = colors)) + 
 geom_line() + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "Media Tone") + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA)) + scale_colour_continuous(guide = FALSE) 

fig4<- ggplot(data_2, aes(x = datem, y = count)) + 
  geom_line(colour = "red") + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "General Negative from Media") + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA)) + scale_colour_continuous(guide = FALSE) +
  theme(axis.title.y=element_blank())

fig5 <- ggplot(data_2, aes(x = datem, y = socretro)) +
  geom_line(colour = "blue") + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "Retros Evaluation") + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA)) + scale_colour_continuous(guide = FALSE) 

## dual axis ##
library(gtable)
library(grid)

## extract gtable ##
g1 <- ggplot_gtable(ggplot_build(fig1))
g2 <- ggplot_gtable(ggplot_build(fig2))

## overlap the panel of 2nd plot on that of 1st plot ##
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)
## axis tweaks ##
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

## graph of consumers' sentiment within income terciles##
plot_senti <- ggplot(data_2, aes(x = datem)) + 
  geom_line(aes(y = socretro, colour = "Public Evaluation" )) + 
  geom_line(aes(y = Lower.Third, colour = "lower income")) +
  geom_line(aes(y = Middle.Third, colour = "middle income")) +
  geom_line(aes(y = Upper.Third, colour = "upper income")) +
  scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Year", y = "Public Evaluation")

## overlap of tone and sentiment ##



## social class sentiment, employment and partisanship ##
## lower class are much more sensitive to umemployment rate ##
## lower and middle class are likely to support democratic compared to upper class ##
## umempolyment versus party ##
up_em <- lm(Upper.Third ~ party_id * unemB, data = data_2)
summary(up_em)

mi_em <- lm(Middle.Third ~ party_id *unemB , data = data_2)
summary(mi_em)

low_em <- lm(Lower.Third ~ party_id *unemB , data = data_2)
summary(low_em)

## sentiment versus  partisanship ##
fit_senti_par <- lm(socretro ~ party_id, data = data_2)
summary(fit_senti_par)

fit_high_par <- lm(Upper.Third ~ party_id * hp_cb_Leadrevised, data = data_2)

summary(fit_high_par)

fit_mid_par <- lm(Middle.Third ~ party_id *hp_cb_Leadrevised , data = data_2)
summary(fit_mid_par)

fit_low_par <- lm(Lower.Third ~ party_id *hp_cb_Leadrevised , data = data_2)
summary(fit_low_par)

## sentiment versus media and party ##
up_par_media <- lm(Upper.Third ~ party_id * recession + unemB * party_id, data = data_2)
summary(up_par_media)

mi_par_media <- lm(Middle.Third ~ party_id * recession + unemB * party_id, data = data_2)
summary(mi_par_media)

low_par_media <- lm(Lower.Third ~ party_id * recession + unemB * party_id, data = data_2)
summary(low_par_media)


## making tables ##
stargazer(fit_high_par, fit_mid_par, fit_low_par, 
          title = "Public Opinion by Social Class to the Economy and Partisanship",
          dep.var.labels= c("Upper Class", "Middle Class", "Lower Class"), 
          covariate.labels=c("Republican", "Lead", "Interaction: Rep* Lead"), 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

######### making tables ##############
install.packages("stargazer")
library(stargazer)
stargazer(data_2)
stargazer(coll)


########## non-stationary ###########
## Plots the ACF and PACF of economic indicators and media for lag 1-19 ##
lagging <- ts(data_2$hp_cblagus, start = c(1980), frequency = 12)
coincident <- ts(data_2$hp_cbcoinus, start = c(1980), frequency = 12)
leading <- ts(data_2$hp_cb_Leadrevised, start = c(1980), frequency = 12)
med_tone <- ts(data_2$tone1, start = c(1980), frequency = 12)
med_volume <- ts(data_2$count, start = c(1980), frequency = 12)
socretro <- ts(data_2$socretro, start = c(1980), frequency = 12)
socprosp <- ts(data_2$socprosp, start = c(1980), frequency = 12)

## ACF of all series decreases slowly ##
library(astsa)
invisible(acf2(lagging, xlim=c(1,19)))
invisible(acf2(coincident, xlim=c(1,19)))
invisible(acf2(leading, xlim=c(1,19)))
invisible(acf2(med_tone, xlim=c(1,19)))
invisible(acf2(med_volume, xlim=c(1,19)))
invisible(acf2(socretro, xlim=c(1,19)))
invisible(acf2(socprosp, xlim=c(1,19))) ## lag(1) ##


## ADF Test ##
library(urca)
df_lagging <- ur.df(lagging, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_lagging)
## reject the null hypothesis and there is no unit root ##

df_coincident <- ur.df(coincident, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_coincident)
## reject the null hypothesis and there is no unit root ##

df_leading <- ur.df(leading, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_leading)
## reject the null ##

df_tone <- ur.df(med_tone, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_tone)
## reject the null ##

df_volume <- ur.df(med_volume, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_volume)
## fail to reject the null, gamma = 1, random walk non-stationary ##

df_socretro <- ur.df(socretro, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_socretro)
## reject the null ##

df_socprosp <- ur.df(socprosp, type = "none", lags = 2) ## delta y(t) = gamma * y(t-1) + delta y(t-1) + e(t) ##
summary(df_socprosp)
## reject the null ##

############## cointegration: Engle- Granger test ##############
## lagging and tone are not cointegrated ##
Engle <- lm(socretro ~ leading)
summary(Engle)

## obtain the residuals ##
residual <- resid(Engle)

## plot the residual along time ##
ts.plot(residual)


library(tseries)
adf.test(residual, k = 1)
# socretro ~ leading: cointegration at 0.05, but pvalue  = 0.04782 too close to 0.05, 
# possible no cointegration #
# socretro ~ coincident: no cointegration #
# socretro ~ lagging: no cointegration #
# socprosp ~ lagging: cointegration at 0.05 level#
# socprosp ~ leading: cointegration at 0.05 level #
# socprosp ~ coincident: cointegration at 0.05 level #
# socprosp ~ tone: cointegration at 0.05 level #
# socretro ~ tone: cointegration at 0.05 level #
# tone ~ leading: cointegration at 0.05 level #
# tone ~ lagging: cointegraiton at 0.05 level #
# tone ~ coincident: cointegration at 0.05 level #
# leading ~ tone: no cointegration #
# coincident ~ tone: no cointegration #
# lagging ~ tone: p-value is large than 0.05, we accept the null and 
# reject the alternative, that is, no cointegrated #
## unit root test of residuals ##
# df_residual <- ur.df(residual, type = "none", lags = 1)
# summary(df_residual)


############## ECMS ###############
#order data by year, month
install.packages("zoo")
library(zoo)
mydata <- zoo(data_2[order(data_2$year, data_2$month), ])
count_lag1 <- as.numeric(lag(mydata$count, -1, na.pad = TRUE))
tone1_lag1 <- as.numeric(lag(mydata$tone1, -1, na.pad = TRUE))
tone3_lag1 <- as.numeric(lag(mydata$tone3, -1, na.pad = TRUE))
lag_lag1 <- as.numeric(lag(mydata$hp_cblagus, -1, na.pad = TRUE))
coin_lag1 <- as.numeric(lag(mydata$hp_cbcoinus, -1, na.pad = TRUE))
lead_lag1 <- as.numeric(lag(mydata$hp_cbleadus, -1, na.pad = TRUE))
unem_lag1 <- as.numeric(lag(mydata$unemB, -1, na.pad = TRUE))

## revised lead indice ##
re_lead_lag1 <- as.numeric(lag(mydata$hp_cb_Leadrevised, -1, na.pad = TRUE))
retro_lag1 <- as.numeric(lag(mydata$socretro, -1, na.pad = TRUE))
prosp_lag1 <- as.numeric(lag(mydata$socprosp, -1, na.pad = TRUE))
up_lag1 <- as.numeric(lag(mydata$Upper.Third, -1, na.pad = TRUE))
mi_lag1 <- as.numeric(lag(mydata$Middle.Third, -1, na.pad = TRUE))
low_lag1 <- as.numeric(lag(mydata$Lower.Third, -1, na.pad = TRUE))

count_diff1 <- as.numeric(mydata$count) - count_lag1
tone1_diff1 <- as.numeric(mydata$tone1) - tone1_lag1
tone3_diff1 <- as.numeric(mydata$tone3) - tone3_lag1
lag_diff1 <- as.numeric(mydata$hp_cblagus) - lag_lag1
coin_diff1 <- as.numeric(mydata$hp_cbcoinus) - coin_lag1
lead_diff1 <- as.numeric(mydata$hp_cbleadus) - lead_lag1
unem_diff1 <- as.numeric(mydata$unemB) - unem_lag1

## revised lead first different ##
re_lead_diff1 <- as.numeric(mydata$hp_cb_Leadrevised) - re_lead_lag1
retro_diff1 <- as.numeric(mydata$socretro) - retro_lag1
prosp_diff1 <- as.numeric(mydata$socprosp) - prosp_lag1
up_diff1 <- as.numeric(mydata$Upper.Third) - up_lag1
mi_diff1 <- as.numeric(mydata$Middle.Third) - mi_lag1
low_diff1 <- as.numeric(mydata$Lower.Third) - low_lag1
additional <- data.frame(cbind(count_lag1, tone1_lag1, tone3_lag1, lag_lag1, coin_lag1, lead_lag1, re_lead_lag1,
                               retro_lag1, prosp_lag1, count_diff1, tone1_diff1, tone3_diff1, lag_diff1, coin_diff1, 
                               lead_diff1, re_lead_diff1, retro_diff1, prosp_diff1, up_lag1, mi_lag1, low_lag1,
                               up_diff1, mi_diff1, low_diff1, unem_lag1, unem_diff1))
mydata1 <- lapply(cbind(data.frame(mydata), additional)[-1,], as.numeric)

## soroka table 2 column 1 ##
fit_count_lag <- lm(count_diff1 ~ count_lag1 + lag_diff1 + lag_lag1, data = mydata1)
summary(fit_count_lag)

## add party_id into column 1##
fit_count_lagparty <- lm(count_diff1 ~ count_lag1 + lag_diff1 + lag_lag1 + factor(party_id), data = mydata1)
summary(fit_count_lagparty)

## soroka table 2 column 2 ##
fit_count_coin <- lm(count_diff1 ~ count_lag1 + coin_diff1 + coin_lag1, data = mydata1)
summary(fit_count_coin)

## add party_id  in col2 ##
fit_count_coinparty <- lm(count_diff1 ~ count_lag1 + coin_diff1 + coin_lag1 + factor(party_id), data = mydata1)
summary(fit_count_coinparty)

## soroka table 2 column 3 ##
fit_count_lead <- lm(count_diff1 ~ count_lag1 + lead_diff1 + lead_lag1, data = mydata1)
summary(fit_count_lead)

## soroka table 2 column 3, revised lead ##
fit_count_relead <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_count_relead)

## add party_id in col3 revised ##
fit_count_releadparty <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1 + factor(party_id), data = mydata1)
summary(fit_count_releadparty)

## table 2 column 4 - 6 ##
fit_tone1_lag <- lm(tone1_diff1 ~ tone1_lag1 + lag_diff1 + lag_lag1, data = mydata1)
summary(fit_tone1_lag)

## add party_id in col4 ##
fit_tone1_lagparty <- lm(tone1_diff1 ~ tone1_lag1 + lag_diff1 + lag_lag1 + factor(party_id), data = mydata1)
summary(fit_tone1_lagparty)

fit_tone1_coin <- lm(tone1_diff1 ~ tone1_lag1 + coin_diff1 + coin_lag1, data = mydata1)
summary(fit_tone1_coin)

## add party in col5 ##
fit_tone1_coinparty <- lm(tone1_diff1 ~ tone1_lag1 + coin_diff1 + coin_lag1 + factor(party_id), data = mydata1)
summary(fit_tone1_coinparty)

fit_tone1_relead <- lm(tone1_diff1 ~ tone1_lag1 + re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_tone1_relead)

## add party into col6 ##
fit_tone1_releadparty <- lm(tone1_diff1 ~ tone1_lag1 + re_lead_diff1 + re_lead_lag1 + factor(party_id), data = mydata1)
summary(fit_tone1_releadparty)

## table 3 column 1 -3 ##
fit_count_lagcoin <- lm(count_diff1 ~ count_lag1 + lag_diff1 + lag_lag1 + coin_diff1 + coin_lag1, data = mydata1)
summary(fit_count_lagcoin)
fit_count_coinrelead <- lm(count_diff1 ~ count_lag1 + coin_diff1 + coin_lag1 + 
                             re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_count_coinrelead)

fit_count_lagcoinrelead <- lm(count_diff1 ~ count_lag1 + lag_diff1 + lag_lag1 + coin_diff1 + coin_lag1 + 
                             re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_count_lagcoinrelead)

## table 3 column 4 - 6 ##
fit_tone1_lagcoin <- lm(tone1_diff1 ~ tone1_lag1 + lag_diff1 + lag_lag1 + coin_diff1 + coin_lag1, data = mydata1)
summary(fit_tone1_lagcoin)

fit_tone1_coinrelead <- lm(tone1_diff1 ~ tone1_lag1 + coin_diff1 + coin_lag1 + 
                                re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_tone1_coinrelead)

fit_tone1_lagcoinrelead <- lm(tone1_diff1 ~ tone1_lag1 + lag_diff1 + lag_lag1 + coin_diff1 + coin_lag1 + 
                                re_lead_diff1 + re_lead_lag1, data = mydata1)
summary(fit_tone1_lagcoinrelead)

## table 5 
#library(vars)
#library(astsa)
#var_retro <- data.frame(mydata1$socretro, mydata1$hp_cbleadus, mydata1$count, mydata1$tone1)
#summary(VAR(var_retro, p = 1, type = "both"))


## table 6 column 1&2 ##
# fit_count_allecon <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1 + 
#                           retro_diff1 + retro_lag1 + prosp_diff1 + prosp_lag1, data = mydata1)

fit_count_allecon <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1 + 
                          retro_diff1 + retro_lag1, data = mydata1)
summary(fit_count_allecon)

fit_tone1_allecon <- lm(tone1_diff1 ~ tone1_lag1 + re_lead_diff1 + re_lead_lag1 + 
                          retro_diff1 + retro_lag1 + prosp_diff1 + prosp_lag1, data = mydata1)
summary(fit_tone1_allecon)


## add party_id into table 6 col 1&2 ##
# fit_count_alleconparty <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1 + 
#                           retro_diff1 + retro_lag1 + prosp_diff1 + prosp_lag1 + factor(party_id), data = mydata1)

fit_count_alleconparty <- lm(count_diff1 ~ count_lag1 + re_lead_diff1 + re_lead_lag1 + 
                               retro_diff1 + retro_lag1 + factor(party_id), data = mydata1)
summary(fit_count_alleconparty)

fit_tone1_alleconparty <- lm(tone1_diff1 ~ tone1_lag1 + re_lead_diff1 + re_lead_lag1 + 
                          retro_diff1 + retro_lag1 + prosp_diff1 + prosp_lag1 + factor(party_id), data = mydata1)
summary(fit_tone1_alleconparty)


## table 7 column 1-2 ##
mydata1$int_count_tone1lag1 <- mydata1$count * mydata1$tone1_lag1
fit_retro_wo_int <- lm(retro_diff1 ~ retro_lag1 + re_lead_diff1 + re_lead_lag1 + count_diff1 + count_lag1 + 
                         tone1_diff1 + tone1_lag1 , data = mydata1)
summary(fit_retro_wo_int)

fit_retro_wt_int <- lm(retro_diff1 ~ retro_lag1 + re_lead_diff1 + re_lead_lag1 + count_diff1 + count_lag1 + 
                         tone1_diff1 + tone1_lag1 + int_count_tone1lag1, data = mydata1)
summary(fit_retro_wt_int)

## change table 7 DV (col 1&2) into different social class evaluation versus media; and add party_id ##
fit_retro_wo_int_up <- lm(up_diff1 ~ up_lag1 + re_lead_diff1 + re_lead_lag1 +
                         tone1_diff1 + tone1_lag1  , data = mydata1)
summary(fit_retro_wo_int_up)

fit_retro_wo_int_mi <- lm(mi_diff1 ~ mi_lag1 + re_lead_diff1 + re_lead_lag1 + 
                                  tone1_diff1 + tone1_lag1 , data = mydata1)
summary(fit_retro_wo_int_mi)

fit_retro_wo_int_low <- lm(low_diff1 ~ low_lag1 + re_lead_diff1 + re_lead_lag1 + 
                                  tone1_diff1 + tone1_lag1, data = mydata1)
summary(fit_retro_wo_int_low)


## table 7 column 3-4 ##
fit_pro_wo_int <- lm(prosp_diff1 ~ prosp_lag1 + re_lead_diff1 + re_lead_lag1 + count_diff1 + count_lag1 + 
                         tone1_diff1 + tone1_lag1, data = mydata1)
summary(fit_pro_wo_int)

fit_pro_wt_int <- lm(prosp_diff1 ~ prosp_lag1 + re_lead_diff1 + re_lead_lag1 + count_diff1 + count_lag1 + 
                       tone1_diff1 + tone1_lag1 + int_count_tone1lag1, data = mydata1)
summary(fit_pro_wt_int)

## social class versus party by controlliing lag ##
up_party_econ<- lm(up_diff1 ~ up_lag1 + unem_diff1 + unem_lag1 +factor(party_id), data = mydata1)
summary(up_party_econ)

mi_party_econ<- lm(mi_diff1 ~ mi_lag1 +  unem_diff1 + unem_lag1 +factor(party_id), data = mydata1)
summary(mi_party_econ)

low_party_econ<- lm(low_diff1 ~ low_lag1 + unem_diff1 + unem_lag1 +factor(party_id), data = mydata1)
summary(low_party_econ)

## making table ##
stargazer(up_party_econ, mi_party_econ, low_party_econ, 
          title="Social Class Opinion to Unemployment Rate and Partisanship",
          dep.var.labels="Change in Opinion(t)", 
          covariate.labels=c("DV(t-1)","Change in Unemployment","Unemployment(t-1)"), 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

######### making tables in latex ############

## soroka table 2 ##
stargazer(fit_count_lag, fit_count_coin, fit_count_relead, 
          title="Responsiveness of Media to Lagging, Coincident, and Leading Indicators",
          dep.var.labels="Change in Count(t)", 
          covariate.labels=c("DV(t-1)","Change in lag","Lag(t-1)", "Change in Coin", "Coin(t-1)",
                             "Change in lead", "Lead(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)
                                                                  
stargazer(fit_tone1_lag, fit_tone1_coin, fit_tone1_relead,
          title="Responsiveness of Media(tone) to Lagging, Coincident, and Leading Indicators",
          dep.var.labels="Change in Tone1(t)",
          covariate.labels=c("DV(t-1)","Change in lag","Lag(t-1)", "Change in Coin", "Coin(t-1)",
                             "Change in lead", "Lead(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

stargazer(fit_tone1_lagparty, fit_tone1_coinparty, fit_tone1_releadparty,
          title="Responsiveness of Media(tone) to Lagging, Coincident, and Leading Indicators",
          dep.var.labels="Change in Tone1(t)",
          covariate.labels=c("DV(t-1)","Change in lag","Lag(t-1)", "Change in Coin", "Coin(t-1)",
                             "Change in lead", "Lead(t-1)", "Republican"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)


stargazer(fit_count_lagcoin, fit_count_coinrelead, fit_count_lagcoinrelead, 
          title="Responsiveness of Media (volume) to Lagging, Coincident, and Leading Indicators",
          dep.var.labels="Change in Count(t)", 
          covariate.labels=c("DV(t-1)","Change in lag","Lag(t-1)", "Change in Coin", "Coin(t-1)",
                             "Change in lead", "Lead(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

stargazer(fit_tone1_lagcoin, fit_tone1_coinrelead, fit_tone1_lagcoinrelead, 
          title="Responsiveness of Media (tone) to Lagging, Coincident, and Leading Indicators",
          dep.var.labels="Change in Tone1(t)", 
          covariate.labels=c("DV(t-1)","Change in lag","Lag(t-1)", "Change in Coin", "Coin(t-1)",
                             "Change in lead", "Lead(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

stargazer(fit_count_allecon, fit_tone1_allecon,
          title="Responsiveness of Media Coverage to Economic Evaluation and the Economy",
          dep.var.labels=c("Change in Count", "Change in Tone"), 
          covariate.labels=c("DV(t-1)","Change in Lead","Lead(t-1)", "Change in Retros Evaluation", "Retros(t-1)",
                             "Change in Prosp Evaluation", "Prosp(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

stargazer(fit_retro_wo_int, fit_retro_wt_int,
          title="Responsiveness of Economic Evaluation to Media",
          dep.var.labels= "Change in Retros Evaluation", 
          covariate.labels=c("DV(t-1)","Change in Lead","Lead(t-1)", "Change in Count", "Count(t-1)",
                             "Change in Tone1", "Tone1(t-1)", "Count*Tone(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

stargazer(fit_pro_wo_int, fit_pro_wt_int,
          title="Responsiveness of Economic Evaluation to Media",
          dep.var.labels= "Change in Prospective Evaluation", 
          covariate.labels=c("DV(t-1)","Change in Lead","Lead(t-1)", "Change in Count", "Count(t-1)",
                             "Change in Tone1", "Tone1(t-1)", "Count*Tone(t-1)"), omit.stat = c("ser", "adj.rsq"), no.space=TRUE)


####### household income and presidential partisanship ######
rm(list=ls())
getwd()
setwd("/Users/Rocio/Library/Mobile Documents/com~apple~CloudDocs/PS549_2015/final_paper/voting")


#install.packages("gsubfn")
#library(gsubfn)
#install.packages("dplyr")
library(dplyr)
library(plyr)
library(ggplot2)

income <- read.csv("household_income_us.csv", header = TRUE,  sep=",", colClasses="character")
income[, c(1:12)] <- lapply(income[, c(1:12)], function(income){as.numeric(gsub(",", "", income))})
party_2 <- read.csv("party_2.csv", header = TRUE, sep = ",")
party_2 <- party_2[, -c(1:2, 4:7)]
household <- cbind(income, party_2)
household <- household[complete.cases(household), ]


#growth <- ddply(household, "party_2", transform, growth_20p = c(NA, exp(diff(log(lowest)))-1),
#      growth_40p = c(NA, (exp(diff(log(second)))-1)*100), growth_60p = c(NA, (exp(diff(log(third)))-1)*100),
#      growth_80p = c(NA, (exp(diff(log(fourth)))-1)*100), growth_95p = c(NA, (exp(diff(log(lowlim_t5)))-1)*100))
#growth <- ddply(household, "party_2", summarise, n = length(growth_20p), mean = mean(growth_20p), 

#sd = sd(growth_20p), se = sd/sqrt(n))
gro_dem <- filter(household, party_2 == "Democrat")
gro_rep <- filter(household, party_2 == "Republican")
by_party <- group_by(household, party_2)
df_gro <- household %>% group_by(party_2) %>% summarise_each(funs(mean))
df_gro[, c(1, 9:13)]
df_gro[1, c(9:13)] - df_gro[2, c(9:13)]

ave_dem <- as.numeric(df_gro[1, c(9:13)])
ave_rep <- as.numeric(df_gro[2, c(9:13)])
percentile <- c(20, 40, 60, 80, 95)
ave_dem_name <- "Democrat"
ave_rep_name <- "Republican"
percentile_name <- "Percentile"

df_income <- data.frame(ave_dem, ave_rep, percentile)
names(df_income) <- c(ave_dem_name, ave_rep_name, percentile_name)

income_compar <- ggplot(data = df_income, aes(x = Percentile)) + geom_line(aes(y = Democrat, colour= "Democrat")) +
  geom_line(aes(y = Republican, colour = "Republican"))

f_dem <- ggplot(data = gro_dem, aes(x = year)) + 
  geom_smooth(aes(y = growth_20p, colour = "20th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_40p, colour = "40th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_60p, colour = "60th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_80p, colour = "80th percentile"), se = FALSE) +
  geom_vline(xintercept = 1977) + geom_vline(xintercept = 1980) + geom_vline(xintercept = 1993) +
  geom_vline(xintercept = 2000) + geom_vline(xintercept = 2009) + 
  annotate("rect", xmin = 1977, xmax = 1980, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 1993, xmax = 2000, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 2009, xmax = 2014, ymin = -Inf, ymax = Inf, alpha = .2)

f_rep <- ggplot(data = gro_rep, aes(x = year)) + 
  geom_smooth(aes(y = growth_20p, colour = "20th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_40p, colour = "40th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_60p, colour = "60th percentile"), se = FALSE) + 
  geom_smooth(aes(y = growth_80p, colour = "80th percentile"), se = FALSE) + 
  geom_vline(xintercept = 1970) + geom_vline(xintercept = 1976) + geom_vline(xintercept = 1981) +
  geom_vline(xintercept = 1992) + geom_vline(xintercept = 2001) + geom_vline(xintercept = 2008) +
  annotate("rect", xmin = 1970, xmax = 1976, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 1981, xmax = 1992, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 2001, xmax = 2008, ymin = -Inf, ymax = Inf, alpha = .2)

