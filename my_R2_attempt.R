library(readxl) #подгрузка данных из экселя
library(readr) #подгрузка данных из CSV
library(dplyr) #вычисления, соединения датасетов, описательные статистики
library(tidyr) #вычисления, соединения датасетов, описательные статистики
library(ggplot2) #графички :)
library(lmtest) # регрессия, тестирование гипотез
library(modelsummary) #красивые таблички для summary по статистикам и по результатам регрессий:)
library(psych) #пакет для психологов - но тоже считает описательные статистики
library(sandwich) # ковариация, ст. ошибки
library(ggpubr)
library(wooldridge)
library(tidyverse)
library(sandwich)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(conflicted)
library(corrplot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
conflicts_prefer(dplyr::filter)

spotify <- read.csv('music_data.csv', sep=',')
spotify


barplot(height = spotify$principal_artist_followers, breaks=2000)

plot(spotify$principal_artist_followers, type='h')

barplot(table(spotify$principal_artist_followers), density = T)
?barplot


hist(spotify$principal_artist_followers, density = T, breaks=10,
     prob = TRUE)
lines(density(spotify$principal_artist_followers), col = "red") 
lines(x =spotify$principal_artist_followers, 
      y =density(spotify$principal_artist_followersers), col = "red")   

library(psych)
spotify[spotify$principal_artist_followers == describe(spotify$principal_artist_followers)$max, ]

summary(spotify$principal_artist_followers)

tier4 <- summary(spotify$principal_artist_followers)['Min.'] : summary(spotify$principal_artist_followers)['1st Qu.']
tier3 <- (summary(spotify$principal_artist_followers)['1st Qu.'] + 1) : summary(spotify$principal_artist_followers)['Median']
tier2 <- (summary(spotify$principal_artist_followers)['Median'] + 1) : summary(spotify$principal_artist_followers)['3rd Qu.']
tier1 <- (summary(spotify$principal_artist_followers)['3rd Qu.'] + 1) : summary(spotify$principal_artist_followers)['Max.']

?mutate

spotify1 <- spotify %>%
  mutate(artist_tier = case_when(principal_artist_followers %in% tier1 ~ 1,
                principal_artist_followers %in% tier2 ~ 2,
                principal_artist_followers %in% tier3 ~ 3,
                principal_artist_followers %in% tier4 ~ 4
  ))

hist(spotify1$artist_tier)

barplot(table(spotify1$sound_system, spotify1$popularity))
?barplot

plot(spotify1$sound_system, spotify1$popularity)

barplot(table(spotify1$sound_system))

barplot(table(spotify1$mode))

barplot(table(spotify1[spotify1$mode == 'B', ]$sound_system))
barplot(table(spotify1[spotify1$mode == 'A', ]$sound_system))

spotify1[is.na(spotify1)] = 0
spotify1$explicit <- as.integer(as.logical(spotify1$explicit))

#Добавим переменную по временным периодам 
spotify1 <- spotify1 %>%
  mutate(period = case_when(
    year >= 1996 & year <= 2000 ~ '1',
    year >= 2001 & year <= 2005 ~ '2',
    year >= 2006 & year <= 2010 ~ '3',
    year >= 2011 & year <= 2015 ~ '4',
    year >= 2016 & year <= 2020 ~ '5'
  ))

spotify1 <- spotify1 %>%
  filter(!is.na(period))
head(spotify1)

reg_baseline <- popularity ~ acousticness + explicit + danceability + liveness + loudness + speechiness

reg_basic <- lm(reg_baseline, data = spotify1)

coeftest(reg_basic, df=Inf, type='HC0', vcoc.=vcovHC)
# Summary and F-test
summary(reg_basic)

spotify1$artist_tier <- factor( spotify1$artist_tier )

reg_control1 <- popularity ~ acousticness + explicit + danceability +
   liveness + loudness + speechiness + artist_tier

reg_con1 <- lm(reg_control1, data = spotify1, type='HC0')

# Summary and F-test
summary(reg_con1)

reg_control2 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + period

reg_con2 <- lm(reg_control2, data = spotify1, type='HC0')

# Summary and F-test
summary(reg_con2)


reg_control3 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + artist_tier  + period

reg_con3 <- lm(reg_control3, data = spotify1, type='HC0')

# Summary and F-test
summary(reg_con3, vcov='HC0')
coeftest(reg_con3, df=Inf, type='HC0', vcoc.=vcovHC)

barplot(table(spotify1$available_markets))

reg_control4 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + artist_tier  + period + duration_ms

reg_con4 <- lm(reg_control4, data = spotify1, type='HC0')

# Summary and F-test
coeftest(reg_con4, df=Inf, type='HC0', vcoc.=vcovHC)
summary(reg_con4, robust=T)

?barplot(table(spotify1$available_markets))

table(summary(reg_con4, robust=T))
data.frame(unclass(summary(reg_con4, robust=T)),
           check.names = F)


library(modelsummary)

modelsummary(models=list(reg_basic, reg_con1,
                         reg_con3, reg_con4),
             vcov='HC0',
             statistic = 'std.error',
             stars=T,
             title='Reg_con4')

spotify1$period <- as.factor(spotify1$period)

reg_baseline <- popularity ~ acousticness + explicit + 
                    danceability + liveness + loudness + speechiness
reg_basic <- lm(reg_baseline, data = spotify1)

reg_control1 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + artist_tier
reg_control1 <- lm(reg_control1, data = spotify1)

reg_control2 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + period
reg_control2 <- lm(reg_control2, data = spotify1)

reg_control3 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms + key + mode
reg_control3 <- lm(reg_control3, data = spotify1)

reg_control4 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence
reg_control4 <- lm(reg_control4, data = spotify1)

reg_control5 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + period
reg_control5 <- lm(reg_control5, data = spotify1)

reg_control6 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + artist_tier
reg_control6 <- lm(reg_control6, data = spotify1)

reg_control7 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period
reg_control7 <- lm(reg_control7, data = spotify1)

reg_control8 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo + time_signature +
  valence + artist_tier + period + period * explicit
reg_control8 <- lm(reg_control8, data = spotify1)

modelsummary(models=list(reg_basic, reg_control1,
                         reg_control2, reg_control3,
                         reg_control4, reg_control5,
                         reg_control6, reg_control7,
                         reg_control8),
             vcov='HC0',
             statistic = 'std.error',
             stars=T,
             title='Reg_con4')



#tab_model(lm)

plot(spotify1$acousticness, spotify1$period)

boxplot(spotify1$acousticness ~ spotify1$period)

barplot(table(spotify1$explicit, spotify1$period),
        main='Explicit/Periods',
        xlab='Period Number',
        ylab='Explicit')

boxplot(spotify1$liveness ~ spotify1$period)


models <- c()
coef <- summary(reg_basic, robust=T)$coef
v.coef <- c(t(coef))
names(v.coef) <- paste(rep(rownames(coef), each = 4), c("coef", "stderr", "t", "p-value"))
v.model_info <- c(r.squared = summary(reg_basic, robust=T)$r.squared,
                  F = summary(reg_basic, robust=T)$fstatistic[1], df.res = summary(reg_basic, robust=T)$df[2])

# in case of glm:
#v.model_info <- c(overdisp = summary(m)$dispersion, res.deviance = m$deviance, df.res = m$df.residual, null.deviance = m$null.deviance, df.null = m$df.null)

v.all <- c(v.coef, v.model_info)    
models <- rbind(models, cbind(data.frame(model = paste("model", 1, sep = "")), t(v.all)))
models

install.packages('stargazer')
library(stargazer)
cov <- vcovHC(reg_basic, type = "HC0")
robust.se <- sqrt(diag(cov))
#stargazer(reg_basic, type = "text")
stargazer(reg_basic, se=list(robust.se),
          column.labels=c("robust"),
          align=TRUE, type = 'text',
          omit.stat=c('ser'))

?stargazer
summary(reg_basic, robust=T)
coeftest(reg_basic, df=Inf, type='HC0', vcov=vcovHC)

plot(fitted(reg_basic), resid(reg_basic))

coeftest(reg_basic, vcov = vcovHC(reg_basic, type = 'HC0'), df=Inf)

  
rq.se="iid"
stargazer(reg_basic, se=list(robust.se),
          column.labels=c("robust"),
          align=TRUE, type = 'text',
          rq.se="iid",
          omit.stat=c('ser'))

stargazer(reg_basic, se=list(robust.se),
          column.labels=c("robust"),
          align=TRUE,
          omit.stat=c('ser'))

cov <- vcovHC(reg_basic, type = "HC0")
robust.se <- sqrt(diag(cov))
#stargazer(reg_basic, type = "text")
stargazer(reg_basic, se=list(robust.se),
          column.labels=c("robust"),
          align=TRUE, type = 'text',
          omit.stat=c('ser'))


reg_baseline <- popularity ~ acousticness + explicit + 
  danceability + liveness + loudness + speechiness
reg_basic <- lm(reg_baseline, data = spotify1)
summary(reg_basic, robust=T)
coeftest(reg_basic, df=Inf, type='HC0', vcov=vcovHC)


install.packages('aod')
library(aod)
wald.test(Sigma = vcov(reg_basic ), b = coef(reg_basic), Terms = 1:6)

spotify1$key <- as.factor(spotify1$key)

reg_control4 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period + period * explicit
reg_control4 <- lm(reg_control4, data = spotify1)
summary(reg_control4, robust=T)
coeftest(reg_control4, df=Inf, type='HC0', vcov=vcovHC)
wald.test(Sigma = vcov(reg_control4), b = coef(reg_control4), Terms = 1:6)

barplot(table(spotify1$time_signature))



reg_baseline <- popularity ~ acousticness + explicit + 
  danceability + liveness + loudness + speechiness
reg_basic <- lm(reg_baseline, data = spotify1)

reg_control1 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + artist_tier
reg_control1 <- lm(reg_control1, data = spotify1)

reg_control2 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + period
reg_control2 <- lm(reg_control2, data = spotify1)

reg_control3 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms + key + mode
reg_control3 <- lm(reg_control3, data = spotify1)

reg_control4 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence
reg_control4 <- lm(reg_control4, data = spotify1)

reg_control5 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + period
reg_control5 <- lm(reg_control5, data = spotify1)

reg_control6 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + artist_tier
reg_control6 <- lm(reg_control6, data = spotify1)

reg_control7 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period
reg_control7 <- lm(reg_control7, data = spotify1)

reg_control8 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period + period * explicit
reg_control8 <- lm(reg_control8, data = spotify1)

cov <- vcovHC(reg_basic, type = "HC0")
robust.se <- sqrt(diag(cov))
cov <- vcovHC(reg_control1, type = "HC0")
robust.se1 <- sqrt(diag(cov))
cov <- vcovHC(reg_control2, type = "HC0")
robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(reg_control3, type = "HC0")
robust.se3 <- sqrt(diag(cov))
cov <- vcovHC(reg_control4, type = "HC0")
robust.se4 <- sqrt(diag(cov))
cov <- vcovHC(reg_control5, type = "HC0")
robust.se5 <- sqrt(diag(cov))
cov <- vcovHC(reg_control6, type = "HC0")
robust.se6 <- sqrt(diag(cov))
cov <- vcovHC(reg_control7, type = "HC0")
robust.se7 <- sqrt(diag(cov))
cov <- vcovHC(reg_control8, type = "HC0")
robust.se8 <- sqrt(diag(cov))
#stargazer(reg_basic, type = "text")
stargazer(reg_basic, 
          reg_control1,
          reg_control2,
          reg_control3,
          reg_control4, 
          reg_control5,
          reg_control6,
          reg_control7,
          reg_control8,
          se=list(robust.se, robust.se1, robust.se2,
                  robust.se3, robust.se4, robust.se5,
                  robust.se6, robust.se7, robust.se8),
          column.labels=c("Basic reg",
                          '+ artist tier',
                          '+ period',
                          '+ music ch.',
                          '+ music ch2',
                          'music ch2 + period',
                          'music ch2 + artist tier',
                          'music ch2 + artist tier + period',
                          'cross effect'
                          ),
          align=TRUE, type = 'text',
          omit.stat=c('ser'))

stargazer(reg_basic, 
          reg_control1,
          reg_control2,
          reg_control3,
          reg_control4, 
          reg_control5,
          reg_control6,
          reg_control7,
          reg_control8,
          se=list(robust.se, robust.se1, robust.se2,
                  robust.se3, robust.se4, robust.se5,
                  robust.se6, robust.se7, robust.se8),
           type = 'text')
          
          

          
reg_baseline <- popularity ~ acousticness + explicit + 
  danceability + liveness + loudness + speechiness
reg_basic <- lm(reg_baseline, data = spotify1)

reg_control1 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + artist_tier
reg_control1 <- lm(reg_control1, data = spotify1)

reg_control2 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + period
reg_control2 <- lm(reg_control2, data = spotify1)

reg_control3 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms + key + mode
reg_control3 <- lm(reg_control3, data = spotify1)

reg_control4 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence
reg_control4 <- lm(reg_control4, data = spotify1)

reg_control5 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + period
reg_control5 <- lm(reg_control5, data = spotify1)

reg_control6 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  + valence + artist_tier
reg_control6 <- lm(reg_control6, data = spotify1)

reg_control7 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period
reg_control7 <- lm(reg_control7, data = spotify1)

reg_control8 <- popularity ~ acousticness + explicit + danceability +
  liveness + loudness + speechiness + duration_ms +
  key + mode + tempo  +
  valence + artist_tier + period + period * explicit
reg_control8 <- lm(reg_control8, data = spotify1)

cov <- vcovHC(reg_basic, type = "HC0")
robust.se <- sqrt(diag(cov))
cov <- vcovHC(reg_control1, type = "HC0")
robust.se1 <- sqrt(diag(cov))
cov <- vcovHC(reg_control2, type = "HC0")
robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(reg_control3, type = "HC0")
robust.se3 <- sqrt(diag(cov))
cov <- vcovHC(reg_control4, type = "HC0")
robust.se4 <- sqrt(diag(cov))
cov <- vcovHC(reg_control5, type = "HC0")
robust.se5 <- sqrt(diag(cov))
cov <- vcovHC(reg_control6, type = "HC0")
robust.se6 <- sqrt(diag(cov))
cov <- vcovHC(reg_control7, type = "HC0")
robust.se7 <- sqrt(diag(cov))
cov <- vcovHC(reg_control8, type = "HC0")
robust.se8 <- sqrt(diag(cov))
#stargazer(reg_basic, type = "text")
stargazer(reg_basic, reg_control1, reg_control2, reg_control3, reg_control4, type='text'), 
stargazer(reg_control5, reg_control6, reg_control7, reg_control8)

modelsummary(models=list(reg_basic, 
                         reg_control1,
                         reg_control2,
                         reg_control3,
                         reg_control4, 
                         reg_control5,
                         reg_control6,
                         reg_control7,
                         reg_control8),
             vcov='HC0',
             statistic = 'std.error',
             stars=T,
             title='Reg_con4')
      