## This code is written by Chia Liu April 2021 ## 

x<-c('tidyverse','haven','survival','survminer')
lapply(x,require,character.only=T)

## function to turn all negative values to NA 
allna_f<-function(x){
  x[x<0]=NA
  return(x)
}
## Note: Due to SOEP's tendency to over-sample migrants, only non-migrants are used in this part

soep<-read_dta("ppathl.dta") %>% 
  filter(sex==2 & gebjahr %in% c(1952:1972) & migback==1) %>%  
  select(pid,gebjahr,gebmonat,migback,syear,sampreg) %>% 
  group_by(pid) %>% top_n(1, syear) %>% 
  mutate(ew=ifelse(sampreg==2,'East','West'))

birth<-read_dta("biobirth.dta") %>% 
  select(pid,kidgeb01,kidmon01, kidgeb02,kidmon02,kidgeb03,kidmon03,kidgeb04,kidmon04) %>% 
  filter(pid %in% c(soep$pid)) %>% 
  allna_f()

myd<-left_join(birth,soep,by='pid') %>% 
  mutate(minyear=ifelse(syear<1990,syear,1990),
         gebmonat=ifelse(gebmonat<0,sample(1:12,replace=T),gebmonat),
         kid1=ifelse(!is.na(kidgeb01) & is.na(kidgeb02),1,0),
         kid2=ifelse(!is.na(kidgeb02) & is.na(kidgeb03),1,0),
         kid3=ifelse(!is.na(kidgeb03) & is.na(kidgeb04),1,0),
         kid4=ifelse(!is.na(kidgeb04),1,0),
         kidn=case_when(
           kid1==1~1,
           kid2==1~2,
           kid3==1~3,
           kid4==1~3,TRUE~0
         ))
         East & West by number of children

## East & West by number of children
table(myd$kidn,myd$ew) %>% prop.table(2) %>% round(2)

## KM curve to first child by East West 
km1<-myd %>% select(pid,gebjahr,gebmonat,kidgeb01,kidmon01,minyear,ew) %>% 
  mutate(event=ifelse(is.na(kidgeb01) | kidgeb01>1990,0,1),
         kidmon01=ifelse(event==1 & is.na(kidmon01),sample(1:12,replace=T),kidmon01),
         dur=ifelse(event==1,(kidgeb01-gebjahr)*12+kidmon01-9,(minyear-gebjahr)*12+gebmonat))

km1$surv_obj<-Surv(time=km1$dur,event=km1$event)
         
fit<-survfit(surv_obj~ew, data=km1)
attr(fit$strata,'names') <- c('East', 'West')

ggsurvplot(fit,size=0.5, surv.median.line="hv",censor=F,xlim=c(180,420),
           legend.title='',palette=c('red','blue'),break.time.by=60,xscale=12,
           x='Age of woman',title='First child',
           ggtheme = theme_survminer( 
             font.main=c(12),
                      font.x = c(9),
                      font.y = c(9),
             font.caption=c(9),
                      font.tickslab = c()))+
  labs(captions='SOEP, calculation by CL')
  
## KM curve to second child by East West
km2<-myd %>% select(pid,gebjahr,gebmonat,kidgeb01,kidgeb02,kidmon02,minyear,ew) %>% 
  filter(!is.na(kidgeb01)) %>% 
  mutate(flag=ifelse(kidgeb01==kidgeb02 | kidgeb01>1990,1,0),
         event=ifelse(is.na(kidgeb02) | kidgeb02>1990 | flag==1,0,1),
         kidmon02=ifelse(event==1 & is.na(kidmon02),sample(1:12,replace=T),kidmon02),
         dur=ifelse(event==1,((kidgeb02-kidgeb01)*12+kidmon02-9),(minyear-kidgeb01)*12+gebmonat-9))

km2$surv_obj<-Surv(km2$dur,km2$event)

fit2<-survfit(surv_obj~ew,data=km2)
attr(fit2$strata,'names') <- c('East', 'West')

ggsurvplot(fit2,size=0.5, surv.median.line="hv",censor=F, xlim=c(0,120),
           legend.title='',palette=c('red','blue'),break.time.by=24,xscale=12,
           x='Age of first child',title='Second child',
           ggtheme = theme_survminer( 
             font.main = c(12),
             font.x = c(9),
             font.y = c(9),
             font.caption=c(9),
             font.tickslab = c()))+
  labs(captions='SOEP, calculation by CL')
  
  ## KM curve to third child by East West
  
  km3 <-myd %>% select(pid,gebjahr,gebmonat,kidgeb01,kidgeb02,kidgeb03,kidmon03,minyear,ew) %>% 
  filter(!is.na(kidgeb02)) %>% 
  mutate(flag=ifelse(kidgeb02==kidgeb03 | kidgeb02>1990,1,0),
         event=ifelse(is.na(kidgeb03) | kidgeb03>1990 | flag==1,0,1),
         kidmon03=ifelse(event==1 & is.na(kidmon03),sample(1:12,replace=T),kidmon03),
         dur=ifelse(event==1,((kidgeb03-kidgeb02)*12+kidmon03-9),
                    (minyear-kidgeb02)*12+gebmonat-9),
         dur=ifelse(dur<0,1,dur)) %>%
  filter(kidgeb02<1990) 

km3$surv_obj<-Surv(km3$dur,km3$event)

fit3<-survfit(surv_obj~ew,data=km3)
attr(fit3$strata,'names') <- c('East', 'West')

ggsurvplot(fit3,size=0.5, surv.median.line="hv",censor=F, xlim=c(0,120), 
           legend.title='', palette=c('red','blue'),break.time.by=24,xscale=12,  
           x='Age of second child',title='Third child',
           ggtheme = theme_survminer( 
             font.main=c(12),
             font.x = c(9),
             font.y = c(9),
             font.caption = c(9),
             font.tickslab = c()))+
  labs(captions='SOEP, calculation by CL')
  
## Second comparison: Transition to first cohabitation or marriage.  
## Using:Marriage and Cohabitation in western Germany and France. Katja Koppen. (2010)
## https://www.demogr.mpg.de/publications/files/4277_1318519041_1_Full%20Text.pdf

## Note: Migrants and East German observations are excluded from the SOEP sample.

soep<-read_dta("ppathl.dta") %>% 
  filter(gebjahr %in% c(1944:1982) & sampreg==1 & migback==1 & sex==2) %>%
  select(pid,gebjahr,syear) %>% group_by(pid) %>% slice(which.max(syear)) %>% ungroup() %>% 
  mutate(ageg=case_when(
    gebjahr %in% c(1944:1954)~'1944-1954',
    gebjahr %in% c(1955:1964)~'1955-1964',
    gebjahr %in% c(1965:1974)~'1965-1974',
    gebjahr %in% c(1975:1982)~'1975-1982'
  ))

## 7099 out of 17043 have partnership history on record ## 
bioc<-read_dta("biocouply.dta") %>% 
  filter(pid %in% c(soep$pid)) %>% select(pid,begin,spelltyp) %>%
  sapply(as.numeric) %>% as.data.frame() %>% 
  mutate(spelltyp=case_when(
    spelltyp %in% c(1,2)~'married',
    spelltyp == 3~'cohab',
    TRUE~'other'
  ))

## combine, to get unique pid in the biocouply file## 
all<-inner_join(soep,bioc,by='pid') %>% 
  filter(begin>0) %>% group_by(pid) %>% slice(which.min(begin)) %>% select(1:4)##just keep one ob per id
   
bios<-bioc %>% filter(spelltyp %in% c('cohab','married')) %>% group_by(pid) %>% slice(which.min(begin)) %>% 
  mutate(spelltyp=1) %>% rename(union=spelltyp)

all$mvec <- sample(1:12, nrow(all), replace = TRUE) ## create random month vec to smooth out lines

all2<-left_join(all,bios,by='pid') %>% 
  mutate(begin=ifelse(is.na(begin),syear-gebjahr,begin),
         union=ifelse(is.na(union),0,union),
         dur=((begin*12)+mvec)/12)

all2$surv_obj1<-Surv(time=all2$dur,event=all2$union)
fit1<-survfit(surv_obj1~ageg, data=all2) 
names(fit1$strata) <- gsub("ageg=", "", names(fit1$strata))
ggsurvplot(fit1,censor=F,xlim=c(15,40),break.time.by=5,conf.int=F,
           palette=c('black','blue3','cadetblue1','cornflowerblue'),
           ggtheme = theme_bw(base_size = 10),
           surv.median.line=c('hv'),
           legend.title="Birth cohort",title='First union (yearly data)')+
  labs(captions='SOEP, calculation by CL')
  
## transition from cohab to marriage
bio2<-bioc %>% filter(spelltyp %in% c('cohab','married')) %>% group_by(pid,spelltyp) %>% slice(which.min(begin)) %>% 
  pivot_wider(names_from=spelltyp,values_from=begin) %>% filter(cohab<=married | is.na(married)) 

bio3<-bio2 %>% filter(!is.na(cohab))

combo<-left_join(bio3,all,by='pid') %>% 
  mutate(dur=ifelse(is.na(married),syear-gebjahr-cohab,married-cohab),
         dur=ifelse(dur<0,0,dur),
         event=ifelse(is.na(married),0,1)) %>% filter(cohab>=16)

## convert yearly duration to monthly to smooth curves ## 
combo$mvec <- sample(1:12, nrow(combo), replace = TRUE) 
combo$dur<-ifelse(combo$dur==0,combo$mvec,(combo$dur*12)+combo$mvec)

combo$surv_obj1<-Surv(time=combo$dur,event=combo$event)
fit1<-survfit(surv_obj1~ageg, data=combo) 
names(fit1$strata) <- gsub("ageg=", "", names(fit1$strata))
ggsurvplot(fit1,censor=F,xlim=c(0,240),break.time.by=12,xscale=12,conf.int=F,
           palette=c('black','blue3','cadetblue1','cornflowerblue'),
           ggtheme = theme_bw(base_size = 10),
           surv.median.line=c('hv'),
           legend.title="Birth cohort",title='Cohab to marriage (yearly data)')
           
## exclude same year transitions (-cohab/married same year)
combo2<-combo %>% filter(cohab!=married)
combo2$surv_obj1<-Surv(time=combo2$dur,event=combo2$event)
fit1<-survfit(surv_obj1~ageg, data=combo2) 
names(fit1$strata) <- gsub("ageg=", "", names(fit1$strata))
ggsurvplot(fit1,censor=F,xlim=c(0,240),break.time.by=12,xscale=12,conf.int=F,
           palette=c('black','blue3','cadetblue1','cornflowerblue'),
           surv.median.line=c('hv'),
           ggtheme = theme_bw(base_size = 10),
           legend.title="Birth cohort",title='Cohab to marriage, exclude joint transitions (yearly data)')
           
## Reproduce cumulative incidence of cohab/marriage 
bio4<-bioc %>% filter(begin>=16) %>% group_by(pid,spelltyp) %>% slice(which.min(begin)) %>% ungroup() %>% arrange(pid,begin)

bwide<-pivot_wider(bio4,names_from=spelltyp,values_from=begin) %>% mutate_all(~replace(., is.na(.), 9999))

all4<-left_join(all,bwide,by='pid') %>% 
  mutate(cm=ifelse(married==9999 & cohab==9999, 0,
                   ifelse(married<=cohab,1,0)),
         cc=ifelse(married==9999 & cohab==9999, 0,
                   ifelse(cohab<married,1,0)),
         dur=ifelse(cc==1,cohab,
                    ifelse(cm==1,married,
                           syear-gebjahr)))
  
all4$status<-ifelse(all4$cc==1, 1, ## cohab
                       ifelse(all4$cm==1,2, ## married
                            99))

all4$status_f<-factor(all4$status,levels=c(0,1,2),
                         labels=c(0,'cohab','married'))

all4$durcm<-((all4$dur*12)+all4$mvec)/12
  
fitc<-cmprsk::cuminc(all4$durcm,all4$status_f,all4$ageg,rho=0, cencode=0,na.action=na.omit)

trace(survminer:::ggcompetingrisks.cuminc, edit = T) ## add ,nrow = 1, to facet_wrap(~group)  ## 

ggcompetingrisks(fitc,conf.int=T, xlim = c(15,55),ylim=c(0,1),palette=c('dodgerblue4','firebrick4'))+
   labs(title='Competing risk of entrance into cohabitation or marriage', x='Age',caption='Source: SOEP, calculation by CL') +
  scale_x_continuous(breaks=seq(15, 55, 10))+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  theme(axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        axis.title.x=element_text(size=11),
        axis.title.y=element_text(size=11),
        plot.title = element_text(size=12),
        plot.caption=element_text(size=10),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.grid.minor = element_line(colour="gray90", size=0.5))

