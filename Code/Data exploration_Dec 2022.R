#Fish abundance
#Load data
Ngerumekaol_Fish_All<-read.csv(file.choose(),header=T,sep=",")
names(Ngerumekaol_Fish_All)

#Check if abundance data is normally distributed
hist(Ngerumekaol_Fish_All$Total_Biomass)
shapiro.test(Ngerumekaol_Fish_Ab_Feeding$Abundance_Total)
#Not normal


#Make basic figures
#All fish data
require(Rmisc)
require(ggplot2)
Fish_AB<-summarySE(Ngerumekaol_Fish_All, measurevar="Individuals_square_meter", groupvars=c("Year", "Habitat", "Protection"))
Fish1<-ggplot(Fish_AB, aes(x=factor(Year), y=Individuals_square_meter,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Fish density (Individuals/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=Individuals_square_meter-se, ymax=Individuals_square_meter+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Fish1

#Data without the Humpback Red Snapper
Fish_AB<-summarySE(Ngerumekaol_Fish_All, measurevar="SANS_m2", groupvars=c("Year", "Habitat", "Protection"))
Fish2<-ggplot(Fish_AB, aes(x=factor(Year), y=SANS_m2,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=SANS_m2-se, ymax=SANS_m2+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish density without Lutjanus gibbus (Individuals/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Fish2

#Mixed Effects model
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station),data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Abundance_Total~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Abundance_Total~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to see which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

fit3.res = resid(fish_all2)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all2)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all2)
plot(x)

#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish1<-lmer(Total_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

fish2<-lmer(Total_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

fish3<-lmer(Total_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(fish1,fish2,fish3)
library(effects)
plot(allEffects(fish1))
plot(allEffects(fish2))
plot(allEffects(fish3))

#check normality of residuals
fit3.res = resid(fish1)
plot(fit3.res)
abline(0, 0)    

Plot.fit3.Linearity<-plot(resid(fish1))
plot(fish1)
qqmath(fit3)

#Check collinearity of parameters
require(performance)
check_collinearity(fish1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish1)
plot(x)


#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish1, pairwise ~ Year*Protection*Habitat)


##############BIOMASS##############
#Make basic figures
#All fish data
require(Rmisc)
require(ggplot2)
Fish_BIOMASS<-summarySE(Ngerumekaol_Fish_All, measurevar="Grams_square_meter", groupvars=c("Year", "Habitat", "Protection"))
FishBIO<-ggplot(Fish_BIOMASS, aes(x=factor(Year), y=Grams_square_meter,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Grams_square_meter-se,max=Grams_square_meter+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO

#Figure wihtout snapper biomass
Fish_BIOMASS_SANS<-summarySE(Ngerumekaol_Fish_All, measurevar="Biomass_Sans_g_m2", groupvars=c("Year", "Habitat", "Protection"))
FishBIO_SANS<-ggplot(Fish_BIOMASS_SANS, aes(x=factor(Year), y=Biomass_Sans_g_m2,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Biomass_Sans_g_m2-se,max=Biomass_Sans_g_m2+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass without Lutjanus gibbus (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO_SANS


#Mixed Effects model for biomass
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Total_Biomass~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Total_Biomass~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

#Check your model
#Plot residuals

fit3.res = resid(fish_all1)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all1)
plot(x)


#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
Biomass_Sans1<-lmer(Biomass_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

Biomass_Sans2<-lmer(Biomass_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

Biomass_Sans3<-lmer(Biomass_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(Biomass_Sans1,Biomass_Sans2,Biomass_Sans3)
library(effects)
plot(allEffects(Biomass_Sans1))
plot(allEffects(Biomass_Sans2))
plot(allEffects(Biomass_Sans3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(Biomass_Sans1.res, pairwise ~ Year*Protection*Habitat)

#check normality of residuals
Biomass_Sans1.res = resid(Biomass_Sans1)
plot(Biomass_Sans.res)
abline(0, 0)    

#Check collinearity of parameters
require(performance)
check_collinearity(Biomass_Sans1.res)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(Biomass_Sans1.res)
plot(x)