rm(list=ls())
setwd("C:/Users/USER/Desktop/school/TS_data_analysis")
game = read.csv("SkillCraft1_Dataset.csv")
head(game)
library(dplyr)
names(game)
dim(game)

#등급에대한 전체변수들의 평균변화량 Plot 
x11()
par(mfrow = c(4,5))
game1 = game[game$Age != "?",]
game1$Age = as.numeric(as.character(game1$Age))
Age_a = aggregate(formula = Age ~ LeagueIndex, data = game1,
          FUN = mean,na.rm = F, trim = 0.05) #trim 양옆5%씩 총 10% 빼고
Age_a
table(game1$Age)
plot(Age_a,xlab = "LeagueIndex",type="b",pch=19,ylab="Age(16-44)",main ="decreasing",col = "red")#감소

game1 = game[game$HoursPerWeek != "?",]
game1$HoursPerWeek = as.numeric(as.character(game1$HoursPerWeek))
HoursPerWeek_a = aggregate(formula = HoursPerWeek ~ LeagueIndex, data = game1,
              FUN = mean,na.rm = F, trim = 0.05) #trim 양옆5%씩 총 10% 빼고
HoursPerWeek_a
table(game1$HoursPerWeek)
plot(HoursPerWeek_a,xlab = "LeagueIndex",type="b",pch=19,ylab="HoursPerWeek(0-168)",main ="increasing")#지수분포처럼 기하급수적으로 증가

game1 = game[game$TotalHours != "?",]
game1$TotalHours = as.numeric(as.character(game1$TotalHours))
TotalHours_a = aggregate(formula = TotalHours ~ LeagueIndex, data = game1,
              FUN = mean,na.rm = F, trim = 0.05) #trim 양옆5%씩 총 10% 빼고
TotalHours_a
table(game1$TotalHours)
plot(TotalHours_a,xlab = "LeagueIndex",type="b",pch=19,ylab="TotalHours(3-1000000)",main ="increasing") #증가 7이 outlier

b1 = paste("APM ~ LeagueIndex")
game$APM = as.numeric(as.character(game$APM))
APM_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
APM_a
game$APM[which.min(game$APM)]
game$APM[which.max(game$APM)]
plot(APM_a,xlab = "LeagueIndex",type="b",pch=19,ylab="APM(22-390)",main ="increasing") #지수분포 증가8이 outlier

b1 = paste("SelectByHotkeys ~ LeagueIndex")
game$SelectByHotkeys = as.numeric(as.character(game$SelectByHotkeys))*85.5*60 #1min당 핫키 사용
SelectByHotkeys_a = aggregate(formula = as.formula(b1), data = game,
          FUN = mean, na.rm = F,trim = 0.05)
SelectByHotkeys_a
table(game$SelectByHotkeys)
game$SelectByHotkeys[which.min(game$SelectByHotkeys)]
game$SelectByHotkeys[which.max(game$SelectByHotkeys)]
plot(SelectByHotkeys_a,xlab = "LeagueIndex",type="b",pch=19,ylab="SelectByHotkeys(0-221)",main ="increasing") #지수분포 증가8이 outlier

b1 = paste("AssignToHotkeys ~ LeagueIndex")
game$AssignToHotkeys = as.numeric(as.character(game$AssignToHotkeys))*85.5*60 #1min당
AssignToHotkeys_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
AssignToHotkeys_a
game$AssignToHotkeys[which.min(game$AssignToHotkeys)]
game$AssignToHotkeys[which.max(game$AssignToHotkeys)]
plot(AssignToHotkeys_a,xlab = "LeagueIndex",type="b",pch=19,ylab="AssignToHotkeys(0-9)",main ="increasing") #지수분포 증가8이 outlier

b1 = paste("UniqueHotkeys ~ LeagueIndex")
game$UniqueHotkeys = as.numeric(as.character(game$UniqueHotkeys)) #한 타임에서
UniqueHotkeys_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
UniqueHotkeys_a
game$UniqueHotkeys[which.min(game$UniqueHotkeys)]#?????????????????
game$UniqueHotkeys[which.max(game$UniqueHotkeys)]
plot(UniqueHotkeys_a,xlab = "LeagueIndex",type="b",pch=19,ylab="UniqueHotkeys(0-10)",main ="increasing") #지수분포 증가

b1 = paste("MinimapAttacks ~ LeagueIndex")
game$MinimapAttacks = as.numeric(as.character(game$MinimapAttacks))*85.5*60 #1min당
MinimapAttacks_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
MinimapAttacks_a
game$MinimapAttacks[which.min(game$MinimapAttacks)]
game$MinimapAttacks[which.max(game$MinimapAttacks)]
plot(MinimapAttacks_a,xlab = "LeagueIndex",type="b",pch=19,ylab="MinimapAttacks(0-15)",main ="increasing") #지수분포 증가

b1 = paste("MinimapRightClicks ~ LeagueIndex")
game$MinimapRightClicks = as.numeric(as.character(game$MinimapRightClicks))*85.5*60 #1min당
MinimapRightClicks_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
MinimapRightClicks_a
game$MinimapRightClicks[which.min(game$MinimapRightClicks)]
game$MinimapRightClicks[which.max(game$MinimapRightClicks)]
plot(MinimapRightClicks_a,xlab = "LeagueIndex",type="b",pch=19,ylab="MinimapRightClicks(0-21)",main ="increasing") #지수분포 증가

b1 = paste("NumberOfPACs ~ LeagueIndex")
game$NumberOfPACs = as.numeric(as.character(game$NumberOfPACs))*85.5*60 #1min당
NumberOfPACs_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$NumberOfPACs[which.min(game$NumberOfPACs)]
game$NumberOfPACs[which.max(game$NumberOfPACs)]
plot(NumberOfPACs_a,xlab = "LeagueIndex",type="b",pch=19,ylab="NumberOfPACs(3-41)",main ="increasing") #지수분포 증가

b1 = paste("GapBetweenPACs ~ LeagueIndex")
GapBetweenPACs_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
GapBetweenPACs_a
table(game$GapBetweenPACs)
game$GapBetweenPACs[which.min(game$GapBetweenPACs)]
game$GapBetweenPACs[which.max(game$GapBetweenPACs)]
plot(GapBetweenPACs_a,xlab = "LeagueIndex",type="b",pch=19,ylab="GapBetweenPACs(6-238)",main ="decreasing",col = "red") #감소

b1 = paste("ActionLatency ~ LeagueIndex")
ActionLatency_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$ActionLatency[which.min(game$ActionLatency)]
game$ActionLatency[which.max(game$ActionLatency)]
plot(ActionLatency_a,xlab = "LeagueIndex",type="b",pch=19,ylab="ActionLatency(24-177)",main ="decreasing",col = "red") #감소

b1 = paste("ActionsInPAC ~ LeagueIndex")
ActionsInPAC_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$ActionsInPAC[which.min(game$ActionsInPAC)]
game$ActionsInPAC[which.max(game$ActionsInPAC)]
plot(ActionsInPAC_a,xlab = "LeagueIndex",type="b",pch=19,ylab="ActionsInPAC(2-19)",main ="increasing") #증가

b1 = paste("TotalMapExplored ~ LeagueIndex")
TotalMapExplored_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$TotalMapExplored[which.min(game$TotalMapExplored)]
game$TotalMapExplored[which.max(game$TotalMapExplored)]
plot(TotalMapExplored_a,xlab = "LeagueIndex",type="b",pch=19,ylab="TotalMapExplored(5-58)",main ="increasing") #증가

b1 = paste("WorkersMade ~ LeagueIndex")
game$WorkersMade = as.numeric(as.character(game$WorkersMade))*85.5*60 #1min당
WorkersMade_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$WorkersMade[which.min(game$WorkersMade)]
game$WorkersMade[which.max(game$WorkersMade)]
plot(WorkersMade_a,xlab = "LeagueIndex",type="b",pch=19,ylab="WorkersMade(0-27)",main ="increasing") #증가 #8번 빼고

b1 = paste("UniqueUnitsMade ~ LeagueIndex")
UniqueUnitsMade_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$UniqueUnitsMade[which.min(game$UniqueUnitsMade)]
game$UniqueUnitsMade[which.max(game$UniqueUnitsMade)]
plot(UniqueUnitsMade_a,xlab = "LeagueIndex",type="b",pch=19,ylab="UniqueUnitsMade(2-13)",main ="increasing") #증가 #8번 빼고

b1 = paste("ComplexUnitsMade ~ LeagueIndex")
game$ComplexUnitsMade = as.numeric(as.character(game$ComplexUnitsMade))*85.5*60 #1min당
ComplexUnitsMade_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$ComplexUnitsMade[which.min(game$ComplexUnitsMade)]
game$ComplexUnitsMade[which.max(game$ComplexUnitsMade)]
plot(ComplexUnitsMade_a,xlab = "LeagueIndex",type="b",pch=19,ylab="ComplexUnitsMade(0-5)",main ="increasing") #증가 #8번 빼고

b1 = paste("ComplexAbilitiesUsed ~ LeagueIndex")
game$ComplexAbilitiesUsed = as.numeric(as.character(game$ComplexAbilitiesUsed))*85.5*60 #1min당
ComplexAbilitiesUsed_a = aggregate(formula = as.formula(b1), data = game,
              FUN = mean, na.rm = F,trim = 0.05)
game$ComplexAbilitiesUsed[which.min(game$ComplexAbilitiesUsed)]
game$ComplexAbilitiesUsed[which.max(game$ComplexAbilitiesUsed)]
plot(ComplexAbilitiesUsed_a,xlab = "LeagueIndex",type="b",pch=19,ylab="ComplexAbilitiesUsed(0-16)",main ="increasing") #증가 #8번 빼고

#회귀분석

#merge
total_a = merge(Age_a,HoursPerWeek_a,by ="LeagueIndex")
total_a = merge(total_a,TotalHours_a,by ="LeagueIndex")
total_a = merge(total_a,APM_a,by ="LeagueIndex")
total_a = merge(total_a,SelectByHotkeys_a,by ="LeagueIndex")
total_a = merge(total_a,AssignToHotkeys_a,by ="LeagueIndex")
total_a = merge(total_a,UniqueHotkeys_a,by ="LeagueIndex")
total_a = merge(total_a,MinimapAttacks_a,by ="LeagueIndex")
total_a = merge(total_a,MinimapRightClicks_a,by ="LeagueIndex")
total_a = merge(total_a,NumberOfPACs_a,by ="LeagueIndex")
total_a = merge(total_a,GapBetweenPACs_a,by ="LeagueIndex")
total_a = merge(total_a,ActionLatency_a,by ="LeagueIndex")
total_a = merge(total_a,ActionsInPAC_a,by ="LeagueIndex")
total_a = merge(total_a,TotalMapExplored_a,by ="LeagueIndex")
total_a = merge(total_a,WorkersMade_a,by ="LeagueIndex")
total_a = merge(total_a,UniqueUnitsMade_a,by ="LeagueIndex")
total_a = merge(total_a,ComplexUnitsMade_a,by ="LeagueIndex")
total_a = merge(total_a,ComplexAbilitiesUsed_a,by ="LeagueIndex")
total_a


##########이렇게 하는방법이 있을것같은데.....
#names(game)
#
#m1 = paste0(names(game),"_a")[4:20]
#as.formula(m1[4])
#class(Age_a)
#for(i in m1){
#   Age_a = merge(Age_a,i,by = "LeagueIndex")
#   #print(i)
#}
############################################
#names(game)[3:20]
game_lm = lm(LeagueIndex~.,data = total_a)
game_lm
x11()
lm(Age~LeagueIndex, data=total_a) %>% summary()#0.7378
lm(log(HoursPerWeek)~LeagueIndex, data=total_a) %>% summary()#0.7611
exp(0.13593)
exp(0.13593*8+2.21082)
lm(log(TotalHours)~LeagueIndex, data=total_a) %>% summary()
exp(0.28203)
exp(0.28203*8+5.16915)
lm(log(APM)~LeagueIndex, data=total_a) %>% summary()
exp(0.193651)
lm(log(SelectByHotkeys)~LeagueIndex, data=total_a) %>% summary()
exp(0.396297)
lm(log(AssignToHotkeys)~LeagueIndex, data=total_a) %>% summary()
exp(0.226732)
lm(log(UniqueHotkeys)~LeagueIndex, data=total_a) %>% summary()
exp(0.12794)
lm(log(MinimapAttacks)~LeagueIndex, data=total_a) %>% summary()
exp(0.42612)
lm(log(MinimapRightClicks)~LeagueIndex, data=total_a) %>% summary()
exp(0.168960)
lm(log(NumberOfPACs)~LeagueIndex, data=total_a) %>% summary()
exp(0.124475)
lm(log(GapBetweenPACs)~LeagueIndex, data=total_a) %>% summary()
exp(-0.160478)
lm(log(ActionLatency)~LeagueIndex, data=total_a) %>% summary()
exp(-0.136183)
lm(log(ActionsInPAC)~LeagueIndex, data=total_a) %>% summary()
exp(0.026825)
lm(log(TotalMapExplored)~LeagueIndex, data=total_a) %>% summary() #0.7228
exp(0.06289)
lm(log(WorkersMade)~LeagueIndex, data=total_a) %>% summary()
exp(0.11219)
lm(log(UniqueUnitsMade)~LeagueIndex, data=total_a) %>% summary()
exp(0.031467)
lm(log(ComplexUnitsMade)~LeagueIndex, data=total_a) %>% summary()
exp(0.40882)
lm(log(ComplexAbilitiesUsed)~LeagueIndex, data=total_a) %>% summary()
exp(0.33161)

ylab = name(range)

