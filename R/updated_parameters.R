library(FishRAM)
library(dplyr)

rm(list=ls())
data("seabass")
#recruitment <- 10105156 #medium recruitment individuals (geo mean 2010-2019)
recruitment <- 20892000  #medium recruitment individuals (median  2000-2019)
recruitment <- 11388500 #low recruitment individuals (median  2010-2019)
#recruitment <- 3732789 #low  recruitment individuals (geo mean 2010-2019)
#recruitment<- original high: 23,151,200; medium 8,606,000; low 757,600
recruitment<- 8606000

params@initialSA<-11509000 # fish at age data WGCSE 2022 sum age 6-16+ in 2010
params@initialSJ<-67475000 # fish at age data WGCSE 2022 sum age 0-5 in 2010
#price 11.23 (average Eumofa price for 2018*average exchange rate of 0.885)
params@nu<-11.23*1000 #price per tonne

CLim_func <- function(stock){
  return(0.203/(0.203 + 0.24) * stock * (1 - exp(-(0.203 + 0.24))))
}

#no fishing
params@theta<-0 #14165
#params@initialTC<-0
params@gamma<-7000000*0.75 #max number of recreational trips
params@initialTR<-6575000*0.3 #total number of recreational trips * proportion of seabass anglers

sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043)
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/no_fishing.csv", row.names = F)
plotEconomicImpact(sim)
plotStock(sim)
plotActivity(sim)

#self-management under MSY
params@theta<-24525*2.5 #24525 number of commercial trips of UK landing seabass in 2010 - double to account for french and other countries fishing activities
params@initialTC<-24525
params@gamma<-7000000
params@initialTR<-6575000*0.2

sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043)
sim@states
plotEconomicImpact(sim)
plotStock(sim)
plotActivity(sim)
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/msy.csv", row.names = F)

#100:0
sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043,CLim_func = CLim_func, CLim_alloc = c(1,0))
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/100-commercial.csv", row.names = F)

#75:25
sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043,CLim_func = CLim_func, CLim_alloc = c(0.75,0.25))
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/75-commercial.csv", row.names = F)

#50:50
sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043,CLim_func = CLim_func, CLim_alloc = c(0.5,0.5))
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/50-commercial.csv", row.names = F)

#25:75
sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043,CLim_func = CLim_func, CLim_alloc = c(0.25,0.75))
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/25-commercial.csv", row.names = F)

#0:100
sim <- project(params, R = recruitment, t_start = 2010, t_end = 2043,CLim_func = CLim_func, CLim_alloc = c(0,1))
sim@states
sim1<-data.frame(sim@states)
#sim1<-select(sim@states,select=c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC"))
#colnames(sim1)<-c("t","R","tau","SA","SJ","LR","LC","VR","VC","ER","EC")
write.csv(sim1,"C://Users/am21/OneDrive - CEFAS/seabass/mncea/0-commercial.csv", row.names = F)

