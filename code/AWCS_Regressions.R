setwd("E:/research/git-repos/AWCS-analysis")
awcsdata=read.csv("./data/AWCS Data.csv")
install.packages("devtools")
library(devtools)
install.packages("digest")
install_github("vqv/ggbiplot",force=T)
library(ggbiplot)


awcsdata$tiring=NA
awcsdata$tiring[awcsdata$ms436_q24a<=4]=1
awcsdata$tiring[awcsdata$ms436_q24a>4]=0
awcsdata$tiring
awcsdata$lifting=NA
awcsdata$lifting[awcsdata$ms436_q24b<=4]=1
awcsdata$lifting[awcsdata$ms436_q24b>4]=0
awcsdata$lifting
awcsdata$carrying=NA
awcsdata$carrying[awcsdata$ms436_q24c<=4]=1
awcsdata$carrying[awcsdata$ms436_q24c>4]=0
awcsdata$carrying
awcsdata$sitting=NA
awcsdata$sitting[awcsdata$ms436_q24d<=4]=1
awcsdata$sitting[awcsdata$ms436_q24d>4]=0
awcsdata$sitting
awcsdata$repetitivearm=NA
awcsdata$repetitivearm[awcsdata$ms436_q24e<=4]=1
awcsdata$repetitivearm[awcsdata$ms436_q24e>4]=0
awcsdata$repetitivearm
awcsdata$directlydealing=NA
awcsdata$directlydealing[awcsdata$ms436_q24f<=4]=1
awcsdata$directlydealing[awcsdata$ms436_q24f>4]=0
awcsdata$directlydealing
awcsdata$computer=NA
awcsdata$computer[awcsdata$ms436_q24i<=4]=1
awcsdata$computer[awcsdata$ms436_q24i>4]=0
awcsdata$computer

physdemandmodel=lm(awcsdata$ms436_ef10~awcsdata$tiring+awcsdata$lifting+awcsdata$carrying+awcsdata$sitting+awcsdata$repetitivearm
                   +awcsdata$directlydealing+awcsdata$computer,na.action=na.exclude)
summary(physdemandmodel)

awcsdata$vibrations=NA
awcsdata$vibrations[awcsdata$ms436_q23a<=4]=1
awcsdata$vibrations[awcsdata$ms436_q23a>4]=0
awcsdata$vibrations
awcsdata$noise=NA
awcsdata$noise[awcsdata$ms436_q23b<=4]=1
awcsdata$noise[awcsdata$ms436_q23b>4]=0
awcsdata$noise
awcsdata$hightemp=NA
awcsdata$hightemp[awcsdata$ms436_q23c<=4]=1
awcsdata$hightemp[awcsdata$ms436_q23c>4]=0
awcsdata$hightemp
awcsdata$lowtemp=NA
awcsdata$lowtemp[awcsdata$ms436_q23d<=4]=1
awcsdata$lowtemp[awcsdata$ms436_q23d>4]=0
awcsdata$lowtemp
awcsdata$smokefumes=NA
awcsdata$smokefumes[awcsdata$ms436_q23e<=4]=1
awcsdata$smokefumes[awcsdata$ms436_q23e>4]=0
awcsdata$smokefumes
awcsdata$vapors=NA
awcsdata$vapors[awcsdata$ms436_q23f<=4]=1
awcsdata$vapors[awcsdata$ms436_q23f>4]=0
awcsdata$vapors
awcsdata$chem=NA
awcsdata$chem[awcsdata$ms436_q23g<=4]=1
awcsdata$chem[awcsdata$ms436_q23g>4]=0
awcsdata$chem
awcsdata$tobacco=NA
awcsdata$tobacco[awcsdata$ms436_q23h<=4]=1
awcsdata$tobacco[awcsdata$ms436_q23h>4]=0
awcsdata$tobacco
awcsdata$infectious=NA
awcsdata$infectious[awcsdata$ms436_q23i<=4]=1
awcsdata$infectious[awcsdata$ms436_q23i>4]=0
awcsdata$infectious

physexposemodel=lm(awcsdata$ms436_ef10~awcsdata$vibrations+awcsdata$noise+awcsdata$hightemp+awcsdata$lowtemp
                   +awcsdata$smokefumes+awcsdata$vapors+awcsdata$chem+awcsdata$tobacco+awcsdata$infectious
                   +awcsdata$ms436_q28+awcsdata$ms436_pc1a+awcsdata$ms436_pc1b+awcsdata$ms436_pc1c+awcsdata$ms436_pc1d+
                     awcsdata$ms436_pc1e+awcsdata$ms436_pc1f+awcsdata$ms436_pc1g+awcsdata$ms436_pc1h+awcsdata$ms436_pc1i+
                     awcsdata$ms436_pc1j+awcsdata$ms436_pc1k+awcsdata$ms436_pc1l+awcsdata$ms436_pc1m+awcsdata$ms436_pc1n+
                     awcsdata$ms436_pc1o+awcsdata$ms436_pc1p+awcsdata$ms436_pc1q+awcsdata$ms436_pc1r+awcsdata$ms436_pc1s+
                     awcsdata$ms436_pc1t+awcsdata$ms436_pc1u, na.action=na.exclude)
summary(physexposemodel)


worklifebalance=as.data.frame(awcsdata$ms436_q31)
worklifebalance[2]=awcsdata$ms436_q32
worklifebalance[3]=awcsdata$ms436_q36
worklifebalance[4]=awcsdata$ms436_q41
worklifebalance[5]=awcsdata$ms436_q41aa
worklifebalance[6]=awcsdata$ms436_q41ab
worklifebalance[7]=awcsdata$ms436_q41ac
worklifebalance[8]=awcsdata$ms436_q41ad
worklifebalance[9]=awcsdata$ms436_q41ae
worklifebalance[10]=awcsdata$ms436_q41af
worklifebalance[11]=awcsdata$ms436_n1
worklifebalance[12]=awcsdata$ms436_q43
wlbnames=c("q31","q32","q36","q41","q41aa","q41ab","q41ac","q41ad","q41ae","q41af","qn1","q43")
names(worklifebalance)=wlbnames
worklifebalancenona=na.omit(worklifebalance)
worklifebalance

worklifepca<-prcomp(worklifebalancenona,center=TRUE,scale=TRUE)
summary(worklifepca)
ggbiplot(worklifepca)

background=as.data.frame(awcsdata$ms436_q11)
background[2]=awcsdata$ms436_q11a
background[3]=awcsdata$ms436_q11b
background[4]=awcsdata$ms436_q12_years
background[5]=awcsdata$ms436_n7
backnames=c("q11","q11a","q11b","q12_years","n7")
names(background)=backnames
backgroundnona=na.omit(background)
backgroundnona

backgroundpca<-prcomp(backgroundnona,center=TRUE,scale=TRUE)
summary(backgroundpca)
ggbiplot(backgroundpca)

hourssched=as.data.frame(awcsdata$ms436_q18)
hourssched
hourssched[2]=awcsdata$ms436_q20
hourssched[3]=awcsdata$ms436_q20_wks
hourssched[4]=awcsdata$ms436_q37a
hourssched[5]=awcsdata$ms436_q37b
hourssched[6]=awcsdata$ms436_q37c
hourssched[7]=awcsdata$ms436_q37d
hourssched[8]=awcsdata$ms436_q37e
hoursschednames=c("q18","q20","q20_wks","q37a","q37b","q37c","q37d","q37e")
names(hourssched)=hoursschednames
hoursschednona=na.omit(hourssched)
hoursschednona

hoursschedpca<-prcomp(hoursschednona,center=TRUE,scale=TRUE)
summary(hoursschedpca)
ggbiplot(hoursschedpca)

ptobenefits=as.data.frame(awcsdata$ms436_n4a)
ptobenefits[2]=awcsdata$ms436_n4b
ptobenefits[3]=awcsdata$ms436_n4c
ptobenefits[4]=awcsdata$ms436_n4d
ptobenefits[5]=awcsdata$ms436_n4e
ptobenefits[6]=awcsdata$ms436_n4f
ptobenefits[7]=awcsdata$ms436_n4g
ptobenefits[8]=awcsdata$ms436_n4h
ptobenefits[9]=awcsdata$ms436_n4i
ptobenefits[10]=awcsdata$ms436_n5a
ptobenefits[11]=awcsdata$ms436_n5b
ptobenefits[12]=awcsdata$ms436_n6_days
ptonames=c("n4a","n4b","n4c","n4d","n4e","n4f","n4g","n4h","n4i","n5a","n5b","n6days")
names(ptobenefits)=ptonames
ptobenefitsnona=na.omit(ptobenefits)

ptobenefitspca<-prcomp(ptobenefitsnona,center=TRUE,scale=TRUE)
summary(ptobenefitspca)
ggbiplot(ptobenefitspca)

location=as.data.frame(awcsdata$ms436_q27a)
location[2]=awcsdata$ms436_q27b
location[3]=awcsdata$ms436_q27c
location[4]=awcsdata$ms436_q27d
location[5]=awcsdata$ms436_q27e
location[6]=awcsdata$ms436_q27f
location[7]=awcsdata$ms436_n8
locnames=c("q27a","q27b","q27c","q27d","q27e","q27f","n8")
names(location)=locnames
locationnona=na.omit(location)

locationpca<-prcomp(locationnona,center=TRUE,scale=TRUE)
summary(locationpca)
ggbiplot(locationpca)

autonomy=as.data.frame(awcsdata$ms436_q49a)
autonomy[2]=awcsdata$ms436_q49b
autonomy[3]=awcsdata$ms436_q49c
autonomy[4]=awcsdata$ms436_q49d
autonomy[5]=awcsdata$ms436_q49e
autonomy[6]=awcsdata$ms436_q49f
autonomy[7]=awcsdata$ms436_q50a
autonomy[8]=awcsdata$ms436_q50b
autonomy[9]=awcsdata$ms436_q50c
autonomy[10]=awcsdata$ms436_q51a
autonomy[11]=awcsdata$ms436_q51b
autonomy[12]=awcsdata$ms436_q51c
autonomy[13]=awcsdata$ms436_q51d
autonomy[14]=awcsdata$ms436_q51e
autonomy[15]=awcsdata$ms436_q51f
autnames=c("q49a","q49b","q49c","q49d","q49e","q49f","q50a","q50b","q50c","q51a","q51b","q51c","q51d","q51e","q51f")
names(autonomy)=autnames
autonomynona=na.omit(autonomy)

autonomypca=prcomp(autonomynona,center=TRUE,scale=TRUE)
summary(autonomypca)
ggbiplot(autonomypca)

pace=as.data.frame(awcsdata$ms436_q45a)
pace[2]=awcsdata$ms436_q45b
pace[3]=awcsdata$ms436_q46a
pace[4]=awcsdata$ms436_q46b
pace[5]=awcsdata$ms436_q46c
pace[6]=awcsdata$ms436_q46d
pace[7]=awcsdata$ms436_q46e
pace[8]=awcsdata$ms436_q47
pace[9]=awcsdata$ms436_q48
pacenames=c("q45a","q45b","q46a","q46c","q46d","q46e","q47","q48")
names(pace)=pacenames
pacenona=na.omit(pace)

pacepca=prcomp(pacenona,center=TRUE,scale=TRUE)
summary(pacepca)
ggbiplot(pacepca)

learning=as.data.frame(awcsdata$ms436_q60)
learning[2]=awcsdata$ms436_q61a
learning[3]=awcsdata$ms436_q61b
learning[4]=awcsdata$ms436_q61c
learning[5]=awcsdata$ms436_q61d
learnnames=c("q60","q61a","q61b","q61c","q61d")
names(learning)=learnnames
learningnona=na.omit(learning)
learningnona

learningpca=prcomp(learningnona,center=TRUE,scale=TRUE)
summary(learningpca)
ggbiplot(learningpca)

collab=as.data.frame(awcsdata$ms436_q56)
collab[2]=awcsdata$ms436_q57a
collab[3]=awcsdata$ms436_q57b
collab[4]=awcsdata$ms436_q57c
collab[5]=awcsdata$ms436_n10
collabnames=c("q56","q57a","q57b","q57c","n10")
names(collab)=collabnames
collabnona=na.omit(collab)
collabnona

collabpca=prcomp(collabnona,center=TRUE,scale=TRUE)
summary(collabpca)
ggbiplot(collabpca)

physdemand=as.data.frame(awcsdata$ms436_q24a)
physdemand[2]=awcsdata$ms436_q24b
physdemand[3]=awcsdata$ms436_q24c
physdemand[4]=awcsdata$ms436_q24d
physdemand[5]=awcsdata$ms436_q24e
physdemand[6]=awcsdata$ms436_q24f
physdemand[7]=awcsdata$ms436_q24i
physdemand
physdemand[8]=awcsdata$ms436_q24j
physdemand[9]=awcsdata$ms436_q24k
physdnames=c("q24a","q24b","q24c","q24d","q24e","q24f","q24g","q24h","q24i")
names(physdemand)=physdnames
physdemandnona=na.omit(physdemand)

physdemandpca=prcomp(physdemandnona,center=TRUE,scale=TRUE)
summary(physdemandpca)
ggbiplot(physdemandpca)

physexpose=as.data.frame(awcsdata$ms436_q23a)
physexpose[2]=awcsdata$ms436_q23b
physexpose[3]=awcsdata$ms436_q23c
physexpose[4]=awcsdata$ms436_q23d
physexpose[5]=awcsdata$ms436_q23e
physexpose[6]=awcsdata$ms436_q23f
physexpose[7]=awcsdata$ms436_q23g
physexpose[8]=awcsdata$ms436_q23h
physexpose[9]=awcsdata$ms436_q23i
physexpose[10]=awcsdata$ms436_q28
physexpose[11]=awcsdata$ms436_pc1a
physexpose[12]=awcsdata$ms436_pc1b
physexpose[13]=awcsdata$ms436_pc1c
physexpose[14]=awcsdata$ms436_pc1d
physexpose[15]=awcsdata$ms436_pc1e
physexpose[16]=awcsdata$ms436_pc1f
physexpose[17]=awcsdata$ms436_pc1g
physexpose[18]=awcsdata$ms436_pc1h
physexpose[19]=awcsdata$ms436_pc1i
physexpose[20]=awcsdata$ms436_pc1j
physexpose[21]=awcsdata$ms436_pc1k
physexpose[22]=awcsdata$ms436_pc1l
physexpose[23]=awcsdata$ms436_pc1m
physexpose[24]=awcsdata$ms436_pc1n
physexpose[25]=awcsdata$ms436_pc1o
physexpose[26]=awcsdata$ms436_pc1p
physexpose[27]=awcsdata$ms436_pc1q
physexpose[28]=awcsdata$ms436_pc1r
physexpose[29]=awcsdata$ms436_pc1s
physexpose[30]=awcsdata$ms436_pc1t
physexpose[31]=awcsdata$ms436_pc1u
physenames=c("q23a","q23b","q23c","q23d","q23e","q23f","q23g","q23h","q23i","q28","pc1a","pc1b","pc1c","pc1d","pc1e","pc1f","pc1g","pc1h","pc1i",
             "pc1j","pc1k","pc1l","pc1m","pc1n","pc1o","pc1p","pc1q","pc1r","pc1s","pc1t","pc1u")
names(physexpose)=physenames
physexposenona=na.omit(physexpose)

physexposepca=prcomp(physexposenona,center=TRUE,scale=TRUE)
summary(physexposepca)
ggbiplot(physexposepca)

management=as.data.frame(awcsdata$ms436_q17)
management[2]=awcsdata$ms436_q59
management[3]=awcsdata$ms436_q58a
management[4]=awcsdata$ms436_q58b
management[5]=awcsdata$ms436_q58c
management[6]=awcsdata$ms436_q58d
management[7]=awcsdata$ms436_q58e
management[8]=awcsdata$ms436_q58f
management[9]=awcsdata$ms436_q58g
management[10]=awcsdata$ms436_q61_4a
management[11]=awcsdata$ms436_q61_4b
management[12]=awcsdata$ms436_q61_4c
management[13]=awcsdata$ms436_q61_4d
management[14]=awcsdata$ms436_q61_4e
management[15]=awcsdata$ms436_q61_4f
management[16]=awcsdata$ms436_q61_4g
managenames=c("q17","q59","q58a","q58b","q58c","q58d","q58e","q58f","q58g","q614a","q614b","q614c","q614d","q614e","q614f","q614g")
names(management)=managenames
managementnona=na.omit(management)

managementpca=prcomp(managementnona,center=TRUE,scale=TRUE)
summary(managementpca)
ggbiplot(managementpca)

stress=as.data.frame(awcsdata$ms436_q52a)
stress[2]=awcsdata$ms436_q52b
stress[3]=awcsdata$ms436_q52c
stress[4]=awcsdata$ms436_q52d
stress[5]=awcsdata$ms436_q52e
stress[6]=awcsdata$ms436_q52f
stress[7]=awcsdata$ms436_q52g
stress[8]=awcsdata$ms436_q77a
stress[9]=awcsdata$ms436_q77b
stress[10]=awcsdata$ms436_q77c
stress[11]=awcsdata$ms436_q77d
stress[12]=awcsdata$ms436_q77e
stress[13]=awcsdata$ms436_q65a
stress[14]=awcsdata$ms436_q65b
stress[15]=awcsdata$ms436_q65c
stress[16]=awcsdata$ms436_q65d
stress[17]=awcsdata$ms436_q65e
stress[18]=awcsdata$ms436_q65f
stress[19]=awcsdata$ms436_q65g
stress[20]=awcsdata$ms436_q70a
stress[21]=awcsdata$ms436_q70b
stress[22]=awcsdata$ms436_q70c
stress[23]=awcsdata$ms436_q70d
stress[24]=awcsdata$ms436_q71a
stress[25]=awcsdata$ms436_q71b
stress[26]=awcsdata$ms436_q71c
stressnames=c("q52a","q52b","q52c","q52d","q52e","q52f","q52g","q77a","q77b","q77c","q77d","q77e","q65a","q65b","q65c","q65d","q65e","q65f",
              "q65g","q70a","q70b","q70c","q70d","q71a","q71b","q71c")
names(stress)=stressnames
stressnona=na.omit(stress)

stresspca=prcomp(stressnona,center=TRUE,scale=TRUE)
summary(stresspca)
ggbiplot(stresspca)

meaningful=as.data.frame(awcsdata$ms436_n3a)
meaningful[2]=awcsdata$ms436_n3b
meaningful[3]=awcsdata$ms436_n3c
meaningful[4]=awcsdata$ms436_n3d
meaningful[5]=awcsdata$ms436_n3e
meaningful[6]=awcsdata$ms436_n3f
meaningnames=c("n3a","n3b","n3c","n3d","n3e","n3f")
names(meaningful)=meaningnames
meaningfulnona=na.omit(meaningful)
s
meaningfulpca=prcomp(meaningfulnona,center=TRUE,scale=TRUE)
summary(meaningfulpca)
ggbiplot(meaningfulpca)


total=cbind(background,worklifebalance,hourssched,ptobenefits,location,autonomy,pace,learning,collab,physdemand,physexpose,management,stress,meaningful)
total=total[!names(total)%in%c("q57a","q57b","q57c","n5a")]
totalnona=na.omit(total)
total
totalnona
totalpca<-prcomp(totalnona,center=TRUE,scale=TRUE)
summary(totalpca)

kmeans(totalnona,4)



control=as.data.frame(awcsdata$ms436_q39)
control[2]=awcsdata$ms436_ef10
control[3]=0
control[3][control[1]==1]=control[2]
control[3]
control=na.omit(control)
control=matrix(control)
controlbar=matrix(ncol=2,nrow=4)
controlbar[1,1]=1
controlbar[1,2]=2
controlbar[1,3]=3
controlbar[1,4]=4
controlbar[2,1]=mean(control[3])
controlbar[2,2]=2
controlbar[2,3]=3
controlbar[2,4]=4



wage=awcsdata$ms436_ef10
gender=as.factor(awcsdata$ms436_gender)
livingsituation=as.factor(awcsdata$ms436_currentlivingsituation)
ethnicity=as.factor(awcsdata$ms436_ethnicity)
educ=as.factor(awcsdata$ms436_highesteducation)
selfemp=as.factor(awcsdata$ms436_q6)
sector=as.factor(awcsdata$ms436_q10)

wageframe=as.data.frame(wage)
wageframe[2]=gender
wageframe[3]=livingsituation
wageframe[4]=ethnicity
wageframe[5]=educ
wageframe[6]=selfemp
wageframe[7]=sector
wageframenona=na.omit(wageframe)
wageframenona

wagemodel=lm(wage~.,data=wageframenona)
summary(wagemodel)
