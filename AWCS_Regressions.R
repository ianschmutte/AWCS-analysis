awcsdata=read.csv("AWCS Data.csv")
install.packages(devtools)
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
hourssched[4]=awcsdata$ms436_q22
hourssched[5]=awcsdata$ms436_q37a
hourssched[6]=awcsdata$ms436_q37b
hourssched[7]=awcsdata$ms436_q37c
hourssched[8]=awcsdata$ms436_q37d
hourssched[9]=awcsdata$ms436_q37e
hoursschednames=c("q18","q20","q20_wks","q22","q37a","q37b","q37c","q37d","q37e")
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
pacenona=na.omit(pace)

pacepca=prcomp(pacenona,center=TRUE,scale=TRUE)
summary(pacepca)
ggbiplot(pacepca)

learning=as.data.frame(awcsdata$ms436_q60)
learning[2]=awcsdata$ms436_q61a
learning[3]=awcsdata$ms436_q61b
learning[4]=awcsdata$ms436_q61c
learning[5]=awcsdata$ms436_q61d
learningnona=na.omit(learning)
learningnona

learningpca=prcomp(learningnona,center=TRUE,scale=TRUE)
summary(learningpca)
ggbiplot(learningpca)

total<-merge(background,worklifebalance,by="row.names")
total
total=merge(total,hourssched,by="row.names")
total=merge(total,ptobenefits,by="row.names")
total
total=total[-c(1,2,3)]
totalnona=na.omit(total)
totalnona
totalnona=totalnona[-c(2)]
totalpca<-prcomp(totalnona,center=TRUE,scale=TRUE)
summary(totalpca)

kmeans(totalnona,3)


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