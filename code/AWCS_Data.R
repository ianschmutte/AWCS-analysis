awcsdata=read.csv("AWCS DATA.csv",na.strings=c(""))
awcsdata[1:10,1:10]
awcsdata$ms436_borninus[1:50]

hist(awcsdata$ms436_calcage)
hist(awcsdata$ms436_currentlivingsituation)


#Compare % age 25-34 in my downloaded file to the report
summary(awcsdata$ms436_calcage)
age25to34sub=subset(awcsdata,awcsdata$ms436_calcage<=34&awcsdata$ms436_calcage>=25)
age25to34=awcsdata$ms436_calcage[which(awcsdata$ms436_calcage<=34&awcsdata$ms436_calcage>=25)]
age25to34
NROW(age25to34)/NROW(awcsdata)
# 13.92% in my data, 14.2% in report
pctage25to34=NROW(age25to34)/NROW(awcsdata)
pctage25to34
pctage25to34wgt=(NROW(age25to34)/NROW(awcsdata))*23.2/14.2
pctage25to34wgt
#22.75% vs. 23.2%

#Same for 35-49
summary(awcsdata$ms436_calcage)
age35to49sub=subset(awcsdata,awcsdata$ms436_calcage<=35&awcsdata$ms436_calcage>=49)
age35to49=awcsdata$ms436_calcage[which(awcsdata$ms436_calcage<=49&awcsdata$ms436_calcage>=35)]
age35to49
NROW(age35to49)/NROW(awcsdata)
# 26.64% vs 27.2%
pctage35to49=NROW(age35to49)/NROW(awcsdata)
pctage35to49
pctage35to49wgt=NROW(age35to49)/NROW(awcsdata)*32.7/27.2
pctage35to49wgt
#32.02% vs. 32.7%

# Compare gender ratios
summary(awcsdata$ms436_gender)
femalesub=subset(awcsdata,awcsdata$ms436_gender==2)
female=awcsdata$ms436_gender[which(awcsdata$ms436_gender==2)]
female
NROW(femalesub)/NROW(awcsdata)
#58.64% vs. 58.5%
pctfemale=NROW(femalesub)/NROW(awcsdata)
pctfemale
pctfemalewgt=NROW(femalesub)/NROW(awcsdata)*51.4/58.5
pctfemalewgt
#51.52% vs. 51.4%

#Compare white non-Hispanic ratios
summary(awcsdata$ms436_hispaniclatino)
summary(awcsdata$ms436_ethnicity)
nonhiswhitesub=subset(awcsdata,awcsdata$ms436_hispaniclatino==2&awcsdata$ms436_ethnicity==1)
NROW(nonhiswhitesub)/NROW(awcsdata)
#64.23% vs 64.9% 
pctnonhiswhite=NROW(nonhiswhitesub)/NROW(awcsdata)
pctnonhiswhite
pctnonhiswhitewgt=NROW(nonhiswhitesub)/NROW(awcsdata)*64.9/64.9
pctnonhiswhitewgt
#64.23% vs. 64.3%

#Compare black non-Hispanic ratios
summary(awcsdata$ms436_hispaniclatino)
summary(awcsdata$ms436_ethnicity)
nonhisblacksub=subset(awcsdata,awcsdata$ms436_hispaniclatino==2&awcsdata$ms436_ethnicity==2)
NROW(nonhisblacksub)/NROW(awcsdata)
#11.91% vs. 11.9%
pctnonhisblack=NROW(nonhisblacksub)/NROW(awcsdata)
pctnonhisblack
pctnonhisblackwgt=NROW(nonhisblacksub)/NROW(awcsdata)*12.5/11.9
pctnonhisblackwgt
#12.51% vs. 12.4%

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Compare hispanic ratios
summary(awcsdata$ms436_hispaniclatino)
summary(awcsdata$ms436_ethnicity)
awcsdata$ms436_hispaniclatino
hispanicsub=subset(awcsdata,awcsdata$ms436_hispaniclatino==1)
NROW(hispanicsub)/NROW(awcsdata)
#18.72% vs. 11.1% (!!!)
pcthispanic=NROW(hispanicsub)/NROW(awcsdata)
pcthispanic
pcthispanicwgt=NROW(hispanicsub)/NROW(awcsdata)*15.3/11.1
pcthispanicwgt
#25.8% vs. 15.8%
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Compare max. high school degree ratios
summary(awcsdata$ms436_highesteducation)
awcsdata$ms436_highesteducation
highschoolmaxsub=subset(awcsdata,awcsdata$ms436_highesteducation<=9)
NROW(highschoolmaxsub)/NROW(awcsdata)
pcthighschoolmax=NROW(highschoolmaxsub)/NROW(awcsdata)
#17.47% vs. 17.4%
pcthighschoolmax
pcthighschoolmaxwgt=NROW(highschoolmaxsub)/NROW(awcsdata)*39.3/17.4
pcthighschoolmaxwgt
#39.46% vs. 37.6%

# Compare some college/ass. degree ratios
summary(awcsdata$ms436_highesteducation)
awcsdata$ms436_highesteducation
somecollegesub=subset(awcsdata,awcsdata$ms436_highesteducation>=10&awcsdata$ms436_highesteducation<=12)
NROW(somecollegesub)/NROW(awcsdata)
#36.86% vs. 36.5%
pctsomecollege=NROW(somecollegesub)/NROW(awcsdata)
pctsomecollege
pctsomecollegewgt=NROW(somecollegesub)/NROW(awcsdata)*26.9/36.5
pctsomecollegewgt
#27.16% vs. 28.2%

#Compare bachelor's degree or higher ratios
summary(awcsdata$ms436_highesteducation)
awcsdata$ms436_highesteducation
bachelorsminsub=subset(awcsdata,awcsdata$ms436_highesteducation>=13&awcsdata$ms436_highesteducation<=16)
NROW(bachelorsminsub)/NROW(awcsdata)
#45.67% vs. 46.1%
pctbachelorsmin=NROW(bachelorsminsub)/NROW(awcsdata)
pctbachelorsmin
pctbachelorsminwgt=NROW(bachelorsminsub)/NROW(awcsdata)*33.7/46.1
pctbachelorsminwgt
#33.39% vs. 34.2%

#???
#Compare % in labor force
awcsdata$ms436_currentjobstatuss
inlaborforcesub=subset(awcsdata,awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss2==2|
                         awcsdata$ms436_currentjobstatuss3==3)
NROW(inlaborforcesub)/NROW(awcsdata)
#72.79% vs. 76.8%   (??)
pctinlaborforce=NROW(inlaborforcesub)/NROW(awcsdata)
pctinlaborforcewgt=NROW(inlaborforcesub)/NROW(awcsdata)*72.3/76.8
pctinlaborforcewgt
#68.52% vs. 72.3%
#???

#Compare % working
awcsdata$ms436_doyouwork
workingsub=subset(awcsdata,awcsdata$ms436_doyouwork==1|awcsdata$ms436_doyouwork==2)
NROW(workingsub)/NROW(awcsdata)
workingsub2=subset(awcsdata,awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)
NROW(workingsub2)/NROW(awcsdata)
#76.3% vs. 67.4% 65.86% vs. 67.4%
pctworking2=NROW(workingsub2)/NROW(awcsdata)
pctworking2
pctworking2wgt=NROW(workingsub2)/NROW(awcsdata)*69.4/67.4
pctworking2wgt
#67.81% vs. 69.4%






awcsdata$ms436_weight


# Compare max. high school degree ratios
summary(awcsdata$ms436_highesteducation)
awcsdata$ms436_highesteducation
highschoolmaxsubwgt=subset(awcsdata,awcsdata$ms436_highesteducation*awcsdata$ms436_weight<=9)
NROW(highschoolmaxsubwgt)/NROW(awcsdata)
#17.47% vs. 17.4%

#///////////////////////////////////////////////////////////////////// work for 9/11

 
#Make age25to34 column
awcsdata$age25to34=0
awcsdata$age25to34[awcsdata$ms436_calcage<=34&awcsdata$ms436_calcage>=25]<-1
awcsdata$age25to34
sum(awcsdata$age25to34)/3131

#age35to49
awcsdata$age35to49=0
awcsdata$age35to49[awcsdata$ms436_calcage<=49&awcsdata$ms436_calcage>=35]<-1
awcsdata$age35to49
sum(awcsdata$age35to49)/3131

#Gender
awcsdata$female=0
awcsdata$female[awcsdata$ms436_gender==2]<-1
awcsdata$female
sum(awcsdata$female)/3131

#Non-hispanic white
awcsdata$nonhiswhite=0
awcsdata$nonhiswhite[awcsdata$ms436_hispaniclatino==2&awcsdata$ms436_ethnicity==1]<-1
awcsdata$nonhiswhite
sum(awcsdata$nonhiswhite)/3131

#Non-hispanic black
awcsdata$nonhisblack=0
awcsdata$nonhisblack[awcsdata$ms436_hispaniclatino==2&awcsdata$ms436_ethnicity==2]<-1
awcsdata$nonhisblack
sum(awcsdata$nonhisblack)/3131

#Hispanic !!!!!
awcsdata$hispanic=0
awcsdata$hispanic[awcsdata$ms436_hispaniclatino==1]<-1
awcsdata$hispanic
sum(awcsdata$hispanic)/3131
#!!!!!

#High school max
awcsdata$highschoolmax=0
awcsdata$highschoolmax[awcsdata$ms436_highesteducation<=9]=1
awcsdata$highschoolmax
sum(awcsdata$highschoolmax)/3131

#Some college
awcsdata$somecollege=0
awcsdata$somecollege[awcsdata$ms436_highesteducation>=10&awcsdata$ms436_highesteducation<=12]<-1
awcsdata$somecollege
sum(awcsdata$somecollege)/3131

#Bachelors min
awcsdata$bachelorsmin=0
awcsdata$bachelorsmin[awcsdata$ms436_highesteducation>=13&awcsdata$ms436_highesteducation<=16]<-1
awcsdata$bachelorsmin
sum(awcsdata$bachelorsmin)/3131

#In labor force
awcsdata$inlaborforce=0
awcsdata$inlaborforce[awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss2==2|
                        awcsdata$ms436_currentjobstatuss3==3]<-1
awcsdata$inlaborforce
sum(awcsdata$inlaborforce)/3131

#Working
awcsdata$working=0
awcsdata$working[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1]<-1
awcsdata$working
sum(awcsdata$working)


awcstable=read.csv("AWCSTable2.3.csv")
awcstable
awcstable=awcstable[1:11,1:4]
awcstable
awcstable$CPS
awcstable$Replicate.unwgt= 0
awcstable
awcstable[1,5]<-sum(awcsdata$age25to34)
awcstable
awcstable[2,5]<-sum(awcsdata$age35to49)
awcstable[3,5]<-sum(awcsdata$female)
awcstable[4,5]<-sum(awcsdata$nonhiswhite)
awcstable[5,5]<-sum(awcsdata$nonhisblack)
awcstable[6,5]<-sum(awcsdata$hispanic)
awcstable[7,5]<-sum(awcsdata$highschoolmax)
awcstable[8,5]<-sum(awcsdata$somecollege)
awcstable[9,5]<-sum(awcsdata$bachelorsmin)
awcstable[10,5]<-sum(awcsdata$inlaborforce)
awcstable[11,5]<-sum(awcsdata$working)
awcstable
awcstable$Replicate.unwgt=awcstable$Replicate.unwgt/3131
awcstable
awcstable$Unwgt.Pct.Error=0
awcstable
awcstable$Unwgt.Pct.Error=(abs(awcstable$Replicate.unwgt-awcstable$RAND.unwgt)/awcstable$RAND.unwgt)
awcstable

awcstable$Replicate.wgt=0
awcstable$Replicate.wgt<-(awcstable$RAND.wgt/awcstable$RAND.unwgt)*awcstable$Replicate.unwgt
awcstable
awcstable$Replicate.CPS.Error=0
awcstable$Replicate.CPS.Error<-abs(awcstable$Replicate.wgt-awcstable$CPS)/awcstable$CPS
awcstable


install.packages("stargazer")
library(stargazer)


stargazer(awcstable)
stargazer(awcstable, summary=FALSE, rownames=TRUE)


awcstable2=read.csv("AWCSTable2.4.csv")
awcstable2
awcstable2$Overallreplicate=0
awcstable2

awcsdata$age25to71=0
awcsdata$age25to71[awcsdata$ms436_calcage<=71&awcsdata$ms436_calcage>=25]<-1
awcsdata$age25to71
sum(awcsdata$age25to71)

awcsdata$underage35=0
awcsdata$underage35[awcsdata$ms436_calcage<35&awcsdata$ms436_calcage>=25]<-1
awcsdata$underage35
sum(awcsdata$underage35)

awcsdata$age35=0
awcsdata$age35[awcsdata$ms436_calcage==35]<-1
sum(awcsdata$age35)

sum(awcsdata$age35to49)

awcsdata$overage50=0
awcsdata$overage50[awcsdata$ms436_calcage>=50]<-1
awcsdata$overage50
sum(awcsdata$overage50)


awcsdata$age25to71men=0
awcsdata$age25to71men[awcsdata$age25to71==1&awcsdata$female==0]<-1
sum(awcsdata$age25to71men)

awcsdata$age25to71women=0
awcsdata$age25to71women[awcsdata$age25to71==1&awcsdata$female==1]<-1
sum(awcsdata$age25to71women)

awcsdata$underage35men=0
awcsdata$underage35men[awcsdata$underage35==1&awcsdata$female==0]<-1
sum(awcsdata$underage35men)

awcsdata$underage35women=0
awcsdata$underage35women[awcsdata$underage35==1&awcsdata$female==1]<-1
sum(awcsdata$underage35women)

awcsdata$age35to49men=0
awcsdata$age35to49men[awcsdata$age35to49==1&awcsdata$female==0]<-1
sum(awcsdata$age35to49men)

awcsdata$age35to49women=0
awcsdata$age35to49women[awcsdata$age35to49==1&awcsdata$female==1]<-1
sum(awcsdata$age35to49women)

awcsdata$overage50men=0
awcsdata$overage50men[awcsdata$overage50==1&awcsdata$female==0]<-1
sum(awcsdata$overage50men)

awcsdata$overage50women=0
awcsdata$overage50women[awcsdata$overage50==1&awcsdata$female==1]<-1
sum(awcsdata$overage50women)

awcsdata$ncgage25to71men=0
awcsdata$ncgage25to71men[awcsdata$bachelorsmin==0&awcsdata$age25to71men==1]<-1
awcsdata$ncgage25to71men
sum(awcsdata$ncgage25to71men)

awcsdata$ncgage25to71women=0
awcsdata$ncgage25to71women[awcsdata$bachelorsmin==0&awcsdata$age25to71women==1]<-1
awcsdata$ncgage25to71women
sum(awcsdata$ncgage25to71women)

awcsdata$ncgunderage35men=0
awcsdata$ncgunderage35men[awcsdata$bachelorsmin==0&awcsdata$underage35men==1]<-1
awcsdata$ncgunderage35men
sum(awcsdata$ncgunderage35men)

awcsdata$ncgunderage35women=0
awcsdata$ncgunderage35women[awcsdata$bachelorsmin==0&awcsdata$underage35women==1]<-1
awcsdata$ncgunderage35women
sum(awcsdata$ncgunderage35women)

awcsdata$ncgage35to49men=0
awcsdata$ncgage35to49men[awcsdata$bachelorsmin==0&awcsdata$age35to49men==1]<-1
awcsdata$ncgage35to49men
sum(awcsdata$ncgage35to49men)

awcsdata$ncgage35to49women=0
awcsdata$ncgage35to49women[awcsdata$bachelorsmin==0&awcsdata$age35to49women==1]<-1
awcsdata$ncgage35to49women
sum(awcsdata$ncgage35to49women)

awcsdata$ncgoverage50men=0
awcsdata$ncgoverage50men[awcsdata$bachelorsmin==0&awcsdata$overage50men==1]<-1
awcsdata$ncgoverage50men
sum(awcsdata$ncgoverage50men)

awcsdata$ncgoverage50women=0
awcsdata$ncgoverage50women[awcsdata$bachelorsmin==0&awcsdata$overage50women==1]<-1
awcsdata$ncgoverage50women
sum(awcsdata$ncgoverage50women)

awcsdata$cgage25to71men=0
awcsdata$cgage25to71men[awcsdata$bachelorsmin==1&awcsdata$age25to71men==1]<-1
awcsdata$cgage25to71men
sum(awcsdata$cgage25to71men)

awcsdata$cgage25to71women=0
awcsdata$cgage25to71women[awcsdata$bachelorsmin==1&awcsdata$age25to71women==1]<-1
awcsdata$cgage25to71women
sum(awcsdata$cgage25to71women)

awcsdata$cgunderage35men=0
awcsdata$cgunderage35men[awcsdata$bachelorsmin==1&awcsdata$underage35men==1]<-1
awcsdata$cgunderage35men
sum(awcsdata$cgunderage35men)

awcsdata$cgunderage35women=0
awcsdata$cgunderage35women[awcsdata$bachelorsmin==1&awcsdata$underage35women==1]<-1
awcsdata$cgunderage35women
sum(awcsdata$cgunderage35women)

awcsdata$cgage35to49men=0
awcsdata$cgage35to49men[awcsdata$bachelorsmin==1&awcsdata$age35to49men==1]<-1
awcsdata$cgage35to49men
sum(awcsdata$cgage35to49men)

awcsdata$cgage35to49women=0
awcsdata$cgage35to49women[awcsdata$bachelorsmin==1&awcsdata$age35to49women==1]<-1
awcsdata$cgage35to49women
sum(awcsdata$cgage35to49women)

awcsdata$cgoverage50men=0
awcsdata$cgoverage50men[awcsdata$bachelorsmin==1&awcsdata$overage50men==1]<-1
awcsdata$cgoverage50men
sum(awcsdata$cgoverage50men)

awcsdata$cgoverage50women=0
awcsdata$cgoverage50women[awcsdata$bachelorsmin==1&awcsdata$overage50women==1]<-1
awcsdata$cgoverage50women
sum(awcsdata$cgoverage50women)

awcstable2
awcstable2$Menreplicate=0
awcstable2$Womenreplicate=0
awcstable2$non.college.Menreplicate=0
awcstable2$non.college.Womenreplicate=0
awcstable2$college.Menreplicate=0
awcstable2$college.Womenreplicate=0
awcstable2

awcstable2[1,9]=sum(awcsdata$age25to71)
awcstable2[2,9]=sum(awcsdata$underage35)
awcstable2[3,9]=sum(awcsdata$age35to49)
awcstable2[4,9]=sum(awcsdata$overage50)
awcstable2

awcstable2[1,10]=sum(awcsdata$age25to71men)
awcstable2[2,10]=sum(awcsdata$underage35men)
awcstable2[3,10]=sum(awcsdata$age35to49men)
awcstable2[4,10]=sum(awcsdata$overage50men)
awcstable2

awcstable2[1,11]=sum(awcsdata$age25to71women)
awcstable2[2,11]=sum(awcsdata$underage35women)
awcstable2[3,11]=sum(awcsdata$age35to49women)
awcstable2[4,11]=sum(awcsdata$overage50women)
awcstable2

awcstable2[1,12]=sum(awcsdata$ncgage25to71men)
awcstable2[2,12]=sum(awcsdata$ncgunderage35men)
awcstable2[3,12]=sum(awcsdata$ncgage35to49men)
awcstable2[4,12]=sum(awcsdata$ncgoverage50men)
awcstable2

awcstable2[1,13]=sum(awcsdata$ncgage25to71women)
awcstable2[2,13]=sum(awcsdata$ncgunderage35women)
awcstable2[3,13]=sum(awcsdata$ncgage35to49women)
awcstable2[4,13]=sum(awcsdata$ncgoverage50women)
awcstable2

awcstable2[1,14]=sum(awcsdata$cgage25to71men)
awcstable2[2,14]=sum(awcsdata$cgunderage35men)
awcstable2[3,14]=sum(awcsdata$cgage35to49men)
awcstable2[4,14]=sum(awcsdata$cgoverage50men)
awcstable2

awcstable2[1,15]=sum(awcsdata$cgage25to71women)
awcstable2[2,15]=sum(awcsdata$cgunderage35women)
awcstable2[3,15]=sum(awcsdata$cgage35to49women)
awcstable2[4,15]=sum(awcsdata$cgoverage50women)
awcstable2

stargazer(awcstable2, summary=FALSE, rownames=TRUE)

#////////////////////////////////////////////////////

awcstable3=read.csv("AWCSTable2.5.csv")
awcstable3
awcstable3$Overallreplicate=0
awcstable3

sum(awcsdata$working)

awcsdata$workingunderage35=0
awcsdata$workingunderage35[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1]<-1
awcsdata$workingunderage35
sum(awcsdata$workingunderage35)

awcsdata$workingage35to49=0
awcsdata$workingage35to49[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1]<-1
awcsdata$workingage35to49
sum(awcsdata$workingage35to49)

awcsdata$workingoverage50=0
awcsdata$workingoverage50[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1]<-1
awcsdata$workingoverage50
sum(awcsdata$workingoverage50)

awcsdata$workingmen=0
awcsdata$workingmen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                 &awcsdata$female==0]<-1
awcsdata$workingmen
sum(awcsdata$workingmen)

awcsdata$workingwomen=0
awcsdata$workingwomen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                    &awcsdata$female==1]<-1
awcsdata$workingwomen
sum(awcsdata$workingwomen)

awcsdata$workingunderage35men=0
awcsdata$workingunderage35men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
&awcsdata$female==0]<-1
awcsdata$workingunderage35men
sum(awcsdata$workingunderage35men)

awcsdata$workingunderage35women=0
awcsdata$workingunderage35women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
                              &awcsdata$female==1]<-1
awcsdata$workingunderage35women
sum(awcsdata$workingunderage35women)

awcsdata$workingage35to49men=0
awcsdata$workingage35to49men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                              &awcsdata$female==0]<-1
awcsdata$workingage35to49men
sum(awcsdata$workingage35to49men)

awcsdata$workingage35to49women=0
awcsdata$workingage35to49women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                             &awcsdata$female==1]<-1
awcsdata$workingage35to49women
sum(awcsdata$workingage35to49women)

awcsdata$workingoverage50men=0
awcsdata$workingoverage50men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                              &awcsdata$female==0]<-1
awcsdata$workingoverage50men
sum(awcsdata$workingoverage50men)

awcsdata$workingoverage50women=0
awcsdata$workingoverage50women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                             &awcsdata$female==1]<-1
awcsdata$workingoverage50women
sum(awcsdata$workingoverage50women)



awcsdata$workingncgmen=0
awcsdata$workingncgmen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                       &awcsdata$female==0&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgmen
sum(awcsdata$workingncgmen)

awcsdata$workingncgwomen=0
awcsdata$workingncgwomen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                         &awcsdata$female==1&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgwomen
sum(awcsdata$workingncgwomen)

awcsdata$workingncgunderage35men=0
awcsdata$workingncgunderage35men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
                                 &awcsdata$female==0&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgunderage35men
sum(awcsdata$workingncgunderage35men)

awcsdata$workingncgunderage35women=0
awcsdata$workingncgunderage35women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
                                   &awcsdata$female==1&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgunderage35women
sum(awcsdata$workingncgunderage35women)

awcsdata$workingncgage35to49men=0
awcsdata$workingncgage35to49men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                                &awcsdata$female==0&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgage35to49men
sum(awcsdata$workingncgage35to49men)

awcsdata$workingncgage35to49women=0
awcsdata$workingncgage35to49women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                                  &awcsdata$female==1&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgage35to49women
sum(awcsdata$workingncgage35to49women)

awcsdata$workingncgoverage50men=0
awcsdata$workingncgoverage50men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                                &awcsdata$female==0&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgoverage50men
sum(awcsdata$workingncgoverage50men)

awcsdata$workingncgoverage50women=0
awcsdata$workingncgoverage50women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                                  &awcsdata$female==1&awcsdata$bachelorsmin==0]<-1
awcsdata$workingncgoverage50women
sum(awcsdata$workingncgoverage50women)


awcsdata$workingcgmen=0
awcsdata$workingcgmen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                      &awcsdata$female==0&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgmen
sum(awcsdata$workingcgmen)

awcsdata$workingcgwomen=0
awcsdata$workingcgwomen[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age25to71==1
                        &awcsdata$female==1&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgwomen
sum(awcsdata$workingcgwomen)

awcsdata$workingcgunderage35men=0
awcsdata$workingcgunderage35men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
                                &awcsdata$female==0&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgunderage35men
sum(awcsdata$workingcgunderage35men)

awcsdata$workingcgunderage35women=0
awcsdata$workingcgunderage35women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$underage35==1
                                  &awcsdata$female==1&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgunderage35women
sum(awcsdata$workingcgunderage35women)

awcsdata$workingcgage35to49men=0
awcsdata$workingcgage35to49men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                               &awcsdata$female==0&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgage35to49men
sum(awcsdata$workingcgage35to49men)

awcsdata$workingcgage35to49women=0
awcsdata$workingcgage35to49women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$age35to49==1
                                 &awcsdata$female==1&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgage35to49women
sum(awcsdata$workingcgage35to49women)

awcsdata$workingcgoverage50men=0
awcsdata$workingcgoverage50men[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                               &awcsdata$female==0&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgoverage50men
sum(awcsdata$workingcgoverage50men)

awcsdata$workingcgoverage50women=0
awcsdata$workingcgoverage50women[(awcsdata$ms436_currentjobstatuss1==1|awcsdata$ms436_currentjobstatuss3==3)&awcsdata$overage50==1
                                 &awcsdata$female==1&awcsdata$bachelorsmin==1]<-1
awcsdata$workingcgoverage50women
sum(awcsdata$workingcgoverage50women)


awcstable3
awcstable3$Menreplicate=0
awcstable3$Womenreplicate=0
awcstable3$non.college.Menreplicate=0
awcstable3$non.college.Womenreplicate=0
awcstable3$college.Menreplicate=0
awcstable3$college.Womenreplicate=0
awcstable3


awcstable3[1,9]=sum(awcsdata$working)
awcstable3[2,9]=sum(awcsdata$workingunderage35)
awcstable3[3,9]=sum(awcsdata$workingage35to49)
awcstable3[4,9]=sum(awcsdata$workingoverage50)
awcstable3

awcstable3[1,10]=sum(awcsdata$workingmen)
awcstable3[2,10]=sum(awcsdata$workingunderage35men)
awcstable3[3,10]=sum(awcsdata$workingage35to49men)
awcstable3[4,10]=sum(awcsdata$workingoverage50men)
awcstable3

awcstable3[1,11]=sum(awcsdata$workingwomen)
awcstable3[2,11]=sum(awcsdata$workingunderage35women)
awcstable3[3,11]=sum(awcsdata$workingage35to49women)
awcstable3[4,11]=sum(awcsdata$workingoverage50women)
awcstable3

awcstable3[1,12]=sum(awcsdata$workingncgmen)
awcstable3[2,12]=sum(awcsdata$workingncgunderage35men)
awcstable3[3,12]=sum(awcsdata$workingncgage35to49men)
awcstable3[4,12]=sum(awcsdata$workingncgoverage50men)
awcstable3

awcstable3[1,13]=sum(awcsdata$workingncgwomen)
awcstable3[2,13]=sum(awcsdata$workingncgunderage35women)
awcstable3[3,13]=sum(awcsdata$workingncgage35to49women)
awcstable3[4,13]=sum(awcsdata$workingncgoverage50women)
awcstable3

awcstable3[1,14]=sum(awcsdata$workingcgmen)
awcstable3[2,14]=sum(awcsdata$workingcgunderage35men)
awcstable3[3,14]=sum(awcsdata$workingcgage35to49men)
awcstable3[4,14]=sum(awcsdata$workingcgoverage50men)
awcstable3

awcstable3[1,15]=sum(awcsdata$workingcgwomen)
awcstable3[2,15]=sum(awcsdata$workingcgunderage35women)
awcstable3[3,15]=sum(awcsdata$workingcgage35to49women)
awcstable3[4,15]=sum(awcsdata$workingcgoverage50women)
awcstable3

stargazer(awcstable3, summary=FALSE, rownames=TRUE)



# Work for 9/25
awcsdata$female[awcsdata$ms436_gender==2]<-1
sum(awcsdata$female)
awcsdata$underage35men=0
awcsdata$underage35men[awcsdata$ms436_calcage<35&awcsdata$ms436_calcage>=25&awcsdata$female==0]<-1




awcstable4=read.csv("AWCSTable3.1.csv")
awcstable4

awcstable4$overallreplication=0
awcstable4$menreplication=0
awcstable4$womenreplication=0
awcstable4$ncgmenreplication=0
awcstable4$ncgwomenreplication=0
awcstable4$cgmenreplication=0
awcstable4$cgwomenreplication=0

awcsdata$unemployed=0
awcsdata$unemployed[awcsdata$ms436_currentjobstatuss2==2]<-1
awcsdata$notinlaborforce=0
awcsdata$notinlaborforce[awcsdata$ms436_currentjobstatuss2==5|awcsdata$ms436_currentjobstatuss2==6|
                           awcsdata$ms436_currentjobstatuss2==4]<-1

awcsdata$unemployedallages=0
awcsdata$unemployedallages[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71]<-1
sum(awcsdata$unemployedallages)
awcsdata$notinlaborforceallages=0
awcsdata$notinlaborforceallages[awcsdata$ms436_currentjobstatuss5==5|awcsdata$ms436_currentjobstatuss6==6|
                                  awcsdata$ms436_currentjobstatuss4==4&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71]<-1
sum(awcsdata$notinlaborforceallages)

awcsdata$unemployedunderage35=0
awcsdata$unemployedunderage35[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35]<-1
sum(awcsdata$unemployedunderage35)
awcsdata$notinlaborforceunderage35=0
awcsdata$notinlaborforceunderage35[awcsdata$notinlaborforceallages==1&awcsdata$underage35==1]<-1
sum(awcsdata$notinlaborforceunderage35)
sum(awcsdata$underage35)

awcsdata$unemployedage35to49=0
awcsdata$unemployedage35to49[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49]<-1
sum(awcsdata$unemployedage35to49)
awcsdata$notinlaborforceage35to49=0
awcsdata$notinlaborforceage35to49[awcsdata$notinlaborforceallages==1&awcsdata$age35to49==1]<-1
sum(awcsdata$notinlaborforceage35to49)
sum(awcsdata$age35to49)
awcsdata$workingage35to49[is.na(awcsdata$workingage35to49)]=0
awcsdata$workingage35to49

awcsdata$unemployedoverage50=0
awcsdata$unemployedoverage50[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50]<-1
sum(awcsdata$unemployedoverage50)
awcsdata$notinlaborforceoverage50=0
awcsdata$notinlaborforceoverage50[awcsdata$notinlaborforceallages==1&awcsdata$overage50==1]<-1
sum(awcsdata$notinlaborforceoverage50)
sum(awcsdata$overage50)

awcsdata$unemployedallagesmen=0
awcsdata$unemployedallagesmen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$female==0]<-1
sum(awcsdata$unemployedallagesmen)
awcsdata$notinlaborforceallagesmen=0
awcsdata$notinlaborforceallagesmen[awcsdata$notinlaborforceallages&awcsdata$female==0]<-1
sum(awcsdata$notinlaborforceallagesmen)



awcsdata$unemployedunderage35men=0
awcsdata$unemployedunderage35men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$female==0]<-1
sum(awcsdata$unemployedunderage35men)
awcsdata$notinlaborforceunderage35men=0
awcsdata$notinlaborforceunderage35men[awcsdata$notinlaborforceunderage35==1&awcsdata$female==0]<-1
sum(awcsdata$notinlaborforceunderage35men)
sum(awcsdata$underage35men)


awcsdata$unemployedage35to49men=0
awcsdata$unemployedage35to49men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$female==0]<-1
sum(awcsdata$unemployedage35to49men)
awcsdata$notinlaborforceage35to49men=0
awcsdata$notinlaborforceage35to49men[awcsdata$notinlaborforceallagesmen==1&awcsdata$age35to49men==1]<-1
sum(awcsdata$notinlaborforceage35to49men)
sum(awcsdata$age35to49men)
awcsdata$workingage35to49men[is.na(awcsdata$workingage35to49men)]=0
awcsdata$workingage35to49men

awcsdata$unemployedoverage50men=0
awcsdata$unemployedoverage50men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$female==0]<-1
sum(awcsdata$unemployedoverage50men)
awcsdata$notinlaborforceoverage50men=0
awcsdata$notinlaborforceoverage50men[awcsdata$notinlaborforceallagesmen==1&awcsdata$overage50men==1]<-1
sum(awcsdata$notinlaborforceoverage50men)
sum(awcsdata$overage50men)


awcsdata$unemployedallageswomen=0
awcsdata$unemployedallageswomen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$female==1]<-1
sum(awcsdata$unemployedallageswomen)
awcsdata$notinlaborforceallageswomen=0
awcsdata$notinlaborforceallageswomen[awcsdata$notinlaborforceallages&awcsdata$female==1]<-1
sum(awcsdata$notinlaborforceallageswomen)



awcsdata$unemployedunderage35women=0
awcsdata$unemployedunderage35women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$female==1]<-1
sum(awcsdata$unemployedunderage35women)
awcsdata$notinlaborforceunderage35women=0
awcsdata$notinlaborforceunderage35women[awcsdata$notinlaborforceunderage35==1&awcsdata$female==1]<-1
sum(awcsdata$notinlaborforceunderage35women)
sum(awcsdata$underage35women)


awcsdata$unemployedage35to49women=0
awcsdata$unemployedage35to49women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$female==1]<-1
sum(awcsdata$unemployedage35to49women)
awcsdata$notinlaborforceage35to49women=0
awcsdata$notinlaborforceage35to49women[awcsdata$notinlaborforceallageswomen==1&awcsdata$age35to49women==1]<-1
sum(awcsdata$notinlaborforceage35to49women)
sum(awcsdata$age35to49women)
awcsdata$workingage35to49women[is.na(awcsdata$workingage35to49women)]=0
awcsdata$workingage35to49women

awcsdata$unemployedoverage50women=0
awcsdata$unemployedoverage50women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$female==1]<-1
sum(awcsdata$unemployedoverage50women)
awcsdata$notinlaborforceoverage50women=0
awcsdata$notinlaborforceoverage50women[awcsdata$notinlaborforceallageswomen==1&awcsdata$overage50women==1]<-1
sum(awcsdata$notinlaborforceoverage50women)
sum(awcsdata$overage50women)



awcsdata$unemployedncgallagesmen=0
awcsdata$unemployedncgallagesmen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$ncgage25to71men==1]<-1
sum(awcsdata$unemployedncgallagesmen)
awcsdata$notinlaborforcencgallagesmen=0
awcsdata$notinlaborforcencgallagesmen[awcsdata$notinlaborforceallages==1& awcsdata$ncgage25to71men ==1]<-1
sum(awcsdata$notinlaborforcencgallagesmen)

awcsdata$unemployedncgunderage35men=0
awcsdata$unemployedncgunderage35men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$ncgunderage35men==1]<-1
sum(awcsdata$unemployedncgunderage35men)
awcsdata$notinlaborforcencgunderage35men=0
awcsdata$notinlaborforcencgunderage35men[awcsdata$notinlaborforceunderage35==1& awcsdata$ncgunderage35men==1]<-1
sum(awcsdata$notinlaborforcencgunderage35men)
sum(awcsdata$underage35men)


awcsdata$unemployedncgage35to49men=0
awcsdata$unemployedncgage35to49men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$ncgage35to49men]<-1
sum(awcsdata$unemployedncgage35to49men)
awcsdata$notinlaborforcencgage35to49men=0
awcsdata$notinlaborforcencgage35to49men[awcsdata$notinlaborforcencgallagesmen==1&awcsdata$ncgage35to49men==1]<-1
sum(awcsdata$notinlaborforcencgage35to49men)
sum(awcsdata$age35to49men)

awcsdata$unemployedncgoverage50men=0
awcsdata$unemployedncgoverage50men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$ncgoverage50men==1]<-1
sum(awcsdata$unemployedncgoverage50men)
awcsdata$notinlaborforcencgoverage50men=0
awcsdata$notinlaborforcencgoverage50men[awcsdata$notinlaborforcencgallagesmen==1&awcsdata$ncgoverage50men==1]<-1
sum(awcsdata$notinlaborforcencgoverage50men)
sum(awcsdata$overage50men)



awcsdata$unemployedncgallageswomen=0
awcsdata$unemployedncgallageswomen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$ncgage25to71women==1]<-1
sum(awcsdata$unemployedncgallageswomen)
awcsdata$notinlaborforcencgallageswomen=0
awcsdata$notinlaborforcencgallageswomen[awcsdata$notinlaborforceallages==1& awcsdata$ncgage25to71women ==1]<-1
sum(awcsdata$notinlaborforcencgallageswomen)

awcsdata$unemployedncgunderage35women=0
awcsdata$unemployedncgunderage35women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$ncgunderage35women==1]<-1
sum(awcsdata$unemployedncgunderage35women)
awcsdata$notinlaborforcencgunderage35women=0
awcsdata$notinlaborforcencgunderage35women[awcsdata$notinlaborforceunderage35==1& awcsdata$ncgunderage35women==1]<-1
sum(awcsdata$notinlaborforcencgunderage35women)
sum(awcsdata$underage35women)


awcsdata$unemployedncgage35to49women=0
awcsdata$unemployedncgage35to49women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$ncgage35to49women]<-1
sum(awcsdata$unemployedncgage35to49women)
awcsdata$notinlaborforcencgage35to49women=0
awcsdata$notinlaborforcencgage35to49women[awcsdata$notinlaborforcencgallageswomen==1&awcsdata$ncgage35to49women==1]<-1
sum(awcsdata$notinlaborforcencgage35to49women)
sum(awcsdata$age35to49women)

awcsdata$unemployedncgoverage50women=0
awcsdata$unemployedncgoverage50women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$ncgoverage50women==1]<-1
sum(awcsdata$unemployedncgoverage50women)
awcsdata$notinlaborforcencgoverage50women=0
awcsdata$notinlaborforcencgoverage50women[awcsdata$notinlaborforcencgallageswomen==1&awcsdata$ncgoverage50women==1]<-1
sum(awcsdata$notinlaborforcencgoverage50women)
sum(awcsdata$overage50women)


awcsdata$unemployedcgallagesmen=0
awcsdata$unemployedcgallagesmen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$cgage25to71men==1]<-1
sum(awcsdata$unemployedcgallagesmen)
awcsdata$notinlaborforcecgallagesmen=0
awcsdata$notinlaborforcecgallagesmen[awcsdata$notinlaborforceallages==1& awcsdata$cgage25to71men ==1]<-1
sum(awcsdata$notinlaborforcecgallagesmen)

awcsdata$unemployedcgunderage35men=0
awcsdata$unemployedcgunderage35men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$cgunderage35men==1]<-1
sum(awcsdata$unemployedcgunderage35men)
awcsdata$notinlaborforcecgunderage35men=0
awcsdata$notinlaborforcecgunderage35men[awcsdata$notinlaborforceunderage35==1& awcsdata$cgunderage35men==1]<-1
sum(awcsdata$notinlaborforcecgunderage35men)
sum(awcsdata$underage35men)


awcsdata$unemployedcgage35to49men=0
awcsdata$unemployedcgage35to49men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$cgage35to49men]<-1
sum(awcsdata$unemployedcgage35to49men)
awcsdata$notinlaborforcecgage35to49men=0
awcsdata$notinlaborforcecgage35to49men[awcsdata$notinlaborforcecgallagesmen==1&awcsdata$cgage35to49men==1]<-1
sum(awcsdata$notinlaborforcecgage35to49men)
sum(awcsdata$age35to49men)

awcsdata$unemployedcgoverage50men=0
awcsdata$unemployedcgoverage50men[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$cgoverage50men==1]<-1
sum(awcsdata$unemployedcgoverage50men)
awcsdata$notinlaborforcecgoverage50men=0
awcsdata$notinlaborforcecgoverage50men[awcsdata$notinlaborforcecgallagesmen==1&awcsdata$cgoverage50men==1]<-1
sum(awcsdata$notinlaborforcecgoverage50men)
sum(awcsdata$overage50men)



awcsdata$unemployedcgallageswomen=0
awcsdata$unemployedcgallageswomen[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<=71&awcsdata$cgage25to71women==1]<-1
sum(awcsdata$unemployedcgallageswomen)
awcsdata$notinlaborforcecgallageswomen=0
awcsdata$notinlaborforcecgallageswomen[awcsdata$notinlaborforceallages==1& awcsdata$cgage25to71women ==1]<-1
sum(awcsdata$notinlaborforcecgallageswomen)

awcsdata$unemployedcgunderage35women=0
awcsdata$unemployedcgunderage35women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage<35&awcsdata$cgunderage35women==1]<-1
sum(awcsdata$unemployedcgunderage35women)
awcsdata$notinlaborforcecgunderage35women=0
awcsdata$notinlaborforcecgunderage35women[awcsdata$notinlaborforceunderage35==1& awcsdata$cgunderage35women==1]<-1
sum(awcsdata$notinlaborforcecgunderage35women)
sum(awcsdata$underage35women)


awcsdata$unemployedcgage35to49women=0
awcsdata$unemployedcgage35to49women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=35&awcsdata$ms436_calcage<49&awcsdata$cgage35to49women]<-1
sum(awcsdata$unemployedcgage35to49women)
awcsdata$notinlaborforcecgage35to49women=0
awcsdata$notinlaborforcecgage35to49women[awcsdata$notinlaborforcecgallageswomen==1&awcsdata$cgage35to49women==1]<-1
sum(awcsdata$notinlaborforcecgage35to49women)
sum(awcsdata$age35to49women)

awcsdata$unemployedcgoverage50women=0
awcsdata$unemployedcgoverage50women[awcsdata$ms436_currentjobstatuss2==2&awcsdata$ms436_calcage>=25&awcsdata$ms436_calcage>=50&awcsdata$cgoverage50women==1]<-1
sum(awcsdata$unemployedcgoverage50women)
awcsdata$notinlaborforcecgoverage50women=0
awcsdata$notinlaborforcecgoverage50women[awcsdata$notinlaborforcecgallageswomen==1&awcsdata$cgoverage50women==1]<-1
sum(awcsdata$notinlaborforcecgoverage50women)
sum(awcsdata$overage50women)



awcstable4[1,9]=sum(awcsdata$workingallages)/sum(awcsdata$age25to71)
awcstable4[2,9]=sum(awcsdata$unemployedallages)/sum(awcsdata$age25to71)
awcstable4[3,9]=sum(awcsdata$notinlaborforceallages)/sum(awcsdata$age25to71)
awcstable4[4,9]=sum(awcsdata$workingunderage35)/sum(awcsdata$underage35)
awcstable4[5,9]=sum(awcsdata$unemployedunderage35)/sum(awcsdata$underage35)
awcstable4[6,9]=sum(awcsdata$notinlaborforceunderage35)/sum(awcsdata$underage35)
awcstable4[7,9]=sum(awcsdata$workingage35to49)/sum(awcsdata$age35to49)
awcstable4[8,9]=sum(awcsdata$unemployedage35to49)/sum(awcsdata$age35to49)
awcstable4[9,9]=sum(awcsdata$notinlaborforceage35to49)/sum(awcsdata$age35to49)
awcstable4[10,9]=sum(awcsdata$workingoverage50)/sum(awcsdata$overage50)
awcstable4[11,9]=sum(awcsdata$unemployedoverage50)/sum(awcsdata$overage50)
awcstable4[12,9]=sum(awcsdata$notinlaborforceoverage50)/sum(awcsdata$overage50)
awcstable4[1,10]=sum(awcsdata$workingmen)/sum(awcsdata$age25to71men)
awcstable4[2,10]=sum(awcsdata$unemployedallagesmen)/sum(awcsdata$age25to71men)
awcstable4[3,10]=sum(awcsdata$notinlaborforceallagesmen)/sum(awcsdata$age25to71men)
awcstable4[4,10]=sum(awcsdata$workingunderage35men)/sum(awcsdata$underage35men)
awcstable4[5,10]=sum(awcsdata$unemployedunderage35men)/sum(awcsdata$underage35men)
awcstable4[6,10]=sum(awcsdata$notinlaborforceunderage35men)/sum(awcsdata$underage35men)
awcstable4[7,10]=sum(awcsdata$workingage35to49men)/sum(awcsdata$age35to49men)
awcstable4[8,10]=sum(awcsdata$unemployedage35to49men)/sum(awcsdata$age35to49men)
awcstable4[9,10]=sum(awcsdata$notinlaborforceage35to49men)/sum(awcsdata$age35to49men)
awcstable4[10,10]=sum(awcsdata$workingoverage50men)/sum(awcsdata$overage50men)
awcstable4[11,10]=sum(awcsdata$unemployedoverage50men)/sum(awcsdata$overage50men)
awcstable4[12,10]=sum(awcsdata$notinlaborforceoverage50men)/sum(awcsdata$overage50men)
awcstable4[1,11]=sum(awcsdata$workingwomen)/sum(awcsdata$age25to71women)
awcstable4[2,11]=sum(awcsdata$unemployedallageswomen)/sum(awcsdata$age25to71women)
awcstable4[3,11]=sum(awcsdata$notinlaborforceallageswomen)/sum(awcsdata$age25to71women)
awcstable4[4,11]=sum(awcsdata$workingunderage35women)/sum(awcsdata$underage35women)
awcstable4[5,11]=sum(awcsdata$unemployedunderage35women)/sum(awcsdata$underage35women)
awcstable4[6,11]=sum(awcsdata$notinlaborforceunderage35women)/sum(awcsdata$underage35women)
awcstable4[7,11]=sum(awcsdata$workingage35to49women)/sum(awcsdata$age35to49women)
awcstable4[8,11]=sum(awcsdata$unemployedage35to49women)/sum(awcsdata$age35to49women)
awcstable4[9,11]=sum(awcsdata$notinlaborforceage35to49women)/sum(awcsdata$age35to49women)
awcstable4[10,11]=sum(awcsdata$workingoverage50women)/sum(awcsdata$overage50women)
awcstable4[11,11]=sum(awcsdata$unemployedoverage50women)/sum(awcsdata$overage50women)
awcstable4[12,11]=sum(awcsdata$notinlaborforceoverage50women)/sum(awcsdata$overage50women)
awcstable4[1,12]=sum(awcsdata$workingncgmen)/sum(awcsdata$ncgage25to71men)
awcstable4[2,12]=sum(awcsdata$unemployedncgallagesmen)/sum(awcsdata$ncgage25to71men)
awcstable4[3,12]=sum(awcsdata$notinlaborforcencgallagesmen)/sum(awcsdata$ncgage25to71men)
awcstable4[4,12]=sum(awcsdata$workingncgunderage35men)/sum(awcsdata$ncgunderage35men)
awcstable4[5,12]=sum(awcsdata$unemployedncgunderage35men)/sum(awcsdata$ncgunderage35men)
awcstable4[6,12]=sum(awcsdata$notinlaborforcencgunderage35men)/sum(awcsdata$ncgunderage35men)
awcstable4[7,12]=sum(awcsdata$workingncgage35to49men)/sum(awcsdata$ncgage35to49men)
awcstable4[8,12]=sum(awcsdata$unemployedncgage35to49men)/sum(awcsdata$ncgage35to49men)
awcstable4[9,12]=sum(awcsdata$notinlaborforcencgage35to49men)/sum(awcsdata$ncgage35to49men)
awcstable4[10,12]=sum(awcsdata$workingncgoverage50men)/sum(awcsdata$ncgoverage50men)
awcstable4[11,12]=sum(awcsdata$unemployedncgoverage50men)/sum(awcsdata$ncgoverage50men)
awcstable4[12,12]=sum(awcsdata$notinlaborforcencgoverage50men)/sum(awcsdata$ncgoverage50men)
awcstable4[1,13]=sum(awcsdata$workingncgwomen)/sum(awcsdata$ncgage25to71women)
awcstable4[2,13]=sum(awcsdata$unemployedncgallageswomen)/sum(awcsdata$ncgage25to71women)
awcstable4[3,13]=sum(awcsdata$notinlaborforcencgallageswomen)/sum(awcsdata$ncgage25to71women)
awcstable4[4,13]=sum(awcsdata$workingncgunderage35women)/sum(awcsdata$ncgunderage35women)
awcstable4[5,13]=sum(awcsdata$unemployedncgunderage35women)/sum(awcsdata$ncgunderage35women)
awcstable4[6,13]=sum(awcsdata$notinlaborforcencgunderage35women)/sum(awcsdata$ncgunderage35women)
awcstable4[7,13]=sum(awcsdata$workingncgage35to49women)/sum(awcsdata$ncgage35to49women)
awcstable4[8,13]=sum(awcsdata$unemployedncgage35to49women)/sum(awcsdata$ncgage35to49women)
awcstable4[9,13]=sum(awcsdata$notinlaborforcencgage35to49women)/sum(awcsdata$ncgage35to49women)
awcstable4[10,13]=sum(awcsdata$workingncgoverage50women)/sum(awcsdata$ncgoverage50women)
awcstable4[11,13]=sum(awcsdata$unemployedncgoverage50women)/sum(awcsdata$ncgoverage50women)
awcstable4[12,13]=sum(awcsdata$notinlaborforcencgoverage50women)/sum(awcsdata$ncgoverage50women)
awcstable4[1,14]=sum(awcsdata$workingcgmen)/sum(awcsdata$cgage25to71men)
awcstable4[2,14]=sum(awcsdata$unemployedcgallagesmen)/sum(awcsdata$cgage25to71men)
awcstable4[3,14]=sum(awcsdata$notinlaborforcecgallagesmen)/sum(awcsdata$cgage25to71men)
awcstable4[4,14]=sum(awcsdata$workingcgunderage35men)/sum(awcsdata$cgunderage35men)
awcstable4[5,14]=sum(awcsdata$unemployedcgunderage35men)/sum(awcsdata$cgunderage35men)
awcstable4[6,14]=sum(awcsdata$notinlaborforcecgunderage35men)/sum(awcsdata$cgunderage35men)
awcstable4[7,14]=sum(awcsdata$workingcgage35to49men)/sum(awcsdata$cgage35to49men)
awcstable4[8,14]=sum(awcsdata$unemployedcgage35to49men)/sum(awcsdata$cgage35to49men)
awcstable4[9,14]=sum(awcsdata$notinlaborforcecgage35to49men)/sum(awcsdata$cgage35to49men)
awcstable4[10,14]=sum(awcsdata$workingcgoverage50men)/sum(awcsdata$cgoverage50men)
awcstable4[11,14]=sum(awcsdata$unemployedcgoverage50men)/sum(awcsdata$cgoverage50men)
awcstable4[12,14]=sum(awcsdata$notinlaborforcecgoverage50men)/sum(awcsdata$cgoverage50men)
awcstable4[1,15]=sum(awcsdata$workingcgwomen)/sum(awcsdata$cgage25to71women)
awcstable4[2,15]=sum(awcsdata$unemployedcgallageswomen)/sum(awcsdata$cgage25to71women)
awcstable4[3,15]=sum(awcsdata$notinlaborforcecgallageswomen)/sum(awcsdata$cgage25to71women)
awcstable4[4,15]=sum(awcsdata$workingcgunderage35women)/sum(awcsdata$cgunderage35women)
awcstable4[5,15]=sum(awcsdata$unemployedcgunderage35women)/sum(awcsdata$cgunderage35women)
awcstable4[6,15]=sum(awcsdata$notinlaborforcecgunderage35women)/sum(awcsdata$cgunderage35women)
awcstable4[7,15]=sum(awcsdata$workingcgage35to49women)/sum(awcsdata$cgage35to49women)
awcstable4[8,15]=sum(awcsdata$unemployedcgage35to49women)/sum(awcsdata$cgage35to49women)
awcstable4[9,15]=sum(awcsdata$notinlaborforcecgage35to49women)/sum(awcsdata$cgage35to49women)
awcstable4[10,15]=sum(awcsdata$workingcgoverage50women)/sum(awcsdata$cgoverage50women)
awcstable4[11,15]=sum(awcsdata$unemployedcgoverage50women)/sum(awcsdata$cgoverage50women)
awcstable4[12,15]=sum(awcsdata$notinlaborforcecgoverage50women)/sum(awcsdata$cgoverage50women)

awcsdata$ms436_q18[is.na(awcsdata$ms436_q18)]=0
awcsdata$ms436_q22[is.na(awcsdata$ms436_q22)]=0



awcsdata$selfemployed=0
awcsdata$selfemployed[awcsdata$ms436_q6==2]<-1
awcsdata$multiplejobs=0
awcsdata$multiplejobs[awcsdata$ms436_q21_emp==1|awcsdata$ms436_q21_se==1]<-1
awcsdata$avgweeklyhoursall=0
awcsdata$avgweeklyhoursall=awcsdata$ms436_q18+awcsdata$ms436_q22
awcsdata$avgweeklyhoursmain=0
awcsdata$avgweeklyhoursmain=awcsdata$avgweeklyhoursall-awcsdata$ms436_q22
awcsdata$parttime=0
awcsdata$parttime[awcsdata$ms436_q18+awcsdata$ms436_q22<35&awcsdata$ms436_q1==1&awcsdata$additional==1]<-1
awcsdata$avweeklyhoursall=awcsdata$ms436_q18+awcsdata$ms436_q22
awcsdata$longhours=0
awcsdata$longhours[awcsdata$avgweeklyhoursall>=48]<-1
awcsdata$freqlongdays=0
awcsdata$freqlongdays[awcsdata$ms436_q36>=10]<-1

awcsdata$parttime

awcstable5=read.csv("AWCSTable3.2.csv")
awcstable5
awcstable5=awcstable5[1:28,]

awcstable5$overallreplication=0
awcstable5$menreplication=0
awcstable5$womenreplication=0
awcstable5$ncgmenreplication=0
awcstable5$ncgwomenreplication=0
awcstable5$cgmenreplication=0
awcstable5$cgwomenreplication=0
awcstable5

awcsdata$additional=0
awcsdata$additional[awcsdata$ms436_additional==1]<-1

sum(awcsdata$avgweeklyhoursmain)


awcstable5[1,9]=sum(awcsdata$selfemployed[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,9]=sum(awcsdata$multiplejobs[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,9]=sum(awcsdata$parttime[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,9]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,9]=sum(awcsdata$avgweeklyhoursall[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,9]=sum(awcsdata$longhours[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,9]=sum(awcsdata$freqlongdays[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,9]=sum(awcsdata$selfemployed[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,9]=sum(awcsdata$multiplejobs[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,9]=sum(awcsdata$parttime[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,9]=sum(awcsdata$avgweeklyhoursmain[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,9]=sum(awcsdata$avgweeklyhoursall[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,9]=sum(awcsdata$longhours[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,9]=sum(awcsdata$freqlongdays[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,9]=sum(awcsdata$selfemployed[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,9]=sum(awcsdata$multiplejobs[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,9]=sum(awcsdata$parttime[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,9]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,9]=sum(awcsdata$avgweeklyhoursall[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,9]=sum(awcsdata$longhours[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,9]=sum(awcsdata$freqlongdays[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,9]=sum(awcsdata$selfemployed[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,9]=sum(awcsdata$multiplejobs[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,9]=sum(awcsdata$parttime[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,9]=sum(awcsdata$avgweeklyhoursmain[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,9]=sum(awcsdata$avgweeklyhoursall[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,9]=sum(awcsdata$longhours[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,9]=sum(awcsdata$freqlongdays[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,10]=sum(awcsdata$selfemployed[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,10]=sum(awcsdata$multiplejobs[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,10]=sum(awcsdata$parttime[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,10]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,10]=sum(awcsdata$avgweeklyhoursall[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,10]=sum(awcsdata$longhours[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,10]=sum(awcsdata$freqlongdays[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,10]=sum(awcsdata$selfemployed[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,10]=sum(awcsdata$multiplejobs[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,10]=sum(awcsdata$parttime[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,10]=sum(awcsdata$avgweeklyhoursmain[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,10]=sum(awcsdata$avgweeklyhoursall[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,10]=sum(awcsdata$longhours[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,10]=sum(awcsdata$freqlongdays[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,10]=sum(awcsdata$selfemployed[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,10]=sum(awcsdata$multiplejobs[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,10]=sum(awcsdata$parttime[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,10]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,10]=sum(awcsdata$avgweeklyhoursall[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,10]=sum(awcsdata$longhours[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,10]=sum(awcsdata$freqlongdays[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,10]=sum(awcsdata$selfemployed[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,10]=sum(awcsdata$multiplejobs[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,10]=sum(awcsdata$parttime[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,10]=sum(awcsdata$avgweeklyhoursmain[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,10]=sum(awcsdata$avgweeklyhoursall[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,10]=sum(awcsdata$longhours[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,10]=sum(awcsdata$freqlongdays[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,11]=sum(awcsdata$selfemployed[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,11]=sum(awcsdata$multiplejobs[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,11]=sum(awcsdata$parttime[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,11]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,11]=sum(awcsdata$avgweeklyhoursall[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,11]=sum(awcsdata$longhours[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,11]=sum(awcsdata$freqlongdays[awcsdata$age25to71women==1])/sum(awcsdata$age25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,11]=sum(awcsdata$selfemployed[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,11]=sum(awcsdata$multiplejobs[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,11]=sum(awcsdata$parttime[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,11]=sum(awcsdata$avgweeklyhoursmain[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,11]=sum(awcsdata$avgweeklyhoursall[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,11]=sum(awcsdata$longhours[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,11]=sum(awcsdata$freqlongdays[awcsdata$underage35women==1])/sum(awcsdata$underage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,11]=sum(awcsdata$selfemployed[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,11]=sum(awcsdata$multiplejobs[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,11]=sum(awcsdata$parttime[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,11]=sum(awcsdata$avgweeklyhoursmain[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,11]=sum(awcsdata$avgweeklyhoursall[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,11]=sum(awcsdata$longhours[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,11]=sum(awcsdata$freqlongdays[awcsdata$age35to49women==1])/sum(awcsdata$age35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,11]=sum(awcsdata$selfemployed[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,11]=sum(awcsdata$multiplejobs[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,11]=sum(awcsdata$parttime[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,11]=sum(awcsdata$avgweeklyhoursmain[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,11]=sum(awcsdata$avgweeklyhoursall[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,11]=sum(awcsdata$longhours[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,11]=sum(awcsdata$freqlongdays[awcsdata$overage50women==1])/sum(awcsdata$overage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,12]=sum(awcsdata$selfemployed[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,12]=sum(awcsdata$multiplejobs[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,12]=sum(awcsdata$parttime[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,12]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,12]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,12]=sum(awcsdata$longhours[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,12]=sum(awcsdata$freqlongdays[awcsdata$ncgage25to71men==1])/sum(awcsdata$ncgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,12]=sum(awcsdata$selfemployed[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,12]=sum(awcsdata$multiplejobs[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,12]=sum(awcsdata$parttime[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,12]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,12]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,12]=sum(awcsdata$longhours[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,12]=sum(awcsdata$freqlongdays[awcsdata$ncgunderage35men==1])/sum(awcsdata$ncgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,12]=sum(awcsdata$selfemployed[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,12]=sum(awcsdata$multiplejobs[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,12]=sum(awcsdata$parttime[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,12]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,12]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,12]=sum(awcsdata$longhours[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,12]=sum(awcsdata$freqlongdays[awcsdata$ncgage35to49men==1])/sum(awcsdata$ncgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,12]=sum(awcsdata$selfemployed[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,12]=sum(awcsdata$multiplejobs[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,12]=sum(awcsdata$parttime[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,12]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,12]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,12]=sum(awcsdata$longhours[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,12]=sum(awcsdata$freqlongdays[awcsdata$ncgoverage50men==1])/sum(awcsdata$ncgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,13]=sum(awcsdata$selfemployed[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,13]=sum(awcsdata$multiplejobs[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,13]=sum(awcsdata$parttime[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,13]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,13]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,13]=sum(awcsdata$longhours[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,13]=sum(awcsdata$freqlongdays[awcsdata$ncgage25to71women==1])/sum(awcsdata$ncgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,13]=sum(awcsdata$selfemployed[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,13]=sum(awcsdata$multiplejobs[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,13]=sum(awcsdata$parttime[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,13]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,13]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,13]=sum(awcsdata$longhours[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,13]=sum(awcsdata$freqlongdays[awcsdata$ncgunderage35women==1])/sum(awcsdata$ncgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,13]=sum(awcsdata$selfemployed[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,13]=sum(awcsdata$multiplejobs[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,13]=sum(awcsdata$parttime[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,13]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,13]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,13]=sum(awcsdata$longhours[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,13]=sum(awcsdata$freqlongdays[awcsdata$ncgage35to49women==1])/sum(awcsdata$ncgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,13]=sum(awcsdata$selfemployed[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,13]=sum(awcsdata$multiplejobs[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,13]=sum(awcsdata$parttime[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,13]=sum(awcsdata$avgweeklyhoursmain[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,13]=sum(awcsdata$avgweeklyhoursall[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,13]=sum(awcsdata$longhours[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,13]=sum(awcsdata$freqlongdays[awcsdata$ncgoverage50women==1])/sum(awcsdata$ncgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,14]=sum(awcsdata$selfemployed[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,14]=sum(awcsdata$multiplejobs[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,14]=sum(awcsdata$parttime[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,14]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,14]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,14]=sum(awcsdata$longhours[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,14]=sum(awcsdata$freqlongdays[awcsdata$cgage25to71men==1])/sum(awcsdata$cgage25to71men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,14]=sum(awcsdata$selfemployed[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,14]=sum(awcsdata$multiplejobs[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,14]=sum(awcsdata$parttime[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,14]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,14]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,14]=sum(awcsdata$longhours[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,14]=sum(awcsdata$freqlongdays[awcsdata$cgunderage35men==1])/sum(awcsdata$cgunderage35men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,14]=sum(awcsdata$selfemployed[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,14]=sum(awcsdata$multiplejobs[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,14]=sum(awcsdata$parttime[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,14]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,14]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,14]=sum(awcsdata$longhours[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,14]=sum(awcsdata$freqlongdays[awcsdata$cgage35to49men==1])/sum(awcsdata$cgage35to49men[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,14]=sum(awcsdata$selfemployed[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,14]=sum(awcsdata$multiplejobs[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,14]=sum(awcsdata$parttime[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,14]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,14]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,14]=sum(awcsdata$longhours[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,14]=sum(awcsdata$freqlongdays[awcsdata$cgoverage50men==1])/sum(awcsdata$cgoverage50men[awcsdata$ms436_q1==1&awcsdata$additional==1])



awcstable5[1,15]=sum(awcsdata$selfemployed[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[2,15]=sum(awcsdata$multiplejobs[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[3,15]=sum(awcsdata$parttime[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[4,15]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[5,15]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[6,15]=sum(awcsdata$longhours[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[7,15]=sum(awcsdata$freqlongdays[awcsdata$cgage25to71women==1])/sum(awcsdata$cgage25to71women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[8,15]=sum(awcsdata$selfemployed[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[9,15]=sum(awcsdata$multiplejobs[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[10,15]=sum(awcsdata$parttime[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[11,15]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[12,15]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[13,15]=sum(awcsdata$longhours[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[14,15]=sum(awcsdata$freqlongdays[awcsdata$cgunderage35women==1])/sum(awcsdata$cgunderage35women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[15,15]=sum(awcsdata$selfemployed[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[16,15]=sum(awcsdata$multiplejobs[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[17,15]=sum(awcsdata$parttime[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[18,15]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[19,15]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[20,15]=sum(awcsdata$longhours[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[21,15]=sum(awcsdata$freqlongdays[awcsdata$cgage35to49women==1])/sum(awcsdata$cgage35to49women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5[22,15]=sum(awcsdata$selfemployed[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[23,15]=sum(awcsdata$multiplejobs[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[24,15]=sum(awcsdata$parttime[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[25,15]=sum(awcsdata$avgweeklyhoursmain[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[26,15]=sum(awcsdata$avgweeklyhoursall[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[27,15]=sum(awcsdata$longhours[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])
awcstable5[28,15]=sum(awcsdata$freqlongdays[awcsdata$cgoverage50women==1])/sum(awcsdata$cgoverage50women[awcsdata$ms436_q1==1&awcsdata$additional==1])

awcstable5


awcstable6=read.csv("AWCSTable3.3.csv")
awcstable6

awcsdata$ms436_ef10=as.numeric(awcsdata$ms436_ef10)
awcsdata$ms436_ef10[is.na(awcsdata$ms436_ef10)]<-0
median(awcsdata$ms436_ef10)
median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>0])

awcsdata$ms436_ef10
awcstable6

awcstable6$overallreplication=0
awcstable6$menreplication=0
awcstable6$womenreplication=0
awcstable6$ncgmenreplication=0
awcstable6$ncgwomenreplication=0
awcstable6$cgmenreplication=0
awcstable6$cgwomenreplication=0
awcstable6



awcstable6[1,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71==1])
awcstable6[2,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35==1])
awcstable6[4,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49==1])
awcstable6[6,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50==1])
awcstable6[8,9]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50==1&awcsdata$avgweeklyhoursall>35])

awcstable6[1,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71men==1])
awcstable6[2,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35men==1])
awcstable6[4,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49men==1])
awcstable6[6,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50men==1])
awcstable6[8,10]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50men==1&awcsdata$avgweeklyhoursall>35])

awcstable6[1,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71women==1])
awcstable6[2,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age25to71women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35women==1])
awcstable6[4,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$underage35women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49women==1])
awcstable6[6,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$age35to49women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50women==1])
awcstable6[8,11]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$overage50women==1&awcsdata$avgweeklyhoursall>35])

awcstable6[1,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage25to71men==1])
awcstable6[2,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage25to71men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgunderage35men==1])
awcstable6[4,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgunderage35men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage35to49men==1])
awcstable6[6,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage35to49men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgoverage50men==1])
awcstable6[8,12]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgoverage50men==1&awcsdata$avgweeklyhoursall>35])

awcstable6[1,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage25to71women==1])
awcstable6[2,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage25to71women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgunderage35women==1])
awcstable6[4,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgunderage35women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage35to49women==1])
awcstable6[6,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgage35to49women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgoverage50women==1])
awcstable6[8,13]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$ncgoverage50women==1&awcsdata$avgweeklyhoursall>35])


awcstable6[1,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage25to71men==1])
awcstable6[2,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage25to71men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgunderage35men==1])
awcstable6[4,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgunderage35men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage35to49men==1])
awcstable6[6,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage35to49men==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgoverage50men==1])
awcstable6[8,14]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgoverage50men==1&awcsdata$avgweeklyhoursall>35])

awcstable6[1,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage25to71women==1])
awcstable6[2,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage25to71women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[3,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgunderage35women==1])
awcstable6[4,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgunderage35women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[5,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage35to49women==1])
awcstable6[6,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgage35to49women==1&awcsdata$avgweeklyhoursall>35])
awcstable6[7,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgoverage50women==1])
awcstable6[8,15]=median(awcsdata$ms436_ef10[awcsdata$ms436_ef10>200&awcsdata$cgoverage50women==1&awcsdata$avgweeklyhoursall>35])


awcstable6


awcstable7=read.csv("AWCSTable3.4.csv")

awcstable7$overallreplication=0
awcstable7$menreplication=0
awcstable7$womenreplication=0
awcstable7$ncgmenreplication=0
awcstable7$ncgwomenreplication=0
awcstable7$cgmenreplication=0
awcstable7$cgwomenreplication=0
awcstable7

awcsdata$ms436_n4a[is.na(awcsdata$ms436_n4a)]<-0
awcsdata$ms436_n4a
awcsdata$ms436_n4b[is.na(awcsdata$ms436_n4b)]<-0
awcsdata$ms436_n4b
awcsdata$ms436_n4c[is.na(awcsdata$ms436_n4c)]<-0
awcsdata$ms436_n4d[is.na(awcsdata$ms436_n4d)]<-0
awcsdata$ms436_n4a
awcsdata$ms436_n4e[is.na(awcsdata$ms436_n4e)]<-0
awcsdata$ms436_n4a
awcsdata$ms436_n4f[is.na(awcsdata$ms436_n4f)]<-0
awcsdata$ms436_n4a
awcsdata$ms436_n4g[is.na(awcsdata$ms436_n4g)]<-0
awcsdata$ms436_n4a
awcsdata$ms436_n4h[is.na(awcsdata$ms436_n4h)]<-0
awcsdata$ms436_n4h
awcsdata$ms436_n4i[is.na(awcsdata$ms436_n4i)]<-0
awcsdata$ms436_n4i
awcsdata$ms436_additional[is.na(awcsdata$ms436_additional)]<-0
awcsdata$ms436_additional
awcsdata$ms436_q1


awcsdata$sicktime=0
awcsdata$sicktime[awcsdata$ms436_n4a==1]<-1
awcsdata$paidvacation=0
awcsdata$paidvacation[awcsdata$ms436_n4b==1]<-1
awcsdata$paidholidays=0
awcsdata$paidholidays[awcsdata$ms436_n4c==1]<-1
awcsdata$healthinsurance=0
awcsdata$healthinsurance[awcsdata$ms436_n4d==1]<-1
awcsdata$dental=0
awcsdata$dental[awcsdata$ms436_n4e==1]<-1
awcsdata$pension=0
awcsdata$pension[awcsdata$ms436_n4f==1]<-1
awcsdata$lifeinsurance=0
awcsdata$lifeinsurance[awcsdata$ms436_n4g==1]<-1
awcsdata$disability=0
awcsdata$disability[awcsdata$ms436_n4h==1]<-1
awcsdata$flexspending=0
awcsdata$flexspending[awcsdata$ms436_n4i==1]<-1

awcsdata$ms436_n4d

awcstable7
awcstable7[1,9]=sum(awcsdata$paidvacation[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[2,9]=sum(awcsdata$paidholidays[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[3,9]=sum(awcsdata$healthinsurance[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[4,9]=sum(awcsdata$sicktime[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[5,9]=sum(awcsdata$pension[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[6,9]=sum(awcsdata$dental[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[7,9]=sum(awcsdata$disability[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[8,9]=sum(awcsdata$lifeinsurance[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[9,9]=sum(awcsdata$flexspending[awcsdata$age25to71==1])/sum(awcsdata$age25to71[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[10,9]=sum(awcsdata$paidvacation[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[11,9]=sum(awcsdata$paidholidays[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[12,9]=sum(awcsdata$healthinsurance[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[13,9]=sum(awcsdata$sicktime[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[14,9]=sum(awcsdata$pension[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[15,9]=sum(awcsdata$dental[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[16,9]=sum(awcsdata$disability[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[17,9]=sum(awcsdata$lifeinsurance[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[18,9]=sum(awcsdata$flexspending[awcsdata$underage35==1])/sum(awcsdata$underage35[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[19,9]=sum(awcsdata$paidvacation[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[20,9]=sum(awcsdata$paidholidays[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[21,9]=sum(awcsdata$healthinsurance[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[22,9]=sum(awcsdata$sicktime[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[23,9]=sum(awcsdata$pension[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[24,9]=sum(awcsdata$dental[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[25,9]=sum(awcsdata$disability[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[26,9]=sum(awcsdata$lifeinsurance[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[27,9]=sum(awcsdata$flexspending[awcsdata$age35to49==1])/sum(awcsdata$age35to49[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[28,9]=sum(awcsdata$paidvacation[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[29,9]=sum(awcsdata$paidholidays[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[30,9]=sum(awcsdata$healthinsurance[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[31,9]=sum(awcsdata$sicktime[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[32,9]=sum(awcsdata$pension[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[33,9]=sum(awcsdata$dental[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[34,9]=sum(awcsdata$disability[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[35,9]=sum(awcsdata$lifeinsurance[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[36,9]=sum(awcsdata$flexspending[awcsdata$overage50==1])/sum(awcsdata$overage50[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])



awcstable7[1,10]=sum(awcsdata$paidvacation[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[2,10]=sum(awcsdata$paidholidays[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[3,10]=sum(awcsdata$healthinsurance[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[4,10]=sum(awcsdata$sicktime[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[5,10]=sum(awcsdata$pension[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[6,10]=sum(awcsdata$dental[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[7,10]=sum(awcsdata$disability[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[8,10]=sum(awcsdata$lifeinsurance[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[9,10]=sum(awcsdata$flexspending[awcsdata$age25to71men==1])/sum(awcsdata$age25to71men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[10,10]=sum(awcsdata$paidvacation[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[11,10]=sum(awcsdata$paidholidays[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[12,10]=sum(awcsdata$healthinsurance[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[13,10]=sum(awcsdata$sicktime[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[14,10]=sum(awcsdata$pension[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[15,10]=sum(awcsdata$dental[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[16,10]=sum(awcsdata$disability[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[17,10]=sum(awcsdata$lifeinsurance[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[18,10]=sum(awcsdata$flexspending[awcsdata$underage35men==1])/sum(awcsdata$underage35men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[19,10]=sum(awcsdata$paidvacation[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[20,10]=sum(awcsdata$paidholidays[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[21,10]=sum(awcsdata$healthinsurance[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[22,10]=sum(awcsdata$sicktime[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[23,10]=sum(awcsdata$pension[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[24,10]=sum(awcsdata$dental[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[25,10]=sum(awcsdata$disability[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[26,10]=sum(awcsdata$lifeinsurance[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[27,10]=sum(awcsdata$flexspending[awcsdata$age35to49men==1])/sum(awcsdata$age35to49men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])

awcstable7[28,10]=sum(awcsdata$paidvacation[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[29,10]=sum(awcsdata$paidholidays[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[30,10]=sum(awcsdata$healthinsurance[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[31,10]=sum(awcsdata$sicktime[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[32,10]=sum(awcsdata$pension[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[33,10]=sum(awcsdata$dental[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[34,10]=sum(awcsdata$disability[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[35,10]=sum(awcsdata$lifeinsurance[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])
awcstable7[36,10]=sum(awcsdata$flexspending[awcsdata$overage50men==1])/sum(awcsdata$overage50men[awcsdata$ms436_additional==1&awcsdata$ms436_q1==1])


awcstable7























