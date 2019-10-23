awcsdata=read.csv("AWCS Data.csv")


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


