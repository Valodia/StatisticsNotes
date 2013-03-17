### R code from vignette source 'LMMlong.Rnw'

###################################################
### code chunk number 1: LMMlong.Rnw:36-45
###################################################
## customize
options(prompt="> ", continue="   ",width=35,
show.signif.stars=FALSE)

options(SweaveHooks=list(fig=function() par(bg="white", fg="black")))

#set.seed(987654321)
library(lme4)
load("MAS473.RData")


###################################################
### code chunk number 2: LMMnotes.Rnw:10-12
###################################################
fm1<-lmer(wear~material+(1|Subject),BHHshoes)


###################################################
### code chunk number 3: LMMnotes.Rnw:51-52
###################################################
VarCorr(fm1)


###################################################
### code chunk number 4: LMMnotes.Rnw:70-72
###################################################
fm2<-lmer(wear~material+(1+material|Subject),BHHshoes)
VarCorr(fm2)


###################################################
### code chunk number 5: LMMnotes.Rnw:113-114
###################################################
0.1645525/(sqrt(5.93634)*sqrt(0.0045617))


###################################################
### code chunk number 6: LMMnotes.Rnw:119-120
###################################################
fm3<-lmer(wear~material-1 + (material-1|Subject),BHHshoes)


###################################################
### code chunk number 7: LMMnotes.Rnw:160-162
###################################################
fm1<-lmer(score~Machine-1+(1|Worker/Machine),
data=Machines)


###################################################
### code chunk number 8: LMMnotes.Rnw:199-201
###################################################
mat<-matrix(unlist(ranef(fm1)$`Machine:Worker`),6,3) +
matrix(unlist(ranef(fm1)$Worker),6,3)


###################################################
### code chunk number 9: LMMnotes.Rnw:203-205
###################################################
rownames(mat)<-c(6,2,4,1,3,5)
colnames(mat)<-LETTERS[1:3]


###################################################
### code chunk number 10: LMMnotes.Rnw:208-209
###################################################
mat


###################################################
### code chunk number 11: LMMnotes.Rnw:215-216
###################################################
var(mat)


###################################################
### code chunk number 12: LMMnotes.Rnw:264-266
###################################################
fm2<-lmer(score~Machine-1+(1|Worker),
data=Machines)


###################################################
### code chunk number 13: LMMnotes.Rnw:269-270
###################################################
ranef(fm2)


###################################################
### code chunk number 14: LMMnotes.Rnw:282-284
###################################################
fm3<-lmer(score~Machine-1+(Machine-1|Worker),
data=Machines)


###################################################
### code chunk number 15: LMMnotes.Rnw:287-288
###################################################
ranef(fm3)


###################################################
### code chunk number 16: LMMnotes.Rnw:303-304
###################################################
#var(ranef(fm3)$Worker)


###################################################
### code chunk number 17: LMMnotes.Rnw:338-341
###################################################
diag(var(ranef(fm3)$Worker))
cor(ranef(fm3)$Worker)
# look at the fm3 output (the random effects table)


###################################################
### code chunk number 18: LMMnotes.Rnw:375-379
###################################################
#fm1's ranefs summed up are roughly the same as the fm3 ranefs:
matrix(unlist(ranef(fm1)$`Machine:Worker`),6,3) +
matrix(unlist(ranef(fm1)$Worker),6,3)
ranef(fm3)


