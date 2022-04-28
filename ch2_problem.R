#범주형 자료분석
#2.16
a.lungcancer = array(c(688, 21, 650, 59), dim = c(2, 2))
t.lungcancer = as.table(a.lungcancer)
install.packages("epiR")
library(epiR)
??epiR.2by2
epi.2by2(t.lungcancer, method = "case.control") 

#2.33
a.death = array (c(19.5, 11.5, 132.5, 52.5), dim = c(2,2), dimnames = list(DefendantRace = c("white", "Black"), DeathPenalty = c("Yes", "No")))
a.death
t.death = as.table(a.death)
library(epiR)
epi.2by2(t.death, method = "cross.sectional")

a.death2 = array(c(0.5, 6.5, 9.5, 97.5), dim = c(2,2), dimnames = list(DefendantRace = c("white", "black"), DeathPenalty = c("Yes", "No")))
a.death2
t.death2 = as.table(a.death2)
epi.2by2(t.death2)

a.death_marginal = array(c(19, 17, 141, 149), dim = c(2, 2), dimnames = list(DefendantRace = c("white", "black"), DeathPenalty = c("Yes", "No")))
t.death_marginal = as.table(a.death_marginal)
epi.2by2(t.death_marginal)

##
arr = array(c(19,11,132,52,0.5,6,9,97), dim=c(2,2,2), 
              dimnames=list(Defend=c("W","B"), Response=c("Y","N"), Victim=c("W","B")))
tbl = as.table(arr) 
tbl
ftable(tbl,row.vars=c(3,1),col.vars=2)

#b
(tbl.5=tbl[,,2]+0.5)
epi.2by2(tbl[,,1]) # the conditional XY odds ratio for clinic 1
epi.2by2(tbl.5) # the conditional XY odds ratio for clinic 2
epi.2by2(tbl[,1,]) # the conditional XZ odds ratio for Y=1
epi.2by2(tbl[,2,]) # the conditional XZ odds ratio for Y=2
epi.2by2(tbl[1,,]) # the conditional YZ odds ratio for X=1
epi.2by2(tbl[2,,]) # the conditional YZ odds ratio for X=2

#table3.1
snoring=c(0,2,4,5)
(y=matrix(c(24,35,21,30,1355,603,192,224),nrow=4))
glm.identity=glm(y~snoring,family=binomial(link='identity'))
summary(glm.identity)

#figure 3.1 : observed proportions and fitted lines by linear probability
pihat=y[,1]/rowSums((y))
plot(snoring,pihat,ylab='predicted prob.',ylim=c(0,0.2),pch=20)
lines(snoring,fitted.values(glm.identity),lty=1)

#try some other link functions and make figure 3.1 complete
glm.logit = glm(y~snoring,family=binomial(link='logit'))
lines(snoring,fitted.values(glm.logit),lty=2,col=2)
glm.probit=glm(y~snoring,family = binomial(link='probit'))
lines(snoring,fitted.values(glm.probit),lty=3,col=3)
legend('topright',legend = c('linear','logistic','probit'),lty=1:3,col=1:3)

#
pi.fun = function(x,alpha=1,beta=-1) 1/(1+exp(-alpha-beta*x))
curve(pi.fun,from=0,to=1,ylab='probability')




