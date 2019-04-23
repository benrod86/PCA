library(psych) 
DataI <- read.table(file.choose(),h=1)
DataI
head(DataI)
tail(DataI)
Data=DataI[,1:8]
head(Data)
CorDat=cor(Data)
CorDat
pairs(Data, panel=function(x,y){points(x,y); abline(lm(y~x), col='red',lwd=2) }) 
N=dim(Data)[1]
N
cortest.bartlett(CorDat, n=N)

eigVal=eigen(CorDat)
eigVal
eigVal$values
vectData=eigVal$vectors # cargas que tienen los componentes principales, como esas variables cargan el PC #
vectData
plot(c(1:length(eigVal$values)),eigVal$values, type="l") 
points(c(1:length(eigVal$values)),eigVal$values, pch=19)

prt=prcomp(Data, retx=TRUE, center=TRUE, scale=TRUE)
summary(prt) # el PC explica el ..% que establece el proportion of variance: los valores se deben multiplicar por 100. La cumulative proportion es la varianza acumulada, también lo debo multiplicar por 100 y el último PC debe tener un valor de 1.0000 que multiplicado da 100% #

scoreData=prt$x
head(scoreData)
LoadsData=prt$rotation
LoadsData # como éstan cargados cada uno de los PCs #
biplot(prt)
quartz()
biplot(scoreData[,c(1,2)],LoadsData[,c(1,2)], xlabs=rep("*",980), col=c("orange", "black"))

# Visualización incluyedo una variable agrupadora #

newData=cbind(scoreData,data.frame(DataI[,9])) 
head(newData)
colnames(newData)=c(colnames(newData[1:8]), "Risk")
head(newData)
newData_alto=subset(newData, newData[,9]=="high") 
newData_medio=subset(newData, newData[,9]=="med") 
newData_bajo=subset(newData, newData[,9]=="low")
newData_nulo=subset(newData, newData[,9]=="null")

lmin=min(scoreData[,c(1,2)])
lmax=max(scoreData[,c(1,2)])
quartz()
plot(scoreData[,c(1,2)], type="n", xlim=c(lmin,lmax), ylim=c(lmin,lmax))
points(newData_alto[,1:2], pch=19, col="darkorchid1") 
points(newData_medio[,1:2], pch=19, col="aquamarine") 
points(newData_bajo[,1:2], pch=19, col="khaki1")
points(newData_nulo[,1:2], pch=19, col="plum2")

arrows(0,0, LoadsData[,1]*9,LoadsData[,2]*9, length = 0.1 ) 
text(LoadsData[,1]*11,LoadsData[,2]*11, labels=rownames(LoadsData))
legend("topleft", legend=c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo", "Riesgo Nulo"), col=c("darkorchid1", "aquamarine", "khaki1", "plum2"), pch=16)

lmin=min(scoreData[,c(1,3)])
lmax=max(scoreData[,c(1,3)])
quartz()
plot(scoreData[,c(1,3)], type="n", xlim=c(lmin,lmax), ylim=c(lmin,lmax))
points(newData_alto[,1:3], pch=19, col="darkorchid1") 
points(newData_medio[,1:3], pch=19, col="aquamarine") 
points(newData_bajo[,1:3], pch=19, col="khaki1")
points(newData_nulo[,1:3], pch=19, col="plum2")

arrows(0,0, LoadsData[,1]*8.5,LoadsData[,3]*8.5, length = 0.1 ) 
text(LoadsData[,1]*10.2,LoadsData[,3]*10.2, labels=rownames(LoadsData))
legend("topleft", legend=c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo", "Riesgo Nulo"), col=c("darkorchid1", "aquamarine", "khaki1", "plum2"), pch=16)

lmin=min(scoreData[,c(1,4)])
lmax=max(scoreData[,c(1,4)])
quartz()
plot(scoreData[,c(1,4)], type="n", xlim=c(lmin,lmax), ylim=c(lmin,lmax))
points(newData_alto[,1:4], pch=19, col="darkorchid1") 
points(newData_medio[,1:4], pch=19, col="aquamarine") 
points(newData_bajo[,1:4], pch=19, col="khaki1")
points(newData_nulo[,1:4], pch=19, col="plum2")

arrows(0,0, LoadsData[,1]*8.5,LoadsData[,4]*8.5, length = 0.1 ) 
text(LoadsData[,1]*10.2,LoadsData[,4]*10.2, labels=rownames(LoadsData))
legend("topright", legend=c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo", "Riesgo Nulo"), col=c("darkorchid1", "aquamarine", "khaki1", "plum2"), pch=16)

# Análisis Discriminante #

Data=DataI[,1:9]
library(MASS)
mda=lda(Risk ~., Data)
mda
plot(mda) 
predict(mda)$x
newDA=cbind(predict(mda)$x, data.frame(Data$Risk)) 
colnames(newDA)=c("LD1","LD2","LD3", "Risk")
head(newDA)
tail(newDA)
Csup=max(newDA[,1:2])
Cinf=min(newDA[,1:2])
levels(newDA$Risk)
DAalto=subset(newDA, newDA$Risk =="high") 
DAmed=subset(newDA, newDA$Risk =="med") 
head(DAmed)
DAbajo=subset(newDA, newDA$Risk =="low")
DAnulo=subset(newDA, newDA$Risk =="null") 
quartz()
plot(DAalto[,1:2], pch=19, col="darkorchid1",xlim=c(Cinf,Csup),ylim=c(Cinf,Csup)) 
points(DAmed[,1:2], pch=19, col="aquamarine") 
points(DAbajo[,1:2], pch=19, col="khaki1") 
points(DAnulo[,1:2], pch=19, col="plum2") 
points(mean(DAalto[,1]), mean(DAalto[,2]), pch="+", col="mediumorchid4", cex=2) 
points(mean(DAmed[,1]), mean(DAmed[,2]), pch="+", col="forestgreen", cex=2) 
points(mean(DAbajo[,1]), mean(DAbajo[,2]), pch="+", col="goldenrod",cex=2)
points(mean(DAnulo[,1]), mean(DAnulo[,2]), pch="+", col="violetred",cex=2)




arrows(0,0, LoadsData[,1]*9,LoadsData[,2]*9, length = 0.1 ) 
text(LoadsData[,1]*11,LoadsData[,2]*11, labels=rownames(LoadsData))




legend("bottomright", legend=c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo", "Riesgo Nulo"), col=c("mediumorchid4", "forestgreen", "goldenrod", "violetred"), pch=16)

Csup=max(newDA[,c(1,3)])
Cinf=min(newDA[,c(1,3)])
quartz()
plot(DAalto[,c(1,3)], pch=19, col="darkorchid1",xlim=c(Cinf,Csup),ylim=c(Cinf,Csup)) 
points(DAmed[,c(1,3)], pch=19, col="aquamarine") 
points(DAbajo[,c(1,3)], pch=19, col="khaki1") 
points(DAnulo[,c(1,3)], pch=19, col="plum2") 

points(mean(DAalto[,1]), mean(DAalto[,3]), pch="+", col="mediumorchid4", cex=2) 
points(mean(DAmed[,1]), mean(DAmed[,3]), pch="+", col="forestgreen", cex=2) 
points(mean(DAbajo[,1]), mean(DAbajo[,3]), pch="+", col="goldenrod",cex=2)
points(mean(DAnulo[,1]), mean(DAnulo[,3]), pch="+", col="violetred",cex=2)
legend("bottomright", legend=c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo", "Riesgo Nulo"), col=c("mediumorchid4", "forestgreen", "goldenrod", "violetred"), pch=16)