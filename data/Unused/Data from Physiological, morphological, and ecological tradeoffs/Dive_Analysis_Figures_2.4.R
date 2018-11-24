##### FIGURES ####

###### Figure 1. Histograms of BEHAV log foraging dive depth distributions #####

pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "HIST_BEHAV_FORAGING",".pdf",sep=""),
    height=5.5,width=7)
split.screen(figs=rbind(c(0.15, 0.31, 0.10, 0.85),
                        c(0.31, 0.47, 0.10, 0.85),
                        c(0.47, 0.63, 0.10, 0.85),
                        c(0.63, 0.79, 0.10, 0.85),
                        c(0.79, 0.95, 0.10, 0.85)))
screen(1)
par(mar=c(0,0,0,0),xpd=T)

#Create a dummy plot so that foraging range rectangle can be placed under histogram
HIST_VERT(5000:5500,AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,50),XLAB=NA,YLAB=NA,COL="lightpink")
#Add foraging range rectangle
rect(xleft = 0, xright = 50, ybottom=40-550*40/2000, ytop=40-0*40/2000,col=gray(0.7),lty=0)
#Add Dive histogram
HIST_VERT(BEHAV[BEHAV$SPP=="Pe" ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,50),XLAB=NA,YLAB=NA,COL="lightpink")
#Add Axes and labels
axis(1,at=25,labels="P. electra",las=1,line=-0.75,cex.axis=0.8)
axis(2,at=seq(0,40,by=4),labels=seq(2000,0,by=-200),las=1)
axis(3,line=-0.75)
abline(h=0,lwd=1.5);lines(x=c(50,50),y=c(0,40),lwd=1.5)
mtext("Depth (m)",side=2,line=3.5)

screen(2)
par(mar=c(0,0,0,0))
HIST_VERT(5000:5500,AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightpink")
rect(xleft = 0, xright = 50, ybottom=40-1000*40/2000, ytop=40-0*40/2000,col=gray(0.7),lty=0)
HIST_VERT(BEHAV[BEHAV$SPP=="Gm" ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightsalmon")
#Add discontinuity line
lines(x=c(24,22,24,22),y=c(40.4,39.8,39.2,38.6))
axis(1,at=12.5,labels="G. macrorhynch.",las=1,line=-0.75,cex.axis=0.8)
axis(3,line=-0.75)
abline(h=0,lwd=1.5);lines(x=c(25,25),y=c(0,40),lwd=1.5)

screen(3)
par(mar=c(0,0,0,0))
HIST_VERT(5000:5500,AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightpink")
rect(xleft = 0, xright = 50, ybottom=40-1350*40/2000, ytop=40-550*40/2000,col=gray(0.7),lty=0)
HIST_VERT(BEHAV[BEHAV$SPP=="Pm" ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="plum")
axis(1,at=12.5,labels="P. macrocephal.",las=1,line=-0.75,cex.axis=0.8)
axis(3,line=-0.75)
abline(h=0,lwd=1.5);lines(x=c(25,25),y=c(0,40),lwd=1.5)
mtext("% Dives",side=3,line=1.75)

screen(4)
par(mar=c(0,0,0,0))
HIST_VERT(5000:5500,AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightpink")
rect(xleft = 0, xright = 50, ybottom=40-1900*40/2000, ytop=40-650*40/2000,col=gray(0.7),lty=0)
HIST_VERT(BEHAV[BEHAV$SPP=="Md"  ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightblue")
#Add discontinuity line
lines(x=c(24,22,24,22),y=c(40.4,39.8,39.2,38.6))
axis(1,at=12.5,labels="M. densirostris",las=1,line=-0.75,cex.axis=0.8)
axis(3,line=-0.75)
abline(h=0,lwd=1.5);lines(x=c(25,25),y=c(0,40),lwd=1.5)

screen(5)
par(mar=c(0,0,0,0))
HIST_VERT(5000:5500,AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightpink")
rect(xleft = 0, xright = 50, ybottom=40-1900*40/2000, ytop=40-750*40/2000,col=gray(0.7),lty=0)
HIST_VERT(BEHAV[BEHAV$SPP=="Zc" ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,25),XLAB=NA,YLAB=NA,COL="lightgreen")
axis(1,at=12.5,labels="Z. cavirostris",las=1,line=-0.75,cex.axis=0.8)
axis(3,line=-0.75)
abline(h=0,lwd=1.5);lines(x=c(25,25),y=c(0,40),lwd=1.5)

close.screen(all = TRUE)
dev.off()

##### Figure 2. Scatter plots of dive duration and depth #####

pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "MAX_DEPTH_DURATION_BIPLOT_ALL",".pdf",sep=""),height=11,width=8.5)
par(oma=c(3,3,0,0),mar=c(3,3,2,4),mfrow=c(3,2))

#Open plot window
plot(x=seq(1,100,length.out=2000),y=1:2000,ylim=c(2000,0),type="n",ylab=NA,xlab=NA,cex.axis=1.2)
#Add daytime and nighttime points
points(y=BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DurationMin"]/60,col="pink",pch=25,cex=1.2,bg="pink")
points(y=BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DurationMin"]/60,col="pink4",pch=25,cex=1.2,bg="pink4")
#Add legend with sample sizes in lower left
legend(x=grconvertX(x=0.03, from = "npc", to = "user"),
       y=grconvertY(y=0.27, from = "npc", to = "user"),
       legend=c(paste("Day (n= ",nrow(BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive" ,]),")",sep=""),
                paste("Night (n= ",nrow(BEHAV[BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive" ,]),")",sep="")), 
       title="P. electra", col=c("lightpink","lightpink4"),pch=25,cex=1.2,pt.cex=1.2,bg="white",pt.bg = c("lightpink","lightpink4"))
#Add power law trendline
lines(x=seq(0,13,length=100),y=10^predict(LM_DEPTH_DURATION_Pe,data.frame(DurationMin=seq(0*60,13*60,length=100))))
#Print model formula and R^2 values in upper right
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("log(Z) =", round(summary(LM_DEPTH_DURATION_Pe)$coeff[2,1],3),"* log(T) -",
               abs(round(summary(LM_DEPTH_DURATION_Pe)$coeff[1,1],3))))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.91, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("R\U00B2 :",round(summary(LM_DEPTH_DURATION_Pe)$adj.r.squared,3)))


plot(x=seq(1,100,length.out=2000),y=1:2000,ylim=c(2000,0),type="n",ylab=NA,xlab=NA,cex.axis=1.2)
points(y=BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DurationMin"]/60,col="lightsalmon",pch=17,cex=1.4)
points(y=BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DurationMin"]/60,col="lightsalmon4",pch=17,cex=1.4)
legend(x=grconvertX(x=0.03, from = "npc", to = "user"),
       y=grconvertY(y=0.27, from = "npc", to = "user"),
       legend=c(paste("Day (n= ",nrow(BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive" ,]),")",sep=""),
                paste("Night (n= ",nrow(BEHAV[BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive" ,]),")",sep="")), 
       title="G. macrorhynchus", col=c("lightsalmon","lightsalmon4"),pch=17,cex=1.2,pt.cex=1.4,bg="white")
#Add exponential trendline
lines(x=seq(0,19,length=100),y=10^predict(LM_DEPTH_DURATION_Gm,data.frame(DurationMin=seq(0*60,19*60,length=100))))
#Print model formula and R^2 values in upGmr right
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("log(Z) =", round(summary(LM_DEPTH_DURATION_Gm)$coeff[2,1],3),"* T +",
               abs(round(summary(LM_DEPTH_DURATION_Gm)$coeff[1,1],3))))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.91, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("R\U00B2 :",round(summary(LM_DEPTH_DURATION_Gm)$adj.r.squared,3)))

plot(x=seq(1,100,length.out=2000),y=1:2000,ylim=c(2000,0),type="n",ylab=NA,xlab=NA,cex.axis=1.2)
points(y=BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DurationMin"]/60,col="lightblue",pch=15,cex=1.4)
points(y=BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DurationMin"]/60,col="lightblue4",pch=15,cex=1.4)
legend(x=grconvertX(x=0.03, from = "npc", to = "user"),
       y=grconvertY(y=0.27, from = "npc", to = "user"),
       legend=c(paste("Day (n= ",nrow(BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive" ,]),")",sep=""),
                paste("Night (n= ",nrow(BEHAV[BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive" ,]),")",sep="")), 
       title="M. densirostris", col=c("lightblue",COL2="lightblue4"),pch=15,cex=1.2,pt.cex=1.4,bg="white")
#Add power law trendline
lines(x=seq(7,70,length=100),y=predict(LM_DEPTH_DURATION_Md,data.frame(DurationMin=seq(7*60,70*60,length=100))))
#Print model formula and R^2 values in upMdr right
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("Z =", round(summary(LM_DEPTH_DURATION_Md)$coeff[2,1],3),"* T -",
               abs(round(summary(LM_DEPTH_DURATION_Md)$coeff[1,1],3))))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.91, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("R\U00B2 :",round(summary(LM_DEPTH_DURATION_Md)$adj.r.squared,3)))


plot(x=seq(1,100,length.out=2000),y=1:2000,ylim=c(2000,0),type="n",ylab=NA,xlab=NA,cex.axis=1.2)
points(y=BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DurationMin"]/60,col="lightgreen",pch=18,cex=1.5)
points(y=BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DurationMin"]/60,col="darkseagreen4",pch=18,cex=1.5)
legend(x=grconvertX(x=0.03, from = "npc", to = "user"),
       y=grconvertY(y=0.27, from = "npc", to = "user"),
       legend=c(paste("Day (n= ",nrow(BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive" ,]),")",sep=""),
                paste("Night (n= ",nrow(BEHAV[BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive" ,]),")",sep="")), 
       title="Z. cavirostris", col=c("lightgreen","darkseagreen4"),pch=18,cex=1.2,pt.cex=1.5,bg="white")
#Add power law trendline
lines(x=seq(7,90,length=100),y=predict(LM_DEPTH_DURATION_Zc,data.frame(DurationMin=seq(7*60,90*60,length=100))))
#Print model formula and R^2 values in upZcr right
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("Z =", round(summary(LM_DEPTH_DURATION_Zc)$coeff[2,1],3),"* T -",
               abs(round(summary(LM_DEPTH_DURATION_Zc)$coeff[1,1],3))))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.91, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("R\U00B2 :",round(summary(LM_DEPTH_DURATION_Zc)$adj.r.squared,3)))


plot(x=seq(1,100,length.out=2000),y=1:2000,ylim=c(2000,0),type="n",ylab=NA,xlab=NA,cex.axis=1.2)
points(y=BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive","DurationMin"]/60,col="plum",pch=16,cex=1.4)
points(y=BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DepthMin"],
       x=BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive","DurationMin"]/60,col="plum4",pch=16,cex=1.4)
legend(x=grconvertX(x=0.03, from = "npc", to = "user"),
       y=grconvertY(y=0.27, from = "npc", to = "user"),
       legend=c(paste("Day (n= ",nrow(BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Day" & BEHAV$What=="Dive" ,]),")",sep=""),
                paste("Night (n= ",nrow(BEHAV[BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night" & BEHAV$What=="Dive" ,]),")",sep="")), 
       title="P. macrocephalus", col=c("plum","plum4"),pch=16,cex=1.2,pt.cex=1.4,bg="white")
#Add power law trendline
lines(x=seq(0,60,length=100),y=10^predict(LM_DEPTH_DURATION_Pm,data.frame(DurationMin=seq(0*60,60*60,length=100))))
#Print model formula and R^2 values in upPmr right
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("log(Z) =", round(summary(LM_DEPTH_DURATION_Pm)$coeff[2,1],3),"* log(T) -",
               abs(round(summary(LM_DEPTH_DURATION_Pm)$coeff[1,1],3))))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.91, from = "npc", to = "user"),pos=2,offset=0,cex=1.2,
     lab=paste("R\U00B2 :",round(summary(LM_DEPTH_DURATION_Pm)$adj.r.squared,3)))

mtext(text=c("Duration (min)","Depth (m)"),side=c(1,2),line=c(0.5,0.5),outer=T)
dev.off()


###### Analysis 2. Fit Phylogenetic Generalized Linear Mixed Effects Models of DURATION_MAX at the individual level using a Function of MASS, MB, and/or IDDI #####

# Goal: introduce species level random effects to account for the lack of independence 
# between individuals within a species, and phylogenetic effect to account for the differences 
# in relatedness between species

#Load required packages
library(ape)
library(MCMCglmm)

#Prune PHYLO tree so that it contains only the study species from this study
PHYLO_TREE_temp=drop.tip(phy=PHYLO_TREE,tip=c("St_attenuata","Or_orca","Ph_phocoena","De_leucas",
                                              "Mo_monoceros","Hy_ampullatus","Ko_breviceps"))
PHYLO_TREE_temp$tip.label=c("Gm","Pe","Md","Zc","Pm")

#Invert the SPP matrix to calculate the Sigma matrix of phylogenetic correlation detailed in Villemereuil and Nakagawa 2014
INV_PHYLO_TREE_temp<-inverseA(PHYLO_TREE_temp,nodes="TIPS",scale=TRUE)

#re-format grouping variables as factors for model specification
TAGS$SPP=as.factor(TAGS$SPP)
TAGS$FAM=as.factor(TAGS$FAM)

#compare a linear model and different random intercept structures
PRIOR<-list(R=list(V=1,nu=0.02))
PGLMM_DURATION_MAX_MASS_1A<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED)+log10(MB),
                                     family="gaussian",prior=PRIOR,
                                     data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)


PRIOR<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DURATION_MAX_MASS_2A<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED)+log10(MB),random=~SPP,
                                     family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                     data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MAX_MASS_1A$DIC
PGLMM_DURATION_MAX_MASS_2A$DIC

#compare fixed effects structures given a random intercept structure
PRIOR<-list(R=list(V=1,nu=0.02))
PGLMM_DURATION_MAX_MASS_1<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED)+log10(MB),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_2<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_3<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(MB),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_4<-MCMCglmm(log10(DURATION_MAX/60)~log10(IDDI_MED)+log10(MB),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_5<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_6<-MCMCglmm(log10(DURATION_MAX/60)~log10(MB),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)
PGLMM_DURATION_MAX_MASS_7<-MCMCglmm(log10(DURATION_MAX/60)~log10(IDDI_MED),
                                    family="gaussian",prior=PRIOR,
                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],
                                    nitt=100000,burnin=1000,thin=50)


PGLMM_DURATION_MAX_MASS_1$DIC
PGLMM_DURATION_MAX_MASS_2$DIC
PGLMM_DURATION_MAX_MASS_3$DIC
PGLMM_DURATION_MAX_MASS_4$DIC
PGLMM_DURATION_MAX_MASS_5$DIC
PGLMM_DURATION_MAX_MASS_6$DIC
PGLMM_DURATION_MAX_MASS_7$DIC

#PGLMM_DURATION_MAX_MASS_1<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED)+log10(MB),random=~SPP,
#                                     family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                     data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50,pr=TRUE)
#PGLMM_DURATION_MAX_MASS_2<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(MB),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)
#PGLMM_DURATION_MAX_MASS_3<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED)+log10(IDDI_MED),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)
#PGLMM_DURATION_MAX_MASS_4<-MCMCglmm(log10(DURATION_MAX/60)~log10(IDDI_MED)+log10(MB),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)
#PGLMM_DURATION_MAX_MASS_5<-MCMCglmm(log10(DURATION_MAX/60)~log10(MASS_MED),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)
#PGLMM_DURATION_MAX_MASS_6<-MCMCglmm(log10(DURATION_MAX/60)~log10(MB),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)
#PGLMM_DURATION_MAX_MASS_7<-MCMCglmm(log10(DURATION_MAX/60)~log10(IDDI_MED),random=~SPP,
#                                    family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
#                                    data=TAGS[!is.na(TAGS$DURATION_MAX),],nitt=100000,burnin=1000,thin=50)

#PGLMM_DURATION_MAX_MASS_1$DIC
#PGLMM_DURATION_MAX_MASS_2$DIC
#PGLMM_DURATION_MAX_MASS_3$DIC
#PGLMM_DURATION_MAX_MASS_4$DIC
#PGLMM_DURATION_MAX_MASS_5$DIC
#PGLMM_DURATION_MAX_MASS_6$DIC
#PGLMM_DURATION_MAX_MASS_7$DIC


###### Figure 3. GLM and PGLMM's of max. dive duration as a function of body mass and myoglobin concentration#####
#Plot of median whale MASS relative to MAX_DURATION of dives 
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "GLM_DURATION_MAX_MASS",".pdf",sep=""),height=7,width=8.5)

par(oma=c(7,3,0,0),mar=c(3,3,2,3),mfrow=c(2,2),xpd=TRUE)

#Plot of Noren and Williams 2000 model fit overlaid on observed data
#a single DURATION_MAX record from an Md female was eliminated as non-biologically credible 
#(likely resulting from the wet-dry sensor not picking up the end of a dive)
plot(1:10,1:10,type="n",xlim=c(100,14000),ylim=c(8,110),xlab=NA,ylab=NA)
points(TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","MASS_MED"],
       TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","DURATION_MAX"]/60,col="black",bg="lightpink",pch=25)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightsalmon",pch=24)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","DURATION_MAX"]/60,col="black",bg="lightsalmon4",pch=24)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightblue",pch=22)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"DURATION_MAX"]/60,col="black",bg="lightblue4",pch=22)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightgreen",pch=23)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="darkseagreen4",pch=23)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="plum",pch=21)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="plum4",pch=21)
lines(x=seq(150,14000,length=100),y=0.68*seq(150,14000,length=100)^0.47,lty=1,lwd=1)
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="Noren and Williams 2000 Eq. 2 (Mass):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(T) =","0.47 log(m) + log(0.68)"))
mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Max. Dive Duration (min)")
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(a)",pos=4,offset=0,cex=1.2, font=3,xpd=T)


#Plot of Model 5 (Mass) fit overlaid on observed data
plot(1:10,1:10,type="n",xlim=c(100,14000),ylim=c(8,110),xlab=NA,ylab=NA)
points(TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","MASS_MED"],
       TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","DURATION_MAX"]/60,col="black",bg="lightpink",pch=25)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightsalmon",pch=24)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","DURATION_MAX"]/60,col="black",bg="lightsalmon4",pch=24)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightblue",pch=22)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"DURATION_MAX"]/60,col="black",bg="lightblue4",pch=22)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightgreen",pch=23)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="darkseagreen4",pch=23)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="plum",pch=21)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="plum4",pch=21)

#Create a data.frame of with mean of PGLMM parameter posterior distributions
PGLMM_DURATION_MAX_MASS_5_temp=as.data.frame(t(colMeans(PGLMM_DURATION_MAX_MASS_5$Sol)))

#add a line representing the fixed effects component of the model 
lines(seq(150,14000,length=100),
      10^(PGLMM_DURATION_MAX_MASS_5_temp$'(Intercept)'
          +PGLMM_DURATION_MAX_MASS_5_temp$'log10(MASS_MED)'*log10(seq(150,14000,length=100))),
      lwd=1)

text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="Mod. 5b (Mass):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(T) =",round(PGLMM_DURATION_MAX_MASS_5_temp$'log10(MASS_MED)',2),
               "log(m) +",round(PGLMM_DURATION_MAX_MASS_5_temp$'(Intercept)',2)))
#text(x=grconvertX(0.98, from = "npc", to = "user"),
#     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
#     lab=paste("DIC :",round(PGLMM_DURATION_MAX_MASS_5$DIC,1)))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("R\U00B2 :",round(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_5),3)))
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(b)",pos=4,offset=0,cex=1.2, font=3,xpd=T)


mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Max. Dive Duration (min)")



#Plot of Model 6 (Mb) fit overlaid on observed data
plot(1:10,1:10,type="n",xlim=c(10,90),ylim=c(8,110),xlab=NA,ylab=NA)
points(TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","MB"],
       TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","DURATION_MAX"]/60,col="black",bg="lightpink",pch=25)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","MB"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightsalmon",pch=24)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","MB"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","DURATION_MAX"]/60,col="black",bg="lightsalmon4",pch=24)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MB"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightblue",pch=22)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"MB"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"DURATION_MAX"]/60,col="black",bg="lightblue4",pch=22)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MB"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightgreen",pch=23)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","MB"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="darkseagreen4",pch=23)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","MB"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="plum",pch=21)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","MB"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="plum4",pch=21)

#Create a data.frame of with mean of PGLMM parameter posterior distributions
PGLMM_DURATION_MAX_MASS_6_temp=as.data.frame(t(colMeans(PGLMM_DURATION_MAX_MASS_6$Sol)))

#add lines representing the specific random effects offsets of the intercepts at the FAM level (level 1)
lines(seq(20,80,length=100),
      10^(PGLMM_DURATION_MAX_MASS_6_temp$'(Intercept)'
          +PGLMM_DURATION_MAX_MASS_6_temp$'log10(MB)'*log10(seq(20,80,length=100))),
      lwd=1)


text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="Mod. 6b (Myoglobin):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(T) =",round(PGLMM_DURATION_MAX_MASS_6_temp$'log10(MB)',2),
               "log([Mb]) -",abs(round(PGLMM_DURATION_MAX_MASS_6_temp$'(Intercept)',2))))
#text(x=grconvertX(0.98, from = "npc", to = "user"),
#     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
#     lab=paste("DIC :",round(PGLMM_DURATION_MAX_MASS_6$DIC,1)))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("R\U00B2 :",round(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_6),3)))
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(c)",pos=4,offset=0,cex=1.2, font=3,xpd=T)
mtext(side=1,line=2.5,cex=0.9, text="[Myoglobin] (mg kg-1)")
mtext(side=2,line=2.5,cex=0.9, text="Max. Dive Duration (min)")


#Plot of Model 2 (Mass & IDDI) fit overlaid on observed data
plot(1:10,1:10,type="n",xlim=c(100,14000),ylim=c(8,110),xlab=NA,ylab=NA)
points(TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","MASS_MED"],
       TAGS[TAGS$SPP=="Pe" & TAGS$Sex=="U","DURATION_MAX"]/60,col="black",bg="lightpink",pch=25)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightsalmon",pch=24)
points(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","MASS_MED"],
       TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F or M","DURATION_MAX"]/60,col="black",bg="lightsalmon4",pch=24)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightblue",pch=22)
points(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"MASS_MED"],
       TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$DURATION_MAX<=100*60,"DURATION_MAX"]/60,col="black",bg="lightblue4",pch=22)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="lightgreen",pch=23)
points(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="darkseagreen4",pch=23)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M","DURATION_MAX"]/60,col="black",bg="plum",pch=21)
points(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","MASS_MED"],
       TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F","DURATION_MAX"]/60,col="black",bg="plum4",pch=21)

points(x = (TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MASS_MED"][2]
            + TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F","MASS_MED"][2])/2,
       y = 25,col="lightblue",bg="white",pch=22)
points(x = (TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"][1] 
            + TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"][1])/2,
       y = 33,col="lightgreen",bg="white",pch=23)
lines(x = c((TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"][1] 
           + TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M","MASS_MED"][1])/2,
           2700,
           (TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M","MASS_MED"][2]
            + TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F","MASS_MED"][2])/2),
      y = c(33,27,25))
text(x=c(2800),y=c(32),lab="Tyack et al. (2006):",
     pos=c(4),offset=c(0),font=2,cex=0.8)
text(x=c(2800),y=c(25),lab="cADL (Z. cavi.): 33 min",
     pos=c(4),offset=c(0),font=1,cex=0.8)
text(x=c(2800),y=c(18),lab="cADL (M. densi.): 25 min",
     pos=c(4),offset=c(0),font=1,cex=0.8)



#Create a data.frame of with mean of PGLMM parameter posterior distributions
PGLMM_DURATION_MAX_MASS_2_temp=as.data.frame(t(colMeans(PGLMM_DURATION_MAX_MASS_2$Sol)))

#add lines representing the specific random effects offsets of the intercepts at the FAM level (level 1)
lines(seq(100,14000,length=100),
      10^(PGLMM_DURATION_MAX_MASS_2_temp$'(Intercept)'
          +PGLMM_DURATION_MAX_MASS_2_temp$'log10(MASS_MED)'*log10(seq(100,14000,length=100))
          +PGLMM_DURATION_MAX_MASS_2_temp$'log10(IDDI_MED)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm"),"IDDI"],na.rm=T))),
      lty=1)
lines(seq(300,2500,length=100),
      10^(PGLMM_DURATION_MAX_MASS_2_temp$'(Intercept)'
          +PGLMM_DURATION_MAX_MASS_2_temp$'log10(MASS_MED)'*log10(seq(300,2500,length=100))
          +PGLMM_DURATION_MAX_MASS_2_temp$'log10(IDDI_MED)'*log10(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T))),
      lty=1)

text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="Mod. 2b (Mass & IDDI):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(T) =",round(PGLMM_DURATION_MAX_MASS_2_temp$'log10(MASS_MED)',2),"log(m) +",
               round(PGLMM_DURATION_MAX_MASS_2_temp$'log10(IDDI_MED)',2),"log(IDDI)",
               round(PGLMM_DURATION_MAX_MASS_2_temp$'(Intercept)',2)))
#text(x=grconvertX(0.98, from = "npc", to = "user"),
#     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
#     lab=paste("DIC :",round(PGLMM_DURATION_MAX_MASS_2$DIC,1)))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("R\U00B2 :",round(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_2),3)))

text(x=c(8000),y=c(43),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm"),"IDDI"],na.rm=T)/60,0),"min"),
     pos=c(4),offset=c(0),srt=c(8),font=3,cex=0.8)
text(x=c(1050),y=c(45),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T)/60,0),"min"),
     pos=c(4),offset=c(0),srt=c(60),font=3,cex=0.8)
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(d)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Max. Dive Duration (min)")



par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", ncol=5, x.intersp=0.5, inset=0.02, cex=1,title="Legend",title.adj=0.01,
       legend = c("P. electra (M or U)","P. electra  (F)","G. macrorhynchus (M)","G. macrorhynchus (F)",
                  "M. densirostris (M)","M. densirostris (F)","Z. cavirostris (M)","Z. cavirostris (F)",
                  "P. macrocephalus (M)","P. macrocephalus (F)"), 
       pt.bg=c("lightpink","lightpink4","lightsalmon","lightsalmon4",
               "lightblue","lightblue4","lightgreen","darkseagreen4","plum","plum4"), pch=c(25,25,24,24,22,22,23,23,21,21))

dev.off()


#####   Figure 4. Dive profile plots highlighting differences in IDDI  #####

pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "BEHAV_SERIES_ALL",".pdf",sep=""),height=11,width=8.5)
par(oma=c(0,1,1,1),mar=c(3,5,5.5,2),mfrow=c(3,2))
BEHAV_SERIES_PLOT(DATA_SERIES=SERIES[SERIES$Ptt==133069,], DATA_BEHAV=BEHAV[BEHAV$Ptt==133069,],
                  START="2014/05/01 03:00:00 GMT", 
                  END="2014/05/01 11:00:00 GMT",
                  COL_SERIES = NA ,COL_BEHAV = "lightpink", XLAB=NA,YLAB=NA,
                  CEX.AXIS = 1,INTV_Y=200, N_X_AXIS=11,TIME_DIG_1=11)
mtext("Depth (m)",side=2,line=3.5)  
mtext("Time",side=3,line=4)
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.23, from = "npc", to = "user"),pos=4,offset=0,cex=1,font=2,
     lab="Peponocephala electra")
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(Z"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),0))~"m"))
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(T"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60,0))~"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=paste("med(IDDI) = ",round(median(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","IDDI"]/60,na.rm=T),0),"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote(rho[' T, IDDI'] ~ " = " ~ .(round(cor(y=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DurationMin"],
                                                                  x=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","IDDI"]),2))))


BEHAV_SERIES_PLOT(DATA_SERIES=SERIES[SERIES$Ptt==112031,], DATA_BEHAV=BEHAV[BEHAV$Ptt==112031,],
                  START="2012/06/16 04:00:00 GMT", 
                  END="2012/06/16 12:00:00 GMT",
                  COL_SERIES = "lightsalmon",COL_BEHAV = NA, XLAB=NA,YLAB=NA,
                  CEX.AXIS = 1,INTV_Y=200, N_X_AXIS=11,TIME_DIG_1=11)
mtext("Depth (m)",side=2,line=3.5)  
mtext("Time",side=3,line=4)
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.23, from = "npc", to = "user"),pos=4,offset=0,cex=1,font=2,
     lab="Globicephala macrorhynchus")
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(Z"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),0))~"m"))
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(T"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60,0))~"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=paste("med(IDDI) = ",round(median(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","IDDI"]/60,na.rm=T),0),"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote(rho[' T, IDDI'] ~ " = " ~ .(round(cor(y=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DurationMin"],
                                                      x=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","IDDI"]),2))))


BEHAV_SERIES_PLOT(DATA_SERIES=SERIES[SERIES$Ptt==111674,], DATA_BEHAV=BEHAV[BEHAV$Ptt==111674,],
                  START="2013/06/25 06:00:00 GMT", 
                  END="2013/06/25 14:00:00 GMT",
                  COL_SERIES = "plum4",COL_BEHAV = NA, XLAB=NA,YLAB=NA,
                  CEX.AXIS = 1,INTV_Y=200, N_X_AXIS=11,TIME_DIG_1=11)
mtext("Depth (m)",side=2,line=3.5)  
mtext("Time",side=3,line=4)
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.23, from = "npc", to = "user"),pos=4,offset=0,cex=1,font=2,
     lab="Physeter macrocephalus")
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(Z"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),0))~"m"))
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(T"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60,0))~"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=paste("med(IDDI) = ",round(median(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","IDDI"]/60,na.rm=T),0),"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote(rho[' T, IDDI'] ~ " = " ~ .(round(cor(y=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DurationMin"],
                                                      x=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","IDDI"]),2))))


BEHAV_SERIES_PLOT(DATA_SERIES=SERIES[SERIES$Ptt==108419,], DATA_BEHAV=BEHAV[BEHAV$Ptt==108419,],
                  START="2011/06/05 16:30:00 GMT", 
                  END="2011/06/06 00:30:00 GMT",
                  COL_SERIES = "lightblue3",COL_BEHAV = NA, XLAB=NA,YLAB=NA,
                  CEX.AXIS = 1,INTV_Y=200, N_X_AXIS=11,TIME_DIG_1=11)
mtext("Depth (m)",side=2,line=3.5)  
mtext("Time",side=3,line=4)
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.23, from = "npc", to = "user"),pos=4,offset=0,cex=1,font=2,
     lab="Mesoplodon densirostris")
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(Z"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),0))~"m"))
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(T"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60,0))~"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=paste("med(IDDI) = ",round(median(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","IDDI"]/60,na.rm=T),0),"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote(rho[' T, IDDI'] ~ " = " ~ .(round(cor(y=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DurationMin"],
                                                      x=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","IDDI"]),2))))


BEHAV_SERIES_PLOT(DATA_SERIES=SERIES[SERIES$Ptt==129712,], DATA_BEHAV=BEHAV[BEHAV$Ptt==129712,],
                  START="2013/06/19 01:30:00 GMT", 
                  END="2013/06/19 09:30:00 GMT",
                  COL_SERIES = "palegreen3",COL_BEHAV = NA, XLAB=NA,YLAB=NA,
                  CEX.AXIS = 1,INTV_Y=200, N_X_AXIS=11,TIME_DIG_1=11)
mtext("Depth (m)",side=2,line=3.5)  
mtext("Time",side=3,line=4)
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.23, from = "npc", to = "user"),pos=4,offset=0,cex=1,font=2,
     lab="Ziphius cavirostris")
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(Z"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),0))~"m"))
text(x=grconvertX(0.07, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote("med(T"['foraging'] ~ ") =" ~ .(round(median(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60,0))~"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.14, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=paste("med(IDDI) = ",round(median(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","IDDI"]/60,na.rm=T),0),"min"))
text(x=grconvertX(0.52, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=4,offset=0,cex=1,
     lab=bquote(rho[' T, IDDI'] ~ " = " ~ .(round(cor(y=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DurationMin"],
                                                      x=BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","IDDI"]),2))))

dev.off()

#####   Figure 5. IDDI: dive duration ratio boxplots and dive efficiency boxplots #####

# Boxplots comparing distributions estimated from time-at-temperature (TAT) from SPOT satellite tags
# time-at-depth (TAD) summaries generated from directly observed dive depth time series from SPLASH tag deployments
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "DIVE_EFFICIENCY_BOXPLOTS",".pdf",sep=""),height=4,width=9)
par(mar=c(3,4,2,0),oma=c(0,0,0,2),mfrow=c(1,2))

boxplot.quant(c(rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0 & TAT$T46==0,5:15][1:127,]),
                rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,5:15])+0.5*TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,4]),
              c(rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0,9:15]),
                (rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0 ,5:15])+0.5*TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0,4])),
              c(rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Day",10:15]),
                rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Night",10:15])),
              c(rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Day",12:15]),
                rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Night",12:15])),
              c(rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Day",13:15]),
                rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Night",13:15])),
              col=c("lightpink","lightsalmon","plum","lightblue","lightgreen"),
              pars=list(boxwex=0.50),range=0,cex.axis=0.85,las=1,
              names=c("P. elec.","G. mac.","P. mac.","M. dens.","Z. cav."),ylab=NA)
abline(v=c(1.5,2.5,3.5,4.5))
mtext("% Time in Foraging Strata",side=2,line=2.5)
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(a)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

boxplot(BEHAV[BEHAV$SPP=="Pe","IDDI"]/BEHAV[BEHAV$SPP=="Pe","DurationMin"],
        BEHAV[BEHAV$SPP=="Gm","IDDI"]/BEHAV[BEHAV$SPP=="Gm","DurationMin"],
        BEHAV[BEHAV$SPP=="Pm","IDDI"]/BEHAV[BEHAV$SPP=="Pm","DurationMin"],
        BEHAV[BEHAV$SPP=="Md","IDDI"]/BEHAV[BEHAV$SPP=="Md","DurationMin"],
        BEHAV[BEHAV$SPP=="Zc","IDDI"]/BEHAV[BEHAV$SPP=="Zc","DurationMin"],
        ylim=c(0,4),ylab=NA,boxwex=0.5,cex.axis=0.85,las=1,
        names=c("P. elec.","G. mac.","P. mac.","M. dens.","Z. cav."),
        col=c("lightpink","lightsalmon","plum","lightblue","lightgreen"))
abline(v=c(1.5,2.5,3.5,4.5))
mtext("IDDI (min):Dive Duration (min)",side=2,line=2)
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.04, from = "npc", to = "user"),lab="(b)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

dev.off()

###### Analysis 3. Fit phylogenetic generalized least squares (PGLS) model of maximum duration per species functions of MASS, MB, and/or IDDI #####

library(ape)
library(geiger)
library(nlme)
library(phytools)

# compare the different fixed effect structures accounting for MASS, Mb_MAX, IDDI while defining the correlation structure
PGLS_DURATION_MAX_1 <- gls(log10(T_MAX) ~ log10(MASS) + log10(Mb_MAX) + log10(IDDI),
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_2 <- gls(log10(T_MAX) ~ log10(MASS) + log10(IDDI), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_3 <- gls(log10(T_MAX) ~ log10(MASS) + log10(Mb_MAX), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_4 <- gls(log10(T_MAX) ~ log10(Mb_MAX) + log10(IDDI), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_5 <- gls(log10(T_MAX) ~ log10(MASS), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_6 <- gls(log10(T_MAX) ~ log10(Mb_MAX), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")
PGLS_DURATION_MAX_7 <- gls(log10(T_MAX) ~ log10(IDDI), 
                           correlation = corBrownian(phy = PHYLO_TREE),
                           data = PHYLO_DATA, method = "ML")

anova(PGLS_DURATION_MAX_1,PGLS_DURATION_MAX_2,PGLS_DURATION_MAX_3,
      PGLS_DURATION_MAX_4,PGLS_DURATION_MAX_5,PGLS_DURATION_MAX_6,PGLS_DURATION_MAX_7)



###### Figure 6. Phylogenetic generalized least squares (PGLS) model of maximum duration per species functions of MASS, MB, and/or IDDI #####

# plot the phylogenetic tree hypothesis for the crown cetacean lineages from McGowen et al. 2009 
# for the species represented in PHYLO_DATA
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "PGLS_DURATION_MAX_MASS",".pdf",sep=""),height=3.5,width=8.5)

par(oma=c(0,0,0,0),mar=c(5,5,3,1),mfrow=c(1,2),xpd=TRUE)


#Plot of Noren and Williams 2000 model fit overlaid on observed data
mar=c(5,5,3,3)
plot(1:10,1:10,type="n",xlim=c(0,14000),ylim=c(8,140),xlab=NA,ylab=NA)
text(PHYLO_DATA$MASS,PHYLO_DATA$T_MAX,labels=PHYLO_DATA$SPP,cex=0.7,
     col=c("purple",rep("red",5),rep("blue",3),rep("red",2),"purple"))
#points(PHYLO_DATA$MASS,PHYLO_DATA$T_MAX)

lines(x=seq(10,14000,length=100),
      y=(10^predict(PGLS_DURATION_MAX_2,data.frame(MASS=seq(10,13000,length=100),
                                                   IDDI=mean(PHYLO_DATA[!PHYLO_DATA$SPP%in%c("Zc","Md","Ha","Pm","Kb"),"IDDI"]),
                                                   Mb_MAX=mean(PHYLO_DATA[!PHYLO_DATA$SPP%in%c("Zc","Md","Ha","Pm","Kb"),"Mb_MAX"])))),
      lty=1,lwd=1,col="red")

lines(x=seq(10,14000,length=100),
      y=(10^predict(PGLS_DURATION_MAX_2,data.frame(MASS=seq(100,13000,length=100),
                                                   IDDI=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Pm","Kb"),"IDDI"]),
                                                   Mb_MAX=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Pm","Kb"),"Mb_MAX"])))),
      lty=1,lwd=1,col="purple")

#lines(x=seq(10,13000,length=100),
#      y=(10^predict(PGLS_DURATION_MAX_2,data.frame(MASS=seq(100,13000,length=100),
#                                                   IDDI=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Kb"),"IDDI"]),
#                                                   Mb_MAX=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Kb"),"Mb_MAX"])))),
#      lty=1,lwd=1,col="purple")

lines(x=seq(10,14000,length=100),
      y=(10^predict(PGLS_DURATION_MAX_2,data.frame(MASS=seq(10,13000,length=100),
                                                   IDDI=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Zc","Md","Ha"),"IDDI"]),
                                                   Mb_MAX=mean(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Zc","Md","Ha"),"Mb_MAX"])))),
      lty=1,lwd=1,col="blue")


text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.7,font=2,
     lab="Mod. 2c (Mass & IDDI):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.7,
     lab=paste("log(T) =",round(summary(PGLS_DURATION_MAX_2)$coeff[2],2),"log(m) + ",
               round(summary(PGLS_DURATION_MAX_2)$coeff[3],2),"log(IDDI) +",
               round(summary(PGLS_DURATION_MAX_2)$coeff[1],2)))

text(x=c(7000),y=c(28),lab=bquote(paste("med(IDDI"["Delphinoidea"],") = ",.(round(median(PHYLO_DATA[!PHYLO_DATA$SPP%in%c("Zc","Md","Ha","Pm","Kb"),"IDDI"],na.rm=T),0))," min")),
     pos=c(4),offset=c(0),srt=c(4),font=3,cex=0.7,col="red")
text(x=c(7000),y=c(45),lab=bquote(paste("med(IDDI"["Physeteroidea"],") = ",.(round(median(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Pm","Kb"),"IDDI"],na.rm=T),0))," min")),
     pos=c(4),offset=c(0),srt=c(4),font=3,cex=0.7,col="purple")
text(x=c(7000),y=c(95),lab=bquote(paste("med(IDDI"["Ziphioidea"],") = ",.(round(median(PHYLO_DATA[PHYLO_DATA$SPP%in%c("Zc","Md","Ha"),"IDDI"],na.rm=T),0))," min")),
     pos=c(4),offset=c(0),srt=c(9),font=3,cex=0.7,col="blue")

mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Max. Dive Duration (min)")
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.15, from = "npc", to = "user"),lab="(a)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

library(ape)
PHYLO_TREE_temp=PHYLO_TREE
PHYLO_TREE_temp$tip.label=c("Stenella_attenuata","Globicephala_macrorhynchus",
                            "Peponocephala_electra","Orcinus_orca","Phocoena_phocoena",
                            "Delphinapterus_leucas","Monodon_monoceros","Mesoplodon_densirostris",
                            "Hyperoodon_ampullatus","Ziphius_cavirostris","Physeter_macrocephalus",
                            "Kogia_breviceps")

plot(PHYLO_TREE_temp,root.edge = T, label.offset = 0.5, cex=0.7,
     tip.color = c(rep("red",7),rep("blue",3),rep("purple",2)))
axisPhylo()
mtext(expression(paste("10"^"6"," Years BP")),side=1,line=2.5,at=15,cex=0.9)
remove(PHYLO_TREE_temp)
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.15, from = "npc", to = "user"),lab="(b)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

dev.off()


#####   Figure 7. Diurnal comparison plots #####

pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "BEHAV_DIURNAL_BOXPLOTS",".pdf",sep=""),height=5.5,width=7)
par(mar=c(5,5,3,3),oma=c(0,0,0,0),mfrow=c(1,1))
#Plot illustrating the mixed effects model fitted values including fixed effects (Level 0) and random effects (Level 1)
boxplot(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
         BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Day","DepthMin"]-
           median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Day","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Day","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Pm" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Day","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Md" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
        BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Day","DepthMin"]-
          median(BEHAV[BEHAV$What=="Dive" & BEHAV$FORAGING=="Y" & BEHAV$SPP=="Zc" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T),
         col=c("lightpink4","lightpink","lightsalmon4","lightsalmon","plum4","plum","lightblue4","lightblue",
               "darkseagreen4","lightgreen"),range=0, pars = list(cex.axis=0.9),
        names=rep(c("Night","Day"),5),ylim=c(1000,-1000), ylab = "Dive Depth Normalized to Night Median (m)",boxwex=0.5)
mtext(c("P. elec.","G. mac.","P. mac.","M. dens.","Z. cav."),at=c(1.5,3.5,5.5,7.5,9.5),side=c(1),line=c(2.7),cex=0.9)
text(2,0,"No Daytime Foraging Dives",srt=90)
abline(v=c(2.5,4.5,6.5,8.5))
abline(h=0,lty=3)
dev.off()



#####   Figure 8. Maps of overlap with dive range. #####

#map effect of threshold isobath on distribution of ARGOS_CTCRW locations (spreading of activity over areas where bathymetry is close enough to reach)
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/","MAP_DIVE_RANGES_ARGOS",".pdf",sep=""),
    height=11,width=8.5)
par(mar=c(3,3,3.35,5),oma=c(3,3,0,0),mfrow=c(3,2))

BATHY_temp=BATHY
BATHY_temp[BATHY_temp<50 | BATHY_temp>450]=NA
plot(BATHY_HILL,ylim=c(23,27),xlim=c(-79.6,-74.9), col=gray(0.8), legend=F)
plot(BATHY_temp,col=OCEAN_COL,add=T,legend=F)
plot(COAST,col=rgb(107/255,142/255,35/255, alpha=0.7),lty=0, add=T)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LAT"],col="lightpink4",bg=NA, pch=25,cex=0.5)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LAT"],col="lightpink",bg="lightpink", pch=25,cex=0.5)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pe","LAT"],col="lightpink4",bg="lightpink4", pch=25,cex=0.5)
legend(x=-79.05, y=24.5, legend=c("M","F","U"), col=c("lightpink","lightpink4","lightpink4"),pt.bg = c("lightpink","lightpink4",NA),title="P. elec.", box.lwd=2, pch=25,cex=1,pt.cex=1.2)
COLOR_BAR(-76.2,26.1,-75.8,26.6,COLORS=rev(OCEAN_COL),TICKS=c(450,50),ROUND=0,CEX=1)
text(-76.2,26.8,"Dive Range (m)",pos=4,offset=0)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-76.2,25.9,paste(round(mean(PROP_FIXES_temp),4)*100,"% fixes",sep=""),pos=4,offset=0)
text(-76.2,25.7,paste("over habitat"),pos=4,offset=0)
text(-76.2,25.5,paste("in dive range"),pos=4,offset=0)
remove(PROP_FIXES_temp)
NORTH_ARROW(c(-79.30,23.8))
LENGTH_BAR(c(-79.05,23.375))
#Labels
mtext(paste("Longitude (\U00B0","W)",sep=""),side=c(1),line=c(2.5),cex=0.8)
mtext(paste("Latitude (\U00B0","N)",sep=""),side=c(2),line=c(2.5),cex=0.8)


BATHY_temp=BATHY
BATHY_temp[BATHY_temp<50 | BATHY_temp>800]=NA
plot(BATHY_HILL,ylim=c(23,27),xlim=c(-79.6,-74.9), col=gray(0.8), legend=F)
plot(BATHY_temp,col=OCEAN_COL,add=T,legend=F)
plot(COAST,col=rgb(107/255,142/255,35/255, alpha=0.7),lty=0, add=T)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LAT"],col="lightsalmon4",bg=NA, pch=24,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LAT"],col="lightsalmon", pch=17,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Gm","LAT"],col="sienna4", pch=17,cex=0.6)
legend(x=-79.05, y=24.5, legend=c("M","F","U"), col=c("lightsalmon","sienna4","sienna4"), pt.bg=c("lightsalmon","sienna4",NA), title="G. mac.", box.lwd=2, pch=c(17,17,24),pt.cex=c(1.4,1.4,1.2),cex=1)
COLOR_BAR(-76.2,26.1,-75.8,26.6,COLORS=rev(OCEAN_COL),TICKS=c(800,50),ROUND=0,CEX=1)
text(-76.2,26.8,"Dive Range (m)",pos=4,offset=0)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-76.2,25.9,paste(round(mean(PROP_FIXES_temp),3)*100,"% fixes",sep=""),pos=4,offset=0)
text(-76.2,25.7,paste("over habitat"),pos=4,offset=0)
text(-76.2,25.5,paste("in dive range"),pos=4,offset=0)
remove(PROP_FIXES_temp)
NORTH_ARROW(c(-79.30,23.8))
LENGTH_BAR(c(-79.05,23.375))
#Labels
mtext(paste("Longitude (\U00B0","W)",sep=""),side=c(1),line=c(2.5),cex=0.8)
mtext(paste("Latitude (\U00B0","N)",sep=""),side=c(2),line=c(2.5),cex=0.8)

BATHY_temp=BATHY
BATHY_temp[BATHY_temp<600 | BATHY_temp>1350]=NA
plot(BATHY_HILL,ylim=c(23,27),xlim=c(-79.6,-74.9), col=gray(0.8), legend=F)
plot(BATHY_temp,col=OCEAN_COL,add=T,legend=F)
plot(COAST,col=rgb(107/255,142/255,35/255, alpha=0.7),lty=0, add=T)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LAT"],col="plum",bg=NA, pch=21,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LAT"],col="plum", pch=16,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Pm","LAT"],col="darkorchid4", pch=16,cex=0.6)
legend(x=-79.05, y=24.5, legend=c("M","F","U"), col=c("plum","darkorchid4","darkorchid4"),pt.bg=c("plum","darkorchid4",NA), title="P. mac.", box.lwd=2, pch=c(16,16,21),pt.cex=c(1.4,1.4,1.2),cex=1)
COLOR_BAR(-76.2,26.1,-75.8,26.6,COLORS=rev(OCEAN_COL),TICKS=c(1350,600),ROUND=0,CEX=1)
text(-76.2,26.8,"Dive Range (m)",pos=4,offset=0)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-76.2,25.9,paste(round(mean(PROP_FIXES_temp),3)*100,"% fixes",sep=""),pos=4,offset=0)
text(-76.2,25.7,paste("over habitat"),pos=4,offset=0)
text(-76.2,25.5,paste("in dive range"),pos=4,offset=0)
remove(PROP_FIXES_temp)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="M","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="M","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="M","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-75.9,25.2,paste(round(mean(PROP_FIXES_temp),3)*100,"% M",sep=""),pos=4,offset=0)
remove(PROP_FIXES_temp)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="F","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="F","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="F","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-75.9,25.0,paste(round(mean(PROP_FIXES_temp),3)*100,"% F",sep=""),pos=4,offset=0)
remove(PROP_FIXES_temp)
NORTH_ARROW(c(-79.30,23.8))
LENGTH_BAR(c(-79.05,23.375))
#Labels
mtext(paste("Longitude (\U00B0","W)",sep=""),side=c(1),line=c(2.5),cex=0.8)
mtext(paste("Latitude (\U00B0","N)",sep=""),side=c(2),line=c(2.5),cex=0.8)


BATHY_temp=BATHY
BATHY_temp[BATHY_temp<650 | BATHY_temp>1800]=NA
plot(BATHY_HILL,ylim=c(23,27),xlim=c(-79.6,-74.9), col=gray(0.8), legend=F)
plot(BATHY_temp,col=OCEAN_COL,add=T,legend=F)
plot(COAST,col=rgb(107/255,142/255,35/255, alpha=0.7),lty=0, add=T)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LAT"],col="navy", pch=18,cex=0.8)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LAT"],col="blue",bg=NA, pch=23,cex=0.8)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Md","LAT"],col="blue", pch=18,cex=0.8)
legend(x=-79.05, y=24.5, legend=c("M","F","U"), col=c("blue","navy","navy"),pt.bg =c("blue","navy",NA), title="M. dens.", box.lwd=2, pch=c(18,18,23),pt.cex=c(1.4,1.4,1.2), cex=1)
COLOR_BAR(-76.2,26.1,-75.8,26.6,COLORS=rev(OCEAN_COL),TICKS=c(1800,650),ROUND=0,CEX=1)
text(-76.2,26.8,"Dive Range (m)",pos=4,offset=0)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-76.2,25.9,paste(round(mean(PROP_FIXES_temp),3)*100,"% fixes",sep=""),pos=4,offset=0)
text(-76.2,25.7,paste("over habitat"),pos=4,offset=0)
text(-76.2,25.5,paste("in dive range"),pos=4,offset=0)
remove(PROP_FIXES_temp)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="M","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="M","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="M","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-75.9,25.2,paste(round(mean(PROP_FIXES_temp),3)*100,"% M",sep=""),pos=4,offset=0)
remove(PROP_FIXES_temp)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="F","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="F","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md" & ARGOS_CTCRW$Sex=="F","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-75.9,25.0,paste(round(mean(PROP_FIXES_temp),3)*100,"% F",sep=""),pos=4,offset=0)
remove(PROP_FIXES_temp)
NORTH_ARROW(c(-79.30,23.8))
LENGTH_BAR(c(-79.05,23.375))
#Labels
mtext(paste("Longitude (\U00B0","W)",sep=""),side=c(1),line=c(2.5),cex=0.8)
mtext(paste("Latitude (\U00B0","N)",sep=""),side=c(2),line=c(2.5),cex=0.8)


BATHY_temp=BATHY
BATHY_temp[BATHY_temp<800 | BATHY_temp>1900]=NA
plot(BATHY_HILL,ylim=c(23,27),xlim=c(-79.6,-74.9), col=gray(0.8), legend=F)
plot(BATHY_temp,col=OCEAN_COL,add=T,legend=F)
plot(COAST,col=rgb(107/255,142/255,35/255, alpha=0.7),lty=0, add=T)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("F") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LAT"],col="darkgreen", pch=15,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("U","F or M","U or AF") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LAT"],col="green",bg=NA, pch=22,cex=0.6)
points(ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LONG"],
       ARGOS_CTCRW[ARGOS_CTCRW$Sex%in%c("M") & ARGOS_CTCRW$LOCTYPE=="o" & ARGOS_CTCRW$SPP=="Zc","LAT"],col="green", pch=15,cex=0.6)
legend(x=-79.05, y=24.5, legend=c("M","F","U"), col=c("green","darkgreen","darkgreen"),pt.bg =c("green","darkgreen",NA), pt.cex=c(1.4,1.4,1.2), pch=c(15,15,22), title="Z. cav.", box.lwd=2,cex=1)
COLOR_BAR(-76.2,26.1,-75.8,26.6,COLORS=rev(OCEAN_COL),TICKS=c(1900,800),ROUND=0,CEX=1)
text(-76.2,26.8,"Dive Range (m)",pos=4,offset=0)
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
text(-76.2,25.9,paste(round(mean(PROP_FIXES_temp),3)*100,"% fixes",sep=""),pos=4,offset=0)
text(-76.2,25.7,paste("over habitat"),pos=4,offset=0)
text(-76.2,25.5,paste("in dive range"),pos=4,offset=0)
remove(PROP_FIXES_temp)
NORTH_ARROW(c(-79.30,23.8))
LENGTH_BAR(c(-79.05,23.375))
#Labels
mtext(paste("Longitude (\U00B0","W)",sep=""),side=c(1),line=c(2.5),cex=0.8)
mtext(paste("Latitude (\U00B0","N)",sep=""),side=c(2),line=c(2.5),cex=0.8)

#Inset
par(new = TRUE,mar=c(0.2,0.2,0.2,0.2),fig = c(grconvertX(x=c(-76.35,-75.25)-0.45, from = "user", to = "ndc"), 
                                              grconvertY(y=c(23.15,24.25)-0.55, from = "user", to = "ndc")))
plot(COAST_LOW,bg=OCEAN_COL[70],xlim=c(-90,-70),ylim=c(15,35),col=rgb(107/255,142/255,35/255, alpha=0.7), axes=T,labels=NA,tck = 0.0001)
rect(-79.5,23,-75.1,27,lwd=0.7,border="red")

dev.off()

###### Analysis S1. Fit linear models of MASS as a function of LENGTH #####

#Fit log-log linear models of MASS as a function of length for each SPP that will be used to
#predict mean and median MASS for each SEX and AGE class based on LENGTH frequency of strandings
LM_Pe_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Pe" & !is.na(MORPH$MASS),])
LM_Gm_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Gm" & !is.na(MORPH$MASS),])
LM_Md_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Md" & !is.na(MORPH$MASS),])
LM_Me_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Me" & !is.na(MORPH$MASS),])
LM_Zc_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Zc" & !is.na(MORPH$MASS),])
LM_Pm_MORPH=lm(log10(MASS)~log10(LENGTH),data=MORPH[MORPH$SPP=="Pm" & !is.na(MORPH$MASS),])

###### Figure S1. Plot linear models of MASS as a function of LENGTH #####

#Fit MASS to LENGTH curves for each SPP and use mean adult LENGTH to predict mean MASS for each SEX

dev.off() #turn off any activ devices, for some reason interferes with split.screen even when in a new device 
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "LM_MORPH",".pdf",sep=""),height=11*1.5,width=8.5*1.5)
split.screen(figs=rbind(c(0.00, 0.50, 0.999-(0.2*0.333), 0.999),
                        c(0.00, 0.50, 0.666, 0.999-(0.2*0.333)),
                        c(0.50, 1.00, 0.999-(0.2*0.333), 0.999),
                        c(0.50, 1.00, 0.666, 0.999-(0.2*0.333)),
                        
                        c(0.00, 0.50, 0.666-(0.2*0.333), 0.666),
                        c(0.00, 0.50, 0.333, 0.666-(0.2*0.333)),
                        c(0.50, 1.00, 0.666-(0.2*0.333), 0.666),
                        c(0.50, 1.00, 0.333, 0.666-(0.2*0.333)),
                        
                        c(0.00, 0.50, 0.333-(0.2*0.333), 0.333),
                        c(0.00, 0.50, 0, 0.333-(0.2*0.333)),
                        c(0.50, 1.00, 0.333-(0.2*0.333), 0.333),
                        c(0.50, 1.00, 0, 0.333-(0.2*0.333))))
screen(1)
par(mar=c(0,5,0,3))
HIST_HORIZ(MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="F","LENGTH"],AXES=F,
           BREAKS=seq(0.75,3.0,length=20),YLIM=c(0,50),ADD=F,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightpink4")[1]/255,col2rgb("lightpink4")[2]/255,col2rgb("lightpink4")[3]/255,alpha=1))
HIST_HORIZ(MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="M","LENGTH"],AXES=F,
           BREAKS=seq(0.75,3.0,length=20),ADD=T,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightpink")[1]/255,col2rgb("lightpink")[2]/255,col2rgb("lightpink")[3]/255,alpha=0.5))

screen(2)
par(mar=c(5,5,0,2))
plot(1:10,1:10,type="n",xlim=c(0.75,3.0),ylim=c(0,270),
     xlab="Length (m)",ylab="Body Mass (kg)")
points(MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="M","MASS"],
       pch=25,col="lightpink",bg="lightpink")
points(MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Pe" & MORPH$SEX=="F","MASS"],
       pch=25,col="lightpink4",bg="lightpink4")
lines(x=seq(0.75,2.8,length=100),y=10^predict(LM_Pe_MORPH,data.frame(LENGTH=seq(0.75,2.8,length=100))))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("log(m) =", round(summary(LM_Pe_MORPH)$coeff[2,1],3),"* log(L) +",
               round(summary(LM_Pe_MORPH)$coeff[1,1],3)))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("R\U00B2 :",round(summary(LM_Pe_MORPH)$adj.r.squared,3)))

lines(x=c(rep(MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="lightpink4",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="lightpink",lty=2)
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]+10,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (M):",round(MASS[MASS$SPP=="Pe" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]-10,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (F):",round(MASS[MASS$SPP=="Pe" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))

legend(x=grconvertX(0.77, from = "npc", to = "user"),
       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=25,col=c("lightpink","lightpink4"),pt.bg=c("lightpink","lightpink4"),
       legend=c("Male","Female"),title="P. electra",cex=0.9)

screen(3)
par(mar=c(0,5,0,3))
#par(fig=c(0,0.8,0.598,0.95), new=TRUE)
HIST_HORIZ(MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="F","LENGTH"],AXES=F,
           BREAKS=seq(1.5,5.5,length=20),YLIM=c(0,30),ADD=F,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightsalmon4")[1]/255,col2rgb("lightsalmon4")[2]/255,col2rgb("lightsalmon4")[3]/255,alpha=1))
HIST_HORIZ(MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="M","LENGTH"],AXES=F,
           BREAKS=seq(1.5,5.5,length=20),ADD=T,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightsalmon")[1]/255,col2rgb("lightsalmon")[2]/255,col2rgb("lightsalmon")[3]/255,alpha=0.5))

screen(4)
par(mar=c(5,5,0,2))
plot(1:10,1:10,type="n",xlim=c(1.5,5.5),ylim=c(0,2000),
     xlab="Length (m)",ylab="Body Mass (kg)")
points(MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="M","MASS"],
       pch=17,col="lightsalmon",cex=1.2)
points(MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Gm" & MORPH$SEX=="F","MASS"],
       pch=17,col="lightsalmon4",cex=1.2)
lines(x=seq(1.5,5.5,length=100),y=10^predict(LM_Gm_MORPH,data.frame(LENGTH=seq(1.5,5.5,length=100))))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("log(m) =", round(summary(LM_Gm_MORPH)$coeff[2,1],3),"* log(L) +",
               round(summary(LM_Gm_MORPH)$coeff[1,1],3)))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("R\U00B2 :",round(summary(LM_Gm_MORPH)$adj.r.squared,3)))
lines(x=c(rep(MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="lightsalmon4",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Gm" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Gm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="lightsalmon",lty=2)
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Gm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]+65,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (M):",round(MASS[MASS$SPP=="Gm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]+65,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (F):",round(MASS[MASS$SPP=="Gm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))
legend(x=grconvertX(0.77, from = "npc", to = "user"),
       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=17,col=c("lightsalmon","lightsalmon4"),
       legend=c("Male","Female"),title="G. macrorhync.",cex=0.9, pt.cex = 1.2)

screen(5)
par(mar=c(0,5,0,3))
#par(fig=c(0,0.8,0.598,0.95), new=TRUE)
HIST_HORIZ(MORPH[MORPH$SPP=="Md" & MORPH$SEX=="F","LENGTH"],AXES=F,
           BREAKS=seq(1.8,5,length=20),YLIM=c(0,30),ADD=F,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightblue4")[1]/255,col2rgb("lightblue4")[2]/255,col2rgb("lightblue4")[3]/255,alpha=1))
HIST_HORIZ(MORPH[MORPH$SPP=="Md" & MORPH$SEX=="M","LENGTH"],AXES=F,
           BREAKS=seq(1.8,5,length=20),ADD=T,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightblue")[1]/255,col2rgb("lightblue")[2]/255,col2rgb("lightblue")[3]/255,alpha=0.5))

screen(6)
par(mar=c(5,5,0,2))
plot(1:10,1:10,type="n",xlim=c(1.8,5),ylim=c(0,1400),
     xlab="Length (m)",ylab="Body Mass (kg)")
points(MORPH[MORPH$SPP=="Md" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Md" & MORPH$SEX=="M","MASS"],
       pch=15,col="lightblue",cex=1.2)
points(MORPH[MORPH$SPP=="Md" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Md" & MORPH$SEX=="F","MASS"],
       pch=15,col="lightblue4",cex=1.2)
lines(x=seq(1.8,4.8,length=100),y=10^predict(LM_Md_MORPH,data.frame(LENGTH=seq(1.8,4.8,length=100))))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("log(m) =", round(summary(LM_Md_MORPH)$coeff[2,1],3),"* log(L) +",
               round(summary(LM_Md_MORPH)$coeff[1,1],3)))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("R\U00B2 :",round(summary(LM_Md_MORPH)$adj.r.squared,3)))
lines(x=c(rep(MASS[MASS$SPP=="Md" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Md" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="lightblue4",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Md" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Md" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="lightblue",lty=2)
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Md" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]-50,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (M):",round(MASS[MASS$SPP=="Md" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Md" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]+50,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (F):",round(MASS[MASS$SPP=="Md" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))
legend(x=grconvertX(0.77, from = "npc", to = "user"),
       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=15,col=c("lightblue","lightblue4"),
       legend=c("Male","Female"),title="M. densirostris",cex=0.9, pt.cex = 1.2)

#screen(7)
#par(mar=c(0,5,0,3))
#HIST_HORIZ(MORPH[MORPH$SPP=="Me" & MORPH$SEX=="F","LENGTH"],AXES=F,
#           BREAKS=seq(1.8,5,length=20),YLIM=c(0,30),ADD=F,XLAB=NA,YLAB=NA,
#           COL=rgb(col2rgb("goldenrod4")[1]/255,col2rgb("goldenrod4")[2]/255,col2rgb("goldenrod4")[3]/255,alpha=1))
#HIST_HORIZ(MORPH[MORPH$SPP=="Me" & MORPH$SEX=="M","LENGTH"],AXES=F,
#           BREAKS=seq(1.8,5,length=20),ADD=T,XLAB=NA,YLAB=NA,
#           COL=rgb(col2rgb("goldenrod")[1]/255,col2rgb("goldenrod")[2]/255,col2rgb("goldenrod")[3]/255,alpha=0.5))

#screen(8)
#par(mar=c(5,5,0,2))
#plot(1:10,1:10,type="n",xlim=c(1.8,5),ylim=c(0,1400),
#     xlab="Length (m)",ylab="Body Mass (kg)")
#points(MORPH[MORPH$SPP=="Me" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Me" & MORPH$SEX=="M","MASS"],
#       pch=20,col="goldenrod")
#points(MORPH[MORPH$SPP=="Me" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Me" & MORPH$SEX=="F","MASS"],
#       pch=20,col="goldenrod4")
#lines(x=seq(1.8,4.8,length=100),y=10^predict(LM_Me_MORPH,data.frame(LENGTH=seq(1.8,4.8,length=100))))
#text(x=grconvertX(0.75, from = "npc", to = "user"),
#     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
#     lab=paste("log(m) =", round(summary(LM_Me_MORPH)$coeff[2,1],3),"* log(L) +",
#               round(summary(LM_Me_MORPH)$coeff[1,1],3)))
#text(x=grconvertX(0.75, from = "npc", to = "user"),
#     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
#     lab=paste("R\U00B2 :",round(summary(LM_Me_MORPH)$adj.r.squared,3)))
#lines(x=c(rep(MASS[MASS$SPP=="Me" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
#      y=c(50000,rep(MASS[MASS$SPP=="Me" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="goldenrod4",lty=2)
#lines(x=c(rep(MASS[MASS$SPP=="Me" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
#      y=c(50000,rep(MASS[MASS$SPP=="Me" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="goldenrod",lty=2)
#text(x=grconvertX(x=0.02, from = "npc", to = "user"),
#     y=MASS[MASS$SPP=="Me" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]-50,pos=4,offset=0,cex=0.9,
#     lab=paste("Med. Mass (M):",round(MASS[MASS$SPP=="Me" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
#text(x=grconvertX(x=0.02, from = "npc", to = "user"),
#     y=MASS[MASS$SPP=="Me" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]+50,pos=4,offset=0,cex=0.9,
#     lab=paste("Med. Mass (F):",round(MASS[MASS$SPP=="Me" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))
#legend(x=grconvertX(0.77, from = "npc", to = "user"),
#       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=20,col=c("goldenrod","goldenrod4"),
#       legend=c("Male","Female"),title="M. europaeus",cex=0.9)

screen(7)
par(mar=c(0,5,0,3))
HIST_HORIZ(MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="F","LENGTH"],AXES=F,
           BREAKS=seq(2.5,7,length=20),YLIM=c(0,30),ADD=F,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("darkseagreen4")[1]/255,col2rgb("darkseagreen4")[2]/255,col2rgb("darkseagreen4")[3]/255,alpha=1))
HIST_HORIZ(MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="M","LENGTH"],AXES=F,
           BREAKS=seq(2.5,7,length=20),ADD=T,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("lightgreen")[1]/255,col2rgb("lightgreen")[2]/255,col2rgb("lightgreen")[3]/255,alpha=0.5))

screen(8)
par(mar=c(5,5,0,2))
plot(1:10,1:10,type="n",xlim=c(2.5,7),ylim=c(0,2700),
     xlab="Length (m)",ylab="Body Mass (kg)")
points(MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="M","MASS"],
       pch=18,col="lightgreen",cex=1.3)
points(MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Zc" & MORPH$SEX=="F","MASS"],
       pch=18,col="darkseagreen4",cex=1.3)
lines(x=seq(2.5,6.7,length=100),y=10^predict(LM_Zc_MORPH,data.frame(LENGTH=seq(2.5,6.7,length=100))))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("log(m) =", round(summary(LM_Zc_MORPH)$coeff[2,1],3),"* log(L) +",
               round(summary(LM_Zc_MORPH)$coeff[1,1],3)))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("R\U00B2 :",round(summary(LM_Zc_MORPH)$adj.r.squared,3)))
lines(x=c(rep(MASS[MASS$SPP=="Zc" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Zc" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="darkseagreen4",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Zc" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(50000,rep(MASS[MASS$SPP=="Zc" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="lightgreen",lty=2)
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Zc" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]+85,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (M):",round(MASS[MASS$SPP=="Zc" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Zc" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]-85,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (F):",round(MASS[MASS$SPP=="Zc" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))
legend(x=grconvertX(0.77, from = "npc", to = "user"),
       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=18,col=c("lightgreen","darkseagreen4"),
       legend=c("Male","Female"),title="Z. cavirostris",cex=0.9, pt.cex = 1.3)

screen(9)
par(mar=c(0,5,0,3))
HIST_HORIZ(MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="F","LENGTH"],AXES=F,
           BREAKS=seq(3,19,length=20),YLIM=c(0,35),ADD=F,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("plum4")[1]/255,col2rgb("plum4")[2]/255,col2rgb("plum4")[3]/255,alpha=1))
HIST_HORIZ(MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="M","LENGTH"],AXES=F,
           BREAKS=seq(3,19,length=20),ADD=T,XLAB=NA,YLAB=NA,
           COL=rgb(col2rgb("plum")[1]/255,col2rgb("plum")[2]/255,col2rgb("plum")[3]/255,alpha=0.5))

screen(10)
par(mar=c(5,5,0,2))
plot(1:10,1:10,type="n",xlim=c(3,19),ylim=c(0,54000),
     xlab="Length (m)",ylab="Body Mass (kg)")
points(MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="M","LENGTH"], MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="M","MASS"],
       pch=16,col="plum",cex=1.2)
points(MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="F","LENGTH"], MORPH[MORPH$SPP=="Pm" & MORPH$SEX=="F","MASS"],
       pch=16,col="plum4",cex=1.2)
lines(x=seq(3,19,length=100),y=10^predict(LM_Pm_MORPH,data.frame(LENGTH=seq(3,19,length=100))))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.07, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("log(m) =", round(summary(LM_Pm_MORPH)$coeff[2,1],3),"* log(L) +",
               round(summary(LM_Pm_MORPH)$coeff[1,1],3)))
text(x=grconvertX(0.75, from = "npc", to = "user"),
     y=grconvertY(y=0.12, from = "npc", to = "user"),pos=2,offset=0,cex=0.9,
     lab=paste("R\U00B2 :",round(summary(LM_Pm_MORPH)$adj.r.squared,3)))
lines(x=c(rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="F" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(55000,rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],2)),col="plum4",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="A","LENGTH_MED"],2),0),
      y=c(55000,rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],2)),col="plum",lty=2)
lines(x=c(rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="SA","LENGTH_MED"],2),0),
      y=c(55000,rep(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"],2)),col="plum",lty=2)
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"]+1500,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (Ad-M):",round(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Pm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]-1500,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (Ad-F):",round(MASS[MASS$SPP=="Pm" & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"],1),"kg"))
text(x=grconvertX(x=0.02, from = "npc", to = "user"),
     y=MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"]+1500,pos=4,offset=0,cex=0.9,
     lab=paste("Med. Mass (SA-M):",round(MASS[MASS$SPP=="Pm" & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"],1),"kg"))
legend(x=grconvertX(0.77, from = "npc", to = "user"),
       y=grconvertY(y=0.25, from = "npc", to = "user"),pch=16,col=c("plum","plum4"),
       legend=c("Male","Female"),title="P. macroceph.",cex=0.9, pt.cex = 1.2)
close.screen(all = TRUE)
dev.off()


###### Analysis S2. Fit Phylogenetic Generalized Linear Mixed Effects Models of DurationMin at the per dive level using a Function of MASS, MB, and/or IDDI #####
#Introduce random individual, and species level phylogenetic effect to account for the lack of independence between individuals within a species

#Load required packages
library(ape)
library(MCMCglmm)

#re-format grouping variables as factors for model specification
BEHAV$Ptt=as.factor(BEHAV$Ptt)
BEHAV$SPP=as.factor(BEHAV$SPP)
BEHAV$FAM=as.factor(BEHAV$FAM)

#Prune PHYLO tree so that it contains only the study species from this study
PHYLO_TREE_temp=drop.tip(phy=PHYLO_TREE,tip=c("St_attenuata","Or_orca","Ph_phocoena","De_leucas",
                                              "Mo_monoceros","Hy_ampullatus","Ko_breviceps"))
PHYLO_TREE_temp$tip.label=c("Gm","Pe","Md","Zc","Pm")

#Invert the SPP matrix to calculate the Sigma matrix of phylogenetic correlation detailed in Villemereuil and Nakagawa 2014
INV_PHYLO_TREE_temp<-inverseA(PHYLO_TREE_temp,nodes="TIPS",scale=TRUE)

#compare a linear model and different random intercept structures
PRIOR<-list(R=list(V=1,nu=0.02))
PGLMM_DURATION_MASS_1A<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(IDDI)+log10(MB),
                                 family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                 data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50)

PRIOR<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DURATION_MASS_2A<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP,
                                 family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                 data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PRIOR<-list(G=list(G1=list(V=1,nu=0.02),G2=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DURATION_MASS_3A<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP+Ptt,
                                 family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                 data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_1A$DIC
PGLMM_DURATION_MASS_2A$DIC
PGLMM_DURATION_MASS_3A$DIC

#compare fixed effects structures given a random intercept structure
PRIOR<-list(G=list(G1=list(V=1,nu=0.02),G2=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DURATION_MASS_1<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_2<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(IDDI),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_3<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED)+log10(MB),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_4<-MCMCglmm(log10(DurationMin/60)~log10(IDDI)+log10(MB),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_5<-MCMCglmm(log10(DurationMin/60)~log10(MASS_MED),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_6<-MCMCglmm(log10(DurationMin/60)~log10(MB),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_7<-MCMCglmm(log10(DurationMin/60)~log10(IDDI),random=~SPP+Ptt,
                                family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                                data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DURATION_MASS_1$DIC
PGLMM_DURATION_MASS_2$DIC
PGLMM_DURATION_MASS_3$DIC
PGLMM_DURATION_MASS_4$DIC
PGLMM_DURATION_MASS_5$DIC
PGLMM_DURATION_MASS_6$DIC
PGLMM_DURATION_MASS_7$DIC


#change formatting of grouping variables back to charater vectors for simpler manipulation
BEHAV$Ptt=as.numeric(as.character(BEHAV$Ptt))
BEHAV$SPP=as.character(BEHAV$SPP)
BEHAV$FAM=as.character(BEHAV$FAM)

#Clean-up temp variables
remove(PHYLO_TREE_temp,INV_PHYLO_TREE_temp)

###### Analysis S3. Fit Phylogenetic Generalized Linear Mixed Effects Models of DepthMin at the per dive level using a Function of MASS, MB, and/or IDDI #####
#Introduce random individual, and species level phylogenetic effect to account for the lack of independence between individuals within a species

#Load required packages
library(ape)
library(MCMCglmm)

#re-format grouping variables as factors for model specification
BEHAV$Ptt=as.factor(BEHAV$Ptt)
BEHAV$SPP=as.factor(BEHAV$SPP)
BEHAV$FAM=as.factor(BEHAV$FAM)

#Prune PHYLO tree so that it contains only the study species from this study
PHYLO_TREE_temp=drop.tip(phy=PHYLO_TREE,tip=c("St_attenuata","Or_orca","Ph_phocoena","De_leucas",
                                              "Mo_monoceros","Hy_ampullatus","Ko_breviceps"))
PHYLO_TREE_temp$tip.label=c("Gm","Pe","Md","Zc","Pm")

#Invert the SPP matrix to calculate the Sigma matrix of phylogenetic correlation detailed in Villemereuil and Nakagawa 2014
INV_PHYLO_TREE_temp<-inverseA(PHYLO_TREE_temp,nodes="TIPS",scale=TRUE)

#compare a linear model and different random intercept structures
PRIOR<-list(R=list(V=1,nu=0.02))
PGLMM_DEPTH_MASS_1A<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(IDDI)+log10(MB),
                              family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                              data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50)

PRIOR<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DEPTH_MASS_2A<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP,
                              family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                              data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PRIOR<-list(G=list(G1=list(V=1,nu=0.02),G2=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DEPTH_MASS_3A<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP+Ptt,
                              family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                              data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_1A$DIC
PGLMM_DEPTH_MASS_2A$DIC
PGLMM_DEPTH_MASS_3A$DIC

#compare fixed effects structures given a random intercept structure
PRIOR<-list(G=list(G1=list(V=1,nu=0.02),G2=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
PGLMM_DEPTH_MASS_1<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(IDDI)+log10(MB),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_2<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(IDDI),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_3<-MCMCglmm(log10(DepthMin)~log10(MASS_MED)+log10(MB),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_4<-MCMCglmm(log10(DepthMin)~log10(IDDI)+log10(MB),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_5<-MCMCglmm(log10(DepthMin)~log10(MASS_MED),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_6<-MCMCglmm(log10(DepthMin)~log10(MB),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_7<-MCMCglmm(log10(DepthMin)~log10(IDDI),random=~SPP+Ptt,
                             family="gaussian",ginverse=list(SPP=INV_PHYLO_TREE_temp$Ainv),prior=PRIOR,
                             data=BEHAV[!is.na(BEHAV$IDDI),],nitt=100000,burnin=1000,thin=50,pr=TRUE)

PGLMM_DEPTH_MASS_1$DIC
PGLMM_DEPTH_MASS_2$DIC
PGLMM_DEPTH_MASS_3$DIC
PGLMM_DEPTH_MASS_4$DIC
PGLMM_DEPTH_MASS_5$DIC
PGLMM_DEPTH_MASS_6$DIC
PGLMM_DEPTH_MASS_7$DIC


#change formatting of grouping variables back to charater vectors for simpler manipulation
BEHAV$Ptt=as.numeric(as.character(BEHAV$Ptt))
BEHAV$SPP=as.character(BEHAV$SPP)
BEHAV$FAM=as.character(BEHAV$FAM)

#Clean-up temp variables
remove(PHYLO_TREE_temp,INV_PHYLO_TREE_temp)


###### Figure S2. PGLMM's of dive duration and depth as a function of body mass and myoglobin concentration#####
#Plot of median whale MASS relative to MAX_DURATION of dives 
pdf(paste("~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/",
          "PGLMM_DURATION_DEPTH_MASS",".pdf",sep=""),height=7,width=8.5)

par(oma=c(7,3,0,0),mar=c(3,3,2,3),mfrow=c(2,2),xpd=TRUE)

#Load required packages
library(ape)
library(MCMCglmm)

#Prune PHYLO tree so that it contains only the study species from this study
PHYLO_TREE_temp=drop.tip(phy=PHYLO_TREE,tip=c("St_attenuata","Or_orca","Ph_phocoena","De_leucas",
                                              "Mo_monoceros","Hy_ampullatus","Ko_breviceps"))
PHYLO_TREE_temp$tip.label=c("Globicephala_macrorhynchus","Peponocephala_electra",
                            "Mesoplodon_densirostris","Ziphius_cavirostris",
                            "Physeter_macrocephalus")
#Plot PHYLO_TREE used in PGLMM models 
plot(PHYLO_TREE_temp,root.edge = T, label.offset = 1, cex=1,
     tip.color = c("lightsalmon","lightpink","lightblue","lightgreen","plum"))
axisPhylo()
mtext(expression(paste("10"^"6"," Years BP")),side=1,line=2.5,at=15,cex=0.9)
remove(PHYLO_TREE_temp)

#Plot posterior histogram for SPP random effect
#Create a data.frame of PGLMM parameter posterior distributions
PGLMM_DURATION_MASS_1_temp=as.data.frame(PGLMM_DURATION_MASS_1$Sol)
HIST_HORIZ(PGLMM_DURATION_MASS_1_temp$SPP.Gm,BREAKS=seq(-1.5,1.5,0.1),YLIM=c(0,25),COL="lightsalmon",
           XLAB="Posterior(Rand.Eff.Spp.|Data)",YLAB="Frequency (%)")
HIST_HORIZ(PGLMM_DURATION_MASS_1_temp$SPP.Pe,BREAKS=seq(-1.5,1.5,0.1),YLIM=c(0,25),COL="lightpink",ADD = T,AXES=F)
HIST_HORIZ(PGLMM_DURATION_MASS_1_temp$SPP.Pm,BREAKS=seq(-1.5,1.5,0.1),YLIM=c(0,25),COL="plum",ADD = T,AXES=F)
HIST_HORIZ(PGLMM_DURATION_MASS_1_temp$SPP.Md,BREAKS=seq(-1.5,1.5,0.1),YLIM=c(0,25),COL="lightblue",ADD = T,AXES=F)
HIST_HORIZ(PGLMM_DURATION_MASS_1_temp$SPP.Zc,BREAKS=seq(-1.5,1.5,0.1),YLIM=c(0,25),COL="lightgreen",ADD = T,AXES=F)
lines(x=c(0,0),y=c(20,25))


#Plot of Model 5 (Mass & IDDI) fit overlaid on observed data
plot(1:10,1:10,type="n",xlim=c(100,14000),ylim=c(5,110),xlab=NA,ylab=NA)
points(BEHAV[BEHAV$SPP=="Pe" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="U","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pe" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="U","DurationMin"]/60,col="black",bg="lightpink",pch=25)
points(BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DurationMin"]/60,col="black",bg="lightsalmon",pch=24)
points(BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F or M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F or M","DurationMin"]/60,col="black",bg="lightsalmon4",pch=24)
points(BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DurationMin"]/60,col="black",bg="lightblue",pch=22)
points(BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F" & BEHAV$DurationMin<=100*60,"MASS_MED"],
       BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F" & BEHAV$DurationMin<=100*60,"DurationMin"]/60,col="black",bg="lightblue4",pch=22)
points(BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DurationMin"]/60,col="black",bg="lightgreen",pch=23)
points(BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","MASS_MED"],
       BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","DurationMin"]/60,col="black",bg="darkseagreen4",pch=23)
points(BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DurationMin"]/60,col="black",bg="plum",pch=21)
points(BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","DurationMin"]/60,col="black",bg="plum4",pch=21)

#Create a data.frame of with mean of PGLMM parameter posterior distributions
PGLMM_DURATION_MASS_1_temp=as.data.frame(t(colMeans(PGLMM_DURATION_MASS_1$Sol)))

#add a line representing the fixed effects component of the model 
lines(seq(10,13000,length=100),
      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(10,13000,length=100))
          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Pm"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])))
          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm","Zc","Md"),"IDDI"],na.rm=T))),
      lwd=2)

#add lines representing the specific random effects offsets of the intercepts at the FAM level (level 1)
lines(seq(5,1800,length=100),
      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1])))
          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(mean(BEHAV[BEHAV$SPP%in%c("Pe","Gm"),"IDDI"],na.rm=T))
          +mean(c(PGLMM_DURATION_MASS_1_temp$SPP.Pe,PGLMM_DURATION_MASS_1_temp$SPP.Gm))),lty=1)
#lines(seq(5,1800,length=100),
#      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
#          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
#          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1])
#          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pe"),"IDDI"],na.rm=T))
#          +PGLMM_DURATION_MASS_1_temp$SPP.Pe),lty=1)
#lines(seq(5,1800,length=100),
#      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
#          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
#          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1])
#          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Gm"),"IDDI"],na.rm=T))
#          +PGLMM_DURATION_MASS_1_temp$SPP.Gm),lty=1)
lines(seq(10500,13000,length=100),
      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(10500,13000,length=100))
          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pm"),"MB"][1])))
          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pm"),"IDDI"],na.rm=T))
          +PGLMM_DURATION_MASS_1_temp$SPP.Pm),lty=1)
lines(seq(400,2200,length=100),
      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1],
                                                               BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])))
          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T))
          +mean(c(PGLMM_DURATION_MASS_1_temp$SPP.Md,PGLMM_DURATION_MASS_1_temp$SPP.Zc))),lty=1)
#lines(seq(400,2200,length=100),
#      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
#          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
#          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1])
#          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Md"),"IDDI"],na.rm=T))
#          +PGLMM_DURATION_MASS_1_temp$SPP.Md),lty=1)
#lines(seq(400,2200,length=100),
#      10^(PGLMM_DURATION_MASS_1_temp$'(Intercept)'
#          +PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
#          +PGLMM_DURATION_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])
#          +PGLMM_DURATION_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Zc"),"IDDI"],na.rm=T))
#         +PGLMM_DURATION_MASS_1_temp$SPP.Zc),lty=1)


text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="PGLMM (Mass, Myoglobin, & IDDI):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(T) =",round(PGLMM_DURATION_MASS_1_temp$'log10(MASS_MED)',2),"log(m) +",round(PGLMM_DURATION_MASS_1_temp$'log10(MB)',2),"log([Mb])"))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("-",abs(round(PGLMM_DURATION_MASS_1_temp$'log10(IDDI)',2)),"log(IDDI) +",round(PGLMM_DURATION_MASS_1_temp$'(Intercept)',2)))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.75, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("R\U00B2 :",round(R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_1),3)))


#text(x=c(8000),y=c(33),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm"),"IDDI"],na.rm=T)/60,0),"min"),
#     pos=c(4),offset=c(0),srt=c(8),font=3,cex=0.8)
#text(x=c(1000),y=c(28),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T)/60,0),"min"),
#     pos=c(4),offset=c(0),srt=c(58),font=3,cex=0.8)

mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Dive Duration (min)")
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.1, from = "npc", to = "user"),lab="(a)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

#Plot of Model 5 (Mass & IDDI) fit overlaid on observed data
plot(1:10,1:10,type="n",xlim=c(100,14000),ylim=c(50,2000),xlab=NA,ylab=NA)
points(BEHAV[BEHAV$SPP=="Pe" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="U","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pe" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="U","DepthMin"],col="black",bg="lightpink",pch=25)
points(BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DepthMin"],col="black",bg="lightsalmon",pch=24)
points(BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F or M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Gm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F or M","DepthMin"],col="black",bg="lightsalmon4",pch=24)
points(BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DepthMin"],col="black",bg="lightblue",pch=22)
points(BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","MASS_MED"],
       BEHAV[BEHAV$SPP=="Md" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","DepthMin"],col="black",bg="lightblue4",pch=22)
points(BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DepthMin"],col="black",bg="lightgreen",pch=23)
points(BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","MASS_MED"],
       BEHAV[BEHAV$SPP=="Zc" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","DepthMin"],col="black",bg="darkseagreen4",pch=23)
points(BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="M","DepthMin"],col="black",bg="plum",pch=21)
points(BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","MASS_MED"],
       BEHAV[BEHAV$SPP=="Pm" & !is.na(BEHAV$IDDI) & BEHAV$Sex=="F","DepthMin"],col="black",bg="plum4",pch=21)

#Create a data.frame of with mean of PGLMM parameter posterior distributions
PGLMM_DEPTH_MASS_1_temp=as.data.frame(t(colMeans(PGLMM_DEPTH_MASS_1$Sol)))

#add a line representing the fixed effects component of the model 
lines(seq(10,13000,length=100),
      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(10,13000,length=100))
          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Pm"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])))
          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm","Zc","Md"),"IDDI"],na.rm=T))),
      lwd=2)

#add lines representing the specific random effects offsets of the intercepts at the FAM level (level 1)
lines(seq(5,1800,length=100),
      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1])))
          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(mean(BEHAV[BEHAV$SPP%in%c("Pe","Gm"),"IDDI"],na.rm=T))
          +mean(c(PGLMM_DEPTH_MASS_1_temp$SPP.Pe,PGLMM_DEPTH_MASS_1_temp$SPP.Gm))),lty=1)
#lines(seq(5,1800,length=100),
#      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Pe"),"MB"][1])
#          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pe"),"IDDI"],na.rm=T))
#          +PGLMM_DEPTH_MASS_1_temp$SPP.Pe),lty=1)
#lines(seq(5,1800,length=100),
#      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(5,1800,length=100))
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Gm"),"MB"][1])
#          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Gm"),"IDDI"],na.rm=T))
#          +PGLMM_DEPTH_MASS_1_temp$SPP.Gm),lty=1)
lines(seq(10500,13000,length=100),
      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(10500,13000,length=100))
          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Pm"),"MB"][1])))
          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Pm"),"IDDI"],na.rm=T))
          +PGLMM_DEPTH_MASS_1_temp$SPP.Pm),lty=1)
lines(seq(400,2200,length=100),
      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(mean(c(BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1],
                                                            BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])))
          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T))
          +mean(c(PGLMM_DEPTH_MASS_1_temp$SPP.Md,PGLMM_DEPTH_MASS_1_temp$SPP.Zc))),lty=1)
#lines(seq(400,2200,length=100),
#      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Md"),"MB"][1])
#          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Md"),"IDDI"],na.rm=T))
#          +PGLMM_DEPTH_MASS_1_temp$SPP.Md),lty=1)
#lines(seq(400,2200,length=100),
#      10^(PGLMM_DEPTH_MASS_1_temp$'(Intercept)'
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)'*log10(seq(400,2200,length=100))
#          +PGLMM_DEPTH_MASS_1_temp$'log10(MB)'*log10(BEHAV[BEHAV$SPP%in%c("Zc"),"MB"][1])
#          +PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)'*log10(median(BEHAV[BEHAV$SPP%in%c("Zc"),"IDDI"],na.rm=T))
#         +PGLMM_DEPTH_MASS_1_temp$SPP.Zc),lty=1)

text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.96, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,font=2,
     lab="PGLMM (Mass, Myoglobin, & IDDI):")
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.89, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("log(Z) =",round(PGLMM_DEPTH_MASS_1_temp$'log10(MASS_MED)',2),"log(m) +",round(PGLMM_DEPTH_MASS_1_temp$'log10(MB)',2),"log([Mb])"))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.82, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("+",round(PGLMM_DEPTH_MASS_1_temp$'log10(IDDI)',2),"log(IDDI) +",round(PGLMM_DEPTH_MASS_1_temp$'(Intercept)',2)))
text(x=grconvertX(0.98, from = "npc", to = "user"),
     y=grconvertY(y=0.75, from = "npc", to = "user"),pos=2,offset=0,cex=0.8,
     lab=paste("R\U00B2 :",round(R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_1),3)))

#text(x=c(8000),y=c(33),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Pe","Gm","Pm"),"IDDI"],na.rm=T)/60,0),"min"),
#     pos=c(4),offset=c(0),srt=c(8),font=3,cex=0.8)
#text(x=c(1000),y=c(28),lab=paste("med(IDDI) =",round(median(BEHAV[BEHAV$SPP%in%c("Zc","Md"),"IDDI"],na.rm=T)/60,0),"min"),
#     pos=c(4),offset=c(0),srt=c(58),font=3,cex=0.8)

mtext(side=1,line=2.5,cex=0.9, text="Body Mass (kg)")
mtext(side=2,line=2.5,cex=0.9, text="Dive Depth (m)")
text(x=grconvertX(x=-0.10, from = "npc", to = "user"),
     y=grconvertY(y=1.1, from = "npc", to = "user"),lab="(b)",pos=4,offset=0,cex=1.2, font=3,xpd=T)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", ncol=5, x.intersp=0.5, inset=0.02, cex=1,title="Legend",title.adj=0.01,
       legend = c("P. electra (M or U)","P. electra  (F)","G. macrorhynchus (M)","G. macrorhynchus (F)",
                  "M. densirostris (M)","M. densirostris (F)","Z. cavirostris (M)","Z. cavirostris (F)",
                  "P. macrocephalus (M)","P. macrocephalus (F)"), 
       pt.bg=c("lightpink","lightpink4","lightsalmon","lightsalmon4",
               "lightblue","lightblue4","lightgreen","darkseagreen4","plum","plum4"), pch=c(25,25,24,24,22,22,23,23,21,21))

dev.off()


##### TABLES ####

#####   Table 1. Summary of Argos telemetry tag deployments #####

DIVE_TABLE_1=data.frame(SPP=c("Pe","Gm","Md","Zc","Pm"),
                        N_SPOT=NA,N_SPLASH=NA,N_EVENT=NA,
                        SUM_BEHAV_DUR=NA,SUM_SERIES_DUR=NA,SUM_TAT_DUR=NA,
                        MEAN_DURATION=NA)
for(i in c("Pe","Gm","Md","Zc","Pm"))
{DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"N_SPOT"]=paste(nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPOT" & 
                                                              TAGS$Sex=="M" & TAGS$ARGOS>0,]),
                                                  nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPOT" & 
                                                              TAGS$Sex=="F" & TAGS$ARGOS>0,]),
                                                  nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPOT" & 
                                                              TAGS$Sex%in%c("U","U or AF","F or U","F or M") & 
                                                              TAGS$ARGOS>0,]),sep="      ")
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"N_SPLASH"]=paste(nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPLASH" & 
                                                               TAGS$Sex=="M" & TAGS$ARGOS>0,]),
                                                   nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPLASH" & 
                                                               TAGS$Sex=="F" & TAGS$ARGOS>0,]),
                                                   nrow(TAGS[TAGS$SPP==i & TAGS$TYPE=="SPLASH" & 
                                                               TAGS$Sex%in%c("U","U or AF","F or U","F or M") & 
                                                               TAGS$ARGOS>0,]),sep="      ")
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"N_EVENT"]=nrow(TAGS[TAGS$SPP==i & !duplicated(TAGS$EVENT) & TAGS$ARGOS>0,])
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"SUM_BEHAV_DUR"]=sum(BEHAV[BEHAV$SPP==i & BEHAV$What=="Message","DurationMin"])/(60*60)
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"SUM_SERIES_DUR"]=nrow(SERIES[SERIES$SPP==i,])*2.5/60
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"SUM_TAT_DUR"]=sum(TAT[TAT$SPP==i,"DURATION"])
DIVE_TABLE_1[DIVE_TABLE_1$SPP==i,"MEAN_DURATION"]=paste(round(mean(TAGS[TAGS$SPP==i & TAGS$ARGOS>0,"DURATION"]),2)," (",
                                                        round(min(TAGS[TAGS$SPP==i & TAGS$ARGOS>0,"DURATION"]),2),",",
                                                        round(max(TAGS[TAGS$SPP==i & TAGS$ARGOS>0,"DURATION"]),2),")",sep="")}

DIVE_TABLE_1$SUM_BEHAV_DUR=round(DIVE_TABLE_1$SUM_BEHAV_DUR,2)
DIVE_TABLE_1$SUM_SERIES_DUR=round(DIVE_TABLE_1$SUM_SERIES_DUR,2)
DIVE_TABLE_1$SUM_TAT_DUR=round(DIVE_TABLE_1$SUM_TAT_DUR,2)

write.csv(DIVE_TABLE_1,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_1.csv")

#####   Table 2. Summary of PGLMM maximum dive duration (Tmax) model selection process #####
DIVE_TABLE_2=data.frame(Formula=c(paste("Model 1a:",as.character(PGLMM_DURATION_MAX_MASS_1A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_1A$Fixed$formula[3])),
                                  paste("Model 2a:",as.character(PGLMM_DURATION_MAX_MASS_2A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_2A$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MAX_MASS_2A$Random$formula[2])),
                                  paste("Model 1b:",as.character(PGLMM_DURATION_MAX_MASS_1$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_1$Fixed$formula[3])),
                                  paste("Model 2b:",as.character(PGLMM_DURATION_MAX_MASS_2$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_2$Fixed$formula[3])),
                                  paste("Model 3b:",as.character(PGLMM_DURATION_MAX_MASS_3$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_3$Fixed$formula[3])),
                                  paste("Model 4b:",as.character(PGLMM_DURATION_MAX_MASS_4$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_4$Fixed$formula[3])),
                                  paste("Model 5b:",as.character(PGLMM_DURATION_MAX_MASS_5$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_5$Fixed$formula[3])),
                                  paste("Model 6b:",as.character(PGLMM_DURATION_MAX_MASS_6$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_6$Fixed$formula[3])),
                                  paste("Model 7b:",as.character(PGLMM_DURATION_MAX_MASS_7$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MAX_MASS_7$Fixed$formula[3]))),
                        k=c(PGLMM_DURATION_MAX_MASS_1A$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_1A$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_2A$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_2A$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_1$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_1$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_2$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_2$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_3$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_3$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_4$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_4$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_5$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_5$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_6$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_6$Random$nfl),
                            PGLMM_DURATION_MAX_MASS_7$Fixed$nfl+length(PGLMM_DURATION_MAX_MASS_7$Random$nfl)),
                        R_squared_m=round(c(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_1A),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_2A),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_1),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_2),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_3),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_4),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_5),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_6),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_7)),2),
                        R_squared_c=round(c(R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_1A),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_2A),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_1),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_2),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_3),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_4),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_5),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_6),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MAX_MASS_7)),2),
                        DIC=round(c(PGLMM_DURATION_MAX_MASS_1A$DIC,PGLMM_DURATION_MAX_MASS_2A$DIC,
                                    PGLMM_DURATION_MAX_MASS_1$DIC,PGLMM_DURATION_MAX_MASS_2$DIC,
                                    PGLMM_DURATION_MAX_MASS_3$DIC,PGLMM_DURATION_MAX_MASS_4$DIC,
                                    PGLMM_DURATION_MAX_MASS_5$DIC,PGLMM_DURATION_MAX_MASS_6$DIC,
                                    PGLMM_DURATION_MAX_MASS_7$DIC),2))
DIVE_TABLE_2[1:2,"d_DIC"]=DIVE_TABLE_2[1:2,"DIC"]-min(DIVE_TABLE_2[1:2,"DIC"])
DIVE_TABLE_2[1:2,"w_DIC"]=exp(-0.5*DIVE_TABLE_2[1:2,"d_DIC"])/sum(exp(-0.5*DIVE_TABLE_2[1:2,"d_DIC"]))
DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"d_DIC"]=DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"DIC"]-
  min(DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"DIC"])
DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"w_DIC"]=exp(-0.5*DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"d_DIC"])/
  sum(exp(-0.5*DIVE_TABLE_2[3:nrow(DIVE_TABLE_2),"d_DIC"]))
DIVE_TABLE_2$d_DIC=round(DIVE_TABLE_2$d_DIC,2)
DIVE_TABLE_2$w_DIC=round(DIVE_TABLE_2$w_DIC,2)

write.csv(DIVE_TABLE_2,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_2.csv")

#####   Table 3. Summary of PGLS maximum dive duration (Tmax) model selection process #####
DIVE_TABLE_3=data.frame(Formula=c(paste("Model 1:",as.character(PGLS_DURATION_MAX_1$call[2])),
                                  paste("Model 2:",as.character(PGLS_DURATION_MAX_2$call[2])),
                                  paste("Model 3:",as.character(PGLS_DURATION_MAX_3$call[2])),
                                  paste("Model 4:",as.character(PGLS_DURATION_MAX_4$call[2])),
                                  paste("Model 5:",as.character(PGLS_DURATION_MAX_5$call[2])),
                                  paste("Model 6:",as.character(PGLS_DURATION_MAX_6$call[2])),
                                  paste("Model 7:",as.character(PGLS_DURATION_MAX_7$call[2]))),
                        k=c(length(PGLS_DURATION_MAX_1$coefficients),
                            length(PGLS_DURATION_MAX_2$coefficients),
                            length(PGLS_DURATION_MAX_3$coefficients),
                            length(PGLS_DURATION_MAX_4$coefficients),
                            length(PGLS_DURATION_MAX_5$coefficients),
                            length(PGLS_DURATION_MAX_6$coefficients),
                            length(PGLS_DURATION_MAX_7$coefficients)),
                        AIC=round(c(AIC(PGLS_DURATION_MAX_1),AIC(PGLS_DURATION_MAX_2),
                                    AIC(PGLS_DURATION_MAX_3),AIC(PGLS_DURATION_MAX_4),
                                    AIC(PGLS_DURATION_MAX_5),AIC(PGLS_DURATION_MAX_6),
                                    AIC(PGLS_DURATION_MAX_7)),2))
DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"d_AIC"]=DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"AIC"]-
  min(DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"AIC"])
DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"w_AIC"]=exp(-0.5*DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"d_AIC"])/
  sum(exp(-0.5*DIVE_TABLE_3[1:nrow(DIVE_TABLE_3),"d_AIC"]))
DIVE_TABLE_3$d_AIC=round(DIVE_TABLE_3$d_AIC,2)
DIVE_TABLE_3$w_AIC=round(DIVE_TABLE_3$w_AIC,2)

write.csv(DIVE_TABLE_3,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_3.csv")

#####   Table S1. Summary of mass and standard-length measurements and estimates #####
DIVE_TABLE_S1=data.frame(SPP=rep(c("Pe","Gm","Md","Me","Zc","Pm"),each=4),
                        SEX=rep(rep(c("M","F"),each=2),6),N_MASS=NA,N_LENGTH=NA,
                        AGE=rep(c("A","SA"),12),THRESH=NA,MED_LENGTH=NA,MED_MASS=NA,
                        MB=rep(c(10^0.511*10^(0.220 * MICERTA[MICERTA$SPP_NAME=="Peponocephala electra  ","Z_MAX"]),
                                 68.2,69.2,74.1,43.2,70.0),each=4),
                        MB_SOURCE=rep(c(1,2,2,2,3,4),each=4),
                        stringsAsFactors = F)
# MB_SOURCE: 1= Micerta et al. 2013, 2= Velten et al. 2013, 3= Noren and Williams 2000, 4= Sharp and Marsh 1953

for(i in 1:24)
{DIVE_TABLE_S1[i,"N_MASS"]=nrow(MORPH[MORPH$SPP==DIVE_TABLE_S1[i,"SPP"] & MORPH$SEX==DIVE_TABLE_S1[i,"SEX"] & 
                                       !is.na(MORPH$MASS),])
DIVE_TABLE_S1[i,"N_LENGTH"]=nrow(MORPH[MORPH$SPP==DIVE_TABLE_S1[i,"SPP"] & MORPH$SEX==DIVE_TABLE_S1[i,"SEX"] & 
                                        !is.na(MORPH$LENGTH),])
DIVE_TABLE_S1[i,"THRESH"]=paste(MASS[MASS$SPP==DIVE_TABLE_S1[i,"SPP"] & 
                                      MASS$AGE==DIVE_TABLE_S1[i,"AGE"] &
                                      MASS$SEX==DIVE_TABLE_S1[i,"SEX"],"THRESH_LOW"],"-",
                               MASS[MASS$SPP==DIVE_TABLE_S1[i,"SPP"] & 
                                      MASS$AGE==DIVE_TABLE_S1[i,"AGE"] &
                                      MASS$SEX==DIVE_TABLE_S1[i,"SEX"],"THRESH_HIGH"],"m",sep="")
DIVE_TABLE_S1[i,"MED_LENGTH"]=MASS[MASS$SPP==DIVE_TABLE_S1[i,"SPP"] & 
                                    MASS$AGE==DIVE_TABLE_S1[i,"AGE"] &
                                    MASS$SEX==DIVE_TABLE_S1[i,"SEX"],"LENGTH_MED"]
DIVE_TABLE_S1[i,"MED_MASS"]=MASS[MASS$SPP==DIVE_TABLE_S1[i,"SPP"] & 
                                  MASS$AGE==DIVE_TABLE_S1[i,"AGE"] &
                                  MASS$SEX==DIVE_TABLE_S1[i,"SEX"],"MASS_MED"]}

DIVE_TABLE_S1$MED_MASS=round(DIVE_TABLE_S1$MED_MASS,2)

write.csv(DIVE_TABLE_S1,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_S1.csv")

#####   Table S2. Summary of PGLMM dive duration (T) model selection process #####
DIVE_TABLE_S2=data.frame(Formula=c(paste("Model 1a:",as.character(PGLMM_DURATION_MASS_1A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_1A$Fixed$formula[3])),
                                  paste("Model 2a:",as.character(PGLMM_DURATION_MASS_2A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_2A$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_2A$Random$formula[2])),
                                  paste("Model 3a:",as.character(PGLMM_DURATION_MASS_3A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_3A$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_3A$Random$formula[2])),
                                  paste("Model 1b:",as.character(PGLMM_DURATION_MASS_1$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_1$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_1$Random$formula[2])),
                                  paste("Model 2b:",as.character(PGLMM_DURATION_MASS_2$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_2$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_2$Random$formula[2])),
                                  paste("Model 3b:",as.character(PGLMM_DURATION_MASS_3$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_3$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_3$Random$formula[2])),
                                  paste("Model 4b:",as.character(PGLMM_DURATION_MASS_4$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_4$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_4$Random$formula[2])),
                                  paste("Model 5b:",as.character(PGLMM_DURATION_MASS_5$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_5$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_5$Random$formula[2])),
                                  paste("Model 6b:",as.character(PGLMM_DURATION_MASS_6$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_6$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_6$Random$formula[2])),
                                  paste("Model 7b:",as.character(PGLMM_DURATION_MASS_7$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DURATION_MASS_7$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DURATION_MASS_7$Random$formula[2]))),
                        k=c(PGLMM_DURATION_MASS_1A$Fixed$nfl+length(PGLMM_DURATION_MASS_1A$Random$nfl),
                            PGLMM_DURATION_MASS_2A$Fixed$nfl+length(PGLMM_DURATION_MASS_2A$Random$nfl),
                            PGLMM_DURATION_MASS_3A$Fixed$nfl+length(PGLMM_DURATION_MASS_3A$Random$nfl),
                            PGLMM_DURATION_MASS_1$Fixed$nfl+length(PGLMM_DURATION_MASS_1$Random$nfl),
                            PGLMM_DURATION_MASS_2$Fixed$nfl+length(PGLMM_DURATION_MASS_2$Random$nfl),
                            PGLMM_DURATION_MASS_3$Fixed$nfl+length(PGLMM_DURATION_MASS_3$Random$nfl),
                            PGLMM_DURATION_MASS_4$Fixed$nfl+length(PGLMM_DURATION_MASS_4$Random$nfl),
                            PGLMM_DURATION_MASS_5$Fixed$nfl+length(PGLMM_DURATION_MASS_5$Random$nfl),
                            PGLMM_DURATION_MASS_6$Fixed$nfl+length(PGLMM_DURATION_MASS_6$Random$nfl),
                            PGLMM_DURATION_MASS_7$Fixed$nfl+length(PGLMM_DURATION_MASS_7$Random$nfl)),
                        R_squared_m=round(c(R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_1A),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_2A),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_3A),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_1),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_2),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_3),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_4),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_5),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_6),
                                            R_SQUARED_M(MODEL=PGLMM_DURATION_MASS_7)),2),
                        R_squared_c=round(c(R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_1A),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_2A),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_3A),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_1),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_2),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_3),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_4),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_5),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_6),
                                            R_SQUARED_C(MODEL=PGLMM_DURATION_MASS_7)),2),
                        DIC=round(c(PGLMM_DURATION_MASS_1A$DIC,PGLMM_DURATION_MASS_2A$DIC,
                                    PGLMM_DURATION_MASS_3A$DIC,PGLMM_DURATION_MASS_1$DIC,
                                    PGLMM_DURATION_MASS_2$DIC,PGLMM_DURATION_MASS_3$DIC,
                                    PGLMM_DURATION_MASS_4$DIC,PGLMM_DURATION_MASS_5$DIC,
                                    PGLMM_DURATION_MASS_6$DIC,PGLMM_DURATION_MASS_7$DIC),2))
DIVE_TABLE_S2[1:3,"d_DIC"]=DIVE_TABLE_S2[1:3,"DIC"]-min(DIVE_TABLE_S2[1:3,"DIC"])
DIVE_TABLE_S2[1:3,"w_DIC"]=exp(-0.5*DIVE_TABLE_S2[1:3,"d_DIC"])/sum(exp(-0.5*DIVE_TABLE_S2[1:3,"d_DIC"]))
DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"d_DIC"]=DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"DIC"]-
  min(DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"DIC"])
DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"w_DIC"]=exp(-0.5*DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"d_DIC"])/
  sum(exp(-0.5*DIVE_TABLE_S2[4:nrow(DIVE_TABLE_S2),"d_DIC"]))
DIVE_TABLE_S2$d_DIC=round(DIVE_TABLE_S2$d_DIC,2)
DIVE_TABLE_S2$w_DIC=round(DIVE_TABLE_S2$w_DIC,2)

write.csv(DIVE_TABLE_S2,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_S2.csv")


#####   Table S3. Summary of PGLMM dive depth (Z) model selection process #####
DIVE_TABLE_S3=data.frame(Formula=c(paste("Model 1a:",as.character(PGLMM_DEPTH_MASS_1A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_1A$Fixed$formula[3])),
                                  paste("Model 2a:",as.character(PGLMM_DEPTH_MASS_2A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_2A$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_2A$Random$formula[2])),
                                  paste("Model 3a:",as.character(PGLMM_DEPTH_MASS_3A$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_3A$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_3A$Random$formula[2])),
                                  paste("Model 1b:",as.character(PGLMM_DEPTH_MASS_1$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_1$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_1$Random$formula[2])),
                                  paste("Model 2b:",as.character(PGLMM_DEPTH_MASS_2$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_2$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_2$Random$formula[2])),
                                  paste("Model 3b:",as.character(PGLMM_DEPTH_MASS_3$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_3$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_3$Random$formula[2])),
                                  paste("Model 4b:",as.character(PGLMM_DEPTH_MASS_4$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_4$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_4$Random$formula[2])),
                                  paste("Model 5b:",as.character(PGLMM_DEPTH_MASS_5$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_5$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_5$Random$formula[2])),
                                  paste("Model 6b:",as.character(PGLMM_DEPTH_MASS_6$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_6$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_6$Random$formula[2])),
                                  paste("Model 7b:",as.character(PGLMM_DEPTH_MASS_7$Fixed$formula[2]),"~",
                                        as.character(PGLMM_DEPTH_MASS_7$Fixed$formula[3]),"+",
                                        as.character(PGLMM_DEPTH_MASS_7$Random$formula[2]))),
                        k=c(PGLMM_DEPTH_MASS_1A$Fixed$nfl+length(PGLMM_DEPTH_MASS_1A$Random$nfl),
                            PGLMM_DEPTH_MASS_2A$Fixed$nfl+length(PGLMM_DEPTH_MASS_2A$Random$nfl),
                            PGLMM_DEPTH_MASS_3A$Fixed$nfl+length(PGLMM_DEPTH_MASS_3A$Random$nfl),
                            PGLMM_DEPTH_MASS_1$Fixed$nfl+length(PGLMM_DEPTH_MASS_1$Random$nfl),
                            PGLMM_DEPTH_MASS_2$Fixed$nfl+length(PGLMM_DEPTH_MASS_2$Random$nfl),
                            PGLMM_DEPTH_MASS_3$Fixed$nfl+length(PGLMM_DEPTH_MASS_3$Random$nfl),
                            PGLMM_DEPTH_MASS_4$Fixed$nfl+length(PGLMM_DEPTH_MASS_4$Random$nfl),
                            PGLMM_DEPTH_MASS_5$Fixed$nfl+length(PGLMM_DEPTH_MASS_5$Random$nfl),
                            PGLMM_DEPTH_MASS_6$Fixed$nfl+length(PGLMM_DEPTH_MASS_6$Random$nfl),
                            PGLMM_DEPTH_MASS_7$Fixed$nfl+length(PGLMM_DEPTH_MASS_7$Random$nfl)),
                        R_squared_m=round(c(R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_1A),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_2A),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_3A),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_1),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_2),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_3),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_4),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_5),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_6),
                                            R_SQUARED_M(MODEL=PGLMM_DEPTH_MASS_7)),2),
                        R_squared_c=round(c(R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_1A),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_2A),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_3A),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_1),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_2),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_3),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_4),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_5),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_6),
                                            R_SQUARED_C(MODEL=PGLMM_DEPTH_MASS_7)),2),
                        DIC=round(c(PGLMM_DEPTH_MASS_1A$DIC,PGLMM_DEPTH_MASS_2A$DIC,
                                    PGLMM_DEPTH_MASS_3A$DIC,PGLMM_DEPTH_MASS_1$DIC,
                                    PGLMM_DEPTH_MASS_2$DIC,PGLMM_DEPTH_MASS_3$DIC,
                                    PGLMM_DEPTH_MASS_4$DIC,PGLMM_DEPTH_MASS_5$DIC,
                                    PGLMM_DEPTH_MASS_6$DIC,PGLMM_DEPTH_MASS_7$DIC),2))
DIVE_TABLE_S3[1:3,"d_DIC"]=DIVE_TABLE_S3[1:3,"DIC"]-min(DIVE_TABLE_S3[1:3,"DIC"])
DIVE_TABLE_S3[1:3,"w_DIC"]=exp(-0.5*DIVE_TABLE_S3[1:3,"d_DIC"])/sum(exp(-0.5*DIVE_TABLE_S3[1:3,"d_DIC"]))
DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"d_DIC"]=DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"DIC"]-
  min(DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"DIC"])
DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"w_DIC"]=exp(-0.5*DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"d_DIC"])/
  sum(exp(-0.5*DIVE_TABLE_S3[4:nrow(DIVE_TABLE_S3),"d_DIC"]))
DIVE_TABLE_S3$d_DIC=round(DIVE_TABLE_S3$d_DIC,2)
DIVE_TABLE_S3$w_DIC=round(DIVE_TABLE_S3$w_DIC,2)

write.csv(DIVE_TABLE_S3,"~/Grad School/Research/3_2012_Whale Tagging/Plots and Figures/Dive_Manuscript_Final Figures/DIVE_TABLE_S3.csv")



##### TEXT VALUES ####

#### ABSTRACT ####

# Abstract: ...generalized linear models (GLM) with terms accounting only for 
# body mass and muscular myoglobin ... (R2 = ___)
round(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_3),2)


# Abstract: ...two beaked whale species, which were exceptional in both the 
# duration and depth of foraging dives (M. densirostris mean ___ min & ____ m; 
# Z. cavirostris mean ____ min & ____ m)
mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T)/60 
#46.17838
mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#1128.843

mean(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T)/60 
#65.35938
mean(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#1179.762


# Abstract: The inclusion IDDI as a covariate in allometric GLM.... 
# ...substantially improved goodness-of-fit metrics for both model types (GLM R2=__). 
round(R_SQUARED_M(MODEL=PGLMM_DURATION_MAX_MASS_1),2)

#### RESULTS ####

#### Tagging and Biopsy P1 ####

# Tagging and Biopsy P1: ...a total of _ tags were successfully deployed...
nrow(TAGS[TAGS$ARGOS>0,]) 
#75

# Tagging and Biopsy P1:...yielding in total ___ position fixes and ___h of dive data...
nrow(ARGOS) 
#7752

sum(BEHAV[BEHAV$What=="Message","DurationMin"])/(60*60) + 
  nrow(SERIES)*2.5/60 + sum(TAT$DURATION) 
#12204

# Tagging and Biopsy P1:...SPLASH tags were deployed in a higher 
# proportion of tagging attempts on the beaked whale species (__ and __)...
# while the tagging of the delphinids (__ and __) and sperm whales (__) emphasized SPOT...
nrow(TAGS[TAGS$SPP=="Md" & TAGS$TYPE=="SPLASH" & TAGS$ARGOS>0,])*100/
  nrow(TAGS[TAGS$SPP=="Md" & TAGS$ARGOS>0,]) 
# 75

nrow(TAGS[TAGS$SPP=="Zc" & TAGS$TYPE=="SPLASH" & TAGS$ARGOS>0,])*100/
  nrow(TAGS[TAGS$SPP=="Zc" & TAGS$ARGOS>0,]) 
# 85.71429

# Tagging and Biopsy P1:Sex ratios in tagged individuals where sex was known .... ranged from __ to __.
nrow(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="M" & TAGS$ARGOS>0,])
nrow(TAGS[TAGS$SPP=="Zc" & TAGS$Sex=="F" & TAGS$ARGOS>0,])

nrow(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M" & TAGS$ARGOS>0,])
nrow(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="F" & TAGS$ARGOS>0,])

nrow(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="M" & TAGS$ARGOS>0,]) 
#6
nrow(TAGS[TAGS$SPP=="Md" & TAGS$Sex=="F" & TAGS$ARGOS>0,]) 
#6

nrow(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="M" & TAGS$ARGOS>0,]) 
#9
nrow(TAGS[TAGS$SPP=="Gm" & TAGS$Sex=="F" & TAGS$ARGOS>0,]) 
#4

# Tagging and Biopsy P1:...average transmission durations ranging from __ to __ 
mean(TAGS[TAGS$SPP=="Pm" & TAGS$ARGOS>0,"DURATION"]) 
# 8.204424
mean(TAGS[TAGS$SPP=="Zc" & TAGS$ARGOS>0,"DURATION"]) 
# 23.71825

#### Tagging and Biopsy P2 ####

# Tagging and Biopsy P2:...biopsy sampling program (199_ -2014) ... 
# extending to more than ___ individuals representing 6 of 8 species

#### Tagging and Biopsy P3 ####

# Tagging and Biopsy P3:...more than __ individuals were tagged and biopsied simultaneously
# ... further __ individuals that were tagged and biopsied on different occasions ...
nrow(BIOPSY) 
#24 
39-nrow(BIOPSY) 
#15

# Tagging and Biopsy P3:...allowed the direct identification ... in _% and _% of the tagging dataset
39*100/nrow(TAGS[TAGS$ARGOS>0,]) 
#52

#### Morphometrics P1 ####

# Morphometrics P1:...total __ measurements of mass and standard length, 
# and __ measurements of standard length only
nrow(MORPH[!is.na(MORPH$MASS) & !is.na(MORPH$LENGTH) & !MORPH$SPP%in%"Me",]) 
#175

nrow(MORPH[is.na(MORPH$MASS) & !is.na(MORPH$LENGTH) & !MORPH$SPP%in%"Me",]) 
#1912

# Morphometrics P1:...Median masses from the smallest species ranged ... from __kg to ___kg, 
median(MASS[MASS$SPP=="Pe" & MASS$AGE=="A","MASS_MED"])
#169.638

median(c(MASS[MASS$SPP=="Pm"  & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"],
         MASS[MASS$SPP=="Pm"  & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]))
#11774.47

# Morphometrics P1:...Adult P. macrocephalus typically exhibit extreme sexual size 
# dimorphism (male : female body mass ratio of ___)

median(MASS[MASS$SPP=="Pm"  & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"])/
  median(MASS[MASS$SPP=="Pm"  & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"])
#2.295514

#### Dive Distribution P1 ####

# Dive Distribution P1:...distributed from... P. electra (___±__m) and G. macrorhynchus (___±__m) 
# P. macrocephalus (___±__m) ... M. densirostris (___±__m) and Z. cavirostris (___±__m)
mean(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#310.0714
sd(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#100.588

mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#203.711
sd(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#220.7085

mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#899.8667
sd(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#104.6961

mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#1128.843
sd(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#229.6171

mean(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#1179.762
sd(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm = T) 
#225.2855

#Response to Rus Andrews comment
summary(lm(DepthMin~DurationMin,data=BEHAV[BEHAV$DepthMin>0 & BEHAV$SPP=="Md" & BEHAV$What=="Dive" & BEHAV$FORAGING=="Y",]))
summary(lm(DepthMin~DurationMin,data=BEHAV[BEHAV$DepthMin>0 & BEHAV$SPP=="Zc" & BEHAV$What=="Dive" & BEHAV$FORAGING=="Y",]))

#### Models of Maximum Dive Duration P1 ####

# Models of Maximum Dive Duration P1:...power law model from Noren and Williams (2000)... explained less of the variance in dive duration than an overall mean (R2 < 0,Table 4) 
TAGS_temp=TAGS[!is.na(TAGS$DURATION_MAX),]


1-sum((log10(TAGS_temp$DURATION_MAX/60)-
       (0.47*log10(TAGS_temp$MASS_MED)+log10(0.68)))^2)/
  sum((log10(TAGS_temp$DURATION_MAX/60)-
         mean(log10(TAGS_temp$DURATION_MAX/60)))^2)   

#-0.7019216
remove(TAGS_temp)

#deprecated: calculations of R2 for models in linear space
#1-(sum((log10(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"])-
#          0.68*(TAGS[!is.na(TAGS$DURATION_MAX),"MASS_MED"]^0.47))^2/
#         length(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]))/
#     sum((TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]-
#            mean(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]))^2/
#           length(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"])))
#-1.91873e+16

# Models of Maximum Dive Duration P1:...Noren and Williams model however did performed considerably better  (R2=0._, Fig. 3a) 
TAGS_temp=TAGS[!is.na(TAGS$DURATION_MAX) & TAGS$SPP%in%c("Pe","Gm","Pm"),]

1-(sum((log10(TAGS_temp$DURATION_MAX/60)-
          (0.47*log10(TAGS_temp$MASS_MED)+log10(0.68)))^2/
         nrow(TAGS_temp))/
     sum((log10(TAGS_temp$DURATION_MAX/60)-
            mean(log10(TAGS_temp$DURATION_MAX/60)))^2/
           nrow(TAGS_temp)))
#0.902621
remove(TAGS_temp)

# Models of Maximum Dive Duration  P1:...max. dive duration of M. densirostris ... exceed the max. dive durations of G. macrorhynchus males by a factor __x, 
mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T)/
  mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$Sex=="M" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T) 
#5.467986

# Models of Maximum Dive Duration  P1: ....despite their similar median masses (M.d.: ___ kg, G.m.: ___ kg)
100*(median(MASS[MASS$SPP=="Gm" & MASS$AGE=="A" & MASS$SEX=="M","MASS_MED"])-
  median(MASS[MASS$SPP=="Md" & MASS$AGE=="A","MASS_MED"]))/
  median(MASS[MASS$SPP=="Gm" & MASS$AGE=="A" & MASS$SEX=="M","MASS_MED"])

median(MASS[MASS$SPP=="Md" & MASS$AGE=="A","MASS_MED"]) 
#842.9224
median(MASS[MASS$SPP=="Gm" & MASS$AGE=="A" & MASS$SEX=="M","MASS_MED"]) 
#1195.421

# Models of Maximum Dive Duration  P1:...despite the weighing ___x less, 
# Z. cavirostris ... P. macrocephalus
median(c(MASS[MASS$SPP=="Pm"  & MASS$SEX=="M" & MASS$AGE=="SA","MASS_MED"],
         MASS[MASS$SPP=="Pm"  & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]))/
  median(MASS[MASS$SPP=="Zc" & MASS$AGE=="A","MASS_MED"]) 
#7.563733

# Models of Maximum Dive Duration  P1:...Z. cavirostris dove on average __ min longer than P. macrocephalus
mean(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T)/60-
  mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y","DurationMin"],na.rm = T)/60 
#17.63799

# Models of Maximum Dive Duration P1:...overall allometric model (Model 1a, Table 4) 
#of logged maximum dive duration as a function of logged estimated body mass 
#(i.e., a power law model) explained only __% of the variance in dive duration.

summary(LM_DURATION_MAX_MASS_1)$adj.r.squared *100
#32.59028

#deprecated: calculations of R^2 for models in linear space
#1-(sum((TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]-
#          (10^predict(LM_DURATION_MAX_MASS_1,TAGS[!is.na(TAGS$DURATION_MAX),]))*60)^2/
#         length(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]))/
#     sum((TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]-
#            mean(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"]))^2/
#           length(TAGS[!is.na(TAGS$DURATION_MAX),"DURATION_MAX"])))
#-0.137311


# Models of Maximum Dive Duration  P1:...myoglobin concentrations,(Model 2a, Table 4) 
# explained only __% of the variance in dive durations
summary(LM_DURATION_MAX_MASS_2)$adj.r.squared*100
#26.01019

# Models of Maximum Dive Duration  P1:...myoglobin concentrations...in combination 
# with body mass (Model 4a, Table 4) explained only __%. 
summary(LM_DURATION_MAX_MASS_4)$adj.r.squared*100
#37.64189

#### Models of Maximum Dive Duration P2 ####

# Models of Maximum Dive Duration  P2:...[beaked whales] recovery periods were also long in 
# proportion to dive durations (Fig. 6b), with median ratio of __ and __ times 
# the duration of the preceding dive spent recovering
median(BEHAV[BEHAV$SPP=="Md","IDDI"]/BEHAV[BEHAV$SPP=="Md","DurationMin"],na.rm=T)
#1.409106
median(BEHAV[BEHAV$SPP=="Zc","IDDI"]/BEHAV[BEHAV$SPP=="Zc","DurationMin"],na.rm=T)
#1.011126

# Models of Maximum Dive Duration  P2:...compared to ratios of __, __, and __ in the 
#delphinids and sperm whales
median(BEHAV[BEHAV$SPP=="Pe","IDDI"]/BEHAV[BEHAV$SPP=="Pe","DurationMin"],na.rm=T)
#0.3852786
median(BEHAV[BEHAV$SPP=="Gm","IDDI"]/BEHAV[BEHAV$SPP=="Gm","DurationMin"],na.rm=T)
#0.3756098
median(BEHAV[BEHAV$SPP=="Pm","IDDI"]/BEHAV[BEHAV$SPP=="Pm","DurationMin"],na.rm=T)
#0.1781099


#### Models of Maximum Dive Duration P3 ####

# Models of Maximum Dive Duration  P3:...Model 5a, Table 4) yielded considerably 
# stronger explanations ...explaining more that __% of the variance in maximum dive durations.
summary(LM_DURATION_MAX_MASS_5)$adj.r.squared*100
#93.02371

# Models of Median Dive Duration and Depth P1:
# proportion of the variance in Zmean (R2=___) explained by these covariates 
# was lower than the proportion of variance explained by models of Tmax  (R2=___). 
summary(LM_DEPTH_MEAN_MASS_5)$adj.r.squared
#0.8115317
summary(LM_DURATION_MAX_MASS_5)$adj.r.squared
#0.9302371

#### Time Efficiency P1 ####

# Time Efficiency P1:...M. densirostris and Z. cavirostris spent maxima of __% and __% ...
max(c(rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Day",12:15]),
      rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Night",12:15])))
#37
max(c(rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Day",13:15]),
      rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Night",13:15])))
#34.2

# Time Efficiency P1:...P. electra, G. macrorhynchus, and P. macrocephalus were able to spend maxima of __%, __%, and __%, 
max(rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,5:15])+
      0.5*TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0 & TAT$T46==0,4])
#84.1

max(c(rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0,9:15]),
      (rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,5:15])+0.5*TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,4])))
#89.15

max(c(rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Day",10:15]),
      rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Night",10:15])))
#72.3

# Time Efficiency P1:..., on average sperm whales were able to spend ___x more 
# time in their target foraging strata than M. densirostris
median(c(rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Day",10:15]),
         rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Night",10:15])))/
  median(c(rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Day",12:15]),
           rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Night",12:15])))
#2.05428


# Time Efficiency P1:...delphinids therefore spend only _%  and _% of their total time 
# budgets within target foraging strata relative to the beaked whales _%  and _%
median(c(rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0 & TAT$T46==0,5:15][1:127,]),
         rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,5:15])+
           0.5*TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0 & TAT$T46==0,4]))
#27.225

median(c(rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0,9:15]),
         (rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,5:15])+0.5*TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,4])))
#31.6

median(c(rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Day",12:15]),
         rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Night",12:15])))
#23.95
median(c(rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Day",13:15]),
         rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Night",13:15])))
#22.6

#### Diurnal Variation in Habitat Use P1 ####

# Diurnal Variation in Habitat Use P1:...P. electra, displayed a binary response with no daytime dives recorded below 25m 
# or the 24°C isotherm (median depth ___ m), in over __ hours 
median(TAT[TAT$REGION%in%c("NEPC","SEPC","TOTO"),"ISO24"])
#117.1804

sum(BEHAV[BEHAV$What%in%c("Dive", "Surface") & BEHAV$SPP=="Pe" & BEHAV$DAY_NIGHT=="Day","DurationMin"])/(60*60) + 
  nrow(SERIES[SERIES$SPP=="Pe" & SERIES$DAY_NIGHT=="Day",])*2.5/60 + 
  sum(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Day","DURATION"])
#869.5994


#  Diurnal Variation in Habitat Use P1:...G. macorhynchus...undertook daytime dives () that were on 
#average __m deeper 
mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
  mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)
#416.8482

#  Diurnal Variation in Habitat Use P1:...G. macorhynchus...undertook daytime dives () that were...
# __% less frequent than nighttime dives.
100*(nrow(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Day",])/
  sum(BEHAV[BEHAV$What%in%c("Dive", "Surface") & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Day","DurationMin"])/(60*60))/
  (nrow(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night",])/
     sum(BEHAV[BEHAV$What%in%c("Dive", "Surface") & BEHAV$SPP=="Gm" & BEHAV$DAY_NIGHT=="Night","DurationMin"])/(60*60))
#69.99633

# Diurnal Variation in Habitat Use P1: highly sexually dimorphic species with 
#males that were ___x larger by mass
median(MASS[MASS$SPP=="Gm"  & MASS$SEX=="M" & MASS$AGE=="A","MASS_MED"])/
  median(MASS[MASS$SPP=="Gm"  & MASS$SEX=="F" & MASS$AGE=="A","MASS_MED"]) 
#2.05547

# Diurnal Variation in Habitat Use P1: highly sexually dimorphic species with males 
# ... able to dive __% longer than females
100*mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night" & BEHAV$Sex=="M","DurationMin"],na.rm=T)/
  mean(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night" & BEHAV$Sex=="F or M","DurationMin"],na.rm=T)-100
#5.248385

# Diurnal Variation in Habitat Use P1: Overall P. macrocephalus displayed small diurnal 
# differences between median daytime (__m) and nighttime (__m) dive depths
median(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)
#920
median(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)
#888

# Diurnal Variation in Habitat Use P1: Females and sub-adult males from matrilineal 
# social groups diving on average to ___% shallower depths 
100*mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="F","DepthMin"],na.rm=T)/
  mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y" & BEHAV$Sex=="M","DepthMin"],na.rm=T)-100
#-3.524818

# Diurnal Variation in Habitat Use P1: Females and sub-adult males from matrilineal 
# social groups... showing 26.4% larger diurnal variation in dive depths 
# relative to sub-adult males 
100*(mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="F" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="F" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)-
(mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="M" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="M" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)))/
(mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="F" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
  mean(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y"  & BEHAV$Sex=="F" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T))
#26.38725

# Diurnal Variation in Habitat Use P1:...M. densirostris exhibited daytime dives that were on average __m shallower 
mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
  mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)
#-142.3897
100*(mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Day","DepthMin"],na.rm=T)-
  mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T))/
  mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y" & BEHAV$DAY_NIGHT=="Night","DepthMin"],na.rm=T)
#-11.63822

# Diurnal Variation in Habitat Use P1:...o	This pattern was also observed in time 
# series of dives from _ of _ individual, M. densirostris,
for(i in unique(BEHAV[BEHAV$SPP=="Md","Ptt"]))
{HIST_VERT(BEHAV[BEHAV$Ptt==i & BEHAV$DAY_NIGHT=="Night" ,"DepthMin"],AXES=T,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=F,XLIM=c(0,50),XLAB=NA,YLAB=NA,
          COL=rgb(col2rgb("lightblue4")[1]/255,col2rgb("lightblue4")[2]/255,col2rgb("lightblue4")[3]/255,alpha=1))
HIST_VERT(BEHAV[BEHAV$Ptt==i & BEHAV$DAY_NIGHT=="Day" ,"DepthMin"],AXES=F,
          BREAKS=seq(0,2000,length=41),THIN=4,ADD=T,XLIM=c(0,50),XLAB=NA,YLAB=NA,
          COL=rgb(col2rgb("lightblue")[1]/255,col2rgb("lightblue")[2]/255,col2rgb("lightblue")[3]/255,alpha=0.5))
text(x=20,y=10,labels=i)}
#5 (108419 111675 111680 111682 52627) of 7 (108419 111664 111670 111675 111680 111682 52627)

#### Spatial Habitat Use P1 ####

#Spatial Habitat Use P1: In particular, <__% of CTCRW corrected position fixes in 
#the shallowest diving species, P. electra, occurred over habitats where the 
#benthos was less than the ___m maximum dive depth

PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pe","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
mean(PROP_FIXES_temp)*100
#0.4517828
remove(PROP_FIXES_temp)

#Spatial Habitat Use P1: In particular, <__% of CTCRW corrected position fixes in 
#the shallowest diving species, P. electra, occurred over habitats where the 
#benthos was less than the ___m maximum dive depth
max(BEHAV[BEHAV$SPP=="Pe" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)
#504

# Spatial Habitat Use P1: At the other end of the dive depth and duration spectrum, 
# __% of CTCRW corrected position fixes from Z. cavirostris, 
# occurred over habitats where the benthos
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
mean(PROP_FIXES_temp)*100
#73.86023
remove(PROP_FIXES_temp)

# Spatial Habitat Use P1: Z. cavirostris, occurred over habitats where the benthos 
# depth was <____m, the maximum dive depth recorded in this species 
max(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)
#1888

# Spatial Habitat Use P2: Females (n = 10) were consistently localized along the northern slope of the Great Bahama Canyon...
length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="F","PTT"]))
#10

# Spatial Habitat Use P2: By contrast, sub-adult males (n = 16) that were typically encountered solitarily or in small bachelor groups ...
length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Pm" & ARGOS_CTCRW$Sex=="M","PTT"]))
#16

# Spatial Habitat Use P1: Other species fell along a spectrum from a relatively 
# low proportion of position fixes in G. macorhynchus (<__%) ... 
# occurring over habitats where the benthos depth
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Gm","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
mean(PROP_FIXES_temp)*100
#38.18928
remove(PROP_FIXES_temp)

# Spatial Habitat Use P1: Other species fell along a spectrum...
# high proportion of M. densirostris position fixes (>__%) occurring over habitats 
# where the benthos depth
PROP_FIXES_temp=c()
for(i in 1:length(unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])))
{PROP_FIXES_temp[i]=nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])[i] & 
                                       ARGOS_CTCRW$LOCTYPE=="o" &
                                       ARGOS_CTCRW$BATHY>=min(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T) & 
                                       ARGOS_CTCRW$BATHY<=max(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T),])/
  nrow(ARGOS_CTCRW[ARGOS_CTCRW$PTT==unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"])[i] & ARGOS_CTCRW$LOCTYPE=="o",])}
mean(PROP_FIXES_temp)*100
#60.06335
remove(PROP_FIXES_temp)

# Spatial Habitat Use P1: M. densirostris in particular showed considerable 
# inter-individual variation, with certain individuals (Ptt _____, ______) 
# exhibiting consistently strong associations with areas where the benthos fell 
# between __ and __m 
for(i in unique(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Md","PTT"]))
{plot(BATHY,col=OCEAN_COL,ylim=c(23,27),xlim=c(-79.6,-74.9), legend=F)
 points(ARGOS_CTCRW[ARGOS_CTCRW$PTT==i & ARGOS_CTCRW$LOCTYPE=="o","LONG"],
        ARGOS_CTCRW[ARGOS_CTCRW$PTT==i & ARGOS_CTCRW$LOCTYPE=="o","LAT"],col="navy", pch=20,cex=0.5)
 text(x=-79,y=23.5,labels=i,pos=4)}
#9 (52627  93222  93232 108419 111664 111670 111675 111676 129715) of 
#11 (52627  93222  93232 108419 111664 111670 111675 111676 111680 111682 129715)

# Spatial Habitat Use P1: M. densirostris....consistently strong associations 
# with areas where the benthos fell between __ and __m 
min(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)
max(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)

# Spatial Habitat Use P1: The other two M. densirostris individuals, 
# both a male (Ptt _____) and a female (Ptt _____), were tagged over deeper 
# waters in the center of the Great Bahama 
# 111682 (M)
# 111680 (F)


# Spatial Habitat Use P1: Z. cavirostris showed a relatively high correlation 
# of dive depths with bottom depths (ρ = 0.62), 
cor(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DepthMin"],
    BEHAV[BEHAV$SPP=="Zc"  & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DEPTH_CRW"])
#0.6147266

# Spatial Habitat Use P1: Z. cavirostris...broad range of water column depths that this species occupied (central 90% quantile, __-__m)
quantile(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc" & !is.na(ARGOS_CTCRW$BATHY),"BATHY"],0.05,na.rm=T)
#851.2 
quantile(ARGOS_CTCRW[ARGOS_CTCRW$SPP=="Zc" & !is.na(ARGOS_CTCRW$BATHY),"BATHY"],0.95,na.rm=T)
#2247

# Spatial Habitat Use P1:...other species all showed low overall correlations of 
# dive depths with bottom depths (r=__-__)
cor(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DepthMin"],
    BEHAV[BEHAV$SPP=="Gm"  & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DEPTH_CRW"])
#0.1643434
cor(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DepthMin"],
    BEHAV[BEHAV$SPP=="Pm"  & BEHAV$FORAGING=="Y" & 
            !is.na(BEHAV$DepthMin) & !is.na(BEHAV$DEPTH_CRW),"DEPTH_CRW"])
#0.2672341

# Spatial Habitat Use P1: 

nrow(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M",])

# Spatial Habitat Use P1: By contrast sub-adult males (n=___) P. macrocephalus ....
# both shallower and deeper than their maximum dive depth (___).

nrow(TAGS[TAGS$SPP=="Pm" & TAGS$Sex=="M",])

max(BEHAV[BEHAV$SPP=="Pm" & BEHAV$FORAGING=="Y" & BEHAV$Sex=="M","DepthMin"],na.rm=T)


#### DISCUSSION ####

#### Dive behavior and body mass P1 ####

# Dive behavior and body mass P1:...the relatively strong performance of the 
# Noren and Williams (2000) Eq. 2 model among the 
# delphinids and physeterids (R2=_),
TAGS_temp=TAGS[!is.na(TAGS$DURATION_MAX) & TAGS$SPP%in%c("Pe","Gm","Pm"),]

1-(sum((log10(TAGS_temp$DURATION_MAX/60)-
          (0.47*log10(TAGS_temp$MASS_MED)+log10(0.68)))^2/
         nrow(TAGS_temp))/
     sum((log10(TAGS_temp$DURATION_MAX/60)-
            mean(log10(TAGS_temp$DURATION_MAX/60)))^2/
           nrow(TAGS_temp)))
#0.902621
remove(TAGS_temp)


# Dive behavior and body mass P1: ...increase in maximum and median dive duration 
# between M. densirostris (__kg) and Z. cavirostris (__kg)
median(MASS[MASS$SPP=="Md" & MASS$AGE=="A","MASS_MED"])
#842.9224
median(MASS[MASS$SPP=="Zc" & MASS$AGE=="A","MASS_MED"])
#1556.701

# Dive behavior and body mass P1: the comparatively low proportion of variance 
#in maximum dive durations explained by body mass alone in this 
# assemblage (R2=___), 
summary(LM_DURATION_MAX_MASS_1)$adj.r.squared
#0.3259028

####  Dive behavior and myoglobin concentration P1  ####

# Dive behavior and myoglobin concentration P1: which focused particularly on 
# deep-diving specialist taxa, the inclusion of [Mb] as an explanatory variable 
# alone (R2=_) and within allometric models (R2=_) 
summary(LM_DURATION_MAX_MASS_2)$adj.r.squared
#0.2601019
summary(LM_DURATION_MAX_MASS_4)$adj.r.squared
#0.3764189

####  Dive behavior and Aerobic Dive Limits P1  ####


# Dive behavior and Aerobic Dive Limits P1:...Both estimates of cADL were substantially 
# less than the observed mean dive duration of M. densirostris and Z. cavirostris 
# (___ min and ___ min, respectively). 
mean(BEHAV[BEHAV$SPP=="Md" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60
#46.17838
mean(BEHAV[BEHAV$SPP=="Zc" & BEHAV$FORAGING=="Y","DurationMin"],na.rm=T)/60
#65.35938

####  Dive behavior and Aerobic Dive Limits P3  ####


# Dive behavior and Aerobic Dive Limits P3:..the beaked whales were much less time efficient 
# with a ceiling of 34-37% of the proportion of time budget spent foraging

max(c(rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Day",12:15]),
      rowSums(TAT[TAT$SPP=="Md" & TAT$DAY_NIGHT=="Night",12:15])))
#37
max(c(rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Day",13:15]),
      rowSums(TAT[TAT$SPP=="Zc" & TAT$DAY_NIGHT=="Night",13:15])))
#34.2

# Dive behavior and Aerobic Dive Limits P3:..compared with upwards of 72-89% of 
#time spent in target foraging strata by the sperm whales and delphinids 

max(rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,5:15])+
      0.5*TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0 & TAT$T46==0,4])
#84.1

max(c(rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Day" & TAT$T2460!=0,9:15]),
      (rowSums(TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,5:15])+0.5*TAT[TAT$SPP=="Gm" & TAT$DAY_NIGHT=="Night"& TAT$T2460!=0,4])))
#89.15

max(c(rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Day",10:15]),
      rowSums(TAT[TAT$SPP=="Pm" & TAT$DAY_NIGHT=="Night",10:15])))
#72.3

####  Dive behavior and Aerobic Dive Limits P4  ####

# Dive behavior and Aerobic Dive Limits P4: In a few aberrant cases 
# (<_% of IDDI recorded in these species) successive extended deep dives 
# were separated by only a short surface interval (<__min)
100*nrow(BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP%in%c("Md","Zc") & BEHAV$IDDI<10*60,])/
  nrow(BEHAV[!is.na(BEHAV$IDDI) & BEHAV$SPP%in%c("Md","Zc"),])
#1.463415


##### Delphinid Synthesis Hypothesis P1 ####



# Delphinid Synthesis Hypothesis P1: This species [P. electra] instead 
# forages consistently over nighttime periods, spending upwards of  _% of time 
# between __m and __m
mean(rowSums(TAT[TAT$SPP=="Pe" & TAT$DAY_NIGHT=="Night" & TAT$T2460!=0 & TAT$T46==0,5:15]))
#45.96457

TAT$MED_ISO24[1]
#109.0197


# Delphinid Synthesis Hypothesis P1: By contrast G. macrorhynchus pursued a mixed 
# strategy consisting of 1) less frequent but also deeper (up to __m; Figs. 2 and 8)
max(BEHAV[BEHAV$SPP=="Gm" & BEHAV$FORAGING=="Y","DepthMin"],na.rm=T)




###### DRYAD DATA EXPORT ######

DRYAD_README_temp = list(Name = "Project Name: Bahamas deep-diving toothed whale telemetry and dive behavior",
                         Space = "",
                         Date_Range = "Date Range: 2009 to 2014",
                         Space = "",
                         Subject = "Subject: Movements and dive behaviors of deep-diving toothed whales in the Bahamas",
                         Space = "",
                         Region = "Region: Channels of the Northern Bahama Archipelago in the Sub-tropical North Atlantic", 
                         Space = "",
                         Contact_Person = "Contact Person(s): Trevor Joyce, trevorwjoyce@gmail.com, John Durban, john.durban@noaa.gov, Diane Claridge, dclaridge@bahamaswhales.org",
                         Space = "",
                         Data_Description = "Data Description: ",
                         Space = "",
                         Related_Figures = "Related_Figures:",
                         Space = "",
                         Data_Fields_Title = "Data Fields: ",
                         Space = "")

##### ARGOS ####

write.csv(ARGOS[,c("PTT","Date","Class","LastOfLatitude","LastOfLongitude",
                   "LastOfSemi.major.axis","LastOfSemi.minor.axis","LastOfEllipse.orientation",
                   "Species")],
          "/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/ARGOS.csv")

DRYAD_README_ARGOS_temp = DRYAD_README_temp
DRYAD_README_ARGOS_temp$Data_Description = paste("Data Description: Location estimates and time stamps",
                                           "returned by Argos satellites from SPOT5 and SPLASH (Mk10a)",
                                           "tags produced by Wildlife Computers. Location error information",
                                           "derives from Argos Kalman filtering algorithm",sep=" ")
DRYAD_README_ARGOS_temp$Related_Figures = paste("Related Figures: Fig. 8",sep=" ")

DRYAD_ARGOS_temp = data.frame(Field = c("PTT","Date","Class","LastOfLatitude","LastOfLongitude",
                                        "LastOfSemi.major.axis","LastOfSemi.minor.axis","LastOfEllipse.orientation",
                                        "Species"),
                               Definition = c("Platform Transmitter Tag unique ID number",
                                              "Date-Time (MM/DD/YY HH:MM)",
                                              "Argos Location Quality Classes (best: 3, worst: B)",
                                              "Latitude (degrees, WGS84)",
                                              "Longitude (degrees, WGS84)",
                                              "Length of major axis of Argos error ellipse (km)",
                                              "Length of minor axis of Argos error ellipse (km)",
                                              "Orientation of Argos error ellipse (degrees)",
                                              "Common Name"),stringsAsFactors = F)

for(i in 1:nrow(DRYAD_ARGOS_temp))
{DRYAD_README_ARGOS_temp[[DRYAD_ARGOS_temp$Field[i]]] = paste(DRYAD_ARGOS_temp$Field[i],": ",DRYAD_ARGOS_temp$Definition[i],sep="")}
lapply(DRYAD_README_ARGOS_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_ARGOS.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_ARGOS_temp,DRYAD_ARGOS_temp)

##### BEHAV ####

write.csv(BEHAV[,c("Ptt","Instr","Start","End","What","Shape",
                   "DepthMin","DurationMin","Species",
                   "FORAGING")],"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/BEHAV.csv")

DRYAD_README_BEHAV_temp = DRYAD_README_temp
DRYAD_README_BEHAV_temp$Data_Description = paste("Data Description: A log of dives and surface intervals",
                                                 "returned by Argos satellites from SPLASH (Mk10a)",
                                                 "transmitter tags produced by Wildlife Computers. Depths represent",
                                                 "the maximum depth recorded on a dive, the duration of which",
                                                 "was defined as the interval between successive", 
                                                 "dry measurements on the wet-dry sensor.", 
                                                 "Dives that did not exceed 15m was logged as Surface Time.",sep=" ")
DRYAD_README_BEHAV_temp$Related_Figures = paste("Related Figures: Figs. 1,2,3,5b,6, and 7",sep=" ")

DRYAD_BEHAV_temp = data.frame(Field = c("Ptt","Instr","Start","End","What","Shape",
                                        "DepthMin","DurationMin","Species",
                                        "FORAGING"),
                               Definition =c("Platform Transmitter Tag unique ID number",
                                             "Tag type",
                                             "Start Date-Time (MM/DD/YYYY HH:MM)",
                                             "End Date-Time (MM/DD/YYYY HH:MM)",
                                             "Row designation",
                                             "Dive shape description",
                                             "Maximum Depth Reached on Dive (m)",
                                             "Duration of dive (sec)",
                                             "Common name",
                                             "Categorization of dive as foraging or non-foraging based on depth"),stringsAsFactors = F)

for(i in 1:nrow(DRYAD_BEHAV_temp))
{DRYAD_README_BEHAV_temp[[DRYAD_BEHAV_temp$Field[i]]] = paste(DRYAD_BEHAV_temp$Field[i],": ",DRYAD_BEHAV_temp$Definition[i], sep="")}

lapply(DRYAD_README_BEHAV_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_BEHAV.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_BEHAV_temp, DRYAD_BEHAV_temp)

##### SERIES ####

write.csv(SERIES[,c("Ptt","Instr","Day","Time","LocationQuality",
                    "Latitude","Longitude","Depth","DRange","Species")],
          "/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/SERIES.csv")

DRYAD_README_SERIES_temp = DRYAD_README_temp
DRYAD_README_SERIES_temp$Data_Description = paste("Data Description: A moderate resolution time series of time-depth recorder (TDR) measurements",
                                                  "returned via Argos satellites from SPLASH (Mk10a)",
                                                  "transmitter tags produced by Wildlife Computers.",sep=" ")
DRYAD_README_SERIES_temp$Related_Figures = paste("Related Figures: Fig. 4",sep=" ")

DRYAD_SERIES_temp = data.frame(Field = c("Ptt","Instr","Day","Time","LocationQuality",
                                         "Latitude","Longitude","Depth","DRange","Species"),
                            Definition = c("Platform Transmitter Tag unique ID number",
                                           "Tag type",
                                           "Date (MM/DD/YY)",
                                           "Time (HH:MM:SS)",
                                           "Argos Location Quality Categories (best: 3, worst: -2(B))",
                                           "Latitude (degrees, WGS84)",
                                           "Longitude (degrees, WGS84)",
                                           "Depth (m)",
                                           "Depth error estimate (m)",
                                           "Common Name"),stringsAsFactors = F)

for(i in 1:nrow(DRYAD_SERIES_temp))
{DRYAD_README_SERIES_temp[[DRYAD_SERIES_temp$Field[i]]] = paste(DRYAD_SERIES_temp$Field[i],": ",DRYAD_SERIES_temp$Definition[i], sep="")}
lapply(DRYAD_README_SERIES_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_SERIES.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_SERIES_temp,DRYAD_SERIES_temp)

##### TAT ####

write.csv(TAT[,c("Ptt","RDate","Period","T2460","T2224","T2022","T1820","T1618",           
                 "T1416","T1214","T1012","T810","T68","T46","T04","Species")],
          "/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/TAT.csv")

DRYAD_README_TAT_temp = DRYAD_README_temp
DRYAD_README_TAT_temp$Data_Description = paste("Data Description: Histograms representing the proportion of time a tag",
                                               "spent in 12 temperature bins define in 2C increments between 4C and 24C",
                                               "TAT histograms were returned by Argos satellites from SPOT5",
                                               "tags produced by Wildlife Computers.",
                                               "TAT were collected in 6-hour sampling periods, and sampling periods",
                                               "were programmed to begin at 01:00, 07:00, 13:00, or 21:00 local time,",
                                               "so that the majority (>80%) of sampling of each TAT histogram fell ",
                                               "within either daytime or nighttime.",
                                               "TAT histograms that were translated into units of depth using",
                                               "the hydrographic data and interpolation methods detailed in",
                                               "Joyce et al. (2016). ",sep=" ")
DRYAD_README_TAT_temp$Related_Figures = paste("Related Figures: Fig. 5a",sep=" ")

DRYAD_TAT_temp = data.frame(Field = c("Ptt","RDate","T2460","T2224","T2022","T1820","T1618",           
                                      "T1416","T1214","T1012","T810","T68","T46","T04","Species"),
                             Definition = c("Platform Transmitter Tag unique ID number",
                                            "Date-time of histogram start time (MM/DD/YY HH:MM)",
                                            "% in 24C - 60C","% in 22C - 24C",
                                            "% in 20C - 22C","% in 18C - 20C",
                                            "% in 16C - 18C","% in 14C - 16C",
                                            "% in 12C - 14C","% in 10C - 12C",
                                            "% in 8C - 10C","% in 6C - 8C",
                                            "% in 4C - 6C","% in 0C - 4C",
                                            "Species Name"),stringsAsFactors = F)

for(i in 1:nrow(DRYAD_TAT_temp))
{DRYAD_README_TAT_temp[[DRYAD_TAT_temp$Field[i]]] = paste(DRYAD_TAT_temp$Field[i],": ",DRYAD_TAT_temp$Definition[i],sep="")}
lapply(DRYAD_README_TAT_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_TAT.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_TAT_temp,DRYAD_TAT_temp)

##### TAGS ####

write.csv(TAGS[,c("PTT","Species","Age","How.age","Sex","How.sex","Deploy.date","Tag.type",
                  "DURATION_MAX","IDDI_MED","MB","MASS_MED")],"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/TAGS.csv")

DRYAD_README_TAGS_temp = DRYAD_README_temp
DRYAD_README_TAGS_temp$Data_Description = paste("Data Description: A summary of tagged individuals.",
                                                "The variables DURATION_MAX and IDDI_MED were calculated from the behavior log",
                                                "returned by Argos satellites from SPLASH (Mk10a)",
                                                "transmitter tags produced by Wildlife Computers.",
                                                "The variable MASS_MED was calculated from models", 
                                                "of body mass as a function of standard length.",
                                                "The variable MB derived from a variety of literature", 
                                                "sources.",sep=" ")
DRYAD_README_TAGS_temp$Related_Figures = paste("Related Figures: Fig. 3",sep=" ")
DRYAD_TAGS_temp = data.frame(Field = c("PTT","Species","Age","How.age","Sex","How.sex","Deploy.date","Tag.type",
                                       "DURATION_MAX","IDDI_MED","MB","MASS_MED"),
                                   Definition = c("Platform Transmitter Tag unique ID number",
                                                  "Species Name",
                                                  "Age Class","How age class was determined",
                                                  "Sex","How sex was determined",
                                                  "Tag deployment date (MM/DD/YY HH:MM)","Tag type",
                                                  "Maximum Dive Duration (minutes) per individual",
                                                  "Median Inter-deep dive interval (minutes) per individual",
                                                  "Myoglobin Concentration (g/kg muscle tissue)",
                                                  "Body Mass (kg)"),stringsAsFactors = F)

for(i in 1:nrow(DRYAD_TAGS_temp))
{DRYAD_README_TAGS_temp[[DRYAD_TAGS_temp$Field[i]]] = paste(DRYAD_TAGS_temp$Field[i],": ",DRYAD_TAGS_temp$Definition[i],sep="")}
lapply(DRYAD_README_TAGS_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_TAGS.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_TAGS_temp,DRYAD_TAGS_temp)

##### PHYLO_DATA ####

write.csv(PHYLO_DATA[,c("COMMON_NAME","SPP_NAME","T_MAX","MASS","Mb_MAX","IDDI")],"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/PHYLO_DATA.csv")

DRYAD_README_PHYLO_DATA_temp = DRYAD_README_temp
DRYAD_README_PHYLO_DATA_temp$Data_Description = paste("Data Description: A table of species used to implement",
                                                 "a phylogenetic generalized least squares (PGLS) inter-specific comparative",
                                                 "analysis. Aside from the five species tagged in this",
                                                 "study maximum dive duration (Tmax) and inter-deep-dive interval (IDDI)", 
                                                 "as well as body mass (m) and myoglobin concentration ([Mb])",
                                                 "derive from literature-reported values or were extracted",
                                                 "from supplemental materials.",sep=" ")
DRYAD_README_PHYLO_DATA_temp$Related_Figures = paste("Related Figures: Fig. 6",sep=" ")

DRYAD_PHYLO_DATA_temp = data.frame(Field = c("COMMON_NAME","SPP_NAME","T_MAX",
                                             "MASS","Mb_MAX","IDDI"),
                                   Definition = c("Common Name",
                                                  "Species Name",
                                                  "Maximum Dive Duration (minutes)",
                                                  "Body Mass (kg)",
                                                  "Myoglobin Concentration (g/kg muscle tissue)",
                                                  "Inter-deep dive interval (minutes)"),stringsAsFactors = F)
for(i in 1:nrow(DRYAD_PHYLO_DATA_temp))
{DRYAD_README_PHYLO_DATA_temp[[DRYAD_PHYLO_DATA_temp$Field[i]]] = paste(DRYAD_PHYLO_DATA_temp$Field[i],": ",DRYAD_PHYLO_DATA_temp$Definition[i],sep="")}
lapply(DRYAD_README_PHYLO_DATA_temp, write,"/Users/trevorjoyce/Grad School/Research/3_2012_Whale Tagging/Data/Dryad Export/README_PHYLO_DATA.txt", 
       append=T,ncolumns=1000 )
remove(DRYAD_README_PHYLO_DATA_temp,DRYAD_PHYLO_DATA_temp)