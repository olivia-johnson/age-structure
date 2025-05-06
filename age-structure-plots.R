#########################
## Age-structure model ##
#########################

library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)

setwd("~/Documents/yuseob/forplots/")

af_files=list.files(path ="~/Documents/yuseob/forplots/",pattern ="af.+txt")
af=NULL
for (i in af_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(strsplit(i, "_")[[1]][3], "y")[[1]][2]
  J=strsplit(strsplit(strsplit(i, "_")[[1]][4], ".txt")[[1]], "J")[[1]][1]
  fit_type=strsplit(strsplit(i, "_")[[1]][1], "af")[[1]][2]
  dt=fread(i)
  dt[, `:=` (as=as, bs=bs, fit=fit_type, J=J)]
  af=rbind(af,dt)
}
options(scipen = 999)
af[, label:=paste("J = ", J, "d =", as, "c =", as.numeric(bs)), by=c("as", "bs", "J")]
af[, seg:= ifelse(af>0.0 & af<1.0, T, F)]
af[, nseg:=sum(seg==T), by=c("Time", "label", "fit")]
# af[, seg5:=ifelse(af>0.05 & af<0.95, T, F)]
# af[, seg3:=ifelse(af>0.03 & af<0.97, T, F)]
af[, seg1:=ifelse(af>0.1 & af<0.9, T, F)]
af[, prop:=(sum(seg==T)/(max(mut_pos)+1)), by=c("Time", "label", "fit")]
# af[, prop5:=(sum(seg5==T)/(max(mut_pos)+1)), by=c("Time", "label", "fit")]
# af[, prop3:=(sum(seg3==T)/(max(mut_pos)+1)), by=c("Time", "label", "fit")]
af[, prop1:=(sum(seg1==T)/(max(mut_pos)+1)), by=c("Time", "label", "fit")]
af[, het:=(2*af)*(1-af), by=c("Time", "label","fit", "mut_pos")]
af[, mean_het:=mean(het), by=c("Time", "label","fit")]
af[,L:=max(mut_pos), by="fit"]
af[,l.id:=paste0(J,fit,mut_id)]
af[as==0 &bs==0, fit:="Neutral"]



pop_files=list.files(path ="~/Documents/yuseob/forplots/",pattern ="pop.+txt")
pop=NULL
for (i in pop_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(strsplit(i, "_")[[1]][3], "y")[[1]][1]
  J=strsplit(strsplit(strsplit(i, "_")[[1]][4], ".txt")[[1]], "J")[[1]][1]
  fit_type=strsplit(strsplit(strsplit(i, "_")[[1]][4], ".txt")[[1]], "J")[[1]][2]
  pdt=fread(i)
  pdt[, `:=` (as=as, bs=bs, fit=fit_type, J=J)]
  pop=rbind(pop,pdt)
}
pop[as==0 &bs==0, fit:=paste0(" Neutral")]

pop[, label:=paste("J = ", J, "d =", as, "y =", bs), by=c("as", "bs", "J")]




# library(scales)
# show_col(hue_pal()(3))
# 
# ggplot(pop, aes(x=Time))+
#   geom_line(aes(y=larva-Kt), col="#F8766D", alpha=0.8)+
#   geom_line(aes(y=subadult-`Kt-1`), col="#00BA38", alpha=0.8)+
#   geom_line(aes(y=adult-`Kt-2`), col="#619CFF", alpha=0.8)+
#   facet_wrap(~label+fit, ncol=4) +theme_bw()+
#   labs(y="Difference from K")


pop=melt(pop, id.vars=c("Time", "label","N", "as", "bs", "fit", "J"), measure.vars = c("young_adult", "old_adult"), variable.name = "age", value="ageN")



pplot=ggplot(pop, aes(x=Time, y=ageN, fill=age))+
  geom_area() + labs(x="Generations", y="Number of Individuals", fill="Age")+
  theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "none")
# pplot


setwd("~/Documents/yuseob/")
m100=sample(99,10, replace=FALSE)
m1000=sample(999,10, replace=FALSE)
m500=sample(499,10, replace=FALSE)
m300=sample(299,10, replace=FALSE)

muts=c(af[mut_pos<10 & J==10, unique(l.id)],af[mut_pos%in%m100 & J==100, unique(l.id)],af[mut_pos%in%m1000 & J==1000, unique(l.id)],af[mut_pos%in%m500 & J==500, unique(l.id)],af[mut_pos%in%m300 & J==300, unique(l.id)])

plot=ggplot(af, aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line(linewidth=0.3) + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "none") + coord_cartesian(y = c(0,1))
 plot
 
 hplot=ggplot(af, aes(x=Time, y=mean_het))+
   geom_line()+
   facet_wrap(~label, ncol=1) + 
   labs(x="Generations", y="Mean Heterozygosity")+
   theme(legend.position = "right")+theme_bw()
 
 
 dd=unique(af[, .(prop, prop1), by=c("Time", "fit", "label")])
 propplot=ggplot(dd, aes(x=Time))+
   #geom_line(aes(y=prop5), col="hotpink", linewidth=0.3)+
   #geom_line(aes(y=prop3), col="lightgreen", linewidth=0.3)+
   geom_line(aes(y=prop1), linewidth=0.3, alpha=0.8)+
   #geom_line(aes( y=prop), linewidth=0.3, alpha = 0.8) + 
   labs(x="Generations", y="Proportion of segregating alleles (MAF >= 0.1)")+ coord_cartesian(ylim=c(0,1))+
   theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "right")
 propplot

 afp=(plot|propplot|hplot)+ plot_layout(axes = "collect")&theme(legend.position="None")
 afp
ggsave(paste0(fit_type,"_all.pdf"), afp, height=10, width=14)

dt=unique(melt(af[Time==19999], id.vars = c("Time", "as", "bs", "fit", "J"), measure.vars = c("nseg")))
dt[, J:=as.numeric(J)]
linear=ggplot(dt)+
  geom_line(aes(x=J, y=value, group="fit"))+facet_wrap(as~bs)+
  theme_bw() + scale_y_log10() +scale_x_log10()
linear
ggsave(paste0(fit_type,"_linear.pdf"), linear, height=12, width=12)

dd=af[Time==19999, mean_het, by=c("as", "bs", "fit", "J", "label")]
data=full_join(dt, dd, by=c("as", "bs", "fit", "J"))
data[, J:=as.numeric(J)]

hetlin=ggplot(data)+
  geom_line(aes(x=J, y=mean_het, group="fit"))+facet_wrap(as~bs)+
  theme_bw() + labs(y="Mean heterozygosity") + scale_x_log10()
ggsave(paste0(fit_type,"_hetlinear.pdf"), hetlin, height=12, width=12)


plot=ggplot(af, aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line(linewidth=0.3) + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "none") + coord_cartesian(x=c(17000, 17050), y = c(0,1))
plot
ggsave(paste0(fit_type,"_zoom.pdf"), plot, height=10, width=6)







adult_af=ggplot(af, aes(x=Time,  col=factor(mut_pos), group=mut_id))+
  geom_line(aes(y=af), col="darkgray")+
  geom_line(aes(y=old_af),linewidth=0.3)+
   labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "none") + coord_cartesian(x=c(5000, 5200), y = c(0,1))
adult_af
ggsave("JAnoagecut_adultaf.pdf", afp, height=12, width=12)

afp=(adult_af|propplot|hplot)+ plot_layout(axes = "collect")&theme(legend.position="None")
afp
ggsave("JAnoagecut_all.pdf", afp, height=10, width=14)

plot=ggplot(af, aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line(linewidth=0.3) + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme_bw() + facet_wrap(~label, ncol=1) +theme(legend.position = "none") + coord_cartesian(x=c(17000, 17100), y = c(0,1))
plot
ggsave("CPSLFS_zoom.pdf", plot, height=10, width=6)



afp=(propplot|plot)+ plot_layout(axes = "collect")&theme(legend.position="None")
afp

ggsave("allele_plot.pdf", afp, height=12, width=12)


plot=ggplot(af[fit=="halfJhalfK"], aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line(linewidth=0.3) + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()
plot
ggsave("af_halfJhalfK.pdf", plot, height=8, width=9)
ggsave("af_halfJhalfK.jpg", plot, height=8, width=9)





pplot=ggplot(pop[as=="nfs" & (bs!="test"&bs!="vec")], aes(x=Time, y=ageN, fill=age))+
  geom_area() + labs(x="Generations", y="Number of Individuals", fill="Age")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1, scales = "free_y") +theme_bw()
# pplot

plot=ggplot(af[as=="nfs"& (bs!="test"&bs!="vec")], aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line() + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1, scales = "free_y") +theme_bw()
# plot

afp=(plot|pplot)+ plot_layout(axes = "collect")&theme(legend.position="bottom")
afp

ggsave("afp_Kt.pdf", afp, height=8, width=12)

pplot=ggplot(pop[as<0.5], aes(x=Time, y=ageN, fill=age))+
  geom_area() + labs(x="Generations", y="Number of Individuals", fill="Age")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()
# pplot

plot=ggplot(af[as<0.5], aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line() + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()
# plot

afp=(plot|pplot)+ plot_layout(axes = "collect")&theme(legend.position="bottom")
afp

ggsave("afp_0.5.pdf", afp)

