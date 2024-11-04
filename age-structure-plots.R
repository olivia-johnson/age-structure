#########################
## Age-structure model ##
#########################

library(data.table)
library(ggplot2)
library(patchwork)

setwd("~/Documents/yuseob/")

af_files=list.files(path ="~/Documents/yuseob/",pattern ="af_.+txt")
af=NULL
for (i in af_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(i, "_")[[1]][3]
  fit_type=strsplit(strsplit(i, "_")[[1]][4], ".txt")[[1]]
  dt=fread(i)
  dt[, `:=` (as=as, bs=bs, fit=fit_type)]
  af=rbind(af,dt)
}

af[, label:=paste("as =", as, "bs =", bs), by=c("as", "bs")]
af[, seg:= ifelse(af>0 & af<1.0, T, F)]
af[, prop:=(sum(seg==T)/(max(mut_pos)+1)), by=c("Time", "label", "fit")]
af[,L:=max(mut_pos), by="fit"]
af[,l.id:=paste0(fit,mut_id)]

pop_files=list.files(path ="~/Documents/yuseob/",pattern ="pop_.+txt")
pop=NULL
for (i in pop_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(i, "_")[[1]][3]
  fit_type=strsplit(strsplit(i, "_")[[1]][4], ".txt")[[1]]
  pdt=fread(i)
  pdt[, `:=` (as=as, bs=bs, fit=fit_type)]
  pop=rbind(pop,pdt)
}

pop[, label:=paste("as =", as, "bs =", bs), by=c("as", "bs")]

# library(scales)
# show_col(hue_pal()(3))
# 
# ggplot(pop, aes(x=Time))+
#   geom_line(aes(y=larva-Kt), col="#F8766D", alpha=0.8)+
#   geom_line(aes(y=subadult-`Kt-1`), col="#00BA38", alpha=0.8)+
#   geom_line(aes(y=adult-`Kt-2`), col="#619CFF", alpha=0.8)+
#   facet_wrap(~label+fit, ncol=4) +theme_bw()+
#   labs(y="Difference from K")


pop=melt(pop, id.vars=c("Time", "label","N", "as", "bs","Kt", "Kt-1", "Kt-2", "fit"), measure.vars = c("larva", "subadult","adult"), variable.name = "age", value="ageN")



pplot=ggplot(pop, aes(x=Time, y=ageN, fill=age))+
  geom_area() + labs(x="Generations", y="Number of Individuals", fill="Age")+
  theme_bw() + facet_wrap(~fit, ncol=1) +theme(legend.position = "none")
# pplot

m100=sample(99,10, replace=FALSE)
m1000=sample(999,10, replace=FALSE)
m500=sample(499,10, replace=FALSE)
m800=sample(799,10, replace=FALSE)

muts=c(af[mut_pos<10 & L==9, unique(l.id)],af[mut_pos%in%m100 & L==99, unique(l.id)],af[mut_pos%in%m1000 & L==999, unique(l.id)],af[mut_pos%in%m500 & L==499, unique(l.id)],af[mut_pos%in%m800 & L==799, unique(l.id)])

plot=ggplot(af[l.id%in%muts], aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line(linewidth=0.3, alpha=0.5) + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme_bw() + facet_wrap(~fit, ncol=1) +theme(legend.position = "none")
 plot
 
 dd=af[, unique(prop), by=c("Time", "fit", "label")]
 propplot=ggplot(dd, aes(x=Time, y=V1, col=fit))+
   geom_line(linewidth=0.3, alpha = 0.8) + labs(x="Generations", y="Proportion of segregating alleles")+
   theme_bw() + facet_wrap(~fit, ncol=1) #+theme(legend.position = "none")
 propplot
afp=(propplot|plot|pplot)+ plot_layout(axes = "collect")&theme(legend.position="None")
afp

ggsave("all_plot.pdf", afp, height=12, width=12)

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

