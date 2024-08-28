#########################
## Age-structure model ##
#########################

library(data.table)
library(ggplot2)
library(patchwork)

setwd("~/Documents/yuseob/")

af_files=list.files(path ="~/Documents/yuseob/",pattern ="af_")
af=NULL
for (i in af_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(strsplit(i, "_")[[1]][3], ".txt")[[1]]
  dt=fread(i)
  dt[, `:=` (as=as, bs=bs)]
  af=rbind(af,dt)
}

af[, label:=paste("as =", as, "bs =", bs), by=c("as", "bs")]

pop_files=list.files(path ="~/Documents/yuseob/",pattern ="pop_")
pop=NULL
for (i in pop_files){
  as=strsplit(i, "_")[[1]][2]
  bs=strsplit(strsplit(i, "_")[[1]][3], ".txt")[[1]]
  pdt=fread(i)
  pdt[, `:=` (as=as, bs=bs)]
  pop=rbind(pop,pdt)
}

pop[, label:=paste("as =", as, "bs =", bs), by=c("as", "bs")]

pop=melt(pop, id.vars=c("Time", "label","N"), measure.vars = c("larva", "subadult","adult"), variable.name = "age", value="ageN")

pplot=ggplot(pop, aes(x=Time, y=ageN, fill=age))+
  geom_area() + labs(x="Generations", y="Number of Individuals", fill="Age")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()
# pplot

plot=ggplot(af, aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line() + labs(x="Generations", y="Allele Frequency", col="Mutation Position")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()
# plot

afp=(plot|pplot)+ plot_layout(axes = "collect")&theme(legend.position="bottom")
afp

ggsave("afp_nf0.7.pdf", afp)

