#########################
## Age-structure model ##
#########################

library(data.table)
library(ggplot2)

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


plot=ggplot(af, aes(x=Time, y=af, col=factor(mut_pos), group=mut_id))+
  geom_line() + labs(x="Generations", y="Allele Frequency")+
  theme(legend.position = "none") + facet_wrap(~label, ncol=1) +theme_bw()

ggsave("allele_plot.pdf", plot)
