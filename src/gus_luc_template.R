library(openxlsx)
library(ggplot2)
library(dplyr)
library(ggsignif)

setwd('~/Desktop/R_analysis_lab')
gus <- read.xlsx('./data/21.12.22/21.12.22 GUS .xlsx')
luc <- read.xlsx('./data/21.12.22/21.12.22 LUC.xlsx')

# サンプル名とサンプル数
sample_name <- c("WT","AR124","ARK","ARK S1029D")
sample_num <- 24
assay <- c(rep("-ABA",sample_num/2),rep("+ABA",sample_num/2))

gus_luc <- function() {
  df_sample_name <- c(rep(sample_name,2,each=3))
  
  lane_num <- (sample_num+1)%/%8+1
  
  gus_raw <- c()
  luc_raw <- c()
  for (n in 1:lane_num+1) {
    gus_raw <- as.numeric(c(gus_raw,gus[3:10,n]))
    luc_raw <- as.numeric(c(luc_raw,luc[3:10,n]))
  }
  
  
  df <- data.frame(df_sample_name,assay,gus_raw[1:sample_num],luc_raw[1:sample_num])
  
  df$gus <- df[,3]-gus_raw[sample_num+1]
  df$luc <- df[,4]-luc_raw[sample_num+1]
  df$gus_luc <- df$gus/df$luc
  
  df %>%
    mutate(df_sample_name=factor(df_sample_name,levels=sample_name)) %>%
    ggplot(aes(x=factor(df_sample_name),y=gus_luc,fill=factor(assay))) +
    stat_summary(fun="mean",geom="bar",position=position_dodge(width = 0.9),width=0.8,colour="black",alpha=0.6) +
    scale_fill_manual(values=c("#f5f5f5","#000000")) +
    theme_classic() +
    xlab("sample_name") + ylab("PpLEA1-GUS/LUC") +
    stat_summary(fun="mean",fun.min=function(x)mean(x)-(sd(x)/sqrt(3)),fun.max=function(x)mean(x)+(sd(x)/sqrt(3)),position=position_dodge(width = 0.9)) +
    geom_point(position = position_dodge(width = 0.9)) +
    theme(axis.title.x = element_text(size=9, family = "Arial"), 
          axis.title.y = element_text(size=9, family = "Arial"), 
          axis.text.x = element_text(size=9, colour = 1, family = "Arial"), 
          axis.text.y = element_text(size =9, colour = 1, family = "Arial"))
}

gus_luc()
