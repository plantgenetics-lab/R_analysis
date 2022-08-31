library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(psych)
library(car)
library(multcomp)

setwd('~/Desktop/R_analysis/kohara/data')
df <- read.csv("osmo-shock_600.csv")

df$Accession <- factor(df$Accession)
attach(df)
table(Accession)

TukeyHSD(aov(Chlorophyll~Accession))

glht(aov(Chlorophyll ~ Accession, data = df), linfct = mcp(Accession = "Tukey"))
