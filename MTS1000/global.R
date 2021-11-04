# Dashboard v1, 2021/08/18
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyverse)
library(reshape2)

XYcurves = read.csv2('./dataset/XYcurves.csv',sep = ',')
#saveRDS(XYcurves,file ='./dataset/XYcurves.RDS')
# XYcurves = readRDS('./dataset/XYcurves.RDS')
XYcurves_1<-melt(XYcurves, id.vars=c('X'))
XYcurves_1$amini.acid<-as.numeric(XYcurves_1$variable)
XYcurves_1$value<-as.numeric(XYcurves_1$value)
XYcurves_1<-XYcurves_1[,-2]
colnames(XYcurves_1)[1]<-"Genes"

ClinVarMutations = read.csv2('./dataset/ClinVarMutations.csv',sep = ',')
#saveRDS(ClinVarMutations,file ='ClinVarMutations.RDS')
#ClinVarMutations = readRDS('ClinVarMutations.RDS')
ClinVarMutations_1=data.frame(GeneSymbol=ClinVarMutations$GeneSymbol,ColourDot=ClinVarMutations$ColourDot,amino.acid=ClinVarMutations$amino.acid,ProteinChange=ClinVarMutations$Protein.Change, NTChange=ClinVarMutations$NT.Change  )

XYcurves_ClinVarMutations<-merge(XYcurves_1, ClinVarMutations_1, by.x=c('Genes','amini.acid'), by.y=c('GeneSymbol','amino.acid'), sort = TRUE)
XYcurves_ClinVarMutations$label<-NULL

XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$ColourDot=="Red"]<- "Pathogenic"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$ColourDot=="Black"]<-"Uncertain"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$ColourDot=="Green"]<-"benign"

MitoFates = read.csv2('./dataset/MitoFates.csv',sep = ',')
#saveRDS(MitoFates,file ='MitoFates.RDS')
#MitoFates = readRDS('MitoFates.RDS')
MitoFates_1=MitoFates[,1:8]

GnomADVariants = read.csv2('./dataset/GnomADVariants.csv',sep = ',')
#save(GnomADVariants,file ='GnomADVariants.RDATA')
#GnomADVariants = readRDS('GnomADVariants.RDS')
head(GnomADVariants)

XYCG<- merge(XYcurves_ClinVarMutations, GnomADVariants, by.x=c('Genes','amini.acid'), by.y=c('Gene','amino.acid'), all = TRUE)

XYCG$join=paste(XYCG[,7],XYCG[,11], sep=":")


