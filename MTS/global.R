# Dashboard v 0.1, 2021/08/18
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyverse)
library(NGLVieweR)
library(RColorBrewer)
library(faq)
library(stringr)
library(reshape2)



###FAQ###
faqdf <- data.frame(
  question = c("Q: Where are the structures from?", "Q: Where is the rest of the proteome?", "Q: Where are you getting the variants from? Which reference genome are you using?", "Q: What version of gnomAD are you using?", "Q: Where are your N-terminomics cleavage sites derived from?", "Q: What are some limitations of the database?", "Q: I found a bug or want one of my proteins added, who do I contact?"),
  answer = c("Structures are taken from AlphaFold, using the human proteome from 21 July 2021. We will update our viewer with the latest AlphaFold updates as they come out.",
             "In this version of MTSviewer, we are only taking proteins from the MitoCarta 3.0. In later versions, we hope to expand this viewer to the entire human proteome, as cytosolic proteins can also contain iMTS's worth studying.",
             "Variants are parsed by Uniprot ID through the dbNSFP v4.2a, and then filtered and matched to the VEP canonical transcript. All genomic coordinates correspond to GRCH38/hg38.",
             "v3.1, derived from dbNSFP v4.2a", 
             "Currently they are derived from Marshall et al. 2018 and Vaca Jacome et al. 2015. We recommend that for exhaustive resource N-terminomics data or raw mass spectrometry evidence, users should visit TopFIND at https://topfind.clip.msl.ubc.ca/",
             "The current version of MTSviewer features the inherent limitation that N-terminal MTS's within AlphaFold predictions are typically low confidence and are depicted as unstructured. In the future, structural determination of human MTS's in complexes with TOM/TIM and/or MPP will enable us to model N-MTS's more accurately, and could be integrated as a scoring metric or docking module into later versions of MTSviewer",
             "We appreciate all inquiries - upon publication we will set up a bug reporting / comment form, as the database is still being perfected.")
)

###Index###

#Gene_index = read.csv2('./Outputs/NP_Genename_index.csv',sep = ',')
#saveRDS(Gene_index,'./Outputs/NP_Genename_index.RDS')
Gene_index=readRDS('./Outputs/NP_Genename_index.RDS')

###reading XY curves to plot onto plotly###

#XY = read.csv2('./Outputs/iMLP_new.csv',sep = ',')
#saveRDS(XY,'./Outputs/iMLP_new.RDS')
XY = readRDS('./Outputs/iMLP_new.RDS')
XY_1<-melt(XY, id.vars=c('Genes'))
XY_1$amino.acid<-as.numeric(XY_1$variable)
XY_1$value<-as.numeric(XY_1$value)
XY_1<-XY_1[,-2]



###XY curves + Clinvar + GnomAD to plot points on plotly###

#XYcurves = read.csv2('./Outputs/iMLP_new.csv',sep = ',')
#saveRDS(XYcurves,'./Outputs/iMLP_new.RDS')
XYcurves = readRDS('./Outputs/iMLP_new.RDS')

XYcurves_1<-melt(XYcurves, id.vars=c('Genes'))
XYcurves_1$amini.acid<-as.numeric(XYcurves_1$variable)
XYcurves_1$value<-as.numeric(XYcurves_1$value)
XYcurves_1<-XYcurves_1[,-2]




###Read ClinVar Mutations####
#ClinVarMutations = read.csv2('./Outputs/Clinvar_SVM.csv',sep = ',')
#saveRDS(ClinVarMutations, './Outputs/Clinvar_SVM.RDS')
ClinVarMutations =readRDS('./Outputs/Clinvar_SVM.RDS')

#ClinVarMutations <- subset(ClinVar, select=c("X.chr","pos.1.based","aapos", "genename","HGVSc_VEP", "HGVSp_VEP", "MetaSVM_score","MetaSVM_pred","clinvar_clnsig","clinvar_trait"))


XYcurves_ClinVarMutations<-merge(XYcurves_1, ClinVarMutations, by.x=c('Genes','amini.acid'), by.y=c('genename','aapos'), sort = TRUE)
XYcurves_ClinVarMutations$label<-NULL

XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$clinvar_clnsig=="Pathogenic"]<- "Pathogenic"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$clinvar_clnsig=="Likely_pathogenic"]<- "Pathogenic"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$clinvar_clnsig=="Uncertain_significance"]<-"Uncertain"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$clinvar_clnsig=="Conflicting_interpretations_of_pathogenicity"]<-"Uncertain"
XYcurves_ClinVarMutations$label[XYcurves_ClinVarMutations$clinvar_clnsig=="Benign"]<-"Benign"

XYC = XYcurves_ClinVarMutations


###Read GnomAD Variants###
#GnomADVariants = read.csv2('./Outputs/GnomAD_SVM.csv',sep = ',')
#saveRDS(GnomADVariants,'./Outputs/GnomAD_SVM.RDS')
GnomADVariants = readRDS('./Outputs/GnomAD_SVM.RDS')
GnomADVariants$label <- "Uncertain"
XYG<-merge(XYcurves_1, GnomADVariants, by.x=c('Genes','amini.acid'), by.y=c('genename','aapos'), sort = TRUE)




###Read MPC score to plot on plotly for bar charts###

#MPC = read.csv2('./Outputs/Clinvar.csv',sep = ',')
#saveRDS(MPC,'./Outputs/Clinvar.RDS')
MPC = readRDS('./Outputs/Clinvar.RDS')

MPC_1<-melt(MPC, id.vars=c('genename','aapos'))
MPC_1$MPC_score<-as.numeric(MPC$MPC_score)

### Read MitoFates to output cleavage prediction in table###

#MitoFates = read.csv2('./Outputs/MitoFates.csv',sep = ',')
#saveRDS(MitoFates,'./Outputs/MitoFates.RDS')
MitoFates = readRDS('./Outputs/MitoFates.RDS')
MitoFates_proc <- merge(MitoFates, Gene_index, by.x=c('Header'), by.y=c('Header'), sort = TRUE)

###Read N-terminomics###
#N_term = read.csv2('./Outputs/N-terminomics.csv',sep = ',')
#saveRDS(N_term,'./Outputs/N-terminomics.RDS')
N_term = readRDS('./Outputs/N-terminomics.RDS')

N_term_filt <- subset(N_term, select=c("Gene.symbol", "Protein.name", "N.terminal.peptide.sequence", "P6.P6.", "Cleavage.site", "Cleaving.protease..if.known."))
### Read TargetP to output cleavage prediction in table###

#TargetP = read.csv2('./Outputs/TargetP.csv',sep = ',')
#saveRDS(TargetP,'./Outputs/TargetP.RDS')
TargetP = readRDS('./Outputs/TargetP.RDS')



#DeepMito = read.csv2('./Outputs/DeepMito.csv',sep = ',')
#saveRDS(DeepMito,'./Outputs/DeepMito.RDS')
DeepMito = readRDS('./Outputs/DeepMito.RDS')
DeepMito_proc <- merge (Gene_index, DeepMito, by.x=c('Header'), by.y=c('Header'), sort = TRUE)


