
#IMPORTANT, Set the working directory. The directory should be the one under which ENB_workshop is directly located.
setwd("~/Git_root")
library(data.table)
library(tm)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(scales)
library(magrittr)
library(stringdist)
library(R.utils)

#Call the pre-built harmonisation functions. These include both functions written by Peeter Tiinits for the harmonisation of the
#ENB as well as functions used to harmonise the same fields in Fennica by Leo Lahti
source("ENB_workshop/scripts/functions.R")

#Load the data sets containing Estonian National Bipliography

ENB_1 <-  read.csv("ENB_workshop/data/ERB_works_v_24_05_2019.tsv",sep="\t",header = TRUE) %>% .[,c("RecordNumber","koht","kirjastus","aeg","autor_name","autor_dates")]
ENB_1$RecordNumber <- paste(ENB_1$RecordNumber,"_1",sep="")

ENB_2 <- read.csv("ENB_workshop/data/ERB_muuk_works_v_24_05_2019.tsv",sep="\t",header = TRUE) %>% .[,c("RecordNumber","koht","kirjastus","aeg","autor_name","autor_dates")]
ENB_2$RecordNumber <- paste(ENB_2$RecordNumber,"_2",sep="")

ENB <- rbind.data.frame(ENB_1,ENB_2)

#ENB_place contains the data needed by the group that focuses on the publication place. The field to harmonise is koht.
ENB_place <- ENB[,c("RecordNumber","koht","aeg")] %>% unique.data.frame(.)

#ENB_publisher contains the data needed by the group that focuses on the publishers. The field to harmonise is kirjastus.
ENB_publisher <- ENB[,c("RecordNumber","kirjastus","aeg")] %>% unique.data.frame(.)

#ENB_author contains the data needed by the group that focuses on the authors. The fields to harmonise are autor_name and autor_dates
ENB_author <- ENB[,c("RecordNumber","autor_name","autor_dates","aeg")] %>% unique.data.frame(.) 
#small pre-harmonisation step to the author name field.
ENB_author$autor_name <- gsub(x=ENB_author$autor_name,pattern="\\$a",replacement="")
ENB_author$autor_dates[ENB_author$autor_dates==""] <- NA
ENB_author$autor_dates <- gsub(x=ENB_author$autor_dates,pattern="(\\$d[u]{0,1})|(\\?)",replacement = "")


