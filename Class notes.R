library(tidyverse)
library(Momocs)
library(ape)
library(phytools)
library(RRphylo)
library(vroom)


f <- list.files("class_out_data",pattern=".txt|.csv",full.names = TRUE)


out <- read_delim(f[1],delim="\t") %>% 
  as.matrix()


out %>% 
  list() %>% 
  Out() %>% #turns into Momocs outline
  coo_flipx() %>% #flip thing around x-axis
  stack() #visualize outlines - (can even do it on top of another)

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

outs.l %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#add wing info
out.df <- out.df %>% 
  mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
  na.omit()

#make list, outputs data frame
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

#extract wing info
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l)))

outs <-  outs.l %>% 
  Out(fac=list(wing=wings)) %>% 
  coo_flipx()

forewings <- outs %>% 
  filter(wing=="forewing")

hindwings <- outs %>% 
  filter(wing=="hindwing")

forewings %>% 
  stack()

