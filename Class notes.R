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
  Out() %>% #takes a list of matrices and makes each individual matrix into an outline
  coo_flipx() %>% #closed outlines, flips the outline about the x axis bc when looking at plots the origin is in the lower left but for image analysis covention is upper left
  stack() #stacks all the  outlines on one another

#add wing info
out.df <- out.df %>% 
  mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
  na.omit()

#make list, outputs data frame
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

#extract wing info
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l))) #info appended in file name, the "_" and ".txt" tells it to look for the name between those things in the file name

outs <-  outs.l %>% 
  Out(fac=list(wing=wings)) %>% #list with name in it, list of one dimensional vectors
  coo_flipx()

#separate out forewing and hindwings
forewings <- outs %>% 
  filter(wing=="forewing")

hindwings <- outs %>% 
  filter(wing=="hindwing")

#have outlines for each stored in different varibles, so we can look at them separately
forewings %>% 
  stack()

hindwings %>% 
  stack()

#procrustese transformation, make all data same size so that it is not what we focus on
#general procrustese superinposition make the mean of all the things and fit to that?
#full procrustese transformation because making them same size and overlaps

fore.min <- forewings %>% #minumum number of points in all our outlines
  coo_nb() %>% # #of points
  min() # minimum of it


forewings %>%
  coo_interpolate(fore.min) %>% #goes around outline, fits high res spline and picsk the same # of points in each outline (here min # of point in dataset)
  fgProcrustes() %>% #procrustese superinposition
  stack()

#same as prev but for hind
hind.min <- hindwings %>% 
  coo_nb() %>% 
  min()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_slide(id=1) %>% 
  coo_align()  %>%
  fgProcrustes() %>%
  stack()

#the little arrows represent the first point in the outline
#EFA - elliptical fourrier analysis
#normalization - should all the sizes be normalized to a similar scales? DO NOT WANT TO NORMALIZE bc we have elliptical shapes
forewings %>%
  coo_interpolate(fore.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE) #not normalizing the coefficient values

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE) #not normalizing the coefficient values

#performing PCA
forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  stack

#just plot the PCA
forewing.pca %>% 
  plot_PCA(title = "forewings")

hindwing.pca %>% 
  plot_PCA(title = "hindwings")

#comparative analysis
library(ape)

lep.tree <- ape::read.tree("lep_tree2.tre")

plot(lep.tree,cex=0.1)

lep.tree <- ladderize(lep.tree)
plot(lep.tree,cex=0.1)

lep.tree$tip.label <- gsub("_"," ",lep.tree$tip.label)

basename(names(outs))[1:5]
lep.sp <- read_csv("lep_image_data.csv")
head(lep.sp)
head(lep.sp$identifier)

#make a tibble with name of outlines data
out.data <- tibble(xy.file=basename(names(outs))) %>% 
  mutate(identifier=gsub("XY_|_hindwing|_forewing|.txt","",xy.file)) %>% 
  left_join(lep.sp)

head(out.data)
head(hindwing.pca$x,1)
head(forewing.pca$x,1)
hindwing.pca2 <-  tibble(xy.file=basename(rownames(hindwing.pca$x)),PC1=hindwing.pca$x[,1],PC2=hindwing.pca$x[,2]) %>% 
  left_join(out.data)
forewing.pca2 <-  tibble(xy.file=basename(rownames(forewing.pca$x)),PC1=forewing.pca$x[,1],PC2=forewing.pca$x[,2])%>% 
  left_join(out.data)

#evolutionary rates
#run this code to make second tree that just includes the species that are in PCA analysis
drops <- lep.tree$tip.label[!lep.tree$tip.label%in%unique(out.data$species)]

lep.tree2 <- drop.tip(lep.tree,drops)

plot(lep.tree2,cex=0.5)

#storing forewing and hindwing PCAs as variables
#Make sure tree only includes species name in the data but have to make sure species names data only inclides data that is in the treee
#PC1s
hind.pc1 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull

names(hind.pc1) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)

fore.pc1 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(PC1)

names(fore.pc1) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)

#PC2s
hind.pc2 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)

names(hind.pc2) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>% 
  pull(species)

fore.pc2 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)

names(fore.pc2) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(species)

#^examples of analysiing overall ratezs, and shift in rates, for just PC1 but need to analyse both PC1 & PC2

#multiply PC score by 10, just make pcs scores larger bc function has an issue with small numbers
forePC1.BM<-brownie.lite(lep.tree2,fore.pc1*10)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc1*10)

forePC2.BM<-brownie.lite(lep.tree2,fore.pc2*10)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc2*10)

#retreive estimated single rate, wont mean anything of not compared to smt
forePC1.BM$sig2.single # extracting for any one PC for any one wing, use this to compare between wings
#will divide fore and hind by each other to see with has the highest rate of evolution

#Shifts in evolutionary rates

hindPC1.RR <- RRphylo(tree=lep.tree2,y=hind.pc1) 
hindPC1.RR$rates

hindPC1.SS<- search.shift(RR=hindPC1.RR,status.type="clade")

hindPC1.SS$single.clades

plot(lep.tree2)
nodelabels(node = as.numeric(rownames(hindPC1.SS$single.clades)),text = rownames(hindPC1.SS$single.clades))

hindPC1.plot <- plotShift(RR=hindPC1.RR,SS=hindPC1.SS)

forePC1.plot <- plotShift(RR=hindPC1.RR,SS=hindPC1.SS)
hindPC1.plot$plotClades()

#below was run once
# if (!require("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")

#BiocManager::install("ggtree")

library(ggtree)
library(wesanderson)

plot_SS <- function(tre=NULL,SS=NULL,tax=NULL){
  
  
  nodes <- as.numeric(rownames(SS$single.clades))
  
  pal <- wes_palette("Zissou1",n=length(nodes))
  sp <- list()
  for(i in nodes){
    sp.i <- extract.clade(tre,i)$tip.label
    
    #print(head(tax))
    sub.names <- lapply(tax,function(x) x[x%in%sp.i]) 
    
    in.clades <- lapply(sub.names,function(x) length(x)>0) 
    all.of.clade <- lapply(sub.names,function(x) all(sapply(sp.i,function(z) z%in%x))) 
    
    high.clade <- names(sub.names)[last(which(all.of.clade==T))]
    all.clades <- names(sub.names)[which(in.clades==T)]
    crown <- ""
    if(high.clade!=last(names(sub.names))) crown <- "crown-"
    
    sub.clades <- NULL
    if(length(grepl("oidea",all.clades))>0) sub.clades <- all.clades[grepl("oidea",all.clades)]
    
    high.clade2 <- paste0(crown,high.clade,": ",paste0(sub.clades,collapse = "+"))
    sp[[paste0(i)]] <- tibble(n=i,species=sp.i,clade=high.clade2)
    
  }
  
  
  d <- do.call(rbind,sp)%>% 
    rename(label=species) 
  
  d2<- d %>% rename(clade_name=clade) 
  
  p <- ggtree(tre)+ scale_y_reverse()
  
  p$data <- p$data %>% left_join(d) %>% left_join(tibble(node=nodes,SS$single.clades) %>% mutate(shift=ifelse(rate.difference>0,"+","-")))
  
  p <-  p+geom_tiplab(aes(col=clade),geom="text",size=1.2)+
    geom_cladelab(data=d2,mapping=aes(node=n,col=clade_name,label=clade_name),offset=1,size=1.5)+
    geom_hilight(data=d2,mapping = aes(node = n,fill=clade_name),alpha = 0.01)+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)+
    theme(legend.position = "none")+geom_nodepoint(mapping=aes(subset = shift =="-"), size=5, shape=25,fill='blue',color='blue',alpha=0.7)+
    geom_nodepoint(mapping=aes(subset = shift =="+"), size=5, shape=24, fill='red',color='red',alpha=0.7)
  p <- p+xlim(NA,6)
  res <- tibble(n=nodes,SS$single.clades) %>% left_join(d %>% select(n,clade) %>% unique)
  
  return(list(plot=p,res=res))
  
}
tax.names <- readRDS("Lep_classification.RDS")

hindPC1.res <- plot_SS(lep.tree2,hindPC1.SS,tax = tax.names)

hindPC1.res$plot

hindPC1.res$res



#Shape evolution correlation

hindPC1.pic <- pic(hind.pc1,phy = lep.tree2)
forePC1.pic <- pic(fore.pc1,phy = lep.tree2)

PC1.pic <- tibble(
  hind=hindPC1.pic,
  fore=forePC1.pic
)

PC1.pic %>% 
  ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")

summary(lm(hind~fore,PC1.pic))
