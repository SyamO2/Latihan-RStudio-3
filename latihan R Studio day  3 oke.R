library(tidyverse)
library(readxl)
library(ggpubr)
library(nortest)
library(agricolae)
library(DescTools)
library(car)

##..................Membaca Data......................##
padi<-read_excel('Latihan Faktorial.xlsx', sheet = 'RAL Faktorial')
padi
str(padi)
padi<-padi%>%
  mutate(A=as, factor(A),
         B=as, factor(B))
  
## Analisis Sidik Ragam
produksi<-aov(P1~A*B, data=padi)
summary(produksi)

## Uji Lanjut
perlakuanA<-duncan.test(produksi, 'A')
perlakuanA
perlakuanB<-duncan.test(produksi, 'B')
perlakuanB

perlakuanAB<-duncan.test(produksi, c('A','B'))
perlakuanAB

deviasi<-padi%>%
  group_by(AB)%>%
  summarise(sd=sd(P1))





perlakuanAB$groups%>%
  rownames_to_column('Perlakuan')%>%
  separate(Perlakuan, c('A','B'), sep=':')%>%
  left_join(deviasi, by=c('A'='A', 'B' = 'B'))%>%
  ggplot(aes(x=A, y=P1, fill = B))+
  geom_bar(stat = 'identity', position = position_dodge(), alpha=0.7)+
  geom_text(aes(label=groups), position = position_dodge(width = 0.9), vjust=-1)+
  geom_errorbar(aes(ymax = P1+sd, ymin =P1-sd), position = position_dodge(width = 0.9), width=0.5)+
  scale_y_continuous(limits = c(0,120))+
  theme_bw()+
  theme(legend.position = 'right')

padi
simpleB130<-padi[padi$B=='130',]
simpanova<-aov(P1~A, data=simpleB130)
summary(simpanova)


   
  