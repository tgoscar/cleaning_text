



install.packages("stringdist")
install.packages("cellranger")
install.packages("readxl")
install.packages("rlang")

library('stringdist')
library('cellranger')
library('rlang')
library('readxl')


rm(list = ls())
setwd("F:/string matching")


## Incorporamos la matriz

#c=read_excel("F:/analistas/otoledo/string matching/names.xlsx", sheet = 2)
#c=read_excel("F:/analistas/otoledo/string matching/names.xlsx", sheet = 1)
c=read_excel("F://string matching/names.xlsx",  col_names = "EMPRESAS", sheet = 1)
d=c
## Remover espacions en blanco que no quiero contar

for (i in c) {

  c=t(t(gsub(" ", "", i))) ##juntar los caracteres
  
}

n=nrow(c)

for (i in 1:n) {
  
    c[i,]=gsub("SOCIEDADANONIMACERRADA", "", c[i,], fixed = TRUE) # sacar los puntos de los strings
    c[i,]=gsub("SOCIEDADANONIMA", "", c[i,], fixed = TRUE) # sacar los puntos de los strings
    c[i,]=gsub(".", "", c[i,], fixed = TRUE) # sacar los puntos de los strings
  
}


m=matrix(0,n,n)

for (i in 1:n) {
  f=c[i,1]
  ncharf=nchar(f)
    
    for (j in 1:n) {
      r=c[j,1]
      ncharr=nchar(r)
      l=ifelse(nchar(f)>nchar(r), ncharf, ncharr)
      g=0
##Creamos una suma ponderada a los primeros caracteres que pesan mas para encotrar similitud        
          for (k in 1:l) {
            g=g+ifelse(substr(f, k, k)==substr(r, k, k),1,0)
            g=ifelse(k==3, ifelse(g==3,g,-5), g)
            #g=g+ifelse(substr(f, k, k)==substr(r, k, k),1,0)*(l-k+1)/l
            #g=g+ifelse(substr(f, k, k)==substr(r, k, k),1,0)*(ncharf+ncharr-k+1)/l
            
          }
      ##g=ifelse(i=j,0,g)
      #m[i,j]=ifelse(i==j,0,2*(g)/(nchar(f) + nchar(r))) #dividido total de caracteres
      m[i,j]=ifelse(i==j,0,g/min(ncharf, ncharr)) #dividido mínimo de caracteres
    }
}

mm=matrix(0,n,n)

## Definición del límite de similitud 85%
for (i in 1:n) {
  
  for (j in 1:n) {
    
    mm[i,j]=ifelse(m[i,j]>0.85,as.matrix(d[j,1]),0)
    
  }
}

mmm=matrix(0,n,n)

for (i in 1:n) {
  
mmm[i,]=as.matrix(sort(t(as.matrix(mm[i,])), decreasing=TRUE))
}

mmm=cbind(as.matrix(d), as.matrix(mmm))
mmm=mmm[rowSums(mmm[,2:n] != 0) != 0, ] 


write.csv(mmm, file = "Similitud.csv")
