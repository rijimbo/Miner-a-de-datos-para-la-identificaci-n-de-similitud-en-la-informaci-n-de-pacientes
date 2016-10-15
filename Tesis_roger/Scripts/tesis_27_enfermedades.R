library (lsa) # Se utiliza para utilizar las funciones de LSA y cosine. 
library (xlsx) # Se la utiliza para poder exportar las matrices en formato xlsx
library (plotly) # con esta libreria se integra la herramienta plotly para poder realizar muchas graficas


getwd() # muestra la ruta en donde nos encontremos
setwd("C:/Users/USUARIO/Dropbox/Tesis 4.1/Tesis 4.1/Tesis_roger/Scripts")
dir() # muestra  los archisvos que existen en el directorio que nos encontremos

#----------------------- Leer el CORPUS -------------------------------------
corpus <- read.csv("../Data/dataset_27_enfermedades.csv", header = T) # se coloca la direccion del dataset en formato .csv
#write.xlsx((corpus),'matriz termino-documento_27_enfermedades.xlsx') # exportar la matriz de documento-termino

MatrxCorpus <- data.matrix (corpus) #convertir a una matriz numerica

Transponer=t(MatrxCorpus)# t() es una Matriz Transpuesta, lo que siginifica que las enfermedades las ubica en donde van los pacientes y viceversa

# ----------------    COSENO ---------------------------------
coseno=cosine(t(MatrxCorpus[1:692, 5:31])) #Funcion coseno para  ver la similitud que tiene un paciente con otra en base a las enfermedades
#write.xlsx((coseno),'Coseno.xlsx')   # Exportar la matriz coseno

cosenoTerminos=cosine(MatrxCorpus[1:692, 5:31]) #Funcion coseno para ver la similitud de una enfermedad con otra en vase a las personas

#---------------------------------  LSA  ------------------------------------

myLSAspace<-lsa(MatrxCorpus[1:692, 5:31]) # se crea un espacio de Lsa que contiene 3 matrices 

tk<-myLSAspace$tk   # con "tk" obtenemos la primera matriz, esta contiene los términos (Pacientes) y valores a lo largo de cada dimensión
dk<-myLSAspace$dk   # con "dk" obtenemos la segunda matriz, constarán los documentos (Enfermedades) y sus valores a lo largo de cada dimensión.
sk<-matrix(myLSAspace$sk) # con "sk" obtenemos la tercera matriz contiene los valores singulares en orden descendente. 

#write.xlsx(as.matrix(tk),'Matrix tk_pacientes(27 enfer).xlsx')  # se exporta la matriz tk sobre los pacientes

freq = rowSums(dk[,1:2]) # se suma las dos primeras columnas de la matriz dk para optener la freceuncia de cada enfermedad
freq_df = data.frame(freq)# el resultado de cada frecuencia se pasa un matriz para apreciar mejor el resultado

as.character(MatrxCorpus[, 1]) # con esta linea se opttiene el numero de documento de cada paciente

as.matrix(MatrxCorpus[0, 5:31]) # con esta linea se optiene los nombres de las enfermedades que son las 27 columnas


# -------------------------- DISEÑO LSA ----------------------
# Grafica 1 Matriz t (Pacientes)

plot(tk[1:692, 1:2],  type="n", col="blue", xlab = "Eje x ", ylab = "Eje y", main = "Grafica Documentos(pacientes)")
text(tk[1:692, 1:2], as.character(MatrxCorpus[, 1]), col="blue")

# Grafica 2 Matriz d (Enfermedades) 

plot(dk[1:27, 1:2], type="n", col="blue", xlim = c(-0.7,0.20), ylim = c(-0.8,0.8), xlab = "Eje x ", ylab = "Eje y", main = "Grafica Terminos (Enfermedades)")
text(dk[1:27, 1:2], rownames(data.frame(dk[1:27,1:2 ])), col="red")

#grafica 3 Matriz t (Pacientes) con plotly

plotly1<-qplot(x=tk[1:692, 1] , y=tk[1:692, 2], colour=I('orange'), geom = "auto",main = "Matriz t (Pacientes)") # se crea una variable llamada plotly1 que guarda las 
                                                             # 2 columnas de la matriz t(personas) con todos sus datos
ggplotly(plotly1, tooltip = c('x','y','colour'))# Se crea la grafica de la matriz t(Pacientes) con plotly

#grafica 4 Matriz d (Enferedades) con plotly

plot_ly(x=dk[1:27, 1], y=dk[1:27, 2], mode = "markers")# Se crea una grafica de la matriz d (Enfermedades)
                      
# -------------------------------- DBSCAN ---------------------------

#library("dbscan") # permite utilizar funciones propias de dbscan
dbscan::kNNdistplot(tk[1:692, 1:2], k=10)#kNNdistplot usado para dibujar la trama k-distancia:
abline(h = 0.008, lty = 2)# con esta liniea se busca encontrar el punto en el que se produce un 
                          # cambio brusco a lo largo de la curva k-distancia.

cluster1 <- fpc::dbscan(tk[1:692, 1:2], 0.008, MinPts = 10) # creación de 5 clusters eps= 0.008 y MintPts =10 con la matriz de pacientes tk
cluster1  # imprimir los clsters creados

c1 <- cluster1$cluster  # guardar la infomacion de los clusters en c1

#write.xlsx((c1),'DBSCAN.xlsx') se exporta los clusters de DBSCAN.

View(c1)# visualizarel cluster c1 creado. 

#-------------------------- DISEÑO DBSCAN --------------------------

library("factoextra")# utilizada para graficar DBSCAN y Kmenas

# Grafica 1 matriz paciente con el figuras de los pacientes.
fviz_cluster(cluster1,tk[1:692, 1:2], stand = FALSE, frame = FALSE, geom = "point", title = "DBSCAN", outlier.shape = 3)

# Grafica 2 matriz paciente con los  ID de los pacientes.
fviz_cluster(cluster1,tk[1:692, 1:2], stand = FALSE, frame = FALSE, geom = "text", title = "DBSCAN", outlier.shape = 3)

# Grafica 3  matriz pacientes con el numero de documento de pacientes
color<- ifelse(c1==0, "black",ifelse(c1==1, "blue", ifelse(c1==2, "red", ifelse(c1==3, "yellow","green3")))) # 3 clusters eps= 0.01
par(bg = "8")# poner el fondo gris
plot(tk[1:692, 1:2], type = 'n', pch=19,  main="DBSCAN \n eps = 0.011  negro= 0, azul = 1, rojo = 2, amarillo=3, verde = 4 ",xlab = "Columna 1 Grupo Pacientes", ylab = " Columna 2 Grupo Pacientes")
text(tk[1:692, 1:2], as.character(MatrxCorpus[, 1]), col = color)


#------------------------------------- Kmeans---------------------------------

k.means<- kmeans((tk[1:692, 1:2]), centers = 4,  nstart = 5, iter.max = 5, trace=TRUE) # creacion de 4 clusters, nstr =5 con la matriz de pacientes tk

k.means$size # muestra el tamaño que tiene cada cluster

k.means2tabla<-table(tk[1:692, 1:1], k.means$cluster) # crea una matriz con los pacientes y los cluster obtenidos
#write.xlsx((k.means2tabla),'matrix de confucion.xlsx')   # Exportar la matriz de confucion de kmeans 

k.means$cluster # presenta el numero cluster otorgado a cada paciente
#write.xlsx((k.means$cluster),'kmeans.xlsx') $ se exporta los clusters de Kmenas

attributes(k.means) # muestra las distintos opcines que tiene el algorimo k-means
k.means$centers #  muestra los centros de cada cluster


#-------------------------------Diseño de Kmeans

# ---------------- Grafico # 1 Kmeanas con figuras--------------
fviz_cluster(k.means, tk[1:692, 1:2], frame = FALSE, title = "Kmeans", geom = "point")

# ---------------- Grafico # 2 Kmeanas con Id de pacientes--------------
fviz_cluster(k.means, tk[1:692, 1:2], frame = FALSE, title = "Kmeans", geom = "text")

# ---------------- Grafico # 3 Kmeanas con el numero de Documento de pacientes--------------
plot(tk[1:692, 1:2], type = 'n',  main="kmeans\n  K = 4",xlab = "Grupo Pacientes", ylab = "Grupo Pacientes")
text(tk[1:692, 1:2], as.character(MatrxCorpus[, 1]), col=k.means$cluster)
points(k.means$centers,cex=2, col=14, pch=19) # Representacion de los centros da cada grupo
# Representacion del centro de gravedad del conjunto de datOS
# colMeans: calcula el promedio de cada fila tan de x como de y para tener el centro delos datos
points(matrix(colMeans(tk[1:692, 1:2]), nrow = 1, ncol = 2), cex=3, col=15, pch=19)#

# ------------------Grafico  # 4 cluster de kmeans -----------------
library (cluster) # ¿ermite graficar clusters
clusplot((tk[1:692, 1:2]), k.means$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE,labels=1,lines=0)


