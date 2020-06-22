

### Autores: Andrés Ochoa; Jefferson Peña, Cristian Garcia, Kevin Mosquera

library(FactoMineR) ## Realiza un afm con datos completos
library(missMDA)  ## Realiza un afm con datos faltantes

read.table("coffe_arabica_robusta_June_2020.csv",header=T,sep=";",
dec=".",row.names=1) -> dat.act

summary(dat.act)
dim(dat.act)

## AFM con datos completos y variables suplemenatarias
res.MFA.cafe <- MFA(dat.act[-1340,],group=c(2,6,3,2),type=c("n","s","s","s"),
name=c("origen","sabor","presentacion.copa","juicio"),
num.group.sup=1)

##### generación aleatoria de NAs
#### 5%

load.vect.df <- function(n_sample, n_nas, m_source = NULL){
  if(!is.null(m_source)){ 
    dat.act <- as.matrix(m_source) # Conversion de dataframe a matriz 
  }else{ dat.act <- as.matrix(dat.act) }
  ff <- seq(1, ncol(dat.act)*nrow(dat.act)) ## Posiciones matriz
  vec.mat <- vector(mode = "list", length = n_sample) 
  for (i in 1:n_sample) { # va desde 1 hasta el numero de matrices n_sample
    dat.mat.temp <- dat.act # Copia la matriz original
    pos.to.nas <- sample(ff, n_nas) # posiciones aleatorias para los NAs
    for (k in 1:n_nas){ # Va desde 1 hasta el numero de NAs
      dat.mat.temp[ pos.to.nas[k] ] <- NA # modifica las n_nas posiciiones en la matriz
    }
    vec.mat[[i]] <- data.frame(dat.mat.temp) # Graba las matrices con los NAs en el Vector
  }
  vec.mat # Retorna el Vector con todas las matrices
}

## funcion que dada un vector de matrices retorna la matriz n_mat
# param vet_mat: vector de matrices
# param n_mat: numero de matriz que se necesita
# uso: > get(mat.data, 3), para obtener la matriz 3 del vector de matrices
# Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
get <- function(vect_mat, n_mat){
  vect_mat[[ n_mat ]] # Forma de extraer una matriz del vector
}
## funcion que muestra los datos de la matriz en vector de matrices como una tabla
# param vet_mat: vector de matrices
# param n_mat: numero de matriz que se va pintar en la tabla
# uso: > fix2(mat.data, 3), para pinta la matriz 3 del vector de matrices
# Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
fix2 <- function(vect_mat, n_mat){
  data.tmp <-  vect_mat[[n_mat]]  # extraccion temporal de la tabla
  fix(data.tmp) # pintar en la tabla
}
## funcion que indica la cantidad de NAs en una matriz de un vector de matrices
# param vet_mat: vector de matrices
# param n_mat: numero de matriz para contarle los NA
# uso: > na.count(mat.data, 3), Indica la cantidad de NAs en la matriz 3
# Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
na.count <- function(vect_mat, n_mat){
  data.tmp <- vect_mat[[ n_mat ]] # Extraccion de la matriz 
  value <-as.numeric(is.na(data.tmp)) # Cuenta los NAs
  sum(value) ## Cantidad NAs total 
}


######## Estudio con el 5% na #########

##Generación de la matrix con el 5% de NA
vect.df1.na5<- load.vect.df(1, 737,xx) ## 1 Matrices con 5% NAs

## vizualización de la matrix
rr  <- data.frame(vect.df1.na5) ; fix(rr)

## imputación de la matrix
res.imputeMFA.5 <- imputeMFA(data.frame(vect.df1.na5),group=c(6,3,2),type=c("s","s","s"),
method="EM")


## se junta la matrix imputada con la parte cualitativa suplementaria
new.xx.5na <- cbind(dat.act[-1340,1:2],res.imputeMFA.5$completeObs)

## realización del afm con la parte cualitativa suplementaria y la parte estimada con el
## método de imputación

res.MFA.5na.complete <- MFA(new.xx.5na,group=c(2,6,3,2),type=c("n","s","s","s"),
name=c("origen","sabor","presentacion.copa","juicio"),
num.group.sup=1)


--------------------------------------------------------------------------------------------
#### Estudio con el 10%

### 1339*11
### 1473

## generación de la matriz
vect.df1.na10<- load.vect.df(1, 1473,xx) ## 1 Matrices con 10% NAs

## imputación
res.imputeMFA.10 <- imputeMFA(data.frame(vect.df1.na10),group=c(6,3,2),type=c("s","s","s"),
method="EM")

## se junta con la parte cualitativa
new.xx.10na <- cbind(dat.act[-1340,1:2],res.imputeMFA.10$completeObs)

## nuevo  afm con la parte estimada
res.MFA.10na.complete <- MFA(new.xx.10na,group=c(2,6,3,2),type=c("n","s","s","s"),
name=c("origen","sabor","presentacion.copa","juicio"),
num.group.sup=1)




-------------------------------------------------------------------------------------
#### Estudio con el 20%

### 1339*11*0.2
### 2946

## generación de la matriz
vect.df1.na20<- load.vect.df(1, 2946,xx) ## 1 Matrices con 20% NAs

## imputación
res.imputeMFA.20 <- imputeMFA(data.frame(vect.df1.na20),group=c(6,3,2),type=c("s","s","s"),
method="EM")

## se junta con la parte cualitativa
new.xx.20na <- cbind(dat.act[-1340,1:2],res.imputeMFA.20$completeObs)

## nuevo  afm con la parte estimada
res.MFA.20na.complete <- MFA(new.xx.20na,group=c(2,6,3,2),type=c("n","s","s","s"),
name=c("origen","sabor","presentacion.copa","juicio"),
num.group.sup=1)


--------------------------------------------------------------------------------------------

### Estudio con el 30% de NA
### 1339*11*0.3
### 4419

## generación de la matriz
vect.df1.na30<- load.vect.df(1, 4419,xx) ## 1 Matrices con 10% NAs

## imputación
res.imputeMFA.30 <- imputeMFA(data.frame(vect.df1.na30),group=c(6,3,2),type=c("s","s","s"),
method="EM")

## se junta con la parte cualitativa
new.xx.30na <- cbind(dat.act[-1340,1:2],res.imputeMFA.30$completeObs)

## nuevo  afm con la parte estimada
res.MFA.30na.complete <- MFA(new.xx.30na,group=c(2,6,3,2),type=c("n","s","s","s"),
name=c("origen","sabor","presentacion.copa","juicio"),
num.group.sup=1)




########### Comparación del método en función del porcentaje de na ###########################

### comparacion del porcentaje de inercia

y <- c(res.MFA.5na.complete$eig[2,3],
res.MFA.10na.complete$eig[2,3],
res.MFA.20na.complete$eig[2,3],
res.MFA.30na.complete$eig[2,3])


plot(c(5,10,20,30),y,xlab="porcentaje de NA",
ylab="porcentaje de inercia",ylim=c(50,100))

abline(h=res.MFA.cafe$eig[2,3],col="blue")


##### comparación en correlaciones primer eje  ### individuos

abs(cor(res.MFA.5na.complete$global.pca$ind$coord[,1],res.MFA.cafe$global.pca$ind$coord[,1]))

abs(cor(res.MFA.10na.complete$global.pca$ind$coord[,1],res.MFA.cafe$global.pca$ind$coord[,1]))

abs(cor(res.MFA.20na.complete$global.pca$ind$coord[,1],res.MFA.cafe$global.pca$ind$coord[,1]))

abs(cor(res.MFA.30na.complete$global.pca$ind$coord[,1],res.MFA.cafe$global.pca$ind$coord[,1]))


##### comparación en correlaciones segundo eje ## individuos

abs(cor(res.MFA.5na.complete$global.pca$ind$coord[,2],res.MFA.cafe$global.pca$ind$coord[,2]))

abs(cor(res.MFA.10na.complete$global.pca$ind$coord[,2],res.MFA.cafe$global.pca$ind$coord[,2]))

abs(cor(res.MFA.20na.complete$global.pca$ind$coord[,2],res.MFA.cafe$global.pca$ind$coord[,2]))

abs(cor(res.MFA.30na.complete$global.pca$ind$coord[,2],res.MFA.cafe$global.pca$ind$coord[,2]))

