#' Simulate NAs
#' Created by:
#' Created on: 2020/06/23
#'
#' Make a simulated MCAR
#' @param n_sample number of sample
#' @param n_nas number of NAs in sample
#' @param m_source Optional matrix soruce
#' @return n_sample matrix with n_nas values inside
#'
#' @examples
#' mat.sample <- sim.load.vect.df(100,10)
#'
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

#' get
#' funcion que dada un vector de matrices retorna la matriz n_mat
#' @param vet_mat: vector de matrices
#' @param n_mat: numero de matriz que se necesita
#' uso: > get(mat.data, 3), para obtener la matriz 3 del vector de matrices
#' Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
get <- function(vect_mat, n_mat){
  vect_mat[[ n_mat ]] # Forma de extraer una matriz del vector
}
#' fix2
#' funcion que muestra los datos de la matriz en vector de matrices como una tabla
#' @param vet_mat: vector de matrices
#' @param n_mat: numero de matriz que se va pintar en la tabla
#' uso: > fix2(mat.data, 3), para pinta la matriz 3 del vector de matrices
#' Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
fix2 <- function(vect_mat, n_mat){
  data.tmp <-  vect_mat[[n_mat]]  # extraccion temporal de la tabla
  fix(data.tmp) # pintar en la tabla
}
#' na.count
#' funcion que indica la cantidad de NAs en una matriz de un vector de matrices
#' @param vet_mat: vector de matrices
#' @param n_mat: numero de matriz para contarle los NA
#' uso: > na.count(mat.data, 3), Indica la cantidad de NAs en la matriz 3
#' Note: el vector debe tener mayor o igual cantidad de matrices que las solicitadas
na.count <- function(vect_mat, n_mat){
  data.tmp <- vect_mat[[ n_mat ]] # Extraccion de la matriz
  value <-as.numeric(is.na(data.tmp)) # Cuenta los NAs
  sum(value) ## Cantidad NAs total
}

############################################################################
### All rights are reserved by the authors.
### Authors:Andrés Ochoa; Jefferson Peña, Cristian Garcia, Kevin Mosquera
############################################################################