#Para leer de acuerdo al patrón
leer <- function(data_mode, prefix) {
 nombre <- paste(prefix, "_", data_mode, ".txt", sep="")
  
 dir <- paste(parent_data_dir, data_mode, nombre, sep="/")
  
  return(read.table(dir))
}


#Combinar actividades 
combinar <- function(data_mode = "train") {
  
  # X data
  
  slctd <- leer(data_mode, "X")[, columns]
  
  # Y data
  
  act <- leer(data_mode, "y")
  
   
  act.val <- act.nombre[act[, 1]]
  
    
  subjects <- leer(data_mode, "subject")
  
  return(cbind(subjects, act.val, slctd))
}


#COmbinar training and testing data, realiza los calculos 
#y guarda en un archivo tidy.txt separado por tabulaciones

tidy.data <- function() {
  
  train <- combinar("train")
  
  test <- combinar("test")
  
  all.data <- rbind(train, test)
  
  colnames(all.data) <- c("subject", "activity", features.nombre)
  
  tidy.dat = aggregate(all.data[,3:68],by=list(all.data$activity, all.data$subject), FUN=mean)
  
  colnames(tidy.dat)[1:2] <-c('activity', 'subject')
  
  tidy.dat = tidy.dat[c(2, 1, 3:ncol(tidy.dat))]
  
  write.table(tidy.dat, file = "tidy.txt")
}


#PROGRAMA PRINCIPAL


parent_data_dir <- "H:/Usuarios/kflores/Desktop/a"

features <- read.table("H:/Usuarios/kflores/Desktop/a/features.txt", stringsAsFactors=F)[, 2]

columns <- grep("(mean\\(\\))|(std\\(\\))", features)

features.nombre <- features[columns]

act.nombre <- read.table("H:/Usuarios/kflores/Desktop/a/activity_labels.txt")[, 2]

tidy.data()
