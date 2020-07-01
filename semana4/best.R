best <- function(state, outcome) {

    #lectura
    salida<-read.csv("outcome-of-care-measures.csv",na.strings ="Not Available", stringsAsFactors = FALSE)
    #Obtengo la listas de los estados y los ordeno
    estados<-c(as.character(unique( salida$State)))
    estados<-sort(estados)
    causa<-c("heart attack", "heart failure", "pneumonia")
    bandS<-state %in% estados
    bandC<-outcome %in% causa
    
    #Valido 
    if (!bandS) {
        stop( "invalid state")   
    }
    else
    {
        if (!bandC) 
        {
            stop( "invalid outcome")   
        }
    }
    
    #Elimino columnas que no usare
    columnas_utiles<-salida[,c(2,7,11,17,23)]
    
    
    
    #Renombro columnas por facilidad
    names(columnas_utiles)[1]<-"name"
    names(columnas_utiles)[2]<-"state"
    names(columnas_utiles)[3]<-"attack"
    names(columnas_utiles)[4]<-"failure"
    names(columnas_utiles)[5]<-"neumonia"
    
    #Obtengo causa de muerte
    index<-which(causa  %in% outcome)+2
    
    #Filtro por estado y por causa de muerte
    filtrada<-columnas_utiles[columnas_utiles$state==state ,c(1,index)]
    #Lo paso a numerico porque antes venia como caracter
    filtrada[,2]<-as.numeric(filtrada[,2])
    
    #Filtro el minimo 
    resultado<-c(as.character(filtrada[filtrada[,2]==min(filtrada[,2],na.rm=TRUE),1]))
    resultado<-sort(resultado)
    resultado[1]
    #source("best.R") para probar
}
