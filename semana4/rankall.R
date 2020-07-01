rankall <- function(outcome, num = "best") {
    
    #lectura
    salida<-read.csv("outcome-of-care-measures.csv",na.strings ="Not Available", stringsAsFactors = FALSE)
    #Obtengo la listas de los estados y los ordeno
    estados<-c(as.character(unique( salida$State)))
    estados<-sort(estados)
    causa<-c("heart attack", "heart failure", "pneumonia")
    bandC<-outcome %in% causa
    
    #Valido 

        if (!bandC) 
        {
            stop( "invalid outcome")   
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
    index<-which(causa  %in% outcome)
    respuestas<-data.frame()
    for(state in estados)
    {
        #divido por estado
        partido<-split(columnas_utiles,columnas_utiles$state)
        #obtengo el estado actual
        filtrada<-partido[state]
        filtrada<-filtrada[[1]]
        
        #Lo paso a numerico porque antes venia como caracter

    
        #Filtro el minimo 
        if(index==1)
        {
            ord<-filtrada[with(filtrada,order(filtrada$attack,filtrada$name)),]   
        }
        else
        {
            if(index==2)
            {
                ord<-filtrada[with(filtrada,order(filtrada$failure,filtrada$name)),]
            }
            else
            {
                ord<-filtrada[with(filtrada,order(filtrada$neumonia,filtrada$name)),]
            }
        }
        if(num=="best")
        {
            resp<-ord[1,c(1,2)]
        }
        else
        {
            if(num=="worst")
            {
                resp<-ord[nrow(ord),c(1,2)]
            }
            else
            {
                resp<-ord[num,c(1,2)]    
            }
            
        }
        respuestas<-rbind(respuestas,data.frame(name=resp[1,1],state=resp[1,2]))
        #source("best.R") para probar
    }
    respuestas
}
