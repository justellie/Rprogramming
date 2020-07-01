rankhospital <- function(state, outcome,num="best") {
    
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
    
    #ord<-filtrada[with(filtrada,order(filtrada$failure,filtrada$name)),]

    #Renombro columnas por facilidad
    names(columnas_utiles)[1]<-"name"
    names(columnas_utiles)[2]<-"state"
    names(columnas_utiles)[3]<-"attack"
    names(columnas_utiles)[4]<-"failure"
    names(columnas_utiles)[5]<-"neumonia"
    
    #Obtengo causa de muerte
    index<-which(causa  %in% outcome)
    
    #Filtro por estado y por causa de muerte
    filtrada<-columnas_utiles[columnas_utiles$state==state ,c(1,index+2)]
    #Lo paso a numerico porque antes venia como caracter
    filtrada[,2]<-as.numeric(filtrada[,2])
    
    #
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
        resp<-ord[1,1]
    }
    else
    {
        if(num=="worst")
        {
            resp<-ord[which(ord[,2] %in% NA)[1]-1,1]
        }
        else
        {
            resp<-ord[num,1]    
        }
        
    }
    resp
    #source("best.R") para probar
}
