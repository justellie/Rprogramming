pollutantmean<- function(directory,pollutant,id=1:332)
{
    #Inicializo vector
    v<-numeric()
    #Itero sobre los id
    for (i in id)
    {   #Este condicional formatea los 0 en caso de ser necesario y pasa la dirección
        if(i<10)
        {
            path<-paste(directory,"/00",i,".csv",sep='')
        }
        else
        {
            if(i<100)
            {
                path<-paste(directory,"/0",i,".csv",sep='')
            }
            else
            {
                path<-paste(directory,"/",i,".csv",sep='')    
            }
        }
        #Leo el arrchivo y obtengo las columnas necesarias
        df<-read.csv(path)
        v<-c(v,df[,pollutant])
        
    } 
    #Devuelvo la media
    mean(v,na.rm=TRUE)
 
}

