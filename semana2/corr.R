corr<- function(directory,threshold=0)
{
    #Inicializo vector
    
    correlacion<-numeric()
    
    #Itero sobre los id
    for (i in 1:332)
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
        mask<-complete.cases(df)
        filas<-dim(df[mask,])[1]
        df<-df[mask,]
        if(filas>threshold)
        {
            resultado<-cor(df$nitrate,df$sulfate)
            correlacion<-c(correlacion,resultado)
        }
    }
    #Devuelvo vector
    correlacion
}