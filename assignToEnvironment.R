assignToEnvironment <- function(keys, envMatrix, environmentsName){
  
    for (i in 1: length(keys)){
      
      if(class(envMatrix) == "numeric"){
        assign(keys[i], envMatrix[i], envir = environmentsName)  
      }else{
        
    assign(keys[i], envMatrix[i, ], envir = environmentsName)
    }
    }
    
}