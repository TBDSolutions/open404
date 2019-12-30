
foo<-data404

foo<-data404%>%
     select_(.dots = checkSel(foo))
 
  
checkSel<-function(x){
  
  
  if('svc_grp' %in% names(x)){ 
    
    colnames<-x%>%
              select(-cases)%>%
              names()
    }else{
  
    colnames<-x%>%
            names()
    }
return(colnames)
}    
  
b<-checkSel(foo)



a<-checkSel(foo)
   
  
  
  
  
Gender <- '2'

person1 <- subset(foo,select=c(ifelse(Gender== 'x',-cases,-)))

library(eply)