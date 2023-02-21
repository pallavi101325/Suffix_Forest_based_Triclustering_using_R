form_dataframe <- function(df,rules){

first <- TRUE
for(r in keys(rules)){
    l1 <- list()
    for(i in r[["antecedent"]][[1]]){
        l1 <- append(l1,encoded_m[[i]])
    }
    ante <- toString(l1)
    l2 <- list()
    for(i in r[["consequent"]][[1]]){
        l2 <- append(l2,encoded_m[[i]])
    }
    ante <- toString(l1)
    conse <- toString(l2)
    con <- r[["confidence"]]
    lyft <- r[["lift"]]
    str <- ""
    for(p in keys(r[["object"]])){
        str <- paste(str, toString(p))
        str <- paste(str,"=")
        str <- paste(str, toString(r[["object"]][[p]][[1]]))
        str <- paste(str, "\n")
    }
     
    if(first){
    df <-  data.frame(Antecedent = c(ante), Consequent = c(conse), Confidence = c(con) , Lift = c(lyft), ObjectList = c(str))  
        first <- FALSE
    }
    else{
    df <-  df %>% add_row(Antecedent = ante, Consequent = conse, Confidence = con ,Lift = lyft, ObjectList = str)
    }
    
   
}
    return(df)
}

df_AR_E <-  data.frame() 
df_AR_E <- form_dataframe(df_AR_E,AR_E)
df_AR_SB <-  data.frame() 
df_AR_SB <- form_dataframe(df_AR_SB,AR_SB)
df_AR_PB <-  data.frame() 
df_AR_PB <- form_dataframe(df_AR_PB,AR_PB)
write.csv(df_AR_E, "AR_E.csv", row.names=FALSE)
write.csv(df_AR_PB, "AR_PB.csv", row.names=FALSE)
write.csv(df_AR_SB, "AR_SB.csv", row.names=FALSE)
