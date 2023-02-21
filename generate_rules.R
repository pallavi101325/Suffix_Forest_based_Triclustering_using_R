get_support_count <-function(object){
    count <- as.double(0)
    for(k in keys(object)){
        count <- count + as.double(length(object[[k]][[1]]))
    }
    return (count)    
 }
find_support_count <- function(consequent , fcps){
    
     max_count <- as.integer(0)
        for(pattern in keys(fcps)){
            sup_count <- get_support_count(pattern)
            if (sup_count > max_count & is_subset(pattern[["itemset"]] , consequent)){
                max_count <- sup_count
            }
        }        
    return (max_count)    
}

generate_rules <- function(GEN, FCP , AR_E , AR_SB , AR_PB){
        
       for(g in keys(GEN)){
           for(f in keys(FCP)){
               if(setequal( g[["FCP"]][["itemset"]][[1]] , f[["itemset"]][[1]])){

                   if(!setequal(g[["itemset"]][[1]] , f[["itemset"]][[1]])){
                       rule <- hashmap()
                       rule[["antecedent"]] <- g[["itemset"]]
                       rule[["consequent"]] <- setdiff(f[["itemset"]] , g[["itemset"]])
                       rule[["lift"]] <- (get_support_count(f[["object"]])*dataset_size)/(get_support_count(g[["object"]])*find_support_count( rule[["consequent"]] , FCP))
                       rule[["object"]] <- f[["object"]]
                       rule[["confidence"]] <- as.integer(1.0)
                       insert(AR_E,rule)
                   }
               }
               else{
                   if(is_subset(f[["itemset"]][[1]],g[["FCP"]][["itemset"]][[1]])){
                    
                       confidence <- as.double(get_support_count(f[["object"]]))/as.double(get_support_count(g[["FCP"]][["object"]]))
                       rule <- hashmap()
                       rule[["antecedent"]] <- g[["itemset"]]
                       rule[["consequent"]] <- setdiff(f[["itemset"]] , g[["itemset"]])
                       rule[["lift"]] <- (get_support_count(f[["object"]])*dataset_size)/(get_support_count(g[["object"]])*find_support_count( rule[["consequent"]] , FCP))
                       rule[["object"]] <- f[["object"]]
                       rule[["confidence"]] <- as.double(confidence)
                       insert(AR_SB,rule)
                       
                   }
               }
           }
       }
     for(f1 in keys(FCP)){
         for(f2 in keys(FCP)){
             if((length(f1[["itemset"]])>0) & (length(f2[["itemset"]])>0) & (length(f1[["itemset"]][[1]]) < length(f2[["itemset"]][[1]])) & (is_subset(f2[["itemset"]][[1]],f1[["itemset"]][[1]]))){
    
                 confidence <- as.double(get_support_count(f2[["object"]]))/as.double(get_support_count(f1[["object"]]))
                       rule <- hashmap()
                       rule[["antecedent"]] <- f1[["itemset"]]
                       rule[["consequent"]] <- setdiff(f2[["itemset"]],f1[["itemset"]])
                       rule[["lift"]] <- get_support_count(f2[["object"]])*dataset_size/(get_support_count(f1[["object"]])*find_support_count( rule[["consequent"]], FCP))
                       rule[["object"]] <- f2[["object"]]
                       rule[["confidence"]] <- as.double(confidence)
                       insert(AR_PB,rule) 
             }
         }
     }
}
AR_E <- hashset()
AR_SB <- hashset()
AR_PB <- hashset()
generate_rules(gen_fcp,h2,AR_E,AR_SB,AR_PB)
print(length(AR_E))
print(length(AR_SB))
print(length(AR_PB))
