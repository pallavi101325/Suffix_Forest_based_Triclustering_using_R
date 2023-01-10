


get_fcps <- function(h_tree){
 all_patterns <- get_all_patterns(h_tree)
 FCP <- list()
 for(pattern in all_patterns){
   if(is_closed.Pattern(pattern,all_patterns)){
     FCP <- append(FCP,pattern)
   }
 }
 new_may_exist = TRUE
 while(new_may_exist == TRUE){
   NFCP <- list()
   modify_patterns <- list()
   for(pat1 in FCP){
     for(pat2 in FCP){
       if(get_itemset.Pattern(pat1) != get_itemset.Pattern(pat2)){
         pat <- intersection.Pattern(pat1,pat2)
         if(size.Pattern(pat) > 0 ){
           found_in_fcp = FALSE
           for(i in FCP){
             if(get_itemset.Pattern(i) == get_itemset.Pattern(pat)){
               found_in_fcp = TRUE
               if(!is_object_subset.Pattern(get_object.Pattern(pat),get_object.Pattern(i))){
                 key1 <- "idx"
                 value1 <- i
                 
                 key2 <- "leaf"
                 value2 <- get_object.Pattern(i)
                 object <- list()
                 # Build up key value pairs
                 object[[ key1 ]] <- value1
                 object[[ key2 ]] <- value2
                 modify_patterns <- append(modify_patterns,object)
               }
               break
             }
           }
           if(found_in_fcp == FALSE){
             found_in_nfcp = FALSE
             for(i in NFCP){
               if(get_itemset.Pattern(i) == get_itemset(pat)){
                 merge_leaf.Pattern(i,get_object.Pattern(pat))
                 found_in_nfcp = TRUE
                 break
               }
             }
             if(!found_in_nfcp){
               NFCP <- append(NFCP,pat)
             }
           }
         }
       }
     }
   }
 }
 if(length(NFCP) + length(modify_patters) == 0){
   new_may_exist = FALSE
 }
 else{
   for(patch in modify_patterns){
     marge_leaf.Pattern(patch[["idx"]],patch[["leaf"]])
   }
   for(pat in NFCP){
     FCP <- append(FCP,pat)
   }
 }
return (FCP) 
}

form_patterns <- function(h_node){
  pattern_list <- list()
  for(child in h_node[["children"]]){
    partial_patterns <- form_patterns(child)
    for(pattern in partial_patterns){
      pattern.add_item(h_node[["item"]])
      pattern_list <- append(pattern_list,pattern)
      
    }
    }
  if(length(h_node[["leaf"]]) != 0){
      pattern_list <- append(pattern_list, Pattern(h_node[["item"]], h_node[["leaf"]]))
   }
  return (pattern_list)
}

get_all_patterns <- function(h_tree){
  all_patterns <- list()
  for(key in names(h_tree)){
    some_patterns <- form_patterns(h_tree[[key]])
    for(pattern in some_patterns){
        all_patterns <- append(all_patterns , pattern)
      }
        
    }
  return (all_patterns)
}



patterns = get_all_patterns(h_tree)
for (pattern in patterns){
  print (pattern) 
}
  



t3 <- hashmap()
t3[["item"]] <- 2
t3[["leaf"]] <- list()
t3[["children"]] <- list()

f <- list()
f[["AP"]] <- "MDM"
f[["AP"]] <- append(f[["AP"]] , "OM")

t3[["leaf"]] <- f

t2 <- hashmap()
t2[["item"]] <- 3
t2[["leaf"]] <- list()
t2[["children"]] <- list()

g <- list()
g[["AP"]] <- "OM"
g[["AP"]] <- append(g[["AP"]],"MDM")
t2[["leaf"]] <- g

t1 <- hashmap()
t1[["item"]] <- 1
t1[["leaf"]] <- NULL
t1[["children"]] <- list()
t1[["children"]] <- append(t1[["children"]] , t2)
t1[["children"]] <- append(t1[["children"]], t3)
h_tree <- hashmap()

h_tree[[1]] <- t1

get_fcps(h_tree)

print(ls(h_tree))

