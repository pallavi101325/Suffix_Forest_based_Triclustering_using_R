form_patterns <- function(h_node){
  pattern_list <- hashset()
    #print("hi")
  if(length(h_node[["children"]]) > 0){
  for(child in keys(h_node[["children"]])){
    partial_patterns <- hashset()
    partial_patterns <- form_patterns(child)
     for(pattern in keys(partial_patterns)){
         if(!h_node[["item"]] %in% pattern[["itemset"]][[1]]){
             if(length(pattern[["itemset"]][[1]]) == 0){
                 #print("hi")
                 pattern[["itemset"]] <- list(h_node[["item"]])
             }
             else{
                 pattern[["itemset"]][[1]] <- append(pattern[["itemset"]][[1]] , h_node[["item"]])
             }
         }
         insert(pattern_list,pattern)
     }
    }
  }
  if(length(h_node[["leaf"]]) > 0){
      pp <- hashmap()
       #print("hi")
      pp[["itemset"]] <- list(h_node[["item"]])
      pp[["object"]] <- h_node[["leaf"]]
      insert(pattern_list,pp)
 
  }
  return (pattern_list)
}
is_object_subset <- function(p1,p2){
    for(key in keys(p1)){
        if(has_key(p2,key) == FALSE){
            return (FALSE)
        }
        else{
            for(value in p1[[key]][[1]]){
                if(!value %in% p2[[key]][[1]]){
                     return (FALSE)
               }
            }
        }
    }
    return (TRUE)
}

has_same_object <- function(p1,p2){
    return (is_object_subset(p1,p2) & is_object_subset(p2,p1))
}

find_unique_patterns <- function(all_patterns){
    unique_patterns <- hashset()
    for(pat in keys(all_patterns)){
        dupe_found <- FALSE
        for(upat in keys(unique_patterns)){
            if((setequal(pat[["itemset"]],upat[["itemset"]])==TRUE) & (has_same_object(pat[["object"]],upat[["object"]])==TRUE)){
                dupe_found <- TRUE
                break
            }
            
        }
        if(dupe_found == FALSE){
            insert(unique_patterns,pat)
        }
    }
    return (unique_patterns)
}
get_all_patterns <- function(h_tree){
    #print("gap")
  all_patterns <- hashset()
  for(key in keys(h_tree)){
    
     print("gap")
    some_patterns <- hashset()
    some_patterns <- form_patterns(h_tree[[key]])
    for(pattern in keys(some_patterns)){
          insert(all_patterns,pattern)
       }
  }
  return (find_unique_patterns(all_patterns))
}
is_object_subset <- function(p1,p2){
    for(key in keys(p1)){
        if(!has_key(p2,key)){
            return (FALSE)
        }
        else{
            for(value in p1[[key]][[1]]){
                if(!value %in% p2[[key]][[1]]){
                    return (FALSE)
                }
            }
        }
    }
    return (TRUE)
}

has_same_object <- function(p1,p2){
    return (is_object_subset(p1,p2) & is_object_subset(p2,p1))
}

is_closed <- function(P1,pattern_list){
    print("closed")
  for(pat in keys(pattern_list)){
    if((length(pat[["itemset"]][[1]]) > length(P1[["itemset"]][[1]])) & (has_same_object(pat[["object"]],P1[["object"]]))){
      if(all(P1[["itemset"]][[1]] %in% pat[["itemset"]][[1]])){
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

intersection <-function(p1,p2){
           l <- intersect(p1[["itemset"]],p2[["itemset"]])
  new_object <- p1[["object"]]
other_object <- p2[["object"]]
  for(key in keys(other_object)){
    if(has_key(new_object , key) ){
      for(e in other_object[[key]][[1]]){
        if(!e %in% new_object[[key]][[1]]){
          new_object[[key]][[1]] <- append(new_object[[key]][[1]],e)
        }
      }
    }
    else{
      new_object[[key]] <- other_object[[key]]
    }
  }
    new_pat <- hashmap()
    new_pat[["itemset"]] <- l
    new_pat[["object"]] <- new_object
    return (new_pat)
}

merge_leaf <- function(P1,leaf){
  for(state in keys(leaf)){
    if(!has_key(P1[["object"]] , state)){
      P1[["object"]][[state]] <- leaf[[state]]
    }
    else{
      for(type in leaf[[state]][[1]]){
        if(!type %in% P1[["object"]][[state]][[1]]){
          P1[["object"]][[state]][[1]] <- append(P1[["object"]][[state]][[1]],type)
        }
      }
    }
  }
  
}

library(r2r)
get_fcps <- function(h_tree){
    print("fcp")
  all_patterns<- hashset()
 all_patterns <- get_all_patterns(h_tree)
 FCP <- hashset()
 for(pattern in keys(all_patterns)){
   if(is_closed(pattern,all_patterns)){
       insert(FCP,pattern)
   }
 }
 print("FCP")
 new_may_exist <- TRUE
 while(new_may_exist == TRUE){
   NFCP <- hashset()
   modify_patterns <- hashset()
   for(pat1 in keys(FCP)){
     for(pat2 in keys(FCP)){
       if(!setequal(pat1[["itemset"]],pat2[["itemset"]])){
         pat <- intersection(pat1,pat2)
         if(length(pat[["itemset"]]) > 0 ){
           found_in_fcp <- FALSE
           for(i in keys(FCP)){
             if(setequal(i[["itemset"]],pat[["itemset"]])){
               found_in_fcp <- TRUE
               if(!is_object_subset(pat[["object"]],i[["object"]])){
                 key1 <- "idx"
                 value1 <- i
                 
                 key2 <- "leaf"
                 value2 <- pat[["object"]]
                 object <- hashmap()
                 # Build up key value pairs
                 object[[ key1 ]] <- value1
                 object[[ key2 ]] <- value2
                 insert(modify_patterns,object)
               }
               break
             }
           }
           if(found_in_fcp == FALSE){
             found_in_nfcp <- FALSE
             for(i in keys(NFCP)){
               if(setequal(i[["itemset"]] , pat[["itemset"]])){
                 merge_leaf(i,pat[["object"]])
                 found_in_nfcp <- TRUE
                 break
               }
             }
             if(!found_in_nfcp){
               #NFCP <- append(NFCP,pat)
                 insert(NFCP,pat)
             }
           }
         }
       }
     }
   }
 
 if((length(NFCP) + length(modify_patterns)) == 0){
   new_may_exist <- FALSE
 }
 else{
   for(patch in keys(modify_patterns)){
     merge_leaf(patch[["idx"]],patch[["leaf"]])
     delete(modify_patterns,patch)
   }
   for(pat in keys(NFCP)){
         insert(FCP,pat)
         delete(NFCP,pat)
   }
 }
}   
return (FCP) 
}
h2 <- hashset()
h2 <- get_fcps(forest)
