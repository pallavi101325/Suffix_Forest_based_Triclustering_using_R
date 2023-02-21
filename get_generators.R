is_subset <- function(l1,l2){
  if(all(l2 %in% l1)){
    return (TRUE)
  }
  return (FALSE)
}

func2 <- function(all_sets,items,n,data,index,i){
    if(index == n+1){
        #print("Hi")
        insert(all_sets,data)
        return (NULL)
    }
    if(as.integer(i) > length(items)){
        return(NULL)
    }
    data[index] <- items[i]
    func2(all_sets,items,n,data,index+1,i+1)
    func2(all_sets,items,n,data,index,i+1)  
}
get_all_subset <- function(items, n){
    all_sets <- hashset()
    
    data <- list()
    assign("list", NULL, envir = .GlobalEnv)
    func2(all_sets,items,n,data,1,1) 
    return (all_sets)
}

get_generators <- function(FCP){
    GEN <- hashset()
    cur_FCP <- hashset()
    i <- as.integer(1)
    while(length(cur_FCP) < length(FCP)){
        for(pat in keys(FCP)){
            if(length(pat[["itemset"]][[1]]) == i){
                  found_gen = FALSE
                  generator_size <- as.integer(1)
                  if(length(pat[["itemset"]]) > 0){
                      len  <- length(pat[["itemset"]][[1]])
                  }
                  else{
                      len <- as.integer(0)
                  }
                  while((generator_size < len) & found_gen == FALSE){
                      all_subsets <- hashset()
                      all_subsets <- get_all_subset(pat[["itemset"]][[1]], generator_size)
                        for(subset in keys(all_subsets)){
                            not_generator <- FALSE
                            for(generator in keys(GEN)){
                                if(setequal(generator[["itemset"]],subset)){
                                    #print("setequal")
                                    not_generator <- TRUE
                                    break
                                }
                            }
                            if(not_generator == FALSE){
                                print(paste("length_cur_FCP=",length(cur_FCP)))
                               for(pat2 in keys(cur_FCP)){
                                    if(is_subset(pat2[["itemset"]][[1]],subset)){
                                       not_generator <- TRUE
                                       break
                                    }
                                }
                             }
                            if(not_generator == FALSE){
                                temp <- hashmap()
                                temp[["itemset"]] <- subset
                                temp[["object"]] <- pat[["object"]]
                                temp[["FCP"]] <- hashmap()
                                temp[["FCP"]] <- pat
                                insert(GEN,temp)
                                found_gen <- TRUE
                            }
                       }
                        generator_size <- generator_size + 1
                       
                  }
                  insert(cur_FCP,pat)
                   
                  if(found_gen == FALSE){
                       temp2 <- hashmap()
                       temp2[["itemset"]] <- pat[["itemset"]]
                       temp2[["object"]] <- pat[["object"]]
                       temp2[["FCP"]] <- hashmap()
                       temp2[["FCP"]] <- pat
                       insert(GEN,temp2)
                       
                  }
              }
           
      } 
        i <- i+1
        
    }
    return (GEN)
}

gen_fcp <- hashset()
gen_fcp <- get_generators(h2)
length(gen_fcp)
