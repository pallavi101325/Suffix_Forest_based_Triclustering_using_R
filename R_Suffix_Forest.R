library(r2r)
library(rjson)

build <- function(suffix) {
  
  h_node <- hashmap()
  h_node[["item"]] <- suffix[[1]]
  
  h_node[["children"]] <- list()
  h_node[["leaf"]] <- list()
  
  if(length(suffix) == 2) {
    h_node[["leaf"]] <- append(h_node[["leaf"]], suffix[2])
  }
  else{
    len <- length(suffix)
    h_node[["children"]] <- append(h_node[["children"]], build(suffix[2:len]))
  }
  
  return(h_node)
}

match <- function(h_node, suffix) {
  if (length(suffix) == 2){
    if(length(h_node[["leaf"]]) == 0){
      h_node[["leaf"]]  <- list(suffix[2])
    }
    else {
      tree_leaf <- h_node[["leaf"]]
      ########################################
      state <- names(suffix[2])
      type <- suffix[[2]]
      #########################################
      if (state %in% names(tree_leaf)){
        x <- tree_leaf[[state]]
        if (type %in% x){
          print("Warning: Same row already exists in forest !")
        }
        else{
          tree_leaf[[state]] <- append(tree_leaf[[state]], type)
        }
      }
      else{
        tree_leaf[[state]] <- list(type)
      }
      
      h_node[["leaf"]] <- tree_leaf
    }
  }
  else{
    
    if(length(h_node[["children"]]) > 0){
      for (i in 1 : length(h_node[["children"]])){
        child <- h_node[["children"]][[i]]
        if (child[["item"]] == suffix[[2]]){
          t <- length(suffix)
          h_node[["children"]][[i]] <- match(child, suffix[2:t])
          return
        }
      }
    }
    
    len <- length(suffix)
    temp <- build(suffix[1:len])
    h_node[["children"]] <- append(h_node[["children"]], temp)
    
  }
  
  return(h_node)
  
}



get_suffix <- function(arr, idx, state, type) {
  suffix <- list()
  
  for(i in idx:length(arr)){
    suffix <- append(suffix, arr[i])
  }
  
  suffix[[state]] <- list(type)
  
  return (suffix)
}

get_all_suffix <- function(arr, state, type){
  all_suffix = list()
  
  for (i in 1 : length(arr)){
    suffix = get_suffix(arr, i, state, type)
    all_suffix <- append(all_suffix, list(suffix))
  }
  
  
  return (all_suffix)
}



sufix_forest_build <- function(name , SFD_List){
  h_tree <- hashmap()
  SFD <- SFD_List
  
  for(state in keys(SFD)) {
    arr <- SFD[[state]]
    suffixes <- get_all_suffix(arr, state, name)
    
    for(i in 1:length(suffixes)){
      suffix = suffixes[[i]]
      if(has_key(h_tree, suffix[[1]])) {
        res = match(h_tree[suffix[[1]]], suffix)
        insert(h_tree, suffix[[1]], res)
      }
      else {
        h_tree[[suffix[[1]]]] <- build(suffix)
      }
    }
  }
  
  return(h_tree)
}


SFD_mdm <- hashmap()
SFD_mdm[["AP"]] <- c(3, 14, 15, 16, 31)
SFD_mdm[["GU"]] <- c(1, 2, 3, 14, 15, 16, 31)
SFD_mdm[["OD"]] <- c(1, 2, 14, 15, 31)
SFD_mdm[["ANI"]]<- c(16, 31)


SFD_om = hashmap()

SFD_om[["AP"]]<- c(3, 4, 5, 14,15, 16, 31)
SFD_om[["MA"]] <- c(3)
SFD_om[["KE"]] <- c(2, 14, 15, 16, 31)
SFD_om[["WB"]] <- c(4, 5, 14, 15)
SFD_om[["ANI"]]  <- c(2)

forest <- sufix_forest_build("om",  SFD_om)
print(forest[[31]][["leaf"]])
for(l in keys(forest)){
  print(paste(l, "->" , forest[[l]]))

}
# save(l, file = "output_1.RData")
#jsonData <- toJSON(forest[[l]])
#write(jsonData,"output2.json")


