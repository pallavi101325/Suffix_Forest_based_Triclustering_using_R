install.packages("sloop")
library(sloop)

key1 <- "Age"
value1 <- 21

key2 <- "Name"
value2 <- "Pulkit"
object <- list()
# Build up key value pairs
object[[ key1 ]] <- value1
object[[ key2 ]] <- value2 

ll <- list(1,2,3,4)

Pattern <- function(it, ob) {
  
  P1 <- list(itemset = it ,object = ob)
  class(P1) <- "Pattern"
}
Pattern(ll,object)

P1

eq.Pattern <- function(P1, P2){
  if(length(P1$itemset) != length(P2$itemset)){
    return (FALSE)
  }
  if(P1$itemset != P2$itemset){
    return (FALSE)
  }
  if(P1$object != P2$object){
    return (FALSE)
  }
  return (TRUE)
}

add_item.Pattern <- function(P1, item){
  P1$itemset <- append(P1$itemset, item)
}

size.Pattern <- function(P1){
  return (length(P1$itemset))
}

is_ob_subset.Pattern(object1, object2){
  for ( state in names(object1)){
    if (has_key(object2,state)){
      return (FALSE)
    }
    else{
      for(type in object1[[state]]){
        if(!type %in% object2[[state]]){
          return (FALSE)
        }
      }
    }
  }
  return (TRUE)
}
has_same_object(P1, P2){
  return (is_ob_subset(p1$object,P2$object) & is_ob_subset(P2$object, P1$object))
}

is_closed.Pattern <- function(P1,pattern_list){
  for( pat in seq_along(pattern_list)){
    if(length(pat) > size(P1) & has_same_object(pat,P1)){
      if(all(P1$itemset %in% pat$itemset)){
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

intersection.Pattern <- function(P1,P2){
  l <- intersect(P1$itemset,P2$itemset)
  new_object = get_object(P1)
  other_object = get_object(P2)
  for(key in names(other_object)){
    if(has_key(new_object , key) ){
      for(e in other_object[[key]]){
        if(!e %in% new_object[[key]]){
          new_object[[key]] <- append(new_object[[key]],e)
        }
      }
    }
    else{
      new_object[[key]] <- other_object[[key]]
    }
  }
  return (Pattern(l,new_object))
}



merge_leaf.Pattern <- function(P1,leaf){
  for(state in names(leaf)){
    if(!has_key(P1$object , state)){
      P1$object[[state]] <- leaf[[state]]
    }
    else{
      for(type in leaf[[state]]){
        if(!type %in% P1$object[[state]]){
          P1$object[[state]] <- append(P1$object[[state]],type)
        }
      }
    }
  }
  
}




