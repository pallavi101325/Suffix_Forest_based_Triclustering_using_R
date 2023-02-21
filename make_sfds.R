make_sfds <- function(months_map,itemList_2015){
    all_SFDs <- hashmap()
 for(key in keys(months_map)){
     itemList_x <- filter(itemList_2015,Date == key)
     sfd_x <- hashmap()
     for(i in 1:nrow(itemList_x)) { 
        x <- as.character(itemList_x[i,1])
        l <- lapply(strsplit(as.character(itemList_x[i,3]), ","), as.numeric);
        vec <- unlist(l)
         if(length(vec) > 2){
           sfd_x[[x]] <- vec
         }   
        
     }
     all_SFDs[[months_map[[key]]]] <- sfd_x 
         
 }
    return (all_SFDs)
}
SFDS <- hashmap()
SFDS <- make_sfds(months_map,itemList_2015)
length(SFDS)
dataset_size <- as.integer(0)
for(k in keys(SFDS)){
    print(length(SFDS[[k]]))
    dataset_size <- dataset_size + length(SFDS[[k]])
}
