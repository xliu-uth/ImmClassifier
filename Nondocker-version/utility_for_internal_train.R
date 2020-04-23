

# generate mixed bits




cover_set <- function(leaves){
  cset <- c()

  for(leaf in leaves){


    for (node in strsplit(leaf, ";")[[1]]){

      elements <- strsplit(node, ":")[[1]]
      i <- length(elements)

      current <- elements[1]

      cset <- c(cset, current)
      while (i > 1){
        current <- elements[1]
        for(j in 2:i){

          current <- paste(current, elements[j], sep = ":")

          cset <- c(cset, current)
        }
        i <- i-1
      }




    }

  }
  return (cset)
}




# create child-parent-link

convert_to_bits <- function(nodes, ref.nodes){

  target.bits <- rep(0, length(ref.nodes))
  names(target.bits) <- ref.nodes


  for (node in strsplit(nodes, ";")[[1]]){

    elements <- strsplit(node, ":")[[1]]

    i <- 1
    current <- ""
    while(i <= length(elements)){

      if(i == 1){
        current <- paste0(current, elements[i])
      }else{
        current <- paste(current, elements[i], sep = ":")
      }
      target.bits[current] <- 1
      i <- i+1
    }

  }

  #print (paste0("return ",paste(target.bits, collapse = ",")))
  return(paste(target.bits, collapse = ","))
}

