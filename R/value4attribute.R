value4attribute <-
function(node, attribute, value="value") {
    for (i in seq(length = length(node)) ) {
        childNode <- node[[i]]
        nodeAttributes <- xmlAttrs(childNode)

        ## Maybe this is needed for complex Bruker microtof files
        ## if (any(grepl(attribute, nodeAttributes))) {

        if (grep(attribute, nodeAttributes)) {
          return(nodeAttributes[value])
        }
    }
    return(NULL)
}

