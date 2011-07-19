index4attribute <-
function(node, attribute) {
    for (i in seq(length = length(node)) ) {
        childNode <- node[[i]]
        nodeAttributes <- xmlAttrs(childNode)

        if (length(grep(attribute, nodeAttributes))>0) {
            return(i)
        }
    }
    return(NULL)
}

