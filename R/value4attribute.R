value4attribute <-
function(node, attribute, value="value") {
    for (i in seq(length = length(node)) ) {
        childNode <- node[[i]]
        nodeAttributes <- xmlAttrs(childNode)

        if (grep(attribute, nodeAttributes)) {
            return(nodeAttributes[value])
        }
    }
    return(NULL)
}

