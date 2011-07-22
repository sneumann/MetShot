rectIntersectList <-
function(r1, rl) {
    apply (rl, 1, function(r2) {which((rectIntersect(r1,r2)))})
}

