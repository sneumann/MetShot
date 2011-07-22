rectIntersect <-
function (r1,r2) {
  return (  r1[,"rtmin"] %within% r2[c("rtmin","rtmax")]
          & r1[,"rtmax"] %within% r2[c("rtmin","rtmax")]
          & r1[,"mzmin"] %within% r2[c("mzmin","mzmax")]
          & r1[,"mzmax"] %within% r2[c("mzmin","mzmax")] )
}

