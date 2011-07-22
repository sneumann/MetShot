makeMiniXR <-
function(object)
{
    xr <- new("xcmsRaw")

#    xr <- object##new("xcmsRaw")
    xr@env$mz=object@msnPrecursorMz
    xr@env$intensity=object@msnPrecursorIntensity
    xr@scantime = object@msnRt
    xr@scanindex = seq(1,length(object@msnRt))
    xr@acquisitionNum = seq(1,length(object@msnRt))
    xr@mzrange = range(object@msnPrecursorMz)

    xr
}

