addTargetSegment <-
function(traml,
                             MSmode=c("positive","negative"),
                             MSMSManual_ListCollisionEnergy=15,
                             MSMSManual_ListIsolationWidth=8) {
  
  traml$addNode("Target", attrs=c(
                            id=7
                            ##, compoundRef="T108M381"
                            ), close=FALSE)
  traml$addNode("Precursor", close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000827",
                  name="isolation window target m/z",
                  value="4.0",
                  unitCvRef="MS",
                  unitAccession="MS:1000040",
                  unitName="m/z"))
  traml$closeTag() ## Precursor

  traml$addNode("RetentionTime", close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000895",
                  name="local retention time",
                  value="5.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000916",
                  name="retention time window lower offset",
                  value="6.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000917",
                  name="retention time window upper offset",
                  value="7.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
    traml$closeTag() ## RetentionTime

  traml$addNode("ConfigurationList", close=FALSE)
  traml$addNode("Configuration",
                attrs=c(instrumentRef="microtof"),
                close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000045",
                  name="collision energy",
                  value="8.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000266",
                  unitName="electronvolt"))
  traml$closeTag() ## Configuration
  traml$closeTag() ## ConfigurationList  
  traml$closeTag() ## Target

  traml  
}

