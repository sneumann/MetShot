beginTraml <-
function() { 
  tramlVersion="0.9.4"
  schemaLocation="http://psi.hupo.org/ms/traml TraML0.9.4.xsd"
  
  ##
  ## TraML Root
  ##
  
  traml = xmlTree(tag="TraML",
    attrs=c(
      version=tramlVersion,
      "xsi:schemaLocation"=schemaLocation),
    namespaces = c(
      "http://psi.hupo.org/ms/traml",
      xsi="http://www.w3.org/2001/XMLSchema-instance")
    )
  
  ##
  ## CV List
  ##

  traml$addNode("cvList", close = FALSE)
  traml$addNode("cv", attrs=c(
                        id="MS",
                        fullName="Proteomics Standards Initiative Mass Spectrometry Ontology",
                        version="2.29.0",
                        URI="http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo"))
  traml$addNode("cv", attrs=c(
                        id="UO",
                        fullName="Unit Ontology",
                        version="1.20",
                        URI="http://obo.cvs.sourceforge.net/obo/obo/ontology/phenotype/unit.obo"))
  traml$closeTag()

  ##
  ## InstrumentList
  ##

  traml$addNode("InstrumentList", close = FALSE)
  traml$addNode("Instrument",
                attrs=c(id="microtof"),
                close = FALSE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1001536",
                             name="Bruker Daltonics micrOTOF series"
                             ))
  traml$closeTag() ## Instrument
  traml$closeTag() ## InstrumentList

  
  ##
  ## SoftwareList
  ##

  traml$addNode("SoftwareList", close = FALSE)
  traml$addNode("Software",
                attrs=c(
                  id="XCMS-1.12",
                  version=packageDescription("xcms", fields="Version", drop=TRUE)),                
                close = FALSE)

  ##   traml$addNode("cvParam", attrs=c(
  ##                              cvRef="MS",
  ##                              accession="MS:1000xxx",
  ##                              name="XCMS",
  ##                              version=packageDescription("xcms", fields="Version", drop=TRUE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1001582",
                             name="XCMS"))
  traml$closeTag()
  traml$closeTag()

  ##
  ## TargetList
  ##

  traml$addNode("TargetList", close = FALSE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1000920",
                             name="includes supersede excludes"))

  traml$addNode("TargetIncludeList", close = FALSE)

  traml
}

