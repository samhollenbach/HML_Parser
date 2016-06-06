require(XML)

printDir <- "prints/"
#xml.url <- "hml/HML_IND00087_3DL2.xml"

output = 2;

readHML <- function(hml.uri,metadataFile){
  
  
  doc <- xmlTreeParse(hml.uri,useInternalNodes = TRUE)
  sampleID <- strsplit(hml.uri,"_")[[1]][2]
  glstring <- trimws(xpathApply(doc,"//glstring",xmlValue))
  locus <- xmlAttrs(xpathApply(doc,"//sbt-ngs")[[1]])

  
  writeMetadata(doc,metadataFile)
  
  if(output == 1){
    writeGenos1(locus,sampleID,glstring)
  }else if(output == 2){
    writeGenos2(locus,sampleID,glstring)
  }
}

writeMetadata <- function(doc,metadataFile){
  file <- paste(printDir,metadataFile,sep="")
  file.remove(file)
  nodes <- getNodeSet(doc,"//*",namespaces = xmlNamespaceDefinitions(doc,simplify = TRUE))
  for(i in 1:length(nodes)){
    temp <- xmlAttrs(nodes[[i]])
    if(!is.null(temp)){
      for(j in 1:length(temp)){
        if(names(temp)[j] == "locus"){
          break
        }
        str <- paste(names(temp)[j]," = \"",temp[[j]],"\"", sep = "")
        write(str,file,append = TRUE)
      }
    }
  }
}


writeGenos1 <- function(locus,sampleID,glstring){
  line <- paste(sampleID, glstring)
  fileName <- paste(printDir,"genos_",locus,".txt",sep="")
  write(line,fileName,append = TRUE)
}

writeGenos2 <- function(locus,sampleID,glstring){
  fileName <- paste(printDir,"genos.txt",sep="")
  if(!file.exists(fileName)){
    file.create(fileName)
  }
  lines <- readLines(fileName)
  appended <- FALSE
  for(lineNum in 1:length(lines)){
    if(is.na(lines[lineNum])){
      break
    }
    sub <- substr(lines[lineNum],1,8)
    if(sampleID == sub){
      lines[lineNum] <- paste(lines[lineNum],"~",glstring,sep="")
      writeLines(lines,fileName)
      appended <- TRUE
      break
    }
  }
  if(!appended){
    line <- paste(sampleID, glstring)
    write(line,fileName,append = TRUE)
  }
  
}

readFiles <- function(){
  
  tempFiles <- list.files("hml",full=TRUE)
  
  if(file.exists("prints/genos.txt")){
    file.remove("prints/genos.txt")
  }
  for(file in tempFiles){
    readHML(file.path(file),"data.txt") 
  }
  
}

readFiles()





