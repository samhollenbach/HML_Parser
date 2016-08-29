require(XML)

#Set working director as this files directory
setwd(dirname(parent.frame(2)$ofile))

#Directory to print output files to
printDir <- "prints/"


#Main run function
#Accepts a list of files, an output type (1 or 2), and a list of accepted loci
run.parser <- function(files,output_type,accepted_loci){
  if(file.exists(paste0(printDir,"genos.txt")) && (output_type == 2)){
    file.remove(paste0(printDir,"genos.txt"))
  }
  
  
  for(f in files) {
    readHML(f,"data.txt",output_type,accepted_loci)
  }
  
}


#Parses an individual HML file, with same arguments as run function
readHML <- function(file,data_file_name,output_type,accepted_loci){
  
  #Gets file path and name
  hml.uri <- file.path(file)
  hml.name <- basename(file)
  
  #Parses HML document into a DOM
  doc <- xmlInternalTreeParse(hml.uri)
  
  #Finds locus and checks against accepted loci
  locus <- xmlAttrs(xpathApply(doc,"//sbt-ngs")[[1]])
  if(!(locus %in% accepted_loci)){
    return()
  }
  
  #Finds sampleId and glstring from HML file
  sampleID <- strsplit(hml.name,"_")[[1]][2]
  glstring <- trimws(xpathApply(doc,"//glstring",xmlValue))
  
  #Writes all other data to a data.txt file
  writeMetadata(doc,data_file_name)
  
  #Writes genos file(s) depending on output type
  if(output_type == 1){
    #Write multiple genos files for each loci
    writeGenos1(locus,sampleID,glstring)
  }else if(output_type == 2){
    #Write a single genos.txt file for all loci
    writeGenos2(locus,sampleID,glstring)
  }
}

#Writes all data besides sampleId, glstring and locus to a text file
#In the future this may need to be adjusted to accomidate multiple HML files with different metadata
writeMetadata <- function(doc,metadataFile){
  file <- paste(printDir,metadataFile,sep="")
  if(file.exists(file)){
    file.remove(file)
  }
  nodes <- getNodeSet(doc,"//*",namespaces = xmlNamespaceDefinitions(doc,simplify = TRUE))
  for(i in 1:length(nodes)){
    temp <- xmlAttrs(nodes[[i]])
    if(!is.null(temp)){
      for(j in 1:length(temp)){
        if(names(temp)[j] == "locus"){
          break
        }
        str <- paste0(names(temp)[j]," = \"",temp[[j]],"\"")
        write(str,file,append = TRUE)
      }
    }
  }
}

#Writes a seperate genos file for each locus, named genos_LOCUS.txt, containing each sample and corresponding glstrings
writeGenos1 <- function(locus,sampleID,glstring){
  line <- paste(sampleID, glstring)
  fileName <- paste0(printDir,"genos_",locus,".txt")
  write(line,fileName,append = TRUE)
}

#Writes one genos.txt file which contains samples for all loci, 
#Multiple loci for a given sample with have their glstrings appended by ~
writeGenos2 <- function(locus,sampleID,glstring){
  fileName <- paste0(printDir,"genos.txt")
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
      lines[lineNum] <- paste0(lines[lineNum],"~",glstring)
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



