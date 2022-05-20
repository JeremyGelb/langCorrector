# getting the path of all jar files
setwd("E:/R/Packages/Language/inst/extdata/LanguageTool-5.7")
jars1 <- list.files('libs/', pattern = '*.jar', full.names = T)
jars1 <- c(paste0(getwd(),"/",jars1), paste0(getwd(),"/org"), paste0(getwd(),"/META-INF"))
# https://www.geeksforgeeks.org/how-to-add-jar-file-to-classpath-in-java/
# https://languagetool.org/download
# https://datawarrior.wordpress.com/2016/09/10/rjava-running-java-from-r-and-building-r-packages-wrapping-a-jar/

library(rJava)
.jinit()
for(jar in jars1){
  .jaddClassPath(jar)
}
