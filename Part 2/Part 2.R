library(pdftools)
library(XML)
library(RCurl)
library(stringr)


# Get URL
part2_url <- getURL("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/BHCPR_Peer.htm")
part2_doc <- htmlParse(part2_url)

# Get all links on page
links <- xpathApply(part2_doc, '//a', xmlGetAttr, 'href')
links

length(links)

# Get all pdf files names
pdf_files <- grep('.pdf', links, value = TRUE)
pdf_files

# PeerGroup 1 pdf files
peer1_pdf <- grep('PeerGroup_1', pdf_files, value = TRUE)
peer1_pdf

#extract year value from each pdf link
yearValue<-c()
for (i in seq_along(peer1_pdf)) {
  x<-peer1_pdf[i]
  x<-str_sub(x,-8,-5)
  yearValue <- c(yearValue, x)
}
yearValue

#assign quarter value
quarterValue<-c()
for(i in seq_along(peer1_pdf))
{
  monthString<-sub(".*1_", "", peer1_pdf[i])
  monthString<-str_sub(monthString,0,3)
  if(monthString=="Jan" || monthString=="jan" || monthString=="Feb" || monthString=="feb" || monthString=="Mar" || monthString=="mar")
  {
    quarterVal<-1
    quarterValue<-c(quarterValue, quarterVal)
  } else if(monthString=="Apr" || monthString=="apr" || monthString=="May" || monthString=="may" || monthString=="Jun" || monthString=="jun")
  {
    quarterVal<-2
    quarterValue<-c(quarterValue, quarterVal)
  } else if(monthString=="Jul" || monthString=="jul" || monthString=="Aug" || monthString=="aug" || monthString=="Sep" || monthString=="sep")
  {
    quarterVal<-3
    quarterValue<-c(quarterValue, quarterVal)
  } else if(monthString=="Oct" || monthString=="oct" || monthString=="Nov" || monthString=="nov" || monthString=="Dec" || monthString=="dec")
  {
    quarterVal<-4
    quarterValue<-c(quarterValue, quarterVal)
  }
}
quarterValue

# Loop to download all files
for(i in 1 : length(peer1_pdf))
{
  linkk1 <- paste("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/",peer1_pdf[i],sep = "")
  download.file(linkk1,sprintf("C:/Users/Ankur/Desktop/ADS/ADS_Assignments/Assignment1/Part2/PDFFiles/Peer1_%s_q%s.pdf",yearValue[i],quarterValue[i]), mode = "wb")
}

#Convert PDF to Text files
dest <- "C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment1\\Part2\\PDFFiles"
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)
for (i in 1:length(myfiles)) {
  txt <- pdf_text(myfiles[i])
  write.table(data.frame(as.list(txt)),file = sprintf("%s.txt",myfiles[i]), sep = "\t")
}

#Convert Txt file to CSV
filelist = list.files(path= dest, pattern = ".txt", full.names = TRUE)
for (i in 1:length(filelist)) {
  curInputFile <- filelist[i]
  curOutputFile <- paste0(curInputFile, ".csv") 
  print(paste("Processing the file:", curInputFile))
  data = read.delim(curInputFile,sep = "\t", header = FALSE, skip = 1)
  write.table(data, file=curOutputFile, sep = "\t",col.names=FALSE, row.names=FALSE)
}
