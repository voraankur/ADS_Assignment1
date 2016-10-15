library(XML)
library(xml2)
library(rvest)
library(RCurl)

part2_url <- "https://www.ffiec.gov/nicpubweb/nicweb/Y15SnapShot.aspx"
part2_doc <- htmlParse(part2_url)
part2_url <- getURL("https://www.ffiec.gov/nicpubweb/nicweb/Y15SnapShot.aspx")
part2_doc <- htmlParse(part2_url)
links <- xpathApply(part2_doc, '//a', xmlGetAttr, 'href')
links

webpage <- getURL("https://www.ffiec.gov/nicpubweb/nicweb/HCSGreaterThan10B.aspx")
webpage

doc <- htmlParse(webpage)

id <- xpathSApply(doc, "//select[@id='DateDropDown']/option", xmlAttrs)
options <- getNodeSet(doc, "//select[@id='DateDropDown']/option")
ids <- sapply(options, xmlGetAttr, "value")
names <- sapply(options, xmlValue)
data.frame(Quarters=names)

tables <- readHTMLTable(webpage, which = 3)
tables
write.csv(tables,file="Bank_data.csv")
read.csv(file="Bank_data.csv")
