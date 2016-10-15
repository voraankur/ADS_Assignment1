library(RSelenium)
library(XML)
library(magrittr)
library(stringr)
devtools::install_github("ropensci/RSelenium")
#selServ <- startServer(args = c("-port 4455", "-Dwebdriver.chrome.driver=/Users/mark/dev/R/selenium/chromedriver"), log = FALSE)
startServer(args = c("-Dwebdriver.chrome.driver=C:/Users/Ankur/Downloads/chromedriver_win32/chromedriver.exe")
            , log = FALSE, invisible = FALSE)
remDr <- remoteDriver(browserName = "chrome")

remDr$open()
remDr$navigate('https://www.ffiec.gov/nicpubweb/nicweb/HCSGreaterThan10B.aspx')
district_IDs <- remDr$findElements(using = "xpath",
                                   "//select[@name = 'DateDropDown']/option") %>%
  lapply(function(x){x$getElementAttribute('value')}) %>% 
  unlist
result = NULL

for (i in seq_along(district_IDs)) 
{
  remDr$findElement(using = "xpath",
                    paste0("//select[@id = 'DateDropDown']/option[@value = ", 
                           "'", district_IDs[i], "']"))$clickElement()
  
  table <- remDr$getPageSource()[[1]] %>% 
    htmlParse %>% 
    readHTMLTable %>% 
    extract2(3)
  
  location<-str_split_fixed(table$V3, ", ", 2)

  cbind.fill<-function(table, location)
  {
    nm <- list(table, location) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    as.data.frame(do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))))
  }
  total = cbind.fill(table, location)
  quarterDate<-total['1','V4']
  quarterDate<-str_sub(quarterDate, 0,10)
  total$Date<-quarterDate
  total<-total[-c(3)]
  
  names(total)[1]<-"Rank"
  names(total)[2]<-"InstitutionName"
  names(total)[3]<-"TotalAssets"
  names(total)[5]<-"State"
  names(total)[4]<-"City"
  total<-total[-1,]
  
  result = rbind(result, total)
}

write.csv(result,file="Bank_data.csv")

#Unstacking of data
long_format <- unstack(result,InstitutionName ~ State)
long_format

#Stacking of Data
long_format <- stack(result)
head(long_format)
remDr$quit()
remDr$closeServer()