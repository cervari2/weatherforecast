## App Key = 10a2421a4a7b932c758f21eaaf87164b
## Add more cities. Make URL a composed string based on city. Make into a function.

Location<-"4273837"
API_KEY<-"10a2421a4a7b932c758f21eaaf87164b"

Get_Forecast <- function(Location="4273837",API_Key="10a2421a4a7b932c758f21eaaf87164b") {
  
  library("httr")
  library("xml2")
  
  
  URL<-sprintf("api.openweathermap.org/data/2.5/forecast?id=%s&mode=xml&APPID=%s",Location,API_KEY)
  URL
  
  L<-GET(url="api.openweathermap.org/data/2.5/forecast?id=4273837&mode=xml&APPID=10a2421a4a7b932c758f21eaaf87164b")
  content<-rawToChar(L$content)
  
  content.split<-unlist(strsplit(content,c("<forecast>","</time>")))
  content.split1<-strsplit(content.split[2],"</time>")
  ## content.split1 splits by time 2 through end minus 1
  
  end<-length(content.split1[[1]])-1
  
  Times<-c()
  Temps<-c()
  
  j<-1
  for (i in 2:end) {
    
    content.split.time1<-strsplit(content.split1[[1]][i],"\"><symbol number")
    content.split.time2<-strsplit(content.split.time1[[1]][1],'\"')
    content.split.time3<-content.split.time2[[1]][2]
    content.split.time4<-gsub("T"," ",content.split.time3)
    content.split.time5<-as.POSIXct(content.split.time4, tz='UTC')
    Times[j]<-content.split.time5
    
    split.condition<-'</windSpeed><temperature unit=\"kelvin\" value=\"'
    content.split.temperature1<-strsplit(content.split1[[1]][i],split.condition)
    content.split.temperature2<-substring(content.split.temperature1[[1]][2],1,6)
    content.split.temperature3<-as.integer(content.split.temperature2)*(9/5)-459.67 # Rounds down
    Temps[j]<-content.split.temperature3
    
    j<-j+1
  }
  Times<-as.POSIXlt(Times, origin="1970-01-01", tz='UTC')
  Temps
  
  City<-as.data.frame(Times)
  City[,2]<-Temps
  names(City)<-c("Forecast_Time","Temperature (F)")
  return(City)
}

KC<-Get_Forecast(Location="4273837")
KC

