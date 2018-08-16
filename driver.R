
## Add more cities. 

source(file="private_config.R")

Get_Forecast <- function(Location="4273837",API_Key="10a2421a4a7b932c758f21eaaf87164b") {
  
  library("httr")
  library("xml2")
  
  
  URL<-sprintf("api.openweathermap.org/data/2.5/forecast?id=%s&mode=xml&APPID=%s",Location,API_KEY)
  URL
  
  L<-GET(url=URL)
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
Indy<-Get_Forecast(Location="4259418")
Atlanta<-Get_Forecast(Location="4883772")
Chicago<-Get_Forecast(Location="4887398")


Update<-as.character(Sys.time())
Update1<-as.character(sprintf("Last updated: %s",Update))


library("ggplot2")

s<-1.4

forecast<-ggplot() +
         geom_line(data=KC, aes(x=KC[,1], y = KC[,2], color="KC"),size=s) + 
         geom_line(data=Indy, aes(x=Indy[,1], y = Indy[,2], color="Indy"),size=s) +
         geom_line(data=Chicago, aes(x=Chicago[,1], y = Chicago[,2], color="Chicago"),size=s) + 
         geom_line(data=Atlanta, aes(x=Atlanta[,1], y = Atlanta[,2], color="Atlanta"),size=s) +
         ylab("Temperature (F)") +
         xlab("Date") +
         annotate("text",label=Update1,x=KC[12,1],y=max(KC[,2],na.rm=TRUE)-1.2) +
         scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d ") +
         theme_minimal()

ggsave('forecast_image.png',forecast,device="png",path='C:/Users/D495/Documents/GitHub/weatherforecast')
