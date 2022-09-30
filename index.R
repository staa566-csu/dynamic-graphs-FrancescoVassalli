library(xts)
library(dygraphs)
library(dplyr)
library(plotly)
two_bedroom_data<-read.csv("/Users/francesco/Downloads/Viz/City_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
rent_data<-read.csv("/Users/francesco/Downloads/Viz/City_zori_sm_month.csv")
denver_2bd<- two_bedroom_data %>% filter(RegionName=="Denver", State=="CO")
denver_rent<-rent_data%>% filter(RegionName=="Denver", State=="CO")
koko_2bd<- two_bedroom_data %>% filter(RegionName=="Columbus", State=="OH")
koko_rent<-rent_data%>% filter(RegionName=="Columbus", State=="OH")
denver_2bd= denver_2bd[,9:280]
koko_2bd = koko_2bd[,9:280]
denver_rent = denver_rent[,9:98]
koko_rent = koko_rent[,9:98]
to_date<-function(name){
  name=substr(name,2,nchar(name))
  return (as.Date(name,format="%Y.%m.%d"))
}
date_names=colnames(denver_2bd)
dates = rep(NA,length(date_names))
for( i in 1:length(date_names)){
  dates[i] = to_date(date_names[i])
}
df<-data.frame(Date=as.Date(dates),denver_2bd=as.numeric(denver_2bd[1,]),kokomo_2bd=as.numeric(koko_2bd[1,]))
df_rents<-data.frame(Date=as.Date(dates),denver_rent=c(rep(NA,280-98),as.numeric(denver_rent[1,])),kokomo_rent=c(rep(NA,280-98),as.numeric(koko_rent[1,])))
bds<-plot_ly(df,x=~Date)%>%add_lines(y=~denver_2bd,name="Denver, CO",line = list(color = 'rgb(22, 96, 167)'))%>%add_lines(y=~kokomo_2bd,name="Columbus, OH",line = list(color = 'rgb(205, 12, 24)'))
rents<-plot_ly(df_rents,x=~Date)%>%add_lines(y=~denver_rent,showlegend = F,line = list(color = 'rgb(22, 96, 167)'))%>%add_lines(y=~kokomo_rent,,showlegend = F,line = list(color = 'rgb(205, 12, 24)'))

rents
facet<-subplot(list(bds,rents),nrows=2,shareX=TRUE,titleX=FALSE)%>% rangeslider()%>%layout(title="Median Sale vs Rent Price")
facet
