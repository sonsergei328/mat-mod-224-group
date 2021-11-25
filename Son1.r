#��� �.�. ������� 11
#��� ������� 39 ����������� ����������� ������� � 2016 ����, ���� ��� �������� ������� ����� �������� ���������� �� ���������� 13 ���, 
#� 11 ��������� ������������ �� ������ �� �������� �������� ���������� ��� � ������������ ���� 27 ��������
#39 ������ - ��������������� �������
setwd("D:/mathmod/Son1")
getwd()
#������������� ������
install.packages("tidyverse")
install.packages("rnoaa")
#��������� ������ ������
library("tidyverse")
library("rnoaa")
library("lubridate")
#��������� �������
station_data = ghcnd_stations()
write.csv(station_data,file="stations.csv")
station_data=read.csv("stations.csv")


#����� ��������� ������� ���� �������, �������� ������ ������� ��������� � ������� ������ �������,������ ������� � ������ ������� � ������������ ��� �������
Kaliningrad = data.frame(id = "KALININGRAD", latitude = 54.43,  longitude = 20.30)
Kaliningrad_around = meteo_nearby_stations(lat_lon_df = Kaliningrad, station_data = station_data,
                                         limit = 11, var = c("TAVG"),
                                         year_min = 2003, year_max = 2016)

#Kaliningrad_around ��� ������ ������������ ��������� �������� �������� �������, ���������� �������������� ������������ ��������������� �� �� 
# ������������ �� �����������, �������� ��� ������ ��������� ������� ����� ������������� ������������ �����������, ��� �� �� � ���������� ��������
Kaliningrad_id = Kaliningrad_around[["KALININGRAD"]][["id"]][1]
summary(Kaliningrad_id)
# ��� ��������� ������� �� ����� �������������� ������ �����������
# ���������� ������� ������� ������ ������ �� ������
Kaliningrad_table=Kaliningrad_around[[1]]
summary(Kaliningrad_table)
# � ������� Kaliningrad_table ��������� 11 ��������, ������������� �� ���������� �� �����������
# ���������� ������ ����������� �������
# ������� �������������� �������������� ������������
Kaliningrad_stations=Kaliningrad_table
str(Kaliningrad_stations)

# ������ �������� 11 ������������ ������������� ������ ������������
# ��� ��������� ������� �� ����� �������������� ������ 
# ������� �������������� �������������� ������������
Kaliningrad_stations$id

# ����� �������� ��� ������ � 1 ������������ ���������� ������� meteo_tidy_ghcnd
all_Kaliningrad_data=meteo_tidy_ghcnd(stationid = Kaliningrad_id)
# ��������� ��� �� �������
summary(all_Kaliningrad_data)


# ������� ����, � ������� �� ����������� ������ ������ ��� ���� ������������
# c������� ������, ���� ������� ��� ������ ���� ������������ (�����������)
all_Kaliningrad_meteodata = data.frame()
# ������� ���� ��� ����� ������������
stations_names=Kaliningrad_stations$id
stations_names=stations_names[1:11] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2003-01-01",
                              date_max = "2016-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_Kaliningrad_meteodata=rbind(all_Kaliningrad_meteodata, one_meteo)}


# ���������� ���������� ����������
write.csv(all_Kaliningrad_meteodata,"all_Kaliningrad_meteodata.csv")
# ��������� ������ all_Kaliningrad_meteodata.csv
all_Habarovsk_meteodata=read.csv("all_Kaliningrad_meteodata.csv")
# ������� ��� ���������� 
str(all_Kaliningrad_meteodata)

# ������� ���, �����, ����
all_Kaliningrad_meteodata=all_Kaliningrad_meteodata %>% mutate(year=year(date), 
                                                           month=month(date), 
                                                           day=day(date))

# ��������� NA � 0 � ��� tavg<5>27
all_Kaliningrad_meteodata[is.na(all_Kaliningrad_meteodata$tavg),"tavg"] = 0
all_Kaliningrad_meteodata[all_Kaliningrad_meteodata$tavg<5, "tavg"] = 0
all_Kaliningrad_meteodata[all_Kaliningrad_meteodata$tavg>27, "tavg"] = 0
summary(all_Kaliningrad_meteodata)





# ����������� ������������ �� id,������� � ����� � ������������ �������������
# �� ���� �������, ����� ����������� ������ �� ������� � ������ ������� �� �������
# ��� ���� ������������
group_meteodata =all_Kaliningrad_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## ���������� � ������� �� ������� ������ ##
### ���� ��������
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# ��������� �� ����.1. ������� ������
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# ��������� �� ����. 1. ������� ������
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# ��������� ����� ���� i-�� ������,
#�������� � ������ ��������� ��������, � ������
#����� ����  � ������, ��������� �� ����.1
y=1.0
# ����������� ��� ���������� ������ - �������, ��� ��� ���� �������� ������
Kf=300
# ����������� ������������� ��� ������� 
Qj=1600
# ������������ ������ ��������
Lj=2.2
# ����� ������ �������� �������� ���������
Ej=25
# ����������� ��������� �������� 
# ���������� Fi �� �������
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#���������� Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  ����������� ������ 
Yield = (sum(sumT_month$Yi)) 
Yield 
# ��������� 14,75 �/��
