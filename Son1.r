#Сон С.Г. Вариант 11
#для региона 39 рассчитайте урожайность пшеницы в 2016 году, взяв для рассчета средние суммы активных температур за предыдущие 13 лет, 
#с 11 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 27 градусов
#39 регион - Калининградская область
setwd("D:/mathmod/Son1")
getwd()
#устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
#открываем нужные пакеты
library("tidyverse")
library("rnoaa")
library("lubridate")
#скачиваем станции
station_data = ghcnd_stations()
write.csv(station_data,file="stations.csv")
station_data=read.csv("stations.csv")


#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
Kaliningrad = data.frame(id = "KALININGRAD", latitude = 54.43,  longitude = 20.30)
Kaliningrad_around = meteo_nearby_stations(lat_lon_df = Kaliningrad, station_data = station_data,
                                         limit = 11, var = c("TAVG"),
                                         year_min = 2003, year_max = 2016)

#Kaliningrad_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Калининград, очевидно что первым элементом таблицы будет идентификатор метеостанции Калининград, его то мы и попытаемся получить
Kaliningrad_id = Kaliningrad_around[["KALININGRAD"]][["id"]][1]
summary(Kaliningrad_id)
# для получения таблицы со всеми метеостанциями вокруг Калининград
# необходимо выбрать целиком первый объект из списка
Kaliningrad_table=Kaliningrad_around[[1]]
summary(Kaliningrad_table)
# в таблице Kaliningrad_table оказалось 11 объектов, ранжированных по расстоянию от Калининград
# сформируем список необходимых станций
# выведем индетификаторы отфильрованных метеостанций
Kaliningrad_stations=Kaliningrad_table
str(Kaliningrad_stations)

# список содержит 11 метеостанции расположенных вблизи Калининграда
# для получения таблицы со всеми метеостанциями вокруг 
# выведем индетификаторы отфильрованных метеостанций
Kaliningrad_stations$id

# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_Kaliningrad_data=meteo_tidy_ghcnd(stationid = Kaliningrad_id)
# посмотрим что мы скачали
summary(all_Kaliningrad_data)


# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные всех метеостанций (колличество)
all_Kaliningrad_meteodata = data.frame()
# создаем цикл для наших метеостанций
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


# записываем полученные результаты
write.csv(all_Kaliningrad_meteodata,"all_Kaliningrad_meteodata.csv")
# считываем данные all_Kaliningrad_meteodata.csv
all_Habarovsk_meteodata=read.csv("all_Kaliningrad_meteodata.csv")
# смотрим что получилось 
str(all_Kaliningrad_meteodata)

# добавим год, месяц, день
all_Kaliningrad_meteodata=all_Kaliningrad_meteodata %>% mutate(year=year(date), 
                                                           month=month(date), 
                                                           day=day(date))

# превратим NA в 0 и где tavg<5>27
all_Kaliningrad_meteodata[is.na(all_Kaliningrad_meteodata$tavg),"tavg"] = 0
all_Kaliningrad_meteodata[all_Kaliningrad_meteodata$tavg<5, "tavg"] = 0
all_Kaliningrad_meteodata[all_Kaliningrad_meteodata$tavg>27, "tavg"] = 0
summary(all_Kaliningrad_meteodata)





# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata =all_Kaliningrad_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему
#числу дней  в месяце, константа по табл.1
y=1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf=300
# Коэффициент использования ФАР посевом 
Qj=1600
# калорийность урожая культуры
Lj=2.2
# сумма частей основной побочной продукции
Ej=25
# стандартная влажность культуры 
# Рассчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield 
# Результат 14,75 ц/га
