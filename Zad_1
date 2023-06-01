# Ivanovskaya-matmod
Ghh
#Ивановская Елена. Д-А 131. 
#Ивановская Елена Сергеевна — для региона 25* рассчитайте урожайность пшеницы в 2003 году, взяв для рассчета средние суммы активных температур за предыдущие 12 лет, с метеостанций на расстоянии от 90 до 180 км
# 25 регион - Приморский край

#работа с пакетами 
library(tidyverse)
library(rnoaa)
library(lubridate)
#скачиваем станции
station_data = ghcnd_stations()
station_data
write.csv(station_data, file = "stations.csv")
station_data = read.csv("stations.csv")

#После получения списка станций, получим список станций ближайщих к столице региона и координами его столицы
primor = data.frame(id = "PRIMOR" , latitude = 43.115536, longitude = 131.885485)
primor_around = meteo_nearby_stations(lat_lon_df = primor, station_data = station_data,
                                      limit = 100, var = c("TAVG"),
                                      year_min = 1991, year_max = 2003)
primor_around #primor_around  от список содержащий индификаторы метеостанций отсортированные по их удаленности от Владивостока(центральный город Приморского края)
primor_around = primor_around[[1]]

#отфильтруем станции по расстоянию 
primor_around = primor_around %>% filter(distance > 90 & distance < 180)
primor_id = primor_around[["PRIMOR"]][["id"]][1]
summary(primor_id)

#выбираем целиком первый объект из списка
primor_table=primor_around[[1]]
summary(primor_table)
primor_table

# в таблице primor_table оказалось 8 объектов ранжированых по расстоянию от Владивастока
#сформируем список необходимых станций
primor_stations = primor_table
str(primor_stations)

#выводим индетификаторы отфильтрованных метиостанций
primor_id = primor_around$id

#создаем цикл, в котором бы скачивались нужные данные для всех метеостанций
#создадим объект, куда скачаем все данные всех метеостанций
all_primor_data=meteo_tidy_ghcnd(stationid = primor_id)
summary(all_primor_data)

#Создаем объект куда скачаем все данные всех метеостанций(колличество)
all_primor_meteodata = data.frame()
#Создаем цикл для метеостанций
stations_names = primor_around$id
stations_names=stations_names[1:8]

for (sname in stations_names)
{one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "1991-01-01",
                              date_max = "2003-12-31")
  station_vars=names(one_meteo)
  if (!("tavg" %in% station_vars)){
    if(!("tmax"%in% station_vars)){
      next()
    }
    
    
    
    
    one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
  one_meteo=one_meteo %>% select(id,date,tavg)
  one_meteo = one_meteo %>% mutate(tavg=tavg/10)
  all_primor_meteodata=rbind(all_primor_meteodata, one_meteo)}

#Записываем полученные результаты
write.csv(all_primor_meteodata,"all_primor_meteodata.csv")
#считываем данные 
all_vladivostoc_meteodata=read.csv("all_primor_meteodata.csv")
#смотрим, что получилось
str(all_primor_meteodata)

#добавим год, месяц, день
all_primor_meteodata = all_primor_meteodata %>% mutate(year=year(date), 
                                                               month=month(date), 
                                                               day=day(date))
#Превратим NA в 0 и где tavg<5
all_primor_meteodata[is.na(all_primor_meteodata$tavg),"tavg"] = 0
all_primor_meteodata[all_primor_meteodata$tavg<5, "tavg"] = 0
summary(all_primor_meteodata)

#Сгрупируем метеостанции по id, месяцам и годам и проссумируем температуру по этим группа, затем сгруппируем данные по месяцам и найдем среднее по месяцам для всех метеостанций
group_meteodata =all_primor_meteodata %>% group_by(id,year,month)

sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)


sumT_month=groups_month%>%summarise(St=mean(tsum))

##Подготовка к расчету по формуле урожая
#Ввод констант
y = 1.0 #коэффициент для экспозиции склона 
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)#константа, из табл.1

bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)#константа, из табл.1
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)#отношение числа i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце из табл.1
Kf = 300 #  коэффициент использования ФАР посевом
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  коэффициент «Сумма частей основной и побочной продукции
Ej = 25 #   коэффициент «Стандартная влажность культуры

#Расчитаем Fi по месяцам 
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Расчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Расчитаем урожай
Yield = (sum(sumT_month$Yi)) 
Yield 

#Результат 16,40 ц/га
