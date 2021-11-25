#������� 2
#�������� ��� ������ 224 ��� ������
#�������� ������ ������������� �������� ��������� ������ ������� ����� ���� �� ������� ������ 2013 ���� �� ������ ��������� ������� ������������ ���������
#������� � �������� ���������� 
setwd("D:/mathmod/Son2") 
getwd()
#������ � ������������ � ���������� �������
library("tidyverse") 
library("readr")     
library("stringr")   
library("dplyr") 
# ��������� ���� ������
library("ggplot2")
# ����������� ��� ������� � �������� ��������� 
#������ ������ �� �����, ���������� ������ ������, �������� ��������� 'NA',
# ������ � ��������������� ��������� �������� �� NA, ���������� ������ � "[" 
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
                   comment=c("["))
# ������� �������� ������ ������ ������ 
eddypro = eddypro[-1, ]
#�������� ������ ������ � ��������� ������� ������� "roll"
eddypro = select(eddypro, -(roll))
# ����������� � ������� (factor) ������ ���� char (������)
eddypro=eddypro %>% mutate_if(is.character,factor)
#��������� ����������� �������� � �������� ������� �� ���������� ��� ���������� ��������
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
# ��������� ������� ������� � ���� �������� ��� �������� 
glimpse(eddypro)
# ������ na, ��� ��� ������ �������� ����� ������ ������ ������
eddypro=drop_na(eddypro)
# ����������� �� ������� ������ ������ �� ������� ������. � ������ �������� (243 ����) �� ����� ������ (334 ����)
eddypro = filter(eddypro,DOY >= 243 & DOY < 334)
# ����������� ������ �� ������� ������ �� ������ �����
eddypro = filter(eddypro, daytime ==FALSE)
# ���������� ����� numeric (������ � �������) � non numeric (� ���������� ��������� ��������)
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]

# ���� �������� ������������� ��������
# �������� ��������� � ����������� ���������������� ������� � ������� �������� �����������
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
#��������� �������
teaching_tbl = eddypro_numeric[teach,]
#����������� �������
testing_tbl = eddypro_numeric[test,]

# �������� ������ 1, ������� � ��� ��� ���������� � ������� "(.)" � ��������� ��������� �������
mod1 = lm(h2o_flux~(.), data = teaching_tbl)
# ������� ���������� � ������ � ������������
summary(mod1)
# �������������� ���������� �� ����������
anova(mod1)
#������� �������
plot(mod1)
# �������� ������ 2 ������� � ��� �������� ���������� �� ����������� ������� anova() �� ����������� �� 0.01, �������������� ***, ** � ** ������� 
mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H +  rand_err_H  + LE + qc_LE + rand_err_h2o_flux + rand_err_co2_flux + H_strg + h2o_molar_density + h2o_mole_fraction  + h2o_mixing_ratio + co2_molar_density 
          + co2_mole_fraction + co2_mixing_ratio + h2o_time_lag + sonic_temperature  + air_temperature + air_pressure + air_density + air_heat_capacity  + air_molar_volume 
          + water_vapor_density  + e + es + RH +  Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot + wind_dir + yaw + pitch + TKE + L
          + bowen_ratio + x_peak + x_offset  + x_offset+ x_10. +x_30.+ x_50. +x_70. + un_Tau + H_scf + un_LE + un_co2_flux 
          + un_h2o_flux + h2o_spikes + h2o.1 , data = teaching_tbl)
   

# ������� ���������� � ������ � ������������
summary(mod2)
# �������������� ���������� �� ����������
anova(mod2)
# ������� � ���������� �������, �� ���������� �� ���
anova(mod2, mod1)

# �������� ������ 3, �������� ������������
mod3 = lm(h2o_flux~DOY + Tau + qc_Tau +  qc_H +  rand_err_H  + qc_LE + rand_err_h2o_flux + co2_flux + H_strg + h2o_molar_density + co2_molar_density 
          + co2_time_lag+ h2o_mixing_ratio + co2_molar_density + air_pressure + u_unrot + v_unrot + w_unrot + v_rot + yaw 
          + bowen_ratio + TKE + x_peak + un_h2o_flux, data = teaching_tbl )
# ������� ���������� � ������ � ������������
summary(mod3)
# �������������� ���������� �� ����������
anova(mod3)
# ������� � ���������� �������, �� ���������� �� ���
anova(mod3, mod2)
# ������� �������
plot(mod3)

# �������� �������������� ������ ����������
# ������� �� ������� ������ ����������� � �������� ������ ���������� 
cor_teaching_tbl = select(teaching_tbl, h2o_flux, DOY, Tau, qc_Tau, qc_H, rand_err_H, qc_LE, rand_err_h2o_flux, h2o_flux,
                          H_strg, co2_molar_density, h2o_molar_density, h2o_time_lag, air_pressure,
                          u_unrot, v_unrot, w_unrot, v_rot, yaw, bowen_ratio,  x_peak, un_h2o_flux)
#�������� ������� ������������� ����������. � ����������� ������ 3, ������ �� ������ ���� �� ���� ������������� ����� ����� ���������� (������� �� ������������ >= 0.7)
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

# ������� �� ���������� ������
#�������� ������
#�������� ����� h2o_flux �� h2o_flux �� ��������� ��������� �������. ������� ������������� �������� �� ������ 3 �� ��������� ������� ������ � ���� �����
#� ������ ����� ������  ������ ����� ��� �����. � ��� ��� � ��� ������ h2o_flux �� ����� ����, �� �� ������ ���� ��� 45��������
qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#������ ������� ���� ����� �� ����������� ������
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#��� ��� � ��� ������ ������� �� ��������� ����������, �� ����� ������� ����� �������� ������������ h2o_flux �� ����������� � ������ ����������
#� ������ ������������� ����� ������ ������ ����� ��� �����, ��� ��� ����� ����� � ��� �� ����������� �������
#�������
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

