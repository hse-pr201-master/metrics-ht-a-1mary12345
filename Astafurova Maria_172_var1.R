library('rio')
library('ggplot2')
library("car")
library("sandwich")
library("lmtest")
library("maxLik")
df <- import('forestfires.csv') # Импорт данных
for (i in 1:nrow(df)){ # Удалим нулевые значение переменной area
  if (df$area[i] == 0){
    df <- df[-c(i), ]
  }
}
#Описание переменных для понимания
    #FFMC index - показатель влажности верхнего слоя земли (мусор, трава)
    #DMC index - норма засухи (показатель увлажненности неглубоких органических слоев)
    #DC index - норма засухи (показатель увлажненности глубоких органических слоев)
    #ISI index - показатель, коррелирующий с разбросом скорости огня
    #RH - относительная влажность в %
    #temp - темпераутра в ℃
    #wind - скорость ветра в км/ч
    #rain - дождь в мм/м2
    #area - площадь возгорания в га

# Включим в модель следующие факторы: RH, ожидаем отрицательный знак, так как чем больше влажность, тем медленнее распространениие; 
# wind, ожидаем положительный знак, так как чем больше скорость ветра, тем сильнее распространится огонь;
# temp,ожидаем положительный знак, так как чем выше температура,тем более благоприятные условия для распространениия огня.

# Описательные статистики
summary(df$area)
(sd(df$area))^2
summary(df$RH)
(sd(df$RH))^2
summary(df$wind)
(sd(df$wind))^2
summary(df$temp)
(sd(df$temp))^2
# Графики
ggplot(data = df, aes(x = area)) + geom_histogram() + xlab("Площадь пожаров") + 
  ylab("Гистограмма") + ggtitle('Area') +
  theme(plot.title = element_text(colour = "#b21919"))
ggplot(data = df, aes(x = RH)) + geom_histogram() + xlab("Влажность") + 
  ylab("Гистограмма") + ggtitle('RH') +
  theme(plot.title = element_text(colour = "#b21919"))
ggplot(data = df, aes(x = wind)) + geom_histogram() + xlab("Ветер") + 
  ylab("Гистограмма") + ggtitle('wind') +
  theme(plot.title = element_text(colour = "#b21919"))
ggplot(data = df, aes(x = temp)) + geom_histogram() + xlab("Температура") + 
  ylab("Гистограмма") + ggtitle('temp') +
  theme(plot.title = element_text(colour = "#b21919"))
# Присутствуют выбросы по перемнной area. Удалим их
for (i in 1:nrow(df)){
  if (df$area[i] > 100){
    df <- df[-c(i), ]
  }
}
# Строим регрессию
reg <- lm(data = df, area ~ RH + wind + temp)
summary(reg) # К сожалению, все не очень приятно с коэффициентами
# Мультуколлинеарность отсутствует
vif(reg)
# Остатки ненормальны
shapiro.test(residuals(reg))
# Прогнозы
predict(reg, df)
predict(reg, df, interval = 'confidence')
predict(reg, df, interval = 'prediction')
# Есть гетероскедастичность
plot(df$RH, df$area)
plot(df$wind, df$area)
plot(df$temp, df$area)
# Тест также подтверждает наличие гетероскедастичностии
gqtest(reg, point = 0.3)
# Робастные ошибки в форме Уайта
coeftest(reg, vcovHC(reg, type = c('HC0')))
coeftest(reg, vcovHC(reg, type = c('HC3')))
