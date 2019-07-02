library(ggplot2)
library(scales) # for pieplot


df <- read.csv("./postupleniye/2018/podano.csv", dec = ",")
jur <- subset(df, SpecialityCode == "42.03.02")
# Берём только нужные столбцы
jur <- jur[c("Name", "SpecialityCode", "SpecialityName", "ObrazProgrName", "TeachForm",
             "MoneyForm", "KonkursType", "FirstEgeName", "FirstEgeScore", "SecondEgeName",
             "SecondEgeScore", "ThirdEgeName", "ThirdEgeScore", "SumScore")]
#write.csv(x=jur, file="./postupleniye/2018/podano_jur.csv",sep = ";", quote = F)



# Как много заявок подаёт каждый человек?
howMany <- aggregate(jur, list(jur$Name), length)
howMany <- howMany[c("Group.1", "Name")]
howMany <- aggregate(Group.1 ~ Name, howMany, length)
categoriesN <- nrow(howMany)

ggplot(data = howMany, aes(x="", y=howMany$Group.1, fill=Name)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = Group.1/length(Group.1) + c(0, cumsum(Group.1)[-length(Group.1)]), 
                label = percent(Group.1/sum(Group.1))), size=4) +
  labs(title = "Процент людей, подавших N заявок на журналистику")




# Загрузка данных поступивших
postupilo_jur <- read.csv("./postupleniye/2018/postupilo.csv", sep = ";")
postupilo_jur$Wave <- as.factor(postupilo_jur$Wave)
# Валидация
err <- 0
for (recruitted in postupilo_jur$Name){
  if (!(recruitted %in% jur$Name)){
    print(recruitted)
  }
}
if (err != 0){
  print("Невозможно сделать join таблиц подавших и поступивших по приведённым выше людям")
}
# Теперь можно join'ить таблицы
jur_full_info <- merge(jur, postupilo_jur,  by = "Name", all.x = T)

# Очистить от повторных заявок
jur_full_info <- jur_full_info[!duplicated(jur_full_info$Name), ]




# Посмотрим на соотношение баллов принятых и не принятых
jur_full_info$Recruitted <- ifelse(is.na(jur_full_info$Recruitted), 0, 1)

acceptedScore <- jur_full_info[jur_full_info$Recruitted == 1, c("Name", "SumScore")]
rejectedScore <- jur_full_info[jur_full_info$Recruitted == 0, c("Name", "SumScore")]

ggplot() +
  geom_density(data=acceptedScore, aes(x = SumScore), fill = "green", alpha = 0.3) + 
  geom_density(data=rejectedScore, aes(x = SumScore), fill = "red", alpha = 0.3) + 
  xlim(90, 300) +
  labs(title = "Баллы тех, кого приняли (зелёный) и не приняли (красный) в 2018 году",
       xlab = "Суммарный балл",
       ylab = "Количество людей")

# То же только для контрактников
acceptedScore <- subset(jur_full_info, 
                        subset = (Recruitted == 1 & MoneyForm.x == "Договорная"), 
                        select =  c("Name", "SumScore", "FirstEgeScore", "SecondEgeScore"))
rejectedScore <- subset(jur_full_info, 
                        subset = (Recruitted == 0 & MoneyForm.x == "Договорная"), 
                        select =  c("Name", "SumScore", "FirstEgeScore", "SecondEgeScore"))
ggplot() +
  geom_density(data = acceptedScore, aes(x = FirstEgeScore + SecondEgeScore), fill = "green", alpha = 0.3) + 
  geom_density(data = rejectedScore, aes(x = FirstEgeScore + SecondEgeScore), fill = "red",   alpha = 0.3) +
  labs(title = "Баллы ЕГЭ тех, кого приняли (зелёный) и не приняли (красный) НА КОНТРАКТ в 2018 году",
       subtitle = "Чёрная линия - твой ЕГЭ",
       xlab = "Суммарный балл",
       ylab = "Количество людей") + 
  geom_vline(xintercept = 96 + 72)
  xlim(75, 200)




# Посчитаем зависимость Конкурсного балла от баллов егэ
jur_full_info$SumEge <- jur_full_info$FirstEgeScore + jur_full_info$SecondEgeScore
tvorchByEgeDf <- subset(jur_full_info, 
                        subset = (MoneyForm.x == "Договорная"), 
                        select = c("Name", "SumEge", "Recruitted", "ThirdEgeScore", "TeachForm.x"))

ggplot()+
  geom_point(data=subset(tvorchByEgeDf, Recruitted == 1), 
             aes(x=SumEge, y=ThirdEgeScore),
             color="darkgreen", size=2) + 
  geom_point(data=subset(tvorchByEgeDf, Recruitted == 0),
             aes(x=SumEge, y=ThirdEgeScore), color="red", size=2) + 
  
  geom_smooth(data=subset(tvorchByEgeDf, Recruitted == 1 & TeachForm.x == "очно-заочная"),
              aes(x = SumEge, y = ThirdEgeScore)) + 
  
  geom_vline(xintercept = (96 + 72), size=0.2) +
  labs(title="Зависимость баллов за творческий конкурс от баллов по ЕГЭ (рус+лит) в 2018 году",
       subtitle = "Цветом показан успех в поступлении\nЧёрная линия показывает твой результат\nСиняя показывает среднее по больнице") +
  xlab("Сумма баллов по ЕГЭ") + 
  ylab("Балл за творческий конкурс") +
  xlim(135, 195) +
  ylim(63, 100)



