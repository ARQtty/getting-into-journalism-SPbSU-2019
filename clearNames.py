s = '''
1.1. Абуталипова Дарья Руслановна, 272,00 баллов

1.2. Андреева Дария Максимовна, 269,00 баллов

1.3. Архипова Елизавета Сергеевна, 248,00 баллов

1.4. Бареева Александра Денисовна, 272,00 баллов

1.5. Генерова Елизавета Георгиевна, 267,00 баллов

1.6. Добрякова Ярослава Евгеньевна, 275,00 баллов

1.7. Есипенко Софья Романовна, 246,00 баллов

1.8. Зимова Светлана Валерьевна, 276,00 баллов

1.9. Иванец Александр Сергеевич, 269,00 баллов

1.10. Кандинская Галина Игоревна, 277,00 баллов

1.11. Капитанская Анастасия Сергеевна, 258,00 баллов

1.12. Козлова Валерия Алексеевна, 268,00 баллов

1.13. Комиссарова Ангелина Михайловна, 251,00 баллов

1.14. Куканова Дарья Петровна, 229,00 баллов
Мазуренко Мария Сергеевна, 291,00 баллов
Овчаренко Анастасия Андреевна, 228,00 баллов
Пак София Олеговна, 248,00 баллов

Петровская Мария Александровна, 229,00 баллов
Поликарпова Анна Антоновна, 236,00 баллов
Полищук Маргарита Петровна, 265,00 баллов
Сафонова Дарья Ильинична, 262,00 баллов
Свириденко Леонид Михайлович, 254,00 баллов
Федчун Алиса Борисовна, 198,00 баллов
Филатова Александра Евгеньевна, 260,00 баллов
Якимова Ирина Сергеевна, 260,00 баллов
'''

addition = ";1;очно-заочная;Договорная;3"

for string in s.split("\n"):
	
	if len(string) == 0:
		continue

	string = string.split(",")[0] # до баллов
	string = string.split(".")[-1]# после нумерации
	string = string.lstrip()

	print(string + addition)
