# fastparsetry

## Задание 1
### Описание
Надо написать на Scala c использованием fastparse приложение которое будет читать файл table.txt, производить парсинг имеющейся там таблицы и возвращать двумерную мартицу (в структуре Seq[Seq[String]]) только прямоугольной части таблицы. По факту на выходе будет исходная таблица без строк заголовка и подвала.
 
### Детали
Все элементы таблица разделены TAB. Количество элементов в каждой стоке одинаково, так же как и в каждом столбце. В заголовке и подвале нет символов TAB. Заголовок и подвал могут иметь любое (>=0) количество строк.
 
### Указание по реализации
Эту задачу гораздо проще реализовать манипуляциями со строками чем с помощью fastparse, я понимаю это, но сейчас у нас цель – выработать навык. С помощью fastparse эту задачу следует решать в три этапа:
Описать на fastparse все составные элементы наших данных: элемент таблицы, строка элементов, набор строк элементов формирующих таблицу, строку заголовка/ подвала
Объединить полученные парсеры в единую структуру указывая обязательность и повторяемость некоторых элементов что бы получить парсер всей таблицы
Придумать различные варианты того как может выглядеть таблица и написать на эти случаи тесты

## Задание 2
### Описание
Это задание будет промежуточным и будет состоять из нескольких задач:
1) Изменить парсер таблиц так что бы он мог находить таблицу в тексте, содержащем больше чем только таблицу
2) Написать парсер "текст" - одна или больше строк текста идущие подряд, пускай возвращает лист строк (строка - строка)
3) Придумай свою модель данных для данных возвращаемых парсерами из 1) и 2) и используй ее. 
4) Написать парсер описывающий содержимое файла "file.text" от начала и до конца
### Указание по реализации
Таким образом на выходе у тебя должна быть последовательность объектов модели данных. Выводим в консоль

## Задание 3
### Описание
Реализовать универскально приложение для парсинга текстов, в который помимо исходого файла, нужно передать конфигурационный файл
на собственном языке программирования(конфигурирования). Языр будет содержать на данный момент всего два оператора "table" и "text".
В ресурсах можно найти файл "config.txt" который описывает правила парсинга файла "file.txt" из пердыущего задания.
### Указание по реализации
Таким образом ты будешь использовать fastparse дважды, сперва для парсинга конфигурационного файла, затем, 
зная операторы и выстроив написанные тобой парсеры в нужном поарядке, для парсинга исходного файла.

## Задание 4
### Описание
Усложняем процесс парсинга, он будет состоять из 3х этапов:
1) Парсинг config.txt, если он форматирован не верно - выводим ошибку и выходим
2) На основе результатов первого шага строим последовательность функции (парсеров) для парсинга file.txt
3) На основе результатов первого шага парсим file.txt и выводим результаты
