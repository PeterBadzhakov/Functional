Средата е хеш таблица на символи и оценки
Пр. в C++ стековата рамка е среда. Той записва двойки адрес-стойност,
а в scheme хеш мап.
Функциите се свързват с функционални обекти (clojure)
и интерпретират в реално време. Може да се ползват в други среди.
Формалният параметър е този в дефиницията, а
фактическия е стойността в извикването.
Средите са организирани в дърво (дете -> родител) - 
функциите не затриват средите, към
които сочат. GC евентуално изчиства ненужното.
Чрез указателят за среда затваряме свободните променливи в тялото
на всяка функция. Така придаваме контекст.
Рекурсията е позоваване на себе си - по-общо от
"функция, която извиква себе си".

F(x) = 1 + F(x - 1)
-> F(x) = x => 1 + x - 1
-> F(x) = x + c => x + c = 1 + x + c - 1
Кое е по-малко решение?

Празната функция! Множеството от аргументи-решения е празното множество.
Винаги ли работи?
-> Във факториел имаме база fact(0) = 1, тоест (0, 1) винаги присъства.
--> Противоречие.
Празна функция = забиване. Винаги ни трябва база.

Факториелът е линеен рекурсивен процес - гафиката на извиквания е диамант.
Разширяват се оригиналните среди на функцията, а не последователно
при всяко извикване като стек.

Линеен итеративен процес - не диамант, а линия.

(* n ...) -> рекурсивен процес, стекова рекурсия, отложена операция
(for n r i) -> итеративен процес, опашкова рекурсия, директно връщане на резултат

Във for n не се променя, да го махнем? Да, вложени дефиниции.

Функция разширява оригиналната среда, а let - текущата.