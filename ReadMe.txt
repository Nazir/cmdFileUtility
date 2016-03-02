About:
cmdFileUtility v0.2 - Консольная утилита работы с файлами для Windows
Copyright: Nazir © 2002-2016
Programming:
Khusnutdinov Nazir K. aka Naziron or Wild Pointer
Email: Nazir@ovvio.pro
WEB: http://Nazir.ovvio.pro
Email: Naziron@gmail.com
Git: https://github.com/Nazir
___________________
Описания:
Консольная файловая утилита.
С помощью передачи параметров приложению (через командную строку) выполняет необходимые операции с файлами.
Возможности:
1. Объединение ("склейка") нескольких файлов в один.
2. Поиск строк в тексте.
3. Поиск и замена строк в тексте.
4. Преобразование кодовой страницы файла.

Для удобства можно сформировать пакетный файл (*.bat)

___________________
Usage: cmdFileUtility [Option] [Option, [Option]...]
  Option:
    -h or --help     - Help (show this)
    -x or --silent   - Silent mode
    -c or --command  - Command (see below)
    -i or --input    - Input file or directory
    -o or --output   - Output file or directory
    -s or --settings - Settings file or string
  Command:
    1. merge - Merge files in list. Use option -i for file containing a list.
    2. FindLines - Find lines in input file and copy them in output file. Use option -s for file containing a list find string.
    3. replace.
    4. CodePageConvertor - Convert codepage input file and save them as output file. Use option -s for file containing  cpALT, cpISO, cpKOI, cpMAC, cpWIN.


