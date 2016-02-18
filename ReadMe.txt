About:
cmdFileUtility v0.1 - Консольная утилита работы с файлами для Windows
Copyright: Nazir © 2002-2016
Programming:
Khusnutdinov Nazir K. aka Wild Pointer
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
3. Преобразование кодовой страницы файла.

Для удобства можно сформировать пакетный файл (*.bat)

___________________
Usage: cmdFileUtility [Option] [Option, [Option]...]
  Option:
    -h, help     - Help (show this)
    -c, command  - Command (see below)
    -i, input    - Input file or directory
    -o, output   - Output file or directory
    -s, settings - Settings file or string
  Command:
    1. merge - Merge files in list. Use option -i for file containing a list.
    2. FindLines - Find lines in input file and copy them in output file. Use option -s for file containing a list find string.
    3. CodePageConvertor - Convert codepage input file and save them as output file. Use option -s for file containing  cpALT, cpISO, cpKOI, cpMAC, cpWIN.

___________________

Example:

cmdFileUtility.FindLines.bat - пакетный файл :

cmdRegCleaner.exe HKLM "Software\Nszir Software\HM"
cmdRegCleaner.exe HKLU "Software\Nazir Software\HM"
pause

