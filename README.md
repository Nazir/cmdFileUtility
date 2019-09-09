![nazir.pro](http://nazir.pro/img/logo.png)

# cmdFileUtility v0.4 alpha #

About
-----
Command-line utility to work with files (for Windows).

Description:
---------
Using the transfer of parameters to the application (via the command line) performs the necessary operations with files.
Features:
1. Merging ("gluing together") several files into one.
2. Search strings in text.
3. Find and replace lines in text.
4. Conversion of the code page of the file.

> For convenience, you can create a batch file (*.bat).
> 
> Examples are located in the `Examples` folder.

Usage
-----
`cmdFileUtility [Option] [Option, [Option]...]`

```
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
    4. CodePageConvertor - Convert codepage input file and save them as output file. Use option -s for file containing cpALT, cpISO, cpKOI, cpMAC, cpWIN.  As well as encoding UTF-8 convert to UTF-8 with BOM or vice versa. Convert "CP1251"<->"UTF-8".
```

Repo owner
----------
> Copyright: Nazir © 2002-2019
>
> Programming:
>
> Developer: Khusnutdinov Nazir K. aka Naziron or Wild Pointer
>
> Email: Nazir@Nazir.pro
>
> Web: [nazir.pro][1]
>
> Email: Naziron@gmail.com
>
> Git: https://github.com/Nazir

[1]: http://nazir.pro

