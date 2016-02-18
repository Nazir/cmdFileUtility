program cmdFileUtility;

{$mode objfpc}{$H+}

{*****************************************************************}
{                                                                 }
{  Проект cmdFileUtility (Консольная утилит работы с файлами)     }
{                                                                 }
{  Copyright: Nazir © 2002-2016                                   }
{  Development: Nazir K. Khusnutdinov (aka Wild Pointer)          }
{  Разработчик: Хуснутдинов Назир Каримович  (Wild Pointer)       }
{  Create: 02.08.2012                                             }
{  Modify: 09.08.2012, 08.06.2014                                 }
{                                                                 }
{*****************************************************************}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , codepageconvertor;

type

  { TcmdFileUtility }

  TcmdFileUtility = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteAbout; virtual;
    procedure WriteError(AText: string; ATerminate: Boolean = True);
    function MergeFileStream( inFiles: TStringList; outFileName: string): boolean;
    function FindLines(inpFileName, outFileName: string; AFindList: TStringList): boolean;
    function CodePageConvertor(inpFileName, outFileName: string;
      ACodePage: TStringList): boolean;
    function ExtractFileNameEx( fName: string ): string;
    function GetSizeAllFiles( var x: array of string ): longint;
  end;

{ TcmdFileUtility }

procedure TcmdFileUtility.DoRun;
var
  ErrorMsg: string;
  optCommand, optInput, optOutput, optSettings: string;
  bInputIsPath, bOutputIsPath: Boolean;
  bSettingsIsString: Boolean;
  bSettingsFileList: Boolean;

  slSettings, slTemp: TStringList;

  iCounter: Integer;
  sTemp: string;
  //iTemp: Integer;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hcios', ['help', 'command', 'input', 'output', 'settings']);
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    WriteHelp;
    WriteLn('Press any key to exit!');
    ReadLn();
    Terminate;
    Exit;
  end;

  //
  bInputIsPath := False;
  bOutputIsPath := False;
  bSettingsIsString := False;
  bSettingsFileList := False;


  // parse parameters
  // Help
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  // Command
  if HasOption('c', 'command') then
    optCommand := GetOptionValue('c', 'command');
  // Input
  if HasOption('i', 'input') then
    optInput := GetOptionValue('i', 'input');
  // Output
  if HasOption('o', 'ouput') then
    optOutput := GetOptionValue('o', 'ouput');
  // Settings
  if HasOption('s', 'settings') then
    optSettings := GetOptionValue('s', 'settings');

  // Command = merge ===========================================================
  // Example: -c merge -s FilesList.txt -o Output\Result.txt
  if optCommand = 'merge' then
  begin
    WriteLn('Command: ', optCommand);
    bSettingsFileList := True;
    bInputIsPath := False;
    bOutputIsPath := False;
  end;

  // Command = FindLines =======================================================
  // Example:  -c FindLines -i FindLinesInput.txt -o RESULT\cmdFileUtility.Result.txt -s FindLines.txt
  if optCommand = 'FindLines' then
  begin
    WriteLn('Command: ', optCommand);
    bSettingsFileList := True;
    bInputIsPath := False;
    bSettingsIsString := False;
    bOutputIsPath := False;
  end;

  // Command = CodePageConvertor =======================================================
  // Example:  -c CodePageConvertor -i CodePageConvertorInput.txt -o RESULT\CodePageConvertorOutput.txt -s CodePageConvertor.txt
  //cpALT,cpISO,cpKOI,cpMAC,cpWIN
  if optCommand = 'CodePageConvertor' then
  begin
    WriteLn('Command: ', optCommand);
    bSettingsFileList := True;
    bInputIsPath := False;
    bSettingsIsString := False;
    bOutputIsPath := False;
  end;


  // Input check ===============================================================
  if not (optInput = EmptyStr) then
  if bInputIsPath then
  begin
    if DirectoryExists(optInput) then
      WriteLn('Input directory is "', optInput, '"')
    else
      WriteError('Error: Input directory "' + optInput + '" is not exists!');
  end
  else
  begin
    if FileExists(optInput) then
      WriteLn('Input file is "', optInput, '"')
    else
      WriteError('Error: Input file "' + optInput + '" is not exists!');
  end;

  // Output check ==============================================================
  if bOutputIsPath then
  begin
    if optOutput <> EmptyStr then
    if not DirectoryExists(optOutput) then
      if not ForceDirectories(optOutput) then
        optOutput := ExtractFilePath(optInput);
    if not DirectoryExists(optOutput) then
      WriteError('Error: Output directory "' + optOutput + '" is not exists!');
  end
  else
  begin
    if optOutput = EmptyStr then
      optOutput := optInput + '.output'; // Default output file extension

    if FileExists(optOutput) then
      WriteLn('Output file is "', optOutput, '"');
  end;

  // Settings check ==============================================================
  if not (optSettings = EmptyStr) then
  if not bSettingsIsString then
  begin
    if FileExists(optSettings) then
      WriteLn('Settings file is "', optSettings, '"')
    else
      WriteError('Error: Settings file "' + optSettings + '" is not exists!');
  end;
  if bSettingsFileList then
    WriteLn('Settings file is list.');

  // Settings file is list ========================================================
  if bSettingsFileList then
  begin
    slSettings := TStringList.Create;

    slTemp := TStringList.Create;
    slTemp.LoadFromFile(optSettings);

    WriteLn('Settings:');
    for iCounter := 0 to slTemp.Count - 1 do
    begin
      sTemp := Trim(slTemp.Strings[iCounter]);
      if (sTemp <> EmptyStr) and (LeftStr(sTemp, 1) <> '#') then
      begin
        Write(IntToStr(iCounter + 1), '. "', sTemp, '"');
        slSettings.Append(sTemp);
        WriteLn();
      end;
    end;

    slTemp.Free;

    if slSettings.Count > 0 then
    begin
      WriteLn();
      if optOutput = EmptyStr then
      begin
        Write('Enter output file name [default:', optOutput, ']:');
        ReadLn(sTemp);
        if sTemp <> EmptyStr then
          optOutput := sTemp;
        WriteLn();
      end;
      WriteLn('Result:');
      // =========================
      if optCommand = 'merge' then
      begin
        if MergeFileStream(slSettings, optOutput) then
          WriteLn('Merge file is "', optOutput, '"');
      end;
      // =============================
      if optCommand = 'FindLines' then
      begin
        if FindLines(optInput, optOutput, slSettings) then
          WriteLn('Result file is "', optOutput, '"');
      end;
      // =============================
      if optCommand = 'CodePageConvertor' then
      begin
        if CodePageConvertor(optInput, optOutput, slSettings) then
          WriteLn('Result file is "', optOutput, '"');
      end;
    end;

    slSettings.Free;
  end;

//  ============================================================================
  WriteLn('_________________________');


  if optCommand = EmptyStr then
  begin
    WriteHelp;
    ReadLn();
  end;

  WriteAbout;

  WriteLn('');
  Write('Press any key to exit!');

  // stop program loop
  Terminate;
end;

constructor TcmdFileUtility.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TcmdFileUtility.Destroy;
begin
  inherited Destroy;
end;

procedure TcmdFileUtility.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExtractFileName(ExeName), ' [Option] [Option, [Option]...]');
  WriteLn('  Option:');
  WriteLn('    -h, help     - Help (show this)');
  WriteLn('    -c, command  - Command (see below)');
  WriteLn('    -i, input    - Input file or directory');
  WriteLn('    -o, output   - Output file or directory');
  WriteLn('    -s, settings - Settings file or string');
  WriteLn('  Command:');
  WriteLn('    1. merge - Merge files in list. Use option -i for file containing a list.');
  WriteLn('    2. FindLines - Find lines in input file and copy them in output file. Use option -s for file containing a list find string.');
  WriteLn('    3. CodePageConvertor - Convert codepage input file and save them as output file. Use option -s for file containing  cpALT, cpISO, cpKOI, cpMAC, cpWIN.');
  WriteLn('');
//  WriteLn('Example:');
//  WriteLn(ExtractFileName(ExeName), ' -c merge -s FilesList.txt -o RESULT\');
//

  WriteLn('');
  WriteAbout;
end;

procedure TcmdFileUtility.WriteAbout;
begin
  //CharToOem
  WriteLn('Help: ', ExtractFileName(ExeName),' -h');
  WriteLn;
  WriteLn('About:');
  WriteLn('cmdFileUtility v0.1 alpha (24.09.2014)');
  WriteLn('Copyright: Nazir (c) 2002-2016');
  WriteLn('Programming:');
  WriteLn('Nazir K. Khusnutdinov aka Wild Pointer');
  WriteLn('Email: Nazir@ovvio.pro');
  WriteLn('WEB: http://Nazir.ovvio.pro');
  WriteLn('Email: Naziron@gmail.com');
  WriteLn('Git: https://github.com/Nazir');
end;

procedure TcmdFileUtility.WriteError(AText: string; ATerminate: Boolean = True);
begin
  WriteLn(AText);
  WriteLn();
  WriteHelp;
  ReadLn();
  if ATerminate then
    Terminate;
end;


(* Функция для склейки файла *)
// http://decoding.narod.ru/practic/cutfile/cutfile.html
function TcmdFileUtility.MergeFileStream( inFiles: TStringList; outFileName: string): boolean;
var
  inStream: TFileStream;  // Входной файловый поток
  outStream: TFileStream; // Выходной файловый поток
//  outFileName: string;    // Шаблон выходного имени файла
  recFirstFile: Boolean;  // Признак записи первого файла
  i: integer;             // Цикловая переменная

begin
   // Инициализируем переменные
{   if PB <> nil then
   begin
      PB.Position := 0;
      PB.Max := GetSizeAllFiles( inFiles );
   end; //}
   Result := true;
   recFirstFile := false;

   // Здесь происходит склейка файлов
   try
      for i := 0 to inFiles.Count - 1 do
      begin
         if FileExists(inFiles[i]) then
         begin
           inStream := TFileStream.Create( inFiles[i], fmOpenRead );

           // При записи первой части файла выходной файл создаеитс,
           // при записи всех последующих выходной файл открывается на запись
           if recFirstFile then
              outStream := TFileStream.Create( outFileName, fmOpenWrite )
           else
           begin
              outStream := TFileStream.Create( outFileName, fmCreate );
              recFirstFile := true;
           end;

           outStream.Position := outStream.Size;
           outStream.CopyFrom( inStream, inStream.Size );

           {if PB <> nil then
              PB.Position := outStream.Size + inStream.Size; //}

           WriteLn(IntToStr(i + 1), '. Current size ', outStream.Size + inStream.Size, ' bytes');

           outStream.Free;
           inStream.Free;
           //Application.ProcessMessages;
         end;
      end;
   except
      Result := false;
   end;

//   if PB <> nil then PB.Position := 0;
end;

function TcmdFileUtility.FindLines(inpFileName, outFileName: string;
  AFindList: TStringList): boolean;
var
  slInput, slOutput: TStringList;
  iCounter, iCounter2: Integer;
  sFind: String;
begin
  Result := False;
  if not FileExists(inpFileName) then
   Exit;

  slInput := TStringList.Create;
  slInput.LoadFromFile(inpFileName);
  slOutput := TStringList.Create;

  for iCounter := 0 to AFindList.Count - 1 do
  begin
    sFind := AFindList.Strings[iCounter];
    for iCounter2 := 0 to slInput.Count - 1 do
    begin
      if Pos(sFind, slInput.Strings[iCounter2]) > 0 then
       slOutput.Append(slInput.Strings[iCounter2]);
    end;
  end;

  slInput.Free;
  slOutput.SaveToFile(outFileName);
  slOutput.Free;

  Result := True;
end;

function TcmdFileUtility.CodePageConvertor(inpFileName, outFileName: string;
  ACodePage: TStringList): boolean;
var
  Convertor: TCodePageConvertor;
  slInput, slOutput: TStringList;
  iCounter: Integer;
begin
  Result := False;

  if not FileExists(inpFileName) then
   Exit;

  //  cpALT,cpISO,cpKOI,cpMAC,cpWIN
  Convertor := TCodePageConvertor.Create(nil);
  if ACodePage.Strings[0] = 'cpALT' then
    Convertor.InputCodePage := cpALT;
  if ACodePage.Strings[0] = 'cpISO' then
    Convertor.InputCodePage := cpISO;
  if ACodePage.Strings[0] = 'cpKOI' then
    Convertor.InputCodePage := cpKOI;
  if ACodePage.Strings[0] = 'cpMAC' then
    Convertor.InputCodePage := cpMAC;
  if ACodePage.Strings[0] = 'cpWIN' then
    Convertor.InputCodePage := cpWIN;

  if ACodePage.Strings[1] = 'cpALT' then
    Convertor.OutputCodePage := cpALT;
  if ACodePage.Strings[1] = 'cpISO' then
    Convertor.OutputCodePage := cpISO;
  if ACodePage.Strings[1] = 'cpKOI' then
    Convertor.OutputCodePage := cpKOI;
  if ACodePage.Strings[1] = 'cpMAC' then
    Convertor.OutputCodePage := cpMAC;
  if ACodePage.Strings[1] = 'cpWIN' then
    Convertor.OutputCodePage := cpWIN;

  slInput := TStringList.Create;
  slInput.LoadFromFile(inpFileName);
  slOutput := TStringList.Create;

  for iCounter := 0 to slInput.Count - 1 do
  begin
    Convertor.InputString := slInput.Strings[iCounter];
    slOutput.Append(Convertor.OutputString);
  end;

  slInput.Free;
  slOutput.SaveToFile(outFileName);
  slOutput.Free;
  Convertor.Free;
  Result := True;
end;

// В отличие от стандартной функции ExtractFileName, ExtractFileNameEx (ее код приведен ниже) возвращает имя файла без расширения. Если необходимо показать ход выполнения операции склеивания файлов в ProgressBar1, то функция GetSizeAllFiles (она так же приведена ниже) позволит определить размер конечного файла, а, следовательно, и ProgressBar1.Max.

(* Функция возвращает имя файла без расширения *)
function TcmdFileUtility.ExtractFileNameEx( fName: string ): string;
begin
   if FileExists( fName ) then
   begin
      Result := ExtractFileName( fName );
      Delete( Result, LastDelimiter( '.', Result ), Length( Result ) );
   end
   else
      Result := '';
end;

(* Функция находит общий размер склеиваемых файлов *)
function TcmdFileUtility.GetSizeAllFiles( var x: array of string ): longint;
var
  i: integer;
  sr: TSearchRec;
begin
   Result := 0;
   for i := Low( x ) to High( x ) do
      if FileExists( x[i] ) then
      begin
         FindFirst( x[i], faAnyFile, sr );
         Result := Result + sr.Size;
         FindClose( sr );
      end;
end;

var
  Application: TcmdFileUtility;

{$R *.res}

begin
  Application := TcmdFileUtility.Create(nil);
  Application.Run;
  Application.Free;
end.

