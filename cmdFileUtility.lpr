program cmdFileUtility;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Project cmdFileUtility                                                      }
{  Проект cmdFileUtility (Консольная утилита работы с файлами)                 }
{                                                                              }
{  Copyright: Nazir © 2002-2018                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович  (Naziron or Wild Pointer)         }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Create: 02.08.2012                                                          }
{  Modify: 09.08.2012, 08.06.2014, 02.03.2016, 20.11.2018                      }
{                                                                              }
{******************************************************************************}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , Utils;

type

  { TcmdFileUtility }

  TcmdFileUtility = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    bSilentMode: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteLine(AStr: String = '');
    procedure WriteHelp; virtual;
    procedure WriteAbout; virtual;
    procedure WriteError(AText: string; ATerminate: Boolean = True);
    function MergeFileStream( inFiles: TStringList; outFileName: string): boolean;
    function FindLines(inpFileName, outFileName: string; AFindList: TStringList): boolean;
    function Replace(inpFileName, outFileName: string; AReplaceList: TStringList): boolean;
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
  WriteLine();
  // quick check parameters
  ErrorMsg := CheckOptions('hxcios', ['help', 'silent', 'command', 'input', 'output', 'settings']);
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    WriteHelp;
    WriteAbout;
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
  // Silent
  bSilentMode := HasOption('x', 'silent');
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

  case optCommand of
    // Command = merge =========================================================
    // Example: -c merge -s FilesList.txt -o Output\Result.txt
    'merge':
    begin
      WriteLine('Command: ' + optCommand);
      bSettingsFileList := True;
      bInputIsPath := False;
      bOutputIsPath := False;
    end;
    // Command = FindLines =====================================================
    // Example:  -c FindLines -i FindLinesInput.txt -o RESULT\cmdFileUtility.FindLines.Result.txt -s FindLines.txt
    'FindLines':
    begin
      WriteLine('Command: ' + optCommand);
      bSettingsFileList := True;
      bInputIsPath := False;
      bSettingsIsString := False;
      bOutputIsPath := False;
    end;
    // Command = replace =======================================================
    // Example:  -c replace -i ReplaceInput.txt -o RESULT\cmdFileUtility.Replace.Result.txt -s Replace.txt
    'replace':
    begin
      WriteLine('Command: ' + optCommand);
      bSettingsFileList := True;
      bInputIsPath := False;
      bSettingsIsString := False;
      bOutputIsPath := False;
    end;
    // Command = CodePageConvertor =============================================
    // Example:  -c CodePageConvertor -i CodePageConvertorInput.txt -o RESULT\cmdFileUtility.CodePageConvertor.Result.txt -s CodePageConvertor.txt
    // cpALT, cpISO, cpKOI, cpMAC, cpWIN
    'CodePageConvertor':
    begin
      WriteLine('Command: ' + optCommand);
      bSettingsFileList := True;
      bInputIsPath := False;
      bSettingsIsString := False;
      bOutputIsPath := False;
    end;
    else
      WriteHelp;
      WriteLn('');
      Write('Press any key to exit!');
      ReadLn();
      // stop program loop
      Terminate;
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
      WriteLine('Input file is "' + optInput + '"')
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
      WriteLine('Output file is "' + optOutput + '"');
  end;

  // Settings check ==============================================================
  if not (optSettings = EmptyStr) then
  if not bSettingsIsString then
  begin
    if FileExists(optSettings) then
      WriteLine('Settings file is "' + optSettings + '"')
    else
      WriteError('Error: Settings file "' + optSettings + '" is not exists!');
  end;
  if bSettingsFileList then
    WriteLine('Settings file is list.');

  // Settings file is list ========================================================
  if bSettingsFileList then
  begin
    slSettings := TStringList.Create;

    slTemp := TStringList.Create;
    slTemp.LoadFromFile(optSettings);

    WriteLine('Settings:');
    for iCounter := 0 to slTemp.Count - 1 do
    begin
      sTemp := slTemp.Strings[iCounter];
      if (sTemp <> EmptyStr) and (LeftStr(sTemp, 1) <> '#') then
      begin
        WriteLine(IntToStr(iCounter + 1) + '. "' + sTemp + '"');
        WriteLine();
        slSettings.Append(sTemp);
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
      WriteLine('Result:');
      // =============================
      if optCommand = 'merge' then
      begin
        if MergeFileStream(slSettings, optOutput) then
          WriteLine('Merge file is "' + optOutput + '"');
      end;
      // =============================
      if optCommand = 'FindLines' then
      begin
        if FindLines(optInput, optOutput, slSettings) then
          WriteLine('Result file is "' + optOutput + '"');
      end;
      // =============================
      if optCommand = 'replace' then
      begin
        if Replace(optInput, optOutput, slSettings) then
          WriteLine('Result file is "' + optOutput + '"');
      end;
      // =============================
      if optCommand = 'CodePageConvertor' then
      begin
        if CodePageConvertor(optInput, optOutput, slSettings) then
          WriteLine('Result file is "' + optOutput + '"');
      end;
    end;

    slSettings.Free;
  end;

//  ============================================================================
  if not bSilentMode then
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

procedure TcmdFileUtility.WriteLine(AStr: String = '');
begin
  if not bSilentMode then
    WriteLn(AStr);
end;

procedure TcmdFileUtility.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExtractFileName(ExeName), ' [Option] [Option, [Option]...]');
  WriteLn('  Option:');
  WriteLn('    -h or --help     - Help (show this)');
  WriteLn('    -x or --silent   - Silent mode');
  WriteLn('    -c or --command  - Command (see below)');
  WriteLn('    -i or --input    - Input file or directory');
  WriteLn('    -o or --output   - Output file or directory');
  WriteLn('    -s or --settings - Settings file or string');
  WriteLn('  Command:');
  WriteLn('    1. merge - Merge files in list. Use option -i for file containing a list.');
  WriteLn('    2. FindLines - Find lines in input file and copy them in output file. Use option -s for file containing a list find string.');
  WriteLn('    3. replace - Find and replace strings in input file. Use option -s for file containing a list find string=replace string.');
  WriteLn('    4. CodePageConvertor - Convert codepage input file and save them as output file. Use option -s for file containing  cpALT, cpISO, cpKOI, cpMAC, cpWIN.');
  WriteLn('');
//  WriteLn('Example:');
//  WriteLn(ExtractFileName(ExeName), ' -c merge -s FilesList.txt -o RESULT\');

  WriteLn();
  WriteAbout;
end;

procedure TcmdFileUtility.WriteAbout;
begin
  //CharToOem
  WriteLn('___________________________');
  WriteLn('Help: ', ExtractFileName(ExeName), ' -h');
  WriteLn();
  WriteLn('About:');
  WriteLn('cmdFileUtility v0.3 alpha (20.11.2018)');
  WriteLn('Copyright: Nazir (c) 2002-2018');
  WriteLn('Programming:');
  WriteLn('Nazir K. Khusnutdinov aka Wild Pointer');
  WriteLn('Email: Nazir@Nazir.pro');
  WriteLn('WEB: http://Nazir.pro');
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
function TcmdFileUtility.MergeFileStream(inFiles: TStringList; outFileName: string): Boolean;
var
  inStream: TFileStream;  // Входной файловый поток
  outStream: TFileStream; // Выходной файловый поток
  //outFileName: string;    // Шаблон выходного имени файла
  recFirstFile: Boolean;  // Признак записи первого файла
  i: Integer;             // Цикловая переменная

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
       if Pos('#', sFind) <> 1 then
       begin
         if Pos(sFind, slInput.Strings[iCounter2]) > 0 then
           slOutput.Append(slInput.Strings[iCounter2]);
       end;
    end;
  end;

  slInput.Free;
  slOutput.SaveToFile(outFileName);
  slOutput.Free;

  Result := True;
end;

function TcmdFileUtility.Replace(inpFileName, outFileName: string;
  AReplaceList: TStringList): boolean;
var
  slInput, slOutput: TStringList;
  iCounter: Integer;
  sFind, sReplace : String;
begin
  Result := False;
  if not FileExists(inpFileName) then
    Exit;

  slInput := TStringList.Create;
  slInput.LoadFromFile(inpFileName);
  slOutput := TStringList.Create;
  slOutput.Text := slInput.Text;

  for iCounter := 0 to AReplaceList.Count - 1 do
  begin
    sFind := AReplaceList.Strings[iCounter];
    if Pos('#', sFind) <> 1 then
    begin
      sFind := LeftStr(sFind, Pos('=', sFind) - 1);
      sFind := ReplaceExp(sFind);
      sReplace := AReplaceList.Strings[iCounter];
      sReplace := Copy(sReplace, Pos('=', sReplace) + 1, Length(sReplace));
      sReplace := ReplaceExp(sReplace);
      //sReplace := TrimRight(sReplace);
      slOutput.Text := StringReplace(slOutput.Text, sFind, sReplace, [rfIgnoreCase, rfReplaceAll]);
    end;
  end;

  slInput.Free;
  slOutput.SaveToFile(outFileName);
  slOutput.Free;

  Result := True;
end;

var
  Application: TcmdFileUtility;

{$R *.res}

begin
  Application := TcmdFileUtility.Create(nil);
  Application.Title := ExtractFileName(Application.ExeName);
  Application.Run;
  Application.Free;
end.

