unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LConvEncoding,
  codepageconvertor;

function ExtractFileNameEx( fName: string ): string;
function GetSizeAllFiles( var x: array of string ): longint;
function CodePageConvertor(inpFileName, outFileName: string;
  ACodePage: TStringList): boolean;
function ReplaceExp(inpText: String): String;


implementation

// В отличие от стандартной функции ExtractFileName, ExtractFileNameEx (ее код приведен ниже) возвращает имя файла без расширения. Если необходимо показать ход выполнения операции склеивания файлов в ProgressBar1, то функция GetSizeAllFiles (она так же приведена ниже) позволит определить размер конечного файла, а, следовательно, и ProgressBar1.Max.

(* Функция возвращает имя файла без расширения *)
function ExtractFileNameEx( fName: string ): string;
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
function GetSizeAllFiles( var x: array of string ): longint;
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

function CodePageConvertor(inpFileName, outFileName: string;
  ACodePage: TStringList): boolean;
var
  Convertor: TCodePageConvertor;
  slInput, slOutput: TStringList;
  iCounter: Integer;
begin
   { TODO -oNazir -cCodePageConvertor :
     const
       MyName: AnsiString = #$C7#$E5;
     begin
       DefaultSystemCodePage := GetACP;
       Edit1.Text := AnsiToUtf8(MyName);
     end;
   }
  Result := False;
  if not FileExists(inpFileName) then
    Exit;

  slInput := TStringList.Create;
  slInput.SkipLastLineBreak := True;
  slInput.LoadFromFile(inpFileName);

  slOutput := TStringList.Create;
  slOutput.SkipLastLineBreak := True;

  if ACodePage.Strings[0] = 'UTF8BOM' then
  begin
     if ACodePage.Strings[1] = 'UTF8' then
     begin
        slOutput.Append(UTF8BOMToUTF8(slInput.Text));
        slInput.Free;
        slOutput.SaveToFile(outFileName);
        slOutput.Free;
        Result := True;
        Exit;
     end;
  end;

  if ACodePage.Strings[0] = 'UTF8' then
  begin
     if ACodePage.Strings[1] = 'UTF8BOM' then
     begin
       // BOM representation: hexadecimal - EF BB BF decimal - 239 187 191
       // Solution #1
       slOutput.Append(UTF8ToUTF8BOM(slInput.Text));
       slInput.Free;
       slOutput.SaveToFile(outFileName);
       slOutput.Free;

       // Solution #2
       //var
       //  fsInput: TFileStream;
       //  fsOutput: TFileStream;
       //  ChrBuffer: array[0..2] of char;

       //ChrBuffer[0] := Chr(239);
       //ChrBuffer[1] := Chr(187);
       //ChrBuffer[2] := Chr(191);
       //try
       //  fsInput := TFileStream.Create(inpFileName, fmOpenRead);
       //  fsOutput := TFileStream.Create(outFileName, fmCreate);
       //  fsOutput.  Write(ChrBuffer, sizeof(ChrBuffer));
       //  fsOutput.CopyFrom(fsInput, fsInput.Size);
       //  fsInput.Free;
       //  fsOutput.Free;
       //except
       //  on E:Exception do
       //    writeln(E.Message);
       //end;

       Result := True;
       Exit;
     end;
  end;

  if ACodePage.Strings[0] = 'CP1251' then
  begin
     if ACodePage.Strings[1] = 'UTF8' then
     begin
        slOutput.Append(CP1251ToUTF8(slInput.Text));   // cyrillic
        slInput.Free;
        slOutput.SaveToFile(outFileName);
        slOutput.Free;
        Result := True;
        Exit;
     end;
  end;

  if ACodePage.Strings[0] = 'UTF8' then
  begin
     if ACodePage.Strings[1] = 'CP1251' then
     begin
        slOutput.Append(UTF8ToCP1251(slInput.Text));   // cyrillic
        slInput.Free;
        slOutput.SaveToFile(outFileName);
        slOutput.Free;
        Result := True;
        Exit;
     end;
  end;

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

function ReplaceExp(inpText: String): String;
begin
  inpText := StringReplace(inpText, '\r', #13, [rfReplaceAll]);
  inpText := StringReplace(inpText, '\n', #10, [rfReplaceAll]);
  inpText := StringReplace(inpText, '\t', #9, [rfReplaceAll]);
  inpText := StringReplace(inpText, '\0', #0, [rfReplaceAll]);
  { TODO -oNazir : Unicode support: inpText := StringReplace(inpText, '\x...', #..., [rfReplaceAll]) }
  Result := inpText;
end;

end.

