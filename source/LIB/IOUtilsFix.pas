{ *******************************************************

  Additional functionality to IOUtils unit

  2012 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

unit IOUtilsFix;

interface

uses
  System.IOUtils;

type
  TPathFixHelper = record helper for TPath
  public
    // Set or remove an Extended prefix
    class function SetExtendedPrefix(const Path: string;
      prefix: TPathPrefixType = TPathPrefixType.pptExtended): string; static;
    // Transform path\filename.ext to path\prefixfilenamesuffix.ext
    class function AppendToFilename(const Filename, prefix, suffix: string)
      : string; static;
    // Check whether path to file or folder is well-formed
    class function isWellFormed(const Filename: string;
      isDirectory: boolean = false; useWildcards: boolean = false)
      : boolean; static;
    // Replace directory part of full filename
    class function ReplaceDirectory(const Filename, NewPath: string)
      : string; static;
    // Replace file part of full filename
    class function ReplaceFileName(const Filename, NewName: string)
      : string; static;
    // Replace extension part of full filename
    class function ReplaceExtension(const Filename, NewExt: string)
      : string; static;
  end;

{$IFDEF MSWINDOWS}

  TFileCopyProgressEvent = procedure(TotalFileSize, TotalBytesTransferred
    : int64; var CancelOperation: boolean) of object;

  TFileFixHelper = record helper for TFile
  public
    // Check whether a file can be created, but does not create it
    class function CanCreate(Filename: string): boolean; static;
    class function Copy(const SourceFileName, DestFileName: string;
      ProgressCallback: TFileCopyProgressEvent; const Overwrite: boolean = true)
      : boolean; overload; static;
    // Retrieve file size or -1 if unable to access
    class function GetFileSize(const Filename: string): int64; static;
  end;
{$ENDIF}

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  StrUtils;

// -----------------------------------------------------------------------------
// TPathFixHelper
// -----------------------------------------------------------------------------

class function TPathFixHelper.SetExtendedPrefix(const Path: string;
  prefix: TPathPrefixType): string;
begin
  case prefix of
    TPathPrefixType.pptNoPrefix:
      begin
        if (TPath.IsExtendedPrefixed(Path)) and (TPath.IsUNCRooted(Path)) then
          Result := RightStr(Path, Length(Path) - 7)
        else if (TPath.IsExtendedPrefixed(Path)) then
          Result := RightStr(Path, Length(Path) - 4)
        else
          Result := Path
      end;
    TPathPrefixType.pptExtended, TPathPrefixType.pptExtendedUNC:
      begin
        if (TPath.IsExtendedPrefixed(Path)) then
          Result := Path
        else if (TPath.IsUNCRooted(Path)) then
          Result := '\\?\UNC' + ReplaceStr(Path, '\\', '\')
        else if (TPath.IsDriveRooted(Path)) then
          Result := '\\?\' + Path
        else
          Result := Path;
      end;
  end;
end;

class function TPathFixHelper.AppendToFilename(const Filename, prefix,
  suffix: string): string;
var
  Path: string;
begin
  Path := GetDirectoryName(Filename);
  Result := prefix + GetFileNameWithoutExtension(Filename) + suffix;
  Result := Result + GetExtension(Filename);
  Result := Combine(Path, Result);
end;

class function TPathFixHelper.isWellFormed(const Filename: string;
  isDirectory: boolean = false; useWildcards: boolean = false): boolean;
var
  Path, fname: string;
  aux1, aux2: integer;
begin
  if (Filename <> '') then
  begin
    Result := not((ContainsText(Filename, DirectorySeparatorChar) and
      ContainsText(Filename, AltDirectorySeparatorChar)) and
      (DirectorySeparatorChar <> AltDirectorySeparatorChar));
    if (not Result) then
      Exit;
    Path := SetExtendedPrefix(Filename, TPathPrefixType.pptNoPrefix);
    if TPath.IsUNCRooted(Path) then
    begin
      Path := RightStr(Path, Length(Path) - 2);
      aux1 := PosEx(DirectorySeparatorChar, Path, 1);
      aux2 := PosEx(AltDirectorySeparatorChar, Path, 1);
      Result := (aux1 > 0) or (aux2 > 0);
      if (not isDirectory) then
        Result := (PosEx(DirectorySeparatorChar, Path, aux1 + 1) > 0) or
          (PosEx(AltDirectorySeparatorChar, Path, aux2 + 1) > 0);
    end
    else
      Path := TPath.GetDirectoryName(Filename);
    fname := TPath.GetFileName(Filename);
    Result := Result and (isDirectory or (fname <> ''));
    Result := Result and TPath.HasValidPathChars(Path, useWildcards);
    Result := Result and TPath.HasValidFileNameChars(fname, useWildcards);
    Result := Result and
      (not ContainsText(Path, TPath.DirectorySeparatorChar +
      TPath.DirectorySeparatorChar));
    Result := Result and
      (not ContainsText(Path, TPath.DirectorySeparatorChar +
      TPath.AltDirectorySeparatorChar));
    Result := Result and
      (not ContainsText(Path, TPath.AltDirectorySeparatorChar +
      TPath.DirectorySeparatorChar));
    Result := Result and
      (not ContainsText(Path, TPath.AltDirectorySeparatorChar +
      TPath.AltDirectorySeparatorChar));
  end
  else
    Result := false;
end;

class function TPathFixHelper.ReplaceDirectory(const Filename,
  NewPath: string): string;
begin
  Result := TPath.Combine(NewPath, TPath.GetFileName(Filename));
end;

class function TPathFixHelper.ReplaceFileName(const Filename,
  NewName: string): string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(Filename), NewName);
end;

class function TPathFixHelper.ReplaceExtension(const Filename,
  NewExt: string): string;
begin
  if StartsStr('.', NewExt) then
    Result := TPath.Combine(TPath.GetDirectoryName(Filename),
      TPath.GetFileNameWithoutExtension(Filename) + NewExt)
  else
    Result := TPath.Combine(TPath.GetDirectoryName(Filename),
      TPath.GetFileNameWithoutExtension(Filename) + '.' + NewExt);
end;

// -----------------------------------------------------------------------------
// TFileFixHelper
// -----------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

class function TFileFixHelper.CanCreate(Filename: string): boolean;
var
  h: THandle;
  em: cardinal;
  flags: DWORD;
begin
  em := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Filename := TPath.SetExtendedPrefix(Filename);
    flags := FILE_ATTRIBUTE_NORMAL;
    if (not TFile.Exists(Filename)) then
      flags := flags or FILE_FLAG_DELETE_ON_CLOSE;
    h := CreateFile(PCHAR(Filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
      flags, 0);
    Result := (h <> INVALID_HANDLE_VALUE);
    if (Result) then
      closeHandle(h);
  except
    Result := false;
  end;
  SetErrorMode(em);
end;

function CopyExCallback1(TotalFileSize, TotalBytesTransferred, StreamSize,
  StreamBytesTransferred: LARGE_INTEGER;
  dwStreamNumber, dwCallbackReason: DWORD;
  hSourceFile, hDestinationFile: THandle; lpData: Pointer): DWORD; cdecl;
var
  cancel: boolean;
  callback: TFileCopyProgressEvent;
begin
  Result := PROGRESS_CONTINUE;
  cancel := false;
  callback := TFileCopyProgressEvent(lpData^);
  callback(int64(TotalFileSize), int64(TotalBytesTransferred), cancel);
  if cancel then
    Result := PROGRESS_CANCEL;
end;

class function TFileFixHelper.Copy(const SourceFileName, DestFileName: string;
  ProgressCallback: TFileCopyProgressEvent;
  const Overwrite: boolean = true): boolean;
var
  flags: integer;
  cancelled: boolean;
  copyresult: integer;
begin
  if Assigned(ProgressCallback) then
  begin
    if (Overwrite) then
      flags := 0
    else
      flags := COPY_FILE_FAIL_IF_EXISTS;
    cancelled := false;
    if (not CopyFileEx(PCHAR(SourceFileName), PCHAR(DestFileName),
      @CopyExCallback1, @@ProgressCallback, @cancelled, flags)) then
    begin
      copyresult := GetLastError;
      if (copyresult = ERROR_REQUEST_ABORTED) then
        Result := false
      else
        raise EInOutError.Create(SysErrorMessage(copyresult));
    end
    else
      Result := not cancelled;
  end
  else
  begin
    Copy(SourceFileName, DestFileName, Overwrite);
    Result := true;
  end;
end;

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): BOOL;
    stdcall; external 'kernel32.dll';

class function TFileFixHelper.GetFileSize(const Filename: string): int64;
var
  hFile: THandle;
begin
  hFile := CreateFile(PCHAR(Filename), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (hFile=INVALID_HANDLE_VALUE) then
    Result := -1
  else begin
    if (not GetFileSizeEx(hFile,Result)) then
      Result := -1;
    CloseHandle(hFile);
  end;

end;

{$ENDIF}

end.
