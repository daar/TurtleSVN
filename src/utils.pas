unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ConvertBytes(Bytes: int64): string;

implementation

uses
  Math;

function ConvertBytes(Bytes: int64): string;
const
  Description: array [0 .. 8] of string =
    ('Bytes', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB');
var
  i: integer;
begin
  i := 0;

  while Bytes > IntPower(1024, i + 1) do
    Inc(i);

  Result := FormatFloat('###0.#', Bytes / IntPower(1024, i)) + ' ' + Description[i];
end;

end.
