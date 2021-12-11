{$MODE OBJFPC}
{$H+}
program Brackets;

uses
  fgl;

const
  MaxLine = 1000;

type
  IntList = specialize TFPGList<Int64>;
  
var
  stack: ARRAY [1..MaxLine] of Char;
  topIndex: Integer;
  fileIn: Text;
  scores: IntList;
  corruptScore: Int64;
  
  procedure Push(c: Char);
  begin
    stack[topIndex] := c;
    topIndex := topIndex + 1;
  end;

  procedure Pop;
  begin
    topIndex := topIndex - 1;
  end;

  function Top: Char;
  begin
    Result := stack[topIndex-1];
  end;
  
  function Empty: Boolean;
  begin
    Result := topIndex = 1;
  end;
  
  procedure Clear;
  begin
    topIndex := 1;
  end;

  function GetScore: Int64;
  var
    c: Char;
  begin
    Result := 0;
    while not Empty do
    begin
      Result := Result * 5;
      c := Top;
      if c = ')' then
	Result := Result + 1
      else if c = ']' then
	Result := Result + 2
      else if c = '}' then
	Result := Result + 3
      else if c = '>' then
	Result := Result + 4;
      Pop;
    end;
  end;
  
  procedure HandleChar(c: Char; var corrupt: Boolean);
  begin
    if c = '(' then
      Push(')')
    else if c = '<' then
      Push('>')
    else if c = '[' then
      Push(']')
    else if c = '{' then
      Push('}')
    else
    begin
      if Empty then
	corrupt := True
      else
      begin      
	corrupt := c <> Top;
	Pop;
      end;
    end;
  end;
  
  procedure ProcessLine;
  var
    c: Char;
    corrupt: Boolean;
    score: Int64;
  begin
    Clear;
    corrupt := False;
    Read(filein, c);
    while not eof(filein) and not corrupt and (c <> #10) do
    begin
      HandleChar(c, corrupt);
      if not corrupt then
	Read(filein, c);
    end;
    score := GetScore;
    if corrupt then
    begin
      if c = ')' then
	corruptScore := corruptScore + 3
      else if c = ']' then
	corruptScore := corruptScore + 57
      else if c = '}' then
	corruptScore := corruptScore + 1197
      else if c = '>' then
	corruptScore := corruptScore + 25137
    end
    else
      scores.Add(score);
    while not eof(filein) and (c <> #10) do
    begin
      Read(filein, c);
    end;
  end;

  function LongOrder(const a,b: Int64): Longint;
  begin
    if a = b then Result := 0
    else if a < b then Result := -1
    else Result := 1;
  end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('input file missing');
    Halt(1)
  end;
  Assign(filein, ParamStr(1));
  Reset(filein);
  scores := IntList.Create;
  corruptScore := 0;
  while not eof(filein) do
    ProcessLine;
  WriteLn(corruptScore);
  scores.sort(@LongOrder);
  WriteLn(scores[(scores.Count-1) div 2]);
  Close(filein);
end.
