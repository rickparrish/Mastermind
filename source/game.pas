unit Game;

{$mode objfpc}{$h+}

interface

uses
  Door,
  Crt, Classes, StrUtils;

procedure Start;

implementation

const
  COLOURS: Array[1..6] of Integer = (Crt.LightBlue, Crt.LightRed, Crt.LightGreen, Crt.LightMagenta, Crt.Yellow, Crt.White);

var
  AnswerRow: Array[1..4] of Integer = (0, 0, 0, 0);
  Colour: Integer;
  CurrentRow: Array[1..4] of Integer = (0, 0, 0, 0);
  OldExitProc: Pointer;
  Position: TPoint;

procedure DrawAnswer; forward;
procedure DrawBoard; forward;
procedure DrawHints; forward;
procedure DrawPosition(draw: Boolean); forward;
procedure HandleLose; forward;
procedure HandleWin; forward;
procedure MoveDown; forward;
procedure MoveUp; forward;
procedure NewExitProc; forward;
procedure ResetGame; forward;
function  RowIsComplete: Boolean; forward;
function  RowIsCorrect: Boolean; forward;
procedure SwitchColourLeft; forward;
procedure SwitchColourRight; forward;

procedure DrawAnswer;
const
  MIDDLE: Array[0..1] of Char = (#220, #223);
var
  I, Y, NewX, NewY: Integer;
begin
  NewX := 70;

  DoorWrite('`r6');
  for Y := Low(AnswerRow) to High(AnswerRow) do
  begin
    // (Y - 1) * 3: Offset based on which peg we're drawing
    NewY := 8 + (Y - 1) * 3;

    DoorTextColour(COLOURS[AnswerRow[Y]]);

    for I := 0 to 1 do
    begin
      DoorGotoXY(NewX, NewY + I);
      DoorWrite(MIDDLE[I] + MIDDLE[I]);
    end;
  end;
  DoorWrite('`r0');
end;

procedure DrawBoard;
begin
  DoorSession.SethWrite := false;
  {$I board.inc}
  DoorSession.SethWrite := true;
end;

procedure DrawHints;
var
  RCRP, RCWP: Integer;
  I, J: Integer;
  TempAnswerRow: Array[Low(AnswerRow)..High(AnswerRow)] of Integer;
  TempCurrentRow: Array[Low(CurrentRow)..High(CurrentRow)] of Integer;
begin
  // Get a copy of the rows since we'll be modifying them
  for I := Low(AnswerRow) to High(AnswerRow) do
  begin
    TempAnswerRow[I] := AnswerRow[I];
    TempCurrentRow[I] := CurrentRow[I];
  end;

  // Look for right colour in the right place matches
  RCRP := 0;
  for I := Low(TempCurrentRow) to High(TempCurrentRow) do
  begin
    if (TempCurrentRow[I] = TempAnswerRow[I]) then
    begin
      RCRP += 1;
      TempCurrentRow[I] := 0;
      TempAnswerRow[I] := 0;
    end;
  end;

  // Look for right colour in the wrong place matches
  RCWP := 0;
  for I := Low(TempCurrentRow) to High(TempCurrentRow) do
  begin
    for J := Low(TempAnswerRow) to High(TempAnswerRow) do
    begin
      if (TempCurrentRow[I] > 0) and (TempAnswerRow[J] > 0) and (TempCurrentRow[I] = TempAnswerRow[J]) then
      begin
        RCWP += 1;
        TempCurrentRow[I] := 0;
        TempAnswerRow[J] := 0;
        Break;
      end;
    end;
  end;

  // Draw green RCRP pegs
  DoorTextColour(Crt.LightGreen);
  for I := 1 to RCRP do
  begin
    // (Position.X - 1) * 5: Offset based on which guess number we're on
    // (I - 1) mod 2: Offset for peg 2 and 4
    // Round(I / 2 + 0.1): Offset for peg 1 and 3 (0.1 for round up instead of round to even)
    DoorGotoXY(8 + (Position.X - 1) * 5 + (I - 1) mod 2, 19 + Round(I / 2 + 0.1));
    DoorWrite(#254);
  end;

  // Draw yellow RCWP pegs
  DoorTextColour(Crt.Yellow);
  for I := RCRP + 1 to RCRP + RCWP do
  begin
    DoorGotoXY(8 + (Position.X - 1) * 5 + (I - 1) mod 2, 19 + Round(I / 2 + 0.1));
    DoorWrite(#254);
  end;

  DoorTextColour(Crt.LightGray);
end;

procedure DrawPosition(draw: Boolean);
const
  LEFTBORDER: Array[0..1] of Char = (#218, #192);
  RIGHTBORDER: Array[0..1] of Char = (#191, #217);
  MIDDLE: Array[0..1] of Char = (#220, #223);
var
  I, NewX, NewY: Integer;
begin
  // (Position.x - 1) * 5: Offset based on which peg we're drawing
  // (Position.y - 1) * 3: Offset based on which peg we're drawing
  NewX := 7 + (Position.x - 1) * 5;
  NewY := 8 + (Position.y - 1) * 3;

  for I := 0 to 1 do
  begin
    DoorGotoXY(NewX, NewY + I);
    if (draw) then
    begin
      DoorWrite('`r6`%' + LEFTBORDER[I]);
      DoorTextColour(COLOURS[Colour]);
      DoorWrite(MIDDLE[I] + MIDDLE[I]);
      DoorWrite('`%' + RIGHTBORDER[I]);
    end else
    begin
      DoorWrite('`r6 ');
      DoorCursorRight(2);
      DoorWrite(' `r0');
    end;
  end;

  if (draw) then CurrentRow[Position.y] := Colour;
end;

procedure HandleLose;
begin
  {$I lose.inc}
  DoorReadKey;
end;

procedure HandleWin;
begin
  {$I win.inc}
  DoorReadKey;
end;

procedure MoveDown;
begin
  DrawPosition(false);
  Position.Y += 1;
  if (Position.Y > 4) then Position.Y := 1;
  Colour := CurrentRow[Position.Y];
  DrawPosition(true);
end;

procedure MoveUp;
begin
  DrawPosition(false);
  Position.Y -= 1;
  if (Position.Y < 1) then Position.Y := 4;
  Colour := CurrentRow[Position.Y];
  DrawPosition(true);
end;

{
  Custom exit proc to ensure data is saved
}
procedure NewExitProc;
begin
  ExitProc := OldExitProc;
  //TODOZ ensure data is saved
end;

{
  This is where the game begins
}
procedure Start;
var
  Ch: Char;
  I: Integer;
begin
  // Setup exit proc, which ensures the player is saved properly if a HALT is performed
  OldExitProc := ExitProc;
  ExitProc := @NewExitProc;

  ResetGame;
  repeat
    Ch := UpCase(DoorReadKey);
    if (DoorLastKey.Extended) then
    begin
      // Handle arrow keys
      case Ch of
        'H': MoveUp;
        'K': SwitchColourLeft;
        'M': SwitchColourRight;
        'P': MoveDown;
      end;
    end else
    begin
      case Ch of
        #13:
          begin
            if (RowIsComplete) then
            begin
              if (RowIsCorrect) then
              begin
                HandleWin;
                ResetGame;
              end else
              begin
                DrawHints;
                DrawPosition(false);
                Position.X += 1;
                if (Position.X > 12) then
                begin
                  HandleLose;
                  ResetGame;
                end else
                begin
                  // TODOY Multiple DrawPosition is sucky
                  for I := High(CurrentRow) downto Low(CurrentRow) do
                  begin
                    Position.Y := I;
                    Colour := CurrentRow[I];
                    DrawPosition(true);
                    DrawPosition(false);
                  end;
                  DrawPosition(true);
                end;
              end;
            end;
          end;
        '8': MoveUp;
        '4': SwitchColourLeft;
        '6': SwitchColourRight;
        '2': MoveDown;
        'A': ; // TODOX About
        'B':
          begin
            Colour := 1;
            DrawPosition(true);
            MoveDown;
          end;
        'G':
          begin
            Colour := 3;
            DrawPosition(true);
            MoveDown;
          end;
        'H': ; // TODOX Help
        'M':
          begin
            Colour := 4;
            DrawPosition(true);
            MoveDown;
          end;
        'R':
          begin
            Colour := 2;
            DrawPosition(true);
            MoveDown;
          end;
        'Q': begin
               // Confirm exit
               DoorGotoXY(1, 24);
               DoorWrite('`r0`2      Are you sure you want to quit back to the BBS? [`%Y`2] : ');

               repeat
                 // Repeat until we have a valid selection
                 Ch := UpCase(DoorReadKey);
               until (Ch in ['Y', 'N', #13]);

               // Translate selection into either #0 to abort quit, or Q to confirm quit
               if (Ch = 'N') then
               begin
                 Ch := #0;
                 DoorGotoXY(1, 24);
                 DoorWrite(PadRight('', 79));
               end else
               begin
                 Ch := 'Q';
               end;
             end;
        'W':
          begin
            Colour := 6;
            DrawPosition(true);
            MoveDown;
          end;
        'Y':
          begin
            Colour := 5;
            DrawPosition(true);
            MoveDown;
          end;
      end;
    end;
  until (Ch = 'Q');
end;

procedure ResetGame;
var
  I: Integer;
begin
  Colour := 0;
  Position := Point(1, 1);

  for I := Low(CurrentRow) to High(CurrentRow) do
  begin
    AnswerRow[I] := Random(6) + 1;
    CurrentRow[I] := 0;
  end;
  //TODOX (Crt.LightBlue, Crt.LightRed, Crt.LightGreen, Crt.LightMagenta, Crt.Yellow, Crt.White);
  AnswerRow[1] := 3;
  AnswerRow[2] := 5;
  AnswerRow[3] := 4;
  AnswerRow[4] := 5;

  DrawBoard;
  DrawAnswer; // TODOZ
  DrawPosition(true);
end;

function RowIsComplete: Boolean;
var
  I: Integer;
begin
  // Assume success
  Result := true;

  // Look for missing cells
  for I := Low(CurrentRow) to High(CurrentRow) do
  begin
    if (CurrentRow[I] = 0) then
    begin
      Result := false;
      Break;
    end;
  end;
end;

function RowIsCorrect: Boolean;
var
  I: Integer;
begin
  // Assume success
  Result := true;

  // Look for incorrect cells
  for I := Low(CurrentRow) to High(CurrentRow) do
  begin
    if (CurrentRow[I] <> AnswerRow[I]) then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

procedure SwitchColourLeft;
begin
  Colour -= 1;
  if (Colour < 1) then Colour := 6;
  DrawPosition(true);
end;

procedure SwitchColourRight;
begin
  Colour += 1;
  if (Colour > 6) then Colour := 1;
  DrawPosition(true);
end;

end.

