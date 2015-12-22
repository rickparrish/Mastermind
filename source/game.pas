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
  Colour: Integer;
  OldExitProc: Pointer;
  Position: TPoint;

procedure DrawBoard; forward;
procedure DrawPosition(draw: Boolean); forward;
procedure MoveDown; forward;
procedure MoveUp; forward;
procedure NewExitProc; forward;
procedure SwitchColourLeft; forward;
procedure SwitchColourRight; forward;

procedure DrawBoard;
begin
  DoorSession.SethWrite := false;
  DoorClrScr;
  {$I board.inc}
  DoorSession.SethWrite := true;
end;

procedure DrawPosition(draw: Boolean);
const
  LEFTBORDER: Array[0..1] of Char = (#218, #192);
  RIGHTBORDER: Array[0..1] of Char = (#191, #217);
  MIDDLE: Array[0..1] of Char = (#220, #223);
var
  I, NewX, NewY: Integer;
begin
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
end;

procedure MoveDown;
begin
  DrawPosition(false);
  Position.Y += 1;
  if (Position.Y > 4) then Position.Y := 1;
  DrawPosition(true);
end;

procedure MoveUp;
begin
  DrawPosition(false);
  Position.Y -= 1;
  if (Position.Y < 1) then Position.Y := 4;
  DrawPosition(true);
end;

{
  Custom exit proc to ensure data is saved
}
procedure NewExitProc;
begin
  ExitProc := OldExitProc;
  //TODOX ensure data is saved
end;

{
  This is where the game begins
}
procedure Start;
var
  Ch: Char;
begin
  // Setup exit proc, which ensures the player is saved properly if a HALT is performed
  OldExitProc := ExitProc;
  ExitProc := @NewExitProc;

  Colour := 1;
  Position := Point(1, 1);

  DrawBoard;
  DrawPosition(true);
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
            // TODOX
            DrawPosition(false);
            Position.X += 1;
            if (Position.X > 12) then Position.X := 1;
            Position.Y := 1;
            DrawPosition(true);
          end;
        '8': MoveUp;
        '4': SwitchColourLeft;
        '6': SwitchColourRight;
        '2': MoveDown;
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
      end;
    end;
  until (Ch = 'Q');
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

