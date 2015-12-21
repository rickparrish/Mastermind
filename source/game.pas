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
  // http://patorjk.com/software/taag/#p=display&f=Soft&t=Mastermind
  DoorWriteLn('  ,--.   ,--.                ,--.                          ,--.           ,--. ');
  DoorWriteLn('  |   `.''   | ,--,--. ,---.,-''  ''-. ,---. ,--.--.,--,--,--.`--'',--,--,  ,-|  | ');
  DoorWriteLn('  |  |''.''|  |'' ,-.  |(  .-''''-.  .-''| .-. :|  .--''|        |,--.|      \'' .-. | ');
  DoorWriteLn('  |  |   |  |\ ''-''  |.-''  `) |  |  \   --.|  |   |  |  |  ||  ||  ||  |\ `-'' | ');
  DoorWriteLn('  `--''   `--'' `--`--''`----''  `--''   `----''`--''   `--`--`--''`--''`--''''--'' `---''  ');
  DoorWriteLn('  +-----+----++----+----++----+----++----+----++----+----++----+----++------+ ');
  DoorWriteLn('  |   x |  x ||  x |  x ||  x |  x ||  x |  x ||  x |  x ||  x |  x ||      |x');
  DoorWriteLn('  |     |    ||    |    ||    |    ||    |    ||    |    ||    |    ||      |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |     |    ||    |    ||    |    ||    |    ||    |    ||    |    ||      |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |     |    ||    |    ||    |    ||    |    ||    |    ||    |    ||      |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |     |    ||    |    ||    |    ||    |    ||    |    ||    |    ||      |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |  01 | 02 || 03 | 04 || 05 | 06 || 07 | 08 || 09 | 10 || 11 | 12 ||  aa  |x');
  DoorWriteLn('  |     |    ||    |    ||    |    ||    |    ||    |    ||    |    ||      |x');
  DoorWriteLn('  |   x |  x ||  x |  x ||  x |  x ||  x |  x ||  x |  x ||  x |  x ||      |x');
  DoorWriteLn('  +-----+----++----+----++----+----++----+----++----+----++----+----++------+x');
  DoorWriteLn('   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  DoorSession.SethWrite := true;
end;

procedure DrawPosition(draw: Boolean);
var
  NewX, NewY: Integer;
begin
  NewX := 5 + (Position.x - 1) * 5;
  if (Position.X > 2) then NewX += Round(Position.x / 2 + 0.1) - 1; // + 0.1 is because Pascal rounds to even
  NewY := 9 + (Position.y - 1) * 3;

  DoorGotoXY(NewX, NewY);
  if (draw) then
  begin
    DoorWrite('[');
    DoorTextColour(COLOURS[Colour]);
    DoorWrite(#220#220);
    DoorTextColour(Crt.LightGray);
    DoorWrite(']');
  end else
  begin
    DoorWrite(' ');
    DoorCursorRight(2);
    DoorWrite(' ');
  end;

  DoorGotoXY(NewX, NewY + 1);
  if (draw) then
  begin
    DoorWrite('[');
    DoorTextColour(COLOURS[Colour]);
    DoorWrite(#223#223);
    DoorTextColour(Crt.LightGray);
    DoorWrite(']');
  end else
  begin
    DoorWrite(' ');
    DoorCursorRight(2);
    DoorWrite(' ');
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
        '8': MoveUp;
        '4': SwitchColourLeft;
        '6': SwitchColourRight;
        '2': MoveDown;
        'Q': begin
               // Confirm exit
               DoorGotoXY(1, 24);
               DoorWrite('`r0`2  Are you sure you want to quit back to the BBS? [`%Y`2] : ');

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

