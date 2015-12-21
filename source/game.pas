unit Game;

{$mode objfpc}{$h+}

interface

uses
  Door,
  StrUtils;

procedure Start;

implementation

var
  OldExitProc: Pointer;

procedure NewExitProc; forward;

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

  repeat
    Ch := UpCase(DoorReadKey);
    if (DoorLastKey.Extended) then
    begin
      // Handle arrow keys
      case Ch of
        'H': ; // TODOX Up arrow
        'K': ; // TODOX Left arrow
        'M': ; // TODOX Right arrow
        'P': ; // TODOX Down arrow
      end;
    end else
    begin
      case Ch of
        '8': ; // TODOX Up
        '4': ; // TODOX Left
        '6': ; // TODOX Right
        '2': ; // TODOX Down
        'Q': begin
               // Confirm exit
               DoorGotoXY(1, 23);
               DoorWrite('`r0`2  Are you sure you want to quit back to the BBS? [`%Y`2] : ');

               repeat
                 // Repeat until we have a valid selection
                 Ch := UpCase(DoorReadKey);
               until (Ch in ['Y', 'N', #13]);

               // Translate selection into either #0 to abort quit, or Q to confirm quit
               if (Ch = 'N') then
               begin
                 Ch := #0;
                 DoorGotoXY(1, 23);
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

end.

