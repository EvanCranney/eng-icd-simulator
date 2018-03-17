with ClosedLoop;
with Ada.Text_IO;

procedure main is
begin

    ClosedLoop.Init;

    clock:
        for I in Integer range 1 .. 10000 loop
            Ada.Text_IO.Put_Line("...");
            ClosedLoop.Tick;
        end loop clock;

end main;
