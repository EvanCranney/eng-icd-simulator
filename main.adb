with ClosedLoop;

procedure main is
begin

    ClosedLoop.Init;

    clock:
        for I in Integer range 1 .. 100000 loop
            ClosedLoop.Tick;
        end loop clock;

end main;
