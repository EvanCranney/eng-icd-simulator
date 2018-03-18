with HRM;
with ImpulseGenerator;
with Measures; use Measures;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is

    -- initialize defribulator
    procedure Init(Def : out ICDType) is
    begin
        --
        Def.IsOn := False;
        Def.Time := Measures.TickCount'First;

        -- impulse to apply, number of times, and frequency
        Def.Impulse := Measures.BPM(0); -- saved impulse
        Def.ImpulseCount := 0;
        -- Def.ImpulseStart takes no initial value
        Def.ImpulseFreq := Measures.TickCount(0);
        Def.SendImpulse := False;

        -- tachy settings
        Def.TachyThresh := Measures.BPM(100);
        Def.TachyImpulse := Measures.Joules(2);
        Def.TachyImpulseCount := 10;

        -- fibrillation settings
        Def.FibImpulse := Measures.Joules(30);
    end Init;

    -- switch to ON mode
    procedure On(Def : in out ICDType) is
    begin
        Def.IsOn := True;
    end On;

    -- switch to OFF mode
    procedure Off(Def : in out ICDType) is
    begin
        Def.IsOn := False;
    end Off;

    -- checks whether defribulator is in on mode
    function IsOn(Def : in ICDType) return Boolean is
    begin
        return Def.IsOn;
    end IsOn;

    -- fetch rate history : array of (BPM, Time) tuples
    function GetHistory(Def : in ICDType) return HistoryType is
    begin 
        return Def.History;
    end GetHistory;

    -- fetch setting : BPM threshold for tachycardia
    function GetTachyThresh(Def : in ICDType) return Measures.BPM is
    begin
        return Def.TachyThresh;
    end GetTachyThresh;

    -- change setting : BPM threshold for tachycardia
    procedure SetTachyThresh(Def : in out ICDType;
        Thresh : in Measures.BPM) is
    begin
        Def.TachyThresh := Thresh;
    end;

    -- fetch setting : impulse (joules) to respond to tachycardia
    function GetTachyImpulse(Def : in ICDType) return Measures.Joules is
    begin
        return Def.TachyImpulse;
    end GetTachyImpulse;

    -- fetch setting : impules (joules) to respond to fibrillation
    function GetFibImpulse(Def : in ICDType) return Measures.Joules is
    begin
        return Def.FibImpulse;
    end GetFibImpulse;

    -- change setting : impulse (joules) to respond to fibrillation
    procedure SetFibImpulse(Def : in out ICDType;
        Impulse : in Measures.Joules) is
    begin
        Def.FibImpulse := Impulse;
    end;

    -- update medical history
    procedure UpdateHistory(Def : in out ICDType; Rate : in Measures.BPM) is
        J : Integer;
    begin
        -- move each history record forwards in the array
        move_forwards:
            for I in HistoryIndex'First+1 .. HistoryIndex'Last loop
                J := Def.History'Length - I;
                Def.History(J).Rate := Def.History(J-1).Rate;
                Def.History(J).Time := Def.History(J-1).Time;
            end loop move_forwards;

        -- update t=0 with most recent medical history
        Def.History(HistoryIndex'First).Rate := Rate;
        Def.History(HistoryIndex'First).Time := Def.Time;
    end;

    -- check if is tacycardic
    function IsTachycardic(Def : in ICDType) return Boolean is
    begin
        -- check if BPM exceeds Tachy 
        return Def.History(HistoryIndex'First).Rate > Def.TachyThresh;
    end IsTachycardic;

    -- check if is fibrillating
    function IsFibrillating(Def: in ICDType) return Boolean is
        Sum : Integer;
        AvgRateChange : Integer;
    begin
        -- sum up average differences
        Sum := 0;
        for I in 1 .. 6 loop
            Sum := Sum + abs(Def.History(I).Rate-Def.History(I+1).Rate);
        end loop;

        -- compute average heart rate change
        AvgRateChange := Sum / 6;
        
        -- check if average rate change exceeds limit
        return AvgRateChange >= Def.TachyThresh;
    end IsFibrillating;

    -- convert BPM to TPB (ticks-per-beat)
    function BPMToTPB(Rate : in Measures.BPM) return Measures.TickCount is
    begin
        -- cannot divide by zero, return max possible number of ticks
        -- note: <= is necessary because BPM can be registered as -1 if
        -- HRM is off
        if Rate <= Measures.BPM(0) then
            return Measures.TickCount'Last;
        else
            -- equals ticks-per-minute / bpm
            return Measures.TickCount(600/Integer(Rate));
        end if;
    end BPMToTPB;

    -- compute the impulse
    procedure ComputeImpulse(Def : in out ICDType) is
    begin
        -- default, send no impulse
        Def.SendImpulse := False;

        -- check if still need to send impulses
        if Def.ImpulseCount > 0 then
            -- check if we need to administer more impulses
            if ((Def.Time - Def.ImpulseStart) rem Def.ImpulseFreq) = 0 then
                Def.SendImpulse := True;
            end if;
            Def.ImpulseCount := Def.ImpulseCount-1;

        -- check if tachycardia detected
        elsif IsTachycardic(Def) then
            Def.Impulse := Def.TachyImpulse;
            Def.ImpulseCount := Def.TachyImpulseCount;
            Def.ImpulseStart := Def.Time;
            Def.ImpulseFreq := BPMToTPB(Def.History(1).Rate) + 
                BPMToTPB( Measures.BPM(15));
        
        -- check if ventricular fibrillation detected
        elsif IsFibrillating(Def) then
            Def.Impulse := Def.FibImpulse;
            Def.SendImpulse := True;

        -- otherwise set impulse to 0 joules
        else
            Def.Impulse := Measures.Joules(0);
        end if;
    end;

    -- get the impulse to be sent on to impulse generator
    function GetImpulse(Def : in ICDType) return Measures.Joules is
    begin
        if Def.SendImpulse then
            return Def.Impulse;
        else
            return Measures.Joules(0);
        end if;
    end GetImpulse;

    -- Tick defribulator
    procedure Tick(Def : in out ICDType; Rate : in Measures.BPM) is
    begin
        -- update the clock time
        Def.Time := Def.Time + 1;
 
        if Def.IsOn then
            -- update medical history
            UpdateHistory(Def, Rate);

            -- compute the impulse required
            ComputeImpulse(Def);
        end if;

    end Tick;

end ICD;