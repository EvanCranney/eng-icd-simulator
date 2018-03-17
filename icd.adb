with HRM;
with ImpulseGenerator;
with Measures;

package body ICD is

    procedure Init(Def : out ICDType) is
    begin
        Def.IsOn := False;
        -- RateHistory automatically instantiated
        Def.TachyThresh := 100;
        Def.TachyImpulse := 2;
        Def.TachyNumImpulses := 10;
        Def.FibImpulse := 30;
    end Init;

    procedure On(Def : in out ICDType) is
    begin
        Def.IsOn := True;
    end On;

    procedure Off(Def : in out ICDType) is
    begin
        Def.IsOn := False;
    end Off;

    function IsOn(Def : in ICDType) return Boolean is
    begin
        return Def.IsOn;
    end IsOn;

    function GetTachyThresh(Def : in ICDType) return Measures.BPM is
    begin
        return Def.TachyThresh;
    end GetTachyThresh;

    function GetTachyImpulse(Def : in ICDType) return Measures.Joules is
    begin
        return Def.TachyImpulse;
    end GetTachyImpulse;

    function GetFibImpulse(Def : in ICDType) return Measures.Joules is
    begin
        return Def.FibImpulse;
    end GetFibImpulse;

    function GetHistory(Def : in ICDType) return HistoryType is
    begin 
        return Def.History;
    end GetHistory;

    -- update medical history
    procedure UpdateHistory(
        Def : in out ICDType;
        Mon : in HRM.HRMType
    ) is
    begin
        -- move each record t to t-1
        move_records_backward:
            for I in HistoryIndex'First+1 .. HistoryIndex'Last loop
                Def.History(I).Rate := Def.History(I-1).Rate;
                --Def.RateHistory.Times(I) := Def.RateHistory.Times(I-1);
            end loop move_records_backward;

        -- update the record for t = 0
        HRM.GetRate(Mon, Def.History(HistoryIndex'First).Rate);
    end;

    -- check if is tacycardic
    function IsTachycardic(Def : in ICDType) return Boolean is
    begin
        -- check if BPM exceeds Tachy 
        return Def.History(HistoryIndex'First).Rate > Def.TachyThresh;
    end IsTachycardic;

    -- check if is fibrillating
    function IsFibrillating(Def: in ICDType) return Boolean is
        Numer : Integer;
        Denom : Integer;
        AvgRateChange : Integer;
    begin
        -- sum up average differences
        Numer := 0;
        for I in 1 .. 6 loop
            Numer := Numer + abs(Def.History(I).Rate-Def.History(I+1).Rate);
        end loop;

        -- divide by num differences
        Denom := 6;

        -- compute average heart rate change
        AvgRateChange := Numer / Denom;
        
        -- check if average rate change exceeds limit
        return AvgRateChange >= Def.TachyThresh;
    end IsFibrillating;

    procedure Tick(
        Def : in out ICDType;
        Mon : in HRM.HRMType;
        Gen : in out ImpulseGenerator.GeneratorType
    ) is
        Impulse : Measures.Joules;
    begin
        -- check if the fribrillator is in mode on
        if Def.IsOn then

            -- update medical history
            UpdateHistory(Def, Mon);


            -- compute the target impulse
            Impulse := 0;
            
            -- rule for tachycardia
            if IsTachycardic(Def) then
                Impulse := Def.TachyImpulse;
                -- and go into tachycardia state
            elsif IsFibrillating(Def) then
                Impulse := Def.FibImpulse;
            end if;

            -- send the impulse to the Impulse Generator
            ImpulseGenerator.SetImpulse(Gen, Impulse);
        end if;
    end Tick;

end ICD;
