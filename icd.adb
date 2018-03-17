with HRM;
with ImpulseGenerator;
with Measures;

package body ICD is

    procedure Init(Def : out ICDType) is
    begin
        Def.IsOn := False;
        Def.JoulesToDeliver := 0;
        -- Def.Hist := History;
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

    function HasVentricularFibrillation(Def : in ICDType) return Boolean is
    begin
        return Def.Hist(1) > 100;
    end HasVentricularFibrillation;

    function HasTachycardia(Def: in ICDType) return Boolean is
        Numer : Integer;
        Denom : Integer;
        AverageHeartRateChange : Integer;
    begin
        Numer := 0;
        for I in 1 .. 6 loop
            Numer := Numer + abs (Def.Hist(I) - Def.Hist(I+1));
        end loop;
        Denom := 6;
        AverageHeartRateChange := Numer / Denom;
        
        if AverageHeartRateChange >= 10 then
            return True;
        else
            return False;
        end if;
    end HasTachycardia;

    procedure Tick(
        Def : in out ICDType;
        Mon : in HRM.HRMType;
        Gen : in out ImpulseGenerator.GeneratorType) is
        Impulse : Measures.Joules;
    begin
        -- check if the Def is on
        if Def.IsOn then

            -- update medical history
            Update_Medical_History:
                for I in 2 .. 10 loop
                    Def.Hist (I) := Def.Hist(I-1);
                end loop Update_Medical_History;
            HRM.GetRate(Mon, Def.Hist(1));

            -- compute the target impulse
            Impulse := 0;
            
            -- rule for tachycardia
            if HasTachyCardia(Def) then
                Impulse := 2;
                -- and go into tachycardia state
            elsif HasVentricularFibrillation(Def) then
                Impulse := Def.JoulesToDeliver;
            end if;

            -- send the impulse to the Impulse Generator
            ImpulseGenerator.SetImpulse(Gen, Impulse);
        end if;
    end Tick;

end ICD;
