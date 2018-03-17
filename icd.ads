with HRM;
with ImpulseGenerator;
with Measures;

package ICD is

    subtype HistoryIndex is Integer range 0 .. 9;
    type RateHistoryType is array (HistoryIndex) of Measures.BPM;
    type TimeHistoryType is array (HistoryIndex) of Measures.TickCount;

    type HistoryType is record
        Rates : RateHistoryType;
        Times : TimeHistoryType;
    end record;

    type ICDType is record
        IsOn : Boolean;
        History : HistoryType;
        TachyBPMThresh : Measures.BPM;
        TachyImpulse : Measures.Joules;
        TachyNumImpulses : Integer;
        VentFibImpulse : Measures.Joules;
    end record;

    -- create an initialize a new ICD
    procedure Init(Def : out ICDType);

    -- turn on the defribulator
    procedure On(Def: in out ICDType);

    -- turn off the defribulator
    procedure Off(Def : in out ICDType);

    -- query the status of the defribulator
    function IsOn(Def : in ICDType) return Boolean;

    -- Tick the clock: read latest BPM & compute impulse
    procedure Tick(
        Def : in out ICDType;
        Mon : in HRM.HRMType;
        Gen : in out ImpulseGenerator.GeneratorType
    );
 
end ICD;
