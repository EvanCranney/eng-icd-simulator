with HRM;
with ImpulseGenerator;
with Measures;

package ICD is

    type HistoryRecord is record
        Rate : Measures.BPM;
        Time : Measures.TickCount;
    end record;

    subtype HistoryIndex is Integer range 0 .. 9;
    type HistoryType is array (HistoryIndex) of HistoryRecord;

    type ICDType is record
        IsOn : Boolean;
        Time : Measures.TickCount;
        History : HistoryType;    

        Impulse : Measures.Joules;
        ImpulseCount : Integer;
        ImpulseFreq : Measures.TickCount;
        ImpulseStart : Measures.TickCount;

        TachyThresh : Measures.BPM;
        TachyImpulse : Measures.Joules;
        TachyImpulseCount : Integer;

        FibImpulse : Measures.Joules;
    end record;

    -- create an initialize a new ICD
    procedure Init(Def : out ICDType);

    -- turn on the defribulator
    procedure On(Def: in out ICDType);

    -- turn off the defribulator
    procedure Off(Def : in out ICDType);

    -- query the status of the defribulator
    function IsOn(Def : in ICDType) return Boolean;

    function GetTachyThresh(Def : in ICDType) return Measures.BPM;

    function GetTachyImpulse(Def : in ICDType) return Measures.Joules;

    function GetFibImpulse(Def : in ICDType) return Measures.Joules;

    function GetHistory(Def : in ICDType) return HistoryType;
    
    procedure GetImpulse(Def : in ICDType; Impulse : in out Measures.Joules);

    -- Tick the clock: read latest BPM & compute impulse
    procedure Tick(
        Def : in out ICDType;
        Rate : in Measures.BPM);
 
end ICD;
