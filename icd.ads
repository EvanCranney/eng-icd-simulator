with HRM;
with ImpulseGenerator;
with Measures;

package ICD is

    type History is array (1 .. 10) of Measures.BPM;

    type ICDType is record
        IsOn : Boolean;
        Hist : History;
        JoulesToDeliver: Measures.BPM;
    end record;

    -- create an initialize a new ICD
    procedure Init(Def : out ICDType);

    -- turn on the defribulator
    procedure On(Def: in out ICDType);

    -- turn off the defribulator
    procedure Off(Def : in out ICDType);

    -- query the status of the defribulator
    function IsOn(Def : in ICDType) return Boolean;

    -- tick the clock
    procedure Tick(Def : in out ICDType; Mon : in HRM.HRMType;
        Gen : in out ImpulseGenerator.GeneratorType);
 
end ICD;
