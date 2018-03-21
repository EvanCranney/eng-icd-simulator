with HRM;
with ImpulseGenerator;
with Measures;

-- This package simulates the ICD software component. It is responsible for
--  calculating the impulse (joules) to be passed to the impulse generator
--  (and, ulimately, the heart). Since it requires a series of recent
--  heart rate measurements (received from a monitor) to identify
--  anomalous heart rate behaviour, it also stores a "buffer" of recent
--  heart rate measurements (and the time that measurements were taken).
package ICD is

   -- number of historical heart rates to store in history
   HISTORY_LENGTH : constant Integer := 10;
   HISTORY_START_INDEX : constant Integer := 1;

   -- default start mode is off (IsOn = False)
   DEFAULT_START_MODE : constant Boolean := False;
   DEFAULT_START_TIME : constant Measures.TickCount := 
     Measures.TickCount'First;

   -- a single heart rate measurement
   type HistoryRecord is record
      Rate : Measures.BPM;
      Time : Measures.TickCount;
   end record;

   -- heart rate history array
   type HistoryType is array (Integer range HISTORY_START_INDEX
     .. HISTORY_LENGTH) of HistoryRecord;

   -- default anomaly response settings
   DEFAULT_TACHY_THRESH : constant Measures.BPM := Measures.BPM(100);
   DEFAULT_TACHY_IMPULSE : constant Measures.Joules := Measures.Joules(2);
   DEFAULT_TACHY_IMPULSE_COUNT : constant Integer := 10;
   DEFAULT_FIB_THRESH : constant Measures.BPM := Measures.BPM(10);
   DEFAULT_FIB_IMPULSE : constant Measures.Joules := Measures.Joules(30);

   -- number of heart rate records to estimate average change in rate
   NUM_DIFFS_FOR_ESTIMATE : constant Integer := 6;
   
   -- increment over current heart rate to estimate impulse frequency
   --   in response to tachycardia
   TACHY_BPM_INCREMENT : constant Float := 15.0;

   -- number of seconds per clock tick
   SECONDS_PER_MINUTE : constant Float := 60.0;
   SECONDS_PER_TICK : constant Float := 0.1;
   
   -- settings for the ICD software
   type ICDSettings is
      record
         -- for tachycardia
         TachyThresh : Measures.BPM;       -- used to identify tachycardia
         TachyImpulse : Measures.Joules;   -- impulse to deliver
         TachyImpulseCount : Integer;      -- number of impulses to deliver
   
         -- for ventricular fibrillation
         FibImpulse : Measures.Joules;     -- impulse to deliver
      end record; 

   -- default impulse strength and number of impulses to apply at each tick
   DEFAULT_IMPULSE : constant Measures.Joules := Measures.Joules(0);
   DEFAULT_IMPULSE_COUNT : constant Integer := 0;
   DEFAULT_IMPULSE_FREQUENCY : constant Measures.TickCount := 0;
   DEFAULT_SEND_IMPULSE : constant Boolean := False;

   -- ICD software type
   type ICDType is 
      record
         IsOn : Boolean;                    -- indicates ICD mode on/off 
         Time : Measures.TickCount;         -- current time in ticks
         History : HistoryType;             -- heart rate history
         Settings : ICDSettings;
   
         -- impulse to pass at next tick
         Impulse : Measures.Joules;         -- impulse to pass to generator
         ImpulseCount : Integer;            -- number of impulses to pass
         ImpulseFreq : Measures.TickCount;  -- impulse frequency (in ticks)
         ImpulseStart : Measures.TickCount; -- time of first impulse
         SendImpulse : Boolean;             -- whether to send impulse
      end record;

   -- initializes a new ICD ("Defribillator", hence "Def")
   procedure Init(Def : out ICDType);

   -- initialize settings for ICD
   procedure InitSettings(Def : out ICDType);

   -- switch on the defribrillator
    procedure On(Def: in out ICDType);

   -- switch off the defribrillator
   procedure Off(Def : in out ICDType);

   -- query the the mode of the defribrillator
   function IsOn(Def : in ICDType) return Boolean;

   -- fetches time (ticks)
   function GetTime(Def : in ICDType) return Measures.TickCount;
 
   -- fetches the heart rate history
   function GetHistory(Def : in ICDType) return HistoryType;
    
   -- fetches impulse to administer at next tick
   function GetImpulse(Def : in ICDType) return Measures.Joules;

   -- fetches TachyThresh setting (see ICDSettings type above)
   function GetTachyThresh(Def : in ICDType) return Measures.BPM;

   -- fetches TachyImpulse setting
   function GetTachyImpulse(Def : in ICDType) return Measures.Joules;

   -- fetches TachyImpulseCount setting
   function GetTachyImpulseCount(Def : in ICDType) return Integer;

   -- fetches FibImpulse setting
   function GetFibImpulse(Def : in ICDType) return Measures.Joules;

   -- changes TachyThresh setting
   procedure SetTachyThresh(Def : in out ICDType; Thresh : in Measures.BPM);

   -- changes TachyImpulse setting
   procedure SetTachyImpulse(Def : out ICDType;
     Impulse : in Measures.Joules);

   -- changes TachyImpulseCount setting
   procedure SetTachyImpulseCount(Def : out ICDType; Count : in Integer);

   -- changes FibImpulse setting
   procedure SetFibImpulse(Def : in out ICDType; 
     Impulse : in Measures.Joules);

   -- Tick the clock: read latest BPM & compute impulse
   procedure Tick(Def : in out ICDType; Rate : in Measures.BPM);

end ICD;
