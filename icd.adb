with HRM;
with ImpulseGenerator;
with Measures; use Measures;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- implementation of ICD software
package body ICD is

   -- initialize a new ICD ("Defribrillator", hence "Def")
   procedure Init(Def : out ICDType) is
   begin
      -- default start state and time
      Def.IsOn := DEFAULT_START_MODE;
      Def.Time := DEFAULT_START_TIME;
      -- Def.History is instantiated when monitor sends heart rates
      InitSettings(Def);

      -- impulse to apply, number of times, and frequency
      Def.Impulse := DEFAULT_IMPULSE;
      Def.ImpulseCount := DEFAULT_IMPULSE_COUNT;
      Def.ImpulseFreq := DEFAULT_IMPULSE_FREQUENCY;
      Def.SendImpulse := DEFAULT_SEND_IMPULSE;
      -- Def.ImpulseStart takes no initial value, derived from clock time
   end Init;

   -- initialize setting configurations
   procedure InitSettings(Def : out ICDType) is
   begin
      -- default anomaly response settings
      Def.Settings.TachyThresh := DEFAULT_TACHY_THRESH;
      Def.Settings.TachyImpulse := DEFAULT_TACHY_IMPULSE;
      Def.Settings.TachyImpulseCount := DEFAULT_TACHY_IMPULSE_COUNT;
      Def.Settings.FibImpulse := DEFAULT_FIB_IMPULSE;
   end;

   -- switch to ON mode
   procedure On(Def : in out ICDType) is
   begin
      if not IsOn(Def) then
         Def.IsOn := True;
      end if;
   end On;

   -- switch to OFF mode -- FLAG : REFACTOR WITH CONSTANTS OR RECORD
   procedure Off(Def : in out ICDType) is
   begin
      if IsOn(Def) then
         Def.IsOn := False;
         Def.Impulse := Measures.Joules(0);
         Def.ImpulseCount := 0;
         Def.SendImpulse := False;
      end if;
   end Off;

   -- checks whether defribulator is in on mode
   function IsOn(Def : in ICDType) return Boolean is
   begin
      return Def.IsOn;
   end IsOn;

   -- fetches time (clock ticks)
   function GetTime(Def : in ICDType) return Measures.TickCount is
   begin
      return Def.Time;
   end GetTime;

   -- fetch rate history : array of (BPM, Time) tuples
   function GetHistory(Def : in ICDType) return HistoryType is
   begin 
      return Def.History;
   end GetHistory;

   -- get the impulse to be sent on to impulse generator
   function GetImpulse(Def : in ICDType) return Measures.Joules is
   begin
      if Def.SendImpulse then
         Ada.Text_IO.Put_Line("IMPULSE:" & Def.Impulse'Image);
         return Def.Impulse;
      else
         return Measures.Joules(0);
      end if;
   end GetImpulse;

   -- fetch setting : BPM threshold for tachycardia
   function GetTachyThresh(Def : in ICDType) return Measures.BPM is
   begin
      return Def.Settings.TachyThresh;
   end GetTachyThresh;

   -- fetch setting : impulse (joules) to respond to tachycardia
   function GetTachyImpulse(Def : in ICDType) return Measures.Joules is
   begin
      return Def.Settings.TachyImpulse;
   end GetTachyImpulse;

   -- fetch setting : number of impulses for tachycardia
   function GetTachyImpulseCount(Def : in ICDType) return Integer is
   begin
      return Def.Settings.TachyImpulseCount;
   end;

   -- fetch setting : impules (joules) to respond to fibrillation
   function GetFibImpulse(Def : in ICDType) return Measures.Joules is
   begin
      return Def.Settings.FibImpulse;
   end GetFibImpulse;

   -- change setting : BPM threshold for tachycardia
   procedure SetTachyThresh(Def : in out ICDType;
     Thresh : in Measures.BPM) is
   begin
      Def.Settings.TachyThresh := Thresh;
   end;

   -- change setting : impulse (in joules) to deliver for tachycardia
   procedure SetTachyImpulse(Def : out ICDType;
     Impulse : in Measures.Joules) is
   begin
      Def.Settings.TachyImpulse := Impulse;
   end;

   -- change setting : number of impulses to deliver for tachycardia
   procedure SetTachyImpulseCount(Def : out ICDType; Count : in Integer) is
   begin
      Def.Settings.TachyImpulseCount := Count;
   end;

   -- change setting : impulse (joules) to respond to fibrillation
   procedure SetFibImpulse(Def : in out ICDType;
     Impulse : in Measures.Joules) is
   begin
      Def.Settings.FibImpulse := Impulse;
   end;

   -- update medical history
   procedure UpdateHistory(Def : in out ICDType; Rate : in Measures.BPM) is
      -- used to iterate backwards through history array
      J : Integer;
   begin
      -- move each history record forwards one place in the array
      move_records_forward:
         for I in HISTORY_START_INDEX .. HISTORY_LENGTH - 1 loop
            J := HISTORY_LENGTH - I;
            Def.History(J + 1).Rate := Def.History(J).Rate;
            Def.History(J + 1).Time := Def.History(J).Time;
         end loop move_records_forward;

      -- update first record with most recent medical history
      Def.History(HISTORY_START_INDEX).Rate := Rate;
      Def.History(HISTORY_START_INDEX).Time := Def.Time;
   end;

   -- check if heart rate history indicates tacycardia
   function IsTachycardic(Def : in ICDType) return Boolean is
   begin
      -- check if most recent heart rate exceeds tachycardia threshold
      return Def.History(HISTORY_START_INDEX).Rate >
         Def.Settings.TachyThresh;
   end IsTachycardic;

   -- check if heart rate history indicates ventricular fibrillation
   function IsFibrillating(Def: in ICDType) return Boolean is
      -- sum of absolute differences between heart rates
      SumAbsDiffs : Integer;
      -- average rate of change in heart rate across
      AvgRateChange : Float;
   begin
      -- check that the readings used to compute average change in heart
      --  rate are all contiguous. This is necessary using difference in 
      --  the measurements of heart rates at greatly separated times
      --  in the calculation; without this check, there is a tendency to
      --  incorrectly infer ventricular fibrillation from medical history.
      if Def.History(NUM_DIFFS_FOR_ESTIMATE + 1).Time
        - Def.History(1).Time > TickCount(NUM_DIFFS_FOR_ESTIMATE) then
         return False;
      end if;

      -- check that we actually have observations from the monitor for the
      --  most ticks that we need to estimate average change in heart rate.
      --  It is possible that we don't yet if the ICD has only just been
      --  initialized, if the monitor has been feeding OFF mode
      --  observations to the ICD, or if we have just issued an impulse
      --  sufficiently large to cause the heart to stop.
      check_have_observations:
         for I in HISTORY_START_INDEX .. HISTORY_START_INDEX +
           NUM_DIFFS_FOR_ESTIMATE loop
            if Def.History(I).Rate = Measures.BPM'First then
               return False;
            end if;
         end loop check_have_observations;

      -- sum up average differences
      SumAbsDiffs := 0;
      sum_abs_diffs:
         for I in HISTORY_START_INDEX .. HISTORY_START_INDEX +
           NUM_DIFFS_FOR_ESTIMATE loop
            SumAbsDiffs := SumAbsDiffs +
              abs(Def.History(I).Rate-Def.History(I+1).Rate);
        end loop sum_abs_diffs;

      -- then divide by number of observations
      AvgRateChange := Float(SumAbsDiffs) / Float(NUM_DIFFS_FOR_ESTIMATE);
       
      -- infer ventricular fibrillation if average rate change
      --  exceeds predefined limit 
      return AvgRateChange >= Float(DEFAULT_FIB_THRESH);
    end IsFibrillating;

   -- computes the number of ticks between each impulse for the
   --  response to tachycardia, given that the rate at which impulses
   --  must be released in response to the anomaly must be 15 BPM
   --  higher than the most recent heart rate measurement
   function ComputeImpulseFreq(Rate : in Measures.BPM)
     return Measures.TickCount is
      TicksPerBeat : Float;
   begin
      -- if the given rate is <= 0, then it's not possible to convert
      --   BPM to ticks-per-beat; return the maximum possible value for
      --   tick count (a proxy for "undefined" or "infinity")
      if Rate <= Measures.BPM(0) then
         return Measures.TickCount'Last;
      end if;

      -- otherwise perform the conversion
      TicksPerBeat := (SECONDS_PER_MINUTE / SECONDS_PER_TICK ) /
        (Float(Rate) + TACHY_BPM_INCREMENT);

      -- round to nearest integer (since ticks are discrete)
      return Measures.TickCount(Float'Rounding(TicksPerBeat));
   end ComputeImpulseFreq;

   -- compute the impulse to be delivered to the impulse generator for
   --   this tick. Also generates boolean indicating whether an impulse
   --   should be sent at all this tick.
   procedure ComputeImpulse(Def : in out ICDType) is
   begin
      -- default, send no impulse
      Def.SendImpulse := False;

      -- check if currently administering impulses for tachycardia
      if Def.ImpulseCount > 0 then
         -- check if need to send another impulse this tick
         if ((Def.Time - Def.ImpulseStart) rem Def.ImpulseFreq) = 0 then
            Put_Line("TIME: " & Def.Time'Image);
            Put_Line("COUNT: Impulses remaining:" & Def.ImpulseCount'Image);
            Def.SendImpulse := True;
            Def.ImpulseCount := Def.ImpulseCount - 1;
         end if;

      -- check for tachycardia
      elsif IsTachycardic(Def) then
         Put_Line("WARNING: Tachycardia Detected");
         Def.Impulse := ICD.GetTachyImpulse(Def);
         Def.ImpulseCount := ICD.GetTachyImpulseCount(Def);
         Def.ImpulseStart := ICD.GetTime(Def);
         Def.ImpulseFreq := ComputeImpulseFreq(Def.History(1).Rate);
         Def.SendImpulse := True;
        
      -- check if ventricular fibrillation detected
      elsif IsFibrillating(Def) then
         Ada.Text_IO.Put_Line("WARNING: Fibrillation Detected");
         Def.Impulse := ICD.GetFibImpulse(Def);
         Def.SendImpulse := True;

      -- otherwise, default to zero joules for safety
      else
         Def.Impulse := Measures.Joules(0);
      end if;
   end;

   -- Tick defribrillator
   procedure Tick(Def : in out ICDType; Rate : in Measures.BPM) is
   begin
      -- update the clock time - doesn't care aboure on/off mode
      Def.Time := Def.Time + 1;
 
      -- update medical history and compute impulse for this tick
      if Def.IsOn then
         UpdateHistory(Def, Rate);
         ComputeImpulse(Def);
      end if;
   end Tick;

end ICD;
