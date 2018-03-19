with Heart;
with HRM;
with ICD;
with ImpulseGenerator;
with Network;
with Measures;
with Principal; use Principal;

with Ada.Text_IO; use Ada.Text_IO;

package body ClosedLoop is

   -- declare variables to store each component of the closed loop
   Hrt : Heart.HeartType;
   Mon : HRM.HRMType;
   Def : ICD.ICDType;
   Gen : ImpulseGenerator.GeneratorType;
   Net : Network.Network;

   -- variable to store the authorized principals
   KnownPrincipals : access Network.PrincipalArray;
   Cardiologist, Assistant, Patient : Principal.PrincipalPtr;

   -- initialize the closed loop : 
   procedure Init is
   begin
      -- init closed loop components
      Heart.Init(Hrt);
      HRM.Init(Mon);
      ICD.Init(Def);
      ImpulseGenerator.Init(Gen);

      -- create the authorized principals 
      KnownPrincipals := new Network.PrincipalArray(0..2);
      Cardiologist := new Principal.Principal;
      Assistant := new Principal.Principal;
      Patient := new Principal.Principal;

      Principal.InitPrincipalForRole(Cardiologist.all,
        Principal.Cardiologist);
      Principal.InitPrincipalForRole(Assistant.all,
        Principal.ClinicalAssistant);
      Principal.InitPrincipalForRole(Patient.all,
        Principal.Patient);

      KnownPrincipals(0) := Cardiologist;
      KnownPrincipals(1) := Assistant;
      KnownPrincipals(2) := Patient;

      -- init network interface
      Network.Init(Net, KnownPrincipals);
   end;

   -- respond to ModeOff network message
   procedure RespondSwitchModeOff(Msg : Network.NetworkMessage) is
   begin
      Put_Line("MESSAGE: Request Turn Off");

      -- only authorized cardiologist or assistant can switch off
      if (Msg.MOffSource = Cardiologist or
          Msg.MOffSource = Assistant)
      then
         Put_Line("... authorized");
         HRM.Off(Mon);
         ICD.Off(Def);
         ImpulseGenerator.Off(Gen);

         -- make sure to reset Heart Impulse to 0 when switching system off,
         --   or the simulated heart will continuously issue impulses to
         --   itself
         Heart.SetImpulse(Hrt, Measures.Joules(0));
      end if;
   end;

   -- respond to ModeOn network message
   procedure RespondSwitchModeOn(Msg : Network.NetworkMessage) is
   begin
      -- authorized cardiologist or assitant can switch on
      Put_Line("MESSAGE: Request Switch On");
      if (Msg.MOnSource = Cardiologist or 
          Msg.MOnSource = Assistant)
      then
         Put_Line("... authorized");
         HRM.On(Mon, Hrt);
         ICD.On(Def);
         ImpulseGenerator.On(Gen);
      end if;
   end;

   -- fetch ICD representation of medical history, and convert it to the
   --   format required by the Network interface
   function GetMedicalHistory(Def: ICD.ICDType)
     return Network.RateHistory is
      NetworkHistory : Network.RateHistory;
   begin
      -- copy the 5 most recent items from ICD's medical history into
      --   Network interface's representation of medical history
      for I in 1 .. Network.HISTORY_LENGTH loop
         NetworkHistory(I).Rate := ICD.GetHistory(Def)(I).Rate;
         NetworkHistory(I).Time := ICD.GetHistory(Def)(I).Time;
      end loop;
      return NetworkHistory;
   end;

   -- respond to ReadRateHistory message
   procedure RespondReadRateHistoryRequest(Msg : Network.NetworkMessage) is
   begin
      Put_Line("MESSAGE: Request Medical History");
      -- authorized cardiologist, assistant, or patient
      if (Msg.HSource = Cardiologist or
          Msg.HSource = Assistant or
          Msg.HSource = Patient)
      then
         Put_Line("... authorized");
         Network.SendMessage(Net,
           (MessageType => Network.ReadRateHistoryResponse,
            History => GetMedicalHistory(Def),
            HDestination => Msg.HSource));
      end if;
   end;

   -- respond to ReadSettings message
   procedure RespondReadSettingsRequest(Msg : Network.NetworkMessage) is
   begin
      Put_Line("MESSAGE: Request Read Settings");
      -- authorized cardiolgoist or assistant & must be in off mode
      if (Msg.RSource = Cardiologist or
          Msg.RSource = Assistant) and
          (not ICD.IsOn(Def))
      then
         Put_Line("... authorized");
         Network.SendMessage(Net,
           (MessageType => Network.ReadSettingsResponse,
            RDestination => Msg.RSource,
            RTachyBound => ICD.GetTachyThresh(Def),
            RJoulesToDeliver => ICD.GetTachyImpulse(Def)));
      end if;
   end;

   -- respond to ChangeSettings message
   procedure RespondChangeSettingsRequest(Msg : Network.NetworkMessage) is
   begin
      Put_Line("MESSAGE: Request Change Settings");
      -- authorized cardiologist or assistant & must be in off mode
      if (Msg.CSource = Cardiologist) and
         (not ICD.IsOn(Def))
      then
         Put_Line("... authorized");
         -- change settings
         ICD.SetTachyThresh(Def, Msg.CTachyBound);
         ICD.SetFibImpulse(Def, Msg.CJoulesToDeliver);
         Network.SendMessage(Net,
           (MessageType => Network.ChangeSettingsResponse,
            CDestination => Msg.CSource));
      end if;
   end;

   -- identifies the type of network message received, and delegates
   --  it to the proper response procedure
   procedure RespondNetworkMessage(Msg : in Network.NetworkMessage) is
   begin
      case Msg.MessageType is
         when Network.ModeOn => RespondSwitchModeOn(Msg);
         when Network.ModeOff => RespondSwitchModeoff(Msg);
         when Network.ReadRateHistoryRequest =>
           RespondReadRateHistoryRequest(Msg);
         when Network.ReadSettingsRequest =>
           RespondReadSettingsRequest(Msg);
         when Network.ChangeSettingsRequest =>
           RespondChangeSettingsRequest(Msg);
         when others => null;
      end case;
   end;

   -- Tick the closed loop
   procedure Tick is
      -- message retrieved from the network interface this tick
      Msg : Network.NetworkMessage;
      -- boolean querying whether new message available on network this tick
      MsgAvailable : Boolean;
      -- heart rate measurement from heart rate monitor this tick
      Rate: Measures.BPM;
      -- impulse generated by ICD software this tick
      Impulse : Measures.Joules;
   begin
      -- Tick Heart & Monitor : collect most recent reading
      Heart.Tick(Hrt);
      HRM.Tick(Mon, Hrt);
      HRM.GetRate(Mon, Rate);
      Put_Line("Heart Rate" & Rate'Image);
        
      -- Tick ICD : collect impulse
      ICD.Tick(Def, Rate);
      Impulse := ICD.GetImpulse(Def);

      -- pass impulse to generator
      ImpulseGenerator.SetImpulse(Gen, Impulse);
      ImpulseGenerator.Tick(Gen, Hrt);

      -- Tick Network : check for new message and respond
      Network.Tick(Net);
      Network.GetNewMessage(Net, MsgAvailable, Msg);
      if MsgAvailable then
         RespondNetworkMessage(Msg);
      end if;
   end;

end ClosedLoop;
