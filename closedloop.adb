with Heart;
with HRM;
with ICD;
with ImpulseGenerator;
with Network;
with Principal; use Principal;

with Ada.Text_IO; use Ada.Text_IO;

package body ClosedLoop is

    -- declare variables to store each component of system
    Hrt : Heart.HeartType;
    Mon : HRM.HRMType;
    Def : ICD.ICDType;
    Gen : ImpulseGenerator.GeneratorType;

    KnownPrincipals : access Network.PrincipalArray;
    Cardiologist, Assistant, Patient : Principal.PrincipalPtr;

    Net : Network.Network;
    
    -- initialize the closed loop
    procedure Init is
    begin
        -- closed loop components
        Heart.Init(Hrt);
        HRM.Init(Mon);
        ICD.Init(Def);
        ImpulseGenerator.Init(Gen);

        -- authorized principals
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

        -- network interface
        Network.Init(Net, KnownPrincipals);
    end;

    -- turn the system off
    procedure RespondSwitchModeOff(Msg : Network.NetworkMessage) is
    begin
        HRM.Off(Mon);
        ICD.Off(Def);
        ImpulseGenerator.Off(Gen);
    end;

    -- format ICD representation of medical history to network represenation
    function GetMedicalHistory(Def: ICD.ICDType)
        return Network.RateHistory is
        ICDHistory : ICD.HistoryType;
        NetworkHistory : Network.RateHistory;
    begin
        ICDHistory := ICD.GetHistory(Def);
        for I in ICD.HistoryIndex'First .. ICD.HistoryIndex'First+4 loop
            NetworkHistory(I+1).Rate := ICDHistory(I).Rate;
            NetworkHistory(I+1).Time := ICDHistory(I).Time;
        end loop;
        return NetworkHistory;
    end;
    

    -- turn the system of
    procedure RespondSwitchModeOn(Msg : Network.NetworkMessage) is
    begin
        HRM.On(Mon, Hrt);
        ICD.On(Def);
        ImpulseGenerator.On(Gen);
    end;

    -- respond with rate history confirmation/rejection
    procedure RespondReadRateHistoryRequest(Msg : Network.NetworkMessage) is
    begin
        Network.SendMessage(Net,
            (MessageType => Network.ReadRateHistoryResponse,
             History => GetMedicalHistory(Def),
             HDestination => Msg.HSource));
    end;

    -- respond with settings read confirmation/rejection
    procedure RespondReadSettingsRequest(Msg : Network.NetworkMessage) is
    begin
        Network.SendMessage(
            Net,
            (
                MessageType => Network.ReadSettingsResponse,
                RDestination => Msg.RSource,
                RTachyBound => ICD.GetTachyThresh(Def),
                RJoulesToDeliver => ICD.GetTachyImpulse(Def)
            )
        );
    end;

    -- respond with settings change confirmation/rejection
    procedure RespondChangeSettingsRequest(Msg : Network.NetworkMessage) is
    begin
        Network.SendMessage(
            Net,
            (
                MessageType => Network.ChangeSettingsResponse,
                CDestination => Msg.CSource
            )    
        );
    end;

    -- checks if a message is authorized
    procedure RespondNetworkMessage(Msg : in Network.NetworkMessage) is
    begin
        case Msg.MessageType is
            when Network.ModeOn =>
                RespondSwitchModeOn(Msg);
            when Network.ModeOff => 
                RespondSwitchModeoff(Msg);
            when Network.ReadRateHistoryRequest =>
                RespondReadRateHistoryRequest(Msg);
            when Network.ReadSettingsRequest =>
                RespondReadSettingsRequest(Msg);
            when Network.ChangeSettingsRequest =>
                RespondChangeSettingsRequest(Msg);
            when others =>
                null;
        end case;
    end;

    -- simulate one tick of the clock
    procedure Tick is
        Msg : Network.NetworkMessage;
        NewMsgAvailable : Boolean;
    begin
        -- tick the network
        Network.Tick(Net);

        -- fetch new messages from network
        Network.GetNewMessage(Net, NewMsgAvailable, Msg);
        if NewMsgAvailable then
            RespondNetworkMessage(Msg);
        end if;

        -- tick remaining components
        Heart.Tick(Hrt);
        HRM.Tick(Mon, Hrt);
        ICD.Tick(Def, Mon, Gen);
        ImpulseGenerator.Tick(Gen, Hrt);
    end;

end ClosedLoop;
