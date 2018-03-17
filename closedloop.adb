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
    procedure SwitchModeOff is
    begin
        HRM.Off(Mon);
        ICD.Off(Def);
        ImpulseGenerator.Off(Gen);
    end;

    -- turn the system of
    procedure SwitchModeOn is
    begin
        HRM.On(Mon, Hrt);
        ICD.On(Def);
        ImpulseGenerator.On(Gen);
    end;

                --when ReadRateHistoryRequest
                --when ChangeSettingsRequest
                --when ChangeSettingsResponse
                --when ReadSettingsResponse
                --when ReadRateHistoryResponse

    -- checks if a message is authorized
    procedure ExecuteMessage(Msg : in Network.NetworkMessage) is
    begin

        case Msg.MessageType is
            when Network.ModeOn => 
                if (Msg.MOnSource = Cardiologist or 
                        Msg.MOnSource = Assistant) then
                    SwitchModeOn;
                end if;
            when Network.ModeOff =>
                if (Msg.MOffSource = Cardiologist or
                        Msg.MOffSource = Assistant) then
                    SwitchModeOff;
                end if;
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
            ExecuteMessage(Msg);
        end if;

        -- tick remaining components
        Heart.Tick(Hrt);
        HRM.Tick(Mon, Hrt);
        ICD.Tick(Def, Mon, Gen);
        ImpulseGenerator.Tick(Gen, Hrt);
    end;

end ClosedLoop;
