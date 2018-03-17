with Heart;
with HRM;
with ICD;
with ImpulseGenerator;
with Network;

with Ada.Text_IO; use Ada.Text_IO;

package body ClosedLoop is

    -- declare variables to store each component of system
    Hrt : Heart.HeartType;
    Mon : HRM.HRMType;
    Def : ICD.ICDType;
    Gen : ImpulseGenerator.GeneratorType;
    KnownPrincipals : access Network.PrincipalArray;
    Net : Network.Network;
    
    -- instantiate the compon
    procedure Init is
    begin
        Heart.Init(Hrt);
        HRM.Init(Mon);
        ICD.Init(Def);
        ImpulseGenerator.Init(Gen);
        KnownPrincipals := new Network.PrincipalArray(0..2);
                

        Network.Init(Net, KnownPrincipals);
    end;

    -- checks if a message is authorized
    function IsMessageAuthorized(
        Msg : in Network.NetworkMessage
    ) return Boolean is
    begin
        return True;
    end;

    procedure TurnSystemOn is
    begin
        HRM.Off(Mon);
        ICD.Off(Def);
        ImpulseGenerator.Off(Gen);
    end;

    procedure TurnSystemOff is
    begin
        HRM.On(Mon, Hrt);
        ICD.On(Def);
        ImpulseGenerator.On(Gen);
    end;

    -- executes the message instructions, assumed to be authorized already
    procedure ExecuteMessage(
        Msg : in Network.NetworkMessage
    ) is
    begin
        if IsMessageAuthorized(Msg) then
            case Msg.MessageType is
                when Network.ModeOn => TurnSystemOn;
                when Network.ModeOff => TurnSystemOff;
                --when ReadRateHistoryRequest
                --when ChangeSettingsRequest
                --when ChangeSettingsResponse
                --when ReadSettingsResponse
                --when ReadRateHistoryResponse
                when others => null;
            end case;
        end if;
    end;

    -- simulate one tick of the clock
    procedure Tick is
    begin
        -- Stage 1: handle messages from network

        -- tick the network
        Network.Tick(Net);

        -- fetch and handle messages from network
        -- Message = Network.GetNewMessage
        -- if isAuthorized(message) { execute instructions }
        -- handle the message: (1) check authorization; (2) execute message instruction IF authorized

        -- Stage 2: Then tick the other devices
        Heart.Tick(Hrt);
        HRM.Tick(Mon, Hrt);
        ICD.Tick(Def, Mon, Gen);
        ImpulseGenerator.Tick(Gen, Hrt);
    end;

end ClosedLoop;
