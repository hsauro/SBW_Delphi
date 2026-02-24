program SBWBrokerDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWBrokerDemo.dpr
 *
 * Simple demo/test of the SBW Broker.
 *
 * This starts a broker and waits for connections.
 * Use SBWClientDemo to connect test clients.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  SBW.Types in 'SBW.Types.pas',
  SBW.DataBlock in 'SBW.DataBlock.pas',
  SBW.Message in 'SBW.Message.pas',
  SBW.Connection in 'SBW.Connection.pas',
  SBW.Broker in 'SBW.Broker.pas';

type
  TBrokerEventHandler = class
    procedure OnLog(Sender: TObject; const Msg: string);
    procedure OnModuleConnect(Sender: TObject; ModuleInfo: TSBWModuleInfo);
    procedure OnModuleDisconnect(Sender: TObject; ModuleID: SBWModuleID);
  end;

procedure TBrokerEventHandler.OnLog(Sender: TObject; const Msg: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now), ' ', Msg);
end;

procedure TBrokerEventHandler.OnModuleConnect(Sender: TObject; ModuleInfo: TSBWModuleInfo);
begin
  WriteLn(Format('>>> Module connected: ID=%d, Name=%s',
    [ModuleInfo.ModuleID, ModuleInfo.ModuleName]));
end;

procedure TBrokerEventHandler.OnModuleDisconnect(Sender: TObject; ModuleID: SBWModuleID);
begin
  WriteLn(Format('<<< Module disconnected: ID=%d', [ModuleID]));
end;

var
  Broker: TSBWBroker;
  EventHandler: TBrokerEventHandler;

begin
  try
    WriteLn('SBW Broker Demo');
    WriteLn('===============');
    WriteLn;

    InitializeSockets;
    try
      EventHandler := TBrokerEventHandler.Create;
      try
        Broker := TSBWBroker.Create;
        try
          Broker.OnLog := EventHandler.OnLog;
          Broker.OnModuleConnect := EventHandler.OnModuleConnect;
          Broker.OnModuleDisconnect := EventHandler.OnModuleDisconnect;

          Broker.Start(SBW_DEFAULT_PORT);

          WriteLn(Format('Broker running on port %d', [Broker.Port]));
          WriteLn('Press Enter to stop...');
          WriteLn;

          ReadLn;

          Broker.Stop;
        finally
          Broker.Free;
        end;
      finally
        EventHandler.Free;
      end;
    finally
      FinalizeSockets;
    end;

    WriteLn('Broker stopped.');
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.

