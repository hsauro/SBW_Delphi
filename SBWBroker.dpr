program SBWBroker;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWBroker.dpr
 *
 * Standalone SBW Broker.
 *
 * Run this first, then start the MathServer, then run clients.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  SBW.Broker in 'SBW.Broker.pas',
  SBW.Types in 'SBW.Types.pas',
  SBW.Connection in 'SBW.Connection.pas',
  SBW.BrokerTypes in 'SBW.BrokerTypes.pas',
  SBW.Broker.Registry in 'SBW.Broker.Registry.pas',
  SBW.DataBlock in 'SBW.DataBlock.pas',
  SBW.Message in 'SBW.Message.pas',
  SBW.Broker.ModuleService in 'SBW.Broker.ModuleService.pas';

type
  TBrokerEventHandler = class
    procedure OnLog(Sender: TObject; const Msg: string);
    procedure OnModuleConnect(Sender: TObject; ModuleInfo: TSBWModuleInfo);
    procedure OnModuleDisconnect(Sender: TObject; ModuleID: SBWModuleID);
  end;

procedure TBrokerEventHandler.OnLog(Sender: TObject; const Msg: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now), ' [Broker] ', Msg);
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
    WriteLn('=================================');
    WriteLn('  SBW Broker');
    WriteLn('=================================');
    WriteLn;

    try
      EventHandler := TBrokerEventHandler.Create;
      try
        Broker := TSBWBroker.Create;
        Writeln ('Version: ', TSBWBroker.GetVersion);
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

