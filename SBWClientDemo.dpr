program SBWClientDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWClientDemo.dpr
 *
 * Simple demo/test of SBW client connection.
 *
 * This connects to a broker and demonstrates basic communication.
 * Start SBWBrokerDemo first, then run this.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  SBW.DataBlock in 'SBW.DataBlock.pas',
  SBW.Message in 'SBW.Message.pas',
  SBW.Types in 'SBW.Types.pas',
  SBW.Connection in 'SBW.Connection.pas';

var
  Connection: TSBWConnection;
  ModuleName: string;

begin
  try
    WriteLn('SBW Client Demo');
    WriteLn('===============');
    WriteLn;

    Write('Enter module name (or press Enter for "TestModule"): ');
    ReadLn(ModuleName);
    if ModuleName = '' then
      ModuleName := 'TestModule';

    try
      Connection := TSBWConnection.Create;
      try
        WriteLn(Format('Connecting to broker at localhost:%d...', [SBW_DEFAULT_PORT]));

        Connection.Connect('127.0.0.1', SBW_DEFAULT_PORT, ModuleName);

        WriteLn(Format('Connected! Assigned module ID: %d', [Connection.ModuleID]));
        WriteLn;
        WriteLn('Press Enter to disconnect...');
        ReadLn;

        Connection.Disconnect;
        WriteLn('Disconnected.');

      finally
        Connection.Free;
      end;
    finally
      //FinalizeSockets;
    end;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.

