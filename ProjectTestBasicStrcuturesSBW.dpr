program ProjectTestBasicStrcuturesSBW;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWTestRunner.dpr
 *
 * Console application to run SBW unit tests.
 *
 * Build and run this to verify the DataBlock serialization works correctly.
 *****************************************************************************}

uses
  System.SysUtils,
  SBW.Types in 'SBW.Types.pas',
  SBW.DataBlock in 'SBW.DataBlock.pas',
  SBW.Tests.DataBlock in 'SBW.Tests.DataBlock.pas',
  SBW.Message in 'SBW.Message.pas',
  SBW.Tests.Message in 'SBW.Tests.Message.pas',
  SBW.Connection in 'SBW.Connection.pas',
  SBW.Tests.Connection in 'SBW.Tests.Connection.pas',
  SBW.Broker in 'SBW.Broker.pas',
  SBW.Signature in 'SBW.Signature.pas',
  SBW.Tests.Signature in 'SBW.Tests.Signature.pas',
  SBW.Module in 'SBW.Module.pas',
  SBW.Service in 'SBW.Service.pas',
  SBW.Tests.Service in 'SBW.Tests.Service.pas',
  SBW.Tests_Array in 'SBW.Tests_Array.pas',
  SBW_Array in 'SBW_Array.pas',
  SBW.Broker.ModuleService in 'SBW.Broker.ModuleService.pas',
  SBW.Broker.SystemService in 'SBW.Broker.SystemService.pas',
  SBW.BrokerTypes in 'SBW.BrokerTypes.pas';

var
  TotalTests, TotalPass, TotalFail: Integer;
  DataBlockTests: SBW.Tests.DataBlock.TDataBlockTests;
  MessageTests: SBW.Tests.Message.TMessageTests;
  SignatureTests: SBW.Tests.Signature.TSignatureTests;
  ServiceTests: SBW.Tests.Service.TServiceTests;
  ConnectionTests: SBW.Tests.Connection.TConnectionTests;
  RunConnectionTestsFlag: Boolean;

begin
  try
    TotalTests := 0;
    TotalPass := 0;
    TotalFail := 0;

    // Check if connection tests should be run
    RunConnectionTestsFlag := (ParamCount > 0) and (ParamStr(1) = '-all');

    // Run DataBlock tests
    DataBlockTests := SBW.Tests.DataBlock.TDataBlockTests.Create;
    try
      DataBlockTests.RunAllTests;
      Inc(TotalTests, DataBlockTests.TestCount);
      Inc(TotalPass, DataBlockTests.PassCount);
      Inc(TotalFail, DataBlockTests.FailCount);
    finally
      DataBlockTests.Free;
    end;

    WriteLn;

    // Run Message tests
    MessageTests := SBW.Tests.Message.TMessageTests.Create;
    try
      MessageTests.RunAllTests;
      Inc(TotalTests, MessageTests.TestCount);
      Inc(TotalPass, MessageTests.PassCount);
      Inc(TotalFail, MessageTests.FailCount);
    finally
      MessageTests.Free;
    end;

    WriteLn;

    // Run Signature tests
    SignatureTests := SBW.Tests.Signature.TSignatureTests.Create;
    try
      SignatureTests.RunAllTests;
      Inc(TotalTests, SignatureTests.TestCount);
      Inc(TotalPass, SignatureTests.PassCount);
      Inc(TotalFail, SignatureTests.FailCount);
    finally
      SignatureTests.Free;
    end;

    WriteLn;

    RunAllArrayTests;

    // Run Service tests
    ServiceTests := SBW.Tests.Service.TServiceTests.Create;
    try
      ServiceTests.RunAllTests;
      Inc(TotalTests, ServiceTests.TestCount);
      Inc(TotalPass, ServiceTests.PassCount);
      Inc(TotalFail, ServiceTests.FailCount);
    finally
      ServiceTests.Free;
    end;

    WriteLn;

    // Run Connection tests (requires sockets, may be skipped)
    if RunConnectionTestsFlag then
    begin
      ConnectionTests := SBW.Tests.Connection.TConnectionTests.Create;
      try
        ConnectionTests.RunAllTests;
        Inc(TotalTests, ConnectionTests.TestCount);
        Inc(TotalPass, ConnectionTests.PassCount);
        Inc(TotalFail, ConnectionTests.FailCount);
      finally
        ConnectionTests.Free;
      end;
      WriteLn;
    end
    else
    begin
      WriteLn('=== SBW Connection Tests ===');
      WriteLn('  (Skipped - run with -all to include socket tests)');
      WriteLn;
    end;

    WriteLn('============================================');
    WriteLn(Format('TOTAL: %d tests, %d passed, %d failed',
      [TotalTests, TotalPass, TotalFail]));
    if TotalFail = 0 then
      WriteLn('All tests passed!')
    else
      WriteLn('Some tests FAILED!');
    WriteLn('============================================');

    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ' + E.ClassName + ': ' + E.Message);
      ReadLn;
    end;
  end;
end.

