unit SBW.Types;

{******************************************************************************
 * SBW.Types.pas
 *
 * Basic types and constants for the Systems Biology Workbench
 * Delphi implementation.
 *
 * This unit defines the fundamental data types, message types, and
 * constants used throughout the SBW system.
 *****************************************************************************}

interface

type
  /// <summary>
  /// 32-bit signed integer used throughout SBW for IDs and counts
  /// </summary>
  SBWInteger = Int32;

  /// <summary>
  /// 64-bit IEEE double precision floating point
  /// </summary>
  SBWDouble = Double;

  /// <summary>
  /// Boolean type (stored as single byte: 0=false, 1=true)
  /// </summary>
  SBWBoolean = Boolean;

  /// <summary>
  /// Single byte type
  /// </summary>
  SBWByte = Byte;

  /// <summary>
  /// Module instance identifier
  /// </summary>
  SBWModuleID = SBWInteger;

  /// <summary>
  /// Service identifier within a module
  /// </summary>
  SBWServiceID = SBWInteger;

  /// <summary>
  /// Method identifier within a service
  /// </summary>
  SBWMethodID = SBWInteger;

  /// <summary>
  /// Data block type indicators - each value in a data block is preceded
  /// by a type byte indicating what follows
  /// </summary>
  TSBWDataBlockType = (
    dbtByte      = 0,
    dbtInteger   = 1,
    dbtDouble    = 2,
    dbtBoolean   = 3,
    dbtString    = 4,
    dbtArray     = 5,
    dbtList      = 6,
    dbtVoid      = 7,
    dbtComplex   = 8,
    dbtTerminate = 9,
    dbtError     = 10
  );

  /// <summary>
  /// Message types sent over the wire
  /// </summary>
  TSBWMessageType = (
    mtSend  = 0,   // Fire and forget, no reply expected
    mtCall  = 1,   // Blocking call, expects reply
    mtReply = 2,   // Reply to a call
    mtError = 3    // Error response
  );

  /// <summary>
  /// Module management types - how module instances are created and managed
  /// </summary>
  TSBWModuleManagementType = (
    mmtUnique      = 0,  // Single shared instance for all requesters
    mmtSelfManaged = 1   // New instance per request, module controls lifetime
  );

  /// <summary>
  /// Module descriptor - information about a registered module
  /// </summary>
  TSBWModuleDescriptor = record
    Name: string;            // Module identification name (e.g., 'edu.caltech.trigplus')
    DisplayName: string;     // Human-readable name
    ManagementType: TSBWModuleManagementType;
    CommandLine: string;     // Command to launch the module
    Help: string;            // Documentation string
  end;

  /// <summary>
  /// Service descriptor - information about a service within a module
  /// </summary>
  TSBWServiceDescriptor = record
    ServiceName: string;        // Service identification name
    ServiceDisplayName: string; // Human-readable name
    ServiceCategory: string;    // Category path (e.g., 'Analysis/Simulation')
    Module: TSBWModuleDescriptor;
    Help: string;
  end;

  /// <summary>
  /// Complex number type for SBW
  /// </summary>
  TSBWComplex = record
    Real: SBWDouble;
    Imag: SBWDouble;
    class operator Implicit(const Value: SBWDouble): TSBWComplex;
    class operator Equal(const A, B: TSBWComplex): Boolean;
    class operator NotEqual(const A, B: TSBWComplex): Boolean;
  end;

const
  /// <summary>
  /// Default broker port
  /// </summary>
  SBW_DEFAULT_PORT = 10102;

  /// <summary>
  /// Protocol version
  /// </summary>
  SBW_PROTOCOL_VERSION = 1;

  /// <summary>
  /// Message header size in bytes:
  /// Length(4) + DestID(4) + Type(1) + UID(4) + SourceID(4) + ServiceID(4) + MethodID(4) = 25
  /// </summary>
  SBW_MESSAGE_HEADER_SIZE = 25;

  /// <summary>
  /// Reply/Error header size (no service/method IDs needed):
  /// Length(4) + DestID(4) + Type(1) + UID(4) + Payload
  /// </summary>
  SBW_REPLY_HEADER_SIZE = 13;

  /// <summary>
  /// Special module IDs
  /// </summary>
  SBW_BROKER_MODULE_ID = 0;
  SBW_NO_MODULE_ID = -1;

  /// <summary>
  /// Data type byte values (for reference - matches TSBWDataBlockType ordinals)
  /// </summary>
  SBW_TYPE_BYTE      = 0;
  SBW_TYPE_INTEGER   = 1;
  SBW_TYPE_DOUBLE    = 2;
  SBW_TYPE_BOOLEAN   = 3;
  SBW_TYPE_STRING    = 4;
  SBW_TYPE_ARRAY     = 5;
  SBW_TYPE_LIST      = 6;
  SBW_TYPE_VOID      = 7;
  SBW_TYPE_COMPLEX   = 8;
  SBW_TYPE_TERMINATE = 9;
  SBW_TYPE_ERROR     = 10;

implementation

{ TSBWComplex }

class operator TSBWComplex.Implicit(const Value: SBWDouble): TSBWComplex;
begin
  Result.Real := Value;
  Result.Imag := 0.0;
end;

class operator TSBWComplex.Equal(const A, B: TSBWComplex): Boolean;
begin
  Result := (A.Real = B.Real) and (A.Imag = B.Imag);
end;

class operator TSBWComplex.NotEqual(const A, B: TSBWComplex): Boolean;
begin
  Result := not (A = B);
end;

end.

