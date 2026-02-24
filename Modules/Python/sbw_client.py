"""
SBW Client Library for Python

A Python implementation of the Systems Biology Workbench client protocol.
This allows Python code to connect to an SBW broker and call methods on 
Delphi (or other) SBW modules.

Wire Protocol (little-endian):
- Handshake: client sends [length:4][moduleName:string], broker replies [length:4][moduleID:4]
- Call message: [length:4][destID:4][type:1][uid:4][srcID:4][serviceID:4][methodID:4][payload...]
- Reply message: [length:4][destID:4][type:1][uid:4][payload...]
- Error message: [length:4][destID:4][type:1][uid:4][errorCode:1][errorMsg:string][detailedMsg:string]

DataBlock format:
- Each value preceded by type byte
- Types: byte(0), integer(1), double(2), boolean(3), string(4), array(5), list(6), void(7), complex(8)
"""

import socket
import struct
import threading
from typing import Any, List, Tuple, Optional, Union
from dataclasses import dataclass
from enum import IntEnum
import numpy as np


# =============================================================================
# Constants
# =============================================================================

SBW_DEFAULT_PORT = 10102
SBW_BROKER_MODULE_ID = 0
SBW_NO_MODULE_ID = -1

# Message types
class MessageType(IntEnum):
    SEND = 0
    CALL = 1
    REPLY = 2
    ERROR = 3

# Data block types
class DataType(IntEnum):
    BYTE = 0
    INTEGER = 1
    DOUBLE = 2
    BOOLEAN = 3
    STRING = 4
    ARRAY = 5
    LIST = 6
    VOID = 7
    COMPLEX = 8
    TERMINATE = 9
    ERROR = 10

# Broker SYSTEM service method IDs
class BrokerMethod(IntEnum):
    GET_LIST_OF_MODULES = 0
    GET_SERVICE_IDS_BY_ID = 1
    GET_SERVICE_IDS_BY_NAME = 2
    GET_METHOD_IDS_BY_ID = 3
    GET_METHOD_IDS_BY_NAME = 4
    GET_MODULE_ID = 5
    GET_VERSION = 6
    GET_MODULE_DESCRIPTORS = 7
    GET_MODULE_DESC_BY_NAME = 8
    GET_MODULE_DESC_BY_ID = 9
    GET_MODULE_INSTANCE = 10
    FIND_SERVICES = 11
    GET_SERVICE_CATEGORIES = 12
    RELEASE = 13
    GET_SERVICE_DESC_BY_NAME = 14
    GET_SERVICE_DESC_BY_ID = 15
    GET_SERVICE_DESCRIPTORS = 16
    GET_EXISTING_MODULE_IDS = 17

# Module System Service method IDs (service -1)
class ModuleSystemMethod(IntEnum):
    GET_SERVICES = 0
    GET_METHODS = 1
    ON_OTHER_MODULE_SHUTDOWN = 2
    SHUTDOWN = 3
    GET_METHOD_HELP = 4
    ON_OTHER_MODULE_STARTUP = 5
    ON_REGISTRATION_CHANGE = 6


# =============================================================================
# Exceptions
# =============================================================================

class SBWError(Exception):
    """Base exception for SBW errors"""
    pass

class SBWConnectionError(SBWError):
    """Connection-related errors"""
    pass

class SBWCallError(SBWError):
    """Remote call errors"""
    def __init__(self, code: int, message: str, detail: str = ""):
        self.code = code
        self.message = message
        self.detail = detail
        super().__init__(f"[{code}] {message}: {detail}" if detail else f"[{code}] {message}")


# =============================================================================
# DataBlock Writer
# =============================================================================

class DataBlockWriter:
    """Encodes Python values into SBW DataBlock binary format"""
    
    def __init__(self):
        self.buffer = bytearray()
    
    def write_byte(self, value: int) -> 'DataBlockWriter':
        """Write a byte value with type prefix"""
        self.buffer.append(DataType.BYTE)
        self.buffer.append(value & 0xFF)
        return self
    
    def write_boolean(self, value: bool) -> 'DataBlockWriter':
        """Write a boolean value with type prefix"""
        self.buffer.append(DataType.BOOLEAN)
        self.buffer.append(1 if value else 0)
        return self
    
    def write_integer(self, value: int) -> 'DataBlockWriter':
        """Write a 32-bit signed integer with type prefix"""
        self.buffer.append(DataType.INTEGER)
        self.buffer.extend(struct.pack('<i', value))
        return self
    
    def write_double(self, value: float) -> 'DataBlockWriter':
        """Write a 64-bit double with type prefix"""
        self.buffer.append(DataType.DOUBLE)
        self.buffer.extend(struct.pack('<d', value))
        return self
    
    def write_string(self, value: str) -> 'DataBlockWriter':
        """Write a UTF-8 string with type prefix, length, and null terminator"""
        self.buffer.append(DataType.STRING)
        encoded = value.encode('utf-8')
        self.buffer.extend(struct.pack('<i', len(encoded)))
        self.buffer.extend(encoded)
        self.buffer.append(0)  # Null terminator
        return self
    
    def write_complex(self, real: float, imag: float) -> 'DataBlockWriter':
        """Write a complex number with type prefix"""
        self.buffer.append(DataType.COMPLEX)
        self.buffer.extend(struct.pack('<d', real))
        self.buffer.extend(struct.pack('<d', imag))
        return self
    
    def write_integer_array(self, values: List[int]) -> 'DataBlockWriter':
        """Write a 1D array of integers"""
        self.buffer.append(DataType.ARRAY)
        self.buffer.append(DataType.INTEGER)  # Element type
        self.buffer.extend(struct.pack('<i', 1))  # 1 dimension
        self.buffer.extend(struct.pack('<i', len(values)))  # Size
        for v in values:
            self.buffer.extend(struct.pack('<i', v))
        return self
    
    def write_double_array(self, values: List[float]) -> 'DataBlockWriter':
        """Write a 1D array of doubles"""
        self.buffer.append(DataType.ARRAY)
        self.buffer.append(DataType.DOUBLE)  # Element type
        self.buffer.extend(struct.pack('<i', 1))  # 1 dimension
        self.buffer.extend(struct.pack('<i', len(values)))  # Size
        for v in values:
            self.buffer.extend(struct.pack('<d', v))
        return self
    
    def write_string_array(self, values: List[str]) -> 'DataBlockWriter':
        """Write a 1D array of strings"""
        self.buffer.append(DataType.ARRAY)
        self.buffer.append(DataType.STRING)  # Element type
        self.buffer.extend(struct.pack('<i', 1))  # 1 dimension
        self.buffer.extend(struct.pack('<i', len(values)))  # Size
        for v in values:
            encoded = v.encode('utf-8')
            self.buffer.extend(struct.pack('<i', len(encoded)))
            self.buffer.extend(encoded)
            self.buffer.append(0)  # Null terminator
        return self
    
    def write_double_array_2d(self, values: List[List[float]]) -> 'DataBlockWriter':
        """Write a 2D array of doubles"""
        if not values:
            rows, cols = 0, 0
        else:
            rows = len(values)
            cols = len(values[0]) if values else 0
        
        self.buffer.append(DataType.ARRAY)
        self.buffer.append(DataType.DOUBLE)  # Element type
        self.buffer.extend(struct.pack('<i', 2))  # 2 dimensions
        self.buffer.extend(struct.pack('<i', rows))
        self.buffer.extend(struct.pack('<i', cols))
        for row in values:
            for v in row:
                self.buffer.extend(struct.pack('<d', v))
        return self
    
    def write_list_begin(self, count: int) -> 'DataBlockWriter':
        """Begin writing a list with the given item count"""
        self.buffer.append(DataType.LIST)
        self.buffer.extend(struct.pack('<i', count))
        return self
    
    def to_bytes(self) -> bytes:
        """Get the serialized data"""
        return bytes(self.buffer)


# =============================================================================
# DataBlock Reader
# =============================================================================

class DataBlockReader:
    """Decodes SBW DataBlock binary format into Python values"""
    
    def __init__(self, data: bytes):
        self.data = data
        self.pos = 0
    
    def _read_raw_byte(self) -> int:
        if self.pos >= len(self.data):
            raise SBWError("Read past end of data")
        b = self.data[self.pos]
        self.pos += 1
        return b
    
    def _read_raw_int32(self) -> int:
        if self.pos + 4 > len(self.data):
            raise SBWError("Read past end of data")
        value = struct.unpack_from('<i', self.data, self.pos)[0]
        self.pos += 4
        return value
    
    def _read_raw_double(self) -> float:
        if self.pos + 8 > len(self.data):
            raise SBWError("Read past end of data")
        value = struct.unpack_from('<d', self.data, self.pos)[0]
        self.pos += 8
        return value
    
    def _read_raw_bytes(self, count: int) -> bytes:
        if self.pos + count > len(self.data):
            raise SBWError("Read past end of data")
        data = self.data[self.pos:self.pos + count]
        self.pos += count
        return data
    
    def _expect_type(self, expected: DataType):
        actual = self._read_raw_byte()
        if actual != expected:
            raise SBWError(f"Type mismatch: expected {expected}, got {actual}")
    
    def peek_type(self) -> DataType:
        """Peek at the next type without consuming it"""
        if self.pos >= len(self.data):
            raise SBWError("No more data to peek")
        return DataType(self.data[self.pos])
    
    def has_more(self) -> bool:
        """Check if more data is available"""
        return self.pos < len(self.data)
    
    def remaining(self) -> int:
        """Get remaining bytes count"""
        return len(self.data) - self.pos
    
    def read_byte(self) -> int:
        """Read a byte value"""
        self._expect_type(DataType.BYTE)
        return self._read_raw_byte()
    
    def read_boolean(self) -> bool:
        """Read a boolean value"""
        self._expect_type(DataType.BOOLEAN)
        return self._read_raw_byte() != 0
    
    def read_integer(self) -> int:
        """Read a 32-bit integer"""
        self._expect_type(DataType.INTEGER)
        return self._read_raw_int32()
    
    def read_double(self) -> float:
        """Read a 64-bit double"""
        self._expect_type(DataType.DOUBLE)
        return self._read_raw_double()
    
    def read_string(self) -> str:
        """Read a UTF-8 string"""
        self._expect_type(DataType.STRING)
        length = self._read_raw_int32()
        data = self._read_raw_bytes(length)
        self._read_raw_byte()  # Consume null terminator
        return data.decode('utf-8')
    
    def read_complex(self) -> complex:
        """Read a complex number"""
        self._expect_type(DataType.COMPLEX)
        real = self._read_raw_double()
        imag = self._read_raw_double()
        return complex(real, imag)
    
    def read_integer_array(self) -> List[int]:
        """Read a 1D array of integers"""
        self._expect_type(DataType.ARRAY)
        elem_type = self._read_raw_byte()
        if elem_type != DataType.INTEGER:
            raise SBWError(f"Expected integer array, got element type {elem_type}")
        dims = self._read_raw_int32()
        if dims != 1:
            raise SBWError(f"Expected 1D array, got {dims} dimensions")
        size = self._read_raw_int32()
        return [self._read_raw_int32() for _ in range(size)]
    
    def read_double_array(self) -> List[float]:
        """Read a 1D array of doubles"""
        self._expect_type(DataType.ARRAY)
        elem_type = self._read_raw_byte()
        if elem_type != DataType.DOUBLE:
            raise SBWError(f"Expected double array, got element type {elem_type}")
        dims = self._read_raw_int32()
        if dims != 1:
            raise SBWError(f"Expected 1D array, got {dims} dimensions")
        size = self._read_raw_int32()
        return [self._read_raw_double() for _ in range(size)]
    
    def read_string_array(self) -> List[str]:
        """Read a 1D array of strings"""
        self._expect_type(DataType.ARRAY)
        elem_type = self._read_raw_byte()
        if elem_type != DataType.STRING:
            raise SBWError(f"Expected string array, got element type {elem_type}")
        dims = self._read_raw_int32()
        if dims != 1:
            raise SBWError(f"Expected 1D array, got {dims} dimensions")
        size = self._read_raw_int32()
        result = []
        for _ in range(size):
            length = self._read_raw_int32()
            data = self._read_raw_bytes(length)
            self._read_raw_byte()  # Null terminator
            result.append(data.decode('utf-8'))
        return result
    
    def read_double_array_2d(self) -> List[List[float]]:
        """Read a 2D array of doubles"""
        self._expect_type(DataType.ARRAY)
        elem_type = self._read_raw_byte()
        if elem_type != DataType.DOUBLE:
            raise SBWError(f"Expected double array, got element type {elem_type}")
        dims = self._read_raw_int32()
        if dims != 2:
            raise SBWError(f"Expected 2D array, got {dims} dimensions")
        rows = self._read_raw_int32()
        cols = self._read_raw_int32()
        result = []
        for _ in range(rows):
            row = [self._read_raw_double() for _ in range(cols)]
            result.append(row)
        return result
    
    def read_list_begin(self) -> int:
        """Read list header, returns item count"""
        self._expect_type(DataType.LIST)
        return self._read_raw_int32()
    
    def read_list(self) -> List[Any]:
        """Read a complete list, auto-detecting element types"""
        count = self.read_list_begin()
        return [self.read_any() for _ in range(count)]
    
    def read_any(self) -> Any:
        """Read any value, auto-detecting type"""
        if not self.has_more():
            return None
        
        type_byte = self.peek_type()
        
        if type_byte == DataType.BYTE:
            return self.read_byte()
        elif type_byte == DataType.BOOLEAN:
            return self.read_boolean()
        elif type_byte == DataType.INTEGER:
            return self.read_integer()
        elif type_byte == DataType.DOUBLE:
            return self.read_double()
        elif type_byte == DataType.STRING:
            return self.read_string()
        elif type_byte == DataType.COMPLEX:
            return self.read_complex()
        elif type_byte == DataType.LIST:
            return self.read_list()
        elif type_byte == DataType.ARRAY:
            return self._read_any_array()
        elif type_byte == DataType.VOID:
            self._read_raw_byte()  # Consume the void type byte
            return None
        else:
            raise SBWError(f"Unknown type byte: {type_byte}")
    
    def _read_any_array(self) -> Any:
        """Read an array of any type"""
        self._expect_type(DataType.ARRAY)
        elem_type = self._read_raw_byte()
        dims = self._read_raw_int32()
        
        # Read dimension sizes
        sizes = [self._read_raw_int32() for _ in range(dims)]
        total = 1
        for s in sizes:
            total *= s
        
        # Read elements based on type
        if elem_type == DataType.INTEGER:
            data = [self._read_raw_int32() for _ in range(total)]
        elif elem_type == DataType.DOUBLE:
            data = [self._read_raw_double() for _ in range(total)]
        elif elem_type == DataType.BYTE:
            data = list(self._read_raw_bytes(total))
        elif elem_type == DataType.BOOLEAN:
            data = [self._read_raw_byte() != 0 for _ in range(total)]
        elif elem_type == DataType.STRING:
            data = []
            for _ in range(total):
                length = self._read_raw_int32()
                s = self._read_raw_bytes(length).decode('utf-8')
                self._read_raw_byte()  # Null terminator
                data.append(s)
        elif elem_type == DataType.LIST:
            # Array of lists - read each list
            data = []
            for _ in range(total):
                count = self._read_raw_int32()  # List count (no type byte)
                items = [self.read_any() for _ in range(count)]
                data.append(items)
        else:
            raise SBWError(f"Unsupported array element type: {elem_type}")
        
        # Reshape for multi-dimensional arrays
        if dims == 1:
            return data
        elif dims == 2:
            rows, cols = sizes
            return [data[i*cols:(i+1)*cols] for i in range(rows)]
        else:
            return data  # Return flat for higher dimensions


# =============================================================================
# SBW Connection
# =============================================================================

class SBWConnection:
    """
    Connection to an SBW broker.
    
    Usage:
        conn = SBWConnection()
        conn.connect("localhost", 10102, "my.python.module")
        
        # Discover a module
        module_id = conn.get_module_id("edu.demo.mathserver")
        service_id = conn.find_service_id(module_id, "math")
        method_id = conn.find_method_id(module_id, service_id, "sin")
        
        # Call the method
        result = conn.call(module_id, service_id, method_id, 
                          DataBlockWriter().write_double(3.14159))
        
        conn.disconnect()
    """
    
    def __init__(self):
        self.socket: Optional[socket.socket] = None
        self.module_id: int = SBW_NO_MODULE_ID
        self.module_name: str = ""
        self._next_uid: int = 1
        self._uid_lock = threading.Lock()
        self._send_lock = threading.Lock()
    
    def connect(self, host: str = "127.0.0.1", port: int = SBW_DEFAULT_PORT, 
                module_name: str = "python.sbw.client"):
        """Connect to the broker and register this module"""
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))
        self.module_name = module_name
        
        # Send handshake: [length:4][moduleName:string with type prefix]
        writer = DataBlockWriter()
        writer.write_string(module_name)
        handshake_data = writer.to_bytes()
        
        length = 4 + len(handshake_data)  # 4 for length field itself
        self.socket.sendall(struct.pack('<i', length) + handshake_data)
        
        # Receive response: [length:4][moduleID:4]
        response = self._recv_exact(8)
        resp_length, self.module_id = struct.unpack('<ii', response)
    
    def disconnect(self):
        """Disconnect from the broker"""
        if self.socket:
            self.socket.close()
            self.socket = None
        self.module_id = SBW_NO_MODULE_ID
    
    def _recv_exact(self, count: int) -> bytes:
        """Receive exactly count bytes"""
        data = b''
        while len(data) < count:
            chunk = self.socket.recv(count - len(data))
            if not chunk:
                raise SBWConnectionError("Connection closed")
            data += chunk
        return data
    
    def _next_message_uid(self) -> int:
        """Get next unique message ID"""
        with self._uid_lock:
            uid = self._next_uid
            self._next_uid += 1
            return uid
    
    def call(self, dest_module: int, service_id: int, method_id: int,
             args: Union[DataBlockWriter, bytes, None] = None,
             timeout: float = 30.0) -> DataBlockReader:
        """
        Call a method on a remote module and wait for the reply.
        
        Args:
            dest_module: Target module ID
            service_id: Target service ID
            method_id: Target method ID
            args: Arguments as DataBlockWriter or raw bytes
            timeout: Timeout in seconds
        
        Returns:
            DataBlockReader containing the result
        
        Raises:
            SBWCallError: If the remote call returns an error
        """
        if args is None:
            payload = b''
        elif isinstance(args, DataBlockWriter):
            payload = args.to_bytes()
        else:
            payload = args
        
        uid = self._next_message_uid()
        
        # Build call message:
        # Length(4) + DestID(4) + Type(1) + UID(4) + SrcID(4) + SvcID(4) + MethID(4) + Payload
        header_size = 25
        total_length = header_size + len(payload)
        
        message = struct.pack('<i', total_length)  # Length
        message += struct.pack('<i', dest_module)   # Destination ID
        message += struct.pack('B', MessageType.CALL)  # Type
        message += struct.pack('<i', uid)           # UID
        message += struct.pack('<i', self.module_id)  # Source ID
        message += struct.pack('<i', service_id)    # Service ID
        message += struct.pack('<i', method_id)     # Method ID
        message += payload
        
        # Send and receive with locking
        with self._send_lock:
            old_timeout = self.socket.gettimeout()
            self.socket.settimeout(timeout)
            try:
                self.socket.sendall(message)
                
                # Receive response
                length_bytes = self._recv_exact(4)
                resp_length = struct.unpack('<i', length_bytes)[0]
                resp_data = self._recv_exact(resp_length - 4)
            finally:
                self.socket.settimeout(old_timeout)
        
        # Parse response header
        dest_id = struct.unpack('<i', resp_data[0:4])[0]
        msg_type = resp_data[4]
        resp_uid = struct.unpack('<i', resp_data[5:9])[0]
        
        if resp_uid != uid:
            raise SBWError(f"UID mismatch: expected {uid}, got {resp_uid}")
        
        if msg_type == MessageType.REPLY:
            # Reply payload starts at offset 9 (after header)
            return DataBlockReader(resp_data[9:])
        
        elif msg_type == MessageType.ERROR:
            # Error: code(1) + message(string) + detail(string)
            error_code = resp_data[9]
            reader = DataBlockReader(resp_data[10:])
            error_msg = reader.read_string()
            error_detail = reader.read_string() if reader.has_more() else ""
            raise SBWCallError(error_code, error_msg, error_detail)
        
        else:
            raise SBWError(f"Unexpected message type: {msg_type}")
    
    # =========================================================================
    # Broker Query Methods
    # =========================================================================
    
    def get_version(self) -> str:
        """Get broker version string"""
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_VERSION)
        return reader.read_string()
    
    def get_list_of_modules(self) -> List[Tuple[int, str]]:
        """
        Get list of all connected modules.
        
        Returns:
            List of (module_id, module_name) tuples
        """
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_LIST_OF_MODULES)
        # Returns a list where each item is [moduleID, moduleName]
        items = reader.read_list()
        result = []
        for item in items:
            if isinstance(item, list) and len(item) >= 2:
                result.append((item[0], item[1]))
        return result
    
    def get_module_id(self, module_name: str) -> int:
        """
        Get module ID by name.
        
        Returns:
            Module ID, or SBW_NO_MODULE_ID (-1) if not found
        """
        args = DataBlockWriter().write_string(module_name)
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_MODULE_ID, args)
        return reader.read_integer()
    
    def get_service_ids(self, module_id: int) -> List[Tuple[int, str]]:
        """
        Get list of services for a module.
        
        Returns:
            List of (service_id, service_name) tuples
        """
        args = DataBlockWriter().write_integer(module_id)
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_SERVICE_IDS_BY_ID, args)
        items = reader.read_list()
        result = []
        for item in items:
            if isinstance(item, list) and len(item) >= 2:
                result.append((item[0], item[1]))
        return result
    
    def get_method_ids(self, module_id: int, service_id: int) -> List[Tuple[int, str]]:
        """
        Get list of methods for a service.
        
        Returns:
            List of (method_id, signature) tuples
        """
        args = DataBlockWriter().write_integer(module_id).write_integer(service_id)
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_METHOD_IDS_BY_ID, args)
        items = reader.read_list()
        result = []
        for item in items:
            if isinstance(item, list) and len(item) >= 2:
                result.append((item[0], item[1]))
        return result
    
    def find_service_id(self, module_id: int, service_name: str) -> int:
        """
        Find service ID by name.
        
        Returns:
            Service ID, or -1 if not found
        """
        services = self.get_service_ids(module_id)
        for sid, name in services:
            if name.lower() == service_name.lower():
                return sid
        return -1
    
    def find_method_id(self, module_id: int, service_id: int, method_name: str) -> int:
        """
        Find method ID by name (searches in signatures).
        
        Returns:
            Method ID, or -1 if not found
        """
        methods = self.get_method_ids(module_id, service_id)
        method_name_lower = method_name.lower()
        for mid, sig in methods:
            # Check if method name appears in signature (e.g., "double sin(double)")
            if f"{method_name_lower}(" in sig.lower():
                return mid
        return -1
    
    def get_method_help(self, module_id: int, service_id: int, method_id: int) -> str:
        """Get help string for a method"""
        args = DataBlockWriter().write_integer(service_id).write_integer(method_id)
        reader = self.call(module_id, -1, ModuleSystemMethod.GET_METHOD_HELP, args)
        return reader.read_string()
    
    def get_existing_module_ids(self) -> List[int]:
        """Get IDs of all currently connected modules"""
        reader = self.call(SBW_BROKER_MODULE_ID, 0, BrokerMethod.GET_EXISTING_MODULE_IDS)
        return reader.read_integer_array()


# =============================================================================
# High-Level Service Proxy
# =============================================================================

class SBWService:
    """
    High-level proxy for calling methods on an SBW service.
    
    Usage:
        conn = SBWConnection()
        conn.connect()
        
        # Create a service proxy
        math_service = SBWService(conn, "edu.demo.mathserver", "math")
        
        # Call methods dynamically
        result = math_service.call("sin", DataBlockWriter().write_double(1.57))
        print(result.read_double())
    """
    
    def __init__(self, connection: SBWConnection, module_name: str, service_name: str):
        self.connection = connection
        self.module_name = module_name
        self.service_name = service_name
        
        # Resolve IDs
        self.module_id = connection.get_module_id(module_name)
        if self.module_id == SBW_NO_MODULE_ID:
            raise SBWError(f"Module not found: {module_name}")
        
        self.service_id = connection.find_service_id(self.module_id, service_name)
        if self.service_id < 0:
            raise SBWError(f"Service not found: {service_name}")
        
        # Cache method IDs
        self._method_cache: dict = {}
    
    def get_method_id(self, method_name: str) -> int:
        """Get method ID, using cache"""
        if method_name not in self._method_cache:
            mid = self.connection.find_method_id(self.module_id, self.service_id, method_name)
            if mid < 0:
                raise SBWError(f"Method not found: {method_name}")
            self._method_cache[method_name] = mid
        return self._method_cache[method_name]
    
    def call(self, method_name: str, args: Union[DataBlockWriter, None] = None) -> DataBlockReader:
        """Call a method by name"""
        method_id = self.get_method_id(method_name)
        return self.connection.call(self.module_id, self.service_id, method_id, args)
    
    def list_methods(self) -> List[Tuple[int, str]]:
        """List all methods in this service"""
        return self.connection.get_method_ids(self.module_id, self.service_id)


# =============================================================================
# Convenience Functions
# =============================================================================

def connect(host: str = "127.0.0.1", port: int = SBW_DEFAULT_PORT,
            module_name: str = "python.sbw.client") -> SBWConnection:
    """Create and connect an SBW connection"""
    conn = SBWConnection()
    conn.connect(host, port, module_name)
    return conn


# =============================================================================
# Main - Demo
# =============================================================================

if __name__ == "__main__":
    print("SBW Python Client Library")
    print("=" * 50)
    print()
    print("Usage example:")
    print()
    print("    from sbw_client import connect, DataBlockWriter")
    print()
    print("    # Connect to broker")
    print("    conn = connect()")
    print()
    print("    # List modules")
    print("    for mid, name in conn.get_list_of_modules():")
    print("        print(f'  [{mid}] {name}')")
    print()
    print("    # Call a method")
    print("    module_id = conn.get_module_id('edu.demo.mathserver')")
    print("    service_id = conn.find_service_id(module_id, 'math')")
    print("    method_id = conn.find_method_id(module_id, service_id, 'sin')")
    print()
    print("    args = DataBlockWriter().write_double(1.5708)")
    print("    result = conn.call(module_id, service_id, method_id, args)")
    print("    print(f'sin(pi/2) = {result.read_double()}')")
    print()
    print("    conn.disconnect()")
