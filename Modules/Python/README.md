# Python SBW Client Library

A pure Python implementation of the Systems Biology Workbench (SBW) client protocol. This allows Python code to connect to an SBW broker and call methods on Delphi (or other language) SBW modules.

## Files

- **sbw_client.py** - The main library with connection, data encoding/decoding, and service proxy classes
- **sbw_math_client.py** - Example script demonstrating calls to a Math Server
- **test_sbw_datablock.py** - Unit tests for the data serialization layer

## Quick Start

```python
from sbw_client import connect, DataBlockWriter

# Connect to the broker
conn = connect()  # defaults to localhost:10102
print(f"Connected as module {conn.module_id}")

# List all modules
print("Connected modules:")
for mod_id, mod_name in conn.get_list_of_modules():
    print(f"  [{mod_id}] {mod_name}")

# Find a specific module
module_id = conn.get_module_id("edu.demo.mathserver")
service_id = conn.find_service_id(module_id, "math")
method_id = conn.find_method_id(module_id, service_id, "sin")

# Call a method
args = DataBlockWriter().write_double(1.5708)  # pi/2
result = conn.call(module_id, service_id, method_id, args)
print(f"sin(pi/2) = {result.read_double()}")

conn.disconnect()
```

## High-Level Service Proxy

For cleaner code, use the `SBWService` class:

```python
from sbw_client import connect, SBWService, DataBlockWriter

conn = connect()

# Create a service proxy (auto-resolves module/service IDs)
math = SBWService(conn, "edu.demo.mathserver", "math")

# List available methods
for method_id, signature in math.list_methods():
    print(f"  [{method_id}] {signature}")

# Call methods by name
args = DataBlockWriter().write_double(3.14159)
result = math.call("sin", args)
print(f"sin(pi) = {result.read_double()}")

conn.disconnect()
```

## Data Types

The library supports all SBW data types:

```python
from sbw_client import DataBlockWriter, DataBlockReader

writer = DataBlockWriter()

# Scalars
writer.write_byte(42)
writer.write_boolean(True)
writer.write_integer(12345)
writer.write_double(3.14159)
writer.write_string("Hello, SBW!")
writer.write_complex(1.0, 2.0)  # 1 + 2i

# Arrays
writer.write_integer_array([1, 2, 3, 4, 5])
writer.write_double_array([1.1, 2.2, 3.3])
writer.write_string_array(["one", "two", "three"])
writer.write_double_array_2d([[1, 2], [3, 4], [5, 6]])

# Lists (heterogeneous)
writer.write_list_begin(3)
writer.write_integer(42)
writer.write_string("mixed")
writer.write_double(3.14)
```

Reading is symmetric:

```python
reader = DataBlockReader(data)

value = reader.read_integer()
text = reader.read_string()
arr = reader.read_double_array()
items = reader.read_list()  # auto-detects element types
any_value = reader.read_any()  # reads any type
```

## Broker Discovery Methods

```python
# Get broker version
version = conn.get_version()

# List all modules
modules = conn.get_list_of_modules()  # -> [(id, name), ...]

# Get module ID by name
mod_id = conn.get_module_id("module.name")

# Get services for a module
services = conn.get_service_ids(mod_id)  # -> [(id, name), ...]

# Get methods for a service  
methods = conn.get_method_ids(mod_id, svc_id)  # -> [(id, signature), ...]

# Get method help
help_text = conn.get_method_help(mod_id, svc_id, meth_id)

# Get all connected module IDs
ids = conn.get_existing_module_ids()  # -> [1, 2, 3, ...]
```

## Wire Protocol

The library implements the SBW binary protocol:

- **Handshake**: Client sends module name, broker returns assigned module ID
- **Call messages**: 25-byte header + payload
- **Reply messages**: 13-byte header + payload  
- **Error messages**: Error code + message + detail

All integers are 32-bit signed, little-endian. Doubles are IEEE 64-bit. Strings are UTF-8 with length prefix and null terminator.

## Requirements

- Python 3.7+
- No external dependencies (uses only standard library)

## Running the Example

1. Start your SBW broker
2. Start the Math Server module
3. Run the Python client:

```bash
python sbw_math_client.py [host] [port]
```

## Running Tests

```bash
python test_sbw_datablock.py
```

Or with pytest:

```bash
pip install pytest
pytest test_sbw_datablock.py -v
```

## Architecture Notes

The implementation matches your Delphi SBW implementation:

- `DataBlockWriter` / `DataBlockReader` - Matches `TSBWDataBlockWriter` / `TSBWDataBlockReader`
- `SBWConnection.call()` - Matches `TSBWConnection.Call()`
- `SBWService` - Similar to `TSBWRemoteService`

The protocol constants (message types, service IDs, method IDs) match those defined in your `SBW.Types.pas`, `SBW.Broker.SystemService.pas`, and `SBW.Broker.ModuleService.pas`.
