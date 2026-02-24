#!/usr/bin/env python3
"""
SBW Math Client Example

Demonstrates calling a Delphi Math Server via SBW from Python.
This mirrors the functionality of the Delphi SBWMathClient.dpr.

Start the broker first, then the MathServer, then run this script.
"""

import sys
import math
from sbw_client import (
    connect, SBWConnection, SBWService, DataBlockWriter, DataBlockReader,
    SBW_BROKER_MODULE_ID, SBW_NO_MODULE_ID
)


def print_separator(title: str):
    print()
    print("=" * 50)
    print(f"  {title}")
    print("=" * 50)
    print()


def discover_modules(conn: SBWConnection):
    """List all connected modules and their services"""
    print_separator("Module Discovery")
    
    print(f"Broker version: {conn.get_version()}")
    print()
    
    modules = conn.get_list_of_modules()
    print(f"Found {len(modules)} connected modules:")
    
    for mod_id, mod_name in modules:
        print(f"\n  [{mod_id}] {mod_name}")
        
        # Get services for this module
        services = conn.get_service_ids(mod_id)
        for svc_id, svc_name in services:
            print(f"      Service [{svc_id}] {svc_name}")
            
            # Get methods for this service
            methods = conn.get_method_ids(mod_id, svc_id)
            for meth_id, signature in methods:
                print(f"          [{meth_id}] {signature}")


def call_math_server_lowlevel(conn: SBWConnection):
    """Call the math server using low-level API"""
    print_separator("Low-Level Math Server Calls")
    
    # Find the math server
    server_id = conn.get_module_id("edu.demo.mathserver")
    if server_id == SBW_NO_MODULE_ID:
        print("ERROR: Math server not found! Make sure it is running.")
        return
    
    print(f"Math Server found at module ID {server_id}")
    
    # Find the math service
    service_id = conn.find_service_id(server_id, "math")
    if service_id < 0:
        print("ERROR: Math service not found!")
        return
    
    print(f"Math service ID: {service_id}")
    
    # Find method IDs
    sin_method = conn.find_method_id(server_id, service_id, "sin")
    cos_method = conn.find_method_id(server_id, service_id, "cos")
    add_method = conn.find_method_id(server_id, service_id, "add")
    sum_method = conn.find_method_id(server_id, service_id, "sum")
    
    print(f"Method IDs: sin={sin_method}, cos={cos_method}, add={add_method}, sum={sum_method}")
    print()
    
    # Test sin
    print("Testing sin function:")
    for x in [0, math.pi/2, math.pi]:
        args = DataBlockWriter().write_double(x)
        result = conn.call(server_id, service_id, sin_method, args)
        y = result.read_double()
        print(f"  sin({x:.4f}) = {y}")
    print()
    
    # Test cos
    print("Testing cos function:")
    for x in [0, math.pi/2, math.pi]:
        args = DataBlockWriter().write_double(x)
        result = conn.call(server_id, service_id, cos_method, args)
        y = result.read_double()
        print(f"  cos({x:.4f}) = {y}")
    print()
    
    # Test add
    print("Testing add function:")
    test_pairs = [(3.5, 2.5), (-10, 10), (100.5, 0.5)]
    for a, b in test_pairs:
        args = DataBlockWriter().write_double(a).write_double(b)
        result = conn.call(server_id, service_id, add_method, args)
        y = result.read_double()
        print(f"  add({a}, {b}) = {y}")
    print()
    
    # Test sum (integer array)
    print("Testing sum function:")
    test_arrays = [[1, 2, 3, 4, 5], list(range(1, 11)), [100, -50, 25]]
    for arr in test_arrays:
        args = DataBlockWriter().write_integer_array(arr)
        result = conn.call(server_id, service_id, sum_method, args)
        y = result.read_integer()
        print(f"  sum({arr}) = {y}")


def call_math_server_highlevel(conn: SBWConnection):
    """Call the math server using high-level SBWService proxy"""
    print_separator("High-Level Math Server Calls")
    
    try:
        math = SBWService(conn, "edu.demo.mathserver", "math")
    except Exception as e:
        print(f"ERROR: Could not connect to math service: {e}")
        return
    
    print(f"Connected to math service (module={math.module_id}, service={math.service_id})")
    print()
    
    # List available methods
    print("Available methods:")
    for mid, sig in math.list_methods():
        print(f"  [{mid}] {sig}")
    print()
    
    # Call methods using the high-level API
    print("Calling methods:")
    
    # sin(pi/2)
    import math as mathlib
    args = DataBlockWriter().write_double(mathlib.pi/2)
    result = math.call("sin", args)
    print(f"  sin(π/2) = {result.read_double()}")
    
    # cos(0)
    args = DataBlockWriter().write_double(0)
    result = math.call("cos", args)
    print(f"  cos(0) = {result.read_double()}")
    
    # add(3.5, 2.5)
    args = DataBlockWriter().write_double(3.5).write_double(2.5)
    result = math.call("add", args)
    print(f"  add(3.5, 2.5) = {result.read_double()}")
    
    # sum([1,2,3,4,5])
    args = DataBlockWriter().write_integer_array([1, 2, 3, 4, 5])
    result = math.call("sum", args)
    print(f"  sum([1,2,3,4,5]) = {result.read_integer()}")


def main():
    print_separator("SBW Python Math Client")
    
    # Parse command line
    host = "127.0.0.1"
    port = 10102
    
    if len(sys.argv) >= 2:
        host = sys.argv[1]
    if len(sys.argv) >= 3:
        port = int(sys.argv[2])
    
    print(f"Connecting to broker at {host}:{port}...")
    
    try:
        conn = connect(host, port, "python.math.client")
        print(f"Connected! Module ID = {conn.module_id}")
        
        try:
            # Discover all modules
            discover_modules(conn)
            
            # Call math server using low-level API
            call_math_server_lowlevel(conn)
            
            # Call math server using high-level API
            call_math_server_highlevel(conn)
            
        finally:
            print()
            print("Disconnecting...")
            conn.disconnect()
        
        print_separator("All tests completed!")
        
    except Exception as e:
        print(f"ERROR: {type(e).__name__}: {e}")
        import traceback
        traceback.print_exc()
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
