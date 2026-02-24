#!/usr/bin/env python3
"""
Unit tests for SBW DataBlock encoding/decoding.

These tests verify that Python's encoding matches Delphi's expectations.
Run with: python -m pytest test_sbw_datablock.py -v
Or simply: python test_sbw_datablock.py
"""

import struct
from sbw_client import DataBlockWriter, DataBlockReader, DataType


def test_byte():
    """Test byte encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_byte(42)
    writer.write_byte(255)
    writer.write_byte(0)
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_byte() == 42
    assert reader.read_byte() == 255
    assert reader.read_byte() == 0
    assert not reader.has_more()


def test_boolean():
    """Test boolean encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_boolean(True)
    writer.write_boolean(False)
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_boolean() == True
    assert reader.read_boolean() == False
    assert not reader.has_more()


def test_integer():
    """Test 32-bit integer encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_integer(0)
    writer.write_integer(12345)
    writer.write_integer(-12345)
    writer.write_integer(2147483647)   # Max int32
    writer.write_integer(-2147483648)  # Min int32
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_integer() == 0
    assert reader.read_integer() == 12345
    assert reader.read_integer() == -12345
    assert reader.read_integer() == 2147483647
    assert reader.read_integer() == -2147483648
    assert not reader.has_more()


def test_double():
    """Test 64-bit double encoding/decoding"""
    import math
    
    writer = DataBlockWriter()
    writer.write_double(0.0)
    writer.write_double(3.14159265358979)
    writer.write_double(-2.71828)
    writer.write_double(1e308)
    writer.write_double(-1e-308)
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_double() == 0.0
    assert abs(reader.read_double() - 3.14159265358979) < 1e-15
    assert abs(reader.read_double() - (-2.71828)) < 1e-10
    assert reader.read_double() == 1e308
    assert reader.read_double() == -1e-308
    assert not reader.has_more()


def test_string():
    """Test UTF-8 string encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_string("")
    writer.write_string("Hello, World!")
    writer.write_string("Unicode: \u00e9\u00e8\u00ea")  # French accents
    writer.write_string("Emoji: \U0001F600")  # Grinning face
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_string() == ""
    assert reader.read_string() == "Hello, World!"
    assert reader.read_string() == "Unicode: \u00e9\u00e8\u00ea"
    assert reader.read_string() == "Emoji: \U0001F600"
    assert not reader.has_more()


def test_complex():
    """Test complex number encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_complex(1.0, 2.0)
    writer.write_complex(-3.5, 4.5)
    writer.write_complex(0.0, 0.0)
    
    reader = DataBlockReader(writer.to_bytes())
    c = reader.read_complex()
    assert c.real == 1.0 and c.imag == 2.0
    c = reader.read_complex()
    assert c.real == -3.5 and c.imag == 4.5
    c = reader.read_complex()
    assert c.real == 0.0 and c.imag == 0.0
    assert not reader.has_more()


def test_integer_array():
    """Test 1D integer array encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_integer_array([])
    writer.write_integer_array([1, 2, 3, 4, 5])
    writer.write_integer_array([-100, 0, 100])
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_integer_array() == []
    assert reader.read_integer_array() == [1, 2, 3, 4, 5]
    assert reader.read_integer_array() == [-100, 0, 100]
    assert not reader.has_more()


def test_double_array():
    """Test 1D double array encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_double_array([])
    writer.write_double_array([1.1, 2.2, 3.3])
    writer.write_double_array([3.14159, -2.71828])
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_double_array() == []
    arr = reader.read_double_array()
    assert len(arr) == 3
    assert abs(arr[0] - 1.1) < 1e-10
    assert abs(arr[1] - 2.2) < 1e-10
    assert abs(arr[2] - 3.3) < 1e-10
    arr = reader.read_double_array()
    assert len(arr) == 2
    assert not reader.has_more()


def test_string_array():
    """Test 1D string array encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_string_array([])
    writer.write_string_array(["hello", "world"])
    writer.write_string_array(["one", "", "three"])
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_string_array() == []
    assert reader.read_string_array() == ["hello", "world"]
    assert reader.read_string_array() == ["one", "", "three"]
    assert not reader.has_more()


def test_double_array_2d():
    """Test 2D double array encoding/decoding"""
    writer = DataBlockWriter()
    matrix = [
        [1.0, 2.0, 3.0],
        [4.0, 5.0, 6.0]
    ]
    writer.write_double_array_2d(matrix)
    
    reader = DataBlockReader(writer.to_bytes())
    result = reader.read_double_array_2d()
    assert len(result) == 2
    assert len(result[0]) == 3
    assert result[0][0] == 1.0
    assert result[1][2] == 6.0
    assert not reader.has_more()


def test_list():
    """Test list encoding/decoding"""
    writer = DataBlockWriter()
    writer.write_list_begin(3)
    writer.write_integer(42)
    writer.write_string("hello")
    writer.write_double(3.14)
    
    reader = DataBlockReader(writer.to_bytes())
    items = reader.read_list()
    assert len(items) == 3
    assert items[0] == 42
    assert items[1] == "hello"
    assert abs(items[2] - 3.14) < 1e-10


def test_mixed_types():
    """Test reading mixed types with read_any()"""
    writer = DataBlockWriter()
    writer.write_byte(1)
    writer.write_boolean(True)
    writer.write_integer(42)
    writer.write_double(3.14)
    writer.write_string("test")
    
    reader = DataBlockReader(writer.to_bytes())
    assert reader.read_any() == 1
    assert reader.read_any() == True
    assert reader.read_any() == 42
    assert abs(reader.read_any() - 3.14) < 1e-10
    assert reader.read_any() == "test"
    assert not reader.has_more()


def test_wire_format_integer():
    """Verify exact wire format for integers matches Delphi"""
    writer = DataBlockWriter()
    writer.write_integer(0x12345678)
    
    data = writer.to_bytes()
    # Should be: type(1) + value(4 bytes, little-endian)
    assert len(data) == 5
    assert data[0] == DataType.INTEGER
    assert data[1:5] == bytes([0x78, 0x56, 0x34, 0x12])  # Little-endian


def test_wire_format_string():
    """Verify exact wire format for strings matches Delphi"""
    writer = DataBlockWriter()
    writer.write_string("AB")
    
    data = writer.to_bytes()
    # Should be: type(1) + length(4) + chars + null(1)
    assert len(data) == 8
    assert data[0] == DataType.STRING
    assert data[1:5] == bytes([2, 0, 0, 0])  # Length = 2, little-endian
    assert data[5:7] == b"AB"
    assert data[7] == 0  # Null terminator


def test_nested_list():
    """Test nested list like [[id, name], [id, name], ...]"""
    writer = DataBlockWriter()
    # Outer list with 2 items
    writer.write_list_begin(2)
    # First inner list
    writer.write_list_begin(2)
    writer.write_integer(1)
    writer.write_string("module_one")
    # Second inner list
    writer.write_list_begin(2)
    writer.write_integer(2)
    writer.write_string("module_two")
    
    reader = DataBlockReader(writer.to_bytes())
    items = reader.read_list()
    
    assert len(items) == 2
    assert items[0] == [1, "module_one"]
    assert items[1] == [2, "module_two"]


def run_all_tests():
    """Run all tests and report results"""
    import sys
    
    tests = [
        test_byte,
        test_boolean,
        test_integer,
        test_double,
        test_string,
        test_complex,
        test_integer_array,
        test_double_array,
        test_string_array,
        test_double_array_2d,
        test_list,
        test_mixed_types,
        test_wire_format_integer,
        test_wire_format_string,
        test_nested_list,
    ]
    
    passed = 0
    failed = 0
    
    print("Running DataBlock tests...")
    print("-" * 50)
    
    for test in tests:
        name = test.__name__
        try:
            test()
            print(f"  ✓ {name}")
            passed += 1
        except AssertionError as e:
            print(f"  ✗ {name}: {e}")
            failed += 1
        except Exception as e:
            print(f"  ✗ {name}: {type(e).__name__}: {e}")
            failed += 1
    
    print("-" * 50)
    print(f"Results: {passed} passed, {failed} failed")
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    import sys
    sys.exit(run_all_tests())
