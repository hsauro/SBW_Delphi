import sys
import math
import random
from sbw_client import (
    connect, SBWConnection, SBWService, DataBlockWriter, DataBlockReader,
    SBW_BROKER_MODULE_ID, SBW_NO_MODULE_ID
)

 
conn = connect()

module_id = conn.get_module_id('edu.demo.plottingCanvas')
service_id = conn.find_service_id(module_id, 'Draw')
addLine = conn.find_method_id(module_id, service_id, 'addLine')
forceRedraw = conn.find_method_id(module_id, service_id, 'redraw')
clearCanvas = conn.find_method_id(module_id, service_id, 'clear')
setColor = conn.find_method_id(module_id, service_id, 'setColor')
beginUpdate = conn.find_method_id(module_id, service_id, 'beginUpdate')
endUpdate = conn.find_method_id(module_id, service_id, 'endUpdate')

print (module_id, service_id, addLine, forceRedraw)

#try:    
#   math = SBWService(conn, "edu.demo.plottingCanvas", "Canvas")
#except Exception as e:
#   print(f"ERROR: Could not connect to math service: {e}")



conn.call(module_id, service_id, clearCanvas)
conn.call(module_id, service_id, beginUpdate)
for i in range (50):
    
    args = DataBlockWriter()
    args.write_double(random.random()*100)
    args.write_double(random.random()*100)
    args.write_double(random.random()*400)
    args.write_double(random.random()*400)
    
    args.write_integer(random.randint(0, 255))
    args.write_integer(random.randint(0, 255))
    args.write_integer(random.randint(0, 255))
    
    result = conn.call(module_id, service_id, addLine, args)
 
conn.call (module_id, service_id, endUpdate)
#conn.call(module_id, service_id, forceRedraw, DataBlockWriter())