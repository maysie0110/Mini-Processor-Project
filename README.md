# Mini Processor Project

## Project Desciption

 Design  using  VHDL and  components  designor  schematics capture and simulate an 8-bit processor (8-bit registers), which includes two registers R0 and R1, able to execute the instruction set (ADD, ADD Constant, SUB, SUB constant, MOV, EOR, NEG, AND, LSL, LSR, OUT)
 
 The machine has 18 input signals and 14 output signals. The 18 input signals consist of 16 bits used  for  instructions and  2 extra  bits  used  to  clock  the  operations:  one clock  signal  will command the execution of the operation (EXE) and the other will update the destination register  (UPD)  (these  two  bits  cannot  be  zero  at  the  same  time). The 14 output  bits  are connected to two seven-segment displays and should show the value contained in R0 (in hexadecimal format), except when instructed to do it differently by the instruction OUT
