module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import V.V1.Instr
import Utils.Value
import Utils.Var

v2Codegen :: L2 -> Code
v2Codegen (L2 ss)
  = (concatMap s2Codegen ss) ++ [Halt]

s2Codegen :: S2 -> Code
s2Codegen (LRead s v)
  = [Push (VStr s), Print, Input, Store v]
s2Codegen (LPrint e)
  = e2Codegen e ++ [Print]
s2Codegen (LAssign v e)
  = e2Codegen e ++ [Store v]
s2Codegen (LDef v e ss)
  = [Store v] ++ e2Codegen e ++ (concatMap s2Codegen ss)

e2Codegen :: E2 -> Code
e2Codegen (LVal v) = [Push v]
e2Codegen (LVar v) = [Load v]
e2Codegen (LAdd e1 e2)
  = e2Codegen e1 ++ e2Codegen e2 ++ [Add]
e2Codegen (LMinus e1 e2)
  = e2Codegen e1 ++ e2Codegen e2 ++ [Sub]
e2Codegen (LMul e1 e2)
  = e2Codegen e1 ++ e2Codegen e2 ++ [Mul]
e2Codegen (LDiv e1 e2)
  = e2Codegen e1 ++ e2Codegen e2 ++ [Div]
