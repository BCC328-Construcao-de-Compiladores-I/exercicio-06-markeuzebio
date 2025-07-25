module L.L1.Backend.CCodegen where

import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Var
import Utils.Value

-- top level code generation function

cL1Codegen :: L1 -> String
cL1Codegen e
  = unlines $ [ "#include <stdio.h>"
              , "// code generated for expressions"
              , "int main () {" ] ++
              (map (nest 3) (generateBody e)) ++
              [ nest 3 $ "putchar('\\n');"
              , nest 3 "return 0;"
              , "}"
              ]
    where
      nest n v = replicate n ' ' ++ v

generateBody :: L1 -> [String]
generateBody (L1 ss)
  = map generateStmt ss

generateStmt :: S1 -> String
generateStmt (LAssign id e)
  = unwords ["int", pretty id, "=", generateExp e, ";"]
generateStmt (LPrint e)
  = unwords ["printf(%d,", generateExp e, ");"]
generateStmt (LRead s id)
  = unwords ["print(\"",s,"\");\n", "scanf(%d, &", pretty id, ");"]

generateExp :: E1 -> String
generateExp = pretty
