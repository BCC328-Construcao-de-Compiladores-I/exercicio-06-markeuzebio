module L.L2.Backend.CCodegen where

import L.L2.Frontend.Syntax
import Utils.Pretty
import Utils.Value
import Utils.Var

-- top level code generation function

cL2Codegen :: L2 -> String
cL2Codegen e
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

generateBody :: L2 -> [String]
generateBody (L2 ss)
  = map generateStmt ss

generateStmt :: S2 -> String
generateStmt (LAssign v e)
  = unwords ["int", pretty v, "=", generateExp e, ";"]
generateStmt (LPrint e)
  = unwords ["printf(\"%d\",", generateExp e, ");"]
generateStmt (LRead s v)
  = unwords ["printf(",s,");\n", "scanf(%d, &", pretty v, ");"]
generateStmt (LDef id e ss)
  = unwords ["{\n", nest 3 "const int", pretty id, "=", generateExp e, ";\n", unlines $ map (nest 3) (generateBody (L2 ss)), nest 3 "}" ]
  where
    nest n v = replicate n ' ' ++ v

generateExp :: E2 -> String
generateExp = pretty

