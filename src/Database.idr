module Database

import Data.String

import HList

public export
data Column : Type where
  CInt : Column
  CString : Column
  CBoolean : Column

public export
columnToType : Column -> Type
columnToType CInt = Integer
columnToType CString = String
columnToType CBoolean = Bool

public export
Schema : Type
Schema = List Column

public export
Row : Schema -> Type
Row schema = HList (map columnToType schema)

public export
record Table where
  constructor MkTable
  schema : Schema
  rows : List (Row schema)

append : (t : Table) -> (row : Row t.schema) -> Table
append (MkTable schema rows) row = MkTable schema (row :: rows)

-- append (MkTable [CInt, CBoolean, CString] [[1, False, "name"]]) [2, True, "second name"]
-- append (MkTable [CInt, CBoolean, CString] [[1, False, "name"]]) [True, True, "second name"]

parseColumnValue : (c : Column) -> String -> Maybe (columnToType c)
parseColumnValue CInt     str     = parseInteger str
parseColumnValue CString  str     = Just str
parseColumnValue CBoolean "True"  = Just True
parseColumnValue CBoolean "False" = Just False
parseColumnValue CBoolean _       = Nothing

-- parseColumnValue CInt "10"
-- parseColumnValue CString "string"
-- parseColumnValue CBoolean "False"

parseRow : (schema: Schema) -> List String -> Maybe (Row schema)
parseRow []            []        = Just []
parseRow []            (x :: xs) = Nothing
parseRow (col :: cols) []        = Nothing
parseRow (col :: cols) (x :: xs) = let
                                      Just head = parseColumnValue col x | _ => Nothing
                                      Just tail = parseRow cols xs | _ => Nothing
                                   in Just (head :: tail)

test : Maybe Table
test = let
    table1 = MkTable [CInt, CBoolean, CString] [[1, False, "name"]]
    table2 = MkTable [CInt, CBoolean] [[1, False]]
    Just row = parseRow table1.schema ["22", "False", "second name"] | _ => Nothing
  in Just (append table1 row)

Eq Column where
  CInt == CInt = True
  CString == CString = True
  CBoolean == CBoolean = True
  _ == _ = False

-- merge : Table -> Table -> Maybe Table
-- merge (MkTable schema rows) (MkTable schema' rows') = case schema == schema' of
--                                                         False => Nothing
--                                                         True => Just (MkTable schema (rows ++ rows'))
