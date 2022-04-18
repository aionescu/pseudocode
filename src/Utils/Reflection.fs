module Utils.Reflection

open System
open System.Collections.Generic

let ilBool = typeof<bool>
let ilInt = typeof<int>
let ilFloat = typeof<float>
let ilString = typeof<string>
let ilVoid = typeof<Void>
let ilConsole = typeof<Console>
let ilMath = typeof<Math>

let ilList t = typedefof<List<_>>.MakeGenericType([|t|])

let mtd (ty: Type) name args  = ty.GetMethod(name, List.toArray args)

let mathPow = mtd ilMath "Pow" [ilFloat; ilFloat]

let consoleReadLine = mtd ilConsole "ReadLine" []
let consoleWriteLine = mtd ilConsole "WriteLine" []
let consoleWrite t = mtd ilConsole "Write" [t]
let staticParse t = mtd t "Parse" [ilString]

let stringConcat = mtd ilString "Concat" [ilString; ilString]
let stringCompare = mtd ilString "Compare" [ilString; ilString]
let stringEq = mtd ilString "op_Equality" [ilString; ilString]
let stringNeq = mtd ilString "op_Inequality" [ilString; ilString]
let stringLength = mtd ilString "get_Length" []

let listCtor t = (ilList t).GetConstructor([||])
let listCount t = mtd (ilList t) "get_Count" []
let listGetItem t = mtd (ilList t) "get_Item" [ilInt]
let listSetItem t = mtd (ilList t) "set_Item" [ilInt; t]
let listAdd t = mtd (ilList t) "Add" [t]
let listRemoveAt t = mtd (ilList t) "RemoveAt" [ilInt]
