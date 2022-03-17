module Compiler.Backend.Codegen

open System
open System.Reflection
open System.Reflection.Emit

open Utils
open Compiler.Frontend.Syntax
open Compiler.Midend.Core

let panic () = failwith "Panic in Codegen"
let unsupported () = failwith "Instruction is not yet supported"

type IL = ILGenerator

let defaultLbl = Unchecked.defaultof<_>

let consoleReadLine = typeof<Console>.GetMethod("ReadLine", [||])
let intParse = typeof<int>.GetMethod("Parse", [|typeof<string>|])
let floatParse = typeof<float>.GetMethod("Parse", [|typeof<string>|])
let boolParse = typeof<bool>.GetMethod("Parse", [|typeof<string>|])

let consoleWriteString = typeof<Console>.GetMethod("Write", [|typeof<string>|])
let consoleWriteInt = typeof<Console>.GetMethod("Write", [|typeof<int>|])
let consoleWriteFloat = typeof<Console>.GetMethod("Write", [|typeof<float>|])
let consoleWriteBool = typeof<Console>.GetMethod("Write", [|typeof<bool>|])
let consoleWriteLine = typeof<Console>.GetMethod("WriteLine", [||])

let mathPow = typeof<Math>.GetMethod("Pow", [|typeof<float>; typeof<float>|])
let stringConcat = typeof<string>.GetMethod("Concat", [|typeof<string>; typeof<string>|])
let stringCompare = typeof<string>.GetMethod("Compare", [|typeof<string>; typeof<string>|])

let stringEq = typeof<string>.GetMethod("op_Equality", [|typeof<string>; typeof<string>|])
let stringNeq = typeof<string>.GetMethod("op_Inequality", [|typeof<string>; typeof<string>|])
let stringLength = typeof<string>.GetMethod("get_Length", [||])

let ilList = typedefof<Collections.Generic.List<_>>

let rec ilType = function
  | Bool -> typeof<bool>
  | Int -> typeof<int>
  | Float -> typeof<float>
  | String -> typeof<string>
  | List t -> ilList.MakeGenericType([|ilType t|])

let listType = List >> ilType

let listCtor t = (listType t).GetConstructor([||])
let listCount t = (listType t).GetProperty("Count", [||]).GetMethod
let listGetItem t = (listType t).GetProperty("Item", [|typeof<int>|]).GetMethod
let listSetItem t = (listType t).GetProperty("Item", [|typeof<int>|]).SetMethod
let listAdd t = (listType t).GetMethod("Add", [|ilType t|])
let listRemoveAt t = (listType t).GetMethod("RemoveAt", [|typeof<int>|])

let allocVars (il: IL) = List.iter (ilType >> il.DeclareLocal >> ignore)

let rec emitInstr fns (il: IL) breakLbl contLbl =
  function
  | PushBool b -> il.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
  | PushInt i -> il.Emit(OpCodes.Ldc_I4, i)
  | PushFloat f -> il.Emit(OpCodes.Ldc_R8, f)
  | PushString s -> il.Emit(OpCodes.Ldstr, s)

  | NewList t -> il.Emit(OpCodes.Newobj, listCtor t)

  | LoadVar i -> il.Emit(OpCodes.Ldloc, i)
  | SetVar i -> il.Emit(OpCodes.Stloc, i)

  | ClearVar (i, t) ->
      if not (ilType t).IsValueType then
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Stloc, i)

  | Dup -> il.Emit(OpCodes.Dup)

  | LoadArg i -> il.Emit(OpCodes.Ldarg, i)
  | SetArg i -> il.Emit(OpCodes.Starg, i)

  | LoadIndex t -> il.Emit(OpCodes.Callvirt, listGetItem t)
  | SetIndex t -> il.Emit(OpCodes.Callvirt, listSetItem t)
  | Push t -> il.Emit(OpCodes.Callvirt, listAdd t)
  | Pop t ->
      il.Emit(OpCodes.Dup)
      il.Emit(OpCodes.Callvirt, listCount t)
      il.Emit(OpCodes.Ldc_I4_1)
      il.Emit(OpCodes.Sub)
      il.Emit(OpCodes.Callvirt, listRemoveAt t)

  | Read t ->
      il.Emit(OpCodes.Call, consoleReadLine)

      match t with
      | Bool -> il.Emit(OpCodes.Call, boolParse)
      | Int -> il.Emit(OpCodes.Call, intParse)
      | Float -> il.Emit(OpCodes.Call, floatParse)
      | String -> ()
      | List _ -> panic ()

  | Write t ->
      match t with
      | Bool -> il.Emit(OpCodes.Call, consoleWriteBool)
      | Int -> il.Emit(OpCodes.Call, consoleWriteInt)
      | Float -> il.Emit(OpCodes.Call, consoleWriteFloat)
      | String -> il.Emit(OpCodes.Call, consoleWriteString)
      | List _ -> panic ()

  | WriteLine -> il.Emit(OpCodes.Call, consoleWriteLine)

  | Length None -> il.Emit(OpCodes.Call, stringLength)
  | Length (Some t) -> il.Emit(OpCodes.Callvirt, listCount t)

  | Not ->
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)
  | Negate -> il.Emit(OpCodes.Neg)

  | Append -> il.Emit(OpCodes.Call, stringConcat)
  | Pow -> il.Emit(OpCodes.Call, mathPow)

  | Arith Add -> il.Emit(OpCodes.Add)
  | Arith Sub -> il.Emit(OpCodes.Sub)
  | Arith Mul -> il.Emit(OpCodes.Mul)
  | Arith Div -> il.Emit(OpCodes.Div)
  | Arith Mod -> il.Emit(OpCodes.Rem)

  | Comp (Eq, false) -> il.Emit(OpCodes.Ceq)
  | Comp (Neq, false) ->
      il.Emit(OpCodes.Ceq)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)
  | Comp (Lt, false) -> il.Emit(OpCodes.Clt)
  | Comp (Gt, false) -> il.Emit(OpCodes.Cgt)
  | Comp (Lte, false) ->
      il.Emit(OpCodes.Cgt)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)
  | Comp (Gte, false) ->
      il.Emit(OpCodes.Clt)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)

  | Comp (Eq, true) -> il.Emit(OpCodes.Call, stringEq)
  | Comp (Neq, true) -> il.Emit(OpCodes.Call, stringNeq)
  | Comp (Lt, true) ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Clt)
  | Comp (Gt, true) ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Cgt)
  | Comp (Lte, true) ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Cgt)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)
  | Comp (Gte, true) ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Clt)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Ceq)

  | If (t, f) ->
      let elseLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.Emit(OpCodes.Brfalse, elseLbl)
      emitInstr fns il breakLbl contLbl t
      il.Emit(OpCodes.Br, doneLbl)

      il.MarkLabel(elseLbl)
      emitInstr fns il breakLbl contLbl f
      il.MarkLabel(doneLbl)

  | While (c, s) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstr fns il breakLbl contLbl c

      il.Emit(OpCodes.Brfalse, doneLbl)
      emitInstr fns il doneLbl loopLbl s

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

  | DoWhile (s, c) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstr fns il doneLbl loopLbl s
      emitInstr fns il breakLbl contLbl c
      il.Emit(OpCodes.Brtrue, loopLbl)
      il.MarkLabel(doneLbl)

  | For (c, s, u) ->
      let loopLbl = il.DefineLabel()
      let updateLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstr fns il breakLbl contLbl c

      il.Emit(OpCodes.Brfalse, doneLbl)
      emitInstr fns il doneLbl updateLbl s

      il.MarkLabel(updateLbl)
      emitInstr fns il breakLbl contLbl u

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

  | Break -> il.Emit(OpCodes.Br, breakLbl)
  | Continue -> il.Emit(OpCodes.Br, contLbl)
  | Return -> il.Emit(OpCodes.Ret)

  | Call f -> il.Emit(OpCodes.Call, Map.find f fns :> MethodInfo)

  | Nop -> ()
  | Seq (a, b) ->
      emitInstr fns il breakLbl contLbl a
      emitInstr fns il breakLbl contLbl b

let defineFn (ty: TypeBuilder) { name = name; args = args; retType = retType } =
  let args = List.map (snd >> ilType) args
  let retType = option typeof<Void> ilType retType

  let attrs = MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.Static
  ty.DefineMethod(name, attrs, retType, List.toArray args)

let compileFn (fns: Map<Id, MethodBuilder>) { name = name } (vars, instr) =
  let mtd = Map.find name fns
  let il = mtd.GetILGenerator()

  allocVars il vars
  emitInstr fns il defaultLbl defaultLbl instr
  il.Emit(OpCodes.Ret)

let compileProgram { fns = fns } =
  let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Pseudocode"), AssemblyBuilderAccess.Run)
  let mdl = asm.DefineDynamicModule("Module")
  let ty = mdl.DefineType("Program")

  let mtds = Map.map (const' <| (defineFn ty << fst)) fns
  Map.iter (const' <| uncurry (compileFn mtds)) fns

  ty.CreateType()

let runCompiledProgram (ty: System.Type) =
  let program = ty.GetMethod("program", BindingFlags.NonPublic ||| BindingFlags.Static)

  try ignore <| program.Invoke(null, [||])
  with :? TargetInvocationException as e ->
    printfn "An exception was thrown:"
    printfn "%A" e.InnerException
