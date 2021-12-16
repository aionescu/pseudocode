module Backend.Codegen

open System
open System.Reflection
open System.Reflection.Emit

open Frontend.Syntax
open Midend.Core

let panic () = failwith "Panic in Codegen"
let unsupported () = failwith "Instruction is not yet supported"

type IL = ILGenerator

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

let rec ilType = function
  | Bool -> typeof<bool>
  | Int -> typeof<int>
  | Float -> typeof<float>
  | String -> typeof<string>
  | Array t -> (ilType t).MakeArrayType()

let allocVars (il: IL) = List.iter (fun v -> il.DeclareLocal(ilType v) |> ignore)

let rec emitInstr (il: IL) =
  function
  | PushBool b -> il.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
  | PushInt i -> il.Emit(OpCodes.Ldc_I4, i)
  | PushFloat f -> il.Emit(OpCodes.Ldc_R8, f)
  | PushString s -> il.Emit(OpCodes.Ldstr, s)

  | NewArr ty -> il.Emit(OpCodes.Newarr, ilType ty)

  | LoadVar i -> il.Emit(OpCodes.Ldloc, i)
  | SetVar i -> il.Emit(OpCodes.Stloc, i)

  | Dup -> il.Emit(OpCodes.Dup)

  | LoadIndex Bool -> il.Emit(OpCodes.Ldelem_I4)
  | LoadIndex Int -> il.Emit(OpCodes.Ldelem_I4)
  | LoadIndex Float -> il.Emit(OpCodes.Ldelem_R8)
  | LoadIndex _ -> il.Emit(OpCodes.Ldelem_Ref)

  | SetIndex Bool -> il.Emit(OpCodes.Stelem_I4)
  | SetIndex Int -> il.Emit(OpCodes.Stelem_I4)
  | SetIndex Float -> il.Emit(OpCodes.Stelem_R8)
  | SetIndex _ -> il.Emit(OpCodes.Stelem_Ref)

  | Read t ->
      il.Emit(OpCodes.Call, consoleReadLine)

      match t with
      | Bool -> il.Emit(OpCodes.Call, boolParse)
      | Int -> il.Emit(OpCodes.Call, intParse)
      | Float -> il.Emit(OpCodes.Call, floatParse)
      | String -> ()
      | Array _ -> panic ()

  | Write t ->
      match t with
      | Bool -> il.Emit(OpCodes.Call, consoleWriteBool)
      | Int -> il.Emit(OpCodes.Call, consoleWriteInt)
      | Float -> il.Emit(OpCodes.Call, consoleWriteFloat)
      | String -> il.Emit(OpCodes.Call, consoleWriteString)
      | Array _ -> panic ()

  | WriteLine -> il.Emit(OpCodes.Call, consoleWriteLine)

  | Length true -> il.Emit(OpCodes.Call, stringLength)
  | Length false ->
      il.Emit(OpCodes.Ldlen)
      il.Emit(OpCodes.Conv_I4)

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
      List.iter (emitInstr il) t
      il.Emit(OpCodes.Br, doneLbl)

      il.MarkLabel(elseLbl)
      List.iter (emitInstr il) f
      il.MarkLabel(doneLbl)

  | While (c, s) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      List.iter (emitInstr il) c

      il.Emit(OpCodes.Brfalse, doneLbl)
      List.iter (emitInstr il) s

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

let compileAndRun vars instrs =
  let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Pseudocode"), AssemblyBuilderAccess.Run)
  let mdl = asm.DefineDynamicModule("Module")
  let ty = mdl.DefineType("Program")

  let mtd = ty.DefineMethod("Main", MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.Static, typeof<Void>, Array.empty)
  let il = mtd.GetILGenerator()

  allocVars il vars
  List.iter (emitInstr il) instrs
  il.Emit(OpCodes.Ret)

  let ty = ty.CreateType()
  let mtd = ty.GetMethod("Main", BindingFlags.NonPublic ||| BindingFlags.Static)

  try ignore <| mtd.Invoke(null, Array.empty)
  with :? TargetInvocationException as e ->
    printfn "An exception was thrown:"
    printfn "%A" e.InnerException
