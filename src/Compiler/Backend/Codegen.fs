module Compiler.Backend.Codegen

open System
open System.Reflection
open System.Reflection.Emit

open Utils
open Compiler.Frontend.AST
open Compiler.Midend.IR

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

type EmitEnv =
  { fns: Map<Id, MethodBuilder>
    vars: Map<Id, LocalBuilder>
    args: Id list
    il: ILGenerator
    breakLbl: Label
    contLbl: Label
  }

let rec emitInstr env instr =
  let { fns = fns; vars = vars; args = args; il = il; breakLbl = breakLbl; contLbl = contLbl } = env

  match instr with
  | PushBool b -> il.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
  | PushInt i -> il.Emit(OpCodes.Ldc_I4, i)
  | PushFloat f -> il.Emit(OpCodes.Ldc_R8, f)
  | PushString s -> il.Emit(OpCodes.Ldstr, s)

  | NewList t -> il.Emit(OpCodes.Newobj, listCtor t)

  | Let (i, t, e, s) ->
      let ilType = ilType t

      il.BeginScope()
      let l = il.DeclareLocal(ilType)

      emitInstr env e
      il.Emit(OpCodes.Stloc, l)

      emitInstr { env with vars = Map.add i l vars } s

      if not ilType.IsValueType then
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Stloc, l)

      il.EndScope()

  | LoadVar i ->
      match Map.tryFind i vars with
      | Some local -> il.Emit(OpCodes.Ldloc, local)
      | None -> il.Emit(OpCodes.Ldarg, List.findIndex ((=) i) args)

  | SetVar i ->
      match Map.tryFind i vars with
      | Some local -> il.Emit(OpCodes.Stloc, local)
      | None -> il.Emit(OpCodes.Starg, List.findIndex ((=) i) args)

  | Dup -> il.Emit(OpCodes.Dup)

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
      emitInstr env t
      il.Emit(OpCodes.Br, doneLbl)

      il.MarkLabel(elseLbl)
      emitInstr env f
      il.MarkLabel(doneLbl)

  | While (c, s) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstr env c

      il.Emit(OpCodes.Brfalse, doneLbl)
      emitInstr { env with breakLbl = doneLbl; contLbl = loopLbl } s

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

  | DoWhile (s, c) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstr { env with breakLbl = doneLbl; contLbl = loopLbl } s

      emitInstr env c
      il.Emit(OpCodes.Brtrue, loopLbl)
      il.MarkLabel(doneLbl)

  | For (i, init, down, bound, s) ->
      il.BeginScope()

      let loopLbl = il.DefineLabel()
      let updateLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      let l = il.DeclareLocal(typeof<int>)
      let b = il.DeclareLocal(typeof<int>)

      emitInstr env init
      il.Emit(OpCodes.Stloc, l)

      emitInstr env bound
      il.Emit(OpCodes.Stloc, b)

      il.MarkLabel(loopLbl)
      il.Emit(OpCodes.Ldloc, l)
      il.Emit(OpCodes.Ldloc, b)
      il.Emit(if down then OpCodes.Clt else OpCodes.Cgt)

      il.Emit(OpCodes.Brtrue, doneLbl)
      emitInstr { env with vars = Map.add i l vars; breakLbl = doneLbl; contLbl = updateLbl } s

      il.MarkLabel(updateLbl)
      il.Emit(OpCodes.Ldloc, l)
      il.Emit(OpCodes.Ldc_I4_1)
      il.Emit(if down then OpCodes.Sub else OpCodes.Add)
      il.Emit(OpCodes.Stloc, l)

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

      il.EndScope()

  | Break -> il.Emit(OpCodes.Br, breakLbl)
  | Continue -> il.Emit(OpCodes.Br, contLbl)
  | Return -> il.Emit(OpCodes.Ret)

  | Call f -> il.Emit(OpCodes.Call, fns[f] :> MethodInfo)

  | Nop -> ()
  | Seq (a, b) ->
      emitInstr env a
      emitInstr env b

let defineFn (ty: TypeBuilder) { name = name; args = args; retType = retType } _ =
  let args = List.map (snd >> ilType) args
  let retType = option typeof<Void> ilType retType

  let attrs = MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.Static
  ty.DefineMethod(name, attrs, retType, List.toArray args)

let compileFn (fns: Map<_, MethodBuilder>) { name = name; args = args } instr =
  let mtd = fns[name]
  let il = mtd.GetILGenerator()

  emitInstr
    { fns = fns
      vars = Map.empty
      args = List.map fst args
      il = il
      breakLbl = Unchecked.defaultof<_>
      contLbl = Unchecked.defaultof<_>
    }
    instr

  il.Emit(OpCodes.Ret)

let compileProgram p =
  let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Pseudocode"), AssemblyBuilderAccess.Run)
  let mdl = asm.DefineDynamicModule("Module")
  let ty = mdl.DefineType("Program")

  let fns = mapVals snd <| (mapFns (defineFn ty) p).fns
  mapFns (compileFn fns) p |> ignore

  ty.CreateType()

let runCompiledProgram (ty: System.Type) =
  let program = ty.GetMethod("program", BindingFlags.NonPublic ||| BindingFlags.Static)

  try ignore <| program.Invoke(null, [||])
  with :? TargetInvocationException as e ->
    printfn "An exception was thrown:"
    printfn "%A" e.InnerException
