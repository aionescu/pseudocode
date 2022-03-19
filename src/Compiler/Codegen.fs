module Compiler.Codegen

open System
open System.Reflection
open System.Reflection.Emit

open Utils.Misc
open Compiler.AST
open Compiler.IR

let panic () = failwith "Panic in Codegen"

let ilList = typedefof<Collections.Generic.List<_>>

let rec ilTy = function
  | Bool -> typeof<bool>
  | Int -> typeof<int>
  | Float -> typeof<float>
  | String -> typeof<string>
  | List t -> ilList.MakeGenericType([|ilTy t|])

let listTy = List >> ilTy

let mathPow = typeof<Math>.GetMethod("Pow", [|typeof<float>; typeof<float>|])

let readLine = typeof<Console>.GetMethod("ReadLine", [||])
let writeLine = typeof<Console>.GetMethod("WriteLine", [||])
let writeTy t = typeof<Console>.GetMethod("Write", [|ilTy t|])
let parseTy t = (ilTy t).GetMethod("Parse", [|typeof<string>|])

let stringConcat = typeof<string>.GetMethod("Concat", [|typeof<string>; typeof<string>|])
let stringCompare = typeof<string>.GetMethod("Compare", [|typeof<string>; typeof<string>|])
let stringEq = typeof<string>.GetMethod("op_Equality", [|typeof<string>; typeof<string>|])
let stringNeq = typeof<string>.GetMethod("op_Inequality", [|typeof<string>; typeof<string>|])
let stringLength = typeof<string>.GetMethod("get_Length", [||])

let listCtor t = (listTy t).GetConstructor([||])
let listCount t = (listTy t).GetProperty("Count", [||]).GetMethod
let listGetItem t = (listTy t).GetProperty("Item", [|typeof<int>|]).GetMethod
let listSetItem t = (listTy t).GetProperty("Item", [|typeof<int>|]).SetMethod
let listAdd t = (listTy t).GetMethod("Add", [|ilTy t|])
let listRemoveAt t = (listTy t).GetMethod("RemoveAt", [|typeof<int>|])

type IL = ILGenerator

type Env =
  { fns: Map<Id, MethodBuilder>
    vars: Map<Id, LocalBuilder>
    il: ILGenerator
    breakLbl: Label
    contLbl: Label
  }

let emitNot (il: IL) =
  il.Emit(OpCodes.Ldc_I4_0)
  il.Emit(OpCodes.Ceq)

let rec emitInstr env instr =
  let { fns = fns; vars = vars; il = il; breakLbl = breakLbl; contLbl = contLbl } = env

  match instr with
  | PushBool b -> il.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
  | PushInt i -> il.Emit(OpCodes.Ldc_I4, i)
  | PushFloat f -> il.Emit(OpCodes.Ldc_R8, f)
  | PushString s -> il.Emit(OpCodes.Ldstr, s)

  | NewList t -> il.Emit(OpCodes.Newobj, listCtor t)

  | Let (i, t, e, s) ->
      let ilTy = ilTy t

      il.BeginScope()
      let l = il.DeclareLocal(ilTy)

      emitInstrs env e
      il.Emit(OpCodes.Stloc, l)

      emitInstrs { env with vars = Map.add i l vars } s
      il.EndScope()

  | LoadVar i -> il.Emit(OpCodes.Ldloc, vars[i])
  | SetVar i -> il.Emit(OpCodes.Stloc, vars[i])

  | LoadArg i -> il.Emit(OpCodes.Ldarg, i)
  | SetArg i -> il.Emit(OpCodes.Starg, i)

  | Dup -> il.Emit(OpCodes.Dup)

  | LoadIndex t -> il.Emit(OpCodes.Callvirt, listGetItem t)
  | SetIndex t -> il.Emit(OpCodes.Callvirt, listSetItem t)

  | ListPush t -> il.Emit(OpCodes.Callvirt, listAdd t)
  | ListPop t ->
      il.Emit(OpCodes.Dup)
      il.Emit(OpCodes.Callvirt, listCount t)
      il.Emit(OpCodes.Ldc_I4_1)
      il.Emit(OpCodes.Sub)
      il.Emit(OpCodes.Callvirt, listRemoveAt t)

  | Read t ->
      il.Emit(OpCodes.Call, readLine)

      if t <> String then
        il.Emit(OpCodes.Call, parseTy t)

  | Write t -> il.Emit(OpCodes.Call, writeTy t)
  | WriteLine -> il.Emit(OpCodes.Call, writeLine)

  | StrLength -> il.Emit(OpCodes.Call, stringLength)
  | ListLength t -> il.Emit(OpCodes.Callvirt, listCount t)

  | Not -> emitNot il
  | Negate -> il.Emit(OpCodes.Neg)

  | Append -> il.Emit(OpCodes.Call, stringConcat)

  | Pow -> il.Emit(OpCodes.Call, mathPow)

  | Arith Add -> il.Emit(OpCodes.Add)
  | Arith Sub -> il.Emit(OpCodes.Sub)
  | Arith Mul -> il.Emit(OpCodes.Mul)
  | Arith Div -> il.Emit(OpCodes.Div)
  | Arith Mod -> il.Emit(OpCodes.Rem)

  | Comp Eq -> il.Emit(OpCodes.Ceq)
  | Comp Lt -> il.Emit(OpCodes.Clt)
  | Comp Gt -> il.Emit(OpCodes.Cgt)
  | Comp Neq ->
      il.Emit(OpCodes.Ceq)
      emitNot il
  | Comp Lte ->
      il.Emit(OpCodes.Cgt)
      emitNot il
  | Comp Gte ->
      il.Emit(OpCodes.Clt)
      emitNot il

  | StrComp Eq -> il.Emit(OpCodes.Call, stringEq)
  | StrComp Neq -> il.Emit(OpCodes.Call, stringNeq)
  | StrComp Lt ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Clt)
  | StrComp Gt ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Cgt)
  | StrComp Lte ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Cgt)
      emitNot il
  | StrComp Gte ->
      il.Emit(OpCodes.Call, stringCompare)
      il.Emit(OpCodes.Ldc_I4_0)
      il.Emit(OpCodes.Clt)
      emitNot il

  | If (t, f) ->
      let elseLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.Emit(OpCodes.Brfalse, elseLbl)
      emitInstrs env t
      il.Emit(OpCodes.Br, doneLbl)

      il.MarkLabel(elseLbl)
      emitInstrs env f
      il.MarkLabel(doneLbl)

  | While (c, s) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstrs env c

      il.Emit(OpCodes.Brfalse, doneLbl)
      emitInstrs { env with breakLbl = doneLbl; contLbl = loopLbl } s

      il.Emit(OpCodes.Br, loopLbl)
      il.MarkLabel(doneLbl)

  | DoWhile (s, c) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstrs { env with breakLbl = doneLbl; contLbl = loopLbl } s

      emitInstrs env c
      il.Emit(OpCodes.Brtrue, loopLbl)
      il.MarkLabel(doneLbl)

  | For (i, init, down, bound, s) ->
      il.BeginScope()

      let loopLbl = il.DefineLabel()
      let updateLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      let l = il.DeclareLocal(typeof<int>)
      let b = il.DeclareLocal(typeof<int>)

      emitInstrs env init
      il.Emit(OpCodes.Stloc, l)

      emitInstrs env bound
      il.Emit(OpCodes.Stloc, b)

      il.MarkLabel(loopLbl)
      il.Emit(OpCodes.Ldloc, l)
      il.Emit(OpCodes.Ldloc, b)
      il.Emit(if down then OpCodes.Clt else OpCodes.Cgt)

      il.Emit(OpCodes.Brtrue, doneLbl)
      emitInstrs { env with vars = Map.add i l vars; breakLbl = doneLbl; contLbl = updateLbl } s

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

and emitInstrs env = List.iter (emitInstr env)

let defineFn (ty: TypeBuilder) { name = name; args = args; retTy = retTy } _ =
  let args = List.map (snd >> ilTy) args
  let retTy = option typeof<Void> ilTy retTy

  let attrs = MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.Static
  ty.DefineMethod(name, attrs, retTy, List.toArray args)

let compileFn (fns: Map<_, MethodBuilder>) { name = name } instrs =
  let mtd = fns[name]
  let il = mtd.GetILGenerator()

  emitInstrs
    { fns = fns
      vars = Map.empty
      il = il
      breakLbl = Unchecked.defaultof<_>
      contLbl = Unchecked.defaultof<_>
    }
    instrs

  il.Emit(OpCodes.Ret)

let compile p =
  let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Pseudocode"), AssemblyBuilderAccess.Run)
  let mdl = asm.DefineDynamicModule("Module")
  let ty = mdl.DefineType("Program")

  let fns = mapVals snd <| (mapFns (defineFn ty) p).fns
  mapFns (compileFn fns) p |> ignore

  ty.CreateType()

let runProgram (ty: Type) =
  let program = ty.GetMethod("program", BindingFlags.NonPublic ||| BindingFlags.Static)

  try ignore <| program.Invoke(null, [||])
  with :? TargetInvocationException as e ->
    printfn "An exception was thrown:"
    printfn "%A" e.InnerException
