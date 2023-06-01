module Language.Pseudocode.Codegen

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

open Language.Pseudocode.Syntax
open Language.Pseudocode.IR

open type OpCodes

// Reflection utils

let ilList t = typedefof<List<_>>.MakeGenericType([|t|])

let mathPow = typeof<Math>.GetMethod("Pow", [|typeof<float>; typeof<float>|])

let consoleReadLine = typeof<Console>.GetMethod("ReadLine", [||])
let consoleWriteLine = typeof<Console>.GetMethod("WriteLine", [||])
let consoleWrite t = typeof<Console>.GetMethod("Write", [|t|])
let staticParse (t: Type) = t.GetMethod("Parse", [|typeof<string>|])

let stringConcat = typeof<string>.GetMethod("Concat", [|typeof<string>; typeof<string>|])
let stringCompare = typeof<string>.GetMethod("Compare", [|typeof<string>; typeof<string>|])
let stringEq = typeof<string>.GetMethod("op_Equality", [|typeof<string>; typeof<string>|])
let stringNeq = typeof<string>.GetMethod("op_Inequality", [|typeof<string>; typeof<string>|])
let stringLength = typeof<string>.GetMethod("get_Length", [||])

let listCtor t = (ilList t).GetConstructor([||])
let listCount t = (ilList t).GetMethod("get_Count", [||])
let listGetItem t = (ilList t).GetMethod("get_Item", [|typeof<int>|])
let listSetItem t = (ilList t).GetMethod("set_Item", [|typeof<int>; t|])
let listAdd t = (ilList t).GetMethod("Add", [|t|])
let listRemoveAt t = (ilList t).GetMethod("RemoveAt", [|typeof<int>|])

// IL generation

type Env =
  { fns: Map<Id, MethodBuilder>
    vars: Map<Id, LocalBuilder>
    il: ILGenerator
    breakLbl: Label
    contLbl: Label
  }

type ILGenerator with
  member il.EmitNot() =
    il.Emit(Ldc_I4_0)
    il.Emit(Ceq)

let rec ilTy = function
  | Bool -> typeof<bool>
  | Int -> typeof<int>
  | Float -> typeof<float>
  | String -> typeof<string>
  | List t -> ilList (ilTy t)

let rec emitInstr env instr =
  let { fns = fns; vars = vars; il = il; breakLbl = breakLbl; contLbl = contLbl } = env

  match instr with
  | PushBool b -> il.Emit(if b then Ldc_I4_1 else Ldc_I4_0)
  | PushInt i -> il.Emit(Ldc_I4, i)
  | PushFloat f -> il.Emit(Ldc_R8, f)
  | PushString s -> il.Emit(Ldstr, s)

  | NewList t -> il.Emit(Newobj, listCtor <| ilTy t)

  | Let (i, t, e, s) ->
      let ilTy = ilTy t

      il.BeginScope()
      let l = il.DeclareLocal(ilTy)

      emitInstrs env e
      il.Emit(Stloc, l)

      emitInstrs { env with vars = Map.add i l vars } s
      il.EndScope()

  | LoadVar i -> il.Emit(Ldloc, vars[i])
  | SetVar i -> il.Emit(Stloc, vars[i])

  | LoadArg i -> il.Emit(Ldarg, i)
  | SetArg i -> il.Emit(Starg, i)

  | Dup -> il.Emit(Dup)

  | LoadIndex t -> il.Emit(Call, listGetItem <| ilTy t)
  | SetIndex t -> il.Emit(Call, listSetItem <| ilTy t)

  | ListPush t -> il.Emit(Call, listAdd <| ilTy t)
  | ListPop t ->
      il.Emit(Dup)
      il.Emit(Call, listCount <| ilTy t)
      il.Emit(Ldc_I4_1)
      il.Emit(Sub)
      il.Emit(Call, listRemoveAt <| ilTy t)

  | Read t ->
      il.Emit(Call, consoleReadLine)

      if t <> String then
        il.Emit(Call, staticParse <| ilTy t)

  | Write t -> il.Emit(Call, consoleWrite <| ilTy t)
  | WriteLine -> il.Emit(Call, consoleWriteLine)

  | StrLength -> il.Emit(Call, stringLength)
  | ListLength t -> il.Emit(Call, listCount <| ilTy t)

  | Not -> il.EmitNot()
  | Negate -> il.Emit(Neg)

  | Append -> il.Emit(Call, stringConcat)

  | Pow -> il.Emit(Call, mathPow)

  | Arith Add -> il.Emit(Add)
  | Arith Sub -> il.Emit(Sub)
  | Arith Mul -> il.Emit(Mul)
  | Arith Div -> il.Emit(Div)
  | Arith Mod -> il.Emit(Rem)

  | Comp Eq -> il.Emit(Ceq)
  | Comp Lt -> il.Emit(Clt)
  | Comp Gt -> il.Emit(Cgt)
  | Comp Neq ->
      il.Emit(Ceq)
      il.EmitNot()
  | Comp Lte ->
      il.Emit(Cgt)
      il.EmitNot()
  | Comp Gte ->
      il.Emit(Clt)
      il.EmitNot()

  | StrComp Eq -> il.Emit(Call, stringEq)
  | StrComp Neq -> il.Emit(Call, stringNeq)
  | StrComp Lt ->
      il.Emit(Call, stringCompare)
      il.Emit(Ldc_I4_0)
      il.Emit(Clt)
  | StrComp Gt ->
      il.Emit(Call, stringCompare)
      il.Emit(Ldc_I4_0)
      il.Emit(Cgt)
  | StrComp Lte ->
      il.Emit(Call, stringCompare)
      il.Emit(Ldc_I4_0)
      il.Emit(Cgt)
      il.EmitNot()
  | StrComp Gte ->
      il.Emit(Call, stringCompare)
      il.Emit(Ldc_I4_0)
      il.Emit(Clt)
      il.EmitNot()

  | If (t, f) ->
      let elseLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.Emit(Brfalse, elseLbl)
      emitInstrs env t
      il.Emit(Br, doneLbl)

      il.MarkLabel(elseLbl)
      emitInstrs env f
      il.MarkLabel(doneLbl)

  | While (c, s) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstrs env c

      il.Emit(Brfalse, doneLbl)
      emitInstrs { env with breakLbl = doneLbl; contLbl = loopLbl } s

      il.Emit(Br, loopLbl)
      il.MarkLabel(doneLbl)

  | DoWhile (s, c) ->
      let loopLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      il.MarkLabel(loopLbl)
      emitInstrs { env with breakLbl = doneLbl; contLbl = loopLbl } s

      emitInstrs env c
      il.Emit(Brtrue, loopLbl)
      il.MarkLabel(doneLbl)

  | For (i, down, b, s) ->
      let l = vars[i]

      let loopLbl = il.DefineLabel()
      let updateLbl = il.DefineLabel()
      let doneLbl = il.DefineLabel()

      emitInstrs env b

      il.MarkLabel(loopLbl)
      il.Emit(Dup)
      il.Emit(Ldloc, l)
      il.Emit(if down then Cgt else Clt)

      il.Emit(Brtrue, doneLbl)
      emitInstrs { env with vars = Map.add i l vars; breakLbl = doneLbl; contLbl = updateLbl } s

      il.MarkLabel(updateLbl)
      il.Emit(Ldloc, l)
      il.Emit(Ldc_I4_1)
      il.Emit(if down then Sub else Add)
      il.Emit(Stloc, l)

      il.Emit(Br, loopLbl)
      il.MarkLabel(doneLbl)

      il.Emit(Pop)

  | Break -> il.Emit(Br, breakLbl)
  | Continue -> il.Emit(Br, contLbl)
  | Return -> il.Emit(Ret)

  | Call f -> il.Emit(Call, fns[f] :> MethodInfo)

and emitInstrs env = List.iter (emitInstr env)

let defineFn (ty: TypeBuilder) { name = name; args = args; retTy = retTy } _ =
  let args = List.map (snd >> ilTy) args
  let retTy =
    match retTy with
    | None -> typeof<Void>
    | Some t -> ilTy t

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

  il.Emit(Ret)

let compile p =
  let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Pseudocode"), AssemblyBuilderAccess.Run)
  let mdl = asm.DefineDynamicModule("Module")
  let ty = mdl.DefineType("Program")

  let fns = mapVals snd <| (mapFns (defineFn ty) p).fns
  mapFns (compileFn fns) p |> ignore

  ty.CreateType()

let runProgram (ty: Type) =
  let program = ty.GetMethod("program", BindingFlags.NonPublic ||| BindingFlags.Static)

  try program.Invoke(null, [||]) |> ignore
  with :? TargetInvocationException as e ->
    printfn "An exception was thrown:"
    printfn "%A" e.InnerException
