(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* basic tests without functions, classes or import *)

let%expect_test _ =
  let source = "x = 42" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          return None |}]


let%expect_test _ =
  let source = {|
x = 42
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- $Call(n0, n1, None)
          return None |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x + y)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Add(n1, n2, None)
          n4 <- $Call(n0, n3, None)
          return None |}]


let%expect_test _ =
  let source = {|
x = 42
y = 10
print(x - y)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          TOPLEVEL[y] <- 10
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- TOPLEVEL[y]
          n3 <- $Binary.Subtract(n1, n2, None)
          n4 <- $Call(n0, n3, None)
          return None |}]


let%expect_test _ =
  let source = {|
x = 42
x += 10
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Add(n0, 10, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


let%expect_test _ =
  let source = {|
x = 42
x -= 10
print(x)
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[x]
          n1 <- $Inplace.Subtract(n0, 10, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


let%expect_test _ =
  let source = {|
pi = 3.14
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[pi] <- 3.14
          return None |}]


let%expect_test _ =
  let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[byte_data] <- "Hello"
          return None |}]


let%expect_test _ =
  let source = {|
l = [0, 1, 2, 3, 4, 5]
l[0:2]
l[0:2:1]
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[l] <- $BuildList(0, 1, 2, 3, 4, 5)
          n0 <- TOPLEVEL[l]
          n1 <- n0[$BuildSlice(0, 2)]
          n2 <- TOPLEVEL[l]
          n3 <- n2[$BuildSlice(0, 2, 1)]
          return None |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $Compare.neq(true, false, None)
          return None |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
print(l[0])
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[l] <- $BuildList(1, 2, 3)
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[l]
          n2 <- n1[0]
          n3 <- $Call(n0, n2, None)
          return None |}]


let%expect_test _ =
  let source = {|
l = [1, 2, 3]
x = 0
l[x] = 10
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[l] <- $BuildList(1, 2, 3)
          TOPLEVEL[x] <- 0
          n0 <- TOPLEVEL[l]
          n1 <- TOPLEVEL[x]
          n0[n1] <- 10
          return None |}]


let%expect_test _ =
  let source = {|
s = {1, 2, 3}
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[s] <- $BuildSet(1, 2, 3)
          return None |}]


let%expect_test _ =
  let source =
    {|
x = "1"
s = {x : 1, "2": 2}
print(s)

s = {"a": 42, "b": 1664}
print(s["1"])

# from cinder
d = { 0x78: "abc", # 1-n decoding mapping
      b"abc": 0x0078,# 1-n encoding mapping
      0x01: None,   # decoding mapping to <undefined>
      0x79: "",    # decoding mapping to <remove character>
      }
        |}
  in
  PyIR.test source ;
  [%expect
    {xxx|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- "1"
          n0 <- TOPLEVEL[x]
          TOPLEVEL[s] <- $BuildMap(n0, 1, "2", 2)
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[s]
          n3 <- $Call(n1, n2, None)
          n4 <- $BuildConstKeyMap($BuildTuple("a", "b"), 42, 1664, None)
          TOPLEVEL[s] <- n4
          n5 <- TOPLEVEL[print]
          n6 <- TOPLEVEL[s]
          n7 <- n6["1"]
          n8 <- $Call(n5, n7, None)
          n9 <- $BuildConstKeyMap($BuildTuple(120, "abc", 1, 121), "abc", 120, None, "", None)
          TOPLEVEL[d] <- n9
          return None |xxx}]


let%expect_test _ =
  let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[open]
          n1 <- $Call(n0, "foo.txt", "wt", None)
          TOPLEVEL[fp] <- n1
          n2 <- TOPLEVEL[fp]
          n3 <- $CallMethod[write](n2, "yolo", None)
          return None |}]


let%expect_test _ =
  let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[open]
          n1 <- $Call(n0, "foo.txt", "wt", None)
          n2 <- $CallMethod[__enter__](n1, None)
          TOPLEVEL[fp] <- n2
          n3 <- TOPLEVEL[fp]
          n4 <- $CallMethod[write](n3, "yolo", None)
          jmp b1

        b1:
          n5 <- $CallMethod[__enter__](n1, None, None, None, None)
          jmp b2

        b2:
          return None |}]


let%expect_test _ =
  let source =
    {|
values = [1, 2, [3, 4] , 5]
values2 = ('a', 'b')

result = (*[10, 100], *values, *values2)

print(result) # (10, 100, 1, 2, [3, 4], 5, 'a', 'b')

result = [*values, *values2] # [10, 100, 1, 2, [3, 4], 5, 'a', 'b']
print(result)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[values] <- $BuildList(1, 2, $BuildList(3, 4), 5)
          TOPLEVEL[values2] <- $BuildTuple("a", "b")
          n0 <- TOPLEVEL[values]
          n1 <- TOPLEVEL[values2]
          TOPLEVEL[result] <- $BuildTupleUnpack($BuildList(10, 100), n0, n1)
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[result]
          n4 <- $Call(n2, n3, None)
          n5 <- TOPLEVEL[values]
          n6 <- TOPLEVEL[values2]
          TOPLEVEL[result] <- $BuildListUnpack(n5, n6)
          n7 <- TOPLEVEL[print]
          n8 <- TOPLEVEL[result]
          n9 <- $Call(n7, n8, None)
          return None |}]


let%expect_test _ =
  let source = {|
x = 1
x = x + (x := 0)
print(x) # will print 1
          |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 1
          n0 <- TOPLEVEL[x]
          TOPLEVEL[x] <- 0
          n1 <- $Binary.Add(n0, 0, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[print]
          n3 <- TOPLEVEL[x]
          n4 <- $Call(n2, n3, None)
          return None |}]


let%expect_test _ =
  let source =
    {|
x = f(0, 1)
x = f(0, b=1)
x = f(a=0, b=1)
x = f(0, *args)
x = f(0, **d)
x = f(0, *args, **d)
x = f(0, *args1, *args2, **d1, **d2)
x = o.f(0, 1)
# Python3.8 compile all other method calls without CALL_METHOD!
x = o.f(0, b=1)
x = o.f(a=0, b=1)
x = o.f(0, *args)
x = o.f(0, **d)
x = o.f(0, *args, **d)
x = o.f(0, *args1, *args2, **d1, **d2)
|}
  in
  PyIR.test source ;
  [%expect
    {xxx|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, 0, 1, None)
          TOPLEVEL[x] <- n1
          n2 <- TOPLEVEL[f]
          n3 <- $Call(n2, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n5
          n6 <- TOPLEVEL[f]
          n7 <- TOPLEVEL[args]
          n8 <- $CallFunctionEx(n6, $BuildTupleUnpack($BuildTuple(0), n7), None, None)
          TOPLEVEL[x] <- n8
          n9 <- TOPLEVEL[f]
          n10 <- TOPLEVEL[d]
          n11 <- $CallFunctionEx(n9, $BuildTuple(0), n10, None)
          TOPLEVEL[x] <- n11
          n12 <- TOPLEVEL[f]
          n13 <- TOPLEVEL[args]
          n14 <- TOPLEVEL[d]
          n15 <- $CallFunctionEx(n12, $BuildTupleUnpack($BuildTuple(0), n13), n14, None)
          TOPLEVEL[x] <- n15
          n16 <- TOPLEVEL[f]
          n17 <- TOPLEVEL[args1]
          n18 <- TOPLEVEL[args2]
          n19 <- TOPLEVEL[d1]
          n20 <- TOPLEVEL[d2]
          n21 <- $CallFunctionEx(n16, $BuildTupleUnpack($BuildTuple(0), n17, n18), $BuildMapUnpack(n19, n20), None)
          TOPLEVEL[x] <- n21
          n22 <- TOPLEVEL[o]
          n23 <- $CallMethod[f](n22, 0, 1, None)
          TOPLEVEL[x] <- n23
          n24 <- TOPLEVEL[o]
          n25 <- n24.f
          n26 <- $Call(n25, 0, 1, $BuildTuple("b"))
          TOPLEVEL[x] <- n26
          n27 <- TOPLEVEL[o]
          n28 <- n27.f
          n29 <- $Call(n28, 0, 1, $BuildTuple("a", "b"))
          TOPLEVEL[x] <- n29
          n30 <- TOPLEVEL[o]
          n31 <- n30.f
          n32 <- TOPLEVEL[args]
          n33 <- $CallFunctionEx(n31, $BuildTupleUnpack($BuildTuple(0), n32), None, None)
          TOPLEVEL[x] <- n33
          n34 <- TOPLEVEL[o]
          n35 <- n34.f
          n36 <- TOPLEVEL[d]
          n37 <- $CallFunctionEx(n35, $BuildTuple(0), n36, None)
          TOPLEVEL[x] <- n37
          n38 <- TOPLEVEL[o]
          n39 <- n38.f
          n40 <- TOPLEVEL[args]
          n41 <- TOPLEVEL[d]
          n42 <- $CallFunctionEx(n39, $BuildTupleUnpack($BuildTuple(0), n40), n41, None)
          TOPLEVEL[x] <- n42
          n43 <- TOPLEVEL[o]
          n44 <- n43.f
          n45 <- TOPLEVEL[args1]
          n46 <- TOPLEVEL[args2]
          n47 <- TOPLEVEL[d1]
          n48 <- TOPLEVEL[d2]
          n49 <- $CallFunctionEx(n44, $BuildTupleUnpack($BuildTuple(0), n45, n46), $BuildMapUnpack(n47, n48), None)
          TOPLEVEL[x] <- n49
          return None
|xxx}]


let%expect_test _ =
  let source =
    {|
def main(arg):
    def f(x: int, y: str = "ok", z: float = 0.0, *l, key=None):
        return arg

|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["main", "dummy.main"](None, None, None, None, None)
          TOPLEVEL[main] <- n0
          return None


      function dummy.main.f(x, y, z):
        b0:
          n0 <- $LoadDeref[0,"arg"](None)
          return n0


      function dummy.main(arg):
        b0:
          n0 <- $BuildConstKeyMap($BuildTuple("key"), None, None)
          n1 <- GLOBAL[int]
          n2 <- GLOBAL[str]
          n3 <- GLOBAL[float]
          n4 <- $BuildConstKeyMap($BuildTuple("x", "y", "z"), n1, n2, n3, None)
          n5 <- $LoadClosure[0,"arg"](None)
          n6 <- $MakeFunction["f", "dummy.main.f"]($BuildTuple("ok", 0.), n0, n4, $BuildTuple(n5), None)
          LOCAL[f] <- n6
          return None |}]


let%expect_test _ =
  let source = {|
def foo(n):
    o.update({ "key": "*" * n})
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["foo", "dummy.foo"](None, None, None, None, None)
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo(n):
        b0:
          n0 <- GLOBAL[o]
          n1 <- LOCAL[n]
          n2 <- $Binary.Multiply("*", n1, None)
          n3 <- $CallMethod[update](n0, $BuildMap("key", n2), None)
          return None |}]
