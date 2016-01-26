[玩具 Haskell compiler][compiler]的一部份

詳見 https://github.com/CindyLinz/BYOHC-Workshop/

# 使用方法

    stack setup
    stack build
    stack exec transpiler-exe hello.hs

- 如果沒有參數，會讀取 stdin 的內容

- 用 json 輸出 transpile 後的結果

```
$ stack exec transpiler-exe
lambda = 'λ'
main = putChar lambda
^D
["app",["app",["var","Y"],["lam","##gen",["lam","##tuple",["app",["app",["lam","lambda",["lam","main",["app",["app",["var","##tuple"],["prim","str","λ"]],["app",["var","putChar"],["var","lambda"]]]]],["app",["var","##gen"],["lam","lambda",["lam","main",["var","lambda"]]]]],["app",["var","##gen"],["lam","lambda",["lam","main",["var","main"]]]]]]]],["lam","lambda",["lam","main",["app",["var","runIO"],["var","main"]]]]]
```

- 可以跟[玩具 Haskell compiler][compiler] 的 `LC_db_lazy_env.py` 結合使用

```
$ stack exec transpiler-exe hello.hs | python LC_db_lazy_env.py
Hello, World!
```

# Examples

```
$ cd example/ffi/
$ stack exec transpiler-exe hello.hs | PYTHONPATH=. python LC_db_lazy_env.py -m hello
hello
```

# Desugar

- 用 `--output-level Desugared` 來看 desugar 過的 code

```
$ stack exec transpiler -- --output-level Desugared < example/Case.hs
data Main.T where
        Main.A :: Main.T
        Main.B :: Main.T -> Main.T
Main.f
  = \ x_0 ->
      let fallback_2 = 0 in
        case x_0 of
            Main.A -> fallback_2
            Main.B y_1 -> (Prelude.+) 1 (Main.f y_1)
Main.main
  = Prelude.putStrLn
      (case (Main.f (Main.B Main.A)) of
           0 -> "0"
           1 -> "1")
```

- 已實作

  - if
  - where
  - renamer
  - 部份 case
    -  alts reorder & completion

[compiler]: https://github.com/op8867555/BYOHC
