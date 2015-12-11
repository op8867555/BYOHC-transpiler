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


[compiler]: https://github.com/op8867555/BYOHC
