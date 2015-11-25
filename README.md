[玩具 Haskell compiler][compiler]的一部份

詳見 https://gitter.im/CindyLinz/BuildYourOwnHaskellCompiler

# 使用方法

    stack setup
    stack build
    stack exec transpiler-exe hello.hs

- 如果沒有參數，會讀取 stdin 的內容

- 用 json 輸出 transpile 後的結果

    $ stack exec transpiler-exe
    lambda = 'λ'
    main = putChar lambda
    ^D
    [["lambda",["prim","str","λ"]],
     ["main",["app",["var","putChar"],["var","lambda"]]]]
    # 重新排版過的結果

- 可以跟[玩具 Haskell compiler][compiler] 的 `LC_db_lazy_env.py` 結合使用

    $ stack exec transpiler-exe hello.hs | python LC_db_lazy_env.py
    Hello, World!

[compiler]: https://github.com/op8867555/BYOHC
