# RustyMonkey

Thorsten Ball 氏の著書 [Writing An Interpreter In Go](https://interpreterbook.com/)（設樂 洋爾 氏による邦訳：[Go言語でつくるインタプリタ](https://www.oreilly.co.jp/books/9784873118222/)）にある Monkey 言語の Rust 実装です。



## インストール

ビルドには Rust の開発環境（2018 edition）が必要です。

```
$ git clone https://github.com/PickledChair/RustyMonkey.git
$ cd RustyMonkey
$ cargo build
```



## 使い方

### REPL

```
$ cargo run
Hello <your name>! This is the Monkey programming language!
Feel free to type in commands
>>
```

終了時は `exit` または `quit` を入力してください。



### ソースファイルの実行

```
# test.txt
puts("hello!");
```



```
$ cargo run -- test.txt
hello!
```



## 独自拡張

### いくつかのエスケープ文字への対応

```
>> "h\te\tl\tl\to\t!"
=> h	e	l	l	o	!
>> "h\ne\nl\nl\no\n!"
=> h
e
l
l
o
!
>> "\"hello!\""
=> "hello!"
```



### マルチバイト文字への対応

```
>> let japanese = "こんにちは";
>> japanese
=> こんにちは
```



### 行コメント

```
>> # これはコメントです
>> # `#` 記号から改行文字までが無視されます
>> let hello = "hello"; # 式や文の後ろに書くこともできます
>> hello
=> hello
```



### 配列向けの組み込み関数や添字演算子の文字列への対応

```
>> let hello = "hello";
>> # 以下、戻り値はいずれも文字列です
>> first(hello)
=> h
>> rest(hello)
=> ello
>> last(hello)
=> o
>> hello[0]
=> h
>> hello[1]
=> e
```



### 整数に関する `<=`, `>=` 演算子の追加

```
>> 1 <= 1
=> true
>> 2 <= 1
=> false
>> 1 >= 2
=> false
>> 2 >= 1
=> true
>> 1 >= 1
=> true
```



### 追加の組み込み関数

```
>> # init 関数：rest 関数の逆です。引数が配列の場合、末尾の要素を除いた配列を返します。
>> # 引数が文字列の場合は、末尾の文字を除いた文字列を返します。
>> init([1,2,3,4,5])
=> [1, 2, 3, 4]
>> init("hello")
=> hell
>>
>> # print 関数：文字列を１つ引数にとり、改行なしで表示します。戻り値は null です
>> print("hello")
hello=> null
>>
>> # readline 関数：引数なし。入力待ちになるので、文字列を入力後 Enter で確定してください
>> # 戻り値は文字列です
>> let input = readline();
my input
>> input
=> my input
>>
>> # writefile 関数：ファイルパスと書き込む内容の文字列を引数にとります。
>> # 指定されたファイルをテキストモードで開き、内容を書き込みます。
>> # 戻り値は成功時に null, 失敗時にエラーを返します
>> writefile("test.txt", "ok")
=> null
>>
>> # readfile 関数：ファイルパスを引数にとります。
>> # 指定されたファイルをテキストモードで開き、ファイルの内容を文字列で返します
>> readfile("test.txt")
=> ok
>> # ファイルが見つからない場合はエラーを返します
>> readfile("hoge.txt")
=> ERROR: invalid file path: hoge.txt
```



### import 文

指定されたソースファイルをパース・評価して、得られたオブジェクトを現在の環境に追加します。

```
>> writefile("mylibrary.txt", "let myFunc = fn() { return \"This is my function!\"; };\n");
=> null
>> import "mylibrary.txt";
>> myFunc()
=> This is my function!
```

**備考**：原始的な import 文なので、互いに import し合うソースコードを評価すると無限ループに陥ります。



## License

MIT License で公開しています。