
# 数値シミュレーションで読み解く統計の仕組み・サポートサイト

こちらは「数値シミュレーションで読み解く統計の仕組み；Rでわかる心理統計」のサポートサイトです。
ここでは本書で案内していたコード，練習問題の解答例を掲載しています。

本書「はじめに」でも言及しましたが，こちらのサイトにあるコードをコピーして利用することはお勧めしません。
読者の皆さんには，Rプログラミングの技術を習得していただくことを期待しており，コードをコピー＆ペーストすることではプログラミング技術は身に付かないからです。同じデジタルな情報を*書き写す*ことは馬鹿馬鹿しいことのように思えますが，転記ミス，誤タッチ，TYPOを自分で経験し，エラーの対処を体験することが上達への近っ道だからです。

プログラミング技術を磨くためには，まず教本の写経から始め，ついでコードの一部を自分で改変し，どこがどう変わったか確認する，という段階に進んでください。常にコードを「読み」，結果に「触れる」ことで自らが体験すること，自分なりの感覚を把握することが肝要です。

また，ここに示されている練習問題の解答例は，あくまでも解答「例」であり，学力テストのような唯一の正答ではありません。FizzBuzz課題の例で示したように，機能的に一緒であれば，実装の方法についてはいくつものルートがありえます。あくまでも「そういう答え方もあるのか」という参照をしてください。

# 正誤表

## 第1章

- (誤) p.6　Rコード`n <- 2500`　-\> (正) `n <- 25`
  + テキストでは$n=25$の例として示していましたが，コードは続く$n=2500$の例を実行するものになっていました。
 
## 第3章

- (誤) P.134 Rコード10行目 `rt(n, nu) |> var_p` -\> (正) `rt(n, nu) |> var_p()`
  + Rのネイティブパイプは，関数()の形に渡すことが必要です(`magrittr`パッケージのパイプ演算子`%>%`であれば問題ありません)

## 第4章

- (誤) P.181 最終行「パーセンタイル信頼区間の方が広くなっています」 -\> (正) 「今回はパーセンタイル信頼区間の方が狭くなっています」
  + P.182 2行目のコードが誤っていたことに連動する誤記です
- (誤) P.182 2行目のコード`t.test(sample_r)$conf.int[1:2] # t.test()関数で信頼区間を直接求める` -\> (正) `cor.test(dat_obs[, 1], dat_obs[, 2])$conf.int[1:2] # cor.test()関数で信頼区間を直接求める`
  + P.182 2行目のコードが誤っていたことに連動する誤記です
- (誤) P.182 3行目の出力`[1] 0.4890356 0.5016704` -\> (正) `[1] 0.3787639 0.8187475`
  + P.182 2行目のコードが誤っていたことに連動する誤記です
- (誤) P.182 9行目の出力`[1] "0.4973"` -\> (正) `[1] 0.4217412`
  + P.182 2行目のコードが誤っていたことに連動する誤記です
- (誤) P.182 10行目の出力`[1] "0.5011"` -\> (正) `[1] 0.5897387`
  + P.182 2行目のコードが誤っていたことに連動する誤記です


# 各章のコード

- [第1章のコード](ch1/ch1.R)
- [第2章のコード](ch2/ch2.R) / [練習問題の解答例](ch2/ch2_practice.R)
- [第3章のコード](ch3/ch3.R) / 練習問題の解答例
- [第4章のコード](ch4/ch4.R) / 練習問題の解答例
- [第5章のコード](ch5.ch5.R) / [練習問題の解答例](ch5/ch5_practice.R)
- [第6章のコード](ch6/ch6.R) / [練習問題の解答例](ch6/ch6_practice.R)
- [第7章のコード](ch7/ch7.R) / [練習問題の解答例](ch7/ch7_practice.R)

# 確認環境

コードについては以下の環境で確認しました。

------------------------------------------------------------------------

R version 4.3.1 (2023-06-16)

Platform: x86_64-apple-darwin22.4.0 (64-bit)

Running under: macOS Ventura 13.4.1

------------------------------------------------------------------------
