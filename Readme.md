# 数値シミュレーションで読み解く統計の仕組み・サポートサイト

こちらは「数値シミュレーションで読み解く統計の仕組み；Rでわかる心理統計」のサポートサイトです。
ここでは本書で案内していたコード，練習問題の解答例を掲載しています。

本書「はじめに」でも言及しましたが，こちらのサイトにあるコードをコピーして利用することはお勧めしません。
読者の皆さんには，Rプログラミングの技術を習得していただくことを期待しており，コードをコピー＆ペーストすることではプログラミング技術は身に付かないからです。同じデジタルな情報を*書き写す*ことは馬鹿馬鹿しいことのように思えますが，転記ミス，誤タッチ，TYPOを自分で経験し，エラーの対処を体験することが上達への近っ道だからです。

プログラミング技術を磨くためには，まず教本の写経から始め，ついでコードの一部を自分で改変し，どこがどう変わったか確認する，という段階に進んでください。常にコードを「読み」，結果に「触れる」ことで自らが体験すること，自分なりの感覚を把握することが肝要です。

また，ここに示されている練習問題の解答例は，あくまでも解答「例」であり，学力テストのような唯一の正答ではありません。FizzBuzz課題の例で示したように，機能的に一緒であれば，実装の方法についてはいくつものルートがありえます。あくまでも「そういう答え方もあるのか」という参照をしてください。

# 正誤表

万全を期して作成したつもりですが，初版で既にいくつか間違いのご指摘をいただいております。
ご指摘に御礼申し上げ，また，ここにお詫びして修正をご報告いたします。

| page   | 誤                                                   | 正                                                           | 解説                                                                                                                                    |
|--------|------------------------------------------------------|--------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| p.6　  | Rコード`n <- 2500`                                   | `n <- 25`                                                    | テキストでは$n=25$の例として示していましたが，コードは続く$n=2500$の例を実行するものになっていました。                                  |
| p.10（下から5行目）　  | わかりやすく書いる書籍    | わかりやすく書いている書籍          |         |
| p.18L7 | BはAと同じくxを3列2行に                              | BはAと同じくxを3行2列に                                      | rowは行，colは列です。失礼しました。                                                                                                    |
| p.18   | Rの出力の要素が全て0になっている                     | 1から24までの数字が順に入ります。                            | array関数が配列を指定するものです。                                                                                                     |
| p.26-27   | 出力   | コード                            | 右肩に「出力」と書かれているブロックは、「コード」が正しいです。                                      |
| p.35-36   | 出力   | コード                            | 右肩に「出力」と書かれているブロックは、「コード」が正しいです。                                      |
| p.40   | 決して実行しないでくださいのコード                   | 変更なし                                                     | Rではカウンタ変数は別途割り当てられるので，永久ループにはならないそうです。しかしプログラミング言語として，一般的に避けるべき作法です。 |
| p.41   | Rが永遠の計算ループから抜け出せなくなります。        | 抜け出せなくなることはありませんが，おかしな挙動になります。 | 両方`i`で回すと，内側の`i`ループが外側の`i`ループ分繰り返されるという動きになります。                                                   |
| p.104  | Rコード最後の行内 `df(line_x,df1 = nu_1, df2= nu_2)` | `df(line_x,df1 = 1, df2= nu)`                                | これに伴い、図3.29の曲線もわずかに変化します（ヒストグラムに変化はありません）。                                                        |
| P.134  | Rコード`var_p`                                       | `var_p()`                                                    | Rのネイティブパイプは，関数()の形に渡すことが必要です(magritterのパイプ演算子`%>%`であれば問題ありません)                               |
| P.142本文下から3行目  |   サンプルサイズ$n$が4、10、100と大きくなるにつれて　　| サンプルサイズ$n$が4、20、100と大きくなるにつれて          |  |
| P.143図4.14  |  　　|    |  誤った画像ファイルが挿入されていました。コードを実行して出力される図が正しいです  |
| P.182  | Rコード`t.test(sample_r)$conf.int[1:2]`              | `cor.test(dat_obs[, 1], dat_obs[, 2])$conf.int[1:2]`         | 出力も`[1] 0.3787639 0.8187475`となります。                                                                                             |
| P.181  | パーセンタイル信頼区間の方が広くなっています。       | 今回はパーセンタイル信頼区間の方が狭くなっています。         | ここは一般的に狭くなるわけではないので。                                                                                                |
| P.182  | FisherのZ変換の上限(`0.4973`)と下限(`0.5011`)        | 上限は`0.5897387`，下限は`0.4217412`                         | Rのコード変更に伴って修正させていただきます。                                                                                           |

# 各章のコード

- [第1章のコード](ch1/ch1.R)
- [第2章のコード](ch2/ch2.R) / [練習問題の解答例](ch2/ch2_practice.R)
- [第3章のコード](ch3/ch3.R) / [練習問題の解答例](ch3/ch3_practice.R)
- [第4章のコード](ch4/ch4.R) / [練習問題の解答例](ch4/ch4_practice.R)
- [第5章のコード](ch5/ch5.R) / [練習問題の解答例](ch5/ch5_practice.R)
- [第6章のコード](ch6/ch6.R) / [練習問題の解答例](ch6/ch6_practice.R)
- [第7章のコード](ch7/ch7.R) / [練習問題の解答例](ch7/ch7_practice.R)

# フリーディスカッション＆FAQのコーナー

[Github Discussion](https://github.com/ghmagazine/simulation_stats_book/discussions) では，本書に関係するご意見，ご感想，みなさんのアイデアなどを書き込んでいただけます。
筆者陣はもちろん，読者コミュニティとしてもご利用いただけるかと思います。　Enjoy！


# 確認環境

コードについては以下の環境で確認しました。

------------------------------------------------------------------------

R version 4.3.1 (2023-06-16)

Platform: x86_64-apple-darwin22.4.0 (64-bit)

Running under: macOS Ventura 13.4.1

------------------------------------------------------------------------
