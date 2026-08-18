# moderl opus 5 を選択する
# my_chat <- ellmer::chat_anthropic(model = "claude-sonnet-5")
my_chat <- ellmer::chat_anthropic(model = "claude-opus-5") 
chattr::chattr_use(my_chat)
# 日本語使用に備えてフォント名を指定。
chattr::chattr("ggplot2で日本語を使用するときはtheme にbase_family = 日本語フォントを忘れず指定してください。HiraKakuPro-W3とかで良いです。geom_text annotateも同様です。これは今後全てに適用してください。")
# コンフリクトを避けるため、ライブラリ名は明示的に使用するよう指示する
chattr::chattr("今後Rでソースを作成する際はmagrittr,xts,quantmod,mondate,forecaset,ggplot2 以外のライブラリに属する関数は「ライブラリ名::関数名」のようにライブラリ名を明示的に指定してください。")
# データの不要なダウンロードは避ける
chattr::chattr(r"(実行中の環境では1950年以降のS&P500 のデータがxts 形式で変数GSPCに格納され、メンテナンスされています。こちらのデータを使用し、ダウンロードはしないでください。)")
