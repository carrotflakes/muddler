(defpackage muddler.dictionary
  (:use :cl
        :muddler.dic-parser)
  (:export :*dictionary-external-format*
           :dictionary-cforms
           :dictionary-connect
           :dictionary-ctypes
           :dictionary-grammar
           :dictionary-dics
           :load-dictionary))
(in-package :muddler.dictionary)

#|
cforms.cha 活用形定義ファイル
(上二・ハ行
    (   ; (語幹        *         )
     (基本形      ふ    フ    ウ)
     (未然形      ひ    ヒ    イ)
     (連用形      ひ    ヒ    イ)
     (体言接続    ふる  フル  ウル)
     (仮定形      ふれ  フレ  ウレ)
     (命令ｙｏ    ひよ  ヒヨ  イヨ)))

connect.cha 連接表定義ファイル
(((((動詞 自立) 一段・病メル 基本形)) (((名詞 副詞可能)))) 1410)
(((((記号 読点))) (((形容詞 自立)))) 1415)
(((((接続詞))) (((名詞 固有名詞 人名 姓)))) 1417)
(((((名詞 固有名詞 地域 一般))) (((名詞 固有名詞 地域 一般)))) 1417)
(((((動詞 自立) サ変・スル 連用形)) (((動詞 自立) 五段・カ行イ音便))) 1418)

ctypes.cha 活用型定義ファイル
((形容詞 自立)
        (形容詞・アウオ段
         形容詞・イ段
         形容詞・イイ
         不変化型))

grammar.cha 品詞定義ファイル
(接頭詞
    (名詞接続)
    (動詞接続)
    (形容詞接続)
    (数接続))

*.dic
(品詞 (動詞 自立)) ((見出し語 (病める 2817)) (読み ヤメル) (発音 ヤメル) (活用型 一段) )
(品詞 (名詞 一般)) ((見出し語 (仕舞い 3999)) (読み シマイ) (発音 シマイ) )
(品詞 (形容詞 自立)) ((見出し語 (やぼったい 2979)) (読み ヤボッタイ) (発音 ヤボッタイ) (活用型 形容詞・アウオ段))
|#

(defvar *dictionary-external-format* :euc-jp)

(defstruct dictionary
  cforms
  connect
  ctypes
  grammar
  chasenrc
  dics)

(defun load-sexps (path)
  (with-open-file (s path :direction :input :external-format *dictionary-external-format*)
    (parse s)))

(defun load-dic (path)
  (loop
    with sexps = (load-sexps path)
    for pos = (pop sexps)
    for body = (pop sexps)
    while pos
    collect (cons pos body)))

(defun load-dictionary (path)
  (make-dictionary :cforms (load-sexps (merge-pathnames "cforms.cha" path))
                   :connect (load-sexps (merge-pathnames "connect.cha" path))
                   :ctypes (load-sexps (merge-pathnames "ctypes.cha" path))
                   :grammar (load-sexps (merge-pathnames "grammar.cha" path))
                   :chasenrc (load-sexps (merge-pathnames "chasenrc" path))
                   :dics (loop
                           for dic-path in (directory (merge-pathnames "*.dic" path))
                           collect (cons (pathname-name dic-path)
                                         (load-dic dic-path)))))
