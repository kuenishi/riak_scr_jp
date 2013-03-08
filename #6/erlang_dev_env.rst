.. last_update: Wed Feb 27 00:30:03 2013

=====================
 Erlang/OTP 開発環境
=====================

おまえ、誰よ
============

- @shkumagai / Shoji KUMAGAI
- Accense Technology Inc.
- Erlang/OTP 歴: 1 年程度


Erlang/OTP の開発環境ってどうしてますか？
=========================================

OS は？
-------

- Linux
- OS X
- Windows


インストール方法は？
--------------------

- お手軽な バイナリインストール
- もちろん ソース から
- よく分からない


バイナリリリースでのインストール
--------------------------------

ダウンロード: http://www.erlang.org/doc/installation_guide/install-binary.html

- ソースパッケージ
- Windows Installer (32bit & 64bit)
- HTML ドキュメント
- Man Page ファイル

Erlang Solution : https://www.erlang-solutions.com/downloads/download-erlang-otp

- apt-get / yum でインストールする方法が書いてある

ビルド済みバイナリのあるディストリビューション :

- Windows
- Ubuntu
- Raspbian
- OpenSUSE
- Mac OS X
- Fedore
- Debian
- CentOS
- Linux


ソースからのインストール
------------------------

http://www.erlang.org/doc/installation_guide/INSTALL.html

Requirements ::

  展開:
  - unzip

  - tar

  ビルド:
  - make
  - gcc
  - Perl5
  - GNU m4
    HiPE (Native code) サポート時、必須。 --without-hipe 指定可。

  - ncurses, termcap, termlib ヘッダ
    つまり \*-dev パッケージ。但し --without-termcap 指定時は不要。

  - OpenSSL
    オプショナル。但し ssl, crypto を使う場合は必須。ヘッダファイルも必要。
    バージョン 0.9.8 以上が必要。

  - Java JDK
  - X Windows
  - sed
  - flex

  ドキュメント:
  - xsltproc
  - fop

  Git リポジトリからのビルド:
  - autoconf
    version 2.59 以上が必要。

インストール手順
~~~~~~~~~~~~~~~~

参考として、R16B のインストール手順を次に示します。

::

    ## ダウンロード & 展開
    $ VERSION="R16B"
    $ curl -O http://www.erlang.org/download/otp_src_${VERSION}.tar.gz
    $ tar zxf otp_src_${VERSION}.tar.gz

    ## configure
    $ cd otp_src_${VERSION}
    $ LANG=C; export LANG

    ## ./configure -h で configure オプションが確認できる
    $ ./configure --prefix=/opt/local/erlang/${VERSION} [options]

    $ make
    $ make install

Erlang/OTP Japan Mirror by Basho::

    http://download.basho.co.jp.try-cs.ycloud.jp/index.html

R16B は無かった...

Git リポジトリからインストール
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

otp は Git リポジトリ (https://github.com/erlang/otp) で開発が行われていて、
リポジトリのソースを使ってビルドすることもできます。::

    ## リポジトリの clone (readonly)
    $ git clone git://github.com/erlang/otp.git
    $ cd otp
    $ git checkout master

    ## configure & make
    $ ./otp_build autoconf
    $ ./configure --prefix=/opt/local/erlang/master [options]
    $ make
    $ make install


エディタサポートとか
====================

Erlang には標準で Emacs サポート (elisp) が付いてきます。

http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html

.. note::
   Vim な人にはこちら。
   http://www.vim.org/scripts/script.php?script_id=3743

elisp を有効にする
------------------

init.erl に次の記述を追加します。::

    (setq load-path (cons "/opt/local/erlang/<VERSION>/lib/tools-<ToolsVer>/emacs"
                          load-path))
    (setq erlang-root-dir "/opt/local/erlang/<VERSION>")
    (setq exec-path (cons "/opt/local/erlang/<VERSION>/bin" exec-path))
    (require 'erlang-start)

.. note::
   apt-get で erlang をインストールした場合、そのままでは tools がインストールされません。
   次のコマンドで tools をインストールした後、前述の手順で elisp を読み込んでください。::

       $ sudo apt-get install erlang-tools

タグジャンプ
------------

http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html#id64483

Exuberant Ctags (http://ctags.sourceforge.net/) を使った方法を次に示します。
まず Exuberant Ctags をインストールします。MacPorts の場合 ::

    $ sudo port install ctags

src.rpm や Windows binary も提供されているようなので、試してみるとよいでしょう。

init.el には次のように設定を追加します。::

    (require 'ctags)
    (setq tags-revert-without-query t)
    ;; Command-line to call `ctags'.
    (setq ctags-command "/opt/local/bin/ctags -R -e ")
    ;; Comment out when anything-exuberant-ctags.el not in use.
    ;; (setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
    (global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)


.. note::
   anything-exuberant-ctags.el というのもあるので、そちらを使うことも可能。
   自分は使ったことないですけど。


シェル補完
==========

bash-completion:

  http://bash-completion.alioth.debian.org/

zsh-completion:

  https://github.com/zsh-users/zsh-completions


補完スクリプトを追加で動作させるための設定
------------------------------------------
::

    ## .zshrc の先頭の方に以下を追記
    fpath=(path/to/zsh-completions/src $fpath)

    autoload -U compinit && compinit

例:

https://github.com/shkumagai/dotfiles/blob/develop/zshrc#L7


おすすめしたい Erlang 関連の補完スクリプト
------------------------------------------

rebar
~~~~~

https://github.com/rebar/rebar/tree/master/priv/shell-completion -- bash,zsh


riak
~~~~

https://github.com/shkumagai/my-zsh-completions/blob/master/src/_riak -- zsh
