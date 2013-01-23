Golem - Hypo automation bot via XMPP (i.e. GoogleTalk, iChat)

## Build & Running

    sbt assembly
    nohup java -jar target/Golem-assembly-1.0.jar < /dev/null &

## Dependency

Because we turn off the tcp/ip connection of MySQL for security, MySQL JDBC Driver (Connector/J) need to connect to database using unix socket. Hence we need to install [junixsocket](http://code.google.com/p/junixsocket/). We need to put the JNI library to `/opt/newsclub/lib-native` or java.library.path. Check the [installation guide](http://code.google.com/p/junixsocket/wiki/GettingStarted) for more detail.

## Usage

  1. Add `hypobot@hypo.cc` to your google talk contact.

  2. Chat to him. You can use the following commands:

  - `restore #12345`: 把書本進度放回原作者 editor。
  - `restore #12345 to someone@example.org` 把書本進度放回 someone@example.org。
  - `json #12345` 取得 someone@example.org 的 json 資料。