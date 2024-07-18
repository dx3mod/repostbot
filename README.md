# repostbot

Bot for forwarding user's posts from VK to Telegram chat.

## Usage

Installation.
```bash
opam pin https://github.com/dx3mod/repostbot.git
```

See systemd [service example](./service/) where [`EnvironmentFile`][EnvFile] must contain variables:
```dotenv
VK_TOKEN="..."
TG_TOKEN="..."

TARGET_USER="..."
TARGET_CHAT="..."

CACHE_FILE="..."
```

Minimal cache file:
```clojure
((last_post_id ...) (posts ()))
```

[EnvFile]: https://www.freedesktop.org/software/systemd/man/latest/systemd.exec.html#EnvironmentFile=