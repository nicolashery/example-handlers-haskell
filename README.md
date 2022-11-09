# example-handlers-haskell

This repository explores how to implement web handlers that perform common web application operations (logging, database calls, HTTP calls, spawning threads, catching exceptions, acquiring and releasing resources, etc.) in a few popular Haskell web frameworks ([Scotty](https://github.com/scotty-web/scotty), [Yesod](https://www.yesodweb.com/), [Servant](https://docs.servant.dev/)).

It also looks at how to integrate each web framework with a custom monad stack, such as the one used by the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).

## Setup

Prerequisites:
- [GHCup](https://www.haskell.org/ghcup/)
  - Used to install `ghc` and `cabal`
- [PostgreSQL](https://www.postgresql.org/)
  - Need `postgresql` service running and `psql` in path
  - Also install header files to compile Haskell PostgreSQL library (`libpq-dev` on Ubuntu)

Create the project's `example_haskell` database by running:

```
./scripts/create-db.sh
```

(If you need to reset your database, run `./scripts/drop-db.sh` and create it again.)

Manually source the project's environment, or install [direnv](https://direnv.net/) to do it automatically for you:

```
source .envrc
```

This will set the proper `ghc` and `cabal` versions using GHCup, as well as the database connection string environment variable.

Build the project with:

```
cabal build
```

## Running

In a first terminal, run the fake "external" service with:

```
cabal run example-external
```

In a second terminal, run one of the web framework examples:

```
cabal run example-scotty
# or
cabal run example-yesod
# or
cabal run example-servant
```

## Testing

Manually test the single endpoint using a valid Cart ID that exists in the database:

```
$ curl -i -X POST http://localhost:3000/cart/abc123/purchase
HTTP/1.1 200 OK

{"booking_id":"TKCY693D5ACB","cart_id":"abc123","payment_id":"zTNBbSdy3vdOSnRT3xzFHviB","purchase_delay":1000}
```

Use a Cart ID that has already been purchased to test the error response:

```
$ curl -i -X POST http://localhost:3000/cart/def456/purchase
HTTP/1.1 409 Conflict

{"error":"Cart already purchased"}
```

Use a hard-coded Cart ID that will trigger an error from the external service:

```
$ curl -i -X POST http://localhost:3000/cart/ghi789/purchase
HTTP/1.1 500 Internal Server Error

{"error":"Cart purchase failed: Payment failed: Invalid card number"}
```

Use a Cart ID that does not exist:

```
$ curl -i -X POST http://localhost:3000/cart/xyz000/purchase
HTTP/1.1 404 Not Found

{"error":"Cart does not exist"}
```
