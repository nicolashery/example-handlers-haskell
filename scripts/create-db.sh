#!/usr/bin/env bash
set -x

sudo -u postgres psql <<EOF
CREATE DATABASE example_haskell;
CREATE ROLE example_haskell WITH LOGIN PASSWORD 'example_haskell';
GRANT ALL PRIVILEGES ON DATABASE example_haskell TO example_haskell;
EOF

sudo -u postgres psql -d example_haskell <<EOF
CREATE TYPE cart_status AS ENUM ('open', 'locked', 'purchased');

CREATE TABLE carts (
  id text PRIMARY KEY,
  status cart_status NOT NULL
);

INSERT INTO carts (id, status) VALUES
    ('abc123', 'open'),
    ('def456', 'purchased'),
    ('ghi789', 'open');
EOF
