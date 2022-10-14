#!/usr/bin/env bash
set -x

sudo -u postgres psql <<EOF
DROP DATABASE example_haskell;
DROP ROLE example_haskell;
EOF
