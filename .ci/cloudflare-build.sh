#!/usr/bin/env bash

set -euxo pipefail

if [[ "$CF_PAGES_BRANCH" = "master" ]]
then
  zola build
else
  zola build -u "$CF_PAGES_URL"
fi
