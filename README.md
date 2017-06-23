cache
=====

[![Build Status](https://travis-ci.org/hverr/haskell-cache.svg?branch=master)](https://travis-ci.org/hverr/haskell-cache)
[![Hackage](https://img.shields.io/hackage/v/cache.svg?maxAge=2592000)](https://hackage.haskell.org/package/cache)
[![Stackage Nightly](http://stackage.org/package/cache/badge/nightly)](http://stackage.org/nightly/package/cache)
[![Stackage LTS](http://stackage.org/package/cache/badge/lts)](http://stackage.org/lts/package/cache)
[![Documentation](https://img.shields.io/badge/docs-v0.1.0.0-green.svg)](https://hverr.github.io/haskell-cache)

An in-memory key/value store with expiration support, similar to patrickmn/go-cache for Go.

The cache is a shared mutable HashMap implemented using STM. It supports item expiration.
