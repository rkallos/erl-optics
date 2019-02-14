-define(APP, erl_optics).
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(ENV_HOSTNAME, hostname).
-define(ENV_PORT, port).
-define(ENV_INTERVAL, interval).
-define(DEFAULT_PORT, 1055).
-define(DEFAULT_HOSTNAME, "localhost").
-define(DEFAULT_INTERVAL, 10000).
