-define(APP, erl_optics).
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(ENV_HOSTNAME, localhost).
-define(ENV_PORT, 2003).
-define(DEFAULT_PORT, 8431).
-define(DEFAULT_INTERVAL, 1000).
-define(MAX_BUFFER_DURATION, 30000).
-define(MAX_BUFFER_LENGTH, ?MAX_BUFFER_DURATION / ?DEFAULT_INTERVAL).
