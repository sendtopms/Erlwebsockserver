{application, demowebsocket,
 [{description, "demowebsocket"},
  {vsn, "0.01"},
  {modules, [
    demowebsocket,
    demowebsocket_app,
    demowebsocket_sup,
    demowebsocket_web,
    demowebsocket_deps
  ]},
  {registered, []},
  {mod, {demowebsocket_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
